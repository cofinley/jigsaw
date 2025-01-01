(ns jigsaw.algo
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.set]
            [clojure.math :as math]
            [jigsaw.utils :as utils]
            [jigsaw.spec :as specs]))

(defn parts
  [x]
  {:pre [(specs/pitch-or-note? x)]}
  (let [[_ pitch-str letter-str accidental-str octave-str] (re-find specs/pitch-or-note-pattern (name x))]
    (-> {:pitch (keyword pitch-str)
         :letter (first letter-str)
         :accidental accidental-str}
        (cond->
         (some? octave-str) (assoc :octave (utils/parse-int octave-str)
                                   :note (keyword (str pitch-str octave-str)))))))

(defn- staff-distance
  [x1 x2]
  {:pre [(every? specs/pitch-or-note? [x1 x2])]}
  (let [{letter1 :letter} (parts x1)
        {letter2 :letter} (parts x2)
        i1 (int letter1)
        i2 (int letter2)]
    (inc (mod (- i2 i1) 7))))

(defn- lesser? [s] (some (partial string/includes? s) ["d" "m"]))
(defn- greater? [s] (some (partial string/includes? s) ["A"]))

(defn- accidental-match? [accidental-string]
  (fn [p]
    (let [{:keys [accidental]} (parts p)]
      (= accidental accidental-string))))
(def flat? (accidental-match? "b"))
(def natural? (accidental-match? ""))
(def sharp? (accidental-match? "#"))

(defn enharmonic
  [p notation]
  {:post [(specs/pitch? %)]}
  (let [index (mod (specs/pitches p) 12)
        equivalent-pitches (specs/pitches-by-index index)]
    (when (pos? (count equivalent-pitches))
      (if (= 1 (count equivalent-pitches))
        p
        (let [equivalents (case notation
                            :flat (filter flat? equivalent-pitches)
                            :natural (filter natural? equivalent-pitches)
                            :sharp (filter sharp? equivalent-pitches))]
          (if (= 1 (count equivalents))
            (first equivalents)
            p))))))

(defn note->midi [note]
  {:pre [(specs/note? note)]
   :post [(s/valid? ::specs/midi %)]}
  (let [{:keys [pitch octave]} (parts note)
        index (get specs/pitches pitch)]
    (+ index (* 12 (inc octave)))))

(defn midi->note [midi _key]
  {:pre [(s/valid? ::specs/midi midi)]
   :post [(s/valid? ::specs/note %)]}
  (let [octave (dec (int (/ midi 12)))
        index (mod midi 12)
        p (get specs/default-pitch-by-index index)]
    (keyword (str (name p) octave))))

(defn fold-notes
  "Fold notes into a 21 semitone range so the highest interval is a 13th"
  [notes]
  {:pre [(every? specs/note? notes)]}
  (let [notes->midis (zipmap notes (map note->midi notes))
        [_ low-midi] (apply min-key val notes->midis)
        [high-note high-midi] (apply max-key val notes->midis)]
    (if (<= (- high-midi low-midi) 21)
      notes
      (let [{:keys [pitch octave]} (parts high-note)
            new-note (keyword (str (name pitch) (dec octave)))]
        (fold-notes (vec (sort-by note->midi (set (replace {high-note new-note} notes)))))))))

(defn- pitch-semitone-distance
  "Semitone distance, preserving 12, but modulo 12 otherwise"
  [p1 p2]
  {:pre [(every? specs/pitch? [p1 p2])]}
  (inc (mod (dec (- (specs/pitches p2) (specs/pitches p1))) 12)))

(defn- note-semitone-distance
  "Semitone distance, preserving 12, but modulo 12 otherwise"
  [n1 n2]
  {:pre [(every? specs/note? [n1 n2])]}
  (abs (apply - (map note->midi (fold-notes [n1 n2])))))

(defn semitone-distance
  [x1 x2]
  (if (specs/pitch? x1)
    (pitch-semitone-distance x1 x2)
    (note-semitone-distance x1 x2)))

(defn ->interval
  [x1 x2]
  {:pre [(every? specs/pitch-or-note? [x1 x2])]
   :post [(specs/interval? %)]}
  (let [semitone-distance (semitone-distance x1 x2)
        matching-intervals (specs/intervals-by-semitone semitone-distance)]
    (if (= (count matching-intervals) 1)
      (first matching-intervals)
      (let [staff-distance (staff-distance x1 x2)]
        (first (filter #(or (string/includes? % (str staff-distance))
                            (string/includes? % (str (+ 7 staff-distance))))
                       matching-intervals))))))

(defn- letter+
  "Given a letter (as a capital character, like \\A) and an interval to move
   up, returns the resulting letter (A-G), ignoring accidentals.

   e.g. F + 1 == F (unison)
        F + 2 == G (2nd)
        F + 3 == A (3rd)
        F + 4 == B (4th)
        F + 8 == F (octave)

   If multiplier is -1, moves down instead of up.

   e.g. F - 1 == F (unison)
        F - 2 == E (2nd)
        F - 3 == D (3rd)
        F - 4 == C (4th)
        F - 8 == F (octave)"
  [letter interval & [multiplier]]
  {:pre [(char? letter)]}
  (let [letters (if (= multiplier -1) (reverse "ABCDEFG") "ABCDEFG")
        letters (drop-while (partial not= letter) (cycle letters))]
    (nth letters (dec interval))))

(defn clamp-pitch
  "Convert pitch with an extended accidental (more than two flats/sharps) to enharmonic equivalent with max 2 accidental symbols
  This is because we're not supporting triple/quadruple flats/sharps"
  [p]
  {:post [(specs/pitch? %)]}
  (let [extended-pitch-pattern #"^(([A-G])(b*|#*))$"
        [_ _ letter-str accidental] (re-find extended-pitch-pattern (name p))
        letter (first letter-str)
        multiplier (when (string/includes? accidental "b") -1)]
    (loop [letter letter
           accidental accidental]
      (let [new-pitch (keyword (str letter accidental))]
        (if (contains? specs/pitches new-pitch)
          new-pitch
          (recur (letter+ letter 2 multiplier) (subs accidental 2)))))))

(defn- pitch+interval
  [p interval & [multiplier]]
  {:pre [(specs/pitch? p) (specs/interval? interval)]
   :post [(specs/pitch? %)]}
  (if (some? (#{:P1 :P8} interval))
    p
    (let [{:keys [letter]} (parts p)
          interval-staff-distance (utils/parse-int interval)
          new-letter (letter+ letter interval-staff-distance multiplier)
          interval-semitone (get-in specs/intervals [interval ::specs/semitone])
          semitone (specs/pitches p)
          new-semitone ((if (= multiplier -1) - +) semitone interval-semitone)
          difference (* (or multiplier 1)
                        (mod (- new-semitone (specs/pitches (keyword (str new-letter)))) 12))
          new-difference (cond
                           (< difference -2) (+ difference 12)
                           (< 2 difference) (- difference 12)
                           :else difference)
          accidental-str (string/join "" (take (abs new-difference)
                                               (repeat (if (neg? new-difference)
                                                         (if (= multiplier -1) \# \b)
                                                         (if (= multiplier -1) \b \#)))))
          new-pitch (keyword (str new-letter accidental-str))]
      (clamp-pitch new-pitch))))

(defn- note+interval
  [n interval & [multiplier]]
  {:pre [(specs/note? n) (specs/interval? interval)]
   :post [(specs/note? %)]}
  (let [{:keys [pitch octave]} (parts n)
        semitone (specs/pitches pitch)
        interval-semitone (get-in specs/intervals [interval ::specs/semitone])
        new-pitch (pitch+interval pitch interval multiplier)
        new-pitch-str (name new-pitch)
        new-octave (+ octave (* (or multiplier 1) (math/floor-div (+ semitone interval-semitone) 12)))]
    (keyword (str new-pitch-str new-octave))))

(defn +interval
  [x interval & [multiplier]]
  {:pre [(specs/pitch-or-note? x)
         (specs/interval? interval)]
   :post [(specs/pitch-or-note? %)]}
  (if (= :P1 interval)
    x
    (if (specs/pitch? x)
      (pitch+interval x interval multiplier)
      (note+interval x interval multiplier))))

(defn resolve-shape
  [x shape-type shape-name]
  {:pre [(specs/pitch-or-note? x)]}
  (let [{:keys [pitch note]} (parts x)
        shape (get (if (= shape-type :chord) specs/chords specs/scales) shape-name)
        intervals (::specs/intervals shape)
        pitches (mapv (partial +interval pitch) intervals)]
    (cond-> shape
      true (merge #::specs{:pitch pitch
                           :name shape-name
                           :pitches pitches})
      (specs/note? x) (assoc ::specs/notes (mapv (partial +interval note) intervals)))))

(defn intervals->chord [intervals]
  (when (seq intervals)
    (let [interval-set (set intervals)]
      (specs/chords-by-intervals interval-set))))

(defn intervals->chords [intervals]
  (if (seq intervals)
    (let [interval-set (set intervals)]
      (into #{} (comp (filter (fn [[chord-interval-set _]] (clojure.set/subset? interval-set chord-interval-set)))
                      (map val))
            specs/chords-by-intervals))
    []))

(defn pitch->note
  [p & [octave]]
  (keyword (str (name p) (or octave 4))))

(defn pitches->notes
  [pitches]
  (loop [pitches pitches
         notes []
         octave 4]
    (if (seq pitches)
      (let [note (pitch->note (first pitches) octave)]
        (if (seq notes)
          (let [midi (note->midi note)
                last-note (last notes)
                last-midi (note->midi last-note)
                {:keys [pitch octave]} (parts note)]
            (if (< midi last-midi)
              (recur (rest pitches)
                     (conj notes (keyword (str (name pitch) (inc octave))))
                     (inc octave))
              (recur (rest pitches) (conj notes note) octave)))
          (recur (rest pitches) (conj notes note) octave)))
      notes)))

(defn scale-chords-exact
  [scale & {:keys [num-thirds]}]
  (let [{start-pitch ::specs/pitch scale-name ::specs/name} scale
        scale (specs/scales scale-name)
        scale-intervals (::specs/intervals scale)
        scale-pitches (map (partial +interval start-pitch) scale-intervals)]
    (loop [scale-pitches scale-pitches
           num (count scale-pitches)
           chords []]
      (if (zero? num)
        chords
        (let [pitches (take num-thirds (take-nth 2 (cycle scale-pitches)))
              notes (pitches->notes pitches)
              intervals (conj (rest (map (partial ->interval (first notes)) notes)) :P1)
              chord (specs/chords-by-intervals (set intervals))]
          (recur (utils/rotate scale-pitches) (dec num) (conj chords [chord])))))))

(defn scale-chords
  [scale & {:keys [exact? num-thirds] :or {exact? false num-thirds 4}}]
  (if exact?
    (scale-chords-exact scale :num-thirds num-thirds)
    (let [{:keys [::specs/pitch ::specs/name]} scale
          scale (specs/scales name)
          scale-intervals (::specs/intervals scale)
          scale-pitches (set (map (partial +interval pitch) scale-intervals))]
      (mapv (fn [interval]
              (let [pitch (+interval pitch interval)]
                (mapv first (filter
                             (fn [[_ {chord-intervals ::specs/intervals}]]
                               (let [chord-pitches (set (map (partial +interval pitch) chord-intervals))]
                                 ((if exact? utils/perfect-set? clojure.set/subset?) chord-pitches scale-pitches)))
                             specs/chords))))
            scale-intervals))))

(comment
  (let [scale (resolve-shape :E :scale :harmonic-minor)
        {pitches ::specs/pitches chord-lists :chords} (assoc scale :chords (scale-chords scale :exact? true :num-thirds 4))
        chords (map first chord-lists)]))

(defn scale->mode
  [scale n]
  (let [pitches (utils/rotate (::specs/pitches scale) (dec n))
        intervals (into [:P1] (map #(->interval (first pitches) %)) (rest pitches))]
    (when-let [new-scale-name (get specs/scales-by-intervals intervals)]
      (resolve-shape (first pitches) :scale new-scale-name))))

(defn interval->degree [interval]
  {:pre [(specs/interval? interval)]}
  (let [major-intervals (get-in specs/scales [:major ::specs/intervals])
        matching-idx (.indexOf major-intervals interval)]
    (keyword
     (if (neg? matching-idx)
       (str (if (lesser? (name interval)) "b" "#") (last (name interval)))
       (str (inc matching-idx))))))

;; Used for generating initial scale degrees
; (defn- scales-with-degrees []
;   (let [major-intervals (get-in specs/scales [:major ::specs/intervals])]
;     (map (fn [[scale-name details]]
;            (let [{intervals ::specs/intervals} details
;                  degrees (mapv interval->degree intervals)]
;              {scale-name (assoc details :degrees degrees)})) specs/scales)))

(defn roman-numeral
  [n]
  (nth ["I" "II" "III" "IV" "V" "VI" "VII"] (dec n)))

(defn degree-chord->roman-numeral
  [degree chord-name]
  (let [intervals (::specs/intervals (specs/chords chord-name))
        major? (utils/in? intervals :M3)
        degree-str (name degree)
        accidental (if (< 1 (count degree-str)) (first degree-str) "")
        degree-num (utils/parse-int degree-str)
        roman-num (roman-numeral degree-num)]
    (keyword
     (str
      accidental
      ((if major? string/upper-case string/lower-case) roman-num)
      (cond
        (utils/in? intervals :A5) "+"
        (utils/in? intervals :d5) "°"
        :else "")))))

;; TODO
;;  - Chord progressions
;;  - Find scale+degree+roman numeral from just the chord

;; TODO move to search.clj
(defn find-chord [xs])
(defn find-scale [xs])
