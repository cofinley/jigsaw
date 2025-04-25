(ns jigsaw.algo
  (:require
   [clojure.string :as string]
   [clojure.set]
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

(defn pitch->note
  [p & [octave]]
  (keyword (str (name p) (or octave 4))))

(defn- staff-distance
  [x1 x2]
  {:pre [(every? specs/pitch-or-note? [x1 x2])]}
  (let [{letter1 :letter} (parts x1)
        {letter2 :letter} (parts x2)
        i1 (#?(:clj int :cljs .charCodeAt) letter1)
        i2 (#?(:clj int :cljs .charCodeAt) letter2)]
    (inc (mod (- i2 i1) 7))))

(defn- lesser? [s] (some (partial string/includes? s) ["d" "m"]))

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
  (let [chroma (specs/pitches p)
        equivalent-pitches (specs/chroma->pitches chroma)]
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
   :post [(specs/midi? %)]}
  (let [{:keys [pitch octave letter]} (parts note)
        chroma (get specs/pitches pitch)
        base-chroma (specs/letters->chroma letter)  ; Chroma without accidentals
        new-octave (cond  ; Adjust for crossing octave boundary
                     (and (< chroma base-chroma) (not (string/includes? (name pitch) "b"))) (inc octave)  ; E.g. B#4 (0 < 11), only for sharps
                     (and (> chroma base-chroma) (not (string/includes? (name pitch) "#"))) (dec octave)  ; E.g. Cb (11 > 0), only for flats
                     :else octave)]
    (+ chroma (* 12 (inc new-octave)))))

(defn midi->note
  "Convert midi integer to note, optionally specifying the target pitch (otherwise uses default flats/sharps)"
  [midi & [pitch]]
  {:pre [(specs/midi? midi)]
   :post [(specs/note? %)]}
  (let [octave (dec (quot midi 12))
        chroma (mod midi 12)
        p (or pitch (get specs/chroma->default-pitch chroma))
        {:keys [letter]} (parts p)
        base-chroma (specs/letters->chroma letter)  ; Chroma without accidentals
        new-octave (cond  ; Adjust for crossing octave boundary
                     (and (< chroma base-chroma) (not (string/includes? (name p) "b"))) (dec octave)  ; E.g. B#4 (0 < 11), only for sharps
                     (and (> chroma base-chroma) (not (string/includes? (name p) "#"))) (inc octave)  ; E.g. Cb (11 > 0), only for flats
                     :else octave)]
    (keyword (str (name p) new-octave))))

(defn fold-notes
  "Fold notes into a 21 semitone range so the highest interval is a 13th (by default)"
  [notes & {:keys [max-semitones] :or {max-semitones 21}}]
  {:pre [(every? specs/note? notes)]}
  (let [notes->midis (zipmap notes (map note->midi notes))
        [_ low-midi] (apply min-key val notes->midis)
        [high-note high-midi] (apply max-key val notes->midis)]
    (if (<= (- high-midi low-midi) max-semitones)
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
  [n1 n2 & {:keys [fold?] :or {fold? false}}]
  {:pre [(every? specs/note? [n1 n2])]}
  (abs (apply - (map note->midi (if fold? (fold-notes [n1 n2]) [n1 n2])))))

(defn semitone-distance
  [x1 x2 & {:keys [fold?] :or {fold? false}}]
  (if (specs/pitch? x1)
    (pitch-semitone-distance x1 x2)
    (note-semitone-distance x1 x2 :fold? fold?)))

(defn ->interval
  "Find interval between two pitches/notes
   Start with semitone distance, and use staff distance if needed to split hairs between augmented/diminished"
  [x1 x2]
  {:pre [(every? specs/pitch-or-note? [x1 x2])]
   :post [(or (specs/interval? %) (nil? %))]}
  (if (and (specs/note? x1) (< (note->midi x2) (note->midi x1)))
    (let [{:keys [pitch octave]} (parts x2)]
      (->interval x1 (pitch->note pitch (inc octave))))
    (let [semitone-distance (semitone-distance x1 x2 :fold? true)
          matching-intervals (specs/semitones->intervals semitone-distance)]
      (if (= (count matching-intervals) 1)
        (first matching-intervals)
        (let [distance (staff-distance x1 x2)]
          (first (filter #(or (string/includes? (name %) (str distance))
                              (string/includes? (name %) (str (+ 7 distance))))
                         matching-intervals)))))))

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
  (if (some? (#{:P1 :P8} interval))
    p
    (let [{:keys [letter]} (parts p)
          interval-staff-distance (utils/parse-int interval)
          new-letter (letter+ letter interval-staff-distance multiplier)
          interval-semitones (get-in specs/intervals [interval :semitones])
          chroma (specs/pitches p)
          new-semitones ((if (= multiplier -1) - +) chroma interval-semitones)
          difference (* (or multiplier 1)
                        (mod (- new-semitones (specs/pitches (keyword (str new-letter)))) 12))
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
  (let [{:keys [pitch]} (parts n)
        interval-semitones (get-in specs/intervals [interval :semitones])
        new-pitch (pitch+interval pitch interval multiplier)]
    (-> n
        (note->midi)
        (+ (* (or multiplier 1) interval-semitones))
        (midi->note new-pitch))))

(defn +interval
  "Add/subtract interval to/from pitch or note"
  [x interval & [multiplier]]
  {:pre [(specs/pitch-or-note? x)
         (specs/interval? interval)]
   :post [(specs/pitch-or-note? %)]}
  (if (= :P1 interval)
    x
    (if (specs/pitch? x)
      (pitch+interval x interval multiplier)
      (note+interval x interval multiplier))))

(defn ->shape
  "Given a starting pitch/note and a shape definition, derive the rest of the shape (e.g. pitches, intervals, degrees, notes (if x is a note))"
  ([x]
   ; Different notations
   (cond
     ; E.g. :Cmaj
     (keyword? x) (let [pattern (re-pattern (str "^" specs/pitch-pattern-str "([a-z0-9-]+)" "$"))
                        [_ pitch-str _ _ shape-name-str] (re-find pattern (name x))
                        pitch (keyword pitch-str)
                        shape-name (keyword shape-name-str)]
                    (->shape pitch shape-name))
     ; E.g. {:pitch :C :name :maj}
     (specs/shape-ref? x) (if (or (contains? x :pitches) (contains? x :notes))
                            x
                            (->shape (or (:note x) (:pitch x)) (:name x)))))
  ([x shape-name]
   {:pre [(specs/pitch-or-note? x)]}
   (let [{:keys [pitch note]} (parts x)
         shape (get specs/name->shape shape-name)
         intervals (:intervals shape)
         pitches (mapv (partial +interval pitch) intervals)]
     (cond-> shape
       true (merge {:pitch pitch
                    :name shape-name
                    :pitches pitches})
       (specs/note? x) (assoc :notes (mapv (partial +interval note) intervals))))))

(defn pitches->notes
  "Convert one or more pitches to notes, incrementing octaves as needed"
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

(defn ->intervals
  "Convert pitches to intervals, where the first pitch is :P1"
  [xs]
  {:pre [(every? specs/pitch-or-note? xs)]
   :post [(every? specs/interval? %)]}
  (if (specs/pitch? (first xs))
    (->intervals (pitches->notes xs))
    (map (partial ->interval (first xs)) xs)))

(defn interval->degree [interval]
  {:pre [(specs/interval? interval)]}
  (let [major-intervals (get-in specs/scales [:major :intervals])
        matching-idx (.indexOf major-intervals interval)]
    (keyword
     (if (neg? matching-idx)
       (str (if (lesser? (name interval)) "b" "#") (last (name interval)))
       (str (inc matching-idx))))))

;; Used for generating initial scale degrees
; (defn- scales-with-degrees []
;   (let [major-intervals (get-in specs/scales [:major :intervals])]
;     (map (fn [[scale-name details]]
;            (let [{intervals :intervals} details
;                  degrees (mapv interval->degree intervals)]
;              {scale-name (assoc details :degrees degrees)})) specs/scales)))

(defn roman-numeral
  [n]
  (nth ["I" "II" "III" "IV" "V" "VI" "VII"] (dec n)))

(defn roman-numeral->int
  [numeral-string]
  (let [m (into {}
                (map-indexed (fn [idx numeral]
                               [numeral (inc idx)])
                             ["I" "II" "III" "IV" "V" "VI" "VII"]))]
    (second (first (filter
                    #(= (string/upper-case
                         (-> numeral-string
                             (string/replace  "b" "")
                             (string/replace  "#" "")
                             (string/replace  "°" "")
                             (string/replace  "+" "")
                             (string/replace  "7" ""))) (first %)) m)))))

(defn degree-chord->roman-numeral
  [degree chord-name]
  (let [intervals (:intervals (specs/chords chord-name))
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
        (= :7 chord-name) "7"
        :else "")))))

;; TODO
;;  - Chord progressions/cadences (i.e. shape of shapes)
;;  - Preview scales on top of chord (progression)
;;    - With different licks/melody rhythm patterns
;;  - Handle list views/multiplexing the node views
;;  - Circle of fifths view
;;  - Key signature, proper accidentals on music staff
;;  - slash chords
;;  - voicings/inversions/closest voicing

(defn- circle-of-fifths [major-or-minor]
  (zipmap
   (take 15 (iterate (partial #(+interval % :P5))
                     (case major-or-minor
                       :major :Cb
                       :minor :Ab)))
   (range -7 8)))

(defn key-signature [pitch major-or-minor]
  (let [n ((circle-of-fifths major-or-minor) pitch)]
    (if (pos? n)
      (map (comp keyword #(str % "#")) (set (take n "FCGDAEB")))
      (map (comp keyword #(str % "b")) (set (take (Math/abs n) "BEADGCF"))))))

(comment
  (take 3 (cycle '(:G :A)))
  (utils/rotate [:G :A :C :F] 3)
  (->> :F
       (iterate (partial #(+interval % :P5)))  ; Fifths
       (take 7))
  (circle-of-fifths :major)
  (key-signature :B :minor))
