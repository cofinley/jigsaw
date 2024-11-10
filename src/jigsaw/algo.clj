(ns jigsaw.algo
  (:require [jigsaw.spec :as specs]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.math :as math]))

(defn in?
  "Returns true if v in coll, else false."
  [coll v]
  (some? (some #(= v %) coll)))

(defn- pitch-parts
  [p]
  {:pre [(specs/pitch? p)]}
  (let [[_ pitch-str letter accidental] (re-find specs/pitch-pattern (name p))]
    {:pitch (keyword pitch-str)
     :letter (first letter)
     :accidental accidental}))

(defn- note-parts
  [n]
  {:pre [(specs/note? n)]}
  (let [note-str (name n)
        pitch (keyword (subs note-str 0 (dec (count note-str))))
        octave (Integer/parseInt (str (last note-str)))]
    (assoc (pitch-parts pitch) :octave octave)))

(defn parts
  [x]
  {:pre [(specs/pitch-or-note? x)]}
  (if (specs/pitch? x)
    (pitch-parts x)
    (note-parts x)))

(defn- staff-distance
  [p1 p2]
  {:pre [(specs/pitch? p1) (specs/pitch? p2)]}
  (let [{letter1 :letter} (parts p1)
        {letter2 :letter} (parts p2)
        i1 (int letter1)
        i2 (int letter2)]
    (inc (mod (- i2 i1) 7))))

(defn- get-cyclic-distance [a b len]
  (let [distance (mod (- b a) len)
        reverse-distance (mod (- a b) len)]
    (min distance reverse-distance)))

(defn- semitone-distance
  [p1 p2]
  {:pre [(specs/pitch? p1) (specs/pitch? p2)]}
  (mod (- (get specs/pitches p2) (get specs/pitches p1)) 12))

(defn- lesser? [s] (any? (map #(string/includes? s %) ["d" "m"])))
(defn- greater? [s] (any? (map #(string/includes? s %) ["A" "M"])))
(defn- altered? [s] (or (lesser? s) (greater? s)))

(defn pitches->interval
  [p1 p2]
  {:pre [(specs/pitch? p1) (specs/pitch? p2)]
   :post [(specs/interval? %)]}
  (let [semitone-distance (semitone-distance p1 p2)
        matching-intervals (specs/intervals-by-semitone semitone-distance)]
    (if (= (count matching-intervals) 1)
      (first matching-intervals)
      (let [p2-accidental (second (name p2))
            altered-intervals (filter
                               (fn [interval]
                                 (altered? (name interval))) matching-intervals)
            unaltered-intervals (remove
                                 (fn [interval]
                                   (altered? (name interval))) matching-intervals)]
        (if (= "" p2-accidental)
          (first unaltered-intervals)
          (let [staff-distance (staff-distance p1 p2)]
            (first (filter #(string/includes? % (str staff-distance)) altered-intervals))))))))

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
  (let [index (specs/pitches p)
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

(defn- parse-int [x]
  (when-some [int-str (re-find #"\d+" (str x))]
    (Integer/parseInt int-str)))

;; TODO: multi-method
(defn note->midi [note]
  {:pre [(s/valid? ::specs/note note)]
   :post [(s/valid? ::specs/midi %)]}
  (let [octave (parse-int (last (name note)))
        p (keyword (string/join "" (butlast (name note))))
        index (get specs/pitches p)]
    (+ index (* 12 (inc octave)))))

;; TODO: multi-method
(defn midi->note [midi]
  {:pre [(s/valid? ::specs/midi midi)]
   :post [(s/valid? ::specs/note %)]}
  (let [octave (dec (int (/ midi 12)))
        index (mod midi 12)
        p (get specs/default-pitch-by-index index)]
    (keyword (str (name p) octave))))

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
  (if (or (= interval :P1) (= interval :P8))
    p
    (let [{:keys [letter]} (parts p)
          interval-staff-distance (parse-int interval)
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
  (let [{:keys [pitch octave]} (note-parts n)
        semitone (specs/pitches pitch)
        interval-semitone (get-in specs/intervals [interval ::specs/semitone])
        new-pitch (pitch+interval pitch interval multiplier)
        new-pitch-str (name new-pitch)
        ;; TODO: don't change octave if letter on octave boundary (B# going up, Cb going down)
        new-octave (+ octave (* (or multiplier 1) (math/floor-div (+ semitone interval-semitone) 12)))]
    (keyword (str new-pitch-str new-octave))))

(defn +interval
  [x interval & [multiplier]]
  {:pre [(specs/pitch-or-note? x)
         (specs/interval? interval)]
   :post [(specs/pitch-or-note? %)]}
  (if (specs/pitch? x)
    (pitch+interval x interval multiplier)
    (note+interval x interval multiplier)))

(defn +intervals
  [x intervals & [multiplier]]
  {:pre [(specs/pitch-or-note? x)
         (every? #(specs/interval? %) intervals)]
   :post [(or (s/valid? (s/coll-of ::specs/pitch) %)
              (s/valid? (s/coll-of ::specs/note) %))]}
  (map #(+interval x % multiplier) intervals))

(defn resolve-chord
  [x chord-name]
  {:pre [(specs/pitch-or-note? x)]}
  (let [{:keys [pitch]} (parts x)
        chord (specs/chords chord-name)
        interval-mapping (reduce (fn [m interval]
                                   (assoc m interval (+interval x interval)))
                                 {} (::specs/intervals chord))]
    #::specs{:name chord-name
             :root pitch
             :interval-mapping interval-mapping}))

(defn resolve-scale
  [x scale-name]
  {:pre [(specs/pitch-or-note? x)]}
  (let [{:keys [pitch]} (parts x)
        scale (specs/scales scale-name)
        interval-mapping (reduce (fn [m interval]
                                   (assoc m interval (+interval x interval)))
                                 {} (::specs/intervals scale))]
    #::specs{:tonic pitch
             :name scale-name
             :interval-mapping interval-mapping}))
