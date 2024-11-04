(ns jigsaw.algo
  (:require [jigsaw.spec :as specs]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]))

(defn pitch->letter
  [p]
  {:pre [(s/valid? ::specs/pitch p)]}
  (first (name p)))

(defn staff-distance
  [p1 p2]
  {:pre [(s/valid? ::specs/pitch p1) (s/valid? ::specs/pitch p2)]}
  (let [letter1 (pitch->letter p1)
        letter2 (pitch->letter p2)
        i1 (int letter1)
        i2 (int letter2)]
    (inc (mod (- i2 i1) 7))))

(defn get-cyclic-distance [a b len]
  (let [distance (mod (- b a) len)
        reverse-distance (mod (- a b) len)]
    (min distance reverse-distance)))

(defn semitone-distance
  [p1 p2]
  {:pre [(s/valid? ::specs/pitch p1) (s/valid? ::specs/pitch p2)]}
  (mod (- (get specs/pitches p2) (get specs/pitches p1)) 12))

(defn lesser? [s]
  (any? (map #(string/includes? s %) ["d" "m"])))
(defn greater? [s]
  (any? (map #(string/includes? s %) ["A" "M"])))
(defn altered? [s] (or (lesser? s) (greater? s)))

(defn pitches->interval
  [p1 p2]
  {:pre [(s/valid? ::specs/pitch p1) (s/valid? ::specs/pitch p2)]
   :post [(s/valid? ::specs/interval %)]}
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

(def pitch-pattern #"^([A-G])([#b]?)$")
(defn get-pitch-regex-groups [p]
  (rest (re-matches pitch-pattern (name p))))
(defn accidental-match? [accidental-string]
  (fn [p]
    (let [[_ accidental] (get-pitch-regex-groups p)]
      (= accidental accidental-string))))
(def flat? (accidental-match? "b"))
(def natural? (accidental-match? ""))
(def sharp? (accidental-match? "#"))

(defn enharmonic
  [p notation]
  {:pre [(s/valid? ::specs/pitch p)]
   :post [(s/valid? ::specs/pitch %)]}
  (let [index (get specs/pitches p)
        equivalent-pitches (specs/pitches-by-index index)]
    (if (= 1 (count equivalent-pitches))
      p
      (let [equivalents (case notation
                          :flat (filter flat? equivalent-pitches)
                          :sharp (filter sharp? equivalent-pitches))]
        (if (= 1 (count equivalents))
          (first equivalents)
          p)))))

(defn note->midi
  [note]
  {:pre [(s/valid? ::specs/note note)]
   :post [(s/valid? ::specs/midi %)]}
  (let [[p octave] (specs/note-parts note)
        index (get specs/pitches p)]
    (+ index (* 12 (inc octave)))))

(defn midi->note
  [midi]
  {:pre [(s/valid? ::specs/midi midi)]
   :post [(s/valid? ::specs/note %)]}
  (let [octave (dec (int (/ midi 12)))
        index (mod midi 12)
        p (get specs/default-pitch-by-index index)]
    (keyword (str (name p) octave))))
