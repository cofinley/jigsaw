(ns jigsaw.algo
  (:require [jigsaw.spec :as specs]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.math :as math]))

; TODO: break out in specs/pitch-parts
(defn- pitch->letter
  [p]
  {:pre [(s/valid? ::specs/pitch p)]}
  (first (name p)))

(defn- staff-distance
  [p1 p2]
  {:pre [(s/valid? ::specs/pitch p1) (s/valid? ::specs/pitch p2)]}
  (let [letter1 (pitch->letter p1)
        letter2 (pitch->letter p2)
        i1 (int letter1)
        i2 (int letter2)]
    (inc (mod (- i2 i1) 7))))

(defn- get-cyclic-distance [a b len]
  (let [distance (mod (- b a) len)
        reverse-distance (mod (- a b) len)]
    (min distance reverse-distance)))

(defn- semitone-distance
  [p1 p2]
  {:pre [(s/valid? ::specs/pitch p1) (s/valid? ::specs/pitch p2)]}
  (mod (- (get specs/pitches p2) (get specs/pitches p1)) 12))

(defn- lesser? [s] (any? (map #(string/includes? s %) ["d" "m"])))
(defn- greater? [s] (any? (map #(string/includes? s %) ["A" "M"])))
(defn- altered? [s] (or (lesser? s) (greater? s)))

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

(defn- get-pitch-regex-groups [p]
  (rest (re-matches specs/pitch-pattern (name p))))
(defn- accidental-match? [accidental-string]
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

(defn parse-int [x]
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

(defn letter+
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

(defn pitch+interval
  [p interval & [multiplier]]
  {:pre [(s/valid? ::specs/pitch p) (s/valid? ::specs/interval interval)]
   :post [(s/valid? ::specs/pitch %)]}
  (if (or (= interval :P1) (= interval :P8))
    p
    (let [letter (pitch->letter p)
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
                                                         (if (= multiplier -1) \b \#)))))]
      (keyword (str new-letter accidental-str)))))

(defn note+interval
  [n interval & [multiplier]]
  {:pre [(s/valid? ::specs/note n) (s/valid? ::specs/interval interval)]
   :post [(s/valid? ::specs/note %)]}
  (let [{:keys [pitch octave]} (specs/note-parts n)
        interval-semitone (get-in specs/intervals [interval ::specs/semitone])
        new-pitch (pitch+interval pitch interval multiplier)
        new-pitch-str (name new-pitch)
        new-octave (+ octave (* (or multiplier 1) (math/floor-div interval-semitone 12)))]
    (keyword (str new-pitch-str new-octave))))

(defn resolve-intervals
  [x intervals]
  {:pre [(or (s/valid? ::specs/pitch x) (s/valid? ::specs/note x))
         (every? #(s/valid? ::specs/interval %) intervals)]
   :post [(or (s/valid? (s/coll-of ::specs/pitch) %)
              (s/valid? (s/coll-of ::specs/note) %))]}
  (cond
    (s/valid? ::specs/pitch x) (map #(pitch+interval x %) intervals)
    (s/valid? ::specs/note x) (map #(note+interval x %) intervals)
    :else nil))
