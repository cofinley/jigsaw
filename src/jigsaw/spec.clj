(ns jigsaw.spec
  (:gen-class)
  (:require [clojure.spec.alpha :as s]))

;; Semitone: 0, 1, .., 21 (21 == thirteenth)
(s/def ::semitone #(and (int? %) (<= 0 % 21)))
(s/def ::semitones (s/coll-of ::semitone :distinct true))  ; when used for a chord

;; Pitch (class): C, C#, Db, etc.
;;   Has different representations (e.g. C#, Db) depending on preference (and relation to tonic, if in a scale, e.g. Gbb)
(def pitches
  #{:C :B#
    :C# :Db
    :D
    :D# :Eb
    :E
    :E# :F
    :F# :Gb
    :G
    :G# :Ab
    :A
    :A# :Bb
    :B :Cb})
(s/def ::pitch #(and (keyword? %) (contains? pitches %))) ; pitch in isolation or root (chord) or tonic (scale)
;; Derived: enharmonic spelling (e.g. C# vs. Db)

;; Interval: 1, m3, M3, A5, d5, 5, etc.
;;   Distance between two pitches
;;   Can be represented by semitones
;;   Can be arabic (m3), roman (iii) numerals
(def intervals
  {:1   {::name "Root" ::semitone 0}
   :d2  {::name "Diminished 2nd" ::semitone 0}
   :m2  {::name "Minor 2nd" ::semitone 1}
   :M2  {::name "Major 2nd" ::semitone 2}
   :d3  {::name "Diminished 3rd" ::semitone 2}
   :m3  {::name "Minor 3rd" ::semitone 3}
   :A2  {::name "Augmented 2nd" ::semitone 3}
   :M3  {::name "Major 3rd" ::semitone 4}
   :d4  {::name "Diminished 4th" ::semitone 4}
   :4   {::name "Perfect 4th" ::semitone 5}
   :A3  {::name "Augmented 3rd" ::semitone 5}
   :d5  {::name "Diminished 5th" ::semitone 6}
   :A4  {::name "Augmented 4th" ::semitone 6}
   :TT  {::name "Tritone" ::semitone 6}
   :5   {::name "Perfect 5th" ::semitone 7}
   :d6  {::name "Diminished 6th" ::semitone 7}
   :m6  {::name "Minor 6th" ::semitone 8}
   :A5  {::name "Augmented 5th" ::semitone 8}
   :M6  {::name "Major 6th" ::semitone 9}
   :d7  {::name "Diminished 7th" ::semitone 9}
   :m7  {::name "Minor 7th" ::semitone 10}
   :A6  {::name "Augmented 6th" ::semitone 10}
   :M7  {::name "Major 7th" ::semitone 11}
   :8   {::name "Octave" ::semitone 12}
   :m9  {::name "Minor 9th" ::semitone 13}
   :M9  {::name "Major 9th" ::semitone 14}
   :m10 {::name "Minor 10th" ::semitone 15}
   :A9  {::name "Augmented 9th" ::semitone 15}
   :M10 {::name "Major 10th" ::semitone 16}
   :d11 {::name "Diminished 11th" ::semitone 16}
   :11  {::name "Perfect 11th" ::semitone 17}
   :A11 {::name "Augmented 11th" ::semitone 18}
   :12  {::name "Perfect 12th" ::semitone 19}
   :m13 {::name "Minor 13th" ::semitone 20}
   :M13 {::name "Major 13th" ::semitone 21}})
(s/def ::interval #(and (keyword? %) (contains? intervals %)))
(s/def ::intervals (s/coll-of ::intervals))  ; Can be one (in isolation) or more (e.g. chords, scales)

(s/def ::name string?)
(s/def ::aliases (s/coll-of string?))

;; Chord and scales are composition of pitch, name, intervals
;;  e.g. a pitch with intervals is a chord or a scale (think ECS)
;;    maybe use degrees instead of intervals for scale to be able to differentiate

;; Derived: (scale) degree(s), inversions (based on notes and chord intervals)
;; Degree: I, II, III,, bIII, V, #V, VII, etc.
;;   Can be represented by semitones
;;   Can be named as dominant, subdominant, etc.
;;   Can be arabic (5, 6), roman numerals (V, VI)
;;   Can be flattened/sharpened (e.g. a mode formula relative to the base scale), but minor/major/aug/dim not applicable, that's for chords ('quality'), the degree is just the relative pitch
(def degrees
  #{:1
    :b2 :2 :#2
    :b3 :3 :#3
    :b4 :4 :#4
    :b5 :5 :#5
    :b6 :6 :#6
    :b7 :7 :#7
    :b8 :8 :#8
    :b9 :9 :#9
    :b10 :10 :#10
    :b11 :11 :#11
    :b12 :12 :#12})
(s/def ::degree #(and (keyword? %) (contains? degrees %)))
(s/def ::inversion #(and (int? %) (<= 1 % 6))) ; 1st up to 6th chord inversion (e.g. 13th chord)

;; Note: position of a pitch+octave on the keyboard
;;   Represented as an integer
;;   Applicable only to piano
(s/def ::note #(and (int? %) (<= 0 % 127)))

;; Derived
;; Chord: Maj, Maj7, min7, minMaj7 
;;   Made up of root pitch (which will have a scale degree, when figured out (e.g. I, IV)) and intervals (relative to the root)
;;      Cannot rely solely on semitones since more than one interval can share the same amount of semitones
;;   Has positions (root)/can be inverted (first, second inversion) when root not lowest note
;; Scale: Maj, min
;;   Absolute distances: degrees
;;   Relative distances: intervals
;;      Can rely solely on semitones since scales are about steps
;;   Has modes, which are similar to inversions (same intervals as base scale, new tonic)
;;   Tonic (1) is the key
;;   Chords can be derived from a scale
