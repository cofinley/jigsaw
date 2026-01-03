(ns jigsaw.theory
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [jigsaw.utils :as utils]))

; Specifications, music theory constants

;; Pitch class index (PCI): semitones cycling in one octave, where :C is 0, :C# is 1, :Db is 1, :B is 11, and B# is 0
(s/def ::pci (s/and int? #(<= 0 % 11)))

;; Semitone: 0, 1, .., 21 (21 == thirteenth)
(s/def ::semitones (s/and int? #(<= 0 % 21)))

(def letters->pci {\C 0 \D 2 \E 4 \F 5 \G 7 \A 9 \B 11})

;; Pitch: C, C#, Db, etc.
;;   Has different enharmonic representations (e.g. C#, Db) depending on preference (and relation to tonic, if in a scale, e.g. Gbb)
;;   Maps to absolute/fixed do (AKA abdo, i.e. starting at C or "do" in solfege) integer 
;;      https://en.wikipedia.org/wiki/Solf%C3%A8ge#Chromatic_variants
(def pitches
  (reduce-kv
   (fn [m letter pci]
     (assoc
      m
      (keyword (str letter "bb")) (mod (- pci 2) 12)   ; Double-flat
      (keyword (str letter "b")) (mod (- pci 1) 12)    ; Flat
      (keyword (str letter)) pci                       ; Natural
      (keyword (str letter "#")) (mod (+ pci 1) 12)    ; Sharp
      (keyword (str letter "##")) (mod (+ pci 2) 12))) ; Double-sharp
   {}
   letters->pci))

(def pitch-pattern-str "(([A-G])(b{0,2}|#{0,2}))")
(def pitch-pattern (re-pattern (str "^" pitch-pattern-str "$")))
(s/def ::pitch (s/and keyword? #(re-find pitch-pattern (name %)))) ; pitch in isolation or root (chord) or tonic (scale)
(defn pitch? [p] (s/valid? ::pitch p))

(def simple-pitch-keys
  (filter #(and (not (str/includes? (name %) "bb"))
                (not (str/includes? (name %) "##")))
          (keys (sort-by val < pitches))))

(def pci->pitches
  (reduce-kv (fn [m pitch pci] (update m pci conj pitch)) {} pitches))

(def pci->default-pitch
  {0 :C
   1 :C#
   2 :D
   3 :Eb
   4 :E
   5 :F
   6 :F#
   7 :G
   8 :Ab
   9 :A
   10 :Bb
   11 :B})

;; Interval: 1, m3, M3, A5, d5, 5, etc.
;;   Distance between two pitches
;;   Can be represented by, but not constructed from semitones
;;      Must know the start and end pitches/staff positions
;;        e.g. C->F# is an augmented 4th (C->F + 1), and C->Gb is diminished (C->G - 1)
;;   Can be arabic (m3), roman (iii) numerals
;;   4th usually on major and sus chords, 11th on dominant and minor chords
;;   6th usually on major and minor chords, 13th usually on dominant chords
(def intervals
  {:P1  {:name "Root" :semitones 0}
   :d2  {:name "Diminished 2nd" :semitones 0}
   :m2  {:name "Minor 2nd" :semitones 1}
   :M2  {:name "Major 2nd" :semitones 2}
   :d3  {:name "Diminished 3rd" :semitones 2}
   :m3  {:name "Minor 3rd" :semitones 3}
   :A2  {:name "Augmented 2nd" :semitones 3}
   :M3  {:name "Major 3rd" :semitones 4}
   :d4  {:name "Diminished 4th" :semitones 4}
   :P4  {:name "Perfect 4th" :semitones 5}
   :A3  {:name "Augmented 3rd" :semitones 5}
   :d5  {:name "Diminished 5th" :semitones 6 :aliases ["Tritone"]}
   :A4  {:name "Augmented 4th" :semitones 6 :aliases ["Tritone"]}
   :P5  {:name "Perfect 5th" :semitones 7}
   :d6  {:name "Diminished 6th" :semitones 7}
   :m6  {:name "Minor 6th" :semitones 8}
   :A5  {:name "Augmented 5th" :semitones 8}
   :M6  {:name "Major 6th" :semitones 9}
   :d7  {:name "Diminished 7th" :semitones 9}
   :m7  {:name "Minor 7th" :semitones 10}
   :A6  {:name "Augmented 6th" :semitones 10}
   :M7  {:name "Major 7th" :semitones 11}
   :A7  {:name "Augmented 7th" :semitones 12}
   :P8  {:name "Octave" :semitones 12}
   :A8  {:name "Augmented 8th" :semitones 13}
   :m9  {:name "Minor 9th" :semitones 13}
   :M9  {:name "Major 9th" :semitones 14}
   :m10 {:name "Minor 10th" :semitones 15}
   :A9  {:name "Augmented 9th" :semitones 15}
   :M10 {:name "Major 10th" :semitones 16}
   :d11 {:name "Diminished 11th" :semitones 16}
   :P11 {:name "Perfect 11th" :semitones 17}
   :A11 {:name "Augmented 11th" :semitones 18}
   :P12 {:name "Perfect 12th" :semitones 19}
   :m13 {:name "Minor 13th" :semitones 20}
   :M13 {:name "Major 13th" :semitones 21}})
(s/def ::interval (set (keys intervals)))
(defn interval? [interval] (s/valid? ::interval interval))
(s/def ::intervals (s/coll-of ::interval))  ; Can be one (in isolation) or more (e.g. chords, scales)

(def semitones->intervals
  (reduce-kv (fn [m interval {:keys [semitones]}] (update m semitones conj interval)) {} intervals))

;; Chord and scales are composition of pitch, name, intervals
;;  e.g. a pitch with intervals is a chord or a scale (think ECS)
;;    maybe use degrees instead of intervals for scale to be able to differentiate

;; Note: pitch+octave
(def note-pattern-str (str pitch-pattern-str "(\\d{1})"))
(def note-pattern (re-pattern (str "^" note-pattern-str "$")))
(def pitch-or-note-pattern (re-pattern (str "^" note-pattern-str "?" "$")))
(s/def ::note (s/and keyword? #(re-find note-pattern (name %))))
(defn note? [n] (s/valid? ::note n))
(s/def ::pitch-or-note (s/or :pitch pitch? :note note?))
(defn pitch-or-note? [x] (s/valid? ::pitch-or-note x))

;; Midi: position of a note on the keyboard
;;   Represented as an integer
;;   Probably applicable only to piano?
(s/def ::midi (s/and int? #(<= 0 % 127)))
(defn midi? [x] (s/valid? ::midi x))

; TODO: namespace with chord/
(def chords
  (array-map
   ;; Major
   :maj        {:intervals [:P1 :M3 :P5]                    :aliases ["M", "major"]}
   :maj7       {:intervals [:P1 :M3 :P5 :M7]                :aliases ["Δ","ma7","M7","Maj7","^7", "major seventh"]}
   :maj9       {:intervals [:P1 :M3 :P5 :M7 :M9]            :aliases ["Δ9","^9", "major ninth"]}
   :maj13      {:intervals [:P1 :M3 :P5 :M7 :M9 :M13]       :aliases ["Maj13","^13", "major thirteenth"]}
   :6          {:intervals [:P1 :M3 :P5 :M6]                :aliases ["add6","add13","M6", "sixth"]}
   :6add9      {:intervals [:P1 :M3 :P5 :M6 :M9]            :aliases ["6/9","69","M69", "sixth added ninth"]}
   :M7b6       {:intervals [:P1 :M3 :m6 :M7]                :aliases ["^7b6", "major seventh flat sixth"]}
   :maj#4      {:intervals [:P1 :M3 :P5 :M7 :A11]           :aliases ["Δ#4","Δ#11","M7#11","^7#11","maj7#11", "major seventh sharp eleventh"]}
   ;; Minor
   ;;; Normal
   :m          {:intervals [:P1 :m3 :P5]                    :aliases ["min","-", "minor"]}
   :m7         {:intervals [:P1 :m3 :P5 :m7]                :aliases ["min7","mi7","-7", "minor seventh"]}
   :mMaj7      {:intervals [:P1 :m3 :P5 :M7]                :aliases ["m/maj7","mM7","m/M7","-Δ7","mΔ","-^7", "minor/major seventh"]}
   :m6         {:intervals [:P1 :m3 :P5 :M6]                :aliases ["-6", "minor sixth"]}
   :m9         {:intervals [:P1 :m3 :P5 :m7 :M9]            :aliases ["-9", "minor ninth"]}
   :mM9        {:intervals [:P1 :m3 :P5 :M7 :M9]            :aliases ["mMaj9","-^9", "minor/major ninth"]}
   :m11        {:intervals [:P1 :m3 :P5 :m7 :M9 :P11]       :aliases ["-11", "minor eleventh"]}
   :m13        {:intervals [:P1 :m3 :P5 :m7 :M9 :M13]       :aliases ["-13", "minor thirteenth"]}
   ;;; Diminished
   :dim        {:intervals [:P1 :m3 :d5]                    :aliases ["°", "diminished"]}
   :dim7       {:intervals [:P1 :m3 :d5 :d7]                :aliases ["°7", "diminished seventh"]}
   :m7b5       {:intervals [:P1 :m3 :d5 :m7]                :aliases ["ø", "-7b5", "half-diminished"]}
   ;; Dominant/Seventh
   ;;; Normal
   :7          {:intervals [:P1 :M3 :P5 :m7]                :aliases ["dom", "dominant seventh"]}
   :9          {:intervals [:P1 :M3 :P5 :m7 :M9]            :aliases ["dominant ninth"]}
   :13         {:intervals [:P1 :M3 :P5 :m7 :M9 :M13]       :aliases ["dominant thirteenth"]}
   :7#11       {:intervals [:P1 :M3 :P5 :m7 :A11]           :aliases ["7#4", "lydian dominant seventh"]}
   ;;; Altered
   :7b9        {:intervals [:P1 :M3 :P5 :m7 :m9]            :aliases ["dominant flat ninth"]}
   :7#9        {:intervals [:P1 :M3 :P5 :m7 :A9]            :aliases ["dominant sharp ninth"]}
   :alt7       {:intervals [:P1 :M3 :m7 :m9]                :aliases ["altered"]}
   ;;; Suspended
   :sus4       {:intervals [:P1 :P4 :P5]                    :aliases ["sus", "suspended fourth"]}
   :sus2       {:intervals [:P1 :M2 :P5]                    :aliases ["suspended second"]}
   :7sus4      {:intervals [:P1 :P4 :P5 :m7]                :aliases ["7sus", "suspended fourth seventh"]}
   :11         {:intervals [:P1 :P5 :m7 :M9 :P11]           :aliases ["eleventh"]}
   :b9sus      {:intervals [:P1 :P4 :P5 :m7 :m9]            :aliases ["phrygian", "phryg","7b9sus","7b9sus4", "suspended fourth flat ninth"]}
   ;; Other
   :5          {:intervals [:P1 :P5]                        :aliases ["fifth"]}
   :aug        {:intervals [:P1 :M3 :A5]                    :aliases ["+","+5","^#5", "augmented"]}
   :m#5        {:intervals [:P1 :m3 :A5]                    :aliases ["-#5","m+", "minor augmented"]}
   :maj7#5     {:intervals [:P1 :M3 :A5 :M7]                :aliases ["maj7+5","+maj7","^7#5", "augmented seventh"]}
   :maj9#11    {:intervals [:P1 :M3 :P5 :M7 :M9 :A11]       :aliases ["Δ9#11","^9#11", "major sharp eleventh (lydian)"]}
   :sus24      {:intervals [:P1 :M2 :P4 :P5]                :aliases ["sus4add9"]}
   :maj9#5     {:intervals [:P1 :M3 :A5 :M7 :M9]            :aliases ["Maj9#5"]}
   :7#5        {:intervals [:P1 :M3 :A5 :m7]                :aliases ["+7","7+","7aug","aug7"]}
   :7#5#9      {:intervals [:P1 :M3 :A5 :m7 :A9]            :aliases ["7#9#5","7alt"]}
   :9#5        {:intervals [:P1 :M3 :A5 :m7 :M9]            :aliases ["9+"]}
   :9#5#11     {:intervals [:P1 :M3 :A5 :m7 :M9 :A11]}
   :7#5b9      {:intervals [:P1 :M3 :A5 :m7 :m9]            :aliases ["7b9#5"]}
   :7#5b9#11   {:intervals [:P1 :M3 :A5 :m7 :m9 :A11]}
   :+add#9     {:intervals [:P1 :M3 :A5 :A9]}
   :M#5add9    {:intervals [:P1 :M3 :A5 :M9]                :aliases ["+add9"]}
   :M6#11      {:intervals [:P1 :M3 :P5 :M6 :A11]           :aliases ["M6b5","6#11","6b5"]}
   :M7add13    {:intervals [:P1 :M3 :P5 :M6 :M7 :M9]}
   :69#11      {:intervals [:P1 :M3 :P5 :M6 :M9 :A11]}
   :m69        {:intervals [:P1 :m3 :P5 :M6 :M9]            :aliases ["-69"]}
   :7b6        {:intervals [:P1 :M3 :P5 :m6 :m7]}
   :maj7#9#11  {:intervals [:P1 :M3 :P5 :M7 :A9 :A11]}
   :M13#11     {:intervals [:P1 :M3 :P5 :M7 :M9 :A11 :M13]  :aliases ["maj13#11","M13+4","M13#4"]}
   :M7b9       {:intervals [:P1 :M3 :P5 :M7 :m9]}
   :7#11b13    {:intervals [:P1 :M3 :P5 :m7 :A11 :m13]      :aliases ["7b5b13"]}
   :7add6      {:intervals [:P1 :M3 :P5 :m7 :M13]           :aliases ["67","7add13"]}
   :7#9#11     {:intervals [:P1 :M3 :P5 :m7 :A9 :A11]       :aliases ["7b5#9","7#9b5"]}
   :13#9#11    {:intervals [:P1 :M3 :P5 :m7 :A9 :A11 :M13]}
   :7#9#11b13  {:intervals [:P1 :M3 :P5 :m7 :A9 :A11 :m13]}
   :13#9       {:intervals [:P1 :M3 :P5 :m7 :A9 :M13]}
   :7#9b13     {:intervals [:P1 :M3 :P5 :m7 :A9 :m13]}
   :9#11       {:intervals [:P1 :M3 :P5 :m7 :M9 :A11]       :aliases ["9+4","9#4"]}
   :13#11      {:intervals [:P1 :M3 :P5 :m7 :M9 :A11 :M13]  :aliases ["13+4","13#4"]}
   :9#11b13    {:intervals [:P1 :M3 :P5 :m7 :M9 :A11 :m13]  :aliases ["9b5b13"]}
   :7b9#11     {:intervals [:P1 :M3 :P5 :m7 :m9 :A11]       :aliases ["7b5b9","7b9b5"]}
   :13b9#11    {:intervals [:P1 :M3 :P5 :m7 :m9 :A11 :M13]}
   :7b9b13#11  {:intervals [:P1 :M3 :P5 :m7 :m9 :A11 :m13]  :aliases ["7b9#11b13","7b5b9b13"]}
   :13b9       {:intervals [:P1 :M3 :P5 :m7 :m9 :M13]}
   :7b9b13     {:intervals [:P1 :M3 :P5 :m7 :m9 :m13]}
   :7b9#9      {:intervals [:P1 :M3 :P5 :m7 :m9 :A9]}
   :Madd9      {:intervals [:P1 :M3 :P5 :M9]                :aliases ["2","add9","add2"]}
   :Maddb9     {:intervals [:P1 :M3 :P5 :m9]}
   :Mb5        {:intervals [:P1 :M3 :d5]}
   :13b5       {:intervals [:P1 :M3 :d5 :M6 :m7 :M9]}
   :M7b5       {:intervals [:P1 :M3 :d5 :M7]}
   :M9b5       {:intervals [:P1 :M3 :d5 :M7 :M9]}
   :7b5        {:intervals [:P1 :M3 :d5 :m7]}
   :9b5        {:intervals [:P1 :M3 :d5 :m7 :M9]}
   :7no5       {:intervals [:P1 :M3 :m7]}
   :7b13       {:intervals [:P1 :M3 :m7 :m13]}
   :9no5       {:intervals [:P1 :M3 :m7 :M9]}
   :13no5      {:intervals [:P1 :M3 :m7 :M9 :M13]}
   :9b13       {:intervals [:P1 :M3 :m7 :M9 :m13]}
   :madd4      {:intervals [:P1 :m3 :P4 :P5]}
   :mMaj7b6    {:intervals [:P1 :m3 :P5 :m6 :M7]}
   :mMaj9b6    {:intervals [:P1 :m3 :P5 :m6 :M7 :M9]}
   :m7add11    {:intervals [:P1 :m3 :P5 :m7 :P11]           :aliases ["m7add4"]}
   :madd9      {:intervals [:P1 :m3 :P5 :M9]}
   :dim7M7     {:intervals [:P1 :m3 :d5 :M6 :M7]            :aliases ["o7M7"]}
   :dimM7      {:intervals [:P1 :m3 :d5 :M7]                :aliases ["oM7"]}
   :mb6M7      {:intervals [:P1 :m3 :m6 :M7]}
   :m7#5       {:intervals [:P1 :m3 :m6 :m7]}
   :m9#5       {:intervals [:P1 :m3 :m6 :m7 :M9]}
   :m11A       {:intervals [:P1 :m3 :A5 :m7 :M9 :P11]}
   :mb6b9      {:intervals [:P1 :m3 :m6 :m9]}
   :m9b5       {:intervals [:P1 :M2 :m3 :d5 :m7]}
   :M7#5sus4   {:intervals [:P1 :P4 :A5 :M7]}
   :M9#5sus4   {:intervals [:P1 :P4 :A5 :M7 :M9]}
   :7#5sus4    {:intervals [:P1 :P4 :A5 :m7]}
   :M7sus4     {:intervals [:P1 :P4 :P5 :M7]}
   :M9sus4     {:intervals [:P1 :P4 :P5 :M7 :M9]}
   :9sus4      {:intervals [:P1 :P4 :P5 :m7 :M9]            :aliases ["9sus"]}
   :13sus4     {:intervals [:P1 :P4 :P5 :m7 :M9 :M13]       :aliases ["13sus"]}
   :7sus4b9b13 {:intervals [:P1 :P4 :P5 :m7 :m9 :m13]       :aliases ["7b9b13sus4"]}
   :q          {:intervals [:P1 :P4 :m7 :m10]               :aliases ["quartal"]}
   :11b9       {:intervals [:P1 :P5 :m7 :m9 :P11]}))

(def intervals->chords
  (reduce-kv (fn [m chord {:keys [:intervals]}] (assoc m (set intervals) chord)) {} chords))

;; Derived
;; Chord: Maj, Maj7, min7, minMaj7
;;   Made up of root pitch (which will have a scale degree, when figured out (e.g. I, IV)) and intervals (relative to the root)
;;      Cannot rely solely on semitones since more than one interval can share the same amount of semitones
;;   Has positions (root)/can be inverted (first, second inversion) when root not lowest note
;; Scale: Maj, min
;;   Absolute distances: degrees
;;   Relative distances: intervals
;;   Has modes, which are similar to inversions (same intervals as base scale, new tonic)
;;   Tonic (1) is the key
;;   Chords can be derived from a scale

; TODO: namespace with scale/
(def scales
  (array-map
   ;; Basic
   :major {:intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7] :aliases ["ionian"] :degrees [:1 :2 :3 :4 :5 :6 :7]}
   :minor {:intervals [:P1 :M2 :m3 :P4 :P5 :m6 :m7] :aliases ["aeolian"] :degrees [:1 :2 :b3 :4 :5 :b6 :b7]}
   :major-pentatonic {:intervals [:P1 :M2 :M3 :P5 :M6] :aliases ["pentatonic"] :degrees [:1 :2 :3 :5 :6]}
   ;; Jazz common
   :major-blues {:intervals [:P1 :M2 :m3 :M3 :P5 :M6] :degrees [:1 :2 :b3 :3 :5 :6]}
   :minor-blues {:intervals [:P1 :m3 :P4 :d5 :P5 :m7] :aliases ["blues"] :degrees [:1 :b3 :4 :b5 :5 :b7]}
   :melodic-minor {:intervals [:P1 :M2 :m3 :P4 :P5 :M6 :M7] :aliases ["jazz minor" "minor-major"] :degrees [:1 :2 :b3 :4 :5 :6 :7]}
   :harmonic-minor {:intervals [:P1 :M2 :m3 :P4 :P5 :m6 :M7] :degrees [:1 :2 :b3 :4 :5 :b6 :7]}
   :bebop {:intervals [:P1 :M2 :M3 :P4 :P5 :M6 :m7 :M7] :degrees [:1 :2 :3 :4 :5 :6 :b7 :7]}
   :diminished {:intervals [:P1 :M2 :m3 :P4 :d5 :m6 :M6 :M7] :aliases ["whole-half diminished"] :degrees [:1 :2 :b3 :4 :b5 :b6 :6 :7]}
   ;; Modes
   :dorian {:intervals [:P1 :M2 :m3 :P4 :P5 :M6 :m7] :degrees [:1 :2 :b3 :4 :5 :6 :b7]}
   :lydian {:intervals [:P1 :M2 :M3 :A4 :P5 :M6 :M7] :degrees [:1 :2 :3 :#4 :5 :6 :7]}
   :mixolydian {:intervals [:P1 :M2 :M3 :P4 :P5 :M6 :m7] :aliases ["dominant"] :degrees [:1 :2 :3 :4 :5 :6 :b7]}
   :phrygian {:intervals [:P1 :m2 :m3 :P4 :P5 :m6 :m7] :degrees [:1 :b2 :b3 :4 :5 :b6 :b7]}
   :locrian {:intervals [:P1 :m2 :m3 :P4 :d5 :m6 :m7] :degrees [:1 :b2 :b3 :4 :b5 :b6 :b7]}
   ;; 5-note
   :ionian-pentatonic {:intervals [:P1 :M3 :P4 :P5 :M7] :degrees [:1 :3 :4 :5 :7]}
   :mixolydian-pentatonic {:intervals [:P1 :M3 :P4 :P5 :m7] :aliases ["indian"] :degrees [:1 :3 :4 :5 :b7]}
   ; :ritusen {:intervals [:P1 :M2 :P4 :P5 :M6] :degrees [:1 :2 :4 :5 :6]}
   ; :egyptian {:intervals [:P1 :M2 :P4 :P5 :m7] :degrees [:1 :2 :4 :5 :b7]}
   ; :neopolitan-major-pentatonic {:intervals [:P1 :M3 :P4 :d5 :m7] :degrees [:1 :3 :4 :b5 :b7]}
   ; :vietnamese-1 {:intervals [:P1 :m3 :P4 :P5 :m6] :degrees [:1 :b3 :4 :5 :b6]}
   ; :pelog {:intervals [:P1 :m2 :m3 :P5 :m6] :degrees [:1 :b2 :b3 :5 :b6]}
   ; :kumoijoshi {:intervals [:P1 :m2 :P4 :P5 :m6] :degrees [:1 :b2 :4 :5 :b6]}
   ; :hirajoshi {:intervals [:P1 :M2 :m3 :P5 :m6] :degrees [:1 :2 :b3 :5 :b6]}
   ; :iwato {:intervals [:P1 :m2 :P4 :d5 :m7] :degrees [:1 :b2 :4 :b5 :b7]}
   ; :in-sen {:intervals [:P1 :m2 :P4 :P5 :m7] :degrees [:1 :b2 :4 :5 :b7]}
   :lydian-pentatonic {:intervals [:P1 :M3 :A4 :P5 :M7] :aliases ["chinese"] :degrees [:1 :3 :#4 :5 :7]}
   :tizita {:intervals [:P1 :M2 :m3 :P5 :m6] :aliases ["ethiopian"] :degrees [:1 :2 :b3 :5 :b6]}
   ; :malkos-raga {:intervals [:P1 :m3 :P4 :m6 :m7] :degrees [:1 :b3 :4 :b6 :b7]}
   :locrian-pentatonic {:intervals [:P1 :m3 :P4 :d5 :m7] :aliases ["minor seven flat five pentatonic"] :degrees [:1 :b3 :4 :b5 :b7]}
   :minor-pentatonic {:intervals [:P1 :m3 :P4 :P5 :m7] :aliases ["vietnamese 2"] :degrees [:1 :b3 :4 :5 :b7]}
   :minor-six-pentatonic {:intervals [:P1 :m3 :P4 :P5 :M6] :degrees [:1 :b3 :4 :5 :6]}
   :flat-three-pentatonic {:intervals [:P1 :M2 :m3 :P5 :M6] :aliases ["kumoi"] :degrees [:1 :2 :b3 :5 :6]}
   :flat-six-pentatonic {:intervals [:P1 :M2 :M3 :P5 :m6] :degrees [:1 :2 :3 :5 :b6]}
   ; :scriabin {:intervals [:P1 :m2 :M3 :P5 :M6] :degrees [:1 :b2 :3 :5 :6]}
   :whole-tone-pentatonic {:intervals [:P1 :M3 :d5 :m6 :m7] :degrees [:1 :3 :b5 :b6 :b7]}
   :lydian-#5P-pentatonic {:intervals [:P1 :M3 :A4 :A5 :M7] :degrees [:1 :3 :#4 :#5 :7]}
   :lydian-dominant-pentatonic {:intervals [:P1 :M3 :A4 :P5 :m7] :degrees [:1 :3 :#4 :5 :b7]}
   :minor-#7M-pentatonic {:intervals [:P1 :m3 :P4 :P5 :M7] :degrees [:1 :b3 :4 :5 :7]}
   :super-locrian-pentatonic {:intervals [:P1 :m3 :d4 :d5 :m7] :degrees [:1 :b3 :b4 :b5 :b7]}
   ;; 6-note
   :minor-hexatonic {:intervals [:P1 :M2 :m3 :P4 :P5 :M7] :degrees [:1 :2 :b3 :4 :5 :7]}
   :augmented {:intervals [:P1 :A2 :M3 :P5 :A5 :M7] :degrees [:1 :#2 :3 :5 :#5 :7]}
   ; :piongio {:intervals [:P1 :M2 :P4 :P5 :M6 :m7] :degrees [:1 :2 :4 :5 :6 :b7]}
   ; :prometheus-neopolitan {:intervals [:P1 :m2 :M3 :A4 :M6 :m7] :degrees [:1 :b2 :3 :#4 :6 :b7]}
   ; :prometheus {:intervals [:P1 :M2 :M3 :A4 :M6 :m7] :degrees [:1 :2 :3 :#4 :6 :b7]}
   ; :mystery-#1 {:intervals [:P1 :m2 :M3 :d5 :m6 :m7] :degrees [:1 :b2 :3 :b5 :b6 :b7]}
   ; :six-tone-symmetric {:intervals [:P1 :m2 :M3 :P4 :A5 :M6] :degrees [:1 :b2 :3 :4 :#5 :6]}
   :whole-tone {:intervals [:P1 :M2 :M3 :A4 :A5 :A6] :aliases ["messiaen's mode #1"] :degrees [:1 :2 :3 :#4 :#5 :#6]}
   ; :messiaen's-mode-#5 {:intervals [:P1 :m2 :P4 :A4 :P5 :M7] :degrees [:1 :b2 :4 :#4 :5 :7]}
   ;; 7-note
   :locrian-major {:intervals [:P1 :M2 :M3 :P4 :d5 :m6 :m7] :aliases ["arabian"] :degrees [:1 :2 :3 :4 :b5 :b6 :b7]}
   :double-harmonic-lydian {:intervals [:P1 :m2 :M3 :A4 :P5 :m6 :M7] :degrees [:1 :b2 :3 :#4 :5 :b6 :7]}
   ; :altered {:intervals [:P1 :m2 :A2 :M3 :A4 :m6 :m7] :aliases ["super locrian" "diminished whole tone" "pomeroy"] :degrees [:1 :b2 :#2 :3 :#4 :b6 :b7]}
   :altered {:intervals [:P1 :m2 :m3 :d4 :d5 :m6 :m7] :aliases ["super locrian" "diminished whole tone" "pomeroy"] :degrees [:1 :b2 :#2 :3 :#4 :b6 :b7]}
   :locrian-#2 {:intervals [:P1 :M2 :m3 :P4 :d5 :m6 :m7] :aliases ["half-diminished" "aeolian b5"] :degrees [:1 :2 :b3 :4 :b5 :b6 :b7]}
   :mixolydian-b6 {:intervals [:P1 :M2 :M3 :P4 :P5 :m6 :m7] :aliases ["melodic minor fifth mode" "hindu"] :degrees [:1 :2 :3 :4 :5 :b6 :b7]}
   :lydian-dominant {:intervals [:P1 :M2 :M3 :A4 :P5 :M6 :m7] :aliases ["lydian b7" "overtone"] :degrees [:1 :2 :3 :#4 :5 :6 :b7]}
   :lydian-augmented {:intervals [:P1 :M2 :M3 :A4 :A5 :M6 :M7] :degrees [:1 :2 :3 :#4 :#5 :6 :7]}
   :dorian-b2 {:intervals [:P1 :m2 :m3 :P4 :P5 :M6 :m7] :aliases ["phrygian #6" "melodic minor second mode"] :degrees [:1 :b2 :b3 :4 :5 :6 :b7]}
   :ultralocrian {:intervals [:P1 :m2 :m3 :d4 :d5 :m6 :d7] :aliases ["superlocrian bb7" "superlocrian diminished"] :degrees [:1 :b2 :b3 :b4 :b5 :b6 :b7]}
   :locrian-6 {:intervals [:P1 :m2 :m3 :P4 :d5 :M6 :m7] :aliases ["locrian natural 6" "locrian sharp 6"] :degrees [:1 :b2 :b3 :4 :b5 :6 :b7]}
   :augmented-heptatonic {:intervals [:P1 :A2 :M3 :P4 :P5 :A5 :M7] :degrees [:1 :#2 :3 :4 :5 :#5 :7]}
   :dorian-#4 {:intervals [:P1 :M2 :m3 :A4 :P5 :M6 :m7] :aliases ["ukrainian dorian" "romanian minor" "altered dorian"] :degrees [:1 :2 :b3 :#4 :5 :6 :b7]}
   :lydian-diminished {:intervals [:P1 :M2 :m3 :A4 :P5 :M6 :M7] :degrees [:1 :2 :b3 :#4 :5 :6 :7]}
   :leading-whole-tone {:intervals [:P1 :M2 :M3 :A4 :A5 :m7 :M7] :degrees [:1 :2 :3 :#4 :#5 :b7 :7]}
   :lydian-minor {:intervals [:P1 :M2 :M3 :A4 :P5 :m6 :m7] :degrees [:1 :2 :3 :#4 :5 :b6 :b7]}
   :phrygian-dominant {:intervals [:P1 :m2 :M3 :P4 :P5 :m6 :m7] :aliases ["spanish" "phrygian major"] :degrees [:1 :b2 :3 :4 :5 :b6 :b7]}
   ; :balinese {:intervals [:P1 :m2 :m3 :P4 :P5 :m6 :M7] :degrees [:1 :b2 :b3 :4 :5 :b6 :7]}
   ; :neopolitan-major {:intervals [:P1 :m2 :m3 :P4 :P5 :M6 :M7] :degrees [:1 :b2 :b3 :4 :5 :6 :7]}
   :harmonic-major {:intervals [:P1 :M2 :M3 :P4 :P5 :m6 :M7] :degrees [:1 :2 :3 :4 :5 :b6 :7]}
   :double-harmonic-major {:intervals [:P1 :m2 :M3 :P4 :P5 :m6 :M7] :aliases ["gypsy"] :degrees [:1 :b2 :3 :4 :5 :b6 :7]}
   :hungarian-minor {:intervals [:P1 :M2 :m3 :A4 :P5 :m6 :M7] :degrees [:1 :2 :b3 :#4 :5 :b6 :7]}
   :hungarian-major {:intervals [:P1 :A2 :M3 :A4 :P5 :M6 :m7] :degrees [:1 :#2 :3 :#4 :5 :6 :b7]}
   ; :oriental {:intervals [:P1 :m2 :M3 :P4 :d5 :M6 :m7] :degrees [:1 :b2 :3 :4 :b5 :6 :b7]}
   ; :flamenco {:intervals [:P1 :m2 :m3 :M3 :A4 :P5 :m7] :degrees [:1 :b2 :b3 :3 :#4 :5 :b7]}
   ; :todi-raga {:intervals [:P1 :m2 :m3 :A4 :P5 :m6 :M7] :degrees [:1 :b2 :b3 :#4 :5 :b6 :7]}
   ; :persian {:intervals [:P1 :m2 :M3 :P4 :d5 :m6 :M7] :degrees [:1 :b2 :3 :4 :b5 :b6 :7]}
   ; :enigmatic {:intervals [:P1 :m2 :M3 :d5 :m6 :m7 :M7] :degrees [:1 :b2 :3 :b5 :b6 :b7 :7]}
   :major-augmented {:intervals [:P1 :M2 :M3 :P4 :A5 :M6 :M7] :aliases ["major #5" "ionian augmented" "ionian #5"] :degrees [:1 :2 :3 :4 :#5 :6 :7]}
   :lydian-#9 {:intervals [:P1 :A2 :M3 :A4 :P5 :M6 :M7] :degrees [:1 :#2 :3 :#4 :5 :6 :7]}
   ;; 8-note
   ; :messiaen's-mode-#4 {:intervals [:P1 :m2 :M2 :P4 :A4 :P5 :m6 :M7] :degrees [:1 :b2 :2 :4 :#4 :5 :b6 :7]}
   ; :purvi-raga {:intervals [:P1 :m2 :M3 :P4 :A4 :P5 :m6 :M7] :degrees [:1 :b2 :3 :4 :#4 :5 :b6 :7]}
   :spanish-heptatonic {:intervals [:P1 :m2 :m3 :M3 :P4 :P5 :m6 :m7] :degrees [:1 :b2 :b3 :3 :4 :5 :b6 :b7]}
   :bebop-minor {:intervals [:P1 :M2 :m3 :M3 :P4 :P5 :M6 :m7] :degrees [:1 :2 :b3 :3 :4 :5 :6 :b7]}
   :bebop-major {:intervals [:P1 :M2 :M3 :P4 :P5 :A5 :M6 :M7] :degrees [:1 :2 :3 :4 :5 :#5 :6 :7]}
   :bebop-locrian {:intervals [:P1 :m2 :m3 :P4 :d5 :P5 :m6 :m7] :degrees [:1 :b2 :b3 :4 :b5 :5 :b6 :b7]}
   :bebop-harmonic-minor {:intervals [:P1 :M2 :m3 :P4 :P5 :m6 :m7 :M7] :degrees [:1 :2 :b3 :4 :5 :b6 :b7 :7]}
   ; :ichikosucho {:intervals [:P1 :M2 :M3 :P4 :d5 :P5 :M6 :M7] :degrees [:1 :2 :3 :4 :b5 :5 :6 :7]}
   :minor-six-diminished {:intervals [:P1 :M2 :m3 :P4 :P5 :m6 :M6 :M7] :degrees [:1 :2 :b3 :4 :5 :b6 :6 :7]}
   :half-whole-diminished {:intervals [:P1 :m2 :m3 :M3 :A4 :P5 :M6 :m7] :aliases ["dominant diminished" "messiaen's mode #2"] :degrees [:1 :b2 :b3 :3 :#4 :5 :6 :b7]}
   ; :kafi-raga {:intervals [:P1 :m3 :M3 :P4 :P5 :M6 :m7 :M7] :degrees [:1 :b3 :3 :4 :5 :6 :b7 :7]}
   ; :messiaen's-mode-#6 {:intervals [:P1 :M2 :M3 :P4 :A4 :A5 :A6 :M7] :degrees [:1 :2 :3 :4 :#4 :#5 :#6 :7]}
   ;; 9-note
   :composite-blues {:intervals [:P1 :M2 :m3 :M3 :P4 :d5 :P5 :M6 :m7] :degrees [:1 :2 :b3 :3 :4 :b5 :5 :6 :b7]}
   ; :messiaen's-mode-#3 {:intervals [:P1 :M2 :m3 :M3 :A4 :P5 :m6 :m7 :M7] :degrees [:1 :2 :b3 :3 :#4 :5 :b6 :b7 :7]}
   ;; 10-note
   ; :messiaen's-mode-#7 {:intervals [:P1 :m2 :M2 :m3 :P4 :A4 :P5 :m6 :M6 :M7] :degrees [:1 :b2 :2 :b3 :4 :#4 :5 :b6 :6 :7]}
   ;; 12-note
   ; :chromatic {:intervals [:P1 :m2 :M2 :m3 :M3 :P4 :d5 :P5 :m6 :M6 :m7 :M7] :degrees [:1 :b2 :2 :b3 :3 :4 :b5 :5 :b6 :6 :b7 :7]}
   ))

(def intervals->scales
  (reduce-kv (fn [m scale-name {:keys [:intervals]}] (assoc m intervals scale-name)) {} scales))

;; Derived: (scale) degree(s), inversions (based on notes and chord intervals)
;; Degree: I, II, III,, bIII, V, #V, VII, etc.
;;   Can be represented by semitones
;;   Can be named as dominant, subdominant, etc.
;;   Can be arabic (5, 6), roman numerals (V, VI)
;;   Can be flattened/sharpened (e.g. a mode formula relative to the base scale), but minor/major/aug/dim not applicable, that's for chords ('quality'), the degree is just the relative pitch

; Roman numeral degree with chord quality
(def chord-degree-pattern #"[#b]?[ivIV]+[+°7]?")
(s/def ::degree (s/and keyword? #(re-find chord-degree-pattern (name %))))

(def name->shape (merge chords scales))
(s/def ::name (set (concat (keys chords) (keys scales))))

; Base chord/scale shapes
(s/def ::shape-blueprint (s/keys :req-un [::name ::intervals]
                                 :opt-un [::aliases ::degrees]))
; Lookup info for ->shape, enough to resolve final pitches/notes
(s/def ::shape-ref (s/keys :req-un [::name (or ::pitch ::note)]))
; Resolved, with intervals converted into pitches/notes
(s/def ::shape (s/merge ::shape-blueprint
                        (s/keys :req-un [::name
                                         (or ::pitch ::note)
                                         (or ::pitches ::notes)])))

(defn shape-ref? [x] (s/valid? ::shape-ref x))
(defn shape? [x] (s/valid? ::shape x))

(s/def ::bass ::pitch)

(s/def ::chord (s/and ::shape
                      #(contains? chords (:name %))
                      (s/keys :opt-un [::bass])))

(s/def ::scale (s/and ::shape
                      #(contains? scales (:name %))))

(defn chord? [x] (s/valid? ::chord x))
(defn scale? [x] (s/valid? ::scale x))

; Shapes coming from other shapes; recursive; denotes chord degree relationship
(s/def ::context (s/merge ::shape
                          (s/keys :req-un [::degree]
                                  :opt-un [::context])))

; Chord coming from a scale context
(s/def ::scale-chord (s/and ::chord
                            ::context
                            #(s/valid? ::scale (:context %))))

; Scale coming from a chord context
(s/def ::chord-scale (s/and ::scale
                            ::context
                            #(s/valid? ::chord (:context %))))

; Arithmetic

(defn parts
  [x]
  {:pre [(pitch-or-note? x)]}
  (let [[_ pitch-str letter-str accidental-str octave-str] (re-find pitch-or-note-pattern (name x))
        pitch (keyword pitch-str)]
    (-> {:pitch pitch
         :pci (pitches pitch)
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
  {:pre [(every? pitch-or-note? [x1 x2])]}
  (let [{letter1 :letter} (parts x1)
        {letter2 :letter} (parts x2)
        i1 (#?(:clj int :cljs .charCodeAt) letter1)
        i2 (#?(:clj int :cljs .charCodeAt) letter2)]
    (inc (mod (- i2 i1) 7))))

(defn- lesser? [s] (some (partial str/includes? s) ["d" "m"]))

(defn- accidental-match? [accidental-string]
  (fn [p]
    (let [{:keys [accidental]} (parts p)]
      (= accidental accidental-string))))
(def flat? (accidental-match? "b"))
(def natural? (accidental-match? ""))
(def sharp? (accidental-match? "#"))

(defn enharmonic
  [p notation]
  {:post [(pitch? %)]}
  (let [pci (pitches p)
        equivalent-pitches (pci->pitches pci)]
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
  {:pre [(note? note)]
   :post [(midi? %)]}
  (let [{:keys [pitch octave letter]} (parts note)
        pci (pitches pitch)
        base-pci (letters->pci letter)  ; PCI without accidentals
        new-octave (cond  ; Adjust for crossing octave boundary
                     (and (< pci base-pci) (not (str/includes? (name pitch) "b"))) (inc octave)  ; E.g. B#4 (0 < 11), only for sharps
                     (and (> pci base-pci) (not (str/includes? (name pitch) "#"))) (dec octave)  ; E.g. Cb (11 > 0), only for flats
                     :else octave)]
    (+ pci (* 12 (inc new-octave)))))

(defn midi->note
  "Convert midi integer to note, optionally specifying the target pitch (otherwise uses default flats/sharps)"
  [midi & [pitch]]
  {:pre [(midi? midi)]
   :post [(note? %)]}
  (let [octave (dec (quot midi 12))
        pci (mod midi 12)
        p (or pitch (pci->default-pitch pci))
        {:keys [letter]} (parts p)
        base-pci (letters->pci letter)  ; PCI without accidentals
        new-octave (cond  ; Adjust for crossing octave boundary
                     (and (< pci base-pci) (not (str/includes? (name p) "b"))) (dec octave)  ; E.g. B#4 (0 < 11), only for sharps
                     (and (> pci base-pci) (not (str/includes? (name p) "#"))) (inc octave)  ; E.g. Cb (11 > 0), only for flats
                     :else octave)]
    (keyword (str (name p) new-octave))))

(defn fold-notes
  "Fold notes into a 21 semitone range so the highest interval is a 13th (by default)"
  [notes & {:keys [max-semitones] :or {max-semitones 21}}]
  {:pre [(every? note? notes)]}
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
  {:pre [(every? pitch? [p1 p2])]}
  (inc (mod (dec (- (pitches p2) (pitches p1))) 12)))

(defn- note-semitone-distance
  [n1 n2 & {:keys [fold?] :or {fold? false}}]
  {:pre [(every? note? [n1 n2])]}
  (abs (apply - (map note->midi (if fold? (fold-notes [n1 n2]) [n1 n2])))))

(defn semitone-distance
  [x1 x2 & {:keys [fold?] :or {fold? false}}]
  (if (pitch? x1)
    (pitch-semitone-distance x1 x2)
    (note-semitone-distance x1 x2 :fold? fold?)))

(defn ->interval
  "Find interval between two pitches/notes
   Start with semitone distance, and use staff distance if needed to split hairs between augmented/diminished"
  [x1 x2]
  {:pre [(every? pitch-or-note? [x1 x2])]
   :post [(or (interval? %) (nil? %))]}
  (if (and (note? x1) (< (note->midi x2) (note->midi x1)))
    (let [{:keys [pitch octave]} (parts x2)]
      (->interval x1 (pitch->note pitch (inc octave))))
    (let [semitone-distance (semitone-distance x1 x2 :fold? true)
          matching-intervals (semitones->intervals semitone-distance)]
      (if (= (count matching-intervals) 1)
        (first matching-intervals)
        (let [distance (staff-distance x1 x2)]
          (first (filter #(or (str/includes? (name %) (str distance))
                              (str/includes? (name %) (str (+ 7 distance))))
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
  {:post [(pitch? %)]}
  (let [extended-pitch-pattern #"^(([A-G])(b*|#*))$"
        [_ _ letter-str accidental] (re-find extended-pitch-pattern (name p))
        letter (first letter-str)
        multiplier (when (str/includes? accidental "b") -1)]
    (loop [letter letter
           accidental accidental]
      (let [new-pitch (keyword (str letter accidental))]
        (if (contains? pitches new-pitch)
          new-pitch
          (recur (letter+ letter 2 multiplier) (subs accidental 2)))))))

(defn- pitch+interval
  [p interval & [multiplier]]
  (if (some? (#{:P1 :P8} interval))
    p
    (let [{:keys [letter]} (parts p)
          pitch-pci (pitches p)
          staff-distance (utils/parse-int interval)
          new-letter (letter+ letter staff-distance multiplier)
          new-letter-pci (pitches (keyword (str new-letter)))
          interval-semitones (get-in intervals [interval :semitones])
          new-pitch-pci (mod ((if (= multiplier -1) - +) pitch-pci interval-semitones) 12)
          difference (* (or multiplier 1)
                        (- new-pitch-pci new-letter-pci))
          new-difference (cond
                           (< difference -3) (+ difference 12)
                           (< 3 difference) (- difference 12)
                           :else difference)
          accidental-str (str/join "" (take (abs new-difference)
                                            (repeat (if (pos? new-difference)
                                                      (if (= multiplier -1) \b \#)
                                                      (if (= multiplier -1) \# \b)))))
          new-pitch (keyword (str new-letter accidental-str))]
      (clamp-pitch new-pitch))))

(defn- note+interval
  [n interval & [multiplier]]
  (let [{:keys [pitch]} (parts n)
        interval-semitones (get-in intervals [interval :semitones])
        new-pitch (pitch+interval pitch interval multiplier)]
    (-> n
        (note->midi)
        (+ (* (or multiplier 1) interval-semitones))
        (midi->note new-pitch))))

(defn +interval
  "Add/subtract interval to/from pitch or note"
  [x interval & [multiplier]]
  ; {:pre [(pitch-or-note? x)
  ;        (interval? interval)]
  ;  :post [(pitch-or-note? %)]}
  (if (= :P1 interval)
    x
    (if (pitch? x)
      (pitch+interval x interval multiplier)
      (note+interval x interval multiplier))))

(def +interval-memo (memoize +interval))

(defn ->shape
  "Given a starting pitch/note and a shape definition, derive the rest of the shape (e.g. pitches, intervals, degrees, notes (if x is a note))"
  ([x]
   ; Different notations
   (cond
     ; E.g. :C_maj, C4_maj
     (keyword? x) (let [[pitch-or-note-str shape-name-str] (str/split (name x) #"_")
                        pitch-or-note (keyword pitch-or-note-str)
                        shape-name (keyword shape-name-str)]
                    (->shape pitch-or-note shape-name))
     ; E.g. {:pitch :C :name :maj}
     (shape-ref? x) (if (or (contains? x :pitches) (contains? x :notes))
                      x
                      (->shape (or (:note x) (:pitch x)) (:name x)))))
  ([x shape-name]
   ; {:pre [(pitch-or-note? x)]}
   ; TODO: if :bass provided, reorder pitches and include lower note?
   (let [{:keys [pitch note]} (parts x)
         shape (name->shape shape-name)
         intervals (:intervals shape)
         pitches (mapv (partial +interval-memo pitch) intervals)]
     (cond-> shape
       true (merge {:pitch pitch
                    :name shape-name
                    :pitches pitches})
       (note? x) (assoc :notes (mapv (partial +interval-memo note) intervals))))))

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
  {:pre [(every? pitch-or-note? xs)]
   :post [(every? interval? %)]}
  (if (pitch? (first xs))
    (->intervals (pitches->notes xs))
    (map (partial ->interval (first xs)) xs)))

(defn interval->degree [interval]
  {:pre [(interval? interval)]}
  (let [major-intervals (get-in scales [:major :intervals])
        matching-idx (.indexOf major-intervals interval)]
    (keyword
     (if (neg? matching-idx)
       (str (if (lesser? (name interval)) "b" "#") (last (name interval)))
       (str (inc matching-idx))))))

;; Used for generating initial scale degrees
; (defn- scales-with-degrees []
;   (let [major-intervals (get-in scales [:major :intervals])]
;     (map (fn [[scale-name details]]
;            (let [{intervals :intervals} details
;                  degrees (mapv interval->degree intervals)]
;              {scale-name (assoc details :degrees degrees)})) scales)))

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
                    #(= (str/upper-case
                         (-> numeral-string
                             (str/replace  "b" "")
                             (str/replace  "#" "")
                             (str/replace  "°" "")
                             (str/replace  "+" "")
                             (str/replace  "7" ""))) (first %)) m)))))

(defn degree-chord->roman-numeral
  [degree chord-name]
  (let [intervals (:intervals (chords chord-name))
        major? (utils/in? intervals :M3)
        degree-str (name degree)
        accidental (if (< 1 (count degree-str)) (first degree-str) "")
        degree-num (utils/parse-int degree-str)
        roman-num (roman-numeral degree-num)]
    (keyword
     (str
      accidental
      ((if major? str/upper-case str/lower-case) roman-num)
      (cond
        (utils/in? intervals :A5) "+"
        (utils/in? intervals :d5) "°"
        (= :7 chord-name) "7"
        :else "")))))

(defn- circle-of-fifths [major-or-minor]
  (zipmap
   (take 15 (iterate (partial #(+interval % :P5))
                     (case major-or-minor
                       :major :Cb
                       :minor :Ab)))
   (range -7 8)))

(defn key-signature-accidentals [key-ref]
  (let [fifths->num-accidentals (circle-of-fifths (if (= (:name key-ref) :major) :major :minor))
        pitch (if (contains? fifths->num-accidentals (:pitch key-ref))
                (:pitch key-ref)
                (get (update-keys fifths->num-accidentals pitches)
                     (pitches (:pitch key-ref))))
        n (fifths->num-accidentals pitch)]
    (if (pos? n)
      (map (comp keyword #(str % "#")) (take n "FCGDAEB"))
      (map (comp keyword #(str % "b")) (take (Math/abs n) "BEADGCF")))))

(defn pitch->abc [p]
  (let [{:keys [letter accidental]} (parts p)
        abc-accidental (case accidental
                         "bb" "__"
                         "b" "_"
                         "#" "^"
                         "##" "^^"
                         "")]
    (str abc-accidental letter)))

(defn note->abc [n]
  (let [{:keys [letter octave accidental]} (parts n)
        abc-accidental (case accidental
                         "bb" "__"
                         "b" "_"
                         "#" "^"
                         "##" "^^"
                         "")
        lowercase? (< 4 octave)
        commas (if lowercase? 0 (- 4 octave))
        apostrophes (if lowercase? (- octave 5) 0)]
    (str
     abc-accidental
     ((if lowercase? str/lower-case str) letter)
     (str/join (take commas (repeat ",")))
     (str/join (take apostrophes (repeat "'"))))))

(defn shape->abc
  [shape & {:keys [note-length selected-key]
            :or {note-length "1/4"}}]
  {:pre [(shape? shape)]}
  (let [notes (set (:notes shape))
        scale? (scale? shape)
        pitch (:pitch shape)
        shape-name (:name shape)
        key-ref (cond
                  (some? selected-key) selected-key
                  :else {:pitch :C :name :major})
        key-shape (->shape (assoc key-ref :note (pitch->note (:pitch key-ref))))
        key-abc (str (name (:pitch key-shape))
                     " exp "
                     (str/join " " (map note->abc (:notes key-shape))))
        sorted-notes (sort-by note->midi notes)
        pitches-str (str/join " " (map note->abc sorted-notes))]
    (str/join "\n"
              ["X:1"
               (str "K:" key-abc)
               (str "L:" note-length)
               (str/join " "
                         [(when-not scale?
                            (str "\"" (name pitch) (name shape-name) "\""))
                          (if scale?
                            pitches-str
                            (str "[" pitches-str "]"))])])))

(defn abc-pitch->note
  "
  Convert abc notation pitch to :jigsaw.spec/note format
  \"C\" :C4
  \"^C\" :C#4
  \"^^C\" :C##4
  \"_D\" :Db4
  \"__D\" :Dbb4
  \"c\" :C5
  \"c'\" :C6
  \"c''\" :C7
  \"c'''\" :C8
  \"C,\" :C3
  \"C,,\" :C2
  \"C,,,\" :C1
  \"C,,,,\" :C0
  "
  [abc-pitch]
  (when abc-pitch
    (let [pitch-str (str abc-pitch)
          ;; Parse accidentals (^ for sharp, _ for flat)
          accidental-count (count (take-while #(or (= % \^) (= % \_)) pitch-str))
          accidental-char (when (pos? accidental-count) (first pitch-str))
          accidental-str (case [accidental-char accidental-count]
                           [\^ 1] "#"
                           [\^ 2] "##"
                           [\_ 1] "b"
                           [\_ 2] "bb"
                           "")
          ;; Get the base note letter (after accidentals)
          note-char (nth pitch-str accidental-count)
          base-letter (str/upper-case (str note-char))
          ;; Determine base octave (uppercase = 4, lowercase = 5)
          base-octave (if (= (str/upper-case note-char) note-char) 4 5)
          ;; Parse octave modifiers (' raises, , lowers)
          modifier-part (subs pitch-str (inc accidental-count))
          octave-offset (- (count (filter #(= % \') modifier-part))
                           (count (filter #(= % \,) modifier-part)))
          final-octave (+ base-octave octave-offset)
          ;; Construct the note keyword
          note-name (str base-letter accidental-str final-octave)]
      (keyword note-name))))

(defn key-signature->abc
  "
  B♭ - on the middle line (3rd line)
  E♭ - in the 4th space
  A♭ - in the 2nd space
  D♭ - on the 2nd line
  G♭ - on the 4th line
  C♭ - in the 3rd space
  F♭ - on the 1st line

  For sharps in treble clef:

  F♯ - on the 5th line
  C♯ - in the 3rd space
  G♯ - on the 4th line
  D♯ - in the 2nd space
  A♯ - on the 2nd line
  E♯ - in the 4th space
  B♯ - on the 3rd line
  "
  [key-ref]
  (let [accidental-pitches (key-signature-accidentals key-ref)
        pitch->abc #(case %
                      ;; Flats
                      :Bb "_B"
                      :Eb "_e"
                      :Ab "_A"
                      :Db "_d"
                      :Gb "_G"
                      :Cb "_c"
                      :Fb "_F"

                      ;; Sharps
                      :F# "^f"
                      :C# "^c"
                      :G# "^g"
                      :D# "^d"
                      :A# "^a"
                      :E# "^e"
                      :B# "^B")]
    (str/join " " (map pitch->abc accidental-pitches))))

;; TODO
;;  - Chord progressions/cadences from scales (i.e. shape of shapes)
;;  - Preview scales on top of chord (progression)
;;    - With different licks/melody rhythm patterns
;;  - Key signature, proper accidentals on music staff
;;  - factor in context more
;;  - highlight overlapping nodes
;;  - mood identification, scale and progression, add colors

(comment
  (take 3 (cycle '(:G :A)))
  (utils/rotate [:G :A :C :F] 3)
  (->> :F
       (iterate (partial #(+interval % :P5)))  ; Fifths
       (take 7))
  (circle-of-fifths :minor)
  (key-signature-accidentals {:pitch :Db :name :minor})
  (key-signature->abc {:pitch :Cb :name :major}))
