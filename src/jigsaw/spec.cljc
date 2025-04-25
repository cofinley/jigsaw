(ns jigsaw.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]))

;; Chroma: semitones cycling in one octave, where :C is 0, :C# is 1, :Db is 1, :B is 11, and B# is 0
(s/def ::chroma (s/and int? #(<= 0 % 11)))

;; Semitone: 0, 1, .., 21 (21 == thirteenth)
(s/def ::semitones (s/and int? #(<= 0 % 21)))

(def letters->chroma {\C 0 \D 2 \E 4 \F 5 \G 7 \A 9 \B 11})
;; Pitch (class): C, C#, Db, etc.
;;   Has different enharmonic representations (e.g. C#, Db) depending on preference (and relation to tonic, if in a scale, e.g. Gbb)
;;   Maps to absolute-do (AKA abdo, i.e. starting at C or "do" in solfege) integer 
(def pitches
  (reduce-kv
   (fn [m letter chroma]
     (assoc
      m
      (keyword (str letter "bb")) (mod (- chroma 2) 12)   ; Double-flat
      (keyword (str letter "b")) (mod (- chroma 1) 12)    ; Flat
      (keyword (str letter)) chroma                       ; Natural
      (keyword (str letter "#")) (mod (+ chroma 1) 12)    ; Sharp
      (keyword (str letter "##")) (mod (+ chroma 2) 12))) ; Double-sharp
   {}
   letters->chroma))
(def pitch-pattern-str "(([A-G])(b{0,2}|#{0,2}))")
(def pitch-pattern (re-pattern (str "^" pitch-pattern-str "$")))
(s/def ::pitch (s/and keyword? #(re-find pitch-pattern (name %)))) ; pitch in isolation or root (chord) or tonic (scale)
(defn pitch? [p] (s/valid? ::pitch p))

(def simple-pitch-keys
  (filter #(and (not (string/includes? (name %) "bb"))
                (not (string/includes? (name %) "##")))
          (keys (sort-by val < pitches))))

(def chroma->pitches
  (reduce-kv (fn [m pitch chroma] (update m chroma conj pitch)) {} pitches))

(def chroma->default-pitch
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

; Roman numeral degree with chord quality
(def chord-degree-pattern #"[#b]?[ivIV]+[+°7]?")
(s/def ::degree (s/and keyword? #(re-find chord-degree-pattern (name %))))

(s/def ::name (set (concat (keys chords) (keys scales))))
(s/def ::type #{:chord :scale})

; Base chord/scale shapes
(s/def ::shape-blueprint (s/keys :req-un [::name ::intervals]
                                 :opt-un [::aliases ::degrees]))
; Lookup info, enough to resolve final pitches/notes
(s/def ::shape-ref (s/keys :req-un [::name ::type (or ::pitch ::note)]
                           :opt-un [::degree]))
; Resolved, with intervals converted into pitches/notes
(s/def ::shape (s/merge ::shape-blueprint
                        (s/keys :req-un [::name
                                         (or ::pitch ::note)
                                         (or ::pitches ::notes)])))

(defn shape-ref? [x] (s/valid? ::shape-ref x))
(defn shape? [x] (s/valid? ::shape x))

(s/def ::chord (s/and ::shape
                      #(contains? chords (:name %))))

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
