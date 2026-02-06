(ns jigsaw.impl.theory
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.set :as set]
   [jigsaw.utils :as utils]))

; Specifications, music theory constants

(s/def ::alias string?)
(s/def ::aliases (s/coll-of ::alias))

;;;; PITCHES ;;;;

;; Pitch class index (PCI): semitones cycling in one octave, where :C is 0, :C# is 1, :Db is 1, :B is 11, and B# is 0
(s/def ::pci (s/and int? #(<= 0 % 11)))

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

(def simple-pitch-keys
  (filter #(and (not (str/includes? (name %) "bb"))
                (not (str/includes? (name %) "##")))
          (keys (sort-by val < pitches))))

(def pitch-pattern-str "(([A-G])(b{0,2}|#{0,2}))")
(def pitch-pattern (re-pattern (str "^" pitch-pattern-str "$")))
(s/def ::pitch (s/and keyword? #(re-find pitch-pattern (name %)))) ; pitch in isolation or root (chord) or tonic (scale)
(defn pitch? [x] (s/valid? ::pitch x))

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

;;;; NOTES ;;;;

;; Note: pitch and an octave
;;   This is different depending on who you ask.
;;   For this project, a note keyword of :Gb4 means a pitch of Gb and an octave of 4.
;;   One could argue Gb is a note name or pitch class, or that a note has timing information (e.g. quarter note).
;;   For now, this is what's used and its baked into the spec for consistency.
(def note-pattern-str (str pitch-pattern-str "(\\d{1})"))
(def note-pattern (re-pattern (str "^" note-pattern-str "$")))
(def pitch-or-note-pattern (re-pattern (str "^" note-pattern-str "?" "$")))
(s/def ::note (s/and keyword? #(re-find note-pattern (name %))))
(s/def ::notes (s/coll-of ::note))
(defn note? [x] (s/valid? ::note x))
(defn notes? [xs] (s/valid? ::notes xs))

(defn pitch-or-note? [x]
  (or (pitch? x) (note? x)))

;; Midi: position of a note on the keyboard
;;   Represented as an integer
;;   Probably applicable only to piano?
(s/def ::midi (s/and int? #(<= 0 % 127)))
(defn midi? [x] (s/valid? ::midi x))

;;;; SEMITONES ;;;;

;; Semitone: 0, 1, .., 21 (21 == thirteenth)
(s/def ::semitones (s/and int? #(<= 0 % 21)))

;;;; INTERVALS ;;;

;; Interval: 1, m3, M3, A5, d5, 5, etc.
;;   Distance between two pitches
;;   Can be represented by, but not constructed from semitones
;;      Must know the start and end pitches/staff positions
;;        e.g. C->F# is an augmented 4th (C->F + 1), and C->Gb is diminished (C->G - 1)
;;   Can be arabic (m3), roman (iii) numerals
;;   4th usually on major and sus chords, 11th on dominant and minor chords
;;   6th usually on major and minor chords, 13th usually on dominant chords
(def intervals
  {:P1  {:semitones 0 :aliases ["Root"]}
   :d2  {:semitones 0 :aliases ["Diminished 2nd"]}
   :m2  {:semitones 1 :aliases ["Minor 2nd"]}
   :M2  {:semitones 2 :aliases ["Major 2nd"]}
   :d3  {:semitones 2 :aliases ["Diminished 3rd"]}
   :m3  {:semitones 3 :aliases ["Minor 3rd"]}
   :A2  {:semitones 3 :aliases ["Augmented 2nd"]}
   :M3  {:semitones 4 :aliases ["Major 3rd"]}
   :d4  {:semitones 4 :aliases ["Diminished 4th"]}
   :P4  {:semitones 5 :aliases ["Perfect 4th"]}
   :A3  {:semitones 5 :aliases ["Augmented 3rd"]}
   :d5  {:semitones 6 :aliases ["Diminished 5th", "Tritone"]}
   :A4  {:semitones 6 :aliases ["Augmented 4th", "Tritone"]}
   :P5  {:semitones 7 :aliases ["Perfect 5th"]}
   :d6  {:semitones 7 :aliases ["Diminished 6th"]}
   :m6  {:semitones 8 :aliases ["Minor 6th"]}
   :A5  {:semitones 8 :aliases ["Augmented 5th"]}
   :M6  {:semitones 9 :aliases ["Major 6th"]}
   :d7  {:semitones 9 :aliases ["Diminished 7th"]}
   :m7  {:semitones 10 :aliases ["Minor 7th"]}
   :A6  {:semitones 10 :aliases ["Augmented 6th"]}
   :M7  {:semitones 11 :aliases ["Major 7th"]}
   :A7  {:semitones 12 :aliases ["Augmented 7th"]}
   :P8  {:semitones 12 :aliases ["Octave"]}
   :A8  {:semitones 13 :aliases ["Augmented 8th"]}
   :m9  {:semitones 13 :aliases ["Minor 9th"]}
   :M9  {:semitones 14 :aliases ["Major 9th"]}
   :m10 {:semitones 15 :aliases ["Minor 10th"]}
   :A9  {:semitones 15 :aliases ["Augmented 9th"]}
   :M10 {:semitones 16 :aliases ["Major 10th"]}
   :d11 {:semitones 16 :aliases ["Diminished 11th"]}
   :P11 {:semitones 17 :aliases ["Perfect 11th"]}
   :A11 {:semitones 18 :aliases ["Augmented 11th"]}
   :P12 {:semitones 19 :aliases ["Perfect 12th"]}
   :m13 {:semitones 20 :aliases ["Minor 13th"]}
   :M13 {:semitones 21 :aliases ["Major 13th"]}})

(s/def ::interval (set (keys intervals)))
(defn interval? [x] (s/valid? ::interval x))
(s/def ::intervals (s/coll-of ::interval))  ; Can be one (in isolation) or more (e.g. chords, scales)

;; Possible intervals for a given semitone value
(def semitones->intervals
  (reduce-kv (fn [m interval {:keys [semitones]}] (update m semitones conj interval)) {} intervals))

;;;; DEGREES ;;;;

;; Derived: (scale) degree(s), inversions (based on notes and chord intervals)
;; Degree: I, II, III,, bIII, V, #V, VII, etc.
;;   Can be represented by semitones
;;   Can be named as dominant, subdominant, etc.
;;   Can be arabic (5, 6), roman numerals (V, VI)
;;   Can be flattened/sharpened (e.g. a mode formula relative to the base scale), but minor/major/aug/dim not applicable, that's for chords ('quality'), the degree is just the relative pitch

; Roman (or arabic) numeral degree with chord quality
(def degree-pattern #"[#b]?[ivIV0-9]+[+°o%Mm7]?")
(s/def ::degree (s/and keyword? #(re-find degree-pattern (name %))))
(s/def ::degrees (s/coll-of ::degree))

;;;; SHAPES ;;;;

;; Chord and scales are composition of a pitch, name (i.e. quality), and intervals
;;  e.g. a pitch with intervals is a chord or a scale (think ECS)
;;    maybe use degrees instead of intervals for scale to be able to differentiate

; Base chord/scale shapes
(s/def ::shape-blueprint (s/keys :req-un [::intervals]
                                 :opt-un [::degrees]))
; Lookup info for ->shape, enough to resolve final pitches/notes
(s/def ::shape-ref (s/keys :req-un [::name (or ::pitch ::note)]
                           :opt-un [::context]))
; Resolved, with intervals converted into pitches/notes
(s/def ::shape (s/merge ::shape-blueprint
                        ::shape-ref
                        (s/keys :req-un [(or ::pitches ::notes)])))
(defn shape-ref? [x] (s/valid? ::shape-ref x))
(defn shape? [x] (s/valid? ::shape x))

(s/def ::bass ::pitch)

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
   :dim        {:intervals [:P1 :m3 :d5]                    :aliases ["°", "o", "diminished"]}
   :dim7       {:intervals [:P1 :m3 :d5 :d7]                :aliases ["°7", "o7", "diminished seventh"]}
   :m7b5       {:intervals [:P1 :m3 :d5 :m7]                :aliases ["ø", "%", "-7b5", "half-diminished"]}
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

(s/def ::chord (s/and ::shape
                      #(contains? chords (:name %))
                      (s/keys :opt-un [::bass])))

(s/def ::scale (s/and ::shape
                      #(contains? scales (:name %))))

(defn chord? [x] (s/valid? ::chord x))
(defn scale? [x] (s/valid? ::scale x))

; Shape name, i.e. quality, e.g. maj7
(def name->shape (merge chords scales))
(s/def ::name #(contains? (set (concat (keys chords) (keys scales))) %))

;;;; CONTEXTUAL SHAPES ;;;;

; Shapes coming from other shapes; recursive; denotes chord degree relationship
(s/def ::context keyword?)
; (s/def ::context (s/merge ::shape
;                           (s/keys :req-un [::degree]
;                                   :opt-un [::context])))

; ; Chord coming from a scale context
; (s/def ::scale-chord (s/and ::chord
;                             ::context
;                             #(s/valid? ::scale (:context %))))

; ; Scale coming from a chord context
; (s/def ::chord-scale (s/and ::scale
;                             ::context
;                             #(s/valid? ::chord (:context %))))

; TBD
(s/def ::progression-ref (s/keys :req-un [::degrees]))
(s/def ::progression (s/coll-of ::chord))

(def chord-progressions
  (into
   {}
   (map (fn [[prog-name prog]]
          [prog-name (assoc prog :degrees (mapv #(keyword "chord-degree" (name %))
                                                (:degrees prog)))])
        {"50s progression" {:degrees [:I :vi :IV :V] :quality :major}
         "IV-V-I-vi" {:degrees [:IV :V :I :vi] :quality :major}
         "I–V–vi–IV" {:degrees [:I :V :vi :IV] :quality :major}
         "I–IV–bVII–IV" {:degrees [:I :IV :bVII :IV] :quality :mixolydian}
         "ii–V–I" {:degrees [:ii :V :I] :quality :major}
         "ii–V–I with tritone substitution" {:degrees [:ii :bII :I] :quality :major}
         "ii-V-I with bIII+ as dominant substitute" {:degrees [:ii :bIII+ :I] :quality :mixolydian}
         ; Diminished represented as 'o' for easier typing
         ; Secondary dominant; represent / with _ for Clojure keyword reader compatibility
         "viio7/V–V–I" {:degrees [:viio7_V :V :I] :quality :major}
         "Andalusian cadence" {:degrees [:iv :III :bII :I] :quality :phrygian-dominant}
         "Backdoor progression" {:degrees [:ii :bVII :I] :quality :major}
         ; Half-diminished represented as % for easier typing
         #_#_"Bird changes" {:degrees [:I :vii% :III7 :vi :II7 :v :I7 :IV7 :iv :bVII7 :iii :VI7 :biii :bVI7 :ii :V7 :I :VI7 :ii :V] :quality :major}
         "Chromatic descending 5–6 sequence" {:degrees [:I :V :bVII :IV] :quality :mixolydian}
         "Circle progression" {:degrees [:vi :ii :V :I] :quality :major}
         "Coltrane changes" {:degrees [:I :V_bVI :bVI :V_III :III :V :I] :quality :major}
         "Eight-bar blues" {:degrees [:I :V :IV :IV :I :V :I :V] :quality :major}
         "Folia" {:degrees [:i :V :i :bVII :bIII :bVII :i :V :i :V :i :bVII :bIII :bVII :i :V :i] :quality :minor}
         "Irregular resolution" {:degrees [:V7 :III7] :quality :major}
         "Montgomery–Ward bridge" {:degrees [:I :IV :ii :V] :quality :major}
         "Passamezzo antico" {:degrees [:i :VII :i :V :III :VII :i :V :i] :quality :minor}
         "Passamezzo moderno" {:degrees [:I :IV :I :V :I :IV :I :V :I] :quality :major}
         "Ragtime" {:degrees [:III7 :VI7 :II7 :V7] :quality :major}
         "Romanesca" {:degrees [:III :VII :i :V :III :VII :i :V :i] :quality :major}
         "Sixteen-bar blues" {:degrees [:I :I :I :I :I :I :I :I :IV :IV :I :I :V :IV :I :I] :quality :major}
         "Twelve-bar blues" {:degrees [:I :I :I :I :IV :IV :I :I :V :IV :I :V] :quality :major}
         "I−vi−ii−V" {:degrees [:I :vi :ii :V] :quality :major}
         "bVII–V7 cadence" {:degrees [:bVII :V :I] :quality :mixolydian}
         "V–IV–I turnaround" {:degrees [:V :IV :I] :quality :major}
         "I–bVII–bVI–bVII" {:degrees [:I :bVII :bVI :bVII] :quality :minor}
         ; Major 7th chord; represent with M7 for Clojure keyword reader compatibility
         "Royal road" {:degrees [:IVM7 :V7 :iii7 :vi] :quality :major}
         "bVI-bVII-I" {:degrees [:bVI :bVII :I] :quality :major}})))

; :C (pitch)
; :C4 (note)
; :P5 (interval)
; [:P1 :M3 :P5] (shape based on intervals)
; [:1 :b3 :#5] (scale degrees, relating to harmonic function)
; [:I :iii :bIV :bIII+ :viio7] (chord degrees; like scale degrees, but with chord information like major/minor/dominant/diminished/augmented)
; :C_maj (chord (shape) based on pitch)
; :C4_maj (chord (shape) based on note)
; :C4_major (scale (shape) based on note)
; [:C_maj :E_m :G_maj] (progression based on chords)
; [:I :ii :V] (progression based on scale degrees)

(defn parts
  [x]
  ; {:pre [(pitch-or-note? x)]}
  (let [[_ pitch-str letter-str accidental-str octave-str] (re-find pitch-or-note-pattern (name x))
        pitch (keyword pitch-str)]
    (-> {:pitch pitch
         :pci (pitches pitch)
         :letter (first letter-str)
         :accidental accidental-str}
        (cond->
         (some? octave-str) (assoc :octave (utils/parse-int octave-str)
                                   :note (keyword (str pitch-str octave-str)))))))

(defn staff-distance
  [x1 x2]
  ; {:pre [(every? pitch-or-note? [x1 x2])]}
  (let [{letter1 :letter} (parts x1)
        {letter2 :letter} (parts x2)
        i1 (#?(:clj int :cljs .charCodeAt) letter1)
        i2 (#?(:clj int :cljs .charCodeAt) letter2)]
    (inc (mod (- i2 i1) 7))))

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

(defn lesser? [s] (some (partial str/includes? s) ["d" "m"]))

(defn accidental-match? [accidental-string]
  (fn [p]
    (let [{:keys [accidental]} (parts p)]
      (= accidental accidental-string))))
(def flat? (accidental-match? "b"))
(def natural? (accidental-match? ""))
(def sharp? (accidental-match? "#"))

(defn enharmonic-equivalent
  "Get enharmonic equivalent of pitch, given a notation. Returns equivalent or original pitch."
  [p notation]
  {:post [(pitch? %)]}
  (let [pci (pitches p)
        pci->pitches (reduce-kv (fn [m pitch pci] (update m pci conj pitch)) {} pitches)
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

(defn find-enharomic-equivalent
  "Find enharmonic patches of a source-pitch among target-pitches, based on PCI"
  [source-pitch target-pitches]
  (let [source-pci (pitches source-pitch)
        target-pcis->pitches (reduce (fn [m pitch]
                                       (let [pci (pitches pitch)]
                                         (assoc m pci pitch))) {} target-pitches)]
    (target-pcis->pitches source-pci)))

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

(defn pitch->note
  [p & [octave]]
  (keyword (str (name p) (or octave 4))))

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

(defn pitch-semitone-distance
  "Semitone distance, preserving 12, but modulo 12 otherwise"
  [p1 p2]
  {:pre [(every? pitch? [p1 p2])]}
  (inc (mod (dec (- (pitches p2) (pitches p1))) 12)))

(defn note-semitone-distance
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
  ; {:pre [(every? pitch-or-note? [x1 x2])]
  ;  :post [(or (interval? %) (nil? %))]}
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

(declare ->intervals)

(defn ->intervals-impl
  "Convert pitches to intervals, where the first pitch is :P1"
  [xs]
  ; {:pre [(every? pitch-or-note? xs)]
  ;  :post [(every? interval? %)]}
  (if (pitch? (first xs))
    (->intervals (pitches->notes xs))
    (map (partial ->interval (first xs)) xs)))

(def ->intervals (memoize ->intervals-impl))

(defn transpose-pitch
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

(defn transpose-note
  [n interval & [multiplier]]
  (let [{:keys [pitch]} (parts n)
        interval-semitones (get-in intervals [interval :semitones])
        new-pitch (transpose-pitch pitch interval multiplier)]
    (-> n
        (note->midi)
        (+ (* (or multiplier 1) interval-semitones))
        (midi->note new-pitch))))

(defn transpose
  "Add/subtract interval to/from pitch or note"
  [x interval & [multiplier]]
  ; {:pre [(pitch-or-note? x)
  ;        (interval? interval)]
  ;  :post [(pitch-or-note? %)]}
  (if (= :P1 interval)
    x
    (cond-> x
      (pitch? x) (transpose-pitch interval multiplier)
      (note? x) (transpose-note interval multiplier)
      (some? (:pitch x)) (assoc :pitch (transpose (:pitch x) interval multiplier))
      (some? (:pitches x)) (assoc :pitches (mapv #(transpose % interval multiplier) (:pitches x)))
      (some? (:notes x)) (assoc :notes (mapv #(transpose % interval multiplier) (:notes x)))
      ; Any context is now stale
      (and (map? x) (contains? x :context)) (dissoc :context))))

(def transpose-memo (memoize transpose))

;; DEGREES

(defn interval->degree [interval]
  {:pre [(interval? interval)]}
  (let [major-intervals (get-in scales [:major :intervals])
        matching-idx (.indexOf major-intervals interval)]
    (keyword
     (if (neg? matching-idx)
       (str (if (lesser? (name interval)) "b" "#") (last (name interval)))
       (str (inc matching-idx))))))

;; Used for generating initial scale degrees
; (defn scales-with-degrees []
;   (let [major-intervals (get-in scales [:major :intervals])]
;     (map (fn [[scale-name details]]
;            (let [{intervals :intervals} details
;                  degrees (mapv interval->degree intervals)]
;              {scale-name (assoc details :degrees degrees)})) scales)))

(defn roman-numeral
  [n]
  (nth ["I" "II" "III" "IV" "V" "VI" "VII"] (dec n)))

(defn roman-numeral->int
  [numeral-keyword]
  (let [m (into {} (map-indexed (fn [idx numeral] [numeral (inc idx)]) ["I" "II" "III" "IV" "V" "VI" "VII"]))
        stripped (-> numeral-keyword
                     name
                     (str/replace #"[b#°o%+mM7]" "")
                     str/upper-case)]
    (get m stripped)))

(defn derive-chord-degree
  "From a scale and chord, find the chord degree (roman numeral + optional quality)"
  [scale chord]
  (let [scale-pitch-index (.indexOf (:pitches scale) (first (:pitches chord)))]
    (when (<= 0 scale-pitch-index)
      (let [scale-degree (nth (:degrees scale)
                              scale-pitch-index)
            degree-str (name scale-degree)
            accidental (if (< 1 (count degree-str)) (first degree-str) "")
            degree-num (utils/parse-int degree-str)
            roman-num (roman-numeral degree-num)
            chord-name (:name chord)
            intervals (:intervals (chords chord-name))
            major? (utils/in? intervals :M3)
            chord-degree (str
                          accidental
                          ((if major? str/upper-case str/lower-case) roman-num)
                          ; TODO improve chord quality suffix; feels too hacky
                          (cond
                            (utils/in? intervals :A5) "+"
                            (and (utils/in? intervals :d5) (not= :m7b5 chord-name)) "o"
                            :else "")
                          (case chord-name
                            :m7b5 "%"
                            :dim7 "7" ; 'o' added above
                            :dim ""
                            :maj7 "M7"
                            :m7 "7"
                            :7 "7"
                            ""))]
        (keyword "chord-degree" chord-degree)))))

(defn resolve-chord-degree
  "
  From a scale and a chord degree (roman numeral + optional quality), find the chord

  Prefix:
    #
    b

  Major    I
  Minor    i
  Dim      io
  Major 7th    ...M7
  Minor 7th    ...7 (lowercase roman numeral)
  Dom. 7th     ...7 (uppercase roman numeral)
  Dim 7th      ...o7
  Half-dim 7th ...%
  "
  [scale chord-degree]
  (let [chord-name (condp #(some? (re-find %1 %2)) (name chord-degree)
                     #"%" :m7b5
                     #"o7" :dim7
                     #"o" :dim
                     #"M7" :maj7
                     #"[IV]7" :7
                     #"[iv]7" :m7
                     #"[IV]" :maj
                     #"[iv]" :m)
        scale-degree-int->pitch (reduce (fn [m [scale-degree pitch]]
                                          (assoc m (utils/parse-int (name scale-degree)) pitch))
                                        {}
                                        (zipmap (:degrees scale) (:pitches scale)))
        chord-degree-int (roman-numeral->int chord-degree)
        pitch (scale-degree-int->pitch chord-degree-int)
        new-pitch (keyword (str (name pitch) (re-find #"[#b]" (name chord-degree))))]
    {:pitch new-pitch :name chord-name :context (keyword "chord-degree" (name chord-degree))}))

;; KEY

(defn circle-of-fifths [major-or-minor]
  (zipmap
   (take 15 (iterate (partial #(transpose % :P5))
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

(defn relative-staff-note [key-ref absolute-note]
  (let [accidentals (key-signature-accidentals key-ref)
        pitch->accidentals (reduce #(assoc %1
                                           (keyword (str/replace (name %2) #"[b#]" ""))
                                           %2)
                                   {} accidentals)
        {absolute-pitch :pitch octave :octave} (parts absolute-note)
        relative-pitch (get pitch->accidentals absolute-pitch absolute-pitch)
        relative-note (keyword (str (name relative-pitch) octave))]
    relative-note))

;;;; SEARCH ;;;;

(defn jaccard-index [set1 set2]
  (let [intersection (count (set/intersection set1 set2))
        union (count (set/union set1 set2))]
    (if (zero? union)
      0.0
      (float (/ intersection union)))))

(def heuristic-labels
  {:contains? "partially contains"
   :fully-contains? "fully contains"
   :contained-in? "is partially contained by"
   :fully-contained-in? "is fully contained by"
   :overlap "overlaps with"})

(defn heuristic->float [x]
  (case x
    false 0
    true 1
    x))

(defn calculate-heuristics
  "Read as '<input> <heurstic> <possible shape>'"
  [input candidate]
  (let [input-set (set input)
        candidate-set (set candidate)]
    {:contains? (heuristic->float (set/superset? input-set candidate-set))
     :fully-contains? (heuristic->float (and (set/superset? input-set candidate-set) (not= input-set candidate-set)))
     :contained-in? (heuristic->float (set/subset? input-set candidate-set))
     :fully-contained-in? (heuristic->float (and (set/subset? input-set candidate-set) (not= input-set candidate-set)))
     :overlap (heuristic->float (jaccard-index input-set candidate-set))
     :same-pitch-count? (heuristic->float (= (count input-set) (count candidate-set)))
     :shares-root? (heuristic->float (and (some? (seq input)) (some? (seq candidate)) (= (first input) (first candidate))))}))

; Helper functions for bass/inversion analysis

(defn identify-bass-pitch
  "Identify the bass note (lowest pitch) from a collection of notes"
  [notes]
  (when (seq notes)
    (let [sorted-notes (sort-by note->midi notes)
          lowest-note (first sorted-notes)
          bass-pitch (-> lowest-note parts :pitch)]
      bass-pitch)))

(defn bass->inversion
  "Determine inversion number from bass note and chord.
   Returns:
   - 1 for first inversion (bass is third)
   - 2 for second inversion (bass is fifth)
   - etc.
   - nil if bass note is not a chord tone (slash chord)"
  [chord bass-pitch]
  (let [chord-pitches (:pitches chord)
        bass-index (.indexOf chord-pitches bass-pitch)]
    (when (> bass-index 0)
      bass-index)))

(defn inversion?
  "Check if the chord with bass note represents an inversion
   (bass note is a chord tone)"
  [chord bass-pitch]
  (some? (bass->inversion chord bass-pitch)))

(defn slash-chord?
  "Check if the chord with bass note represents a slash chord
   (bass note is NOT a chord tone)"
  [chord bass-pitch]
  (and (not= (:pitch chord) bass-pitch)
       (not (inversion? chord bass-pitch))))

(defn rotate-intervals
  "Recontextualize intervals by rotating/inverting them"
  [intervals n]
  (let [pitches (map #(transpose :C %) intervals)
        rotated-pitches (utils/rotate pitches n)
        rotated-intervals (->intervals rotated-pitches)]
    rotated-intervals))

(defn scale->mode
  [scale n]
  {:pre [(scale? scale)]}
  (let [pitches (utils/rotate (:pitches scale) n)
        rotated-intervals (rotate-intervals (:intervals scale) n)]
    (when-let [new-scale-name (intervals->scales rotated-intervals)]
      {:pitch (first pitches)
       :name new-scale-name})))

(defn scales->mode [src-scale dest-scale]
  (let [src-pitch-set (set (:pitches src-scale))
        dest-pitch-set (set (:pitches dest-scale))]
    (when (= src-pitch-set dest-pitch-set)
      (first
       (for [rotation (range 1 (count (:pitches src-scale)))
             :let [rotated-pitches (utils/rotate (:pitches src-scale) rotation)]
             :when (= rotated-pitches (:pitches dest-scale))]
         (keyword "mode" (roman-numeral (inc rotation))))))))

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
   ; {:pre [(theory/pitch-or-note? x)]}
   ; TODO: if :bass provided, reorder pitches and include lower note?
   (let [{:keys [pitch note]} (parts x)
         shape (name->shape shape-name)
         intervals (:intervals shape)
         pitches (mapv (partial transpose-memo pitch) intervals)]
     (cond-> shape
       true (merge {:pitch pitch
                    :name shape-name
                    :pitches pitches})
       true (dissoc :aliases)
       (note? x) (assoc :notes (mapv (partial transpose-memo note) intervals))))))

(comment
  (assert (true? (pitch? :C)))
  (assert (true? (note? :C4)))
  (assert (true? (interval? :P5)))
  (assert (true? (shape-ref? {:pitch :C :name :maj})))
  (assert (true? (shape-ref? {:note :C4 :name :maj}))))
