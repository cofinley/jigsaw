(ns jigsaw.spec
  (:gen-class)
  (:require [clojure.spec.alpha :as s]))

;; Semitone: 0, 1, .., 21 (21 == thirteenth)
(s/def ::semitone (s/and int? #(<= 0 % 21)))
(s/def ::semitones (s/coll-of ::semitone :distinct true))  ; when used for a chord

;; Pitch (class): C, C#, Db, etc.
;;   Has different representations (e.g. C#, Db) depending on preference (and relation to tonic, if in a scale, e.g. Gbb)
(def pitches
  (let [letters->semitones {\C 0 \D 2 \E 4 \F 5 \G 7 \A 9 \B 11}]
    (reduce
     (fn [m [letter semitone]]
       (-> m
           (assoc (keyword (str letter "bb")) (mod (- semitone 2) 12)) ; Double-flat
           (assoc (keyword (str letter "b")) (mod (- semitone 1) 12))  ; Flat
           (assoc (keyword (str letter)) semitone)                     ; Natural
           (assoc (keyword (str letter "#")) (mod (+ semitone 1) 12))  ; Sharp
           (assoc (keyword (str letter "##")) (mod (+ semitone 2) 12)) ; Double-sharp
           ))
     {}
     letters->semitones)))
(def pitch-pattern-str "(([A-G])(b{0,2}|#{0,2}))")
(def pitch-pattern (re-pattern (str "^" pitch-pattern-str "$")))
(s/def ::pitch (s/and keyword? #(re-find pitch-pattern (name %)))) ; pitch in isolation or root (chord) or tonic (scale)
(defn pitch? [p] (s/valid? ::pitch p))
(def pitches-by-index
  (reduce
   (fn [acc [pitch index]]
     (update acc index conj pitch))
   {}
   pitches))

(def default-pitch-by-index
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
(def intervals
  {:P1  {::name "Root" ::semitone 0}
   :d2  {::name "Diminished 2nd" ::semitone 0}
   :m2  {::name "Minor 2nd" ::semitone 1}
   :M2  {::name "Major 2nd" ::semitone 2}
   :d3  {::name "Diminished 3rd" ::semitone 2}
   :m3  {::name "Minor 3rd" ::semitone 3}
   :A2  {::name "Augmented 2nd" ::semitone 3}
   :M3  {::name "Major 3rd" ::semitone 4}
   :d4  {::name "Diminished 4th" ::semitone 4}
   :P4  {::name "Perfect 4th" ::semitone 5}
   :A3  {::name "Augmented 3rd" ::semitone 5}
   :d5  {::name "Diminished 5th" ::semitone 6 ::aliases ["Tritone"]}
   :A4  {::name "Augmented 4th" ::semitone 6 ::aliases ["Tritone"]}
   :P5  {::name "Perfect 5th" ::semitone 7}
   :d6  {::name "Diminished 6th" ::semitone 7}
   :m6  {::name "Minor 6th" ::semitone 8}
   :A5  {::name "Augmented 5th" ::semitone 8}
   :M6  {::name "Major 6th" ::semitone 9}
   :d7  {::name "Diminished 7th" ::semitone 9}
   :m7  {::name "Minor 7th" ::semitone 10}
   :A6  {::name "Augmented 6th" ::semitone 10}
   :M7  {::name "Major 7th" ::semitone 11}
   :A7  {::name "Augmented 7th" ::semitone 12}
   :P8  {::name "Octave" ::semitone 12}
   :A8  {::name "Augmented 8th" ::semitone 13}
   :m9  {::name "Minor 9th" ::semitone 13}
   :M9  {::name "Major 9th" ::semitone 14}
   :m10 {::name "Minor 10th" ::semitone 15}
   :A9  {::name "Augmented 9th" ::semitone 15}
   :M10 {::name "Major 10th" ::semitone 16}
   :d11 {::name "Diminished 11th" ::semitone 16}
   :P11  {::name "Perfect 11th" ::semitone 17}
   :A11 {::name "Augmented 11th" ::semitone 18}
   :P12 {::name "Perfect 12th" ::semitone 19}
   :m13 {::name "Minor 13th" ::semitone 20}
   :M13 {::name "Major 13th" ::semitone 21}})
(s/def ::interval (s/and keyword? #(contains? intervals %)))
(defn interval? [interval] (s/valid? ::interval interval))
(s/def ::intervals (s/coll-of ::intervals))  ; Can be one (in isolation) or more (e.g. chords, scales)

(def intervals-by-semitone
  (reduce
   (fn [acc [interval details]]
     (update acc (::semitone details) conj interval))
   {}
   intervals))

(s/def ::name string?)
(s/def ::aliases (s/coll-of string?))

;; Chord and scales are composition of pitch, name, intervals
;;  e.g. a pitch with intervals is a chord or a scale (think ECS)
;;    maybe use degrees instead of intervals for scale to be able to differentiate

(s/def ::octave (s/and int? #(<= -1 % 9)))

;; Note: pitch+octave
(def note-pattern-str (str pitch-pattern-str "(\\d{1})"))
(def note-pattern (re-pattern (str "^" note-pattern-str "$")))
(s/def ::note (s/and keyword? #(re-find note-pattern (name %))))
(defn note? [n] (s/valid? ::note n))
(s/def ::pitch-or-note (s/or :pitch pitch? :note note?))
(defn pitch-or-note? [x] (s/valid? ::pitch-or-note x))

(let [{pitch :pitch} (s/conform ::pitch-or-note :C)]
  pitch)

;; Midi: position of a note on the keyboard
;;   Represented as an integer
;;   Probably applicable only to piano?
(s/def ::midi (s/and int? #(<= 0 % 127)))

(def chords
  (array-map
    ;; Major
   :maj        {::intervals [:P1 :M3 :P5]                    ::aliases ["M", "major"]}
   :maj7       {::intervals [:P1 :M3 :P5 :M7]                ::aliases ["Δ","ma7","M7","Maj7","^7", "major seventh"]}
   :maj9       {::intervals [:P1 :M3 :P5 :M7 :M9]            ::aliases ["Δ9","^9", "major ninth"]}
   :maj13      {::intervals [:P1 :M3 :P5 :M7 :M9 :M13]       ::aliases ["Maj13","^13", "major thirteenth"]}
   :6          {::intervals [:P1 :M3 :P5 :M6]                ::aliases ["add6","add13","M6", "sixth"]}
   :6add9      {::intervals [:P1 :M3 :P5 :M6 :M9]            ::aliases ["6/9","69","M69", "sixth added ninth"]}
   :M7b6       {::intervals [:P1 :M3 :m6 :M7]                ::aliases ["^7b6", "major seventh flat sixth"]}
   :maj#4      {::intervals [:P1 :M3 :P5 :M7 :A11]           ::aliases ["Δ#4","Δ#11","M7#11","^7#11","maj7#11", "major seventh sharp eleventh"]}
   ;; Minor
   ;;; Normal
   :m          {::intervals [:P1 :m3 :P5]                    ::aliases ["min","-", "minor"]}
   :m7         {::intervals [:P1 :m3 :P5 :m7]                ::aliases ["min7","mi7","-7", "minor seventh"]}
   :mMaj7      {::intervals [:P1 :m3 :P5 :M7]                ::aliases ["m/maj7","mM7","m/M7","-Δ7","mΔ","-^7", "minor/major seventh"]}
   :m6         {::intervals [:P1 :m3 :P5 :M6]                ::aliases ["-6", "minor sixth"]}
   :m9         {::intervals [:P1 :m3 :P5 :m7 :M9]            ::aliases ["-9", "minor ninth"]}
   :mM9        {::intervals [:P1 :m3 :P5 :M7 :M9]            ::aliases ["mMaj9","-^9", "minor/major ninth"]}
   :m11        {::intervals [:P1 :m3 :P5 :m7 :M9 :P11]       ::aliases ["-11", "minor eleventh"]}
   :m13        {::intervals [:P1 :m3 :P5 :m7 :M9 :M13]       ::aliases ["-13", "minor thirteenth"]}
   ;;; Diminished
   :dim        {::intervals [:P1 :m3 :d5]                    ::aliases ["°", "diminished"]}
   :dim7       {::intervals [:P1 :m3 :d5 :d7]                ::aliases ["°7", "diminished seventh"]}
   :m7b5       {::intervals [:P1 :m3 :d5 :m7]                ::aliases ["ø", "-7b5", "half-diminished"]}
   ;; Dominant/Seventh
   ;;; Normal
   :7          {::intervals [:P1 :M3 :P5 :m7]                ::aliases ["dom", "dominant seventh"]}
   :9          {::intervals [:P1 :M3 :P5 :m7 :M9]            ::aliases ["dominant ninth"]}
   :13         {::intervals [:P1 :M3 :P5 :m7 :M9 :M13]       ::aliases ["dominant thirteenth"]}
   :7#11       {::intervals [:P1 :M3 :P5 :m7 :A11]           ::aliases ["7#4", "lydian dominant seventh"]}
   ;;; Altered
   :7b9        {::intervals [:P1 :M3 :P5 :m7 :m9]            ::aliases ["dominant flat ninth"]}
   :7#9        {::intervals [:P1 :M3 :P5 :m7 :A9]            ::aliases ["dominant sharp ninth"]}
   :alt7       {::intervals [:P1 :M3 :m7 :m9]                ::aliases ["altered"]}
   ;;; Suspended
   :sus4       {::intervals [:P1 :P4 :P5]                    ::aliases ["sus", "suspended fourth"]}
   :sus2       {::intervals [:P1 :M2 :P5]                    ::aliases ["suspended second"]}
   :7sus4      {::intervals [:P1 :P4 :P5 :m7]                ::aliases ["7sus", "suspended fourth seventh"]}
   :11         {::intervals [:P1 :P5 :m7 :M9 :P11]           ::aliases ["eleventh"]}
   :b9sus      {::intervals [:P1 :P4 :P5 :m7 :m9]            ::aliases ["phryg","7b9sus","7b9sus4", "suspended fourth flat ninth"]}
   ;; Other
   :P5         {::intervals [:P1 :P5]                        ::aliases ["fifth"]}
   :aug        {::intervals [:P1 :M3 :A5]                    ::aliases ["+","+5","^#5", "augmented"]}
   :m#5        {::intervals [:P1 :m3 :A5]                    ::aliases ["-#5","m+", "minor augmented"]}
   :maj7#5     {::intervals [:P1 :M3 :A5 :M7]                ::aliases ["maj7+5","+maj7","^7#5", "augmented seventh"]}
   :maj9#11    {::intervals [:P1 :M3 :P5 :M7 :M9 :A11]       ::aliases ["Δ9#11","^9#11", "major sharp eleventh (lydian)"]}
   :sus24      {::intervals [:P1 :M2 :P4 :P5]                ::aliases ["sus4add9"]}
   :maj9#5     {::intervals [:P1 :M3 :A5 :M7 :M9]            ::aliases ["Maj9#5"]}
   :7#5        {::intervals [:P1 :M3 :A5 :m7]                ::aliases ["+7","7+","7aug","aug7"]}
   :7#5#9      {::intervals [:P1 :M3 :A5 :m7 :A9]            ::aliases ["7#9#5","7alt"]}
   :9#5        {::intervals [:P1 :M3 :A5 :m7 :M9]            ::aliases ["9+"]}
   :9#5#11     {::intervals [:P1 :M3 :A5 :m7 :M9 :A11]}
   :7#5b9      {::intervals [:P1 :M3 :A5 :m7 :m9]            ::aliases ["7b9#5"]}
   :7#5b9#11   {::intervals [:P1 :M3 :A5 :m7 :m9 :A11]}
   :+add#9     {::intervals [:P1 :M3 :A5 :A9]}
   :M#5add9    {::intervals [:P1 :M3 :A5 :M9]                ::aliases ["+add9"]}
   :M6#11      {::intervals [:P1 :M3 :P5 :M6 :A11]           ::aliases ["M6b5","6#11","6b5"]}
   :M7add13    {::intervals [:P1 :M3 :P5 :M6 :M7 :M9]}
   :69#11      {::intervals [:P1 :M3 :P5 :M6 :M9 :A11]}
   :m69        {::intervals [:P1 :m3 :P5 :M6 :M9]            ::aliases ["-69"]}
   :7b6        {::intervals [:P1 :M3 :P5 :m6 :m7]}
   :maj7#9#11  {::intervals [:P1 :M3 :P5 :M7 :A9 :A11]}
   :M13#11     {::intervals [:P1 :M3 :P5 :M7 :M9 :A11 :M13]  ::aliases ["maj13#11","M13+4","M13#4"]}
   :M7b9       {::intervals [:P1 :M3 :P5 :M7 :m9]}
   :7#11b13    {::intervals [:P1 :M3 :P5 :m7 :A11 :m13]      ::aliases ["7b5b13"]}
   :7add6      {::intervals [:P1 :M3 :P5 :m7 :M13]           ::aliases ["67","7add13"]}
   :7#9#11     {::intervals [:P1 :M3 :P5 :m7 :A9 :A11]       ::aliases ["7b5#9","7#9b5"]}
   :13#9#11    {::intervals [:P1 :M3 :P5 :m7 :A9 :A11 :M13]}
   :7#9#11b13  {::intervals [:P1 :M3 :P5 :m7 :A9 :A11 :m13]}
   :13#9       {::intervals [:P1 :M3 :P5 :m7 :A9 :M13]}
   :7#9b13     {::intervals [:P1 :M3 :P5 :m7 :A9 :m13]}
   :9#11       {::intervals [:P1 :M3 :P5 :m7 :M9 :A11]       ::aliases ["9+4","9#4"]}
   :13#11      {::intervals [:P1 :M3 :P5 :m7 :M9 :A11 :M13]  ::aliases ["13+4","13#4"]}
   :9#11b13    {::intervals [:P1 :M3 :P5 :m7 :M9 :A11 :m13]  ::aliases ["9b5b13"]}
   :7b9#11     {::intervals [:P1 :M3 :P5 :m7 :m9 :A11]       ::aliases ["7b5b9","7b9b5"]}
   :13b9#11    {::intervals [:P1 :M3 :P5 :m7 :m9 :A11 :M13]}
   :7b9b13#11  {::intervals [:P1 :M3 :P5 :m7 :m9 :A11 :m13]  ::aliases ["7b9#11b13","7b5b9b13"]}
   :13b9       {::intervals [:P1 :M3 :P5 :m7 :m9 :M13]}
   :7b9b13     {::intervals [:P1 :M3 :P5 :m7 :m9 :m13]}
   :7b9#9      {::intervals [:P1 :M3 :P5 :m7 :m9 :A9]}
   :Madd9      {::intervals [:P1 :M3 :P5 :M9]                ::aliases ["2","add9","add2"]}
   :Maddb9     {::intervals [:P1 :M3 :P5 :m9]}
   :Mb5        {::intervals [:P1 :M3 :d5]}
   :13b5       {::intervals [:P1 :M3 :d5 :M6 :m7 :M9]}
   :M7b5       {::intervals [:P1 :M3 :d5 :M7]}
   :M9b5       {::intervals [:P1 :M3 :d5 :M7 :M9]}
   :7b5        {::intervals [:P1 :M3 :d5 :m7]}
   :9b5        {::intervals [:P1 :M3 :d5 :m7 :M9]}
   :7no5       {::intervals [:P1 :M3 :m7]}
   :7b13       {::intervals [:P1 :M3 :m7 :m13]}
   :9no5       {::intervals [:P1 :M3 :m7 :M9]}
   :13no5      {::intervals [:P1 :M3 :m7 :M9 :M13]}
   :9b13       {::intervals [:P1 :M3 :m7 :M9 :m13]}
   :madd4      {::intervals [:P1 :m3 :P4 :P5]}
   :mMaj7b6    {::intervals [:P1 :m3 :P5 :m6 :M7]}
   :mMaj9b6    {::intervals [:P1 :m3 :P5 :m6 :M7 :M9]}
   :m7add11    {::intervals [:P1 :m3 :P5 :m7 :P11]           ::aliases ["m7add4"]}
   :madd9      {::intervals [:P1 :m3 :P5 :M9]}
   :dim7M7     {::intervals [:P1 :m3 :d5 :M6 :M7]            ::aliases ["o7M7"]}
   :dimM7      {::intervals [:P1 :m3 :d5 :M7]                ::aliases ["oM7"]}
   :mb6M7      {::intervals [:P1 :m3 :m6 :M7]}
   :m7#5       {::intervals [:P1 :m3 :m6 :m7]}
   :m9#5       {::intervals [:P1 :m3 :m6 :m7 :M9]}
   :m11A       {::intervals [:P1 :m3 :A5 :m7 :M9 :P11]}
   :mb6b9      {::intervals [:P1 :m3 :m6 :m9]}
   :m9b5       {::intervals [:P1 :M2 :m3 :d5 :m7]}
   :M7#5sus4   {::intervals [:P1 :P4 :A5 :M7]}
   :M9#5sus4   {::intervals [:P1 :P4 :A5 :M7 :M9]}
   :7#5sus4    {::intervals [:P1 :P4 :A5 :m7]}
   :M7sus4     {::intervals [:P1 :P4 :P5 :M7]}
   :M9sus4     {::intervals [:P1 :P4 :P5 :M7 :M9]}
   :9sus4      {::intervals [:P1 :P4 :P5 :m7 :M9]            ::aliases ["9sus"]}
   :13sus4     {::intervals [:P1 :P4 :P5 :m7 :M9 :M13]       ::aliases ["13sus"]}
   :7sus4b9b13 {::intervals [:P1 :P4 :P5 :m7 :m9 :m13]       ::aliases ["7b9b13sus4"]}
   :P4         {::intervals [:P1 :P4 :m7 :m10]               ::aliases ["quartal"]}
   :11b9       {::intervals [:P1 :P5 :m7 :m9 :P11]}))

(s/def ::inversion (s/and int? #(<= 1 % 6))) ; 1st up to 6th chord inversion (e.g. 13th chord)
(s/def ::interval-mapping (s/map-of interval? pitch-or-note?))

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

; (def scales
;   (let [ionian-sequence     [2 2 1 2 2 2 1]
;         hex-sequence        [2 2 1 2 2 3]
;         pentatonic-sequence [3 2 2 3 2]
;         rotate (fn [scale-sequence offset]
;                  (take (count scale-sequence)
;                        (drop offset (cycle scale-sequence))))]
;     (array-map
;      :major              {::semitones ionian-sequence}
;      :dorian             {::semitones (rotate ionian-sequence 1)}
;      :phrygian           {::semitones (rotate ionian-sequence 2)}
;      :lydian             {::semitones (rotate ionian-sequence 3)}
;      :mixolydian         {::semitones (rotate ionian-sequence 4)}
;      :aeolian            {::semitones (rotate ionian-sequence 5)}
;      :minor              {::semitones (rotate ionian-sequence 5)}
;      :locrian            {::semitones (rotate ionian-sequence 6)}
;      :hex-major6         {::semitones (rotate hex-sequence 0)}
;      :hex-dorian         {::semitones (rotate hex-sequence 1)}
;      :hex-phrygian       {::semitones (rotate hex-sequence 2)}
;      :hex-major7         {::semitones (rotate hex-sequence 3)}
;      :hex-sus            {::semitones (rotate hex-sequence 4)}
;      :hex-aeolian        {::semitones (rotate hex-sequence 5)}
;      :minor-pentatonic   {::semitones (rotate pentatonic-sequence 0)}
;      :yu                 {::semitones (rotate pentatonic-sequence 0)}
;      :major-pentatonic   {::semitones (rotate pentatonic-sequence 1)}
;      :gong               {::semitones (rotate pentatonic-sequence 1)}
;      :egyptian           {::semitones (rotate pentatonic-sequence 2)}
;      :shang              {::semitones (rotate pentatonic-sequence 2)}
;      :jiao               {::semitones (rotate pentatonic-sequence 3)}
;      :zhi                {::semitones (rotate pentatonic-sequence 4)}
;      :ritusen            {::semitones (rotate pentatonic-sequence 4)}
;      :whole-tone         {::semitones [2 2 2 2 2 2]}
;      :chromatic          {::semitones [1 1 1 1 1 1 1 1 1 1 1 1]}
;      :harmonic-minor     {::semitones [2 1 2 2 1 3 1]}
;      :melodic-minor-asc  {::semitones [2 1 2 2 2 2 1]}
;      :hungarian-minor    {::semitones [2 1 3 1 1 3 1]}
;      :octatonic          {::semitones [2 1 2 1 2 1 2 1]}
;      :messiaen1          {::semitones [2 2 2 2 2 2]}
;      :messiaen2          {::semitones [1 2 1 2 1 2 1 2]}
;      :messiaen3          {::semitones [2 1 1 2 1 1 2 1 1]}
;      :messiaen4          {::semitones [1 1 3 1 1 1 3 1]}
;      :messiaen5          {::semitones [1 4 1 1 4 1]}
;      :messiaen6          {::semitones [2 2 1 1 2 2 1 1]}
;      :messiaen7          {::semitones [1 1 1 2 1 1 1 1 2 1]}
;      :super-locrian      {::semitones [1 2 1 2 2 2 2]}
;      :hirajoshi          {::semitones [2 1 4 1 4]}
;      :kumoi              {::semitones [2 1 4 2 3]}
;      :neapolitan-major   {::semitones [1 2 2 2 2 2 1]}
;      :bartok             {::semitones [2 2 1 2 1 2 2]}
;      :bhairav            {::semitones [1 3 1 2 1 3 1]}
;      :locrian-major      {::semitones [2 2 1 1 2 2 2]}
;      :ahirbhairav        {::semitones [1 3 1 2 2 1 2]}
;      :enigmatic          {::semitones [1 3 2 2 2 1 1]}
;      :neapolitan-minor   {::semitones [1 2 2 2 1 3 1]}
;      :pelog              {::semitones [1 2 4 1 4]}
;      :augmented2         {::semitones [1 3 1 3 1 3]}
;      :scriabin           {::semitones [1 3 3 2 3]}
;      :harmonic-major     {::semitones [2 2 1 2 1 3 1]}
;      :melodic-minor-desc {::semitones [2 1 2 2 1 2 2]}
;      :romanian-minor     {::semitones [2 1 3 1 2 1 2]}
;      :hindu              {::semitones [2 2 1 2 1 2 2]}
;      :iwato              {::semitones [1 4 1 4 2]}
;      :melodic-minor      {::semitones [2 1 2 2 2 2 1]}
;      :marva              {::semitones [1 3 2 1 2 2 1]}
;      :melodic-major      {::semitones [2 2 1 2 1 2 2]}
;      :indian             {::semitones [4 1 2 3 2]}
;      :spanish            {::semitones [1 3 1 2 1 2 2]}
;      :prometheus         {::semitones [2 2 2 5 1]}
;      :diminished         {::semitones [1 2 1 2 1 2 1 2] ::aliases ["half-whole diminished"]}
;      :diminished2        {::semitones [2 1 2 1 2 1 2 1] ::aliases ["whole-half diminished"]}
;      :todi               {::semitones [1 2 3 1 1 3 1]}
;      :leading-whole      {::semitones [2 2 2 2 2 1 1]}
;      :augmented          {::semitones [3 1 3 1 3 1]}
;      :purvi              {::semitones [1 3 2 1 1 3 1]}
;      :chinese            {::semitones [4 2 1 4 1]}
;      :lydian-minor       {::semitones [2 2 2 1 1 2 2]}
;      :minor-blues        {::semitones [3 2 1 1 3 2]}
;      :major-blues        {::semitones [2 1 1 3 2 3]})))

;; From tonal.js scale-type
(def scales
  (array-map
   ;; Basic
   :major {::intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7], ::aliases ["ionian"]}
   :minor {::intervals [:P1 :M2 :m3 :P4 :P5 :m6 :m7], ::aliases ["aeolian"]}
   :major-pentatonic {::intervals [:P1 :M2 :M3 :P5 :M6], ::aliases ["pentatonic"]}

   ;; Jazz common
   :major-blues {::intervals [:P1 :M2 :m3 :M3 :P5 :M6]}
   :minor-blues {::intervals [:P1 :m3 :P4 :d5 :P5 :m7], ::aliases ["blues"]}
   :melodic-minor {::intervals [:P1 :M2 :m3 :P4 :P5 :M6 :M7]}
   :harmonic-minor {::intervals [:P1 :M2 :m3 :P4 :P5 :m6 :M7]}
   :bebop {::intervals [:P1 :M2 :M3 :P4 :P5 :M6 :m7 :M7]}
   :diminished {::intervals [:P1 :M2 :m3 :P4 :d5 :m6 :M6 :M7], ::aliases ["whole-half diminished"]}

   ;; Modes
   :dorian {::intervals [:P1 :M2 :m3 :P4 :P5 :M6 :m7]}
   :lydian {::intervals [:P1 :M2 :M3 :A4 :P5 :M6 :M7]}
   :mixolydian {::intervals [:P1 :M2 :M3 :P4 :P5 :M6 :m7], ::aliases ["dominant"]}
   :phrygian {::intervals [:P1 :m2 :m3 :P4 :P5 :m6 :m7]}
   :locrian {::intervals [:P1 :m2 :m3 :P4 :d5 :m6 :m7]}

   ;; 5-note
   :ionian-pentatonic {::intervals [:P1 :M3 :P4 :P5 :M7]}
   :mixolydian-pentatonic {::intervals [:P1 :M3 :P4 :P5 :m7], ::aliases ["indian"]}
   :ritusen {::intervals [:P1 :M2 :P4 :P5 :M6]}
   :egyptian {::intervals [:P1 :M2 :P4 :P5 :m7]}
   :neopolitan-major-pentatonic {::intervals [:P1 :M3 :P4 :d5 :m7]}
   :vietnamese-1 {::intervals [:P1 :m3 :P4 :P5 :m6]}
   :pelog {::intervals [:P1 :m2 :m3 :P5 :m6]}
   :kumoijoshi {::intervals [:P1 :m2 :P4 :P5 :m6]}
   :hirajoshi {::intervals [:P1 :M2 :m3 :P5 :m6]}
   :iwato {::intervals [:P1 :m2 :P4 :d5 :m7]}
   :in-sen {::intervals [:P1 :m2 :P4 :P5 :m7]}
   :lydian-pentatonic {::intervals [:P1 :M3 :A4 :P5 :M7], ::aliases ["chinese"]}
   :malkos-raga {::intervals [:P1 :m3 :P4 :m6 :m7]}
   :locrian-pentatonic {::intervals [:P1 :m3 :P4 :d5 :m7], ::aliases ["minor seven flat five pentatonic"]}
   :minor-pentatonic {::intervals [:P1 :m3 :P4 :P5 :m7], ::aliases ["vietnamese 2"]}
   :minor-six-pentatonic {::intervals [:P1 :m3 :P4 :P5 :M6]}
   :flat-three-pentatonic {::intervals [:P1 :M2 :m3 :P5 :M6], ::aliases ["kumoi"]}
   :flat-six-pentatonic {::intervals [:P1 :M2 :M3 :P5 :m6]}
   :scriabin {::intervals [:P1 :m2 :M3 :P5 :M6]}
   :whole-tone-pentatonic {::intervals [:P1 :M3 :d5 :m6 :m7]}
   :lydian-#5P-pentatonic {::intervals [:P1 :M3 :A4 :A5 :M7]}
   :lydian-dominant-pentatonic {::intervals [:P1 :M3 :A4 :P5 :m7]}
   :minor-#7M-pentatonic {::intervals [:P1 :m3 :P4 :P5 :M7]}
   :super-locrian-pentatonic {::intervals [:P1 :m3 :d4 :d5 :m7]}

   ;; 6-note
   :minor-hexatonic {::intervals [:P1 :M2 :m3 :P4 :P5 :M7]}
   :augmented {::intervals [:P1 :A2 :M3 :P5 :A5 :M7]}
   :piongio {::intervals [:P1 :M2 :P4 :P5 :M6 :m7]}
   :prometheus-neopolitan {::intervals [:P1 :m2 :M3 :A4 :M6 :m7]}
   :prometheus {::intervals [:P1 :M2 :M3 :A4 :M6 :m7]}
   :mystery-#1 {::intervals [:P1 :m2 :M3 :d5 :m6 :m7]}
   :six-tone-symmetric {::intervals [:P1 :m2 :M3 :P4 :A5 :M6]}
   :whole-tone {::intervals [:P1 :M2 :M3 :A4 :A5 :A6], ::aliases ["messiaen's mode #1"]}
   :messiaen's-mode-#5 {::intervals [:P1 :m2 :P4 :A4 :P5 :M7]}

   ;; 7-note
   :locrian-major {::intervals [:P1 :M2 :M3 :P4 :d5 :m6 :m7], ::aliases ["arabian"]}
   :double-harmonic-lydian {::intervals [:P1 :m2 :M3 :A4 :P5 :m6 :M7]}
   :altered {::intervals [:P1 :m2 :A2 :M3 :A4 :m6 :m7], ::aliases ["super locrian" "diminished whole tone" "pomeroy"]}
   :locrian-#2 {::intervals [:P1 :M2 :m3 :P4 :d5 :m6 :m7], ::aliases ["half-diminished" "aeolian b5"]}
   :mixolydian-b6 {::intervals [:P1 :M2 :M3 :P4 :P5 :m6 :m7], ::aliases ["melodic minor fifth mode" "hindu"]}
   :lydian-dominant {::intervals [:P1 :M2 :M3 :A4 :P5 :M6 :m7], ::aliases ["lydian b7" "overtone"]}
   :lydian-augmented {::intervals [:P1 :M2 :M3 :A4 :A5 :M6 :M7]}
   :dorian-b2 {::intervals [:P1 :m2 :m3 :P4 :P5 :M6 :m7], ::aliases ["phrygian #6" "melodic minor second mode"]}
   :ultralocrian {::intervals [:P1 :m2 :m3 :d4 :d5 :m6 :d7], ::aliases ["superlocrian bb7" "superlocrian diminished"]}
   :locrian-6 {::intervals [:P1 :m2 :m3 :P4 :d5 :M6 :m7], ::aliases ["locrian natural 6" "locrian sharp 6"]}
   :augmented-heptatonic {::intervals [:P1 :A2 :M3 :P4 :P5 :A5 :M7]}
   :dorian-#4 {::intervals [:P1 :M2 :m3 :A4 :P5 :M6 :m7], ::aliases ["ukrainian dorian" "romanian minor" "altered dorian"]}
   :lydian-diminished {::intervals [:P1 :M2 :m3 :A4 :P5 :M6 :M7]}
   :leading-whole-tone {::intervals [:P1 :M2 :M3 :A4 :A5 :m7 :M7]}
   :lydian-minor {::intervals [:P1 :M2 :M3 :A4 :P5 :m6 :m7]}
   :phrygian-dominant {::intervals [:P1 :m2 :M3 :P4 :P5 :m6 :m7], ::aliases ["spanish" "phrygian major"]}
   :balinese {::intervals [:P1 :m2 :m3 :P4 :P5 :m6 :M7]}
   :neopolitan-major {::intervals [:P1 :m2 :m3 :P4 :P5 :M6 :M7]}
   :harmonic-major {::intervals [:P1 :M2 :M3 :P4 :P5 :m6 :M7]}
   :double-harmonic-major {::intervals [:P1 :m2 :M3 :P4 :P5 :m6 :M7], ::aliases ["gypsy"]}
   :hungarian-minor {::intervals [:P1 :M2 :m3 :A4 :P5 :m6 :M7]}
   :hungarian-major {::intervals [:P1 :A2 :M3 :A4 :P5 :M6 :m7]}
   :oriental {::intervals [:P1 :m2 :M3 :P4 :d5 :M6 :m7]}
   :flamenco {::intervals [:P1 :m2 :m3 :M3 :A4 :P5 :m7]}
   :todi-raga {::intervals [:P1 :m2 :m3 :A4 :P5 :m6 :M7]}
   :persian {::intervals [:P1 :m2 :M3 :P4 :d5 :m6 :M7]}
   :enigmatic {::intervals [:P1 :m2 :M3 :d5 :m6 :m7 :M7]}
   :major-augmented {::intervals [:P1 :M2 :M3 :P4 :A5 :M6 :M7], ::aliases ["major #5" "ionian augmented" "ionian #5"]}
   :lydian-#9 {::intervals [:P1 :A2 :M3 :A4 :P5 :M6 :M7]}

   ;; 8-note
   :messiaen's-mode-#4 {::intervals [:P1 :m2 :M2 :P4 :A4 :P5 :m6 :M7]}
   :purvi-raga {::intervals [:P1 :m2 :M3 :P4 :A4 :P5 :m6 :M7]}
   :spanish-heptatonic {::intervals [:P1 :m2 :m3 :M3 :P4 :P5 :m6 :m7]}
   :bebop-minor {::intervals [:P1 :M2 :m3 :M3 :P4 :P5 :M6 :m7]}
   :bebop-major {::intervals [:P1 :M2 :M3 :P4 :P5 :A5 :M6 :M7]}
   :bebop-locrian {::intervals [:P1 :m2 :m3 :P4 :d5 :P5 :m6 :m7]}
   :bebop-minor {::intervals [:P1 :M2 :m3 :P4 :P5 :m6 :m7 :M7]}
   :ichikosucho {::intervals [:P1 :M2 :M3 :P4 :d5 :P5 :M6 :M7]}
   :minor-six-diminished {::intervals [:P1 :M2 :m3 :P4 :P5 :m6 :M6 :M7]}
   :half-whole-diminished {::intervals [:P1 :m2 :m3 :M3 :A4 :P5 :M6 :m7], ::aliases ["dominant diminished" "messiaen's mode #2"]}
   :kafi-raga {::intervals [:P1 :m3 :M3 :P4 :P5 :M6 :m7 :M7]}
   :messiaen's-mode-#6 {::intervals [:P1 :M2 :M3 :P4 :A4 :A5 :A6 :M7]}

   ;; 9-note
   :composite-blues {::intervals [:P1 :M2 :m3 :M3 :P4 :d5 :P5 :M6 :m7]}
   :messiaen's-mode-#3 {::intervals [:P1 :M2 :m3 :M3 :A4 :P5 :m6 :m7 :M7]}

   ;; 10-note
   :messiaen's-mode-#7 {::intervals [:P1 :m2 :M2 :m3 :P4 :A4 :P5 :m6 :M6 :M7]}

   ;; 12-note
   :chromatic {::intervals [:P1 :m2 :M2 :m3 :M3 :P4 :d5 :P5 :m6 :M6 :m7 :M7]}))

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
(s/def ::degree (s/and keyword? #(contains? degrees %)))
(s/def ::degree-base-scale (s/and keyword? #(contains? scales %)))

;; Key (signature)
