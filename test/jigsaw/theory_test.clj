(ns jigsaw.theory-test
  (:require
   [clojure.test :refer [deftest testing]]
   [clojure.spec.alpha :as s]
   [jigsaw.core :as jigsaw]
   [jigsaw.impl.theory :as theory]
   [jigsaw.test-utils :refer [are+]]))

(deftest theory-test
  (testing "specifications"
    (testing "with semitones"
      (are+ [value valid] (= valid (s/valid? ::theory/semitones value))
        -1 false
        0 true
        21 true
        22 false
        :C  false)))
  (testing "with pitch"
    (are+ [value valid] (= valid (s/valid? ::theory/pitch value))
      1 false
      "a" false
      :C true
      :c false
      :C# true
      :C## true
      :Db true
      :Dbb true))
  (testing "with pci"
    (are+ [p1 p2] (= (theory/pitches p1) (theory/pitches p2))
      :C :C
      :C :Dbb
      :C :B#)
    (are+ [p pci] (= pci (theory/pitches p))
      :C 0
      :C# 1
      :C## 2
      :D 2
      :Ebb 2
      :Eb 3))
  (testing "with interval"
    (are+ [value valid] (= valid (s/valid? ::theory/interval value))
      :P1 true
      :13 false
      :M13 true))
  (testing "with note"
    (are+ [value valid] (= valid (s/valid? ::theory/note value))
      :C false
      :C2 true
      :c2 false
      :C#2 true
      :C##2 true
      :Db2 true
      :D11 false
      :T2 false))
  (testing "with shape-ref"
    (are+ [m valid] (= valid (s/valid? ::theory/shape-ref m))
      {:pitch :C :name :maj} true
      {:pitch :C :name :major} true
      {:note :C4 :name :major} true
      {:name :major} false
      {:pitch :C} false
      {:pitch :C :name :maj} true))
  (testing "with shape-blueprint"
    (are+ [m valid] (= valid (s/valid? ::theory/shape-blueprint m))
      {:name :maj :intervals [:P1 :M3 :P5]} true
      {:name :maj} false))
  (testing "with shape"
    (are+ [m valid] (= valid (s/valid? ::theory/shape m))
      {:pitch :C :name :maj :intervals [:P1 :M3 :P5] :pitches [:C :E :G]} true
      {:note :C4 :name :maj :intervals [:P1 :M3 :P5] :pitches [:C :E :G] :notes [:C4 :E4 :G4]} true
      {:name :maj} false))
  (testing "with chord"
    (are+ [m valid] (= valid (s/valid? ::theory/chord m))
      {:pitch :C :name :maj :intervals [:P1 :M3 :P5] :pitches [:C :E :G]} true
      {:pitch :C :name :major :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7] :pitches [:C :D :E :F :G :A :B]} false))
  (testing "with scale"
    (are+ [m valid] (= valid (s/valid? ::theory/scale m))
      {:pitch :C :name :maj :intervals [:P1 :M3 :P5] :pitches [:C :E :G]} false
      {:pitch :C :name :major :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7] :pitches [:C :D :E :F :G :A :B]} true))
  ; TBD
  #_(testing "with context"
      (are+ [m valid] (= valid (s/valid? ::theory/context m))
        ; Single context link; i.e. current shape (not shown) came from this
        {:pitch :C
         :name :maj
         :intervals [:P1 :M3 :P5]
         :pitches [:C :E :G]
         ; Original shape not shown, degree of 1 is random here
         :degree :I} true
        ; Context chain, two links; i.e. current shape came from this which came from another shape
        {:pitch :C
         :name :maj
         :intervals [:P1 :M3 :P5]
         :pitches [:C :E :G]
         :degree :I  ; Cmaj = first degree of the C major scale
         :context {:pitch :C
                   :name :major
                   :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                   :pitches [:C :D :E :F :G :A :B]
                   :degrees [:1 :2 :3 :4 :5 :6 :7]
                   ; Original shape not shown, degree of 2 is random here
                   :degree :ii}} true
        ; Context chain, three links; i.e. current shape came from this which came from another shape
        {:pitch :C
         :name :maj
         :intervals [:P1 :M3 :P5]
         :pitches [:C :E :G]
         :degree :I  ; Cmaj is the first degree of the C major scale
         :context {:pitch :C
                   :name :major
                   :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                   :pitches [:C :D :E :F :G :A :B]
                   :degrees [:1 :2 :3 :4 :5 :6 :7]
                   :degree :ii  ; Dm is the second degree of the C major scale (:degree is always the chord's degree, even if the current context is a scale)
                   :context {:pitch :D
                             :name :m
                             :intervals [:P1 :m3 :P5]
                             :pitches [:D :F :A]
                             ; Original shape not shown, degree of 3 is random here
                             :degree :iii}}} true))
  #_(testing "with scale-chord"
      (are+ [m valid] (= valid (s/valid? ::theory/scale-chord m))
        ; Single context; no scale origin
        {:pitch :C
         :name :maj
         :intervals [:P1 :M3 :P5]
         :pitches [:C :E :G]
         ; Original shape not shown, degree of 1 is random here
         :degree :I} false
        ; Context chain; chord with scale origin
        {:pitch :C
         :name :maj
         :intervals [:P1 :M3 :P5]
         :pitches [:C :E :G]
         :degree :I
         :context {:pitch :C
                   :name :major
                   :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                   :pitches [:C :D :E :F :G :A :B]
                   :degrees [:1 :2 :3 :4 :5 :6 :7]
                   ; Original shape not shown, degree of 2 is random here
                   :degree :ii}} true))
  #_(testing "with chord-scale"
      (are+ [m valid] (= valid (s/valid? ::theory/chord-scale m))
        ; Single context; no scale origin
        {:pitch :C
         :name :major
         :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
         :pitches [:C :D :E :F :G :A :B]
         :degrees [:1 :2 :3 :4 :5 :6 :7]
         ; Random degree
         :degree :I} false
        ; Context chain; scale with chord origin
        {:pitch :C
         :name :major
         :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
         :pitches [:C :D :E :F :G :A :B]
         :degrees [:1 :2 :3 :4 :5 :6 :7]
         :degree :I
         :context {:pitch :C
                   :name :maj
                   :intervals [:P1 :M3 :P5]
                   :pitches [:C :E :G]
                   :degree :ii}} true))
  (testing "arithmetic"
    (testing "parts"
      (testing "starting from a pitch"
        (are+ [p m] (= m (theory/parts p))
          :C   {:pitch :C   :letter \C :accidental "" :pci 0}
          :C#  {:pitch :C#  :letter \C :accidental "#" :pci 1}
          :C## {:pitch :C## :letter \C :accidental "##" :pci 2}
          :Dbb {:pitch :Dbb :letter \D :accidental "bb" :pci 0}))
      (testing "starting from a note"
        (are+ [n m] (= m (theory/parts n))
          :C4   {:pitch :C   :letter \C :accidental ""   :octave 4 :note :C4 :pci 0}
          :C#4  {:pitch :C#  :letter \C :accidental "#"  :octave 4 :note :C#4 :pci 1}
          :C##4 {:pitch :C## :letter \C :accidental "##" :octave 4 :note :C##4 :pci 2}
          :Dbb4 {:pitch :Dbb :letter \D :accidental "bb" :octave 4 :note :Dbb4 :pci 0})))
    (testing "with fold-notes"
      (are+ [notes want] (= want (theory/fold-notes notes))
        [:C4] [:C4]
        [:C4 :C5] [:C4 :C5]
        [:C4 :C5 :C6] [:C4 :C5]
        [:C4 :C5 :C6 :C7] [:C4 :C5]
        ; 21 semitones, don't fold
        [:C4 :A5] [:C4 :A5]
        ; 22, max-semitones surpassed; fold back into last octave
        [:C4 :Bb5] [:C4 :Bb4]))
    (testing "with semitone-distance"
      (testing "with pitches"
        (are+ [p1 p2 want] (= want (theory/semitone-distance p1 p2))
          :C :C 12
          :C :Db 1
          :C :C# 1
          :C :D 2
          :C :B 11
          :C :B# 12))
      (testing "with notes"
        (are+ [n1 n2 want] (= want (theory/semitone-distance n1 n2))
          :C4 :C4 0
          :C4 :Db4 1
          :C4 :C#4 1
          :C4 :D4 2
          :C4 :B4 11
          :C4 :B#4 12 ; B#4 is enharmonically equivalent to C5
          :C4 :C5 12
          :C4 :E5 16
          :C4 :E6 28
          :C4 :A5 21
          :C4 :A#5 22)))
    (testing "with flat?"
      (are+ [p want] (= want (theory/flat? p))
        :C  false
        :C# false
        :Db true
        :Ab true))
    (testing "with natural?"
      (are+ [p want] (= want (theory/natural? p))
        :C  true
        :C# false
        :Db false
        :A  true))
    (testing "with sharp?"
      (are+ [p want] (= want (theory/sharp? p))
        :C  false
        :C# true
        :Db false
        :Ab false))
    (testing "with enharmonic"
      (are+ [p notation want] (= want (theory/enharmonic-equivalent p notation))
        :C :flat   :C
        :C :sharp  :B#
        :Dbb :natural :C
        :C# :sharp :C#
        :C# :flat  :Db))
    (testing "with ->interval"
      (testing "starting with a pitch"
        (are+ [p1 p2 want] (= want (theory/->interval p1 p2))
          :C :C# :m2
          :C :Db :m2
          :C :D  :M2
          :C :D# :A2
          :C :Eb :m3
          :C :E  :M3
          :C :E# :A3
          :C :Fb :d4
          :C :F  :P4
          :C :F# :A4
          :C :Gb :d5
          :C :G  :P5
          :C :G# :A5
          :C :Ab :m6
          :C :A  :M6
          :C :A# :A6
          :C :Bb :m7
          :C :B  :M7
          :C :B# :A7
          :C :C  :P8
          :B :C :m2
          :B :E :P4
          :B :F :d5))
      (testing "starting from a note"
        (are+ [n1 n2 want] (= want (theory/->interval n1 n2))
          :C4 :C4 :P1
          :C4 :C#4 :m2
          :C4 :Db4 :m2
          :C4 :D4 :M2
          :C4 :D#4 :A2
          :C4 :Eb4 :m3
          :C4 :E4 :M3
          :C4 :E#4 :A3
          :C4 :Fb4 :d4
          :C4 :F4 :P4
          :C4 :F#4 :A4
          :C4 :Gb4 :d5
          :C4 :G4 :P5
          :C4 :G#4 :A5
          :C4 :Ab4 :m6
          :C4 :A4 :M6
          :C4 :A#4 :A6
          :C4 :Bb4 :m7
          :C4 :B4 :M7
          :C4 :B#4 :A7
          :C4 :C5 :P8
          :C4 :E5 :M10
          ; 21 semitones (13th) ceiling reached
          :C4 :A5 :M13
          ; 22 semitones; ceiling surpassed; fold down octave
          :C4 :A#5 :A6
          ; First note higher than second, bump second note's octave up
          :D4 :C4 :m7)))
    (testing "with note->midi"
      (are+ [note want] (= want (theory/note->midi note))
        :C4  60
        :C#4 61
        :Db4 61
        :Cb4 59 ; In octave 4, but Cb4 is enharmonically equivalent to B3
        :C0  12))
    (testing "with midi->note"
      (are+ [midi want] (= want (theory/midi->note midi nil))
        60 :C4
        61 :C#4
        62 :D4
        63 :Eb4
        12 :C0))
    (testing "with transpose"
      (testing "starting from a pitch"
        (testing "adding"
          (are+ [p interval want] (= want (theory/transpose p interval))
            :C :P1  :C
            :C :d2  :Dbb
            :C :m2  :Db
            :C :M2  :D
            :C :d3  :Ebb
            :C :m3  :Eb
            :C :A2  :D#
            :C :M3  :E
            :C :d4  :Fb
            :C :P4  :F
            :C :A3  :E#
            :C :d5  :Gb
            :C :A4  :F#
            :C :P5  :G
            :C :d6  :Abb
            :C :m6  :Ab
            :C :A5  :G#
            :C :M6  :A
            :C :d7  :Bbb
            :C :m7  :Bb
            :C :A6  :A#
            :C :M7  :B
            :C :P8  :C
            :C :P11 :F
            :C :M13 :A

            :C# :P1 :C#
            :C# :d2 :Db
            :C# :m2 :D
            :C# :M2 :D#
            :C# :d3 :Eb
            :C# :m3 :E
            :C# :A2 :D##
            :C# :M3 :E#
            :C# :d4 :F
            :C# :P4 :F#
            :C# :A3 :E##
            :C# :d5 :G
            :C# :A4 :F##
            :C# :P5 :G#
            :C# :d6 :Ab
            :C# :m6 :A
            :C# :A5 :G##
            :C# :M6 :A#
            :C# :d7 :Bb
            :C# :m7 :B
            :C# :A6 :A##
            :C# :M7 :B#

            :F :d4 :Bbb
            :F :P5 :C

            :B# :A2 :D#  ; technically C### but it's clamped
            :B# :A3 :E#  ; technically D###
            :B# :A4 :E##
            :B# :A5 :G#  ; technically F###
            :B# :A6 :A#  ; technically G###
            :B# :A7 :B#  ; technically A###
            :B# :A8 :B##))
        (testing "subtracting"
          (are+ [p interval want] (= want (theory/transpose p interval -1))
            :C :d2  :B#
            :C :m2  :B
            :C :M2  :Bb
            :C :d3  :A#
            :C :m3  :A
            :C :A2  :Bbb
            :C :M3  :Ab
            :C :d4  :G#
            :C :P4  :G
            :C :A3  :Abb
            :C :d5  :F#
            :C :A4  :Gb
            :C :P5  :F
            :C :d6  :E#
            :C :m6  :E
            :C :A5  :Fb
            :C :M6  :Eb
            :C :d7  :D#
            :C :m7  :D
            :C :A6  :Ebb
            :C :M7  :Db
            :C :P8  :C
            :C :P11 :G
            :C :M13 :Eb

            :C# :P1 :C#
            :C# :d2 :B##
            :C# :m2 :B#
            :C# :M2 :B
            :C# :d3 :A##
            :C# :m3 :A#
            :C# :A2 :Bb
            :C# :M3 :A
            :C# :d4 :G##
            :C# :P4 :G#
            :C# :A3 :Ab
            :C# :d5 :F##
            :C# :A4 :G
            :C# :P5 :F#
            :C# :d6 :E##
            :C# :m6 :E#
            :C# :A5 :F
            :C# :M6 :E
            :C# :d7 :D##
            :C# :m7 :D#
            :C# :A6 :Eb
            :C# :M7 :D
            :C# :P8 :C#)))
      (testing "starting from a note"
        (testing "adding"
          (are+ [n interval want] (= want (theory/transpose n interval))
            :C4 :P1  :C4
            :C4 :P8  :C5
            :C4 :P11 :F5
            :D#4 :M6 :B#4
            :Db4 :m7 :Cb5  ; If going up to boundary pitch, increment octave
            :F#4 :A5 :C##5
            :B#4 :M3 :D##5))
        (testing "subtracting"
          (are+ [n interval want] (= want (theory/transpose n interval -1))
            :C4 :P1  :C4
            :C4 :P8  :C3
            :C4 :P11 :G2
            :D##5 :M3 :B#4))))
    (testing "with clamp-pitch"
      (are+ [p want] (= want (theory/clamp-pitch p))
        :C      :C
        :C#     :C#
        :C##    :C##
        :C###   :D#
        :C####  :D##
        :C##### :E#
        :Db     :Db
        :Dbb    :Dbb
        :Dbbb   :Cb
        :Dbbbb  :Cbb
        :Dbbbbb :Bb))
    (testing "with interval->degree"
      (are+ [interval want] (= want (theory/interval->degree interval))
        :P1 :1
        :d2 :b2
        :m2 :b2
        :M2 :2
        :d3 :b3
        :m3 :b3
        :A2 :#2
        :M3 :3
        :d4 :b4
        :P4 :4
        :A3 :#3
        :d5 :b5
        :A4 :#4
        :P5 :5
        :d6 :b6
        :m6 :b6
        :A5 :#5
        :M6 :6
        :d7 :b7
        :m7 :b7
        :A6 :#6
        :M7 :7))
    (testing "with roman-numeral->int"
      (are+ [degree want] (= want (theory/roman-numeral->int degree))
        :I 1
        :i 1
        :I7 1
        :ii 2
        :bii 2
        :#ii 2
        :iii 3
        :viio 7
        :iio 2
        :bIII+ 3))
    (testing "with note->abc"
      (are+ [note want] (= want (theory/note->abc note))
        :C4 "C"
        :C#4 "^C"
        :C##4 "^^C"
        :Db4 "_D"
        :Dbb4 "__D"
        :C5 "c"
        :C6 "c'"
        :C7 "c''"
        :C8 "c'''"
        :C3 "C,"
        :C2 "C,,"
        :C1 "C,,,"
        :C0 "C,,,,")))

  (testing "heuristics"
    (are+ [set1 set2 m] (= m (theory/calculate-heuristics set1 set2))
      [] [] {:contained-in? 1
             :fully-contained-in? 0
             :contains? 1
             :fully-contains? 0
             :overlap 0.0
             :same-pitch-count? 1
             :shares-root? 0}
      [:C] [] {:contained-in? 0
               :fully-contained-in? 0
               :contains? 1
               :fully-contains? 1
               :overlap 0.0
               :same-pitch-count? 0
               :shares-root? 0}
      [] [:C] {:contained-in? 1
               :fully-contained-in? 1
               :contains? 0
               :fully-contains? 0
               :overlap 0.0
               :same-pitch-count? 0
               :shares-root? 0}
      [:C] [:C] {:contained-in? 1
                 :fully-contained-in? 0
                 :contains? 1
                 :fully-contains? 0
                 :overlap 1.0
                 :same-pitch-count? 1
                 :shares-root? 1}
      [:C] [:C :D] {:contained-in? 1
                    :fully-contained-in? 1
                    :contains? 0
                    :fully-contains? 0
                    :overlap 0.5
                    :same-pitch-count? 0
                    :shares-root? 1}
      [:C :D] [:C] {:contained-in? 0
                    :fully-contained-in? 0
                    :contains? 1
                    :fully-contains? 1
                    :overlap 0.5
                    :same-pitch-count? 0
                    :shares-root? 1}
      [:C :D :E] [:C] {:contained-in? 0
                       :fully-contained-in? 0
                       :contains? 1
                       :fully-contains? 1
                       :overlap (float (/ 1 3))
                       :same-pitch-count? 0
                       :shares-root? 1}))

  (testing "scale->mode"
    (are+ [base-scale-name mode-num want-scale-name] (= want-scale-name (:name (theory/scale->mode (jigsaw/->shape :C base-scale-name) mode-num)))
      :major 0 :major
      :major 1 :dorian
      :major 2 :phrygian
      :major 3 :lydian
      :major 4 :mixolydian
      :major 5 :minor
      :major 6 :locrian
      :major 7 :major
      :melodic-minor 1 :dorian-b2
      :melodic-minor 2 :lydian-augmented
      :melodic-minor 3 :lydian-dominant
      :melodic-minor 4 :mixolydian-b6
      :melodic-minor 5 :locrian-#2
      :melodic-minor 6 :altered)))
