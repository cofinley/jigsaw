(ns jigsaw.algo-test
  (:require
   [clojure.test :refer [deftest testing is are]]
   [jigsaw.algo :as algo]
   [jigsaw.spec :as specs]
   [clojure.template :as temp]))

(defmacro are+
  "are but with assertion message like with `is`"
  [argv expr & args]
  (if (or
       (and (empty? argv) (empty? args))
       (and (pos? (count argv))
            (pos? (count args))
            (zero? (mod (count args) (count argv)))))
    `(temp/do-template ~argv (is ~expr (str '~expr " => " ~expr)) ~@args)
    (throw (IllegalArgumentException. "The number of args doesn't match are's argv."))))

(deftest algo-test
  (testing "Algo"
    (testing "parts"
      (testing "starting from a pitch"
        (are+ [p m] (= m (algo/parts p))
          :C   {:pitch :C   :letter \C :accidental ""}
          :C#  {:pitch :C#  :letter \C :accidental "#"}
          :C## {:pitch :C## :letter \C :accidental "##"}
          :Dbb {:pitch :Dbb :letter \D :accidental "bb"}))
      (testing "starting from a note"
        (are+ [n m] (= m (algo/parts n))
          :C4   {:pitch :C   :letter \C :accidental ""   :octave 4 :note :C4}
          :C#4  {:pitch :C#  :letter \C :accidental "#"  :octave 4 :note :C#4}
          :C##4 {:pitch :C## :letter \C :accidental "##" :octave 4 :note :C##4}
          :Dbb4 {:pitch :Dbb :letter \D :accidental "bb" :octave 4 :note :Dbb4})))
    (testing "with fold-notes"
      (are+ [notes want] (= want (algo/fold-notes notes))
        [:C4] [:C4]
        [:C4 :C5] [:C4 :C5]
        [:C4 :C5 :C6] [:C4 :C5]
        [:C4 :C5 :C6 :C7] [:C4 :C5]
        ; 21 semitones, don't fold
        [:C4 :A5] [:C4 :A5]
        ; 22, fold back into last octave
        [:C4 :Bb5] [:C4 :Bb4]))
    (testing "with semitone-distance"
      (testing "with pitches"
        (are+ [p1 p2 want] (= want (algo/semitone-distance p1 p2))
          :C :C 12
          :C :Db 1
          :C :C# 1
          :C :D 2
          :C :B 11
          :C :B# 12))
      (testing "with notes"
        (are+ [n1 n2 want] (= want (algo/semitone-distance n1 n2))
          :C4 :C4 0
          :C4 :Db4 1
          :C4 :C#4 1
          :C4 :D4 2
          :C4 :B4 11
          :C4 :B#4 12 ; B#4 is enharmonically equivalent to C5
          :C4 :C5 12
          :C4 :E5 16
          :C4 :E6 16
          ; 21 ceiling reached (13th)
          :C4 :A5 21
          ; 22 folded down octave/12 semitones
          :C4 :A#5 10)))
    (testing "with flat?"
      (are+ [p want] (= want (algo/flat? p))
        :C  false
        :C# false
        :Db true
        :Ab true))
    (testing "with natural?"
      (are+ [p want] (= want (algo/natural? p))
        :C  true
        :C# false
        :Db false
        :A  true))
    (testing "with sharp?"
      (are+ [p want] (= want (algo/sharp? p))
        :C  false
        :C# true
        :Db false
        :Ab false))
    (testing "with enharmonic"
      (are+ [p notation want] (= want (algo/enharmonic p notation))
        :C :flat   :C
        :C :sharp  :B#
        :Dbb :natural :C
        :C# :sharp :C#
        :C# :flat  :Db))
    (testing "with ->interval"
      (testing "starting with a pitch"
        (are+ [p1 p2 want] (= want (algo/->interval p1 p2))
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
        (are+ [n1 n2 want] (= want (algo/->interval n1 n2))
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
          :C4 :E5 :M10
          ; 21 semitone ceiling reached
          :C4 :A5 :M13
          ; 22 semitones, fold down octave
          :C4 :A#5 :A6)))
    (testing "with note->midi"
      (are+ [note want] (= want (algo/note->midi note))
        :C4  60
        :C#4 61
        :Db4 61
        :Cb4 59 ; In octave 4, but Cb4 is enharmonically equivalent to B3
        :C0  12))
    (testing "with midi->note"
      (are+ [midi want] (= want (algo/midi->note midi nil))
        60 :C4
        61 :C#4
        62 :D4
        63 :Eb4
        12 :C0))
    (testing "with +interval"
      (testing "starting from a pitch"
        (testing "adding"
          (are+ [p interval want] (= want (algo/+interval p interval))
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
            :F :P5 :C))
        (testing "subtracting"
          (are+ [p interval want] (= want (algo/+interval p interval -1))
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
          (are+ [n interval want] (= want (algo/+interval n interval))
            :C4 :P1  :C4
            :C4 :P8  :C5
            :C4 :P11 :F5
            :D#4 :M6 :B#4
            :Db4 :m7 :Cb5  ; If going up to boundary pitch, increment octave
            :F#4 :A5 :C##5))
        (testing "subtracting"
          (are+ [n interval want] (= want (algo/+interval n interval -1))
            :C4 :P1  :C4
            :C4 :P8  :C3
            :C4 :P11 :G3))))
    (testing "with clamp-pitch"
      (are+ [p want] (= want (algo/clamp-pitch p))
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
    (testing "with resolve-shape"
      (testing "starting from a chord"
        (testing "starting from a pitch"
          (are+ [pitch chord-name want] (= want (algo/resolve-shape pitch :chord chord-name))
            :C  :maj #::specs{:name :maj :pitch :C  :intervals [:P1 :M3 :P5] :pitches [:C :E :G] :aliases ["M" "major"]}
            :C# :m   #::specs{:name :m   :pitch :C# :intervals [:P1 :m3 :P5] :pitches [:C# :E :G#] :aliases ["min" "-" "minor"]}
            :F# :aug #::specs{:name :aug :pitch :F# :intervals [:P1 :M3 :A5] :pitches [:F# :A# :C##] :aliases ["+" "+5" "^#5" "augmented"]}))
        (testing "starting from a note"
          (are+ [note chord-name want] (= want (algo/resolve-shape note :chord chord-name))
            :C3  :maj #::specs{:name :maj :pitch :C  :intervals [:P1 :M3 :P5] :pitches [:C :E :G] :notes [:C3 :E3 :G3] :aliases ["M" "major"]}
            :C#4 :m   #::specs{:name :m   :pitch :C# :intervals [:P1 :m3 :P5] :pitches [:C# :E :G#]  :notes [:C#4 :E4 :G#4] :aliases ["min" "-" "minor"]}
            :F#5 :aug #::specs{:name :aug :pitch :F# :intervals [:P1 :M3 :A5] :pitches [:F# :A# :C##]  :notes [:F#5 :A#5 :C##6] :aliases ["+" "+5" "^#5" "augmented"]})))
      (testing "starting from a scale"
        (testing "starting from a pitch"
          (are+ [pitch scale-name want] (= want (algo/resolve-shape pitch :scale scale-name))
            :C :major #::specs{:name :major
                               :pitch :C
                               :aliases ["ionian"]
                               :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                               :degrees [:1 :2 :3 :4 :5 :6 :7]
                               :pitches [:C :D :E :F :G :A :B]}
            :D :dorian #::specs{:name :dorian
                                :pitch :D
                                :intervals [:P1 :M2 :m3 :P4 :P5 :M6 :m7]
                                :degrees [:1 :2 :b3 :4 :5 :6 :b7]
                                :pitches [:D :E :F :G :A :B :C]}
            :C :minor #::specs{:name :minor
                               :pitch :C
                               :aliases ["aeolian"]
                               :intervals [:P1 :M2 :m3 :P4 :P5 :m6 :m7]
                               :degrees [:1 :2 :b3 :4 :5 :b6 :b7]
                               :pitches [:C :D :Eb :F :G :Ab :Bb]}
            :C# :major #::specs{:name :major
                                :pitch :C#
                                :aliases ["ionian"]
                                :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                                :degrees [:1 :2 :3 :4 :5 :6 :7]
                                :pitches [:C# :D# :E# :F# :G# :A# :B#]}
            :F# :major #::specs{:name :major
                                :pitch :F#
                                :aliases ["ionian"]
                                :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                                :degrees [:1 :2 :3 :4 :5 :6 :7]
                                :pitches [:F# :G# :A# :B :C# :D# :E#]}))
        (testing "starting from a note"
          (are+ [note scale-name want] (= want (algo/resolve-shape note :scale scale-name))
            :C4 :major #::specs{:name :major
                                :pitch :C
                                :aliases ["ionian"]
                                :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                                :degrees [:1 :2 :3 :4 :5 :6 :7]
                                :pitches [:C :D :E :F :G :A :B]
                                :notes [:C4 :D4 :E4 :F4 :G4 :A4 :B4]}
            :C4 :minor #::specs{:name :minor
                                :pitch :C
                                :aliases ["aeolian"]
                                :intervals [:P1 :M2 :m3 :P4 :P5 :m6 :m7]
                                :degrees [:1 :2 :b3 :4 :5 :b6 :b7]
                                :pitches [:C :D :Eb :F :G :Ab :Bb]
                                :notes [:C4 :D4 :Eb4 :F4 :G4 :Ab4 :Bb4]}
            :C#4 :major #::specs{:name :major
                                 :pitch :C#
                                 :aliases ["ionian"]
                                 :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                                 :degrees [:1 :2 :3 :4 :5 :6 :7]
                                 :pitches [:C# :D# :E# :F# :G# :A# :B#]
                                 :notes [:C#4 :D#4 :E#4 :F#4 :G#4 :A#4 :B#4]}
            :F#4 :major #::specs{:name :major
                                 :pitch :F#
                                 :aliases ["ionian"]
                                 :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                                 :degrees [:1 :2 :3 :4 :5 :6 :7]
                                 :pitches [:F# :G# :A# :B :C# :D# :E#]
                                 :notes [:F#4 :G#4 :A#4 :B4 :C#5 :D#5 :E#5]}))))
    (testing "with intervals->chord"
      (are+ [intervals want] (= want (algo/intervals->chord intervals))
        [] nil
        [:P1 :M3] nil
        [:P1 :M3 :P5] :maj
        [:P1 :m3 :P5] :m))
    (testing "with intervals->chords"
      (are+ [intervals want] (= want (algo/intervals->chords intervals))
        [] []
        [:P1 :m3 :P5 :m7 :P11] #{:m11 :m7add11}))
    (testing "with interval->degree"
      (are+ [interval want] (= want (algo/interval->degree interval))
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
    (testing "with scale->mode"
      (are+ [base-scale-name mode-num want-scale-name] (= want-scale-name (::specs/name (algo/scale->mode (algo/resolve-shape :C :scale base-scale-name) mode-num)))
        :major 1 :major
        :major 2 :dorian
        :major 3 :phrygian
        :major 4 :lydian
        :major 5 :mixolydian
        :major 6 :minor
        :major 7 :locrian
        :major 8 :major
        :melodic-minor 2 :dorian-b2
        :melodic-minor 3 :lydian-augmented
        :melodic-minor 4 :lydian-dominant
        :melodic-minor 5 :mixolydian-b6
        :melodic-minor 6 :locrian-#2
        :melodic-minor 7 :altered))
    (testing "with degree-chord->roman-numeral"
      (are+ [degree chord-name want] (= want (algo/degree-chord->roman-numeral degree chord-name))
        :1 :maj :I
        :1 :maj7 :I
        :2 :min :ii
        :2 :min7 :ii
        :b2 :min :bii
        :#2 :min :#ii
        :3 :min :iii
        :7 :dim :vii°
        :2 :dim :ii°
        :b3 :aug :bIII+))))
