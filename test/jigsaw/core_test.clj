(ns jigsaw.core-test
  (:require
   [clojure.test :refer [deftest testing]]
   [jigsaw.core :as jigsaw]
   [jigsaw.test-utils :refer [are+]]))

(deftest core-test
  (testing "->shape"
    (testing "starting from a chord"
      (testing "starting from a pitch"
        (are+ [pitch chord-name want] (= want (jigsaw/->shape pitch chord-name))
          :C  :maj {:name :maj :pitch :C  :intervals [:P1 :M3 :P5] :pitches [:C :E :G] :aliases ["M" "major"]}
          :C# :m   {:name :m   :pitch :C# :intervals [:P1 :m3 :P5] :pitches [:C# :E :G#] :aliases ["min" "-" "minor"]}
          :F# :aug {:name :aug :pitch :F# :intervals [:P1 :M3 :A5] :pitches [:F# :A# :C##] :aliases ["+" "+5" "^#5" "augmented"]}))
      (testing "starting from a note"
        (are+ [note chord-name want] (= want (jigsaw/->shape note chord-name))
          :C3  :maj {:name :maj :pitch :C  :intervals [:P1 :M3 :P5] :pitches [:C :E :G] :notes [:C3 :E3 :G3] :aliases ["M" "major"]}
          :C#4 :m   {:name :m   :pitch :C# :intervals [:P1 :m3 :P5] :pitches [:C# :E :G#]  :notes [:C#4 :E4 :G#4] :aliases ["min" "-" "minor"]}
          :F#5 :aug {:name :aug :pitch :F# :intervals [:P1 :M3 :A5] :pitches [:F# :A# :C##]  :notes [:F#5 :A#5 :C##6] :aliases ["+" "+5" "^#5" "augmented"]})))
    (testing "starting from a scale"
      (testing "starting from a pitch"
        (are+ [pitch scale-name want] (= want (jigsaw/->shape pitch scale-name))
          :C :major {:name :major
                     :pitch :C
                     :aliases ["ionian"]
                     :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                     :degrees [:1 :2 :3 :4 :5 :6 :7]
                     :pitches [:C :D :E :F :G :A :B]}
          :D :dorian {:name :dorian
                      :pitch :D
                      :intervals [:P1 :M2 :m3 :P4 :P5 :M6 :m7]
                      :degrees [:1 :2 :b3 :4 :5 :6 :b7]
                      :pitches [:D :E :F :G :A :B :C]}
          :C :minor {:name :minor
                     :pitch :C
                     :aliases ["aeolian"]
                     :intervals [:P1 :M2 :m3 :P4 :P5 :m6 :m7]
                     :degrees [:1 :2 :b3 :4 :5 :b6 :b7]
                     :pitches [:C :D :Eb :F :G :Ab :Bb]}
          :C# :major {:name :major
                      :pitch :C#
                      :aliases ["ionian"]
                      :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                      :degrees [:1 :2 :3 :4 :5 :6 :7]
                      :pitches [:C# :D# :E# :F# :G# :A# :B#]}
          :F# :major {:name :major
                      :pitch :F#
                      :aliases ["ionian"]
                      :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                      :degrees [:1 :2 :3 :4 :5 :6 :7]
                      :pitches [:F# :G# :A# :B :C# :D# :E#]}))
      (testing "starting from a note"
        (are+ [note scale-name want] (= want (jigsaw/->shape note scale-name))
          :C4 :major {:name :major
                      :pitch :C
                      :aliases ["ionian"]
                      :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                      :degrees [:1 :2 :3 :4 :5 :6 :7]
                      :pitches [:C :D :E :F :G :A :B]
                      :notes [:C4 :D4 :E4 :F4 :G4 :A4 :B4]}
          :C4 :minor {:name :minor
                      :pitch :C
                      :aliases ["aeolian"]
                      :intervals [:P1 :M2 :m3 :P4 :P5 :m6 :m7]
                      :degrees [:1 :2 :b3 :4 :5 :b6 :b7]
                      :pitches [:C :D :Eb :F :G :Ab :Bb]
                      :notes [:C4 :D4 :Eb4 :F4 :G4 :Ab4 :Bb4]}
          :C#4 :major {:name :major
                       :pitch :C#
                       :aliases ["ionian"]
                       :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                       :degrees [:1 :2 :3 :4 :5 :6 :7]
                       :pitches [:C# :D# :E# :F# :G# :A# :B#]
                       :notes [:C#4 :D#4 :E#4 :F#4 :G#4 :A#4 :B#4]}
          :F#4 :major {:name :major
                       :pitch :F#
                       :aliases ["ionian"]
                       :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                       :degrees [:1 :2 :3 :4 :5 :6 :7]
                       :pitches [:F# :G# :A# :B :C# :D# :E#]
                       :notes [:F#4 :G#4 :A#4 :B4 :C#5 :D#5 :E#5]})))
    (testing "with shape-ref (map) input"
      (are+ [shape-ref want] (= want (jigsaw/->shape shape-ref))
        {:pitch :C :name :maj} {:pitch :C
                                :name :maj
                                :intervals [:P1 :M3 :P5]
                                :pitches [:C :E :G]
                                :aliases ["M" "major"]}))
    (testing "with keyword input"
      (are+ [k want] (= want (jigsaw/->shape k))
          ; Pitch-based
        :C_maj {:pitch :C
                :name :maj
                :intervals [:P1 :M3 :P5]
                :pitches [:C :E :G]
                :aliases ["M" "major"]}
          ; Note-based
        :C4_maj {:pitch :C
                 :name :maj
                 :intervals [:P1 :M3 :P5]
                 :pitches [:C :E :G]
                 :notes [:C4 :E4 :G4]
                 :aliases ["M" "major"]}
        :Eb_13sus4 {:aliases ["13sus"],
                    :intervals [:P1 :P4 :P5 :m7 :M9 :M13],
                    :name :13sus4,
                    :pitch :Eb,
                    :pitches [:Eb :Ab :Bb :Db :F :C]})))

  (testing "shape->abc"
    (are+ [shape-ref want] (= want (jigsaw/shape->abc (jigsaw/->shape shape-ref)))
      :C4_maj "X:1
K:C exp C D E F G A B
L:1/4
\"Cmaj\" [C E G]"
      :C#4_m "X:1
K:C exp C D E F G A B
L:1/4
\"C#m\" [^C E ^G]"
      :C##6_sus4 "X:1
K:C exp C D E F G A B
L:1/4
\"C##sus4\" [^^c' ^^f' ^^g']"
      :C0_dim "X:1
K:C exp C D E F G A B
L:1/4
\"Cdim\" [C,,,, _E,,,, _G,,,,]"))

  (testing "->progression"
    (are+ [tonic chord-degrees want] (= (map #(select-keys % [:pitch :name]) want) (jigsaw/->progression tonic chord-degrees))
      :C_major [:ii :V :I] [(jigsaw/->shape :D_m) (jigsaw/->shape :G_maj) (jigsaw/->shape :C_maj)]
      :C_major [:bii :V :I] [(jigsaw/->shape :Db_m) (jigsaw/->shape :G_maj) (jigsaw/->shape :C_maj)]
      :C_major [:iim7 :V7 :IM7] [(jigsaw/->shape :D_m7) (jigsaw/->shape :G_7) (jigsaw/->shape :C_maj7)]))

  (testing "scale->chords"
    (are+ [pitch scale-name expected] (= expected (jigsaw/scale->chords (jigsaw/->shape pitch scale-name)))
      :C :major '({:pitch :C, :name :maj, :degree :I}
                  {:pitch :C, :name :maj7, :degree :I}
                  {:pitch :C, :name :maj9, :degree :I}
                  {:pitch :C, :name :maj13, :degree :I}
                  {:pitch :C, :name :6, :degree :I}
                  {:pitch :C, :name :6add9, :degree :I}
                  {:pitch :C, :name :sus4, :degree :i}
                  {:pitch :C, :name :sus2, :degree :i}
                  {:pitch :C, :name :5, :degree :i}
                  {:pitch :C, :name :sus24, :degree :i}
                  {:pitch :C, :name :M7add13, :degree :I}
                  {:pitch :C, :name :Madd9, :degree :I}
                  {:pitch :C, :name :M7sus4, :degree :i}
                  {:pitch :C, :name :M9sus4, :degree :i}
                  {:pitch :D, :name :m, :degree :ii}
                  {:pitch :D, :name :m7, :degree :ii}
                  {:pitch :D, :name :m6, :degree :ii}
                  {:pitch :D, :name :m9, :degree :ii}
                  {:pitch :D, :name :m11, :degree :ii}
                  {:pitch :D, :name :m13, :degree :ii}
                  {:pitch :D, :name :sus4, :degree :ii}
                  {:pitch :D, :name :sus2, :degree :ii}
                  {:pitch :D, :name :7sus4, :degree :ii}
                  {:pitch :D, :name :11, :degree :ii}
                  {:pitch :D, :name :5, :degree :ii}
                  {:pitch :D, :name :sus24, :degree :ii}
                  {:pitch :D, :name :m69, :degree :ii}
                  {:pitch :D, :name :madd4, :degree :ii}
                  {:pitch :D, :name :m7add11, :degree :ii}
                  {:pitch :D, :name :madd9, :degree :ii}
                  {:pitch :D, :name :9sus4, :degree :ii}
                  {:pitch :D, :name :13sus4, :degree :ii}
                  {:pitch :D, :name :q, :degree :ii}
                  {:pitch :E, :name :m, :degree :iii}
                  {:pitch :E, :name :m7, :degree :iii}
                  {:pitch :E, :name :sus4, :degree :iii}
                  {:pitch :E, :name :7sus4, :degree :iii}
                  {:pitch :E, :name :b9sus, :degree :iii}
                  {:pitch :E, :name :5, :degree :iii}
                  {:pitch :E, :name :madd4, :degree :iii}
                  {:pitch :E, :name :m7add11, :degree :iii}
                  {:pitch :E, :name :m7#5, :degree :iii}
                  {:pitch :E, :name :mb6b9, :degree :iii}
                  {:pitch :E, :name :7sus4b9b13, :degree :iii}
                  {:pitch :E, :name :q, :degree :iii}
                  {:pitch :E, :name :11b9, :degree :iii}
                  {:pitch :F, :name :maj, :degree :IV}
                  {:pitch :F, :name :maj7, :degree :IV}
                  {:pitch :F, :name :maj9, :degree :IV}
                  {:pitch :F, :name :maj13, :degree :IV}
                  {:pitch :F, :name :6, :degree :IV}
                  {:pitch :F, :name :6add9, :degree :IV}
                  {:pitch :F, :name :maj#4, :degree :IV}
                  {:pitch :F, :name :sus2, :degree :iv}
                  {:pitch :F, :name :5, :degree :iv}
                  {:pitch :F, :name :maj9#11, :degree :IV}
                  {:pitch :F, :name :M6#11, :degree :IV}
                  {:pitch :F, :name :M7add13, :degree :IV}
                  {:pitch :F, :name :69#11, :degree :IV}
                  {:pitch :F, :name :M13#11, :degree :IV}
                  {:pitch :F, :name :Madd9, :degree :IV}
                  {:pitch :G, :name :maj, :degree :V}
                  {:pitch :G, :name :6, :degree :V}
                  {:pitch :G, :name :6add9, :degree :V}
                  {:pitch :G, :name :7, :degree :V7}
                  {:pitch :G, :name :9, :degree :V}
                  {:pitch :G, :name :13, :degree :V}
                  {:pitch :G, :name :sus4, :degree :v}
                  {:pitch :G, :name :sus2, :degree :v}
                  {:pitch :G, :name :7sus4, :degree :v}
                  {:pitch :G, :name :11, :degree :v}
                  {:pitch :G, :name :5, :degree :v}
                  {:pitch :G, :name :sus24, :degree :v}
                  {:pitch :G, :name :7add6, :degree :V}
                  {:pitch :G, :name :Madd9, :degree :V}
                  {:pitch :G, :name :7no5, :degree :V}
                  {:pitch :G, :name :9no5, :degree :V}
                  {:pitch :G, :name :13no5, :degree :V}
                  {:pitch :G, :name :9sus4, :degree :v}
                  {:pitch :G, :name :13sus4, :degree :v}
                  {:pitch :A, :name :m, :degree :vi}
                  {:pitch :A, :name :m7, :degree :vi}
                  {:pitch :A, :name :m9, :degree :vi}
                  {:pitch :A, :name :m11, :degree :vi}
                  {:pitch :A, :name :sus4, :degree :vi}
                  {:pitch :A, :name :sus2, :degree :vi}
                  {:pitch :A, :name :7sus4, :degree :vi}
                  {:pitch :A, :name :11, :degree :vi}
                  {:pitch :A, :name :5, :degree :vi}
                  {:pitch :A, :name :sus24, :degree :vi}
                  {:pitch :A, :name :madd4, :degree :vi}
                  {:pitch :A, :name :m7add11, :degree :vi}
                  {:pitch :A, :name :madd9, :degree :vi}
                  {:pitch :A, :name :m7#5, :degree :vi}
                  {:pitch :A, :name :m9#5, :degree :vi}
                  {:pitch :A, :name :9sus4, :degree :vi}
                  {:pitch :A, :name :q, :degree :vi}
                  {:pitch :B, :name :dim, :degree :vii°}
                  {:pitch :B, :name :m7b5, :degree :vii°}
                  {:pitch :B, :name :m7#5, :degree :vii}
                  {:pitch :B, :name :mb6b9, :degree :vii}
                  {:pitch :B, :name :q, :degree :vii})

      :C :diminished '({:pitch :C, :name :dim, :degree :i°}
                       {:pitch :C, :name :dim7M7, :degree :i°}
                       {:pitch :C, :name :dimM7, :degree :i°}
                       {:pitch :C, :name :mb6M7, :degree :i}
                       {:pitch :D, :name :m, :degree :ii}
                       {:pitch :D, :name :m7, :degree :ii}
                       {:pitch :D, :name :m6, :degree :ii}
                       {:pitch :D, :name :dim, :degree :ii°}
                       {:pitch :D, :name :m7b5, :degree :ii°}
                       {:pitch :D, :name :5, :degree :ii}
                       {:pitch :Eb, :name :m#5, :degree :biii+}
                       {:pitch :Eb, :name :M7#5sus4, :degree :biii+}
                       {:pitch :Eb, :name :M9#5sus4, :degree :biii+}
                       {:pitch :F, :name :maj, :degree :IV}
                       {:pitch :F, :name :6, :degree :IV}
                       {:pitch :F, :name :m, :degree :iv}
                       {:pitch :F, :name :m7, :degree :iv}
                       {:pitch :F, :name :m6, :degree :iv}
                       {:pitch :F, :name :7, :degree :IV7}
                       {:pitch :F, :name :7#11, :degree :IV}
                       {:pitch :F, :name :7b9, :degree :IV}
                       {:pitch :F, :name :alt7, :degree :IV}
                       {:pitch :F, :name :5, :degree :iv}
                       {:pitch :F, :name :M6#11, :degree :IV}
                       {:pitch :F, :name :7add6, :degree :IV}
                       {:pitch :F, :name :7b9#11, :degree :IV}
                       {:pitch :F, :name :13b9#11, :degree :IV}
                       {:pitch :F, :name :13b9, :degree :IV}
                       {:pitch :F, :name :Maddb9, :degree :IV}
                       {:pitch :F, :name :7no5, :degree :IV}
                       {:pitch :Ab, :name :maj, :degree :bVI}
                       {:pitch :Ab, :name :6, :degree :bVI}
                       {:pitch :Ab, :name :7, :degree :bVI7}
                       {:pitch :Ab, :name :7#11, :degree :bVI}
                       {:pitch :Ab, :name :7#9, :degree :bVI}
                       {:pitch :Ab, :name :5, :degree :bvi}
                       {:pitch :Ab, :name :M6#11, :degree :bVI}
                       {:pitch :Ab, :name :7add6, :degree :bVI}
                       {:pitch :Ab, :name :7#9#11, :degree :bVI}
                       {:pitch :Ab, :name :13#9#11, :degree :bVI}
                       {:pitch :Ab, :name :13#9, :degree :bVI}
                       {:pitch :Ab, :name :7no5, :degree :bVI}
                       {:pitch :A, :name :dim, :degree :vi°}
                       {:pitch :A, :name :dim7, :degree :vi°}
                       {:pitch :B, :name :dim, :degree :vii°}
                       {:pitch :B, :name :dim7, :degree :vii°}
                       {:pitch :B, :name :m7b5, :degree :vii°})))

  (testing "chord->scales"
    (are+ [pitch chord-name expected] (= (set expected) (set (map #(select-keys % [:pitch :degree :name])
                                                                  (jigsaw/chord->scales (jigsaw/->shape pitch chord-name)))))
      :C :maj [{:pitch :C, :name :lydian-dominant-pentatonic, :degree :I}
               {:pitch :C, :name :bebop-major, :degree :I}
               {:pitch :C, :name :lydian, :degree :I}
               {:pitch :C, :name :hungarian-major, :degree :I}
               {:pitch :C, :name :augmented-heptatonic, :degree :I}
               {:pitch :C, :name :phrygian-dominant, :degree :I}
               {:pitch :C, :name :mixolydian, :degree :I}
               {:pitch :C, :name :composite-blues, :degree :I}
               {:pitch :C, :name :augmented, :degree :I}
               {:pitch :C, :name :double-harmonic-major, :degree :I}
               {:pitch :C, :name :major, :degree :I}
               {:pitch :C, :name :lydian-#9, :degree :I}
               {:pitch :C, :name :bebop, :degree :I}
               {:pitch :C, :name :major-pentatonic, :degree :I}
               {:pitch :C, :name :flat-six-pentatonic, :degree :I}
               {:pitch :C, :name :major-blues, :degree :I}
               {:pitch :C, :name :ionian-pentatonic, :degree :I}
               {:pitch :C, :name :double-harmonic-lydian, :degree :I}
               {:pitch :C, :name :bebop-minor, :degree :I}
               {:pitch :C, :name :spanish-heptatonic, :degree :I}
               {:pitch :C, :name :harmonic-major, :degree :I}
               {:pitch :C, :name :lydian-dominant, :degree :I}
               {:pitch :C, :name :mixolydian-b6, :degree :I}
               {:pitch :C, :name :mixolydian-pentatonic, :degree :I}
               {:pitch :C, :name :half-whole-diminished, :degree :I}
               {:pitch :C, :name :lydian-minor, :degree :I}
               {:pitch :C, :name :lydian-pentatonic, :degree :I}
               {:pitch :B, :name :locrian, :degree :bII}
               {:pitch :B, :name :phrygian-dominant, :degree :bII}
               {:pitch :B, :name :bebop-locrian, :degree :bII}
               {:pitch :B, :name :double-harmonic-major, :degree :bII}
               {:pitch :B, :name :phrygian, :degree :bII}
               {:pitch :B, :name :spanish-heptatonic, :degree :bII}
               {:pitch :Bb, :name :dorian-#4, :degree :II}
               {:pitch :Bb, :name :lydian, :degree :II}
               {:pitch :Bb, :name :lydian-augmented, :degree :II}
               {:pitch :Bb, :name :lydian-diminished, :degree :II}
               {:pitch :Bb, :name :lydian-dominant, :degree :II}
               {:pitch :A, :name :dorian-#4, :degree :bIII}
               {:pitch :A, :name :dorian, :degree :bIII}
               {:pitch :A, :name :bebop-locrian, :degree :bIII}
               {:pitch :A, :name :composite-blues, :degree :bIII}
               {:pitch :A, :name :bebop-harmonic-minor, :degree :bIII}
               {:pitch :A, :name :dorian-b2, :degree :bIII}
               {:pitch :A, :name :minor-pentatonic, :degree :bIII}
               {:pitch :A, :name :phrygian, :degree :bIII}
               {:pitch :A, :name :minor, :degree :bIII}
               {:pitch :A, :name :bebop-minor, :degree :bIII}
               {:pitch :A, :name :spanish-heptatonic, :degree :bIII}
               {:pitch :A, :name :minor-blues, :degree :bIII}
               {:pitch :A, :name :half-whole-diminished, :degree :bIII}
               {:pitch :Ab, :name :bebop-major, :degree :III}
               {:pitch :Ab, :name :augmented-heptatonic, :degree :III}
               {:pitch :Ab, :name :augmented, :degree :III}
               {:pitch :Ab, :name :lydian-augmented, :degree :III}
               {:pitch :Ab, :name :leading-whole-tone, :degree :III}
               {:pitch :Ab, :name :major-augmented, :degree :III}
               {:pitch :Ab, :name :lydian-#5P-pentatonic, :degree :III}
               {:pitch :G, :name :melodic-minor, :degree :IV}
               {:pitch :G, :name :bebop-major, :degree :IV}
               {:pitch :G, :name :dorian, :degree :IV}
               {:pitch :G, :name :minor-six-diminished, :degree :IV}
               {:pitch :G, :name :mixolydian, :degree :IV}
               {:pitch :G, :name :composite-blues, :degree :IV}
               {:pitch :G, :name :locrian-6, :degree :IV}
               {:pitch :G, :name :major, :degree :IV}
               {:pitch :G, :name :dorian-b2, :degree :IV}
               {:pitch :G, :name :bebop, :degree :IV}
               {:pitch :G, :name :minor-six-pentatonic, :degree :IV}
               {:pitch :G, :name :diminished, :degree :IV}
               {:pitch :G, :name :bebop-minor, :degree :IV}
               {:pitch :G, :name :major-augmented, :degree :IV}
               {:pitch :F#, :name :altered, :degree :#IV}
               {:pitch :F, :name :hungarian-minor, :degree :V}
               {:pitch :F, :name :melodic-minor, :degree :V}
               {:pitch :F, :name :bebop-major, :degree :V}
               {:pitch :F, :name :lydian, :degree :V}
               {:pitch :F, :name :minor-six-diminished, :degree :V}
               {:pitch :F, :name :harmonic-minor, :degree :V}
               {:pitch :F, :name :minor-hexatonic, :degree :V}
               {:pitch :F, :name :bebop-harmonic-minor, :degree :V}
               {:pitch :F, :name :major, :degree :V}
               {:pitch :F, :name :lydian-diminished, :degree :V}
               {:pitch :F, :name :bebop, :degree :V}
               {:pitch :F, :name :harmonic-major, :degree :V}
               {:pitch :F#, :name :locrian, :degree :bV}
               {:pitch :F#, :name :bebop-locrian, :degree :bV}
               {:pitch :F#, :name :locrian-6, :degree :bV}
               {:pitch :E, :name :hungarian-minor, :degree :bVI}
               {:pitch :E, :name :locrian, :degree :bVI}
               {:pitch :E, :name :minor-six-diminished, :degree :bVI}
               {:pitch :E, :name :harmonic-minor, :degree :bVI}
               {:pitch :E, :name :bebop-locrian, :degree :bVI}
               {:pitch :E, :name :altered, :degree :bVI}
               {:pitch :E, :name :locrian-#2, :degree :bVI}
               {:pitch :E, :name :bebop-harmonic-minor, :degree :bVI}
               {:pitch :E, :name :ultralocrian, :degree :bVI}
               {:pitch :E, :name :phrygian, :degree :bVI}
               {:pitch :E, :name :minor, :degree :bVI}
               {:pitch :E, :name :diminished, :degree :bVI}
               {:pitch :E, :name :spanish-heptatonic, :degree :bVI}
               {:pitch :E, :name :tizita, :degree :bVI}
               {:pitch :Db, :name :lydian-#9, :degree :VII}
               {:pitch :D, :name :dorian, :degree :bVII}
               {:pitch :D, :name :locrian-major, :degree :bVII}
               {:pitch :D, :name :mixolydian, :degree :bVII}
               {:pitch :D, :name :composite-blues, :degree :bVII}
               {:pitch :D, :name :locrian-#2, :degree :bVII}
               {:pitch :D, :name :bebop-harmonic-minor, :degree :bVII}
               {:pitch :D, :name :bebop, :degree :bVII}
               {:pitch :D, :name :minor, :degree :bVII}
               {:pitch :D, :name :bebop-minor, :degree :bVII}
               {:pitch :D, :name :mixolydian-b6, :degree :bVII}
               {:pitch :D#, :name :ultralocrian, :degree :bVII}]

      :Eb :6add9 [{:pitch :Eb, :name :bebop-major, :degree :I}
                  {:pitch :Eb, :name :lydian, :degree :I}
                  {:pitch :Eb, :name :mixolydian, :degree :I}
                  {:pitch :Eb, :name :composite-blues, :degree :I}
                  {:pitch :Eb, :name :major, :degree :I}
                  {:pitch :Eb, :name :bebop, :degree :I}
                  {:pitch :Eb, :name :major-pentatonic, :degree :I}
                  {:pitch :Eb, :name :major-blues, :degree :I}
                  {:pitch :Eb, :name :bebop-minor, :degree :I}
                  {:pitch :Eb, :name :lydian-dominant, :degree :I}
                  {:pitch :Db, :name :lydian, :degree :II}
                  {:pitch :Db, :name :lydian-augmented, :degree :II}
                  {:pitch :D, :name :locrian, :degree :bII}
                  {:pitch :D, :name :bebop-locrian, :degree :bII}
                  {:pitch :D, :name :phrygian, :degree :bII}
                  {:pitch :D, :name :spanish-heptatonic, :degree :bII}
                  {:pitch :C, :name :dorian, :degree :bIII}
                  {:pitch :C, :name :bebop-locrian, :degree :bIII}
                  {:pitch :C, :name :composite-blues, :degree :bIII}
                  {:pitch :C, :name :bebop-harmonic-minor, :degree :bIII}
                  {:pitch :C, :name :dorian-b2, :degree :bIII}
                  {:pitch :C, :name :minor-pentatonic, :degree :bIII}
                  {:pitch :C, :name :phrygian, :degree :bIII}
                  {:pitch :C, :name :minor, :degree :bIII}
                  {:pitch :C, :name :bebop-minor, :degree :bIII}
                  {:pitch :C, :name :spanish-heptatonic, :degree :bIII}
                  {:pitch :C, :name :minor-blues, :degree :bIII}
                  {:pitch :A, :name :altered, :degree :#IV}
                  {:pitch :Bb, :name :melodic-minor, :degree :IV}
                  {:pitch :Bb, :name :bebop-major, :degree :IV}
                  {:pitch :Bb, :name :dorian, :degree :IV}
                  {:pitch :Bb, :name :minor-six-diminished, :degree :IV}
                  {:pitch :Bb, :name :mixolydian, :degree :IV}
                  {:pitch :Bb, :name :composite-blues, :degree :IV}
                  {:pitch :Bb, :name :major, :degree :IV}
                  {:pitch :Bb, :name :bebop, :degree :IV}
                  {:pitch :Bb, :name :bebop-minor, :degree :IV}
                  {:pitch :A, :name :locrian, :degree :bV}
                  {:pitch :A, :name :bebop-locrian, :degree :bV}
                  {:pitch :Ab, :name :bebop-major, :degree :V}
                  {:pitch :Ab, :name :lydian, :degree :V}
                  {:pitch :Ab, :name :major, :degree :V}
                  {:pitch :Ab, :name :bebop, :degree :V}
                  {:pitch :G, :name :locrian, :degree :bVI}
                  {:pitch :G, :name :bebop-locrian, :degree :bVI}
                  {:pitch :G, :name :locrian-#2, :degree :bVI}
                  {:pitch :G, :name :bebop-harmonic-minor, :degree :bVI}
                  {:pitch :G, :name :phrygian, :degree :bVI}
                  {:pitch :G, :name :minor, :degree :bVI}
                  {:pitch :G, :name :spanish-heptatonic, :degree :bVI}
                  {:pitch :F, :name :dorian, :degree :bVII}
                  {:pitch :F, :name :mixolydian, :degree :bVII}
                  {:pitch :F, :name :composite-blues, :degree :bVII}
                  {:pitch :F, :name :bebop-harmonic-minor, :degree :bVII}
                  {:pitch :F, :name :bebop, :degree :bVII}
                  {:pitch :F, :name :minor, :degree :bVII}
                  {:pitch :F, :name :bebop-minor, :degree :bVII}
                  {:pitch :F, :name :mixolydian-b6, :degree :bVII}]

      :C :13sus4 [{:pitch :C, :name :dorian, :degree :i}
                  {:pitch :C, :name :mixolydian, :degree :i}
                  {:pitch :C, :name :composite-blues, :degree :i}
                  {:pitch :C, :name :bebop, :degree :i}
                  {:pitch :C, :name :bebop-minor, :degree :i}
                  {:pitch :Bb, :name :bebop-major, :degree :ii}
                  {:pitch :Bb, :name :lydian, :degree :ii}
                  {:pitch :Bb, :name :major, :degree :ii}
                  {:pitch :Bb, :name :bebop, :degree :ii}
                  {:pitch :A, :name :locrian, :degree :biii}
                  {:pitch :A, :name :bebop-locrian, :degree :biii}
                  {:pitch :A, :name :phrygian, :degree :biii}
                  {:pitch :A, :name :spanish-heptatonic, :degree :biii}
                  {:pitch :G, :name :dorian, :degree :iv}
                  {:pitch :G, :name :composite-blues, :degree :iv}
                  {:pitch :G, :name :bebop-harmonic-minor, :degree :iv}
                  {:pitch :G, :name :minor, :degree :iv}
                  {:pitch :G, :name :bebop-minor, :degree :iv}
                  {:pitch :F, :name :bebop-major, :degree :v}
                  {:pitch :F, :name :mixolydian, :degree :v}
                  {:pitch :F, :name :composite-blues, :degree :v}
                  {:pitch :F, :name :major, :degree :v}
                  {:pitch :F, :name :bebop, :degree :v}
                  {:pitch :F, :name :bebop-minor, :degree :v}
                  {:pitch :E, :name :locrian, :degree :bvi}
                  {:pitch :E, :name :bebop-locrian, :degree :bvi}
                  {:pitch :Eb, :name :lydian, :degree :vi}
                  {:pitch :D, :name :bebop-locrian, :degree :bvii}
                  {:pitch :D, :name :bebop-harmonic-minor, :degree :bvii}
                  {:pitch :D, :name :phrygian, :degree :bvii}
                  {:pitch :D, :name :minor, :degree :bvii}
                  {:pitch :D, :name :spanish-heptatonic, :degree :bvii}]))

  (testing "scale->modes"
    (are+ [base-scale modes] (= modes (map #(select-keys % [:pitch :name]) (jigsaw/scale->modes base-scale)))
      (jigsaw/->shape :C :major) '({:pitch :C, :name :major}
                                   {:pitch :D, :name :dorian}
                                   {:pitch :E, :name :phrygian}
                                   {:pitch :F, :name :lydian}
                                   {:pitch :G, :name :mixolydian}
                                   {:pitch :A, :name :minor}
                                   {:pitch :B, :name :locrian})))

  (testing "notes->shapes"
    (testing "with basic inversions"
      (are+ [notes expected-pitch expected-name expected-bass]
            (let [results (jigsaw/notes->shapes notes :chord :max-shapes 1)
                  result (first results)]
              (and (= expected-pitch (:pitch result))
                   (= expected-name (:name result))
                   (= expected-bass (:bass result))))
          ; Root position; no bass note specified
        [:C4 :E4 :G4] :C :maj nil
          ; First inversion
        [:E4 :G4 :C5] :C :maj :E
          ; Second inversion
        [:G4 :C5 :E5] :C :maj :G
          ; Seventh chord inversions
        [:B4 :C5 :E5 :G5] :C :maj7 :B))
    (testing "with slash chords"
      (are+ [notes expected-pitch expected-name expected-bass]
            (let [results (jigsaw/notes->shapes notes :chord :max-shapes 1)
                  result (first results)]
              (and (= expected-pitch (:pitch result))
                   (= expected-name (:name result))
                   (= expected-bass (:bass result))))
        [:D4 :C5 :E5 :G5] :C :Madd9 :D  ; could also be C/D
        [:G4 :F5 :A5 :C6] :F :Madd9 :G))  ; Could also be F/G
    (testing "with incomplete chords in inversion"
      (are+ [notes expected-pitch expected-name expected-bass]
            (let [results (jigsaw/notes->shapes notes :chord :max-shapes 1)
                  result (first results)]
              (and (= expected-pitch (:pitch result))
                   (= expected-name (:name result))
                   (= expected-bass (:bass result))))
          ; Just root and third in first inversion
        [:E4 :C5] :C :maj :E
          ; Just third and fifth
        [:G4 :E5] :C :maj :G))
    (testing "with complex chord inversions"
      (are+ [notes expected-pitch expected-name expected-bass]
            (let [results (jigsaw/notes->shapes notes :chord :max-shapes 1)
                  result (first results)]
              (and (= expected-pitch (:pitch result))
                   (= expected-name (:name result))
                   (= expected-bass (:bass result))))
          ; Dm7 in first inversion
        [:F4 :A4 :C5 :D5] :D :m7 :F
          ; G7 in third inversion
        [:F4 :G4 :B4 :D5] :G :7 :F))))
