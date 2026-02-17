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
          :C  :maj {:name :maj :pitch :C  :intervals [:P1 :M3 :P5] :pitches [:C :E :G]}
          :C# :m   {:name :m   :pitch :C# :intervals [:P1 :m3 :P5] :pitches [:C# :E :G#]}
          :F# :aug {:name :aug :pitch :F# :intervals [:P1 :M3 :A5] :pitches [:F# :A# :C##]}))
      (testing "starting from a note"
        (are+ [note chord-name want] (= want (jigsaw/->shape note chord-name))
          :C3  :maj {:name :maj :pitch :C  :intervals [:P1 :M3 :P5] :pitches [:C :E :G] :notes [:C3 :E3 :G3]}
          :C#4 :m   {:name :m   :pitch :C# :intervals [:P1 :m3 :P5] :pitches [:C# :E :G#]  :notes [:C#4 :E4 :G#4]}
          :F#5 :aug {:name :aug :pitch :F# :intervals [:P1 :M3 :A5] :pitches [:F# :A# :C##]  :notes [:F#5 :A#5 :C##6]})))
    (testing "starting from a scale"
      (testing "starting from a pitch"
        (are+ [pitch scale-name want] (= want (jigsaw/->shape pitch scale-name))
          :C :major {:name :major
                     :pitch :C
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
                     :intervals [:P1 :M2 :m3 :P4 :P5 :m6 :m7]
                     :degrees [:1 :2 :b3 :4 :5 :b6 :b7]
                     :pitches [:C :D :Eb :F :G :Ab :Bb]}
          :C# :major {:name :major
                      :pitch :C#
                      :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                      :degrees [:1 :2 :3 :4 :5 :6 :7]
                      :pitches [:C# :D# :E# :F# :G# :A# :B#]}
          :F# :major {:name :major
                      :pitch :F#
                      :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                      :degrees [:1 :2 :3 :4 :5 :6 :7]
                      :pitches [:F# :G# :A# :B :C# :D# :E#]}))
      (testing "starting from a note"
        (are+ [note scale-name want] (= want (jigsaw/->shape note scale-name))
          :C4 :major {:name :major
                      :pitch :C
                      :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                      :degrees [:1 :2 :3 :4 :5 :6 :7]
                      :pitches [:C :D :E :F :G :A :B]
                      :notes [:C4 :D4 :E4 :F4 :G4 :A4 :B4]}
          :C4 :minor {:name :minor
                      :pitch :C
                      :intervals [:P1 :M2 :m3 :P4 :P5 :m6 :m7]
                      :degrees [:1 :2 :b3 :4 :5 :b6 :b7]
                      :pitches [:C :D :Eb :F :G :Ab :Bb]
                      :notes [:C4 :D4 :Eb4 :F4 :G4 :Ab4 :Bb4]}
          :C#4 :major {:name :major
                       :pitch :C#
                       :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                       :degrees [:1 :2 :3 :4 :5 :6 :7]
                       :pitches [:C# :D# :E# :F# :G# :A# :B#]
                       :notes [:C#4 :D#4 :E#4 :F#4 :G#4 :A#4 :B#4]}
          :F#4 :major {:name :major
                       :pitch :F#
                       :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                       :degrees [:1 :2 :3 :4 :5 :6 :7]
                       :pitches [:F# :G# :A# :B :C# :D# :E#]
                       :notes [:F#4 :G#4 :A#4 :B4 :C#5 :D#5 :E#5]})))
    (testing "with shape-ref (map) input"
      (are+ [shape-ref want] (= want (jigsaw/->shape shape-ref))
        {:pitch :C :name :maj} {:pitch :C
                                :name :maj
                                :intervals [:P1 :M3 :P5]
                                :pitches [:C :E :G]}))
    (testing "with keyword input"
      (are+ [k want] (= want (jigsaw/->shape k))
          ; Pitch-based
        :C_maj {:pitch :C
                :name :maj
                :intervals [:P1 :M3 :P5]
                :pitches [:C :E :G]}
          ; Note-based
        :C4_maj {:pitch :C
                 :name :maj
                 :intervals [:P1 :M3 :P5]
                 :pitches [:C :E :G]
                 :notes [:C4 :E4 :G4]}
        :Eb_13sus4 {:intervals [:P1 :P4 :P5 :m7 :M9 :M13],
                    :name :13sus4,
                    :pitch :Eb,
                    :pitches [:Eb :Ab :Bb :Db :F :C]})))

  (testing "->progression"
    (are+ [tonic chord-degrees want] (= (map #(select-keys % [:pitch :name]) want)
                                        (map #(select-keys % [:pitch :name]) (jigsaw/->progression tonic chord-degrees)))
      :C_major [:ii :V :I] [(jigsaw/->shape :D_m)
                            (jigsaw/->shape :G_maj)
                            (jigsaw/->shape :C_maj)]
      :C_major [:bii :V :I] [(jigsaw/->shape :Db_m)
                             (jigsaw/->shape :G_maj)
                             (jigsaw/->shape :C_maj)]
      :C_major [:ii7 :V7 :IM7] [(jigsaw/->shape :D_m7)
                                (jigsaw/->shape :G_7)
                                (jigsaw/->shape :C_maj7)]))

  (testing "scale->chords"
    (are+ [pitch scale-name expected] (= expected (map #(dissoc % :parent-shape) (jigsaw/scale->chords (jigsaw/->shape pitch scale-name))))
      :C :major '({:pitch :C, :name :maj, :context :chord-degree/I}
                  {:pitch :C, :name :maj7, :context :chord-degree/IM7}
                  {:pitch :C, :name :maj9, :context :chord-degree/I}
                  {:pitch :C, :name :maj13, :context :chord-degree/I}
                  {:pitch :C, :name :6, :context :chord-degree/I}
                  {:pitch :C, :name :6add9, :context :chord-degree/I}
                  {:pitch :C, :name :sus4, :context :chord-degree/i}
                  {:pitch :C, :name :sus2, :context :chord-degree/i}
                  {:pitch :C, :name :5, :context :chord-degree/i}
                  {:pitch :C, :name :sus24, :context :chord-degree/i}
                  {:pitch :C, :name :M7add13, :context :chord-degree/I}
                  {:pitch :C, :name :Madd9, :context :chord-degree/I}
                  {:pitch :C, :name :M7sus4, :context :chord-degree/i}
                  {:pitch :C, :name :M9sus4, :context :chord-degree/i}
                  {:pitch :D, :name :m, :context :chord-degree/ii}
                  {:pitch :D, :name :m7, :context :chord-degree/ii7}
                  {:pitch :D, :name :m6, :context :chord-degree/ii}
                  {:pitch :D, :name :m9, :context :chord-degree/ii}
                  {:pitch :D, :name :m11, :context :chord-degree/ii}
                  {:pitch :D, :name :m13, :context :chord-degree/ii}
                  {:pitch :D, :name :sus4, :context :chord-degree/ii}
                  {:pitch :D, :name :sus2, :context :chord-degree/ii}
                  {:pitch :D, :name :7sus4, :context :chord-degree/ii}
                  {:pitch :D, :name :11, :context :chord-degree/ii}
                  {:pitch :D, :name :5, :context :chord-degree/ii}
                  {:pitch :D, :name :sus24, :context :chord-degree/ii}
                  {:pitch :D, :name :m69, :context :chord-degree/ii}
                  {:pitch :D, :name :madd4, :context :chord-degree/ii}
                  {:pitch :D, :name :m7add11, :context :chord-degree/ii}
                  {:pitch :D, :name :madd9, :context :chord-degree/ii}
                  {:pitch :D, :name :9sus4, :context :chord-degree/ii}
                  {:pitch :D, :name :13sus4, :context :chord-degree/ii}
                  {:pitch :D, :name :q, :context :chord-degree/ii}
                  {:pitch :E, :name :m, :context :chord-degree/iii}
                  {:pitch :E, :name :m7, :context :chord-degree/iii7}
                  {:pitch :E, :name :sus4, :context :chord-degree/iii}
                  {:pitch :E, :name :7sus4, :context :chord-degree/iii}
                  {:pitch :E, :name :b9sus, :context :chord-degree/iii}
                  {:pitch :E, :name :5, :context :chord-degree/iii}
                  {:pitch :E, :name :madd4, :context :chord-degree/iii}
                  {:pitch :E, :name :m7add11, :context :chord-degree/iii}
                  {:pitch :E, :name :m7#5, :context :chord-degree/iii}
                  {:pitch :E, :name :mb6b9, :context :chord-degree/iii}
                  {:pitch :E, :name :7sus4b9b13, :context :chord-degree/iii}
                  {:pitch :E, :name :q, :context :chord-degree/iii}
                  {:pitch :E, :name :11b9, :context :chord-degree/iii}
                  {:pitch :F, :name :maj, :context :chord-degree/IV}
                  {:pitch :F, :name :maj7, :context :chord-degree/IVM7}
                  {:pitch :F, :name :maj9, :context :chord-degree/IV}
                  {:pitch :F, :name :maj13, :context :chord-degree/IV}
                  {:pitch :F, :name :6, :context :chord-degree/IV}
                  {:pitch :F, :name :6add9, :context :chord-degree/IV}
                  {:pitch :F, :name :maj#4, :context :chord-degree/IV}
                  {:pitch :F, :name :sus2, :context :chord-degree/iv}
                  {:pitch :F, :name :5, :context :chord-degree/iv}
                  {:pitch :F, :name :maj9#11, :context :chord-degree/IV}
                  {:pitch :F, :name :M6#11, :context :chord-degree/IV}
                  {:pitch :F, :name :M7add13, :context :chord-degree/IV}
                  {:pitch :F, :name :69#11, :context :chord-degree/IV}
                  {:pitch :F, :name :M13#11, :context :chord-degree/IV}
                  {:pitch :F, :name :Madd9, :context :chord-degree/IV}
                  {:pitch :G, :name :maj, :context :chord-degree/V}
                  {:pitch :G, :name :6, :context :chord-degree/V}
                  {:pitch :G, :name :6add9, :context :chord-degree/V}
                  {:pitch :G, :name :7, :context :chord-degree/V7}
                  {:pitch :G, :name :9, :context :chord-degree/V}
                  {:pitch :G, :name :13, :context :chord-degree/V}
                  {:pitch :G, :name :sus4, :context :chord-degree/v}
                  {:pitch :G, :name :sus2, :context :chord-degree/v}
                  {:pitch :G, :name :7sus4, :context :chord-degree/v}
                  {:pitch :G, :name :11, :context :chord-degree/v}
                  {:pitch :G, :name :5, :context :chord-degree/v}
                  {:pitch :G, :name :sus24, :context :chord-degree/v}
                  {:pitch :G, :name :7add6, :context :chord-degree/V}
                  {:pitch :G, :name :Madd9, :context :chord-degree/V}
                  {:pitch :G, :name :7no5, :context :chord-degree/V}
                  {:pitch :G, :name :9no5, :context :chord-degree/V}
                  {:pitch :G, :name :13no5, :context :chord-degree/V}
                  {:pitch :G, :name :9sus4, :context :chord-degree/v}
                  {:pitch :G, :name :13sus4, :context :chord-degree/v}
                  {:pitch :A, :name :m, :context :chord-degree/vi}
                  {:pitch :A, :name :m7, :context :chord-degree/vi7}
                  {:pitch :A, :name :m9, :context :chord-degree/vi}
                  {:pitch :A, :name :m11, :context :chord-degree/vi}
                  {:pitch :A, :name :sus4, :context :chord-degree/vi}
                  {:pitch :A, :name :sus2, :context :chord-degree/vi}
                  {:pitch :A, :name :7sus4, :context :chord-degree/vi}
                  {:pitch :A, :name :11, :context :chord-degree/vi}
                  {:pitch :A, :name :5, :context :chord-degree/vi}
                  {:pitch :A, :name :sus24, :context :chord-degree/vi}
                  {:pitch :A, :name :madd4, :context :chord-degree/vi}
                  {:pitch :A, :name :m7add11, :context :chord-degree/vi}
                  {:pitch :A, :name :madd9, :context :chord-degree/vi}
                  {:pitch :A, :name :m7#5, :context :chord-degree/vi}
                  {:pitch :A, :name :m9#5, :context :chord-degree/vi}
                  {:pitch :A, :name :9sus4, :context :chord-degree/vi}
                  {:pitch :A, :name :q, :context :chord-degree/vi}
                  {:pitch :B, :name :dim, :context :chord-degree/viio}
                  {:pitch :B, :name :m7b5, :context :chord-degree/vii%}
                  {:pitch :B, :name :m7#5, :context :chord-degree/vii}
                  {:pitch :B, :name :mb6b9, :context :chord-degree/vii}
                  {:pitch :B, :name :q, :context :chord-degree/vii})

      :C :diminished '({:pitch :C, :name :dim, :context :chord-degree/io}
                       {:pitch :C, :name :dim7M7, :context :chord-degree/io}
                       {:pitch :C, :name :dimM7, :context :chord-degree/io}
                       {:pitch :C, :name :mb6M7, :context :chord-degree/i}
                       {:pitch :D, :name :m, :context :chord-degree/ii}
                       {:pitch :D, :name :m7, :context :chord-degree/ii7}
                       {:pitch :D, :name :m6, :context :chord-degree/ii}
                       {:pitch :D, :name :dim, :context :chord-degree/iio}
                       {:pitch :D, :name :m7b5, :context :chord-degree/ii%}
                       {:pitch :D, :name :5, :context :chord-degree/ii}
                       {:pitch :Eb, :name :m#5, :context :chord-degree/biii+}
                       {:pitch :Eb, :name :M7#5sus4, :context :chord-degree/biii+}
                       {:pitch :Eb, :name :M9#5sus4, :context :chord-degree/biii+}
                       {:pitch :F, :name :maj, :context :chord-degree/IV}
                       {:pitch :F, :name :6, :context :chord-degree/IV}
                       {:pitch :F, :name :m, :context :chord-degree/iv}
                       {:pitch :F, :name :m7, :context :chord-degree/iv7}
                       {:pitch :F, :name :m6, :context :chord-degree/iv}
                       {:pitch :F, :name :7, :context :chord-degree/IV7}
                       {:pitch :F, :name :7#11, :context :chord-degree/IV}
                       {:pitch :F, :name :7b9, :context :chord-degree/IV}
                       {:pitch :F, :name :alt7, :context :chord-degree/IV}
                       {:pitch :F, :name :5, :context :chord-degree/iv}
                       {:pitch :F, :name :M6#11, :context :chord-degree/IV}
                       {:pitch :F, :name :7add6, :context :chord-degree/IV}
                       {:pitch :F, :name :7b9#11, :context :chord-degree/IV}
                       {:pitch :F, :name :13b9#11, :context :chord-degree/IV}
                       {:pitch :F, :name :13b9, :context :chord-degree/IV}
                       {:pitch :F, :name :Maddb9, :context :chord-degree/IV}
                       {:pitch :F, :name :7no5, :context :chord-degree/IV}
                       {:pitch :Ab, :name :maj, :context :chord-degree/bVI}
                       {:pitch :Ab, :name :6, :context :chord-degree/bVI}
                       {:pitch :Ab, :name :7, :context :chord-degree/bVI7}
                       {:pitch :Ab, :name :7#11, :context :chord-degree/bVI}
                       {:pitch :Ab, :name :7#9, :context :chord-degree/bVI}
                       {:pitch :Ab, :name :5, :context :chord-degree/bvi}
                       {:pitch :Ab, :name :M6#11, :context :chord-degree/bVI}
                       {:pitch :Ab, :name :7add6, :context :chord-degree/bVI}
                       {:pitch :Ab, :name :7#9#11, :context :chord-degree/bVI}
                       {:pitch :Ab, :name :13#9#11, :context :chord-degree/bVI}
                       {:pitch :Ab, :name :13#9, :context :chord-degree/bVI}
                       {:pitch :Ab, :name :7no5, :context :chord-degree/bVI}
                       {:pitch :A, :name :dim, :context :chord-degree/vio}
                       {:pitch :A, :name :dim7, :context :chord-degree/vio7}
                       {:pitch :B, :name :dim, :context :chord-degree/viio}
                       {:pitch :B, :name :dim7, :context :chord-degree/viio7}
                       {:pitch :B, :name :m7b5, :context :chord-degree/vii%})))

  (testing "chord->scales"
    (are+ [pitch chord-name expected] (= (set expected) (set (map #(select-keys % [:pitch :name :context])
                                                                  (jigsaw/chord->scales (jigsaw/->shape pitch chord-name)))))
      :C :maj [{:pitch :C, :name :lydian-dominant-pentatonic, :context :chord-degree/I}
               {:pitch :C, :name :bebop-major, :context :chord-degree/I}
               {:pitch :C, :name :lydian, :context :chord-degree/I}
               {:pitch :C, :name :hungarian-major, :context :chord-degree/I}
               {:pitch :C, :name :augmented-heptatonic, :context :chord-degree/I}
               {:pitch :C, :name :phrygian-dominant, :context :chord-degree/I}
               {:pitch :C, :name :mixolydian, :context :chord-degree/I}
               {:pitch :C, :name :composite-blues, :context :chord-degree/I}
               {:pitch :C, :name :augmented, :context :chord-degree/I}
               {:pitch :C, :name :double-harmonic-major, :context :chord-degree/I}
               {:pitch :C, :name :major, :context :chord-degree/I}
               {:pitch :C, :name :lydian-#9, :context :chord-degree/I}
               {:pitch :C, :name :bebop, :context :chord-degree/I}
               {:pitch :C, :name :major-pentatonic, :context :chord-degree/I}
               {:pitch :C, :name :flat-six-pentatonic, :context :chord-degree/I}
               {:pitch :C, :name :major-blues, :context :chord-degree/I}
               {:pitch :C, :name :ionian-pentatonic, :context :chord-degree/I}
               {:pitch :C, :name :double-harmonic-lydian, :context :chord-degree/I}
               {:pitch :C, :name :bebop-minor, :context :chord-degree/I}
               {:pitch :C, :name :spanish-heptatonic, :context :chord-degree/I}
               {:pitch :C, :name :harmonic-major, :context :chord-degree/I}
               {:pitch :C, :name :lydian-dominant, :context :chord-degree/I}
               {:pitch :C, :name :mixolydian-b6, :context :chord-degree/I}
               {:pitch :C, :name :mixolydian-pentatonic, :context :chord-degree/I}
               {:pitch :C, :name :half-whole-diminished, :context :chord-degree/I}
               {:pitch :C, :name :lydian-minor, :context :chord-degree/I}
               {:pitch :C, :name :lydian-pentatonic, :context :chord-degree/I}
               {:pitch :B, :name :locrian, :context :chord-degree/bII}
               {:pitch :B, :name :phrygian-dominant, :context :chord-degree/bII}
               {:pitch :B, :name :bebop-locrian, :context :chord-degree/bII}
               {:pitch :B, :name :double-harmonic-major, :context :chord-degree/bII}
               {:pitch :B, :name :phrygian, :context :chord-degree/bII}
               {:pitch :B, :name :spanish-heptatonic, :context :chord-degree/bII}
               {:pitch :Bb, :name :dorian-#4, :context :chord-degree/II}
               {:pitch :Bb, :name :lydian, :context :chord-degree/II}
               {:pitch :Bb, :name :lydian-augmented, :context :chord-degree/II}
               {:pitch :Bb, :name :lydian-diminished, :context :chord-degree/II}
               {:pitch :Bb, :name :lydian-dominant, :context :chord-degree/II}
               {:pitch :A, :name :dorian-#4, :context :chord-degree/bIII}
               {:pitch :A, :name :dorian, :context :chord-degree/bIII}
               {:pitch :A, :name :bebop-locrian, :context :chord-degree/bIII}
               {:pitch :A, :name :composite-blues, :context :chord-degree/bIII}
               {:pitch :A, :name :bebop-harmonic-minor, :context :chord-degree/bIII}
               {:pitch :A, :name :dorian-b2, :context :chord-degree/bIII}
               {:pitch :A, :name :minor-pentatonic, :context :chord-degree/bIII}
               {:pitch :A, :name :phrygian, :context :chord-degree/bIII}
               {:pitch :A, :name :minor, :context :chord-degree/bIII}
               {:pitch :A, :name :bebop-minor, :context :chord-degree/bIII}
               {:pitch :A, :name :spanish-heptatonic, :context :chord-degree/bIII}
               {:pitch :A, :name :minor-blues, :context :chord-degree/bIII}
               {:pitch :A, :name :half-whole-diminished, :context :chord-degree/bIII}
               {:pitch :Ab, :name :bebop-major, :context :chord-degree/III}
               {:pitch :Ab, :name :augmented-heptatonic, :context :chord-degree/III}
               {:pitch :Ab, :name :augmented, :context :chord-degree/III}
               {:pitch :Ab, :name :lydian-augmented, :context :chord-degree/III}
               {:pitch :Ab, :name :leading-whole-tone, :context :chord-degree/III}
               {:pitch :Ab, :name :major-augmented, :context :chord-degree/III}
               {:pitch :Ab, :name :lydian-#5P-pentatonic, :context :chord-degree/III}
               {:pitch :G, :name :melodic-minor, :context :chord-degree/IV}
               {:pitch :G, :name :bebop-major, :context :chord-degree/IV}
               {:pitch :G, :name :dorian, :context :chord-degree/IV}
               {:pitch :G, :name :minor-six-diminished, :context :chord-degree/IV}
               {:pitch :G, :name :mixolydian, :context :chord-degree/IV}
               {:pitch :G, :name :composite-blues, :context :chord-degree/IV}
               {:pitch :G, :name :locrian-6, :context :chord-degree/IV}
               {:pitch :G, :name :major, :context :chord-degree/IV}
               {:pitch :G, :name :dorian-b2, :context :chord-degree/IV}
               {:pitch :G, :name :bebop, :context :chord-degree/IV}
               {:pitch :G, :name :minor-six-pentatonic, :context :chord-degree/IV}
               {:pitch :G, :name :diminished, :context :chord-degree/IV}
               {:pitch :G, :name :bebop-minor, :context :chord-degree/IV}
               {:pitch :G, :name :major-augmented, :context :chord-degree/IV}
               {:pitch :F#, :name :altered, :context :chord-degree/#IV}
               {:pitch :F, :name :hungarian-minor, :context :chord-degree/V}
               {:pitch :F, :name :melodic-minor, :context :chord-degree/V}
               {:pitch :F, :name :bebop-major, :context :chord-degree/V}
               {:pitch :F, :name :lydian, :context :chord-degree/V}
               {:pitch :F, :name :minor-six-diminished, :context :chord-degree/V}
               {:pitch :F, :name :harmonic-minor, :context :chord-degree/V}
               {:pitch :F, :name :minor-hexatonic, :context :chord-degree/V}
               {:pitch :F, :name :bebop-harmonic-minor, :context :chord-degree/V}
               {:pitch :F, :name :major, :context :chord-degree/V}
               {:pitch :F, :name :lydian-diminished, :context :chord-degree/V}
               {:pitch :F, :name :bebop, :context :chord-degree/V}
               {:pitch :F, :name :harmonic-major, :context :chord-degree/V}
               {:pitch :F#, :name :locrian, :context :chord-degree/bV}
               {:pitch :F#, :name :bebop-locrian, :context :chord-degree/bV}
               {:pitch :F#, :name :locrian-6, :context :chord-degree/bV}
               {:pitch :E, :name :hungarian-minor, :context :chord-degree/bVI}
               {:pitch :E, :name :locrian, :context :chord-degree/bVI}
               {:pitch :E, :name :minor-six-diminished, :context :chord-degree/bVI}
               {:pitch :E, :name :harmonic-minor, :context :chord-degree/bVI}
               {:pitch :E, :name :bebop-locrian, :context :chord-degree/bVI}
               {:pitch :E, :name :altered, :context :chord-degree/bVI}
               {:pitch :E, :name :locrian-#2, :context :chord-degree/bVI}
               {:pitch :E, :name :bebop-harmonic-minor, :context :chord-degree/bVI}
               {:pitch :E, :name :ultralocrian, :context :chord-degree/bVI}
               {:pitch :E, :name :phrygian, :context :chord-degree/bVI}
               {:pitch :E, :name :minor, :context :chord-degree/bVI}
               {:pitch :E, :name :diminished, :context :chord-degree/bVI}
               {:pitch :E, :name :spanish-heptatonic, :context :chord-degree/bVI}
               {:pitch :E, :name :tizita, :context :chord-degree/bVI}
               {:pitch :Db, :name :lydian-#9, :context :chord-degree/VII}
               {:pitch :D, :name :dorian, :context :chord-degree/bVII}
               {:pitch :D, :name :locrian-major, :context :chord-degree/bVII}
               {:pitch :D, :name :mixolydian, :context :chord-degree/bVII}
               {:pitch :D, :name :composite-blues, :context :chord-degree/bVII}
               {:pitch :D, :name :locrian-#2, :context :chord-degree/bVII}
               {:pitch :D, :name :bebop-harmonic-minor, :context :chord-degree/bVII}
               {:pitch :D, :name :bebop, :context :chord-degree/bVII}
               {:pitch :D, :name :minor, :context :chord-degree/bVII}
               {:pitch :D, :name :bebop-minor, :context :chord-degree/bVII}
               {:pitch :D, :name :mixolydian-b6, :context :chord-degree/bVII}
               {:pitch :D#, :name :ultralocrian, :context :chord-degree/bVII}]

      :Eb :6add9 [{:pitch :Eb, :name :bebop-major, :context :chord-degree/I}
                  {:pitch :Eb, :name :lydian, :context :chord-degree/I}
                  {:pitch :Eb, :name :mixolydian, :context :chord-degree/I}
                  {:pitch :Eb, :name :composite-blues, :context :chord-degree/I}
                  {:pitch :Eb, :name :major, :context :chord-degree/I}
                  {:pitch :Eb, :name :bebop, :context :chord-degree/I}
                  {:pitch :Eb, :name :major-pentatonic, :context :chord-degree/I}
                  {:pitch :Eb, :name :major-blues, :context :chord-degree/I}
                  {:pitch :Eb, :name :bebop-minor, :context :chord-degree/I}
                  {:pitch :Eb, :name :lydian-dominant, :context :chord-degree/I}
                  {:pitch :Db, :name :lydian, :context :chord-degree/II}
                  {:pitch :Db, :name :lydian-augmented, :context :chord-degree/II}
                  {:pitch :D, :name :locrian, :context :chord-degree/bII}
                  {:pitch :D, :name :bebop-locrian, :context :chord-degree/bII}
                  {:pitch :D, :name :phrygian, :context :chord-degree/bII}
                  {:pitch :D, :name :spanish-heptatonic, :context :chord-degree/bII}
                  {:pitch :C, :name :dorian, :context :chord-degree/bIII}
                  {:pitch :C, :name :bebop-locrian, :context :chord-degree/bIII}
                  {:pitch :C, :name :composite-blues, :context :chord-degree/bIII}
                  {:pitch :C, :name :bebop-harmonic-minor, :context :chord-degree/bIII}
                  {:pitch :C, :name :dorian-b2, :context :chord-degree/bIII}
                  {:pitch :C, :name :minor-pentatonic, :context :chord-degree/bIII}
                  {:pitch :C, :name :phrygian, :context :chord-degree/bIII}
                  {:pitch :C, :name :minor, :context :chord-degree/bIII}
                  {:pitch :C, :name :bebop-minor, :context :chord-degree/bIII}
                  {:pitch :C, :name :spanish-heptatonic, :context :chord-degree/bIII}
                  {:pitch :C, :name :minor-blues, :context :chord-degree/bIII}
                  {:pitch :A, :name :altered, :context :chord-degree/#IV}
                  {:pitch :Bb, :name :melodic-minor, :context :chord-degree/IV}
                  {:pitch :Bb, :name :bebop-major, :context :chord-degree/IV}
                  {:pitch :Bb, :name :dorian, :context :chord-degree/IV}
                  {:pitch :Bb, :name :minor-six-diminished, :context :chord-degree/IV}
                  {:pitch :Bb, :name :mixolydian, :context :chord-degree/IV}
                  {:pitch :Bb, :name :composite-blues, :context :chord-degree/IV}
                  {:pitch :Bb, :name :major, :context :chord-degree/IV}
                  {:pitch :Bb, :name :bebop, :context :chord-degree/IV}
                  {:pitch :Bb, :name :bebop-minor, :context :chord-degree/IV}
                  {:pitch :A, :name :locrian, :context :chord-degree/bV}
                  {:pitch :A, :name :bebop-locrian, :context :chord-degree/bV}
                  {:pitch :Ab, :name :bebop-major, :context :chord-degree/V}
                  {:pitch :Ab, :name :lydian, :context :chord-degree/V}
                  {:pitch :Ab, :name :major, :context :chord-degree/V}
                  {:pitch :Ab, :name :bebop, :context :chord-degree/V}
                  {:pitch :G, :name :locrian, :context :chord-degree/bVI}
                  {:pitch :G, :name :bebop-locrian, :context :chord-degree/bVI}
                  {:pitch :G, :name :locrian-#2, :context :chord-degree/bVI}
                  {:pitch :G, :name :bebop-harmonic-minor, :context :chord-degree/bVI}
                  {:pitch :G, :name :phrygian, :context :chord-degree/bVI}
                  {:pitch :G, :name :minor, :context :chord-degree/bVI}
                  {:pitch :G, :name :spanish-heptatonic, :context :chord-degree/bVI}
                  {:pitch :F, :name :dorian, :context :chord-degree/bVII}
                  {:pitch :F, :name :mixolydian, :context :chord-degree/bVII}
                  {:pitch :F, :name :composite-blues, :context :chord-degree/bVII}
                  {:pitch :F, :name :bebop-harmonic-minor, :context :chord-degree/bVII}
                  {:pitch :F, :name :bebop, :context :chord-degree/bVII}
                  {:pitch :F, :name :minor, :context :chord-degree/bVII}
                  {:pitch :F, :name :bebop-minor, :context :chord-degree/bVII}
                  {:pitch :F, :name :mixolydian-b6, :context :chord-degree/bVII}]

      :C :13sus4 [{:pitch :C, :name :dorian, :context :chord-degree/i}
                  {:pitch :C, :name :mixolydian, :context :chord-degree/i}
                  {:pitch :C, :name :composite-blues, :context :chord-degree/i}
                  {:pitch :C, :name :bebop, :context :chord-degree/i}
                  {:pitch :C, :name :bebop-minor, :context :chord-degree/i}
                  {:pitch :Bb, :name :bebop-major, :context :chord-degree/ii}
                  {:pitch :Bb, :name :lydian, :context :chord-degree/ii}
                  {:pitch :Bb, :name :major, :context :chord-degree/ii}
                  {:pitch :Bb, :name :bebop, :context :chord-degree/ii}
                  {:pitch :A, :name :locrian, :context :chord-degree/biii}
                  {:pitch :A, :name :bebop-locrian, :context :chord-degree/biii}
                  {:pitch :A, :name :phrygian, :context :chord-degree/biii}
                  {:pitch :A, :name :spanish-heptatonic, :context :chord-degree/biii}
                  {:pitch :G, :name :dorian, :context :chord-degree/iv}
                  {:pitch :G, :name :composite-blues, :context :chord-degree/iv}
                  {:pitch :G, :name :bebop-harmonic-minor, :context :chord-degree/iv}
                  {:pitch :G, :name :minor, :context :chord-degree/iv}
                  {:pitch :G, :name :bebop-minor, :context :chord-degree/iv}
                  {:pitch :F, :name :bebop-major, :context :chord-degree/v}
                  {:pitch :F, :name :mixolydian, :context :chord-degree/v}
                  {:pitch :F, :name :composite-blues, :context :chord-degree/v}
                  {:pitch :F, :name :major, :context :chord-degree/v}
                  {:pitch :F, :name :bebop, :context :chord-degree/v}
                  {:pitch :F, :name :bebop-minor, :context :chord-degree/v}
                  {:pitch :E, :name :locrian, :context :chord-degree/bvi}
                  {:pitch :E, :name :bebop-locrian, :context :chord-degree/bvi}
                  {:pitch :Eb, :name :lydian, :context :chord-degree/vi}
                  {:pitch :D, :name :bebop-locrian, :context :chord-degree/bvii}
                  {:pitch :D, :name :bebop-harmonic-minor, :context :chord-degree/bvii}
                  {:pitch :D, :name :phrygian, :context :chord-degree/bvii}
                  {:pitch :D, :name :minor, :context :chord-degree/bvii}
                  {:pitch :D, :name :spanish-heptatonic, :context :chord-degree/bvii}]))

  (testing "scale->modes"
    (are+ [base-scale modes] (= modes (map #(select-keys % [:pitch :name]) (jigsaw/scale->modes base-scale)))
      (jigsaw/->shape :C :major) '({:pitch :D, :name :dorian}
                                   {:pitch :E, :name :phrygian}
                                   {:pitch :F, :name :lydian}
                                   {:pitch :G, :name :mixolydian}
                                   {:pitch :A, :name :minor}
                                   {:pitch :B, :name :locrian})))

  (testing "notes->shapes"
    (testing "with basic inversions"
      (are+ [notes expected-pitch expected-name expected-bass]
            (let [results (jigsaw/notes->shapes notes :shape-type :chord :max-shapes 1)
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
            (let [results (jigsaw/notes->shapes notes :shape-type :chord :max-shapes 1)
                  result (first results)]
              (and (= expected-pitch (:pitch result))
                   (= expected-name (:name result))
                   (= expected-bass (:bass result))))
        [:D4 :C5 :E5 :G5] :C :Madd9 :D  ; could also be C/D
        [:G4 :F5 :A5 :C6] :F :Madd9 :G))  ; Could also be F/G
    (testing "with incomplete chords in inversion"
      (are+ [notes expected-pitch expected-name expected-bass]
            (let [results (jigsaw/notes->shapes notes :shape-type :chord :max-shapes 1)
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
            (let [results (jigsaw/notes->shapes notes :shape-type :chord :max-shapes 1)
                  result (first results)]
              (and (= expected-pitch (:pitch result))
                   (= expected-name (:name result))
                   (= expected-bass (:bass result))))
          ; Dm7 in first inversion
        [:F4 :A4 :C5 :D5] :D :m7 :F
          ; G7 in third inversion
        [:F4 :G4 :B4 :D5] :G :7 :F)))

  (testing "contextualize"
    (are+ [input-shape-ref candidate-shape-ref want] (= want (jigsaw/contextualize (jigsaw/->shape input-shape-ref) (jigsaw/->shape candidate-shape-ref)))
      :C_major :C_maj :chord-degree/I
      :C_maj :C_major :chord-degree/I
      :C_major :D_m :chord-degree/ii
      :C_major :B_dim :chord-degree/viio
      :C_major :D_dorian :mode/II
      :D_dorian :C_major :mode/VII)))
