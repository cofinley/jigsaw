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
    (are+ [tonic chord-degrees want] (= (map #(select-keys (jigsaw/->shape %) [:pitch :name]) want)
                                        (map #(select-keys % [:pitch :name]) (jigsaw/->progression tonic chord-degrees)))
      :C_major [:ii :V :I] [:D_m :G_maj :C_maj]
      :C_major [:bii :V :I] [:Db_m :G_maj :C_maj]
      :C_major [:iim7 :V7 :Imaj7] [:D_m7 :G_7 :C_maj7]
      :C_major [:viidim] [:B_dim]))

  (testing "scale->chords"
    (are+ [pitch scale-name expected] (= expected (map #(select-keys % [:pitch :name :context]) (jigsaw/scale->chords (jigsaw/->shape pitch scale-name))))
      :C :major '({:pitch :C, :name :maj, :context :chord-degree/I}
                  {:pitch :C, :name :maj7, :context :chord-degree/Imaj7}
                  {:pitch :C, :name :maj9, :context :chord-degree/Imaj9}
                  {:pitch :C, :name :maj13, :context :chord-degree/Imaj13}
                  {:pitch :C, :name :6, :context :chord-degree/I6}
                  {:pitch :C, :name :6add9, :context :chord-degree/I6add9}
                  {:pitch :C, :name :sus4, :context :chord-degree/isus4}
                  {:pitch :C, :name :sus2, :context :chord-degree/isus2}
                  {:pitch :C, :name :5, :context :chord-degree/i5}
                  {:pitch :C, :name :sus24, :context :chord-degree/isus24}
                  {:pitch :C, :name :M7add13, :context :chord-degree/IM7add13}
                  {:pitch :C, :name :Madd9, :context :chord-degree/IMadd9}
                  {:pitch :C, :name :M7sus4, :context :chord-degree/iM7sus4}
                  {:pitch :C, :name :M9sus4, :context :chord-degree/iM9sus4}
                  {:pitch :D, :name :m, :context :chord-degree/ii}
                  {:pitch :D, :name :m7, :context :chord-degree/iim7}
                  {:pitch :D, :name :m6, :context :chord-degree/iim6}
                  {:pitch :D, :name :m9, :context :chord-degree/iim9}
                  {:pitch :D, :name :m11, :context :chord-degree/iim11}
                  {:pitch :D, :name :m13, :context :chord-degree/iim13}
                  {:pitch :D, :name :sus4, :context :chord-degree/iisus4}
                  {:pitch :D, :name :sus2, :context :chord-degree/iisus2}
                  {:pitch :D, :name :7sus4, :context :chord-degree/ii7sus4}
                  {:pitch :D, :name :11, :context :chord-degree/ii11}
                  {:pitch :D, :name :5, :context :chord-degree/ii5}
                  {:pitch :D, :name :sus24, :context :chord-degree/iisus24}
                  {:pitch :D, :name :m69, :context :chord-degree/iim69}
                  {:pitch :D, :name :madd4, :context :chord-degree/iimadd4}
                  {:pitch :D, :name :m7add11, :context :chord-degree/iim7add11}
                  {:pitch :D, :name :madd9, :context :chord-degree/iimadd9}
                  {:pitch :D, :name :9sus4, :context :chord-degree/ii9sus4}
                  {:pitch :D, :name :13sus4, :context :chord-degree/ii13sus4}
                  {:pitch :D, :name :q, :context :chord-degree/iiq}
                  {:pitch :E, :name :m, :context :chord-degree/iii}
                  {:pitch :E, :name :m7, :context :chord-degree/iiim7}
                  {:pitch :E, :name :sus4, :context :chord-degree/iiisus4}
                  {:pitch :E, :name :7sus4, :context :chord-degree/iii7sus4}
                  {:pitch :E, :name :b9sus, :context :chord-degree/iiib9sus}
                  {:pitch :E, :name :5, :context :chord-degree/iii5}
                  {:pitch :E, :name :madd4, :context :chord-degree/iiimadd4}
                  {:pitch :E, :name :m7add11, :context :chord-degree/iiim7add11}
                  {:pitch :E, :name :m7#5, :context :chord-degree/iiim7#5}
                  {:pitch :E, :name :mb6b9, :context :chord-degree/iiimb6b9}
                  {:pitch :E, :name :7sus4b9b13, :context :chord-degree/iii7sus4b9b13}
                  {:pitch :E, :name :q, :context :chord-degree/iiiq}
                  {:pitch :E, :name :11b9, :context :chord-degree/iii11b9}
                  {:pitch :F, :name :maj, :context :chord-degree/IV}
                  {:pitch :F, :name :maj7, :context :chord-degree/IVmaj7}
                  {:pitch :F, :name :maj9, :context :chord-degree/IVmaj9}
                  {:pitch :F, :name :maj13, :context :chord-degree/IVmaj13}
                  {:pitch :F, :name :6, :context :chord-degree/IV6}
                  {:pitch :F, :name :6add9, :context :chord-degree/IV6add9}
                  {:pitch :F, :name :maj#4, :context :chord-degree/IVmaj#4}
                  {:pitch :F, :name :sus2, :context :chord-degree/ivsus2}
                  {:pitch :F, :name :5, :context :chord-degree/iv5}
                  {:pitch :F, :name :maj9#11, :context :chord-degree/IVmaj9#11}
                  {:pitch :F, :name :M6#11, :context :chord-degree/IVM6#11}
                  {:pitch :F, :name :M7add13, :context :chord-degree/IVM7add13}
                  {:pitch :F, :name :69#11, :context :chord-degree/IV69#11}
                  {:pitch :F, :name :M13#11, :context :chord-degree/IVM13#11}
                  {:pitch :F, :name :Madd9, :context :chord-degree/IVMadd9}
                  {:pitch :G, :name :maj, :context :chord-degree/V}
                  {:pitch :G, :name :6, :context :chord-degree/V6}
                  {:pitch :G, :name :6add9, :context :chord-degree/V6add9}
                  {:pitch :G, :name :7, :context :chord-degree/V7}
                  {:pitch :G, :name :9, :context :chord-degree/V9}
                  {:pitch :G, :name :13, :context :chord-degree/V13}
                  {:pitch :G, :name :sus4, :context :chord-degree/vsus4}
                  {:pitch :G, :name :sus2, :context :chord-degree/vsus2}
                  {:pitch :G, :name :7sus4, :context :chord-degree/v7sus4}
                  {:pitch :G, :name :11, :context :chord-degree/v11}
                  {:pitch :G, :name :5, :context :chord-degree/v5}
                  {:pitch :G, :name :sus24, :context :chord-degree/vsus24}
                  {:pitch :G, :name :7add6, :context :chord-degree/V7add6}
                  {:pitch :G, :name :Madd9, :context :chord-degree/VMadd9}
                  {:pitch :G, :name :7no5, :context :chord-degree/V7no5}
                  {:pitch :G, :name :9no5, :context :chord-degree/V9no5}
                  {:pitch :G, :name :13no5, :context :chord-degree/V13no5}
                  {:pitch :G, :name :9sus4, :context :chord-degree/v9sus4}
                  {:pitch :G, :name :13sus4, :context :chord-degree/v13sus4}
                  {:pitch :A, :name :m, :context :chord-degree/vi}
                  {:pitch :A, :name :m7, :context :chord-degree/vim7}
                  {:pitch :A, :name :m9, :context :chord-degree/vim9}
                  {:pitch :A, :name :m11, :context :chord-degree/vim11}
                  {:pitch :A, :name :sus4, :context :chord-degree/visus4}
                  {:pitch :A, :name :sus2, :context :chord-degree/visus2}
                  {:pitch :A, :name :7sus4, :context :chord-degree/vi7sus4}
                  {:pitch :A, :name :11, :context :chord-degree/vi11}
                  {:pitch :A, :name :5, :context :chord-degree/vi5}
                  {:pitch :A, :name :sus24, :context :chord-degree/visus24}
                  {:pitch :A, :name :madd4, :context :chord-degree/vimadd4}
                  {:pitch :A, :name :m7add11, :context :chord-degree/vim7add11}
                  {:pitch :A, :name :madd9, :context :chord-degree/vimadd9}
                  {:pitch :A, :name :m7#5, :context :chord-degree/vim7#5}
                  {:pitch :A, :name :m9#5, :context :chord-degree/vim9#5}
                  {:pitch :A, :name :9sus4, :context :chord-degree/vi9sus4}
                  {:pitch :A, :name :q, :context :chord-degree/viq}
                  {:pitch :B, :name :dim, :context :chord-degree/viidim}
                  {:pitch :B, :name :m7b5, :context :chord-degree/viim7b5}
                  {:pitch :B, :name :m7#5, :context :chord-degree/viim7#5}
                  {:pitch :B, :name :mb6b9, :context :chord-degree/viimb6b9}
                  {:pitch :B, :name :q, :context :chord-degree/viiq})

      :C :melodic-minor '({:pitch :C, :name :m, :context :chord-degree/i}
                          {:pitch :C, :name :mMaj7, :context :chord-degree/imMaj7}
                          {:pitch :C, :name :m6, :context :chord-degree/im6}
                          {:pitch :C, :name :mM9, :context :chord-degree/imM9}
                          {:pitch :C, :name :sus4, :context :chord-degree/isus4}
                          {:pitch :C, :name :sus2, :context :chord-degree/isus2}
                          {:pitch :C, :name :5, :context :chord-degree/i5}
                          {:pitch :C, :name :sus24, :context :chord-degree/isus24}
                          {:pitch :C, :name :m69, :context :chord-degree/im69}
                          {:pitch :C, :name :madd4, :context :chord-degree/imadd4}
                          {:pitch :C, :name :madd9, :context :chord-degree/imadd9}
                          {:pitch :C, :name :M7sus4, :context :chord-degree/iM7sus4}
                          {:pitch :C, :name :M9sus4, :context :chord-degree/iM9sus4}
                          {:pitch :D, :name :m, :context :chord-degree/ii}
                          {:pitch :D, :name :m7, :context :chord-degree/iim7}
                          {:pitch :D, :name :m6, :context :chord-degree/iim6}
                          {:pitch :D, :name :sus4, :context :chord-degree/iisus4}
                          {:pitch :D, :name :7sus4, :context :chord-degree/ii7sus4}
                          {:pitch :D, :name :b9sus, :context :chord-degree/iib9sus}
                          {:pitch :D, :name :5, :context :chord-degree/ii5}
                          {:pitch :D, :name :madd4, :context :chord-degree/iimadd4}
                          {:pitch :D, :name :m7add11, :context :chord-degree/iim7add11}
                          {:pitch :D, :name :q, :context :chord-degree/iiq}
                          {:pitch :D, :name :11b9, :context :chord-degree/ii11b9}
                          {:pitch :Eb, :name :aug, :context :chord-degree/bIIIaug}
                          {:pitch :Eb, :name :maj7#5, :context :chord-degree/bIIImaj7#5}
                          {:pitch :Eb, :name :maj9#5, :context :chord-degree/bIIImaj9#5}
                          {:pitch :Eb, :name :M#5add9, :context :chord-degree/bIIIM#5add9}
                          {:pitch :F, :name :maj, :context :chord-degree/IV}
                          {:pitch :F, :name :6, :context :chord-degree/IV6}
                          {:pitch :F, :name :6add9, :context :chord-degree/IV6add9}
                          {:pitch :F, :name :7, :context :chord-degree/IV7}
                          {:pitch :F, :name :9, :context :chord-degree/IV9}
                          {:pitch :F, :name :13, :context :chord-degree/IV13}
                          {:pitch :F, :name :7#11, :context :chord-degree/IV7#11}
                          {:pitch :F, :name :sus2, :context :chord-degree/ivsus2}
                          {:pitch :F, :name :5, :context :chord-degree/iv5}
                          {:pitch :F, :name :M6#11, :context :chord-degree/IVM6#11}
                          {:pitch :F, :name :69#11, :context :chord-degree/IV69#11}
                          {:pitch :F, :name :7add6, :context :chord-degree/IV7add6}
                          {:pitch :F, :name :9#11, :context :chord-degree/IV9#11}
                          {:pitch :F, :name :13#11, :context :chord-degree/IV13#11}
                          {:pitch :F, :name :Madd9, :context :chord-degree/IVMadd9}
                          {:pitch :F, :name :7no5, :context :chord-degree/IV7no5}
                          {:pitch :F, :name :9no5, :context :chord-degree/IV9no5}
                          {:pitch :F, :name :13no5, :context :chord-degree/IV13no5}
                          {:pitch :G, :name :maj, :context :chord-degree/V}
                          {:pitch :G, :name :7, :context :chord-degree/V7}
                          {:pitch :G, :name :9, :context :chord-degree/V9}
                          {:pitch :G, :name :sus4, :context :chord-degree/vsus4}
                          {:pitch :G, :name :sus2, :context :chord-degree/vsus2}
                          {:pitch :G, :name :7sus4, :context :chord-degree/v7sus4}
                          {:pitch :G, :name :11, :context :chord-degree/v11}
                          {:pitch :G, :name :5, :context :chord-degree/v5}
                          {:pitch :G, :name :sus24, :context :chord-degree/vsus24}
                          {:pitch :G, :name :7b6, :context :chord-degree/V7b6}
                          {:pitch :G, :name :Madd9, :context :chord-degree/VMadd9}
                          {:pitch :G, :name :7no5, :context :chord-degree/V7no5}
                          {:pitch :G, :name :7b13, :context :chord-degree/V7b13}
                          {:pitch :G, :name :9no5, :context :chord-degree/V9no5}
                          {:pitch :G, :name :9b13, :context :chord-degree/V9b13}
                          {:pitch :G, :name :9sus4, :context :chord-degree/v9sus4}
                          {:pitch :A, :name :dim, :context :chord-degree/vidim}
                          {:pitch :A, :name :m7b5, :context :chord-degree/vim7b5}
                          {:pitch :A, :name :m7#5, :context :chord-degree/vim7#5}
                          {:pitch :A, :name :m9#5, :context :chord-degree/vim9#5}
                          {:pitch :A, :name :m9b5, :context :chord-degree/vim9b5}
                          {:pitch :A, :name :q, :context :chord-degree/viq}
                          {:pitch :B, :name :dim, :context :chord-degree/viidim}
                          {:pitch :B, :name :m7b5, :context :chord-degree/viim7b5}
                          {:pitch :B, :name :m7#5, :context :chord-degree/viim7#5}
                          {:pitch :B, :name :mb6b9, :context :chord-degree/viimb6b9})

      :C :diminished '({:pitch :C, :name :dim, :context :chord-degree/idim}
                       {:pitch :C, :name :dim7M7, :context :chord-degree/idim7M7}
                       {:pitch :C, :name :dimM7, :context :chord-degree/idimM7}
                       {:pitch :C, :name :mb6M7, :context :chord-degree/imb6M7}
                       {:pitch :D, :name :m, :context :chord-degree/ii}
                       {:pitch :D, :name :m7, :context :chord-degree/iim7}
                       {:pitch :D, :name :m6, :context :chord-degree/iim6}
                       {:pitch :D, :name :dim, :context :chord-degree/iidim}
                       {:pitch :D, :name :m7b5, :context :chord-degree/iim7b5}
                       {:pitch :D, :name :5, :context :chord-degree/ii5}
                       {:pitch :Eb, :name :m#5, :context :chord-degree/biiim#5}
                       {:pitch :Eb, :name :M7#5sus4, :context :chord-degree/biiiM7#5sus4}
                       {:pitch :Eb, :name :M9#5sus4, :context :chord-degree/biiiM9#5sus4}
                       {:pitch :F, :name :maj, :context :chord-degree/IV}
                       {:pitch :F, :name :6, :context :chord-degree/IV6}
                       {:pitch :F, :name :m, :context :chord-degree/iv}
                       {:pitch :F, :name :m7, :context :chord-degree/ivm7}
                       {:pitch :F, :name :m6, :context :chord-degree/ivm6}
                       {:pitch :F, :name :7, :context :chord-degree/IV7}
                       {:pitch :F, :name :7#11, :context :chord-degree/IV7#11}
                       {:pitch :F, :name :7b9, :context :chord-degree/IV7b9}
                       {:pitch :F, :name :alt7, :context :chord-degree/IValt7}
                       {:pitch :F, :name :5, :context :chord-degree/iv5}
                       {:pitch :F, :name :M6#11, :context :chord-degree/IVM6#11}
                       {:pitch :F, :name :7add6, :context :chord-degree/IV7add6}
                       {:pitch :F, :name :7b9#11, :context :chord-degree/IV7b9#11}
                       {:pitch :F, :name :13b9#11, :context :chord-degree/IV13b9#11}
                       {:pitch :F, :name :13b9, :context :chord-degree/IV13b9}
                       {:pitch :F, :name :Maddb9, :context :chord-degree/IVMaddb9}
                       {:pitch :F, :name :7no5, :context :chord-degree/IV7no5}
                       {:pitch :Ab, :name :maj, :context :chord-degree/bVI}
                       {:pitch :Ab, :name :6, :context :chord-degree/bVI6}
                       {:pitch :Ab, :name :7, :context :chord-degree/bVI7}
                       {:pitch :Ab, :name :7#11, :context :chord-degree/bVI7#11}
                       {:pitch :Ab, :name :7#9, :context :chord-degree/bVI7#9}
                       {:pitch :Ab, :name :5, :context :chord-degree/bvi5}
                       {:pitch :Ab, :name :M6#11, :context :chord-degree/bVIM6#11}
                       {:pitch :Ab, :name :7add6, :context :chord-degree/bVI7add6}
                       {:pitch :Ab, :name :7#9#11, :context :chord-degree/bVI7#9#11}
                       {:pitch :Ab, :name :13#9#11, :context :chord-degree/bVI13#9#11}
                       {:pitch :Ab, :name :13#9, :context :chord-degree/bVI13#9}
                       {:pitch :Ab, :name :7no5, :context :chord-degree/bVI7no5}
                       {:pitch :A, :name :dim, :context :chord-degree/vidim}
                       {:pitch :A, :name :dim7, :context :chord-degree/vidim7}
                       {:pitch :B, :name :dim, :context :chord-degree/viidim}
                       {:pitch :B, :name :dim7, :context :chord-degree/viidim7}
                       {:pitch :B, :name :m7b5, :context :chord-degree/viim7b5})))

  (testing "chord->scales"
    (are+ [pitch chord-name expected] (= (set expected) (set (map #(select-keys % [:pitch :name :context])
                                                                  (jigsaw/chord->scales (jigsaw/->shape pitch chord-name)))))
      :C :maj [{:pitch :C, :name :major, :context :scale-degree/I}
               {:pitch :C, :name :major-pentatonic, :context :scale-degree/I}
               {:pitch :C, :name :major-blues, :context :scale-degree/I}
               {:pitch :C, :name :bebop, :context :scale-degree/I}
               {:pitch :C, :name :lydian, :context :scale-degree/I}
               {:pitch :C, :name :mixolydian, :context :scale-degree/I}
               {:pitch :C, :name :ionian-pentatonic, :context :scale-degree/I}
               {:pitch :C, :name :mixolydian-pentatonic, :context :scale-degree/I}
               {:pitch :C, :name :lydian-pentatonic, :context :scale-degree/I}
               {:pitch :C, :name :flat-six-pentatonic, :context :scale-degree/I}
               {:pitch :C, :name :lydian-dominant-pentatonic, :context :scale-degree/I}
               {:pitch :C, :name :augmented, :context :scale-degree/I}
               {:pitch :C, :name :double-harmonic-lydian, :context :scale-degree/I}
               {:pitch :C, :name :mixolydian-b6, :context :scale-degree/I}
               {:pitch :C, :name :lydian-dominant, :context :scale-degree/I}
               {:pitch :C, :name :augmented-heptatonic, :context :scale-degree/I}
               {:pitch :C, :name :lydian-minor, :context :scale-degree/I}
               {:pitch :C, :name :phrygian-dominant, :context :scale-degree/I}
               {:pitch :C, :name :harmonic-major, :context :scale-degree/I}
               {:pitch :C, :name :double-harmonic-major, :context :scale-degree/I}
               {:pitch :C, :name :hungarian-major, :context :scale-degree/I}
               {:pitch :C, :name :lydian-#9, :context :scale-degree/I}
               {:pitch :C, :name :spanish-heptatonic, :context :scale-degree/I}
               {:pitch :C, :name :bebop-minor, :context :scale-degree/I}
               {:pitch :C, :name :bebop-major, :context :scale-degree/I}
               {:pitch :C, :name :half-whole-diminished, :context :scale-degree/I}
               {:pitch :C, :name :composite-blues, :context :scale-degree/I}
               {:pitch :Db, :name :lydian-#9, :context :scale-degree/VII}
               {:pitch :D, :name :minor, :context :scale-degree/bVII}
               {:pitch :D, :name :bebop, :context :scale-degree/bVII}
               {:pitch :D, :name :dorian, :context :scale-degree/bVII}
               {:pitch :D, :name :mixolydian, :context :scale-degree/bVII}
               {:pitch :D, :name :locrian-major, :context :scale-degree/bVII}
               {:pitch :D, :name :locrian-#2, :context :scale-degree/bVII}
               {:pitch :D, :name :mixolydian-b6, :context :scale-degree/bVII}
               {:pitch :D, :name :bebop-minor, :context :scale-degree/bVII}
               {:pitch :D, :name :bebop-harmonic-minor, :context :scale-degree/bVII}
               {:pitch :D, :name :composite-blues, :context :scale-degree/bVII}
               {:pitch :D#, :name :ultralocrian, :context :scale-degree/bVII}
               {:pitch :E, :name :minor, :context :scale-degree/bVI}
               {:pitch :E, :name :harmonic-minor, :context :scale-degree/bVI}
               {:pitch :E, :name :diminished, :context :scale-degree/bVI}
               {:pitch :E, :name :phrygian, :context :scale-degree/bVI}
               {:pitch :E, :name :locrian, :context :scale-degree/bVI}
               {:pitch :E, :name :tizita, :context :scale-degree/bVI}
               {:pitch :E, :name :altered, :context :scale-degree/bVI}
               {:pitch :E, :name :locrian-#2, :context :scale-degree/bVI}
               {:pitch :E, :name :ultralocrian, :context :scale-degree/bVI}
               {:pitch :E, :name :hungarian-minor, :context :scale-degree/bVI}
               {:pitch :E, :name :spanish-heptatonic, :context :scale-degree/bVI}
               {:pitch :E, :name :bebop-locrian, :context :scale-degree/bVI}
               {:pitch :E, :name :bebop-harmonic-minor, :context :scale-degree/bVI}
               {:pitch :E, :name :minor-six-diminished, :context :scale-degree/bVI}
               {:pitch :F, :name :major, :context :scale-degree/V}
               {:pitch :F, :name :melodic-minor, :context :scale-degree/V}
               {:pitch :F, :name :harmonic-minor, :context :scale-degree/V}
               {:pitch :F, :name :bebop, :context :scale-degree/V}
               {:pitch :F, :name :lydian, :context :scale-degree/V}
               {:pitch :F, :name :minor-hexatonic, :context :scale-degree/V}
               {:pitch :F, :name :lydian-diminished, :context :scale-degree/V}
               {:pitch :F, :name :harmonic-major, :context :scale-degree/V}
               {:pitch :F, :name :hungarian-minor, :context :scale-degree/V}
               {:pitch :F, :name :bebop-major, :context :scale-degree/V}
               {:pitch :F, :name :bebop-harmonic-minor, :context :scale-degree/V}
               {:pitch :F, :name :minor-six-diminished, :context :scale-degree/V}
               {:pitch :F#, :name :locrian, :context :scale-degree/bV}
               {:pitch :F#, :name :altered, :context :scale-degree/#IV}
               {:pitch :F#, :name :locrian-6, :context :scale-degree/bV}
               {:pitch :F#, :name :bebop-locrian, :context :scale-degree/bV}
               {:pitch :G, :name :major, :context :scale-degree/IV}
               {:pitch :G, :name :melodic-minor, :context :scale-degree/IV}
               {:pitch :G, :name :bebop, :context :scale-degree/IV}
               {:pitch :G, :name :diminished, :context :scale-degree/IV}
               {:pitch :G, :name :dorian, :context :scale-degree/IV}
               {:pitch :G, :name :mixolydian, :context :scale-degree/IV}
               {:pitch :G, :name :minor-six-pentatonic, :context :scale-degree/IV}
               {:pitch :G, :name :dorian-b2, :context :scale-degree/IV}
               {:pitch :G, :name :locrian-6, :context :scale-degree/IV}
               {:pitch :G, :name :major-augmented, :context :scale-degree/IV}
               {:pitch :G, :name :bebop-minor, :context :scale-degree/IV}
               {:pitch :G, :name :bebop-major, :context :scale-degree/IV}
               {:pitch :G, :name :minor-six-diminished, :context :scale-degree/IV}
               {:pitch :G, :name :composite-blues, :context :scale-degree/IV}
               {:pitch :Ab, :name :lydian-#5P-pentatonic, :context :scale-degree/III}
               {:pitch :Ab, :name :augmented, :context :scale-degree/III}
               {:pitch :Ab, :name :lydian-augmented, :context :scale-degree/III}
               {:pitch :Ab, :name :augmented-heptatonic, :context :scale-degree/III}
               {:pitch :Ab, :name :leading-whole-tone, :context :scale-degree/III}
               {:pitch :Ab, :name :major-augmented, :context :scale-degree/III}
               {:pitch :Ab, :name :bebop-major, :context :scale-degree/III}
               {:pitch :A, :name :minor, :context :scale-degree/bIII}
               {:pitch :A, :name :minor-blues, :context :scale-degree/bIII}
               {:pitch :A, :name :dorian, :context :scale-degree/bIII}
               {:pitch :A, :name :phrygian, :context :scale-degree/bIII}
               {:pitch :A, :name :minor-pentatonic, :context :scale-degree/bIII}
               {:pitch :A, :name :dorian-b2, :context :scale-degree/bIII}
               {:pitch :A, :name :dorian-#4, :context :scale-degree/bIII}
               {:pitch :A, :name :spanish-heptatonic, :context :scale-degree/bIII}
               {:pitch :A, :name :bebop-minor, :context :scale-degree/bIII}
               {:pitch :A, :name :bebop-locrian, :context :scale-degree/bIII}
               {:pitch :A, :name :bebop-harmonic-minor, :context :scale-degree/bIII}
               {:pitch :A, :name :half-whole-diminished, :context :scale-degree/bIII}
               {:pitch :A, :name :composite-blues, :context :scale-degree/bIII}
               {:pitch :Bb, :name :lydian, :context :scale-degree/II}
               {:pitch :Bb, :name :lydian-dominant, :context :scale-degree/II}
               {:pitch :Bb, :name :lydian-augmented, :context :scale-degree/II}
               {:pitch :Bb, :name :dorian-#4, :context :scale-degree/II}
               {:pitch :Bb, :name :lydian-diminished, :context :scale-degree/II}
               {:pitch :B, :name :phrygian, :context :scale-degree/bII}
               {:pitch :B, :name :locrian, :context :scale-degree/bII}
               {:pitch :B, :name :phrygian-dominant, :context :scale-degree/bII}
               {:pitch :B, :name :double-harmonic-major, :context :scale-degree/bII}
               {:pitch :B, :name :spanish-heptatonic, :context :scale-degree/bII}
               {:pitch :B, :name :bebop-locrian, :context :scale-degree/bII}]

      :Eb :6add9 [{:pitch :C, :name :minor, :context :scale-degree/bIII}
                  {:pitch :C, :name :minor-blues, :context :scale-degree/bIII}
                  {:pitch :C, :name :dorian, :context :scale-degree/bIII}
                  {:pitch :C, :name :phrygian, :context :scale-degree/bIII}
                  {:pitch :C, :name :minor-pentatonic, :context :scale-degree/bIII}
                  {:pitch :C, :name :dorian-b2, :context :scale-degree/bIII}
                  {:pitch :C, :name :spanish-heptatonic, :context :scale-degree/bIII}
                  {:pitch :C, :name :bebop-minor, :context :scale-degree/bIII}
                  {:pitch :C, :name :bebop-locrian, :context :scale-degree/bIII}
                  {:pitch :C, :name :bebop-harmonic-minor, :context :scale-degree/bIII}
                  {:pitch :C, :name :composite-blues, :context :scale-degree/bIII}
                  {:pitch :Db, :name :lydian, :context :scale-degree/II}
                  {:pitch :Db, :name :lydian-augmented, :context :scale-degree/II}
                  {:pitch :D, :name :phrygian, :context :scale-degree/bII}
                  {:pitch :D, :name :locrian, :context :scale-degree/bII}
                  {:pitch :D, :name :spanish-heptatonic, :context :scale-degree/bII}
                  {:pitch :D, :name :bebop-locrian, :context :scale-degree/bII}
                  {:pitch :Eb, :name :major, :context :scale-degree/I}
                  {:pitch :Eb, :name :major-pentatonic, :context :scale-degree/I}
                  {:pitch :Eb, :name :major-blues, :context :scale-degree/I}
                  {:pitch :Eb, :name :bebop, :context :scale-degree/I}
                  {:pitch :Eb, :name :lydian, :context :scale-degree/I}
                  {:pitch :Eb, :name :mixolydian, :context :scale-degree/I}
                  {:pitch :Eb, :name :lydian-dominant, :context :scale-degree/I}
                  {:pitch :Eb, :name :bebop-minor, :context :scale-degree/I}
                  {:pitch :Eb, :name :bebop-major, :context :scale-degree/I}
                  {:pitch :Eb, :name :composite-blues, :context :scale-degree/I}
                  {:pitch :F, :name :minor, :context :scale-degree/bVII}
                  {:pitch :F, :name :bebop, :context :scale-degree/bVII}
                  {:pitch :F, :name :dorian, :context :scale-degree/bVII}
                  {:pitch :F, :name :mixolydian, :context :scale-degree/bVII}
                  {:pitch :F, :name :mixolydian-b6, :context :scale-degree/bVII}
                  {:pitch :F, :name :bebop-minor, :context :scale-degree/bVII}
                  {:pitch :F, :name :bebop-harmonic-minor, :context :scale-degree/bVII}
                  {:pitch :F, :name :composite-blues, :context :scale-degree/bVII}
                  {:pitch :G, :name :minor, :context :scale-degree/bVI}
                  {:pitch :G, :name :phrygian, :context :scale-degree/bVI}
                  {:pitch :G, :name :locrian, :context :scale-degree/bVI}
                  {:pitch :G, :name :locrian-#2, :context :scale-degree/bVI}
                  {:pitch :G, :name :spanish-heptatonic, :context :scale-degree/bVI}
                  {:pitch :G, :name :bebop-locrian, :context :scale-degree/bVI}
                  {:pitch :G, :name :bebop-harmonic-minor, :context :scale-degree/bVI}
                  {:pitch :Ab, :name :major, :context :scale-degree/V}
                  {:pitch :Ab, :name :bebop, :context :scale-degree/V}
                  {:pitch :Ab, :name :lydian, :context :scale-degree/V}
                  {:pitch :Ab, :name :bebop-major, :context :scale-degree/V}
                  {:pitch :A, :name :locrian, :context :scale-degree/bV}
                  {:pitch :A, :name :altered, :context :scale-degree/#IV}
                  {:pitch :A, :name :bebop-locrian, :context :scale-degree/bV}
                  {:pitch :Bb, :name :major, :context :scale-degree/IV}
                  {:pitch :Bb, :name :melodic-minor, :context :scale-degree/IV}
                  {:pitch :Bb, :name :bebop, :context :scale-degree/IV}
                  {:pitch :Bb, :name :dorian, :context :scale-degree/IV}
                  {:pitch :Bb, :name :mixolydian, :context :scale-degree/IV}
                  {:pitch :Bb, :name :bebop-minor, :context :scale-degree/IV}
                  {:pitch :Bb, :name :bebop-major, :context :scale-degree/IV}
                  {:pitch :Bb, :name :minor-six-diminished, :context :scale-degree/IV}
                  {:pitch :Bb, :name :composite-blues, :context :scale-degree/IV}]

      :C :13sus4 [{:pitch :C, :name :bebop, :context :scale-degree/I}
                  {:pitch :C, :name :dorian, :context :scale-degree/I}
                  {:pitch :C, :name :mixolydian, :context :scale-degree/I}
                  {:pitch :C, :name :bebop-minor, :context :scale-degree/I}
                  {:pitch :C, :name :composite-blues, :context :scale-degree/I}
                  {:pitch :D, :name :minor, :context :scale-degree/bVII}
                  {:pitch :D, :name :phrygian, :context :scale-degree/bVII}
                  {:pitch :D, :name :spanish-heptatonic, :context :scale-degree/bVII}
                  {:pitch :D, :name :bebop-locrian, :context :scale-degree/bVII}
                  {:pitch :D, :name :bebop-harmonic-minor, :context :scale-degree/bVII}
                  {:pitch :Eb, :name :lydian, :context :scale-degree/VI}
                  {:pitch :E, :name :locrian, :context :scale-degree/bVI}
                  {:pitch :E, :name :bebop-locrian, :context :scale-degree/bVI}
                  {:pitch :F, :name :major, :context :scale-degree/V}
                  {:pitch :F, :name :bebop, :context :scale-degree/V}
                  {:pitch :F, :name :mixolydian, :context :scale-degree/V}
                  {:pitch :F, :name :bebop-minor, :context :scale-degree/V}
                  {:pitch :F, :name :bebop-major, :context :scale-degree/V}
                  {:pitch :F, :name :composite-blues, :context :scale-degree/V}
                  {:pitch :G, :name :minor, :context :scale-degree/IV}
                  {:pitch :G, :name :dorian, :context :scale-degree/IV}
                  {:pitch :G, :name :bebop-minor, :context :scale-degree/IV}
                  {:pitch :G, :name :bebop-harmonic-minor, :context :scale-degree/IV}
                  {:pitch :G, :name :composite-blues, :context :scale-degree/IV}
                  {:pitch :A, :name :phrygian, :context :scale-degree/bIII}
                  {:pitch :A, :name :locrian, :context :scale-degree/bIII}
                  {:pitch :A, :name :spanish-heptatonic, :context :scale-degree/bIII}
                  {:pitch :A, :name :bebop-locrian, :context :scale-degree/bIII}
                  {:pitch :Bb, :name :major, :context :scale-degree/II}
                  {:pitch :Bb, :name :bebop, :context :scale-degree/II}
                  {:pitch :Bb, :name :lydian, :context :scale-degree/II}
                  {:pitch :Bb, :name :bebop-major, :context :scale-degree/II}]))

  (testing "scale->modes"
    (are+ [base-scale modes] (= modes (map #(select-keys % [:pitch :name :context]) (jigsaw/scale->modes (jigsaw/->shape base-scale))))
      :C_major '({:pitch :D, :name :dorian :context :mode/II}
                 {:pitch :E, :name :phrygian :context :mode/III}
                 {:pitch :F, :name :lydian :context :mode/IV}
                 {:pitch :G, :name :mixolydian :context :mode/V}
                 {:pitch :A, :name :minor :context :mode/VI}
                 {:pitch :B, :name :locrian :context :mode/VII}
                 {:pitch :C, :name :minor :context :mode/parallel}
                 {:pitch :C, :name :harmonic-minor :context :mode/parallel}
                 {:pitch :C, :name :melodic-minor :context :mode/parallel})
      :C_minor '({:pitch :D, :name :locrian :context :mode/II}
                 {:pitch :Eb, :name :major :context :mode/III}
                 {:pitch :F, :name :dorian :context :mode/IV}
                 {:pitch :G, :name :phrygian :context :mode/V}
                 {:pitch :Ab, :name :lydian :context :mode/VI}
                 {:pitch :Bb, :name :mixolydian :context :mode/VII}
                 {:pitch :C, :name :major :context :mode/parallel})))

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
      :C_maj :C_major :scale-degree/I
      :C_major :D_m :chord-degree/ii
      :D_m :C_major :scale-degree/II
      :C_major :B_dim :chord-degree/viidim
      :B_dim :C_major :scale-degree/VII
      :C_major :D_dorian :mode/II
      :D_dorian :C_major :mode/VII
      :C_major :C_minor :mode/parallel
      :C_minor :C_major :mode/parallel
      ; Relative key is just labeled as 6th/Aeolian mode
      :C_major :A_minor :mode/VI
      #_#_#_:Ebb_locrian :Ab_lydian :mode/V)))
