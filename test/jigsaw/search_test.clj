(ns jigsaw.search-test
  (:require
   [clojure.test :refer [deftest testing]]
   [jigsaw.algo :as algo]
   [jigsaw.search :as search]
   [jigsaw.test-utils :refer [are+]]))

(deftest search-test
  (testing "Search"
    (testing "heuristics"
      (are+ [set1 set2 m] (= m (search/calculate-heuristics set1 set2))
        [] [] {:contained-in? 1
               :fully-contained-in? 0
               :contains? 1
               :fully-contains? 0
               :overlap 0.0
               :shares-root? 0}
        [:C] [] {:contained-in? 0
                 :fully-contained-in? 0
                 :contains? 1
                 :fully-contains? 1
                 :overlap 0.0
                 :shares-root? 0}
        [] [:C] {:contained-in? 1
                 :fully-contained-in? 1
                 :contains? 0
                 :fully-contains? 0
                 :overlap 0.0
                 :shares-root? 0}
        [:C] [:C] {:contained-in? 1
                   :fully-contained-in? 0
                   :contains? 1
                   :fully-contains? 0
                   :overlap 1.0
                   :shares-root? 1}
        [:C] [:C :D] {:contained-in? 1
                      :fully-contained-in? 1
                      :contains? 0
                      :fully-contains? 0
                      :overlap 0.5
                      :shares-root? 1}
        [:C :D] [:C] {:contained-in? 0
                      :fully-contained-in? 0
                      :contains? 1
                      :fully-contains? 1
                      :overlap 0.5
                      :shares-root? 1}
        [:C :D :E] [:C] {:contained-in? 0
                         :fully-contained-in? 0
                         :contains? 1
                         :fully-contains? 1
                         :overlap (float (/ 1 3))
                         :shares-root? 1}))
    (testing "scale-name->chords"
      (testing "with default num-thirds"
        (are+ [scale-name expected] (= expected (search/scale-name->chords scale-name))
          :major      [:maj :m :m :maj :maj :m :dim]
          :dorian     [:m :m :maj :maj :m :dim :maj]
          :diminished [:dim :dim nil nil nil nil :dim :dim]
          :minor      [:m :dim :maj :m :m :maj :maj]))
      (testing "with 4 num-thirds"
        (are+ [scale-name expected] (= expected (search/scale-name->chords scale-name :num-thirds 4))
          :major   [:maj7 :m7 :m7 :maj7 :7 :m7 :m7b5]
          :dorian  [:m7 :m7 :maj7 :7 :m7 :m7b5 :maj7]
          :diminished [nil nil nil nil nil nil :dim7 :dim7]
          :minor   [:m7 :m7b5 :maj7 :m7 :m7 :maj7 :7])))
    (testing "scale->chords"
      (are+ [pitch scale-name expected] (= expected (map #(select-keys % [:pitch :name]) (search/scale->chords (algo/->shape pitch :scale scale-name))))
        :C :major '({:pitch :C, :name :maj}
                    {:pitch :D, :name :m}
                    {:pitch :E, :name :m}
                    {:pitch :F, :name :maj}
                    {:pitch :G, :name :maj}
                    {:pitch :A, :name :m}
                    {:pitch :B, :name :dim})
        :C :diminished '({:pitch :C, :name :dim}
                         {:pitch :D, :name :dim}
                         {:pitch :A, :name :dim}
                         {:pitch :B, :name :dim})))
    (testing "chord->scales"
      (are+ [pitch chord-name expected] (= (set expected) (set (map #(select-keys % [:pitch :degree :name]) (search/chord->scales (algo/->shape pitch :chord chord-name)))))
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
                 {:pitch :Eb, :name :half-whole-diminished, :degree :VI}
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
                 {:pitch :D#, :name :ultralocrian, :degree :bVII}
                 {:pitch :Db, :name :diminished, :degree :VII}]

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
    (testing "with intervals->chord"
      (are+ [intervals want] (= want (search/intervals->chord intervals))
        [] nil
        [:P1 :M3] nil
        [:P1 :M3 :P5] :maj
        [:P1 :m3 :P5] :m))
    (testing "with intervals->chords"
      (are+ [intervals want] (= want (search/intervals->chords intervals))
        [] []
        [:P1 :m3 :P5 :m7 :P11] [:m7add11 :m11]))
    (testing "with scale->mode"
      (are+ [base-scale-name mode-num want-scale-name] (= want-scale-name (:name (search/scale->mode (algo/->shape :C :scale base-scale-name) mode-num)))
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
    (testing "with scale->modes"
      (are+ [base-scale modes] (= modes (map #(select-keys % [:pitch :name]) (search/scale->modes base-scale)))
        (algo/->shape :C :scale :major) '({:pitch :C, :name :major}
                                          {:pitch :D, :name :dorian}
                                          {:pitch :E, :name :phrygian}
                                          {:pitch :F, :name :lydian}
                                          {:pitch :G, :name :mixolydian}
                                          {:pitch :A, :name :minor}
                                          {:pitch :B, :name :locrian})))))
