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
          :major   [:maj :m :m :maj :maj :m :dim]
          :dorian  [:m :m :maj :maj :m :dim :maj]
          :minor   [:m :dim :maj :m :m :maj :maj]))
      (testing "with 4 num-thirds"
        (are+ [scale-name expected] (= expected (search/scale-name->chords scale-name :num-thirds 4))
          :major   [:maj7 :m7 :m7 :maj7 :7 :m7 :m7b5]
          :dorian  [:m7 :m7 :maj7 :7 :m7 :m7b5 :maj7]
          :minor   [:m7 :m7b5 :maj7 :m7 :m7 :maj7 :7])))
    (testing "scale->chords"
      (are+ [pitch scale-name expected] (= expected (search/scale->chords (algo/resolve-shape pitch :scale scale-name)))
        :C :major '({:pitch :C, :name :maj}
                    {:pitch :D, :name :m}
                    {:pitch :E, :name :m}
                    {:pitch :F, :name :maj}
                    {:pitch :G, :name :maj}
                    {:pitch :A, :name :m}
                    {:pitch :B, :name :dim})))
    (testing "chord->scales"
      (are+ [pitch chord-name expected] (= (set expected) (set (search/chord->scales (algo/resolve-shape pitch :chord chord-name))))
        :C :maj [{:pitch :C, :degree :1, :name :lydian-dominant-pentatonic}
                 {:pitch :C, :degree :1, :name :bebop-major}
                 {:pitch :C, :degree :1, :name :lydian}
                 {:pitch :C, :degree :1, :name :hungarian-major}
                 {:pitch :C, :degree :1, :name :augmented-heptatonic}
                 {:pitch :C, :degree :1, :name :phrygian-dominant}
                 {:pitch :C, :degree :1, :name :mixolydian}
                 {:pitch :C, :degree :1, :name :composite-blues}
                 {:pitch :C, :degree :1, :name :augmented}
                 {:pitch :C, :degree :1, :name :double-harmonic-major}
                 {:pitch :C, :degree :1, :name :major}
                 {:pitch :C, :degree :1, :name :lydian-#9}
                 {:pitch :C, :degree :1, :name :bebop}
                 {:pitch :C, :degree :1, :name :major-pentatonic}
                 {:pitch :C, :degree :1, :name :flat-six-pentatonic}
                 {:pitch :C, :degree :1, :name :major-blues}
                 {:pitch :C, :degree :1, :name :ionian-pentatonic}
                 {:pitch :C, :degree :1, :name :double-harmonic-lydian}
                 {:pitch :C, :degree :1, :name :bebop-minor}
                 {:pitch :C, :degree :1, :name :spanish-heptatonic}
                 {:pitch :C, :degree :1, :name :harmonic-major}
                 {:pitch :C, :degree :1, :name :lydian-dominant}
                 {:pitch :C, :degree :1, :name :mixolydian-b6}
                 {:pitch :C, :degree :1, :name :mixolydian-pentatonic}
                 {:pitch :C, :degree :1, :name :half-whole-diminished}
                 {:pitch :C, :degree :1, :name :lydian-minor}
                 {:pitch :C, :degree :1, :name :lydian-pentatonic}
                 {:pitch :B, :degree :b2, :name :locrian}
                 {:pitch :B, :degree :b2, :name :phrygian-dominant}
                 {:pitch :B, :degree :b2, :name :bebop-locrian}
                 {:pitch :B, :degree :b2, :name :double-harmonic-major}
                 {:pitch :B, :degree :b2, :name :phrygian}
                 {:pitch :B, :degree :b2, :name :spanish-heptatonic}
                 {:pitch :Bb, :degree :2, :name :dorian-#4}
                 {:pitch :Bb, :degree :2, :name :lydian}
                 {:pitch :Bb, :degree :2, :name :lydian-augmented}
                 {:pitch :Bb, :degree :2, :name :lydian-diminished}
                 {:pitch :Bb, :degree :2, :name :lydian-dominant}
                 {:pitch :A, :degree :b3, :name :dorian-#4}
                 {:pitch :A, :degree :b3, :name :dorian}
                 {:pitch :A, :degree :b3, :name :bebop-locrian}
                 {:pitch :A, :degree :b3, :name :composite-blues}
                 {:pitch :A, :degree :b3, :name :bebop-harmonic-minor}
                 {:pitch :A, :degree :b3, :name :dorian-b2}
                 {:pitch :A, :degree :b3, :name :minor-pentatonic}
                 {:pitch :A, :degree :b3, :name :phrygian}
                 {:pitch :A, :degree :b3, :name :minor}
                 {:pitch :A, :degree :b3, :name :bebop-minor}
                 {:pitch :A, :degree :b3, :name :spanish-heptatonic}
                 {:pitch :A, :degree :b3, :name :minor-blues}
                 {:pitch :A, :degree :b3, :name :half-whole-diminished}
                 {:pitch :Ab, :degree :3, :name :bebop-major}
                 {:pitch :Ab, :degree :3, :name :augmented-heptatonic}
                 {:pitch :Ab, :degree :3, :name :augmented}
                 {:pitch :Ab, :degree :3, :name :lydian-augmented}
                 {:pitch :Ab, :degree :3, :name :leading-whole-tone}
                 {:pitch :Ab, :degree :3, :name :major-augmented}
                 {:pitch :Ab, :degree :3, :name :lydian-#5P-pentatonic}
                 {:pitch :G, :degree :4, :name :melodic-minor}
                 {:pitch :G, :degree :4, :name :bebop-major}
                 {:pitch :G, :degree :4, :name :dorian}
                 {:pitch :G, :degree :4, :name :minor-six-diminished}
                 {:pitch :G, :degree :4, :name :mixolydian}
                 {:pitch :G, :degree :4, :name :composite-blues}
                 {:pitch :G, :degree :4, :name :locrian-6}
                 {:pitch :G, :degree :4, :name :major}
                 {:pitch :G, :degree :4, :name :dorian-b2}
                 {:pitch :G, :degree :4, :name :bebop}
                 {:pitch :G, :degree :4, :name :minor-six-pentatonic}
                 {:pitch :G, :degree :4, :name :diminished}
                 {:pitch :G, :degree :4, :name :bebop-minor}
                 {:pitch :G, :degree :4, :name :major-augmented}
                 {:pitch :F#, :degree :#4, :name :altered}
                 {:pitch :F, :degree :5, :name :hungarian-minor}
                 {:pitch :F, :degree :5, :name :melodic-minor}
                 {:pitch :F, :degree :5, :name :bebop-major}
                 {:pitch :F, :degree :5, :name :lydian}
                 {:pitch :F, :degree :5, :name :minor-six-diminished}
                 {:pitch :F, :degree :5, :name :harmonic-minor}
                 {:pitch :F, :degree :5, :name :minor-hexatonic}
                 {:pitch :F, :degree :5, :name :bebop-harmonic-minor}
                 {:pitch :F, :degree :5, :name :major}
                 {:pitch :F, :degree :5, :name :lydian-diminished}
                 {:pitch :F, :degree :5, :name :bebop}
                 {:pitch :F, :degree :5, :name :harmonic-major}
                 {:pitch :F#, :degree :b5, :name :locrian}
                 {:pitch :F#, :degree :b5, :name :bebop-locrian}
                 {:pitch :F#, :degree :b5, :name :locrian-6}
                 {:pitch :E, :degree :b6, :name :hungarian-minor}
                 {:pitch :E, :degree :b6, :name :locrian}
                 {:pitch :E, :degree :b6, :name :minor-six-diminished}
                 {:pitch :E, :degree :b6, :name :harmonic-minor}
                 {:pitch :E, :degree :b6, :name :bebop-locrian}
                 {:pitch :E, :degree :b6, :name :altered}
                 {:pitch :E, :degree :b6, :name :locrian-#2}
                 {:pitch :E, :degree :b6, :name :bebop-harmonic-minor}
                 {:pitch :E, :degree :b6, :name :ultralocrian}
                 {:pitch :E, :degree :b6, :name :phrygian}
                 {:pitch :E, :degree :b6, :name :minor}
                 {:pitch :E, :degree :b6, :name :diminished}
                 {:pitch :E, :degree :b6, :name :spanish-heptatonic}
                 {:pitch :Eb, :degree :6, :name :half-whole-diminished}
                 {:pitch :Db, :degree :7, :name :lydian-#9}
                 {:pitch :D, :degree :b7, :name :dorian}
                 {:pitch :D, :degree :b7, :name :locrian-major}
                 {:pitch :D, :degree :b7, :name :mixolydian}
                 {:pitch :D, :degree :b7, :name :composite-blues}
                 {:pitch :D, :degree :b7, :name :locrian-#2}
                 {:pitch :D, :degree :b7, :name :bebop-harmonic-minor}
                 {:pitch :D, :degree :b7, :name :bebop}
                 {:pitch :D, :degree :b7, :name :minor}
                 {:pitch :D, :degree :b7, :name :bebop-minor}
                 {:pitch :D, :degree :b7, :name :mixolydian-b6}
                 {:pitch :D#, :degree :b7, :name :ultralocrian}
                 {:pitch :Db, :degree :7, :name :diminished}]

        :Eb :6add9 [{:pitch :Eb, :degree :1, :name :bebop-major}
                    {:pitch :Eb, :degree :1, :name :lydian}
                    {:pitch :Eb, :degree :1, :name :mixolydian}
                    {:pitch :Eb, :degree :1, :name :composite-blues}
                    {:pitch :Eb, :degree :1, :name :major}
                    {:pitch :Eb, :degree :1, :name :bebop}
                    {:pitch :Eb, :degree :1, :name :major-pentatonic}
                    {:pitch :Eb, :degree :1, :name :major-blues}
                    {:pitch :Eb, :degree :1, :name :bebop-minor}
                    {:pitch :Eb, :degree :1, :name :lydian-dominant}
                    {:pitch :Db, :degree :2, :name :lydian}
                    {:pitch :Db, :degree :2, :name :lydian-augmented}
                    {:pitch :D, :degree :b2, :name :locrian}
                    {:pitch :D, :degree :b2, :name :bebop-locrian}
                    {:pitch :D, :degree :b2, :name :phrygian}
                    {:pitch :D, :degree :b2, :name :spanish-heptatonic}
                    {:pitch :C, :degree :b3, :name :dorian}
                    {:pitch :C, :degree :b3, :name :bebop-locrian}
                    {:pitch :C, :degree :b3, :name :composite-blues}
                    {:pitch :C, :degree :b3, :name :bebop-harmonic-minor}
                    {:pitch :C, :degree :b3, :name :dorian-b2}
                    {:pitch :C, :degree :b3, :name :minor-pentatonic}
                    {:pitch :C, :degree :b3, :name :phrygian}
                    {:pitch :C, :degree :b3, :name :minor}
                    {:pitch :C, :degree :b3, :name :bebop-minor}
                    {:pitch :C, :degree :b3, :name :spanish-heptatonic}
                    {:pitch :C, :degree :b3, :name :minor-blues}
                    {:pitch :A, :degree :#4, :name :altered}
                    {:pitch :Bb, :degree :4, :name :melodic-minor}
                    {:pitch :Bb, :degree :4, :name :bebop-major}
                    {:pitch :Bb, :degree :4, :name :dorian}
                    {:pitch :Bb, :degree :4, :name :minor-six-diminished}
                    {:pitch :Bb, :degree :4, :name :mixolydian}
                    {:pitch :Bb, :degree :4, :name :composite-blues}
                    {:pitch :Bb, :degree :4, :name :major}
                    {:pitch :Bb, :degree :4, :name :bebop}
                    {:pitch :Bb, :degree :4, :name :bebop-minor}
                    {:pitch :A, :degree :b5, :name :locrian}
                    {:pitch :A, :degree :b5, :name :bebop-locrian}
                    {:pitch :Ab, :degree :5, :name :bebop-major}
                    {:pitch :Ab, :degree :5, :name :lydian}
                    {:pitch :Ab, :degree :5, :name :major}
                    {:pitch :Ab, :degree :5, :name :bebop}
                    {:pitch :G, :degree :b6, :name :locrian}
                    {:pitch :G, :degree :b6, :name :bebop-locrian}
                    {:pitch :G, :degree :b6, :name :locrian-#2}
                    {:pitch :G, :degree :b6, :name :bebop-harmonic-minor}
                    {:pitch :G, :degree :b6, :name :phrygian}
                    {:pitch :G, :degree :b6, :name :minor}
                    {:pitch :G, :degree :b6, :name :spanish-heptatonic}
                    {:pitch :F, :degree :b7, :name :dorian}
                    {:pitch :F, :degree :b7, :name :mixolydian}
                    {:pitch :F, :degree :b7, :name :composite-blues}
                    {:pitch :F, :degree :b7, :name :bebop-harmonic-minor}
                    {:pitch :F, :degree :b7, :name :bebop}
                    {:pitch :F, :degree :b7, :name :minor}
                    {:pitch :F, :degree :b7, :name :bebop-minor}
                    {:pitch :F, :degree :b7, :name :mixolydian-b6}]

        :C :13sus4 [{:pitch :C, :degree :1, :name :dorian}
                    {:pitch :C, :degree :1, :name :mixolydian}
                    {:pitch :C, :degree :1, :name :composite-blues}
                    {:pitch :C, :degree :1, :name :bebop}
                    {:pitch :C, :degree :1, :name :bebop-minor}
                    {:pitch :Bb, :degree :2, :name :bebop-major}
                    {:pitch :Bb, :degree :2, :name :lydian}
                    {:pitch :Bb, :degree :2, :name :major}
                    {:pitch :Bb, :degree :2, :name :bebop}
                    {:pitch :A, :degree :b3, :name :locrian}
                    {:pitch :A, :degree :b3, :name :bebop-locrian}
                    {:pitch :A, :degree :b3, :name :phrygian}
                    {:pitch :A, :degree :b3, :name :spanish-heptatonic}
                    {:pitch :G, :degree :4, :name :dorian}
                    {:pitch :G, :degree :4, :name :composite-blues}
                    {:pitch :G, :degree :4, :name :bebop-harmonic-minor}
                    {:pitch :G, :degree :4, :name :minor}
                    {:pitch :G, :degree :4, :name :bebop-minor}
                    {:pitch :F, :degree :5, :name :bebop-major}
                    {:pitch :F, :degree :5, :name :mixolydian}
                    {:pitch :F, :degree :5, :name :composite-blues}
                    {:pitch :F, :degree :5, :name :major}
                    {:pitch :F, :degree :5, :name :bebop}
                    {:pitch :F, :degree :5, :name :bebop-minor}
                    {:pitch :E, :degree :b6, :name :locrian}
                    {:pitch :E, :degree :b6, :name :bebop-locrian}
                    {:pitch :Eb, :degree :6, :name :lydian}
                    {:pitch :D, :degree :b7, :name :bebop-locrian}
                    {:pitch :D, :degree :b7, :name :bebop-harmonic-minor}
                    {:pitch :D, :degree :b7, :name :phrygian}
                    {:pitch :D, :degree :b7, :name :minor}
                    {:pitch :D, :degree :b7, :name :spanish-heptatonic}]))
    (testing "with intervals->chord"
      (are+ [intervals want] (= want (search/intervals->chord intervals))
        [] nil
        [:P1 :M3] nil
        [:P1 :M3 :P5] :maj
        [:P1 :m3 :P5] :m))
    (testing "with intervals->chords"
      (are+ [intervals want] (= want (search/intervals->chords intervals))
        [] []
        [:P1 :m3 :P5 :m7 :P11] [:m7add11 :m11]))))
