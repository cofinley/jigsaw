(ns jigsaw.logic-test
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}
                                 :invalid-arity {:level :off}}}}
  (:require
   [clojure.test :refer [deftest testing]]
   [clojure.core.logic :as l]
   [jigsaw.core :as jigsaw]
   [jigsaw.experiments.logic :as jl]
   [jigsaw.test-utils :refer [are+]]))

(deftest logic-test
  (testing "shape=="
    (are+ [run want] (= want run)
      (l/run 1 [q]
             (jl/with-fresh
               (l/== q {:pitch :C :name :maj :context :chord-degree/I})
               (jl/shape== q {:pitch :C :name :maj :context :chord-degree/IV})
               (jl/shape== q :C_maj)))
      '({:pitch :C, :name :maj, :context :chord-degree/I})))

  (testing "shape!="
    (are+ [run want] (= want run)
      (l/run 1 [q]
             (jl/with-fresh
               (l/== q {:pitch :C :name :maj :context :chord-degree/I})
               (jl/shape!= q {:pitch :D :name :maj :context :chord-degree/II})
               (jl/shape!= q {:pitch :C :name :m :context :chord-degree/i})))
      '({:pitch :C, :name :maj, :context :chord-degree/I})))

  (testing "neighboro"
    (are+ [run want] (= want run)
      ; Cmaj -> ? -> something which is a second mode of the thing before it
      (l/run 1 [q]
             (jl/with-fresh
               (l/== ?start :C_maj)
               (jl/neighboro ?start ?a)

               (jl/neighboro ?a ?b)
               (l/featurec ?b {:context :mode/II})

               (l/== q [?a ?b])))
      '([{:pitch :C,
          :name :major,
          :context :chord-degree/I,
          :parent-shape {:pitch :C, :name :major}}
         {:pitch :D,
          :name :dorian,
          :context :mode/II,
          :parent-shape {:pitch :C, :name :major}}])

      (l/run 1 [q]
             (jl/with-fresh
               ; Start at C major chord
               (l/== ?start {:pitch :C :name :maj})

               ; Find scale where it's a V chord
               (jl/neighboro ?start ?scale)
               (l/featurec ?scale {:context :chord-degree/V})

               ; Find the corresponding I chord (specifically major 7th) of the scale
               (jl/neighboro ?scale ?end)
               (l/featurec ?end {:context :chord-degree/Imaj7})

               (l/== q ?scale)))
      '({:pitch :F
         :name :major
         :context :chord-degree/V
         :parent-shape {:pitch :F :name :major}})

      ; Slower
      #_#_(l/run 1 [q]
                 (jl/with-fresh
                 ; Start at C major chord
                   (l/== ?start {:pitch :C :name :maj})

                   ; Find scale where it's a V chord
                   (jl/neighboro ?start ?scale1)
                   (l/featurec ?scale1 {:context :chord-degree/V})

                   ; Find second mode of that scale
                   (jl/neighboro ?scale1 ?scale2)
                   (l/featurec ?scale2 {:context :mode/II})

                   ; Find the corresponding I chord of the second mode
                   (jl/neighboro ?scale2 ?end)
                   (l/featurec ?end {:context :chord-degree/I})

                   (l/== q [?scale1 ?scale2 ?end])))
        '([{:pitch :F
            :name :lydian
            :context :chord-degree/V
            :parent-shape {:pitch :F, :name :lydian}}
           {:pitch :G
            :name :mixolydian
            :context :mode/II
            :parent-shape {:pitch :F, :name :lydian}}
           {:pitch :G
            :name :maj
            :context :chord-degree/I
            :parent-shape {:pitch :G, :name :mixolydian}}])

      ; Secondary dominant (V/V or V7/V)
      (l/run 1 [q]
             (jl/with-fresh
               ; Start at C major scale
               (l/== ?start-scale {:pitch :C :name :major})

               ; Find the tonic (I) chord
               (jl/neighboro ?start-scale ?i)
               (l/featurec ?i {:context :chord-degree/I})

               ; Find the dominant (V) chord
               (jl/neighboro ?start-scale ?v)
               (l/featurec ?v {:context :chord-degree/V})

               ; Find the scale where the dominant chord is the I (i.e. the now-tonicized chord)
               (jl/neighboro ?v ?dom-scale)
               (l/featurec ?dom-scale {:context :chord-degree/I})

               ; Find that scale's dominant
               (jl/neighboro ?dom-scale ?v-v)
               (l/featurec ?v-v {:context :chord-degree/V})

               ; Return the secondary dominant
               (l/== q [?v-v ?v ?i])))
      '([{:pitch :D,
          :name :maj,
          :context :chord-degree/V,
          :parent-shape {:pitch :G, :name :major}}
         {:pitch :G,
          :name :maj,
          :context :chord-degree/V,
          :parent-shape {:pitch :C, :name :major}}
         {:pitch :C,
          :name :maj,
          :context :chord-degree/I,
          :parent-shape {:pitch :C, :name :major}}]))

    ; Tritone substitution
    (l/run 1 [q]
           (jl/with-fresh
             (l/== ?scale {:pitch :C :name :major})

             (jl/neighboro ?scale ?ii)
             (l/featurec ?ii {:context :chord-degree/iim7})

             (jl/neighboro ?scale ?v)
             (l/featurec ?v {:context :chord-degree/V7})
             ; Find the shape which is a tritone away from the V chord
             (jl/transposo ?v :d5 ?sub)

             (jl/neighboro ?scale ?i)
             (l/featurec ?i {:context :chord-degree/Imaj7})

             (l/== q [?ii ?sub ?i])))
    '([{:pitch :D,
        :name :m7,
        :context :chord-degree/iim7,
        :parent-shape {:pitch :C, :name :major}}
       {:pitch :Db,
        :name :7,
        :context :interval/d5,
        :parent-shape {:pitch :G, :name :7}}
       {:pitch :C,
        :name :maj7,
        :context :chord-degree/Imaj7,
        :parent-shape {:pitch :C, :name :major}}])

    ; Coltrane changes
    (let [key1 (jigsaw/->shape :C_major)]
      (l/run 1 [q]
             (jl/with-fresh
             ; Normal ii-V-I
               (jl/neighboro key1 ?ii)
               (l/featurec ?ii {:context :chord-degree/iim7})

               (jl/neighboro key1 ?v)
               (l/featurec ?v {:context :chord-degree/V7})

               (jl/neighboro key1 ?i)
               (l/featurec ?i {:context :chord-degree/Imaj7})

              ; Key goes down a third
               (jl/transposo ?key2 key1 :M3 -1)

              ; New V-I
               (jl/neighboro ?key2 ?v2)
               (l/featurec ?v2 {:context :chord-degree/V7})

               (jl/neighboro ?key2 ?i2)
               (l/featurec ?i2 {:context :chord-degree/Imaj7})

              ; Key goes down another third
               (jl/transposo ?key2 :M3 ?key3 -1)

              ; New V-I
               (jl/neighboro ?key3 ?v3)
               (l/featurec ?v3 {:context :chord-degree/V7})

               (jl/neighboro ?key3 ?i3)
               (l/featurec ?i3 {:context :chord-degree/Imaj7})

               (l/== q [?ii ?v2 ?i2 ?v3 ?i3 ?v ?i]))))
    '([{:pitch :D, :name :m7, :context :chord-degree/iim7}
       {:pitch :Eb, :name :7, :context :chord-degree/V7}
       {:pitch :Ab, :name :maj7, :context :chord-degree/Imaj7}
       {:pitch :B, :name :7, :context :chord-degree/V7}
       {:pitch :E, :name :maj7, :context :chord-degree/Imaj7}
       {:pitch :G, :name :7, :context :chord-degree/V7}
       {:pitch :C, :name :maj7, :context :chord-degree/Imaj7}]))

  (testing "prepare-shapes"
    (testing "with existing shapes"
      (are+ [run want] (= want run)
        (l/run 1 [q]
               (jl/with-fresh
                 (jl/prepare-shapes [:C_maj] q)))
        '(({:intervals [:P1 :M3 :P5]
            :pitch :C
            :name :maj
            :pitches [:C :E :G]}))))

    (testing "with note seqs"
      (are+ [run want] (= want run)
        (l/run 1 [q]
               (jl/with-fresh
                 (jl/prepare-shapes [[:C4 :E4 :G4]] q)))
        '(({:pitch :C
            :name :maj
            :heuristics
            {:contains? 1
             :fully-contains? 0
             :contained-in? 1
             :fully-contained-in? 0
             :overlap 1.0
             :same-pitch-count? 1
             :shares-root? 1}
            :pcis [0 4 7]
            :input [:C4 :E4 :G4]})))))

  (testing "connecto"
    (testing "with existing shapes"
      (are+ [run want] (= want run)
        (let [ii (jigsaw/->shape :D_m)
              v (jigsaw/->shape :G_maj)
              chords [ii v]]
          (l/run 1 [q]
                 (jl/with-fresh
                   (jl/connecto chords ?conn ?shapes)
                   (l/== q {:connection ?conn
                            :contextualized-shapes ?shapes}))))
        '({:connection {:pitch :C, :name :major},
           :contextualized-shapes
           ({:intervals [:P1 :m3 :P5]
             :pitch :D
             :name :m
             :pitches [:D :F :A]
             :context :chord-degree/ii
             :parent-shape {:pitch :C :name :major}}
            {:intervals [:P1 :M3 :P5]
             :pitch :G
             :name :maj
             :pitches [:G :B :D]
             :context :chord-degree/V
             :parent-shape {:pitch :C :name :major}})})))

    (testing "with note seqs"
      (are+ [run want] (= want run)
        (let [note-seqs [[:C4 :E4 :G4]
                         [:D4 :F4 :A4]]]
          (l/run 1 [q]
                 (jl/with-fresh
                   (jl/prepare-shapes note-seqs ?shapes)
                   (jl/connecto ?shapes ?conn ?new-shapes)
                   (l/== q {:connection ?conn
                            :contextualized-shapes ?new-shapes}))))
        '({:connection {:pitch :C, :name :major}
           :contextualized-shapes
           ({:pitch :C
             :name :maj
             :heuristics
             {:contains? 1,
              :fully-contains? 0,
              :contained-in? 1,
              :fully-contained-in? 0,
              :overlap 1.0,
              :same-pitch-count? 1,
              :shares-root? 1},
             :pcis [0 4 7],
             :input [:C4 :E4 :G4],
             :context :chord-degree/I
             :parent-shape {:pitch :C, :name :major}}
            {:pitch :D,
             :name :m,
             :heuristics
             {:contains? 1,
              :fully-contains? 0,
              :contained-in? 1,
              :fully-contained-in? 0,
              :overlap 1.0,
              :same-pitch-count? 1,
              :shares-root? 1},
             :pcis [2 5 9],
             :input [:D4 :F4 :A4],
             :context :chord-degree/ii
             :parent-shape {:pitch :C, :name :major}})}))))

  (testing "fuzzy-neighborc"
    (are+ [run want] (= want run)
      ; Fit (find closest compatible shape, to a reference shape)
      ; i.e. <input> ~= ? <-> <shape>
      (let [start (jigsaw/->shape :C_m)
            parent (jigsaw/->shape :C_major)]
        (l/run 3 [q]
               (jl/with-fresh
              ; Find the shape(s) in the C major scale...
                 (jl/neighboro parent ?candidate)
              ; ...which are close (pitch-wise) to the C minor chord
                 (jl/fuzzy-neighborc start ?candidate)
                 (l/== q ?candidate))))
      '({:pitch :C, :name :maj, :context :chord-degree/I :parent-shape {:pitch :C :name :major}}
        {:pitch :C, :name :sus4, :context :chord-degree/isus4 :parent-shape {:pitch :C :name :major}}
        {:pitch :C, :name :sus2, :context :chord-degree/isus2 :parent-shape {:pitch :C :name :major}})))

  (testing "progresso"
    (are+ [run want] (= want run)
      ; Autocomplete: based on inputs, see if you're playing a known progression
      ; With shapes
      (let [; Start of a ii-V-I
            ii (jigsaw/->shape :D_m)
            v (jigsaw/->shape :G_maj)
            chords [ii v]]
        (l/run 1 [q]
               (jl/with-fresh
                 (jl/prepare-shapes chords ?shapes)
                 (jl/connecto ?shapes ?conn ?new-shapes)
                 (jl/progresso ?new-shapes ?prog)
                 (l/== q [{:connection ?conn
                           :contextualized-shapes ?new-shapes}
                          ?prog]))))
      '([{:connection {:pitch :C, :name :major},
          :contextualized-shapes
          ({:intervals [:P1 :m3 :P5],
            :pitch :D,
            :name :m,
            :pitches [:D :F :A],
            :context :chord-degree/ii
            :parent-shape {:pitch :C, :name :major}}
           {:intervals [:P1 :M3 :P5],
            :pitch :G,
            :name :maj,
            :pitches [:G :B :D],
            :context :chord-degree/V
            :parent-shape {:pitch :C, :name :major}})}
         ["Montgomery–Ward bridge"
          {:degrees
           [:chord-degree/I
            :chord-degree/IV
            :chord-degree/ii
            :chord-degree/V],
           :quality :major}]])))

  (testing "alto"
    (are+ [run want] (= want run)
      ; What else could the C maj chord be (excluding C maj and its enharmonic equivalents)?
      (l/run 2 [q]
             (jl/with-fresh
               (jl/alto :C_maj q)))
      '({:pitch :Fb,
         :name :m#5,
         :bass :C,
         :heuristics
         {:contains? 1,
          :fully-contains? 0,
          :contained-in? 1,
          :fully-contained-in? 0,
          :overlap 1.0,
          :same-pitch-count? 1,
          :shares-root? 0},
         :pcis [4 7 0],
         :input [:C4 :E4 :G4]}
        {:pitch :E,
         :name :m#5,
         :bass :B#,
         :heuristics
         {:contains? 1,
          :fully-contains? 0,
          :contained-in? 1,
          :fully-contained-in? 0,
          :overlap 1.0,
          :same-pitch-count? 1,
          :shares-root? 0},
         :pcis [4 7 0],
         :input [:C4 :E4 :G4]})))

  (testing "?="
    (are+ [run want] (= want run)
      ; What else could the C maj chord be (including C maj, enharmonic equivalents)?
      (l/run 2 [q]
             (jl/with-fresh
               (jl/?= :C_maj q)))
      '({:pitch :C,
         :name :maj,
         :heuristics
         {:contains? 1,
          :fully-contains? 0,
          :contained-in? 1,
          :fully-contained-in? 0,
          :overlap 1.0,
          :same-pitch-count? 1,
          :shares-root? 1},
         :pcis [0 4 7],
         :input [:C4 :E4 :G4]}
        {:pitch :B#,
         :name :maj,
         :heuristics
         {:contains? 1,
          :fully-contains? 0,
          :contained-in? 1,
          :fully-contained-in? 0,
          :overlap 1.0,
          :same-pitch-count? 1,
          :shares-root? 1},
         :pcis [0 4 7],
         :input [:C4 :E4 :G4]})))

  (testing "patho"
    (are+ [run want] (= want run)
      (l/run 1 [q]
             (jl/with-fresh
               (jl/patho :C_maj :D_m 1 q)))

      '(({:intervals [:P1 :M3 :P5],
          :pitch :C,
          :name :maj,
          :pitches [:C :E :G]}
         {:pitch :C,
          :name :major,
          :context :chord-degree/I,
          :parent-shape {:pitch :C, :name :major}}
         {:pitch :D,
          :name :m,
          :context :chord-degree/ii,
          :parent-shape {:pitch :C, :name :major}}))

      ; Borrowed chord, from parallel C minor
      ; Slow
      #_#_(l/run 1 [q]
                 (jl/with-fresh
                   (jl/patho :C_maj7 :C_m 2 q)))
        '(({:intervals [:P1 :M3 :P5 :M7],
            :pitch :C,
            :name :maj7,
            :pitches [:C :E :G :B]}
           {:pitch :C,
            :name :major,
            :context :chord-degree/Imaj7,
            :parent-shape {:pitch :C, :name :major}}
           {:pitch :C,
            :name :minor,
            :context :mode/parallel,
            :parent-shape {:pitch :C, :name :major}}
           {:pitch :C,
            :name :m,
            :context :chord-degree/i,
            :parent-shape {:pitch :C, :name :minor}}))))

  (testing "resolvo"
    (are+ [run want] (= want run)
      ; Which scale has E maj chord for a fifth?
      (l/run 1 [q]
             (jl/with-fresh
               (jl/resolvo q :chord-degree/V :E_maj)))
      '({:pitch :A,
         :name :major,
         :context :chord-degree/V,
         :parent-shape {:pitch :A, :name :major}})

      ; Said another way, find the degree
      (l/run 1 [q]
             (jl/with-fresh
               (jl/resolvo :A_major q :E_maj)))
      '(:chord-degree/V)

      ; Said another way, find the chord
      (l/run 1 [q]
             (jl/with-fresh
               (jl/resolvo :A_major :chord-degree/V q)))
      '({:pitch :E,
         :name :maj,
         :context :chord-degree/V,
         :parent-shape {:pitch :A, :name :major}})))

  (testing "resolvo*"
    ; Allows for multiple degrees and chords
    (are+ [run want] (= want run)
      ; Which scale has E maj chord for a fifth?
      (l/run 1 [q]
             (jl/with-fresh
               (jl/resolvo* q [:chord-degree/V] [:E_maj])))
      '({:pitch :A,
         :name :major,
         :context :chord-degree/V,
         :parent-shape {:pitch :A, :name :major}})

      ; Said another way, find the degree(s)
      (l/run 1 [q]
             (jl/with-fresh
               (jl/resolvo* :A_major q [:E_maj :A_maj])))
      '([:chord-degree/V :chord-degree/I])

      ; Said another way, find the chord(s)
      (l/run 1 [q]
             (jl/with-fresh
               (jl/resolvo* :A_major [:chord-degree/V :chord-degree/I] q)))
      '([{:pitch :E,
          :name :maj,
          :context :chord-degree/V,
          :parent-shape {:pitch :A, :name :major}}
         {:pitch :A,
          :name :maj,
          :context :chord-degree/I,
          :parent-shape {:pitch :A, :name :major}}])

      ; Destructuring
      (l/run 1 [q]
             (jl/with-fresh
               (jl/resolvo* ?scale [:chord-degree/V :chord-degree/I] [:C_maj q])))
      '({:pitch :F,
         :name :maj,
         :context :chord-degree/I,
         :parent-shape {:pitch :F, :name :major}}))))
