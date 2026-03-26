(ns jigsaw.datomic-test
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}
                                 :invalid-arity {:level :off}}}}
  (:require
   [clojure.test :refer [deftest testing is]]
   [datascript.core :as d]
   [jigsaw.core :as jigsaw]
   [jigsaw.experiments.datomic :as jd]
   [jigsaw.impl.theory :as theory]
   [jigsaw.test-utils :refer [are+]]))

(deftest datomic-test
  (testing "neighbors and edges"
    (is (= '([{:pitch+name [:G :13no5]} :chord-degree/V13no5]
             [{:pitch+name [:D :dorian]} :mode/II]
             [{:pitch+name [:G :13]} :chord-degree/V13]
             [{:pitch+name [:A :madd9]} :chord-degree/vimadd9]
             [{:pitch+name [:F :69#11]} :chord-degree/IV69#11])

           (take 5 (d/q '[:find (pull ?b [:pitch+name]) ?context
                          :in $ %
                          :where
                          [?a :pitch :C]
                          [?a :name :major]
                          (neighbor ?a ?b ?context)]
                        jd/db jd/rules)))) ; #'jigsaw.datomic-test/datomic-test

    ; Cmaj -> ? -> something which is a second mode of the thing before it
    (is (= '([:scale-degree/I
              {:pitch+name [:C :phrygian-dominant]}
              {:pitch+name [:Db :lydian-#9]}]
             [:scale-degree/III
              {:pitch+name [:Ab :lydian-#5P-pentatonic]}
              {:pitch+name [:C :flat-six-pentatonic]}]
             [:scale-degree/I
              {:pitch+name [:C :lydian-#9]}
              {:pitch+name [:D# :ultralocrian]}]
             [:scale-degree/IV
              {:pitch+name [:G :major-augmented]}
              {:pitch+name [:A :dorian-#4]}]
             [:scale-degree/bVII
              {:pitch+name [:D :mixolydian-b6]}
              {:pitch+name [:E :locrian-#2]}])

           (take 5 (d/q '[:find ?context
                          (pull ?b [:pitch+name])
                          (pull ?c [:pitch+name])
                          :in $ %
                          :where
                          [?a :pitch :C]
                          [?a :name :maj]
                          (neighbor ?a ?b ?context)
                          (neighbor ?b ?c :mode/II)]
                        jd/db jd/rules))))

    ; Cmaj -> scale where Cmaj is a V chord -> scale's I chord
    (is (= '([{:pitch+name [:F :bebop-major]}]
             [{:pitch+name [:F :harmonic-major]}]
             [{:pitch+name [:F :bebop]}]
             [{:pitch+name [:F :major]}]
             [{:pitch+name [:F :lydian]}])

           (d/q '[:find (pull ?b [:pitch+name])
                  :in $ %
                  :where
                  ; Start at C major chord
                  [?a :pitch :C]
                  [?a :name :maj]
                  ; Find scale where it's a V chord
                  (neighbor ?a ?b :scale-degree/V)
                  ; Find the corresponding I chord (specifically major 7th) of the scale
                  (neighbor ?b ?c :chord-degree/Imaj7)]
                jd/db jd/rules)))

    (testing "secondary dominant"
      (is (= '([{:pitch+name [:D :maj]}])

             (d/q '[:find (pull ?v-v [:pitch+name])
                    :in $ %
                    :where
                   ; Start at C major chord
                    [?start-scale :pitch :C]
                    [?start-scale :name :major]
         ; Find tonic
                    (neighbor ?start-scale ?i :chord-degree/I)
         ; Find V
                    (neighbor ?start-scale ?v :chord-degree/V)
         ; Find scale where V is a I
                    (neighbor ?v ?dom-scale :scale-degree/I)
         ; Find V of that scale
                    (neighbor ?dom-scale ?v-v :chord-degree/V)]
                  jd/db jd/rules))))

    (testing "tritone substitution"
      (is (= '([{:pitch+name [:D :m7]}
                {:pitch+name [:Db :7]}
                {:pitch+name [:C :maj7]}])

             (d/q '[:find
                    (pull ?ii [:pitch+name])
                    (pull ?sub [:pitch+name])
                    (pull ?i [:pitch+name])
                    :in $ %
                    :where
                    [?scale :pitch :C]
                    [?scale :name :major]
                    (neighbor ?scale ?ii :chord-degree/iim7)
                    (neighbor ?scale ?v :chord-degree/V7)
                    (neighbor ?scale ?i :chord-degree/Imaj7)
                    (transpose ?v :d5 1 ?sub)]
                  jd/db jd/rules))))

    (testing "coltrane changes"

      (is (= '([{:pitch+name [:D :m7]}
                {:pitch+name [:Eb :7]}
                {:pitch+name [:Ab :maj7]}
                {:pitch+name [:Cb :7]}
                {:pitch+name [:Fb :maj7]}
                {:pitch+name [:G :7]}
                {:pitch+name [:C :maj7]}])

             (d/q '[:find
                    (pull ?ii [:pitch+name])
                    (pull ?v2 [:pitch+name])
                    (pull ?i2 [:pitch+name])
                    (pull ?v3 [:pitch+name])
                    (pull ?i3 [:pitch+name])
                    (pull ?v [:pitch+name])
                    (pull ?i [:pitch+name])
                    :in $ %
                    :where
                    [?key1 :pitch :C]
                    [?key1 :name :major]
         ; Normal ii-V-I
                    (neighbor ?key1 ?ii :chord-degree/iim7)
                    (neighbor ?key1 ?v :chord-degree/V7)
                    (neighbor ?key1 ?i :chord-degree/Imaj7)

         ; Key goes down a third
                    (transpose ?key1 :M3 -1 ?key2)

         ; New V-I
                    (neighbor ?key2 ?v2 :chord-degree/V7)
                    (neighbor ?key2 ?i2 :chord-degree/Imaj7)

         ; Key goes down another third
                    (transpose ?key2 :M3 -1 ?key3)

         ; New V-I
                    (neighbor ?key3 ?v3 :chord-degree/V7)
                    (neighbor ?key3 ?i3 :chord-degree/Imaj7)]
                  jd/db jd/rules)))))

  (testing "connect"
    (is (= '([{:pitch+name [:G :bebop-minor]}]
             [{:pitch+name [:A :minor]}]
             [{:pitch+name [:G :bebop]}]
             [{:pitch+name [:C :bebop]}]
             [{:pitch+name [:B :bebop-locrian]}]
             [{:pitch+name [:E :bebop-locrian]}]
             [{:pitch+name [:C :major]}]
             [{:pitch+name [:E :spanish-heptatonic]}]
             [{:pitch+name [:G :mixolydian]}]
             [{:pitch+name [:D :dorian]}]
             [{:pitch+name [:D :composite-blues]}]
             [{:pitch+name [:D :bebop-minor]}]
             [{:pitch+name [:B :locrian]}]
             [{:pitch+name [:E :phrygian]}]
             [{:pitch+name [:C :bebop-major]}]
             [{:pitch+name [:A :bebop-harmonic-minor]}]
             [{:pitch+name [:G :composite-blues]}]
             [{:pitch+name [:F :lydian]}])
           (d/q '[:find
                  (pull ?neighbor [:pitch+name])
                  :in $ %
                  :where
                  [?c :pitch+name [:C :maj]]
                  [?d :pitch+name [:D :m]]
                  [?e :pitch+name [:E :m]]
                  [?f :pitch+name [:F :maj]]
                  [(vector ?c ?d ?e ?f) ?coll]
                  (connect ?coll ?neighbor)]
                jd/db jd/rules))))

  (testing "alts"
    (testing "alt"
      ; Overlapping PCIs, can be same shape
      (is (= '([{:pitch+name [:Fb :m#5]} 1.0]
               [{:pitch+name [:E :m#5]} 1.0]
               [{:pitch+name [:B# :maj]} 1.0]
               [{:pitch+name [:C :maj]} 1.0])

             (d/q '[:find (pull ?b [:pitch+name]) ?index
                    :in $ %
                    :where
                    [?a :pitch :C]
                    [?a :name :maj]
                    (alt ?a ?b ?index)]
                  jd/db jd/rules))))

    (testing "alt!="

; Overlapping PCIs, but not same shape or enharmonic equivalent
      (is (= '([{:pitch+name [:Fb :m#5]} 1.0]
               [{:pitch+name [:E :m#5]} 1.0])

             (d/q '[:find (pull ?b [:pitch+name]) ?index
                    :in $ %
                    :where
                    [?a :pitch :C]
                    [?a :name :maj]
                    (alt!= ?a ?b ?index)]
                  jd/db jd/rules)))))

  (testing "resolving"
    (testing "from chord and degree"
      (is (= '([{:pitch+name [:A :harmonic-minor]}]
               [{:pitch+name [:A :minor-hexatonic]}]
               [{:pitch+name [:A :melodic-minor]}]
               [{:pitch+name [:A :lydian]}]
               [{:pitch+name [:A :bebop-major]}]
               [{:pitch+name [:A :bebop]}]
               [{:pitch+name [:A :major]}]
               [{:pitch+name [:A :bebop-harmonic-minor]}]
               [{:pitch+name [:A :minor-six-diminished]}]
               [{:pitch+name [:A :hungarian-minor]}]
               [{:pitch+name [:A :harmonic-major]}]
               [{:pitch+name [:A :lydian-diminished]}])

             (d/q '[:find (pull ?scale [:pitch+name])
                    :in $ %
                    :where
                    [?chord :pitch :E]
                    [?chord :name :maj]
                    (neighbor ?chord ?scale :scale-degree/V)]
                  jd/db jd/rules))))

    (testing "from chord and scale"

      (is (= #{[:scale-degree/V]}

             (d/q '[:find ?deg
                    :in $ %
                    :where
                    [?chord :pitch :E]
                    [?chord :name :maj]
                    [?scale :pitch :A]
                    [?scale :name :major]
                    (neighbor ?chord ?scale ?deg)]
                  jd/db jd/rules))))

    (testing "from scale and degree"

      (is (= '([{:pitch+name [:E :maj]}]
               [{:pitch+name [:E :6]}]
               [{:pitch+name [:E :sus24]}]
               [{:pitch+name [:E :sus4]}]
               [{:pitch+name [:E :11]}]
               [{:pitch+name [:E :Madd9]}]
               [{:pitch+name [:E :9]}]
               [{:pitch+name [:E :7sus4]}]
               [{:pitch+name [:E :9no5]}]
               [{:pitch+name [:E :13sus4]}]
               [{:pitch+name [:E :6add9]}]
               [{:pitch+name [:E :7]}]
               [{:pitch+name [:E :7no5]}]
               [{:pitch+name [:E :5]}]
               [{:pitch+name [:E :13no5]}]
               [{:pitch+name [:E :sus2]}]
               [{:pitch+name [:E :9sus4]}]
               [{:pitch+name [:E :7add6]}]
               [{:pitch+name [:E :13]}])

             (d/q '[:find (pull ?chord [:pitch+name])
                    :in $ %
                    :where
                    [?scale :pitch :A]
                    [?scale :name :major]
                    (neighbor ?chord ?scale :scale-degree/V)]
                  jd/db jd/rules)))))

  (testing "cluster"
    ; Stringify floats for better comparison
    (is (= (map #(assoc % :avg-partition-jaccard-index (str (:avg-partition-jaccard-index %)))
                '({:partitions ([1 433 857]),
                   :avg-partition-jaccard-index 0.07258064,
                   :matched-shapes
                   {#{:C :E :G} {:name :maj, :pitch :C, :db/id 1},
                    #{:A :F :D} {:name :m, :pitch :D, :db/id 433},
                    #{:B :E :G} {:name :m, :pitch :E, :db/id 857}}}
                  {:partitions ([1 857] [433]),
                   :avg-partition-jaccard-index 0.6551724,
                   :matched-shapes
                   {#{:C :E :G} {:name :maj, :pitch :C, :db/id 1},
                    #{:A :F :D} {:name :m, :pitch :D, :db/id 433},
                    #{:B :E :G} {:name :m, :pitch :E, :db/id 857}}}
                  {:partitions ([1 433] [751]),
                   :avg-partition-jaccard-index 0.5816327,
                   :matched-shapes
                   {#{:C :E :G} {:name :maj, :pitch :C, :db/id 1},
                    #{:A :F :D} {:name :m, :pitch :D, :db/id 433},
                    #{:B :E :G} {:name :m, :pitch :Fb, :db/id 751}}}
                  {:partitions ([1 433] [857]),
                   :avg-partition-jaccard-index 0.5816327,
                   :matched-shapes
                   {#{:C :E :G} {:name :maj, :pitch :C, :db/id 1},
                    #{:A :F :D} {:name :m, :pitch :D, :db/id 433},
                    #{:B :E :G} {:name :m, :pitch :E, :db/id 857}}}
                  {:partitions ([107] [433 857]),
                   :avg-partition-jaccard-index 0.56435645,
                   :matched-shapes
                   {#{:C :E :G} {:name :maj, :pitch :B#, :db/id 107},
                    #{:A :F :D} {:name :m, :pitch :D, :db/id 433},
                    #{:B :E :G} {:name :m, :pitch :E, :db/id 857}}}))

           (map #(assoc % :avg-partition-jaccard-index (str (:avg-partition-jaccard-index %)))
                (jd/cluster [#{:C :E :G}
                             #{:D :F :A}
                             #{:E :G :B}]))))))
