(ns jigsaw.experiments.datomic
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]
   [datascript.core :as d]
   [datascript.storage.sql.core :as storage-sql]
   [jigsaw.core :as jigsaw]
   [jigsaw.impl.theory :as theory]
   [jigsaw.utils :as utils]
   [next.jdbc :as jdbc]))

(def schema
  {; Shapes
   :pitch {:db/doc "Shape's starting pitch"
           ; :db/valueType :db.type/keyword
           :db/cardinality :db.cardinality/one}
   :name {:db/doc "Shape name"
          ; :db/valueType :db.type/keyword
          :db/cardinality :db.cardinality/one}
   :type {:db/doc "Shape type (:chord or :scale)"
          ; Datascript doesn't have types besides ref and tuple
          ; :db/valueType :db.type/keyword
          :db/cardinality :db.cardinality/one}
   ; :type+pitch+name {:db/doc "Composite key"
   ;                   :db/valueType :db.type/tuple
   ;                   :db/tupleAttrs [:type :pitch :name]
   ;                   :db/cardinality :db.cardinality/one
   ;                   ; Use :db.unique/identity instead of db.unique/value to allow upserts
   ;                   :db/unique :db.unique/identity}
   :pitch+name {:db/doc "Composite key"
                :db/valueType :db.type/tuple
                :db/tupleAttrs [:pitch :name]
                :db/cardinality :db.cardinality/one
                ; Use :db.unique/identity instead of db.unique/value to allow upserts
                :db/unique :db.unique/identity}
   :pitches {:db/doc "Pitches"
             ; :db/valueType :db.type/keyword
             :db/cardinality :db.cardinality/one}
   :pitch-set {:db/doc "Pitches (set)"
               ; :db/valueType :db.type/keyword
               :db/cardinality :db.cardinality/one}
   :pci-set {:db/doc "Pitch class indices (set)"
             ; :db/valueType :db.type/long
             :db/cardinality :db.cardinality/one}
   :intervals {:db/doc "Intervals (set)"
               ; :db/valueType :db.type/keyword
               :db/cardinality :db.cardinality/one}
   :degrees {:db/doc "Scale degrees (set)"
             ; :db/valueType :db.type/keyword
             :db/cardinality :db.cardinality/one}
   ; Edges
   :edge/from {:db/doc "Starting shape ref"
               :db/valueType :db.type/ref}
   :edge/to   {:db/doc "Ending shape ref"
               :db/valueType :db.type/ref}
   :edge/context {:db/doc "The context of :edge/to in relation to the :edge/from (e.g. C_major scale -> C_maj chord is :chord-degree/I; C_major scale -> D_dorian scale is mode/II)"
                  ; :db/valueType :db.type/keyword
                  :db/cardinality :db.cardinality/one}
   ; :edge/from is a subset of :edge/to ?
   ; :edge/pci-subset? {:db/doc "From's PCIs are a subset of to's?"
                      ; :db/valueType :db.type/boolean
                      ; :db/cardinality :db.cardinality/one}
   ; :edge/pitch-subset? {:db/doc "From's pitches are a subset of to's?"
                        ; :db/valueType :db.type/boolean
                        ; :db/cardinality :db.cardinality/one}
   ; :edge/interval-subset? {:db/doc "From's intervals are a subset of to's?"
                           ; :db/valueType :db.type/boolean
                           ; :db/cardinality :db.cardinality/one}
   ; :edge/from is a superset of :edge/to ?
   ; :edge/pci-superset? {:db/doc "From's PCIs are a superset of to's?"
                        ; :db/valueType :db.type/boolean
                        ; :db/cardinality :db.cardinality/one}
   ; :edge/pitch-superset? {:db/doc "From's pitches are a superset of to's?"
                          ; :db/valueType :db.type/boolean
                          ; :db/cardinality :db.cardinality/one}
   ; :edge/interval-superset? {:db/doc "From's intervals are a superset of to's?"
                             ; :db/valueType :db.type/boolean
                             ; :db/cardinality :db.cardinality/one}
   ; :edge/from has a % overlap with :edge/to (same for both directions)
   ; :edge/pci-jaccard {:db/doc "Jaccard index of from/to's PCIs"
                      ; :db/valueType :db.type/bigint
                      ; :db/cardinality :db.cardinality/one}
   ; :edge/pitch-jaccard {:db/doc "Jaccard index of from/to's pitches"
                        ; :db/valueType :db.type/bigint
                        ; :db/cardinality :db.cardinality/one}
   ; :edge/interval-jaccard {:db/doc "Jaccard index of from/to's intervals"
                           ; :db/valueType :db.type/bigint
                           ; :db/cardinality :db.cardinality/one}
   })

; (comment
;   ; Model small structures in addition to super structures?
;   ; i.e. using many cardinality?
;   :pci/9
;   :pitch/A
;   :interval/P5
;   :scale-degree/bII
;   :chord-degree/bIImaj7)

(defn resolve-all-shapes [shape-type]
  (for [pitch theory/simple-pitch-keys
        shape-name (keys (if (= shape-type :chord) theory/chords theory/scales))]
    (let [shape (jigsaw/->shape {:pitch pitch :name shape-name})]
      (assoc
       (dissoc shape :aliases)
       ; :pitch-set (set (:pitches shape))
       :pci-set (set (map theory/pitches (:pitches shape)))
       ; :interval-set (set (:intervals shape))
       ; :degree-set (set (:degrees shape))
       :type shape-type))))

(def db-ref {:dbname "db.sqlite" :dbtype "sqlite"})
(def datasource (jdbc/get-datasource db-ref))
(def storage (storage-sql/make datasource
                               {:dbtype :sqlite}))
; (def storage2 (d/file-storage "./db"))
(def db (d/restore storage))

(defn shapes->edge-txns-diatonic [shapes]
  (for [src-shape shapes
        dest-shape (jigsaw/shape->shapes src-shape)
        :when (and (utils/in? theory/simple-pitch-keys (:pitch dest-shape))
                   (some? (:context dest-shape)))
        :let [edge {:edge/from [:pitch+name ((juxt :pitch :name) src-shape)]
                    :edge/to [:pitch+name ((juxt :pitch :name) dest-shape)]
                    :edge/context (:context dest-shape)}]]
    edge))

(defn store-datoms-diatonic []
  (let [conn (if-some [_ (d/restore-conn storage)]
               _
               (d/create-conn schema storage))
        shapes (vec (concat (resolve-all-shapes :chord)
                            (resolve-all-shapes :scale)))
        ; node-txns (map #(dissoc % :intervals :degrees) shapes)
        node-txns shapes
        edge-txns (shapes->edge-txns-diatonic shapes)]
    #_(count (concat node-txns edge-txns))
    (doseq [txn-chunk (partition-all 5000 (concat node-txns edge-txns))]
      (d/transact! conn txn-chunk)
      (d/store @conn storage))))

(comment
  (store-datoms-diatonic))

(defn notes->pci-set [notes]
  (set (map #(-> % theory/parts :pci) notes)))

(defn connect-rules [n]
  (let [entity-sym (fn [i] (symbol (str "?e" (inc i))))
        context-sym (fn [i] (symbol (str "?context" (inc i))))]
    (vec (for [rule-arity (range 2 (inc n))
               :let [entity-syms (map entity-sym (range rule-arity))]]
           `[(~(symbol (str "connect-" rule-arity)) ~'?connection ~@entity-syms)
             ~@(for [i (range rule-arity)
                     :let [e (nth entity-syms i)
                           c (context-sym i)]]
                 `(~'neighbor ~e ~'?connection ~c))]))))

(def base-rules
  '[[(notes->shapes ?notes ?e ?index)
     [(jigsaw.experiments.datomic/notes->pci-set ?notes) ?pcis]
     [?e :pci-set ?pcis']
     [(theory/jaccard-index ?pcis ?pcis') ?index]
     [(>= ?index 0.5)]]

    [(neighbor ?from ?to ?context)
     [?edge :edge/from ?from]
     [?edge :edge/to ?to]
     [?edge :edge/context ?context]]

    ; [(neighbor ?to ?from ?context)
    ;  [?edge :edge/from ?from]
    ;  [?edge :edge/to ?to]
    ;  [?edge :edge/context ?context]]

    [(transpose ?e ?interval ?multiplier ?e')
     [?e :pitch ?pitch]
     [?e :name ?name]
     [(theory/transpose ?pitch ?interval ?multiplier) ?p']
     [?e' :pitch ?p']
     [?e' :name ?name]]

    ; Overlapping PCIs, not same shape
    [(alt ?e ?e' ?index)
     [?e :pci-set ?pcis]
     [?e' :pci-set ?pcis']
     [(not= ?e ?e')]
     [(theory/jaccard-index ?pcis ?pcis') ?index]
     [(>= ?index 0.9)]]

    ; Overlapping PCIs, same shape allowed
    [(alt= ?e ?e' ?index)
     [?e :pci-set ?pcis]
     [?e' :pci-set ?pcis']
     [(theory/jaccard-index ?pcis ?pcis') ?index]
     [(>= ?index 0.9)]]

    ; Overlapping PCIs, not same shape nor starting PCI (i.e. enharmonic equivalent)
    [(alt!= ?e ?e' ?index)
     [?e :pci-set ?pcis]
     [?e' :pci-set ?pcis']
     [(theory/jaccard-index ?pcis ?pcis') ?index]
     [(>= ?index 0.9)]
     [?e :pitch ?pitch]
     [?e' :pitch ?pitch']
     [(theory/pitches ?pitch) ?fp]
     [(theory/pitches ?pitch') ?fp']
     [(not= ?fp ?fp')]]

    [(fit ?e ?target ?e' ?index)
     (neighbor ?target ?e')
     [?e :pci-set ?pcis]
     [?e' :pci-set ?pcis']
     [(count ?pcis) ?pcs]
     [(count ?pcis') ?pcs']
     [(<= ?pcs ?pcs')]
     [(theory/jaccard-index ?pcis ?pcis') ?index]
     [(>= ?index 0.5)]]

    [(fit-pci ?pcis ?target ?e' ?index)
     (neighbor ?target ?e')
     [?e' :pci-set ?pcis']
     [(count ?pcis) ?pcs]
     [(count ?pcis') ?pcs']
     [(<= ?pcs ?pcs')]
     [(theory/jaccard-index ?pcis ?pcis') ?index]
     [(>= ?index 0.5)]]

    [(partition ?coll ?partitioning)
     ; Max of n-1 partitions, i.e. don't allow all elements to have their own partition
     [(count ?coll) ?n]
     [(dec ?n) ?max]
     [(combo/partitions ?coll :max ?max) [?partitioning ...]]]

    ; Shared nearest neighbors
    [(snn ?coll ?shared-neighbors)
     [(jigsaw.experiments.datomic/snn ?coll) ?shared-neighbors]]

    ; Shared nearest neighbors as jaccard index
    [(snn-jaccard ?coll ?index)
     [(jigsaw.experiments.datomic/snn-jaccard ?coll) ?index]]

    ; Shared nearest neighbors, for each partition
    [(partition-snn ?partitioning ?shared-neighbors-per-partition)
     [(jigsaw.experiments.datomic/partition-snn ?partitioning) ?shared-neighbors-per-partition]]

    ; Average jaccard index for a list of partitions (seq of seqs)
    [(partition-snn-jaccard ?partitioning ?avg-index)
     [(jigsaw.experiments.datomic/partition-snn-jaccard ?partitioning) ?avg-index]]])

(def rules
  (concat base-rules (connect-rules 10)))

(defn shape->neighbors [shape-eid]
  (set (d/q '[:find [?neighbor ...]
              :in $ % ?e
              :where
              (neighbor ?e ?neighbor)]
            db rules shape-eid)))

(defn snn [coll]
  (apply set/intersection (map shape->neighbors coll)))

(defn snn-jaccard [coll]
  (apply theory/jaccard-index (map shape->neighbors coll)))

(defn partition-snn [partitioning]
  (map snn partitioning))

(defn partition-snn-jaccard [partitioning]
  (let [indexes (map snn-jaccard partitioning)
        sum (reduce + indexes)]
    (float (/ sum (count partitioning)))))

(comment
  (d/q '[:find (pull ?b [:pitch+name]) ?context
         :in $ %
         :where
         [?a :pitch :C]
         [?a :name :major]
         (neighbor ?a ?b ?context)]
       db rules))

(comment
  ; Cmaj -> ? -> something which is a second mode of the thing before it
  (d/q '[:find ?context
         (pull ?b [:pitch+name])
         (pull ?c [:pitch+name])
         :in $ %
         :where
         [?a :pitch :C]
         [?a :name :maj]
         (neighbor ?a ?b ?context)
         (neighbor ?b ?c :mode/II)]
       db rules))

(comment
  ; Cmaj -> ? -> something which is a second mode of the thing before it
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

       db rules))
; ([{:pitch+name [:F :bebop-major]}]
;  [{:pitch+name [:F :harmonic-major]}]
;  [{:pitch+name [:F :bebop]}]
;  [{:pitch+name [:F :major]}]
;  [{:pitch+name [:F :lydian]}])

(comment
  ; Secondary dominant
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
       db rules))
; ([{:pitch+name [:D :maj]}])

(comment
  ; Tritone substitution
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
       db rules))
; ([{:pitch+name [:D :m7]}
;   {:pitch+name [:Db :7]}
;   {:pitch+name [:C :maj7]}])

(comment
  ; Coltrane changes
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
       db rules))
; ([{:pitch+name [:D :m7]}
;   {:pitch+name [:Eb :7]}
;   {:pitch+name [:Ab :maj7]}
;   {:pitch+name [:Cb :7]}
;   {:pitch+name [:Fb :maj7]}
;   {:pitch+name [:G :7]}
;   {:pitch+name [:C :maj7]}])

(comment
  ; Connect
  (let [shapes [[:C :maj] [:D :m] [:E :m] [:F :maj]]
        nbr (gensym "?")
        syms (reduce #(assoc %1 %2 (gensym "?")) {} shapes)
        query {:find `[(~'pull ~nbr [:pitch+name])]
               :in '[$ %]
               :where (vec (mapcat identity (for [[shape sym] syms]
                                              `[[~sym :pitch+name ~shape]
                                                (~'neighbor ~sym ~nbr)])))}]
    #_query
    (d/q query db rules)))
; {:find [(pull ?17446 [*])],
;  :in [$ %],
;  :where
;  [[?17447 :pitch+name [:C :maj]]
;   (neighbor ?17447 ?17446)
;   [?17448 :pitch+name [:D :m]]
;   (neighbor ?17448 ?17446)
;   [?17449 :pitch+name [:E :m]]
;   (neighbor ?17449 ?17446)
;   [?17450 :pitch+name [:F :maj]]
;   (neighbor ?17450 ?17446)]}
;
; ([{:pitch+name [:G :bebop-minor]}]
;  [{:pitch+name [:A :minor]}]
;  [{:pitch+name [:G :bebop]}]
;  [{:pitch+name [:C :bebop]}]
;  [{:pitch+name [:B :bebop-locrian]}]
;  [{:pitch+name [:E :bebop-locrian]}]
;  [{:pitch+name [:C :major]}]
;  [{:pitch+name [:E :spanish-heptatonic]}]
;  [{:pitch+name [:G :mixolydian]}]
;  [{:pitch+name [:D :dorian]}]
;  [{:pitch+name [:D :composite-blues]}]
;  [{:pitch+name [:D :bebop-minor]}]
;  [{:pitch+name [:B :locrian]}]
;  [{:pitch+name [:E :phrygian]}]
;  [{:pitch+name [:C :bebop-major]}]
;  [{:pitch+name [:A :bebop-harmonic-minor]}]
;  [{:pitch+name [:G :composite-blues]}]
;  [{:pitch+name [:F :lydian]}])

(comment
  ; Connect using generated rules
  (d/q '[:find
         (pull ?neighbor [:pitch+name])
         :in $ %
         :where
         [?c :pitch+name [:C :maj]]
         [?d :pitch+name [:D :m]]
         [?e :pitch+name [:E :m]]
         [?f :pitch+name [:F :maj]]
         (connect-4 ?neighbor ?c ?d ?e ?f)]
       db rules))
; ([{:pitch+name [:G :bebop-minor]}]
;  [{:pitch+name [:A :minor]}]
;  [{:pitch+name [:G :bebop]}]
;  [{:pitch+name [:C :bebop]}]
;  [{:pitch+name [:B :bebop-locrian]}]
;  [{:pitch+name [:E :bebop-locrian]}]
;  [{:pitch+name [:C :major]}]
;  [{:pitch+name [:E :spanish-heptatonic]}]
;  [{:pitch+name [:G :mixolydian]}]
;  [{:pitch+name [:D :dorian]}]
;  [{:pitch+name [:D :composite-blues]}]
;  [{:pitch+name [:D :bebop-minor]}]
;  [{:pitch+name [:B :locrian]}]
;  [{:pitch+name [:E :phrygian]}]
;  [{:pitch+name [:C :bebop-major]}]
;  [{:pitch+name [:A :bebop-harmonic-minor]}]
;  [{:pitch+name [:G :composite-blues]}]
;  [{:pitch+name [:F :lydian]}])

(comment
  ; Alt

  ; Overlapping PCIs, but not same shape
  (d/q '[:find (pull ?b [:pitch+name]) ?index
         :in $ %
         :where
         [?a :pitch :C]
         [?a :name :maj]
         (alt ?a ?b ?index)]
       db rules)
; ([{:pitch+name [:Fb :m#5]} 1.0]
;  [{:pitch+name [:E :m#5]} 1.0]
;  [{:pitch+name [:B# :maj]} 1.0])

  ; Overlapping PCIs, and could be same shape
  (d/q '[:find (pull ?b [:pitch+name]) ?index
         :in $ %
         :where
         [?a :pitch :C]
         [?a :name :maj]
         (alt= ?a ?b ?index)]
       db rules)
; ([{:pitch+name [:Fb :m#5]} 1.0]
;  [{:pitch+name [:E :m#5]} 1.0]
;  [{:pitch+name [:B# :maj]} 1.0]
;  [{:pitch+name [:C :maj]} 1.0])

  ; Overlapping PCIs, but not same shape or enharmonic equivalent
  (d/q '[:find (pull ?b [:pitch+name]) ?index
         :in $ %
         :where
         [?a :pitch :C]
         [?a :name :maj]
         (alt!= ?a ?b ?index)]
       db rules))
; ([{:pitch+name [:Fb :m#5]} 1.0]
;  [{:pitch+name [:E :m#5]} 1.0])

(comment
  ; Resolve
  (d/q '[:find (pull ?scale [:pitch+name])
         :in $ %
         :where
         [?chord :pitch :E]
         [?chord :name :maj]
         (neighbor ?chord ?scale :scale-degree/V)]
       db rules)
; ([{:pitch+name [:A :harmonic-minor]}]
;  [{:pitch+name [:A :minor-hexatonic]}]
;  [{:pitch+name [:A :melodic-minor]}]
;  [{:pitch+name [:A :lydian]}]
;  [{:pitch+name [:A :bebop-major]}]
;  [{:pitch+name [:A :bebop]}]
;  [{:pitch+name [:A :major]}]
;  [{:pitch+name [:A :bebop-harmonic-minor]}]
;  [{:pitch+name [:A :minor-six-diminished]}]
;  [{:pitch+name [:A :hungarian-minor]}]
;  [{:pitch+name [:A :harmonic-major]}]
;  [{:pitch+name [:A :lydian-diminished]}])

  (d/q '[:find ?deg
         :in $ %
         :where
         [?chord :pitch :E]
         [?chord :name :maj]
         [?scale :pitch :A]
         [?scale :name :major]
         (neighbor ?chord ?scale ?deg)]
       db rules)
  ; #{[:scale-degree/V]}

  (d/q '[:find (pull ?chord [:pitch+name])
         :in $ %
         :where
         [?scale :pitch :A]
         [?scale :name :major]
         (neighbor ?chord ?scale :scale-degree/V)]
       db rules))
; ([{:pitch+name [:E :maj]}]
;  [{:pitch+name [:E :6]}]
;  [{:pitch+name [:E :sus24]}]
;  [{:pitch+name [:E :sus4]}]
;  [{:pitch+name [:E :11]}]
;  [{:pitch+name [:E :Madd9]}]
;  [{:pitch+name [:E :9]}]
;  [{:pitch+name [:E :7sus4]}]
;  [{:pitch+name [:E :9no5]}]
;  [{:pitch+name [:E :13sus4]}]
;  [{:pitch+name [:E :6add9]}]
;  [{:pitch+name [:E :7]}]
;  [{:pitch+name [:E :7no5]}]
;  [{:pitch+name [:E :5]}]
;  [{:pitch+name [:E :13no5]}]
;  [{:pitch+name [:E :sus2]}]
;  [{:pitch+name [:E :9sus4]}]
;  [{:pitch+name [:E :7add6]}]
;  [{:pitch+name [:E :13]}])

; Path(s)

(defn paths
  "Returns a lazy seq of all non-looping path vectors starting with
  [<start-node>]"
  [nodes-fn path]
  (let [this-node (peek path)]
    (->> (nodes-fn this-node)
         (filter #(not-any? (fn [edge] (= edge [this-node %]))
                            (partition 2 1 path)))
         (mapcat #(paths nodes-fn (conj path %)))
         (cons path))))

(defn trace-paths [m start]
  (remove #(m (peek %)) (paths m [start])))

(defn- find-paths [from-map to-map matches]
  (for [n matches
        from (map reverse (trace-paths from-map n))
        to (map rest (trace-paths to-map n))]
    (vec (concat from to))))

(defn- neighbor-pairs [neighbors q coll]
  (for [node q
        nbr (neighbors node)
        :when (not (contains? coll nbr))]
    [nbr node]))

(defn bidirectional-bfs [start end neighbors]
  (let [find-pairs (partial neighbor-pairs neighbors)
        overlaps (fn [coll q] (seq (filter #(contains? coll %) q)))
        map-set-pairs (fn [map pairs]
                        (persistent! (reduce (fn [map [key val]]
                                               (assoc! map key (conj (get map key #{}) val)))
                                             (transient map) pairs)))]
    (loop [preds {start nil} ; map of outgoing nodes to where they came from
           succs {end nil}   ; map of incoming nodes to where they came from
           q1 (list start)   ; queue of outgoing things to check
           q2 (list end)]    ; queue of incoming things to check
      (when (and (seq q1) (seq q2))
        (if (<= (count q1) (count q2))
          (let [pairs (find-pairs q1 preds)
                preds (map-set-pairs preds pairs)
                q1 (map first pairs)]
            (if-let [all (overlaps succs q1)]
              (find-paths preds succs (set all))
              (recur preds succs q1 q2)))
          (let [pairs (find-pairs q2 succs)
                succs (map-set-pairs succs pairs)
                q2 (map first pairs)]
            (if-let [all (overlaps preds q2)]
              (find-paths preds succs (set all))
              (recur preds succs q1 q2))))))))

(defn neighbors
  [db eid]
  (d/q '[:find [?b ...]
         :in $ % ?a
         :where
         (neighbor ?a ?b)]
       db rules eid))

(defn find-id-paths [db source target]
  (bidirectional-bfs source target (partial neighbors db)))

(defn shape-name [db eid]
  (let [ent (d/entity db eid)]
    (:pitch+name ent)))

(comment
  (let [a (d/entid db [:pitch+name [:C :maj]])
        b (d/entid db [:pitch+name [:C :m]])]
    (map #(map (partial shape-name db) %) (find-id-paths db a b))))

(comment
  ; Fuzzy connect
  (d/q '[:find
         (pull ?neighbor [:pitch+name])
         (pull ?c-alt [:pitch+name])
         (pull ?f-alt [:pitch+name])
         :in $ %
         :where
         [?c :pitch+name [:C :maj]]
         [?f :pitch+name [:F :maj]]
         ; alt= : can be Cmaj or something else
         (alt= ?c ?c-alt)
         (alt= ?f ?f-alt)
         (connect-2 ?neighbor ?c-alt ?f-alt)]
       db rules))

(comment
  (let [parent-data [{:notes #{:F#4 :C5 :E5 :A4}, :type :input-piano, :pcis #{6 0 4 9}}
                     {:notes #{:Gb4 :Eb5 :B4 :A4}, :type :input-piano}
                     {:notes #{:G4 :E4 :B4}, :type :input-piano}]]
    (jigsaw/connect-memo (map :notes parent-data) :chord :max-shapes 1)))
; {{:pitch :Fb, :name :bebop-harmonic-minor}
;  ({:input #{#{:C5 :F#4 :E5 :A4}},
;    :found
;    {:pitch :Gb,
;     :name :m7b5,
;     :heuristics
;     {:contains? 1,
;      :fully-contains? 0,
;      :contained-in? 1,
;      :fully-contained-in? 0,
;      :overlap 1.0,
;      :same-pitch-count? 1,
;      :shares-root? 0}},
;    :context :chord-degree/iim7b5}
;   {:input #{#{:G4 :E4 :B4}},
;    :found
;    {:pitch :Fb,
;     :name :m,
;     :heuristics
;     {:contains? 1,
;      :fully-contains? 0,
;      :contained-in? 1,
;      :fully-contained-in? 0,
;      :overlap 1.0,
;      :same-pitch-count? 1,
;      :shares-root? 0}},
;    :context :chord-degree/i}
;   {:input #{#{:Gb4 :Eb5 :B4 :A4}},
;    :found
;    {:pitch :Cb,
;     :name :7,
;     :bass :Gb,
;     :heuristics
;     {:contains? 1,
;      :fully-contains? 0,
;      :contained-in? 1,
;      :fully-contained-in? 0,
;      :overlap 1.0,
;      :same-pitch-count? 1,
;      :shares-root? 0}},
;    :context :chord-degree/V7})}

(defn connect [shapes]
  (let [#_#_shapes (mapv #(vec ((juxt :pitch :name) %)) shapes)
        nbr (gensym "?")
        syms (map (fn [_] (gensym "?e-")) shapes)
        contexts (map (fn [_] (gensym "?context-")) shapes)
        query {:find `[(~'pull ~nbr [:pitch :name])
                       ~@(mapcat identity (for [i (range (count shapes))
                                                :let [sym (nth syms i)
                                                      context (nth contexts i)]]
                                            `[(~'pull ~sym [:pitch :name]) ~context]))]
               :in '[$ %]
               :where (vec (mapcat identity (for [i (range (count shapes))
                                                  :let [shape (nth shapes i)
                                                        sym (nth syms i)
                                                        context (nth contexts i)]]
                                              `[[~sym :pitch+name ~shape]
                                                (~'neighbor ~sym ~nbr ~context)])))}]
    ; query
    (d/q query db rules)))

(comment
  (connect [[:C :maj] [:D :m]]))

(comment
  ; Fit, find closest compatible shape to a target shape from notes/pcis
  (let [chord (jigsaw/->shape :C_m)
        pci-set (set (map theory/pitches (:pitches chord)))]
    (d/q '[:find
           (pull ?chord [:pitch+name :pci-set])
           ?index
           :in $ % ?pcis
           :where
           [?target :pitch+name [:C :major]]
           (neighbor ?target ?chord)
           [?chord :pci-set ?chord-pcis]
           [(count ?chord-pcis) ?cc]
           [(count ?pcis) ?pc]
           [(>= ?cc ?pc)]
           [(theory/jaccard-index ?pcis ?chord-pcis) ?index]
           [(>= ?index 0.5)]]
         db rules pci-set)))
; ([{:pci-set #{0 7 2}, :pitch+name [:C :sus2]} 0.5]
;  [{:pci-set #{0 7 5}, :pitch+name [:C :sus4]} 0.5]
;  [{:pci-set #{0 7 5}, :pitch+name [:F :sus2]} 0.5]
;  [{:pci-set #{0 7 2}, :pitch+name [:G :sus4]} 0.5]
;  [{:pci-set #{0 7 4}, :pitch+name [:C :maj]} 0.5])

(comment
  ; Fit, using PCIs as start
  (let [chord (jigsaw/->shape :C_m)
        pitches (notes->pci-set (:pitches chord))]
    (d/q '[:find
           (pull ?chord [:pitch+name :pci-set])
           ?index
           :in $ % ?pcis
           :where
           [?target :pitch+name [:C :major]]
           (fit-pci ?pcis ?target ?chord ?index)]
         db rules pitches)))
; ([{:pci-set #{0 7 2}, :pitch+name [:C :sus2]} 0.5]
;  [{:pci-set #{0 7 5}, :pitch+name [:C :sus4]} 0.5]
;  [{:pci-set #{0 7 5}, :pitch+name [:F :sus2]} 0.5]
;  [{:pci-set #{0 7 2}, :pitch+name [:G :sus4]} 0.5]
;  [{:pci-set #{0 7 4}, :pitch+name [:C :maj]} 0.5])

(comment
  ; Fit, using shape as start
  (d/q '[:find
         (pull ?chord' [:pitch+name :pci-set])
         ?index
         :in $ %
         :where
         [?chord :pitch+name [:C :m]]
         [?target :pitch+name [:C :major]]
         (fit ?chord ?target ?chord' ?index)]
       db rules))
; ([{:pci-set #{0 7 2}, :pitch+name [:C :sus2]} 0.5]
;  [{:pci-set #{0 7 5}, :pitch+name [:C :sus4]} 0.5]
;  [{:pci-set #{0 7 5}, :pitch+name [:F :sus2]} 0.5]
;  [{:pci-set #{0 7 2}, :pitch+name [:G :sus4]} 0.5]
;  [{:pci-set #{0 7 4}, :pitch+name [:C :maj]} 0.5])

(comment
  ; Fit second chord based on parent of first chord
  (let [notes1 #{:C# :E :Ab :B} ; sounds good
        notes2 #{:Bb :D :Gb :A} ; doesn't sound as good, keep going on first
        ]
    (d/q '[:find
           (pull ?target [:pitch+name])
           (pull ?e1 [:pitch+name]) ?i1 ?context
           (pull ?e2 [:pitch+name :pitches]) ?ldist ?context2
          ; (pull ?e2' [:pitch+name]) ?index
           :in $ % ?notes1 ?notes2
           :where
           (notes->shapes ?notes1 ?e1 ?i1)
           [?e1 :name :m7]
           ; [(= 1.0 ?i1)]
           (neighbor ?target ?e1 ?context)
           ; (notes->shapes ?notes2 ?e2 ?i2)
           [(jigsaw.experiments.datomic/notes->pci-set ?notes2) ?pcis]
           [?e2 :pci-set ?pcis']
           [(theory/ldist ?pcis ?pcis') ?ldist]
           [(<= ?ldist 1)]
           ; [(<= 0.9 ?i2)]
           (neighbor ?target ?e2 ?context2)
                    ; (fit ?e2 ?target ?e2' ?index)
           #_[(>= 0.9 ?index)]]
         db rules notes1 notes2)))

; Clustering
;   - Playing one or more shapes
;   - Connecting automatically to scale(s)
;   - But no obvious connection, it gets partitioned into subsets
;     - Shared neighbors (connections) and average jaccard found per partitioning

(comment
  (snn [1 2 3])
  (partition-snn [[1 2] [3]])
  (partition-snn-jaccard [[1 2] [3]]))

(comment
  (d/q '[:find ?p ?avg-index
         :in $ %
         :where
         [?a :pitch :C]
         [?a :name :maj]
         [?b :pitch :D]
         [?b :name :m]
         [?c :pitch :E]
         [?c :name :m]
         [(vector ?a ?b ?c) ?coll]
         (partition ?coll ?p)
         (partition-snn-jaccard ?p ?avg-index)]
       db rules))
; #{[([1 433 857]) 0.07258064]
;   [([1] [433 857]) 0.56435645]
;   [([1 433] [857]) 0.5816327]
;   [([1 857] [433]) 0.6551724]}

(comment
  ; Given chords as raw notes,
  ;   find possible shapes and from those shapes,
  ;     find a partitioning of them which minimizes separation and maximizes their shared neighbors (connections)
  (->> (let [n1 #{:C :E :G}
             n2 #{:D :F :A}
             n3 #{:E :G :B}]
         (d/q '[:find
                (pull ?e1 [:db/id :pitch+name])
                (pull ?e2 [:db/id :pitch+name])
                (pull ?e3 [:db/id :pitch+name])
                ?partitions
                #_?snns  ; shared neighbors per partition
                ?avg-index
                :keys e1 e2 e3 partitions #_snns avg-partition-jaccard-index
                :in $ % ?n1 ?n2 ?n3
                :where
                (notes->shapes ?n1 ?e1 ?i1)
                [(<= 0.9 ?i1)]
                (notes->shapes ?n2 ?e2 ?i2)
                [(<= 0.9 ?i2)]
                (notes->shapes ?n3 ?e3 ?i3)
                [(<= 0.9 ?i3)]
                [(vector ?e1 ?e2 ?e3) ?coll]
                (partition ?coll ?partitions)
                #_(partition-snn ?p ?snns)
                (partition-snn-jaccard ?partitions ?avg-index)]
              db rules n1 n2 n3))
       (remove #(zero? (:avg-partition-jaccard-index %)))
       (sort-by (juxt #(count (:partitions %)) (comp - :avg-partition-jaccard-index)))
       (take 5))) ; nil
; ({:e1 {:pitch+name [:C :maj], :db/id 1},
;   :e2 {:pitch+name [:D :m], :db/id 433},
;   :e3 {:pitch+name [:E :m], :db/id 857},
;   :partitions ([1 433 857]),
;   :avg-partition-jaccard-index 0.07258064}
;  {:e1 {:pitch+name [:C :maj], :db/id 1},
;   :e2 {:pitch+name [:D :m], :db/id 433},
;   :e3 {:pitch+name [:E :m], :db/id 857},
;   :partitions ([1 857] [433]),
;   :avg-partition-jaccard-index 0.6551724}
;  {:e1 {:pitch+name [:C :maj], :db/id 1},
;   :e2 {:pitch+name [:D :m], :db/id 433},
;   :e3 {:pitch+name [:E :m], :db/id 857},
;   :partitions ([1 433] [857]),
;   :avg-partition-jaccard-index 0.5816327}
;  {:e1 {:pitch+name [:C :maj], :db/id 1},
;   :e2 {:pitch+name [:D :m], :db/id 433},
;   :e3 {:pitch+name [:Fb :m], :db/id 751},
;   :partitions ([1 433] [751]),
;   :avg-partition-jaccard-index 0.5816327}
;  {:e1 {:pitch+name [:Fb :m#5], :db/id 776},
;   :e2 {:pitch+name [:D :m], :db/id 433},
;   :e3 {:pitch+name [:E :m], :db/id 857},
;   :partitions ([776] [433 857]),
;   :avg-partition-jaccard-index 0.56435645})
