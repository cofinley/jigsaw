(ns jigsaw.experiments.datomic
  (:require
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
   ; :type {:db/doc "Shape type (:chord or :scale)"
          ; Datascript doesn't have types besides ref and tuple
          ; :db/valueType :db.type/keyword
          ; :db/cardinality :db.cardinality/one}
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

(defn resolve-all-shapes [shape-type]
  (for [pitch theory/simple-pitch-keys
        shape-name (keys (if (= shape-type :chord) theory/chords theory/scales))]
    (let [shape (jigsaw/->shape {:pitch pitch :name shape-name})]
      (assoc
       (dissoc shape :aliases)
       :pitch-set (set (:pitches shape))
       :pci-set (set (map theory/pitches (:pitches shape)))
       ; :interval-set (set (:intervals shape))
       ; :degree-set (set (:degrees shape))
       #_#_:type (if (theory/chord? shape) :chord :scale)))))

(def db-ref {:dbname "db.sqlite" :dbtype "sqlite"})
(def datasource (jdbc/get-datasource db-ref))
(def storage (storage-sql/make datasource
                               {:dbtype :sqlite}))
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

(defn notes->shapes [db notes n]
  (->>
   (d/q
    '[:find (pull ?e [:pitch :name]) ?index
      :keys e index
      :in $ ?input-pcis
      :where
      [?e :pci-set ?pcis]
      [(theory/jaccard-index ?pcis ?input-pcis) ?index]
      [(> ?index 0.6)]]
    db
    (notes->pci-set notes))
   (sort-by :index >)
   (take n)))

(comment
  (notes->shapes db [:C4 :E4 :G4] 5))

(defn connect-rules [n]
  (let [entity-sym (fn [i] (symbol (str "?e" (inc i))))]
    (vec (for [rule-arity (range 2 (inc n))
               :let [entity-syms (map entity-sym (range rule-arity))]]
           `[(~(symbol (str "connect-" rule-arity)) ~'?connection ~@entity-syms)
             ~@(for [i (range rule-arity)
                     :let [e (nth entity-syms i)]]
                 `(~'neighbor ~e ~'?connection ~(symbol (str "?context" i))))]))))
(def base-rules
  '[[(neighbor ?to ?from ?context)
     [?edge :edge/from ?to]
     [?edge :edge/to ?from]
     [?edge :edge/context ?context]]

    [(transpose ?e ?interval ?multiplier ?e')
     [?e :pitch ?pitch]
     [?e :name ?name]
     [(theory/transpose ?pitch ?interval ?multiplier) ?p']
     [?e' :pitch ?p']
     [?e' :name ?name]]

    [(alt ?e ?e' ?index)
     [?e :pci-set ?pcis]
     [?e' :pci-set ?pcis']
     [(theory/jaccard-index ?pcis ?pcis') ?index]
     [(not= ?e ?e')]
     [(>= ?index 0.9)]]

    [(alt= ?e ?e' ?index)
     [?e :pci-set ?pcis]
     [?e' :pci-set ?pcis']
     [(theory/jaccard-index ?pcis ?pcis') ?index]
     [(>= ?index 0.9)]]])

(def rules
  (concat base-rules (connect-rules 10)))

(comment
  ; Cmaj -> ? -> something which is a second mode of the thing before it
  (d/q '[:find (pull ?b [:pitch+name])
         ?context
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
         (neighbor ?a ?b :chord-degree/V)
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
         (neighbor ?v ?dom-scale :chord-degree/I)
         ; Find V of that scale
         (neighbor ?dom-scale ?v-v :chord-degree/V)]
       db rules))

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

(comment
  ; Alt
  (d/q '[:find (pull ?b [:pitch+name]) ?index
         :in $ %
         :where
         [?a :pitch :C]
         [?a :name :maj]
         (alt ?a ?b ?index)]
       db rules))

(comment
  ; Resolve
  (d/q '[:find (pull ?scale [:pitch+name])
         :in $ %
         :where
         [?chord :pitch :E]
         [?chord :name :maj]
         (neighbor ?chord ?scale :chord-degree/V)]
       db rules)

  (d/q '[:find ?deg
         :in $ %
         :where
         [?chord :pitch :E]
         [?chord :name :maj]
         [?scale :pitch :A]
         [?scale :name :major]
         (neighbor ?chord ?scale ?deg)]
       db rules)

  (d/q '[:find (pull ?chord [:pitch+name])
         :in $ %
         :where
         [?scale :pitch :A]
         [?scale :name :major]
         (neighbor ?chord ?scale :chord-degree/V)]
       db rules))

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
  (let [a (d/entid db [:pitch+name [:C :m]])
        b (d/entid db [:pitch+name [:C :major]])]
    (map #(map (partial shape-name db) %) (find-id-paths db a b))))
