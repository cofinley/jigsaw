(ns jigsaw.space
  (:require
   [clojure.set :as set]
   [clojure.math.combinatorics :as combo]
   [jigsaw.impl.theory :as theory]
   [jigsaw.core :as jigsaw]
   [datascript.core :as d]
   [datascript.storage.sql.core :as storage-sql]
   [next.jdbc :as jdbc]))

(def schema
  {; Shapes
   :type {:db/doc "Shape type (:chord or :scale)"
          ; Datascript doesn't have types besides ref and tuple
          ; :db/valueType :db.type/keyword
          :db/cardinality :db.cardinality/one}
   :pitch {:db/doc "Shape's starting pitch"
           ; :db/valueType :db.type/keyword
           :db/cardinality :db.cardinality/one}
   :name {:db/doc "Shape name"
          ; :db/valueType :db.type/keyword
          :db/cardinality :db.cardinality/one}
   :type+pitch+name {:db/doc "Composite key"
                     :db/valueType :db.type/tuple
                     :db/tupleAttrs [:type :pitch :name]
                     :db/cardinality :db.cardinality/one
                     ; Use :db.unique/identity instead of db.unique/value to allow upserts
                     :db/unique :db.unique/identity}
   :pcis {:db/doc "Pitch class indices (set)"
          ; :db/valueType :db.type/long
          :db/cardinality :db.cardinality/many}
   :pitches {:db/doc "Pitches (set)"
             ; :db/valueType :db.type/keyword
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
   ; :edge/from is a subset of :edge/to ?
   :edge/pci-subset? {:db/doc "From's PCIs are a subset of to's?"
                      ; :db/valueType :db.type/boolean
                      :db/cardinality :db.cardinality/one}
   :edge/pitch-subset? {:db/doc "From's pitches are a subset of to's?"
                        ; :db/valueType :db.type/boolean
                        :db/cardinality :db.cardinality/one}
   :edge/interval-subset? {:db/doc "From's intervals are a subset of to's?"
                           ; :db/valueType :db.type/boolean
                           :db/cardinality :db.cardinality/one}
   ; :edge/from is a superset of :edge/to ?
   :edge/pci-superset? {:db/doc "From's PCIs are a superset of to's?"
                        ; :db/valueType :db.type/boolean
                        :db/cardinality :db.cardinality/one}
   :edge/pitch-superset? {:db/doc "From's pitches are a superset of to's?"
                          ; :db/valueType :db.type/boolean
                          :db/cardinality :db.cardinality/one}
   :edge/interval-superset? {:db/doc "From's intervals are a superset of to's?"
                             ; :db/valueType :db.type/boolean
                             :db/cardinality :db.cardinality/one}
   ; :edge/from has a % overlap with :edge/to (same for both directions)
   :edge/pci-jaccard {:db/doc "Jaccard index of from/to's PCIs"
                      ; :db/valueType :db.type/bigint
                      :db/cardinality :db.cardinality/one}
   :edge/pitch-jaccard {:db/doc "Jaccard index of from/to's pitches"
                        ; :db/valueType :db.type/bigint
                        :db/cardinality :db.cardinality/one}
   :edge/interval-jaccard {:db/doc "Jaccard index of from/to's intervals"
                           ; :db/valueType :db.type/bigint
                           :db/cardinality :db.cardinality/one}
   :edge/context {:db/doc "The context of :edge/to in relation to the :edge/from (e.g. C_major scale -> C_maj chord is :chord-degree/I; C_major scale -> D_dorian scale is mode/II)"
                  ; :db/valueType :db.type/keyword
                  :db/cardinality :db.cardinality/one}})

(defn resolve-all-shapes [shape-type]
  (for [pitch theory/simple-pitch-keys
        shape-name (keys (if (= shape-type :chord) theory/chords theory/scales))]
    (let [shape (jigsaw/->shape {:pitch pitch :name shape-name})]
      (assoc
       (dissoc shape :aliases)
       :pitch-set (set (:pitches shape))
       :pci-set (set (map theory/pitches (:pitches shape)))
       :interval-set (set (:intervals shape))
       :degree-set (set (:degrees shape))
       :type (if (theory/chord? shape) :chord :scale)))))

(defn notes->pci-set [notes]
  (set (map #(-> % theory/parts :pci) notes)))

(defn notes->shapes [db notes]
  (->>
   (d/q
    '[:find ?e ?pitch ?name ?index
      :keys db/id pitch name index
      :in $ ?input-pcis ?jac
      :where
      [?e :pcis ?pcis]
      [(?jac ?pcis ?input-pcis) ?index]
      [(> ?index 0.6)]
      [?e :pitch ?pitch]
      [?e :name ?name]]
    db
    (notes->pci-set notes)
    theory/jaccard-index)
   (sort-by :index >)
   (take 1)))

(defn shape->shapes [db shape-ref]
  (let [shape (jigsaw/->shape shape-ref)
        src-pitches (set (:pitches shape))]
    (->>
     (d/q
      '[:find ?pitch ?name
        :keys pitch name
        :in $ ?shape ?src-pcis ?compare
        :where
        [?e :pitches ?dest-pitches]
        [(?compare ?src-pcis ?dest-pitches)]
        [?e :pitch ?pitch]
        [?e :name ?name]]
      db
      shape
      src-pitches
      (if (theory/chord? shape) set/subset? set/superset?))
     (map #(assoc % :context (jigsaw/contextualize shape (jigsaw/->shape %)))))))

(defn connect [db note-seqs]
  (d/q
   '[:find ?pitch ?name ?notes ?shapes
     :keys pitch name notes shapes
     :in $ [?notes ...] ?notes->pci-set ?notes->shapes
     :where
     [?e :pitch ?pitch]
     [?e :name ?name]
     [(?notes->pci-set ?notes) ?input-pcis]
     [?e :pcis ?pcis]
     [(= ?pcis ?input-pcis)]
     [(?notes->shapes $ ?notes) ?shapes]
     ; [?e2 :pitch ?pitch]
     ]
   db
   note-seqs
   notes->pci-set
   (comp set notes->shapes)))

(defn jaccard-int [s1 s2]
  (int (* 100 (theory/jaccard-index s1 s2))))

(defn nodes->edge [src dest]
  (let [context (jigsaw/contextualize src dest)]
    (cond->
     {:edge/from (:db/id src)
      :edge/to (:db/id dest)
      ; :edge/pci-subset? (set/subset? (:pcis src) (:pcis dest))
      ; :edge/pitch-subset? (set/subset? (:pitches src) (:pitches dest))
      ; :edge/interval-subset? (set/subset? (:intervals src) (:intervals dest))
      ; :edge/pci-superset? (set/superset? (:pcis src) (:pcis dest))
      ; :edge/pitch-superset? (set/superset? (:pitches src) (:pitches dest))
      ; :edge/interval-superset? (set/superset? (:intervals src) (:intervals dest))
      :edge/pci-jaccard (jaccard-int (:pci-set src) (:pci-set dest))
      :edge/pitch-jaccard (jaccard-int (:pitch-set src) (:pitch-set dest))
      :edge/interval-jaccard (jaccard-int (:interval-set src) (:interval-set dest))}
      (some? context) (assoc :edge/context context))))

(defn shapes->node-txs [shapes]
  (map-indexed #(assoc %2 :db/id (inc %1)) shapes))

(defn node->edge-txs [nodes]
  (let [jaccard-threshold-pct 50
        ; Pre-number edges, starting at node count + 1
        c (count nodes)
        indexed-combinations (map-indexed #(seq [(+ c 1 %1) %2]) (combo/permuted-combinations (range c) 2))]
    (for [[edge-id [src-node-index dest-node-index]] indexed-combinations
          :let [src-node (nth nodes src-node-index)
                dest-node (nth nodes dest-node-index)]
          :when (and (not= (select-keys src-node [:pitch :name])
                           (select-keys dest-node [:pitch :name]))
                     (not (and (= :chord (:type src-node))
                               (= :chord (:type dest-node)))))
          :let [edge (merge {:db/id edge-id} (nodes->edge src-node dest-node))]
          :when (or (<= jaccard-threshold-pct (:edge/pci-jaccard edge))
                    (<= jaccard-threshold-pct (:edge/pitch-jaccard edge))
                    (<= jaccard-threshold-pct (:edge/interval-jaccard edge))
                    (some? (:edge/context edge)))]
      edge)))

(defn store-datoms []
  (let [db-ref {:dbname "db.sqlite" :dbtype "sqlite"}
        datasource (jdbc/get-datasource db-ref)
        storage (storage-sql/make datasource
                                  {:dbtype :sqlite})
        conn (if-some [_ (d/restore-conn storage)]
               _
               (d/create-conn schema storage))
        ; conn (d/create-conn schema)
        shapes (vec (concat (resolve-all-shapes :chord)
                            (resolve-all-shapes :scale)))
        node-txns (vec (shapes->node-txs shapes))
        edge-txns (node->edge-txs node-txns)
        node-txns-clean (map #(dissoc (assoc %
                                             :pitches (:pitch-set %)
                                             :pcis (:pci-set %)
                                             :intervals (:interval-set %)
                                             :degrees (:degree-set %))
                                      :pitch-set
                                      :pci-set
                                      :interval-set
                                      :degree-set) node-txns)]
    ; (doseq [txn-chunk (partition-all 100 node-txns-clean)]
    (doseq [txn-chunk (partition-all 1000 (concat node-txns-clean edge-txns))]
      (d/transact! conn txn-chunk)
      (d/store @conn storage))))

(comment
  (let [db-ref {:dbname "db.sqlite" :dbtype "sqlite"}
        datasource (jdbc/get-datasource db-ref)
        storage (storage-sql/make datasource
                                  {:dbtype :sqlite})
        db (d/restore storage)]
    (d/q '[:find ?bp ?bc ?cc
           :where
           [?a :pitch :C]
           [?a :name :maj]

           [?x :edge/from ?a]
           [?x :edge/to ?b]
           [?x :edge/context ?bc]

           [?b :pitch ?bp]
           [?b :name :major]

           [?y :edge/from ?b]
           [?y :edge/to ?c]
           [?y :edge/context ?cc]

           [?c :pitch :D]
           [?c :name :m]]
         db)))
