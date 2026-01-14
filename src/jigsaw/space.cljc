(ns jigsaw.space
  (:require
   [clojure.set :as set]
   [jigsaw.impl.theory :as theory]
   [jigsaw.core :as jigsaw]
   [datascript.core :as d]))

(def schema
  {:aka {; Shapes
         :type {:db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
         :pitch {:db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
         :name {:db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
         :pcis {:db/valueType :db.type/long :db/cardinality :db.cardinality/many}
         :pitches {:db/valueType :db.type/keyword :db/cardinality :db.cardinality/many}
         :intervals {:db/valueType :db.type/keyword :db/cardinality :db.cardinality/many}
         :degrees {:db/valueType :db.type/keyword :db/cardinality :db.cardinality/many}
         ; Edges
         :edge/from {:db/valueType :db.type/ref}
         :edge/to {:db/valueType :db.type/ref}
         ; :edge/from is a subset of :edge/to ?
         :edge/pci-subset? {:db/valueType :db.type/boolean}
         :edge/pitch-subset? {:db/valueType :db.type/boolean}
         :edge/interval-subset? {:db/valueType :db.type/boolean}
         ; :edge/from is a superset of :edge/to ?
         :edge/pci-superset? {:db/valueType :db.type/boolean}
         :edge/pitch-superset? {:db/valueType :db.type/boolean}
         :edge/interval-superset? {:db/valueType :db.type/boolean}
         ; :edge/from has a % overlap with :edge/to (same for both directions)
         :edge/pci-jaccard {:db/valueType :db.type/float}
         :edge/pitch-jaccard {:db/valueType :db.type/float}
         :edge/interval-jaccard {:db/valueType :db.type/float}
         :edge/context {:db/doc "The context of :edge/to in relation to the :edge/from (e.g. C_major scale -> C_maj chord is :chord-degree/I; C_major scale -> D_dorian scale is mode/II)"
                        :db/valueType :db.type/any}}})

(defn shape->entity [shape]
  (assoc
   (dissoc shape :aliases)
   :pitches (set (:pitches shape))
   :pcis (set (map theory/pitches (:pitches shape)))
   :intervals (set (:intervals shape))
   :degrees (set (:degrees shape))
   :type (if (theory/chord? shape) :chord :scale)))

(defn resolve-all-shapes [shape-type]
  (for [pitch theory/simple-pitch-keys
        shape-name (keys (if (= shape-type :chord) theory/chords theory/scales))]
    (let [shape (jigsaw/->shape {:pitch pitch :name shape-name})]
      (shape->entity shape))))

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

(defn nodes->edge [src dest]
  (let [context (jigsaw/contextualize (jigsaw/->shape (select-keys src [:pitch :name])) (jigsaw/->shape (select-keys dest [:pitch :name])))]
    (cond->
     {:edge/from (:db/id src)
      :edge/to (:db/id dest)
      :edge/pci-subset? (set/subset? (:pcis src) (:pcis dest))
      :edge/pitch-subset? (set/subset? (:pitches src) (:pitches dest))
      :edge/interval-subset? (set/subset? (:intervals src) (:intervals dest))
      :edge/pci-superset? (set/superset? (:pcis src) (:pcis dest))
      :edge/pitch-superset? (set/superset? (:pitches src) (:pitches dest))
      :edge/interval-superset? (set/superset? (:intervals src) (:intervals dest))
      :edge/pci-jaccard (theory/jaccard-index (:pcis src) (:pcis dest))
      :edge/pitch-jaccard (theory/jaccard-index (:pitches src) (:pitches dest))
      :edge/interval-jaccard (theory/jaccard-index (:intervals src) (:intervals dest))}
      (some? context) (assoc :edge/context context))))

(defn transact-graph! [conn n]
  (let [all-shapes (vec (concat (take n (resolve-all-shapes :chord))
                                (take n (resolve-all-shapes :scale))))
        jaccard-threshold 0.4]
    (doseq [i (range (count all-shapes))
            j (range (count all-shapes))
            :when (< i j)
            :let [src-shape (nth all-shapes i)
                  dest-shape (nth all-shapes j)]
            :when (and (not= (select-keys src-shape [:pitch :name])
                             (select-keys dest-shape [:pitch :name]))
                       (not (and (= :chord (:type src-shape))
                                 (= :chord (:type dest-shape)))))
            :let [src-node-id (d/tempid :src)
                  dest-node-id (d/tempid :dest)
                  src-node (merge {:db/id src-node-id} src-shape)
                  dest-node (merge {:db/id dest-node-id} dest-shape)
                  src->dest-edge (nodes->edge src-node dest-node)
                  dest->src-edge (nodes->edge dest-node src-node)]
            :when (or (<= jaccard-threshold (:edge/pci-jaccard src->dest-edge))
                      (<= jaccard-threshold (:edge/pitch-jaccard src->dest-edge))
                      (<= jaccard-threshold (:edge/interval-jaccard src->dest-edge))
                      (some? (:edge/context src->dest-edge))
                      (some? (:edge/context dest->src-edge)))]
      (let [node-report (d/transact! conn [src-node dest-node])
            real-src-node-id (get-in node-report [:tempids src-node-id])
            real-dest-node-id (get-in node-report [:tempids dest-node-id])]
        (d/transact!
         conn
         [(assoc src->dest-edge
                 :db/id -1
                 :edge/from real-src-node-id
                 :edge/to real-dest-node-id)
          (assoc dest->src-edge
                 :db/id -2
                 :edge/from real-dest-node-id
                 :edge/to real-src-node-id)])))))

(defn run []
  (let [conn (d/create-conn schema)]
    (transact-graph! conn 5)))

(comment
  (def conn (d/create-conn schema))
  (transact-graph! conn 100)
  (d/q '[:find ?p2 ?n2 ?c2 ?n3 ?c3 ;?context ?jac
         :where
         ; From C_maj
         [?from :pitch :C]
         [?from :name :major]
         [?e :edge/from ?from]

         ; to ?

         [?e :edge/to ?to]
         ; [?to :type :scale]
         ; [?to :pitch :D]
         [?to :pitch ?p2]
         [?to :name ?n2]
         [?e :edge/context ?c2]

         ; to D? (fit)
         [?e2 :edge/from ?to]
         ; (not [?e2 :edge/to ?from])
         [?e2 :edge/to ?to-2]
         ; [(not= ?from ?to-2)]
         ; [?to-2 :pitch ?p3]
         [?to2 :pitch :D]
         [?to-2 :name ?n3]
         [?e2 :edge/context ?c3]]
       @conn)

  ; ; (let [scale-n (count (resolve-all-shapes :scale))
  ; ;       chord-n (count (resolve-all-shapes :chord))]
  ;   (let [scale-n 500
  ;         chord-n 500]
  ;     (+ scale-n ;scale nodes
  ;        chord-n ;chord nodes
  ;        (* scale-n scale-n) ; scale-scale edges
  ;        (* scale-n chord-n) ; scale-chord edges
  ;        2))

  ;   (notes->shapes @conn (:notes (jigsaw/->shape :C4_maj)))
  ;   (shape->shapes @conn (jigsaw/->shape :C_maj))
  ;   (connect @conn [(:notes (jigsaw/->shape :C4_maj))
  ;                   (:notes (jigsaw/->shape :G4_maj))]))
  )
