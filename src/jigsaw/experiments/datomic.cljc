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
     [(jigsaw.impl.theory/jaccard-index ?pcis ?pcis') ?index]
     [(>= ?index 0.5)]]

    [(neighbor ?from ?to ?context)
     [?edge :edge/from ?from]
     [?edge :edge/to ?to]
     [?edge :edge/context ?context]]

    [(transpose ?e ?interval ?multiplier ?e')
     [?e :pitch ?pitch]
     [?e :name ?name]
     [(jigsaw.impl.theory/transpose ?pitch ?interval ?multiplier) ?p']
     [?e' :pitch ?p']
     [?e' :name ?name]]

    ; Overlapping PCIs, not same shape
    [(alt ?e ?e' ?index)
     [?e :pci-set ?pcis]
     [?e' :pci-set ?pcis']
     [(jigsaw.impl.theory/jaccard-index ?pcis ?pcis') ?index]
     [(>= ?index 0.9)]]

    ; Overlapping PCIs, not same shape nor starting PCI (i.e. enharmonic equivalent)
    [(alt!= ?e ?e' ?index)
     (alt ?e ?e' ?index)
     [(not= ?e ?e')]
     [?e :pitch ?pitch]
     [?e' :pitch ?pitch']
     [(jigsaw.impl.theory/pitches ?pitch) ?pci]
     [(jigsaw.impl.theory/pitches ?pitch') ?pci']
     [(not= ?pci ?pci')]]

    [(shape? ?e)
     [(clojure.core/integer? ?e)]
     [?e :pitch _]]

    ; From notes
    [(fit ?input ?target ?e ?index)
     (not (shape? ?input))
     (neighbor ?target ?e)
     (notes->shapes ?input ?e ?index)]

    ; From shape
    [(fit ?input ?target ?e ?index)
     (shape? ?input)
     (neighbor ?target ?e)
     [?input :pitches ?pitches]
     (notes->shapes ?pitches ?e ?index)]

    [(partition ?coll ?partitioning)
     ; Max of n-1 partitions, i.e. don't allow all elements to have their own partition
     [(count ?coll) ?n]
     [(dec ?n) ?max]
     [(clojure.math.combinatorics/partitions ?coll :max ?max) [?partitioning ...]]]

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

; Shared nearest neighbor helpers
(defn shape->neighbors [shape-eid]
  (set (d/q '[:find [?neighbor ...]
              :in $ % ?e
              :where
              (neighbor ?e ?neighbor)]
            db rules shape-eid)))

(def shape->neighbors-memo (memoize shape->neighbors))

(defn snn [coll]
  (apply set/intersection (map shape->neighbors-memo coll)))

(defn snn-jaccard [coll]
  (apply theory/jaccard-index (map shape->neighbors-memo coll)))

(defn partition-snn [partitioning]
  (map snn partitioning))

(defn partition-snn-jaccard [partitioning]
  (let [indexes (map snn-jaccard partitioning)
        sum (reduce + indexes)]
    (float (/ sum (count partitioning)))))

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
  ; Fit, using notes/pitches as start
  (let [chord (jigsaw/->shape :C4_m)]
    (prn (:notes chord))
    (d/q '[:find
           (pull ?chord [:pitch+name :pci-set])
           ?index
           ?notes
           :in $ % ?notes
           :where
           [?target :pitch+name [:C :major]]
           (fit ?notes ?target ?chord ?index)]
         db rules (:notes chord))))
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
;     - Shared nearest neighbors (SNN, connections) and average jaccard found per partitioning

(comment
  (snn [1 2 3])
  (partition-snn [[1 2] [3]])
  (partition-snn-jaccard [[1 2] [3]])
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

(defn entity-shape-ref [eid]
  (d/pull db [:pitch :name :db/id] eid))

(def entity-shape-ref-memo (memoize entity-shape-ref))

(defn cluster
  "
  Like connect, but doesn't assume one shared connection for all inputs, could be multiple clusters.
  Given chords as raw notes, find possible shapes
    and from those shapes,
      find a partitioning/clustering of them which minimizes separation and maximizes their shared neighbors (connections)

  Returns n results, defaults to 5
  "
  [note-seqs & n]
  (let [note-seq-syms (map (fn [_] (gensym "?n-")) note-seqs)
        syms (map (fn [_] (gensym "?e-")) note-seqs)
        ; sym-keys (map-indexed (fn [i _] (symbol (str "e" i))) note-seqs)
        index-syms (map (fn [_] (gensym "?i-")) note-seqs)
        ; snns (gensym "?snns")
        coll (gensym "?coll")
        partitioning (gensym "?p")
        avg-index (gensym "?index")
        query {:find `[~coll
                       #_~@(for [sym syms]
                             `(~'pull ~sym [:db/id :pitch :name]))
                       ~partitioning
                       ; ~snns  ; shared neighbors per partition
                       ~avg-index]
               :keys `[~'es #_~@sym-keys ~'partitions ~'avg-partition-jaccard-index]
               :in `[~'$ ~'% ~@note-seq-syms]
               :where `[~@(mapcat identity
                                  (for [i (range (count note-seqs))
                                        :let [n (nth note-seq-syms i)
                                              e (nth syms i)
                                              index (nth index-syms i)]]
                                    `[(~'notes->shapes ~n ~e ~index)
                                      [(<= 0.9 ~index)]]))
                        [(~'vector ~@syms) ~coll]
                        (~'partition ~coll ~partitioning)
                        ; (~'partition-snn ~p ~snns)
                        (~'partition-snn-jaccard ~partitioning ~avg-index)
                        [(~'not= 0.0 ~avg-index)]]}]
    (->> (apply d/q query db rules note-seqs)
         (sort-by (juxt #(count (:partitions %)) (comp - :avg-partition-jaccard-index)))
         (take (or n 5))
         (map (fn [result]
                (dissoc (assoc result :matched-shapes (zipmap note-seqs (map entity-shape-ref-memo (:es result))))
                        :es))))))

(comment
  (cluster [#{:C :E :G}
            #{:D :F :A}
            #{:E :G :B}]))

