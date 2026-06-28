(ns jigsaw.experiments.datomic
  (:require
   #?(:clj [clojure.java.io :as io]
      :cljs [cljs.reader :as reader])
   [clojure.set :as set]
   [datascript.core :as d]
   [datascript.storage.sql.core :as storage-sql]
   [clojure.math.combinatorics :as combo]
   [jigsaw.core :as jigsaw]
   [jigsaw.impl.theory :as theory]
   [jigsaw.utils :as utils]
   [next.jdbc :as jdbc]))

(def schema
  {; Shapes
   :pitch {:db/doc "Shape's starting pitch"
          ; Datascript doesn't have types besides ref and tuple
           ; :db/valueType :db.type/keyword
           :db/cardinality :db.cardinality/one}
   :name {:db/doc "Shape name"
          ; :db/valueType :db.type/keyword
          :db/cardinality :db.cardinality/one}
   :type {:db/doc "Shape type (:chord or :scale)"
          ; :db/valueType :db.type/keyword
          :db/cardinality :db.cardinality/one}
   :pitch+name {:db/doc "Composite key"
                :db/valueType :db.type/tuple
                :db/tupleAttrs [:pitch :name]
                :db/cardinality :db.cardinality/one
                ; Use :db.unique/identity instead of db.unique/value to allow upserts
                :db/unique :db.unique/identity}
   :pitches {:db/doc "Pitches"
             ; :db/valueType :db.type/keyword
             :db/cardinality :db.cardinality/one}
   ; :pitch-set {:db/doc "Pitches (set)"
               ; :db/valueType :db.type/keyword
               ; :db/cardinality :db.cardinality/one}
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
                  :db/cardinality :db.cardinality/one}})

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

; CLJ
(def db-ref {:dbname "db.lg.sqlite" :dbtype "sqlite"})
(def datasource (jdbc/get-datasource db-ref))
(def storage-sql (storage-sql/make datasource
                                   {:dbtype :sqlite}))
; (def storage-fs (d/file-storage "db.sm"))
(def db (d/restore storage-sql))

; CLJS
(def filepath "resources/public/data/db.sm")
; (comment
 ; touch
;   (->> (d/empty-db schema)
;        pr-str
;        (spit filename)))
; (def db (reader/read-string (slurp filename)))
; (def db
;   #?(:clj (with-open [in (io/input-stream filepath)]
;             (dt/read-transit in))
;      :cljs (reader/read-string (slurp filepath))))
; (def db nil)
; (def conn (d/conn-from-db db))

; (def storage storage-fs)

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
  (let [#_#_conn (if-some [_ (d/restore-conn storage)]
                   _
                   (d/create-conn schema storage))
        conn (d/create-conn schema)
        ; db (d/empty-db schema)
        ; conn (d/conn-from-db db)
        shapes (vec (concat (resolve-all-shapes :chord)
                            (resolve-all-shapes :scale)))
        ; node-txns (map #(dissoc % :intervals :degrees) shapes)
        node-txns shapes
        edge-txns (shapes->edge-txns-diatonic shapes)]
    #_(count (concat node-txns edge-txns))
    (doseq [txn-chunk (partition-all 5000 (concat node-txns edge-txns))]
      (d/transact! conn txn-chunk)
      (d/store @conn storage-sql)
      #_(spit filename (pr-str @conn)))
    #_#?(:clj (with-open [out (io/output-stream filepath)]
                (dt/write-transit @conn out)))))

(comment
  (store-datoms-diatonic))

(comment
  (count (d/q '[:find [?f ...]
                :in $ %
                :where
                [?e :pitch :C]
                [?e :name :maj]
                (neighbor ?e ?f)]
              db rules))) ; 40

(defn ^:export notes->pci-set [notes]
  (set (map #(-> % theory/parts :pci) notes)))

(def rules
  ; TODO: rename to something more accurate (e.g. pitch-like -> shape, ->shape)
  '[[(notes->shapes ?notes ?e ?index)
     [(jigsaw.experiments.datomic/notes->pci-set ?notes) ?pcis]
     [?e :pci-set ?pcis']
     [(jigsaw.impl.theory/jaccard-index ?pcis ?pcis') ?index]
     [(>= ?index 0.5)]]

    [(neighbor ?from ?to ?context)
     [?edge :edge/from ?from]
     [?edge :edge/to ?to]
     [?edge :edge/context ?context]]

    [(connect ?coll ?conn)
     [(clojure.core/count ?coll) ?len]
     [(= ?len 1)]
     [(clojure.core/first ?coll) ?first]
     (neighbor ?first ?conn _)]

    [(connect ?coll ?conn)
     [(clojure.core/count ?coll) ?len]
     [(> ?len 1)]
     [(clojure.core/first ?coll) ?first]
     (neighbor ?first ?conn _)
     [(clojure.core/rest ?coll) ?rest]
     (connect ?rest ?conn)]

    [(transpose ?e ?interval ?multiplier ?e')
     [?e :pitch ?pitch]
     [?e :name ?name]
     [(jigsaw.impl.theory/transpose ?pitch ?interval ?multiplier) ?p']
     [?e' :pitch ?p']
     [?e' :name ?name]]

    ; Overlapping PCIs; can be same shape
    [(alt ?e ?e' ?index)
     [?e :pitches ?pitches]
     (notes->shapes ?pitches ?e' ?index)
     [(>= ?index 0.9)]]

    ; Overlapping PCIs; neither same shape nor starting PCI (i.e. enharmonic equivalent)
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
    ; [(partition-snn ?partitioning ?shared-neighbors-per-partition)
    ;  [(jigsaw.experiments.datomic/partition-snn ?partitioning) ?shared-neighbors-per-partition]]

    [(partition-snn ?partitioning ?shared-neighbors-per-partition)
     [(clojure.core/map jigsaw.experiments.datomic/snn ?partitioning) ?shared-neighbors-per-partition]]

    ; Average jaccard index for a list of partitions (seq of seqs)
    [(partition-snn-jaccard ?partitioning ?avg-index)
     [(jigsaw.experiments.datomic/partition-snn-jaccard ?partitioning) ?avg-index]]

    [(partition-connections ?partitioning ?conns)
     [(jigsaw.experiments.datomic/partition-connections ?partitioning) ?conns]]

    #_[(partition-connections ?partitioning ?conns)
       [(clojure.core/map jigsaw.experiments.datomic/connect-ids ?partitioning) ?conns]]])

(defn combo-partitions [coll & args]
  (combo/partitions coll args))

(defn log [& args]
  (prn args)
  true)

; Shared nearest neighbor helpers
(defn shape->neighbors [shape-eid]
  (set (d/q '[:find [?neighbor ...]
              :in $ % ?e
              :where
              (neighbor ?e ?neighbor)]
            db rules shape-eid)))

(defn connect-ids [ids]
  (take 10 (set (d/q '[:find [?conn ...]
                       :in $ % ?coll
                       :where
                       (connect ?coll ?conn)]
                     db rules ids))))

(def connect-ids-memo (memoize connect-ids))

(def shape->neighbors-memo (memoize shape->neighbors))

(defn snn [coll]
  (apply set/intersection (map shape->neighbors-memo coll)))

(defn snn-jaccard [coll]
  (apply theory/jaccard-index (map shape->neighbors-memo coll)))

; (defn partition-snn [partitioning]
;   (map snn partitioning))

(defn avg [& nums]
  (float (/ (reduce + nums) (count nums))))

(defn partition-snn-jaccard [partitioning]
  (->> partitioning
       (map snn-jaccard)
       (apply avg)))

(defn partition-connections [partitioning]
  (map connect-ids-memo partitioning))

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
         (alt ?c ?c-alt)
         (alt ?f ?f-alt)
         [(vector ?c-alt ?f-alt) ?coll]
         (connect ?coll ?neighbor)]
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
  ; Fit second chord based on parent of first chord
  (let [notes1 #{:C# :E :Ab :B} ; sounds good
        notes2 #{:Bb :D :Gb :A} ; doesn't sound as good, keep going on first
        ]
    (d/q '[:find
           (pull ?target [:pitch+name])
           (pull ?e1 [:pitch+name]) #_?i1 ?context
           (pull ?e2 [:pitch+name]) #_?ldist ?context2
          ; (pull ?e2' [:pitch+name]) ?index
           :in $ % ?notes1 ?notes2
           :where
           (notes->shapes ?notes1 ?e1 ?i1)
           [?e1 :name :m7]
           ; [(= 1.0 ?i1)]
           (neighbor ?e1 ?target ?context)
           (notes->shapes ?notes2 ?e2 ?i2)
           ; [(<= 0.9 ?i2)]
           ; [(jigsaw.experiments.datomic/notes->pci-set ?notes2) ?pcis]
           ; [?e2 :pci-set ?pcis']
           ; [(theory/ldist ?pcis ?pcis') ?ldist]
           ; [(<= ?ldist 1)]
           (neighbor ?e2 ?target ?context2)
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
  (map snn [[1 2] [3]])
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
  (d/pull db [:pitch :name] eid))

(def entity-shape-ref-memo (memoize entity-shape-ref))

(defn cluster
  "
  Like connect, but doesn't assume one shared connection for all inputs, could be multiple clusters.
  Given chords as raw notes, find possible shapes
    and from those shapes,
      find a partitioning/clustering of them which minimizes separation and maximizes their shared neighbors (connections)

  Returns n results, defaults to 5
  "
  [db note-seqs & n]
  (let [note-seq-syms (map (fn [_] (gensym "?n-")) note-seqs)
        syms (map (fn [_] (gensym "?e-")) note-seqs)
        ; sym-keys (map-indexed (fn [i _] (symbol (str "e" i))) note-seqs)
        index-syms (map (fn [_] (gensym "?i-")) note-seqs)
        ; snns (gensym "?snns")
        coll (gensym "?coll")
        index-coll (gensym "?index-coll")
        partitioning (gensym "?p")
        connections (gensym "?conns")
        ; avg-index (gensym "?index")
        query {:find `[~coll
                       ~index-coll
                       #_~@(for [sym syms]
                             `(~'pull ~sym [:db/id :pitch :name]))
                       ~partitioning
                       ~connections
                       ; ~snns  ; shared neighbors per partition
                       #_~avg-index]
               :keys `[~'es ~'indexes #_~@sym-keys ~'partitions ~'connections #_~'avg-partition-jaccard-index]
               :in `[~'$ ~'% ~@note-seq-syms]
               :where `[~@(mapcat identity
                                  (for [i (range (count note-seqs))
                                        :let [n (nth note-seq-syms i)
                                              e (nth syms i)
                                              index (nth index-syms i)]]
                                    `[(~'notes->shapes ~n ~e ~index)
                                      ; [(<= 0.9 ~index)]]))
                                      [(<= 1.0 ~index)]]))
                        [(~'vector ~@index-syms) ~index-coll]
                        [(~'vector ~@syms) ~coll]
                        (~'partition ~coll ~partitioning)
                        ; (~'partition-snn ~p ~snns)
                        ; (~'partition-snn-jaccard ~partitioning ~avg-index)
                        ; [(~'not= 0.0 ~avg-index)]
                        (~'partition-connections ~partitioning ~connections)]}]
    (->> (apply d/q query db rules note-seqs)
         (filter #(every? seq (:connections %)))
         (sort-by (juxt #(count (:partitions %)) #_(comp - :avg-partition-jaccard-index)))
         (take (or n 5))
         (map (fn [result]
                (dissoc (assoc result
                               ; :matched-shapes (zipmap note-seqs (zipmap (map entity-shape-ref-memo (:es result))
                               ;                                           (:indexes result)))
                               :matched-shapes (into {} (map-indexed (fn [i note-seq]
                                                                       [note-seq (assoc (entity-shape-ref-memo (nth (:es result) i))
                                                                                        :index (nth (:indexes result) i))]) note-seqs))
                               :partitions (map (fn [part]
                                                  (map entity-shape-ref part)) (:partitions result))
                               :connections (map (fn [part]
                                                   (map entity-shape-ref part)) (:connections result)))
                        :es
                        :indexes)))
         #_(map (fn [result]
                  (dissoc (assoc result :matched-shapes (zipmap note-seqs (map entity-shape-ref-memo (:es result)))
                                 :weighted-index (/ (:avg-partition-jaccard-index result) (count (:partitions result))))
                          :es)))
         #_(sort-by (juxt #(count (:partitions %)) (comp - :avg-partition-jaccard-index)))
         #_(take (or n 10)))))

(comment
  (cluster db [#{:C :E :G}
               #{:D :F :A}
               #{:E :G :B}
               #{:F :A :C}]))

(comment
  (->> (cluster db [#{:Eb4 :Bb4 :C5 :F5}
                    #{:Ab2 :Eb3 :Bb3 :Eb4}
                    #{:Gb2 :Db3 :B3 :E4}])
       (filter #(= 1 (count (nth (:partitions %) 1))))))
; ({:partitions
;   (({:name :sus24, :pitch :Bb} {:name :sus2, :pitch :Ab})
;    ({:name :q, :pitch :Db})),
;   :connections
;   (({:name :melodic-minor, :pitch :Eb}
;     {:name :mixolydian, :pitch :Ab}
;     {:name :bebop, :pitch :Bb}
;     {:name :minor-six-diminished, :pitch :Eb}
;     {:name :composite-blues, :pitch :Ab}
;     {:name :bebop-minor, :pitch :Ab}
;     {:name :bebop-minor, :pitch :Eb}
;     {:name :bebop-locrian, :pitch :C}
;     {:name :locrian, :pitch :G}
;     {:name :locrian-#2, :pitch :C})
;    ({:name :altered, :pitch :Bb}
;     {:name :dorian-#4, :pitch :Fb}
;     {:name :bebop-major, :pitch :Cb}
;     {:name :lydian-diminished, :pitch :Fb}
;     {:name :composite-blues, :pitch :Cb}
;     {:name :bebop-minor, :pitch :Db}
;     {:name :locrian-#2, :pitch :Ab}
;     {:name :minor-hexatonic, :pitch :Cb}
;     {:name :major-pentatonic, :pitch :Fb}
;     {:name :bebop-locrian, :pitch :Eb})),
;   :matched-shapes
;   {#{:F :C :Bb :Eb} {:name :sus24, :pitch :Bb, :index 1.0},
;    #{:Ab2 :Bb3 :Eb3 :Eb4} {:name :sus2, :pitch :Ab, :index 1.0},
;    #{:Db :Gb :B :E} {:name :q, :pitch :Db, :index 1.0}}}
;  {:partitions
;   (({:name :q, :pitch :B#} {:name :sus2, :pitch :G#})
;    ({:name :7sus4, :pitch :F#})),
;   :connections
;   (({:name :bebop-minor, :pitch :G#}
;     {:name :lydian-augmented, :pitch :F#}
;     {:name :lydian, :pitch :G#}
;     {:name :bebop-major, :pitch :D#}
;     {:name :mixolydian, :pitch :G#}
;     {:name :major, :pitch :G#}
;     {:name :spanish-heptatonic, :pitch :E#}
;     {:name :lydian, :pitch :C#}
;     {:name :composite-blues, :pitch :D#}
;     {:name :bebop-minor, :pitch :D#})
;    ({:name :lydian-dominant, :pitch :A}
;     {:name :altered, :pitch :D#}
;     {:name :mixolydian-b6, :pitch :B}
;     {:name :melodic-minor, :pitch :B}
;     {:name :phrygian-dominant, :pitch :F#}
;     {:name :dorian, :pitch :C#}
;     {:name :minor-pentatonic, :pitch :F#}
;     {:name :bebop-locrian, :pitch :A#}
;     {:name :dorian-#4, :pitch :E}
;     {:name :lydian-#9, :pitch :G})),
;   :matched-shapes
;   {#{:F :C :Bb :Eb} {:name :q, :pitch :B#, :index 1.0},
;    #{:Ab2 :Bb3 :Eb3 :Eb4} {:name :sus2, :pitch :G#, :index 1.0},
;    #{:Db :Gb :B :E} {:name :7sus4, :pitch :F#, :index 1.0}}})

(comment
  (let [shape-refs [[:D :m7]
                    [:Eb :7]
                    ; [:Ab :maj7]
                    ; [:B :7]
                    ; [:E :maj7]
                    ; [:G :7]
                    [:C :maj7]]
        ids (map #(d/entid db [:pitch+name %]) shape-refs)]
    (->> (d/q '[:find ?p #_?i ?conns
                :keys partitions #_avg-partition-jaccard-index partition-connections
                :in $ % ?coll
                :where
                (partition ?coll ?p)
                ; (partition-snn-jaccard ?p ?i)
                ; [(not= 0.0 ?i)]
                (partition-connections ?p ?conns)]
              db rules ids)
         (filter #(every? seq (:partition-connections %)))
         (sort-by (juxt #(count (:partitions %)) #_(comp - :avg-partition-jaccard-index)))
         (take 5)
         (map (fn [result]
                (assoc result
                       :partitions (map (fn [part]
                                          (map entity-shape-ref part)) (:partitions result))
                       :partition-connections (map (fn [part]
                                                     (map entity-shape-ref part)) (:partition-connections result))))))))
; ({:partitions
;   (({:name :m7, :pitch :D}
;     {:name :7, :pitch :G}
;     {:name :maj7, :pitch :C})
;    ({:name :7, :pitch :Eb} {:name :maj7, :pitch :Ab})
;    ({:name :7, :pitch :B} {:name :maj7, :pitch :E})),
;   :avg-partition-jaccard-index 0.2708895,
;   :partition-connections
;   (({:name :minor, :pitch :A}
;     {:name :bebop, :pitch :G}
;     {:name :bebop, :pitch :C}
;     {:name :mixolydian, :pitch :G}
;     {:name :locrian, :pitch :B}
;     {:name :dorian, :pitch :D}
;     {:name :phrygian, :pitch :E}
;     {:name :lydian, :pitch :F}
;     {:name :major, :pitch :C})
;    ({:name :mixolydian, :pitch :Eb}
;     {:name :dorian, :pitch :Bb}
;     {:name :bebop, :pitch :Ab}
;     {:name :major, :pitch :Ab}
;     {:name :bebop, :pitch :Eb}
;     {:name :locrian, :pitch :G}
;     {:name :lydian, :pitch :Db}
;     {:name :minor, :pitch :F}
;     {:name :phrygian, :pitch :C})
;    ({:name :bebop, :pitch :E}
;     {:name :minor, :pitch :C#}
;     {:name :mixolydian, :pitch :B}
;     {:name :locrian, :pitch :D#}
;     {:name :major, :pitch :E}
;     {:name :dorian, :pitch :F#}
;     {:name :phrygian, :pitch :G#}
;     {:name :lydian, :pitch :A}
;     {:name :bebop, :pitch :B}))}
;  ...)

; (comment
;   (let [shape-refs [[:D :m7]
;                     [:Eb :7]
;                     [:Ab :maj7]
;                     [:B :7]
;                     [:E :maj7]
;                     [:G :7]
;                     [:C :maj7]]
;         ids (map #(d/entid db [:pitch+name %]) shape-refs)
;         partitions (combo/partitions ids :max (dec (count ids)))
;         ids->neighbors (reduce #(assoc %1 %2 (neighbors db %2)) {} ids)]
;     (->> (for [p partitions
;                :let [n (count p)
;                 ; snns (map snn p)
;                      index (partition-snn-jaccard p)
;                      conns (map connect-ids p)]]
;            {:partitions p
;             :n n
;             :index (/ index n)
;             :conns conns})
;          (filter #(every? seq (:conns %)))
;          (sort-by (juxt (comp - :index) :n))
;          (take 5))))
