(ns jigsaw.core
  (:require
   [clojure.set :as set]
   [clojure.math.combinatorics :as combo]
   [jigsaw.impl.theory :as theory]
   [jigsaw.utils :as utils]))

(def ->shape theory/->shape)

(defn contextualize
  "If src-shape is a chord and dest-shape is a scale or vice versa, find chord's degree of the scale
   If src-shape and dest-shape are both scales, find the dest-shape's mode number
   If src-shape and dest-shape are both chords, return nil (no underlying scale context)"
  [src-shape dest-shape]
  ; {:pre [(every? theory/shape? [src-shape dest-shape])]}
  (let [src-type (if (theory/chord? src-shape) :chord :scale)
        dest-type (if (theory/chord? dest-shape) :chord :scale)]
    (case [src-type dest-type]
      [:chord :scale] (theory/contextualize-chord->scale src-shape dest-shape)
      [:scale :chord] (theory/contextualize-chord src-shape dest-shape)
      [:scale :scale] (theory/contextualize-scale src-shape dest-shape)
      nil)))

(defn ->progression
  "
  :C_major [:ii :V :I] -> [<D_m chord> <G_maj chord> <C_maj chord>]
  :C_major [:ii :bII7 :I] -> [<D_m chord> <Db_7 chord> <C_maj chord>]
  "
  [scale-def chord-degrees]
  (let [scale (->shape scale-def)]
    (mapv #(theory/resolve-chord-degree scale %) chord-degrees)))

; Based on PCI
(defn resolve-all-shapes [shape-type]
  (doall
   (for [pitch theory/simple-pitch-keys
         shape-name (keys (if (= shape-type :chord) theory/chords theory/scales))]
     (let [shape (->shape {:note (theory/pitch->note pitch) :name shape-name})
           pcis (mapv theory/pitches (:pitches shape))]
       (assoc shape :pcis pcis
              :pci-set (apply sorted-set pcis))))))

(def all-chords (resolve-all-shapes :chord))
(def all-scales (resolve-all-shapes :scale))

(defn notes->shapes
  "Fuzzy-find any shape from notes and their PCIs
   Match on PCIs instead of...
    - pitches because PCIs capture enharmonic equivalents
       - Best for input notes, not input chord/scales
    - intervals because PCIs account for missing notes better
       - Intervals would have to account for all possible intervals just in case the root isn't played
         - i.e. is it really :P1?"
  [notes & {:keys [shape-type heuristic max-shapes selected-pitch]
            :or {shape-type :chord
                 heuristic :overlap
                 max-shapes 10
                 selected-pitch nil}}]
  (let [sorted-notes (sort-by theory/note->midi notes)
        pci-set (apply sorted-set (map #(-> % theory/parts :pci) sorted-notes))
        shapes (if (= shape-type :chord) all-chords all-scales)
        bass-pitch (theory/identify-bass-pitch notes)]
    (->> shapes
         (into []
               (comp
                (filter #(if (theory/pitch? selected-pitch) (= selected-pitch (:pitch %)) true))
                (map #(assoc % :heuristics (theory/calculate-heuristics pci-set (:pci-set %))))
                ; Add bass note for chords if not in root position; try to align it with chord enharmonics if possible
                (map #(if (and (= shape-type :chord)
                               bass-pitch
                               (not= (:pitch %) bass-pitch)  ; bass isn't root
                               (not= (theory/pitches (:pitch %)) (theory/pitches bass-pitch)))  ; bass isn't root with different enharmonic (e.g. E and Fb)
                        (assoc % :bass (or (theory/find-enharomic-equivalent bass-pitch (:pitches %)) bass-pitch))
                        %))
                (filter #(or (= :overlap heuristic) (= 1 (get-in % [:heuristics heuristic]))))))
         (sort-by (comp heuristic :heuristics) >)
         (take max-shapes)
         (map #(select-keys % [:pitch :name :bass :heuristics #_:pcis]))
         #_(map #(assoc % :input notes)))))

; Find chords from scales (via matching pitches)

(defn scale->chords
  "Find chords which are diatonic to the scale (i.e. pitch subsets)"
  [{:keys [pitches] :as scale}]
  ; {:post [(every? theory/shape-ref? %)]}
  (let [pitch-set (set pitches)]
    (for [chord all-chords
          :when (set/subset? (set (:pitches chord)) pitch-set)]
      {:pitch (:pitch chord)
       :name (:name chord)
       :context (contextualize scale chord)
       :parent-shape (select-keys scale [:pitch :name])})))

; Find scales from chords

(defn chord->scales
  "Find scales by chord
   Look for overlapping intervals based on pitches
   Optionally filter by desired degree"
  [{:keys [pitches] :as chord} & {:keys [degree] :or {degree nil}}]
  ; {:post [(every? theory/shape-ref? %)]}
  (let [pitch-set (set pitches)]
    ; (sort-by #(theory/roman-numeral->int (:context %))
    (for [scale all-scales
          :when (set/subset? pitch-set (set (:pitches scale)))
          :let [scale-degree (contextualize chord scale)]
          :when (if (some? degree)
                  (= degree (theory/roman-numeral->int scale-degree))
                  true)]
      {:pitch (:pitch scale)
       :name (:name scale)
       :context scale-degree
       :parent-shape (select-keys scale [:pitch :name])})))

(defn scale->modes
  [scale]
  ; {:pre [(theory/scale? scale)]}
  (let [scale-ref (select-keys scale [:pitch :name])]
    (concat
     (for [n (range 1 (count (:pitches scale)))
           :let [mode (theory/scale->mode scale n)]
           :when (some? mode)
           :let [context (contextualize scale (->shape mode))]]
       (assoc mode :context context :parent-shape scale-ref))
     ; Parallel keys/modes
     (when (= (:name scale) :major)
       (for [minor-mode [:minor :harmonic-minor :melodic-minor]]
         {:pitch (:pitch scale)
          :name minor-mode
          :context :mode/parallel
          :parent-shape scale-ref}))
     (when (utils/in? [:minor :harmonic-minor :melodic-minor] (:name scale))
       [{:pitch (:pitch scale)
         :name :major
         :context :mode/parallel
         :parent-shape scale-ref}]))))

(defn chord->chords
  [chord]
  {:pre [(theory/chord? chord)]}
  (when-let [scale (:parent-shape chord)]
    (scale->chords (->shape scale))))

; Find more deeply linked shapes

(defn shape->shapes
  "Get complementary shapes (e.g. chord->scales or scale->chords) without specifying input shape type"
  [shape]
  {:pre (theory/shape? shape)
   :post (every? theory/shape-ref? %)}
  (if (theory/chord? shape)
    (concat (chord->scales shape)
            (chord->chords shape))
    (concat (scale->chords shape)
            (scale->modes shape))))

; TODO/IDEA: list of visited contexts?

(defn fit
  "
  Find closest shape to candidate-notes that is compatible with the target-shape.
  Like notes->shapes with constraint on its neighbor.
  Addresses extra shapes one doesn't know what to do with or how they fit
  I.e. target-shape of Cmajor and candidate-notes of Cm notes => [Cmaj, ...]

  Cmajor (scale, target shape) pitches and PCIs
  C D E F G A B
  0 2 4 5 7 9 11

  Cm (candidate notes)
  C4 Eb4 G4
  0   3  7

  Intersection of PCIs: #{0 7}

  Disjoint #{3}
  Nearest #{2 4}

  Test combinations: #{0 2 7} AKA Csus2, #{0 4 7} AKA Cmaj
  "
  [target-shape candidate-notes & {:keys [max-shapes] :or {max-shapes 1}}]
  ; {:pre [(theory/shape? target-shape) (every? theory/note? candidate-notes)]
  ;  :post [(every? theory/shape-ref? %)]}
  (let [comp-shape-type (if (theory/chord? target-shape) :scale :chord)
        shapes (if (= :chord comp-shape-type) all-chords all-scales)
        target-pitches (:pitches target-shape)
        target-pcis (map theory/pitches target-pitches)
        ; target-pcis->pitches (zipmap target-pcis target-pitches)
        target-set (set target-pcis)
        candidate-set (set (map #(mod (theory/note->midi %) 12) candidate-notes))
        ; intersection (set/intersection candidate-set target-set)
        difference-set (set/difference candidate-set target-set)
        pci->replacements (into {}
                                (for [diff-pci difference-set
                                      :let [closest-offset (first (sort (map #(Math/abs (- diff-pci %)) target-set)))
                                            nearest (set (filter (fn [pci]
                                                                   (= (Math/abs (- diff-pci pci)) closest-offset))
                                                                 target-set))]]
                                  [diff-pci nearest]))
        combinations (apply utils/cartesian-product (map #(if (set? %)
                                                            (seq %)
                                                            (list %))
                                                         (replace pci->replacements candidate-set)))]
    (utils/distinct-by
     (juxt :pitch :name)
     (flatten (for [combination combinations
                    :let [new-set (set combination)]]
                (->> shapes
                     (filter (fn [shape]
                               (let [shape-pci-set (set (:pcis shape))
                                     smaller (if (< (count shape-pci-set) (count new-set)) shape-pci-set new-set)]
                                 (= (set/intersection shape-pci-set new-set) smaller))))
                     (map #(assoc % :heuristics (theory/calculate-heuristics new-set (:pcis %))))
                     (sort-by (juxt #(Math/abs (- (count (:pcis %)) (count new-set)))
                                    #(- (get-in % [:heuristics :overlap]))))
                     (take max-shapes)
                     (map #(select-keys % [:pitch :name :heuristics]))))))))

(def notes->shapes-memo (memoize notes->shapes))
(def shape->shapes-memo (memoize shape->shapes))

(defn avg [& nums]
  (float (/ (reduce + nums) (count nums))))

(defn cluster
  "
  Find best clustering of xs to minimize total clusters and maximize shared connections per cluster.
  All inputs, as a single cluster, may not share common connections. Cluster the inputs until each cluster has common connections.
  xs can be note seqs, which will get turned into some combination of found shapes (max-shapes per note seq).
  xs can be shapes, which will be used as-is.
  "
  [xs & {:keys [shape-type max-results max-shapes max-clusters]
         :or {shape-type :chord
              max-results 3
              max-shapes 5
              max-clusters 2}}]
  (let [shapes (if (every? theory/shape? xs)
                 (map vector xs)
                 (map (fn [note-seq] (notes->shapes note-seq :max-shapes max-shapes :shape-type shape-type)) xs))
        shape-ref->neighbors (reduce (fn [m shape]
                                       (assoc m (theory/->shape-ref shape) (set (map theory/->shape-ref (shape->shapes-memo shape)))))
                                     {} (doall (map ->shape (flatten shapes))))
        snn-info (fn [partition]
                   (let [neighbors-per-shape (map #(shape-ref->neighbors %) partition)
                         shared-neighbors (apply set/intersection neighbors-per-shape)]
                     {:snn shared-neighbors
                      :snn-index (if (seq shared-neighbors)
                                   (apply theory/jaccard-index neighbors-per-shape)
                                   0.0)}))
        max-partitions (dec (count xs))]
    (->> (for [combination (apply combo/cartesian-product shapes)
               :let [trace (zipmap xs combination)
                     combination-refs (map theory/->shape-ref combination)]
               partitioning (combo/partitions combination-refs :max (min max-partitions max-clusters))
               :let [partitioning-snn-info (map snn-info partitioning)
                     partitioning-snn-index (apply avg (map :snn-index partitioning-snn-info))]
               :when (and (not= 0.0 partitioning-snn-index)
                          (every? seq (map :snn partitioning-snn-info)))]
           {:trace trace
            :avg-shape-overlap (apply avg (map #(get-in % [:heuristics :overlap] 1.0) (vals trace)))
            :clusters partitioning
            :avg-connection-overlap partitioning-snn-index
            :connections-by-cluster (map :snn partitioning-snn-info)})
         (sort-by (juxt #(count (:clusters %))
                        (comp - :avg-shape-overlap)
                        (comp - :avg-connection-overlap)))
         (take max-results))))

(comment
  (notes->shapes #{:E4 :G4 :B4})
  (cluster [#{:C4 :E4 :G4} #{:D4 :F4 :A4} #{:E4 :G4 :B4}])
  (cluster (map ->shape [:C_maj :D_maj :E_m]) :max-clusters 2)

  (cluster [#{:Eb4 :Bb4 :C5 :F5}
            #{:Ab2 :Eb3 :Bb3 :Eb4}
            #{:Gb2 :Db3 :B3 :E4}] :max-results 3))

;; TODO
;;  - Preview scales on top of chord (progression)
;;    - With different licks/melody rhythm patterns
;;  - Key signature, proper accidentals on music staff
;;  - highlight overlapping nodes
;;  - mood identification, scale and progression, add colors

(comment
  ;; Resolve a shape from a reference
  (->shape {:pitch :C :name :maj})
  (->shape :C :maj)
  ;; (shorthand, single keywords)
  (->shape :C_maj)
  (->shape :C_major)
  ;; Shape of shapes
  (->progression :C_major [:ii :V :I])
  ;; Shape -> shapes
  (chord->scales (->shape :C :maj))
  (scale->chords (->shape :C :minor))
  (scale->modes (->shape :G_lydian-pentatonic))
  (notes->shapes (:notes (->shape :C4_maj)) :chord)
  ;; (generalized version)
  (shape->shapes (->shape :C_maj))
  (shape->shapes (->shape :C_major))
  ;; Shapes -> common parent shape(s)
  (cluster [(->shape :C_maj) (->shape :D_m) (->shape :E_m)])
  (cluster [[:F4 :A4 :C5] [:Bb5 :D6 :F6]] :max-shapes 5 :max-results 1 :max-clusters 1)
  ;; Shape -> ? -> shape
  (fit (->shape :C4 :major) (:notes (->shape :C4 :m))))
