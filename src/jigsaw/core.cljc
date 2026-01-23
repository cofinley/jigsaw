(ns jigsaw.core
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
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
      [:chord :scale] (theory/derive-chord-degree dest-shape src-shape)
      [:scale :chord] (theory/derive-chord-degree src-shape dest-shape)
      [:scale :scale] (theory/scales->mode src-shape dest-shape)
      nil)))

(defn shape->abc
  [shape & {:keys [note-length selected-key]
            :or {note-length "1/4"}}]
  {:pre [(theory/shape? shape)]}
  (let [notes (set (:notes shape))
        scale? (theory/scale? shape)
        pitch (:pitch shape)
        shape-name (:name shape)
        key-ref (cond
                  (some? selected-key) selected-key
                  :else {:pitch :C :name :major})
        key-shape (->shape (assoc key-ref :note (theory/pitch->note (:pitch key-ref))))
        key-abc (str (name (:pitch key-shape))
                     " exp "
                     (str/join " " (map theory/note->abc (:notes key-shape))))
        sorted-notes (sort-by theory/note->midi notes)
        pitches-str (str/join " " (map theory/note->abc sorted-notes))]
    (str/join "\n"
              ["X:1"
               (str "K:" key-abc)
               (str "L:" note-length)
               (str/join " "
                         [(when-not scale?
                            (str "\"" (name pitch) (name shape-name) "\""))
                          (if scale?
                            pitches-str
                            (str "[" pitches-str "]"))])])))

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
  (for [pitch theory/simple-pitch-keys
        shape-name (keys (if (= shape-type :chord) theory/chords theory/scales))]
    (let [shape (->shape {:note (theory/pitch->note pitch) :name shape-name})
          pcis (mapv theory/pitches (:pitches shape))]
      (assoc shape :pcis pcis))))

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
  (let [pcis (map #(-> % theory/parts :pci) notes)
        shapes (if (= shape-type :chord) all-chords all-scales)
        bass-pitch (theory/identify-bass-pitch notes)]
    (->> shapes
         (into []
               (comp
                (filter #(if (theory/pitch? selected-pitch) (= selected-pitch (:pitch %)) true))
                (map #(assoc % :heuristics (theory/calculate-heuristics pcis (:pcis %))))
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
         (map #(select-keys % [:pitch :name :bass :heuristics])))))

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
       :context (contextualize chord scale)})))

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
          :let [chord-degree (contextualize chord scale)]
          :when (if (some? degree)
                  (= degree (theory/roman-numeral->int chord-degree))
                  true)]
      {:pitch (:pitch scale)
       :name (:name scale)
       :context chord-degree})))

(defn scale->modes
  [scale]
  {:pre [(theory/scale? scale)]}
  (for [n (range 1 (count (:pitches scale)))
        :let [mode (theory/scale->mode scale n)
              context (contextualize scale (->shape mode))]]
    (assoc mode :context context)))

; Find more deeply linked shapes

(defn shape->shapes
  "Get complementary shapes (e.g. chord->scales or scale->chords) without specifying input shape type"
  [shape]
  {:pre (theory/shape? shape)
   :post (every? theory/shape-ref? %)}
  (if (theory/chord? shape)
    (chord->scales shape)
    (concat
     (scale->chords shape)
     (scale->modes shape))))

(defn connect-shapes
  [shapes]
  {:pre [(every? theory/shape? shapes)]}
  (let [shape->comp-shapes (reduce (fn [m shape]
                                     (assoc m (select-keys shape [:pitch :name :heuristics :bass])
                                            (set (remove (comp nil? :name)
                                                         ; Keep comp-shape reusable by removing :degree (added back later)
                                                         (map #(select-keys % [:pitch :name])
                                                              (shape->shapes shape))))))
                                   {} shapes)
        comp-shape->shapes (utils/invert-map-of-sets shape->comp-shapes)]
    (into
     {}
     (for [[comp-shape matched-shapes] comp-shape->shapes
           :when (= (count shapes) (count matched-shapes))]
       [comp-shape (sort-by #(theory/roman-numeral->int (:context %))
                            (map (fn [shape]
                                   {:found shape
                                    :context (contextualize (->shape shape)
                                                            (->shape comp-shape))})
                                 matched-shapes))]))))

(defn connect
  "Given some note sets, find connective shapes
  1. note-sets -> proper shapes
  2. shapes -> complementary shapes (i.e. chord -> scales and vice versa)
  3. Show how the complementary shapes connect all the note sets and their proper shapes"
  [note-seqs input-shape-type & {:keys [max-shapes] :or {max-shapes 1}}]
  (let [note-seq-sets (set note-seqs)
        note-seq->shapes (reduce (fn [m note-seq]
                                   (assoc m note-seq
                                          (set (map #(select-keys % [:pitch :name :bass :heuristics])
                                                    (notes->shapes note-seq input-shape-type :max-shapes max-shapes)))))
                                 {}
                                 note-seqs)
        shape->note-seqs (utils/invert-map-of-sets note-seq->shapes)
        shape->comp-shapes (reduce (fn [m shape]
                                     (assoc m shape
                                            (set (remove (comp nil? :name)
                                                         (map #(select-keys % [:pitch :name])
                                                              (shape->shapes (->shape shape)))))))
                                   {} (keys shape->note-seqs))
        comp-shape->shapes (utils/invert-map-of-sets shape->comp-shapes)]
    (into
     {}
     (for [[comp-shape shapes] comp-shape->shapes
           ; See if complementary shape can account for all note-seqs
           :let [note-seqs-for-comp-shape (->> shapes
                                               (mapcat #(get shape->note-seqs %))
                                               set)]
           :when (or (= (count note-seq-sets) (count note-seqs-for-comp-shape))
                     (>= (count shapes) 2))]
       [comp-shape (sort-by #(theory/roman-numeral->int (:context %))
                            (map (fn [shape]
                                   {:input (shape->note-seqs shape)
                                    :found shape
                                    :context (contextualize (->shape shape)
                                                            (->shape comp-shape))})
                                 shapes))]))))

(defn fit
  "
  Find closest shape to candidate-notes that is compatible with the target-shape
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
(def connect-memo (memoize connect))
(def connect-shapes-memo (memoize connect-shapes))

;; TODO
;;  - Chord progressions/cadences from scales (i.e. shape of shapes)
;;  - Preview scales on top of chord (progression)
;;    - With different licks/melody rhythm patterns
;;  - Key signature, proper accidentals on music staff
;;  - factor in context more
;;  - highlight overlapping nodes
;;  - mood identification, scale and progression, add colors

(comment
  ;; Resolve a shape
  (->shape :C_maj)
  (->shape :C_major)
  ;; Shape of shapes
  (->progression :C_major [:ii :V :I])
  ;; Shape -> shapes
  (chord->scales (->shape :C :maj))
  (scale->chords (->shape :C :major))
  (scale->modes (->shape :G_lydian-pentatonic))
  (notes->shapes (:notes (->shape :C4_maj)) :chord)
  ;; Generalized shape -> shapes
  (shape->shapes (->shape :C_maj))
  (shape->shapes (->shape :C_major))
  ;; Shapes -> parent shape
  (connect-shapes [(->shape :C_maj) (->shape :D_m) (->shape :E_m)])
  ;;; From notes; more generalized; allows args from notes->shapes
  (connect [[:C4 :E4 :G4] [:D4 :F4 :A4]] :chord)
  (connect [(:notes (->shape :C4 :maj)) (:notes (->shape :D4 :m))] :chord)
  (connect [[:F4 :A4 :C5] [:Bb5 :D6 :F6]] :chord :max-shapes 10)
  (connect [[:C4 :E4 :G4 :B4] [:D4 :F4 :A4]] :scale :max-shapes 30)
  ;; Shape -> ? -> shape
  (fit (->shape :C4 :major) (:notes (->shape :C4 :m))))

