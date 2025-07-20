"Functions to find and test compatibility between shapes (intervals) in the entire search space"

(ns jigsaw.search
  (:require
   [clojure.set :as set]
   [jigsaw.algo :as algo]
   [jigsaw.spec :as specs]
   [jigsaw.utils :as utils]))

; Based on chroma
(defn- resolve-all-shapes [shape-type]
  (for [pitch specs/simple-pitch-keys
        shape-name (keys (if (= shape-type :chord) specs/chords specs/scales))]
    (let [shape (algo/->shape {:note (algo/pitch->note pitch) :name shape-name})
          chromas (map specs/pitches (:pitches shape))]
      (assoc shape :chromas chromas))))

(def all-chords (resolve-all-shapes :chord))
(def all-scales (resolve-all-shapes :scale))

(defn- jaccard-index [set1 set2]
  (let [intersection (count (set/intersection set1 set2))
        union (count (set/union set1 set2))]
    (if (zero? union)
      0.0
      (float (/ intersection union)))))

(def heuristic-labels
  {:contains? "partially contains"
   :fully-contains? "fully contains"
   :contained-in? "is partially contained by"
   :fully-contained-in? "is fully contained by"
   :overlap "overlaps with"})

(defn- heuristic->float [x]
  (case x
    false 0
    true 1
    x))

(defn calculate-heuristics
  "Read as '<input> <heurstic> <possible shape>'"
  [input candidate]
  (let [input-set (set input)
        candidate-set (set candidate)]
    {:contains? (heuristic->float (set/superset? input-set candidate-set))
     :fully-contains? (heuristic->float (and (set/superset? input-set candidate-set) (not= input-set candidate-set)))
     :contained-in? (heuristic->float (set/subset? input-set candidate-set))
     :fully-contained-in? (heuristic->float (and (set/subset? input-set candidate-set) (not= input-set candidate-set)))
     :overlap (heuristic->float (jaccard-index input-set candidate-set))
     :shares-root? (heuristic->float (and (some? (seq input)) (some? (seq candidate)) (= (first input) (first candidate))))}))

(defn contextualize
  "If input-shape is a chord, find degree in candidate-shape (scale)
   If input-shape is a scale, find the candidate-shape's (chord) degree"
  [input-shape candidate-shape]
  {:pre [(every? specs/shape? [input-shape candidate-shape])]}
  (let [scale (if (specs/scale? input-shape) input-shape candidate-shape)
        chord (if (= scale input-shape) candidate-shape input-shape)
        degree (nth (:degrees scale) (.indexOf (:pitches scale) (first (:pitches chord))))]
    (algo/degree-chord->roman-numeral degree (:name chord))))

(defn- identify-bass-pitch
  "Identify the bass note (lowest pitch) from a collection of notes"
  [notes]
  (when (seq notes)
    (let [sorted-notes (sort-by algo/note->midi notes)
          lowest-note (first sorted-notes)
          bass-pitch (-> lowest-note algo/parts :pitch)]
      bass-pitch)))

(defn- find-enharomic-equivalent
  "Find enharmonic patches of a source-pitch among target-pitches, based on chroma"
  [source-pitch target-pitches]
  (let [source-chroma (specs/pitches source-pitch)
        target-chromas->pitches (reduce (fn [m pitch]
                                          (let [chroma (specs/pitches pitch)]
                                            (assoc m chroma pitch))) {} target-pitches)]
    (target-chromas->pitches source-chroma)))

(defn notes->shapes
  "Fuzzy-find any shape from notes and their chromas
   Match on chromas instead of...
    - pitches because chromas capture enharmonic equivalents
       - Best for input notes, not input chord/scales
    - intervals because chromas account for missing notes better
       - Intervals would have to account for all possible intervals just in case the root isn't played
         - i.e. is it really :P1?"
  [notes shape-type & {:keys [heuristic max-shapes selected-pitch]
                       :or {heuristic :overlap
                            max-shapes 10
                            selected-pitch nil}}]
  (let [chromas (map #(-> % algo/parts :pitch specs/pitches) notes)
        shapes (if (= shape-type :chord) all-chords all-scales)
        bass-pitch (identify-bass-pitch notes)]
    (->> shapes
         (into []
               (comp
                (filter #(if (specs/pitch? selected-pitch) (= selected-pitch (:pitch %)) true))
                (map #(assoc % :heuristics (calculate-heuristics chromas (:chromas %))))
                (map #(dissoc % :chromas))
                ; Add bass note for chords if not in root position; try to align it with chord enharmonics if possible
                (map #(if (and (= shape-type :chord)
                               bass-pitch
                               (not= (:pitch %) bass-pitch)  ; bass isn't root
                               (not= (specs/pitches (:pitch %)) (specs/pitches bass-pitch)))  ; bass isn't root with different enharmonic (e.g. E and Fb)
                        (assoc % :bass (or (find-enharomic-equivalent bass-pitch (:pitches %)) bass-pitch))
                        %))
                (filter #(or (= :overlap heuristic) (= 1 (get-in % [:heuristics heuristic]))))))
         (sort-by (comp heuristic :heuristics) >)
         (take max-shapes))))

(def notes->shapes-memo (memoize notes->shapes))

; Helper functions for bass/inversion analysis

(defn bass->inversion
  "Determine inversion number from bass note and chord.
   Returns:
   - 1 for first inversion (bass is third)
   - 2 for second inversion (bass is fifth)
   - etc.
   - nil if bass note is not a chord tone (slash chord)"
  [chord-ref bass-pitch]
  (let [chord (algo/->shape chord-ref)
        chord-pitches (:pitches chord)
        bass-index (.indexOf chord-pitches bass-pitch)]
    (when (> bass-index 0)
      bass-index)))

(defn inversion?
  "Check if the chord with bass note represents an inversion
   (bass note is a chord tone)"
  [chord bass-pitch]
  (some? (bass->inversion chord bass-pitch)))

(defn slash-chord?
  "Check if the chord with bass note represents a slash chord
   (bass note is NOT a chord tone)"
  [chord bass-pitch]
  (and (not= (:pitch chord) bass-pitch)
       (not (inversion? chord bass-pitch))))

; Find chords from scales (via matching pitches)

(defn scale->chords
  "Find chords which are diatonic to the scale (i.e. pitch subsets)"
  [{:keys [pitches] :as scale}]
  {:post [(every? specs/shape-ref? %)]}
  (let [pitch-set (set pitches)]
    (for [chord all-chords
          :when (set/subset? (set (:pitches chord)) pitch-set)]
      {:pitch (:pitch chord)
       :name (:name chord)
       :degree (contextualize chord scale)})))

; Find scales from chords

(defn chord->scales
  "Find scales by chord
   Look for overlapping intervals based on pitches
   Optionally filter by desired degree"
  [{:keys [pitches] :as chord} & {:keys [degree] :or {degree nil}}]
  {:post [(every? specs/shape-ref? %)]}
  (let [pitch-set (set pitches)]
    (sort-by #(algo/roman-numeral->int (name (:degree %)))
             (for [scale all-scales
                   :when (set/subset? pitch-set (set (:pitches scale)))
                   :let [chord-degree (contextualize chord scale)]
                   :when (if (some? degree) (= degree (algo/roman-numeral->int (name chord-degree))) true)]
               {:pitch (:pitch scale)
                :name (:name scale)
                :degree chord-degree}))))

; Find more deeply linked shapes

(defn shape->shapes
  "Get complementary shapes (e.g. chord->scales or scale->chords) without specifying input shape type"
  [shape]
  {:pre (specs/shape? shape)
   :post (every? specs/shape-ref? %)}
  ((if (specs/chord? shape)
     chord->scales
     scale->chords) shape))

(defn connect-shapes
  [shapes]
  {:pre [(every? specs/shape? shapes)]}
  (let [shape->comp-shapes (reduce (fn [m shape]
                                     (assoc m shape
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
       [comp-shape (sort-by #(algo/roman-numeral->int (name (:context %)))
                            (map (fn [shape]
                                   {:found shape
                                    :context (contextualize (algo/->shape shape)
                                                            (algo/->shape comp-shape))})
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
                                          (set (map #(select-keys % [:pitch :name :heuristics :bass])
                                                    (notes->shapes note-seq input-shape-type :max-shapes max-shapes)))))
                                 {}
                                 note-seqs)
        shape->note-seqs (utils/invert-map-of-sets note-seq->shapes)
        shape->comp-shapes (reduce (fn [m shape]
                                     (assoc m shape
                                            (set (remove (comp nil? :name)
                                                         (map #(select-keys % [:pitch :name])
                                                              (shape->shapes (algo/->shape shape)))))))
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
       [comp-shape (sort-by #(algo/roman-numeral->int (name (:context %)))
                            (map (fn [shape]
                                   {:input (shape->note-seqs shape)
                                    :found shape
                                    :context (contextualize (algo/->shape shape)
                                                            (algo/->shape comp-shape))})
                                 shapes))]))))

(def memoize-connect (memoize connect))
(def memoize-connect-shapes (memoize connect-shapes))

(defn rotate-intervals
  "Recontextualize intervals by rotating/inverting them"
  [intervals n]
  (let [pitches (map #(algo/+interval :C %) intervals)
        rotated-pitches (utils/rotate pitches n)
        rotated-intervals (algo/->intervals rotated-pitches)]
    rotated-intervals))

(defn scale->mode
  [scale n]
  {:pre [(specs/scale? scale)]
   :post [(specs/shape-ref? %)]}
  (let [pitches (utils/rotate (:pitches scale) n)
        rotated-intervals (rotate-intervals (:intervals scale) n)]
    (when-let [new-scale-name (get specs/intervals->scales rotated-intervals)]
      {:pitch (first pitches)
       :name new-scale-name})))

(defn scale->modes
  [scale]
  {:pre [(specs/scale? scale)]
   :post [(every? specs/shape-ref? %)]}
  (for [n (range (count (:pitches scale)))]
    (scale->mode scale n)))

(defn fit
  "
  Find closest shape to candidate-notes that is compatible with the target-shape
  Addresses extra shapes one doesn't know what to do with or how they fit
  I.e. target-shape of Cmajor and candidate-notes of Cm notes => [Cmaj, ...]

  Cmajor (scale, target shape) pitches and chromas
  C D E F G A B
  0 2 4 5 7 9 11

  Cm (candidate notes)
  C4 Eb4 G4
  0   3  7

  Intersection of chromas: #{0 7}

  Disjoint #{3}
  Nearest #{2 4}

  Test combinations: #{0 2 7} AKA Csus2, #{0 4 7} AKA Cmaj
  "
  [target-shape candidate-notes & {:keys [max-shapes] :or {max-shapes 1}}]
  ; {:pre [(specs/shape? target-shape) (every? specs/note? candidate-notes)]
  ;  :post [(every? specs/shape-ref? %)]}
  (let [comp-shape-type (if (specs/chord? target-shape) :scale :chord)
        shapes (if (= :chord comp-shape-type) all-chords all-scales)
        target-pitches (:pitches target-shape)
        target-chromas (map specs/pitches target-pitches)
        ; target-chromas->pitches (zipmap target-chromas target-pitches)
        target-set (set target-chromas)
        candidate-set (set (map #(mod (algo/note->midi %) 12) candidate-notes))
        ; intersection (set/intersection candidate-set target-set)
        difference-set (set/difference candidate-set target-set)
        chroma->replacements (into {}
                                   (for [diff-chroma difference-set
                                         :let [closest-offset (first (sort (map #(Math/abs (- diff-chroma %)) target-set)))
                                               nearest (set (filter (fn [chroma]
                                                                      (= (Math/abs (- diff-chroma chroma)) closest-offset))
                                                                    target-set))]]
                                     [diff-chroma nearest]))
        combinations (apply utils/cartesian-product (map #(if (set? %)
                                                            (seq %)
                                                            (list %))
                                                         (replace chroma->replacements candidate-set)))]
    (utils/distinct-by
     (juxt :pitch :name)
     (flatten (for [combination combinations
                    :let [new-set (set combination)]]
                (->> shapes
                     (filter (fn [shape]
                               (let [shape-chroma-set (set (:chromas shape))
                                     smaller (if (< (count shape-chroma-set) (count new-set)) shape-chroma-set new-set)]
                                 (= (set/intersection shape-chroma-set new-set) smaller))))
                     (map #(assoc % :heuristics (calculate-heuristics new-set (:chromas %))))
                     (sort-by (juxt #(Math/abs (- (count (:chromas %)) (count new-set)))
                                    #(- (get-in % [:heuristics :overlap]))))
                     (take max-shapes)))))))

(comment
  (let [scale (algo/->shape :E :harmonic-minor)]
    (scale->chords scale))
  (notes->shapes (:notes (algo/->shape :C4 :maj)) :scale)
  (shape->shapes (algo/->shape :Cmajor))
  (map #(abs (apply - %)) (partition 2 1 (map (comp :semitones specs/intervals) (algo/->intervals [:Gb :G :B :Db :D]))))
  (map #(abs (apply - %)) (partition 2 1 (map (comp :semitones specs/intervals) [:P1 :M2 :m3 :P5 :m6])))
  (map #(specs/intervals (algo/+interval :Gb %)) [:P1 :M2 :m3 :P5 :m6])
  (chord->scales (algo/->shape :Eb6add9))
  (scale->chords (algo/->shape :C_diminished))
  (algo/->shape :Db_diminished)
  (contextualize (algo/->shape :B_maj) (algo/->shape :C_major))
  (connect [[:C4 :E4 :G4] [:D4 :F4 :A4]] :chord)
  (connect-shapes [(algo/->shape :Cmaj) (algo/->shape :Dm) (algo/->shape :Em)])
  (connect [(:notes (algo/->shape :C4 :maj)) (:notes (algo/->shape :D4 :m))] :chord)
  (connect [[:Gb4 :A4 :C#5 :E5] [:Gb4 :A4 :B4 :Eb5] [:E4 :G#4 :B4]] :chord)
  (connect [[:F4 :A4 :C5] [:Bb5 :D6 :F6]] :chord :max-shapes 10)
  (connect [[:C4 :E4 :G4 :B4] [:D4 :F4 :A4]] :scale :max-shapes 30)
  (scale->modes (algo/->shape :G_lydian-pentatonic))
  (fit (algo/->shape :C4 :major) (:notes (algo/->shape :C4 :m))))
