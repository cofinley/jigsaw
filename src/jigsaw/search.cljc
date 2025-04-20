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
    (let [shape (algo/resolve-shape (algo/pitch->note pitch) shape-type shape-name)
          chromas (map specs/pitches (:pitches shape))]
      (assoc (select-keys shape [:pitch :name])
             :chromas chromas))))

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

; Fuzzy-find any shape from notes and their chromas
; Match on chroma instead of:
;  - pitches to capture enharmonic equivalents
;     - Best for input notes, not input chord/scales
;  - intervals to account for missing notes better
;     - Intervals would have to account for all possible intervals
;       - i.e. is it really :P1?

(defn notes->shapes
  [notes shape-type & {:keys [heuristic max-shapes selected-pitch]
                       :or {heuristic :overlap
                            max-shapes 10
                            selected-pitch nil}}]
  (let [chromas (map #(-> % algo/parts :pitch specs/pitches) notes)
        shapes (if (= shape-type :chord) all-chords all-scales)]
    (->> shapes
         (filter #(if (specs/pitch? selected-pitch) (= selected-pitch (:pitch %)) true))
         (map (fn [shape]
                (-> shape
                    (assoc :heuristics (calculate-heuristics chromas (:chromas shape)))
                    (dissoc :chromas))))
         (remove #(and (not= :overlap heuristic) (not= 1 (get-in % [:heuristics heuristic]))))
         (sort-by (juxt ;(comp - :shares-root? :heuristics)
                   (comp - heuristic :heuristics)))
         (take max-shapes))))

(def notes->shapes-memo (memoize notes->shapes))

; Find chords from scales (via matching intervals)

(defn intervals->chord [intervals]
  (when (seq intervals)
    (let [interval-set (set intervals)]
      (specs/intervals->chords interval-set))))

(defn intervals->chords [intervals]
  (if (seq intervals)
    (let [interval-set (set intervals)]
      (->> specs/intervals->chords
           (filter (fn [[chord-interval-set _]] (clojure.set/subset? interval-set chord-interval-set)))
           vals))
    []))

(defn scale-name->chords
  "Get diatonic chords based on thirds; lines up with indexes of :pitches, :degrees, and :notes"
  [scale-name & {:keys [num-thirds] :or {num-thirds 3}}]
  (let [scale (specs/scales scale-name)
        pitches (map #(algo/+interval :C %) (:intervals scale))]
    (for [rotation (range (count (:intervals scale)))]
      (let [rotated-pitches (take num-thirds (take-nth 2 (cycle (utils/rotate pitches rotation))))
            intervals (algo/->intervals rotated-pitches)
            chord-name (specs/intervals->chords (set intervals))]
        chord-name))))

(defn scale->chords
  [{:keys [name pitches]} & {:keys [num-thirds] :or {num-thirds 3}}]
  (let [chord-names (scale-name->chords name :num-thirds num-thirds)]
    (map-indexed (fn [idx pitch]
                   {:pitch pitch :name (nth chord-names idx)})
                 pitches)))

; Find scales from chords
; I.e. re-evaluate chord as intervals from different possible roots; find scales with matching intervals

(defn- degree->tonic
  "Given some pitch (e.g. :Eb), scale (e.g. :major), and the pitch's degree in the scale (e.g. :5), find the original tonic pitch (e.g. :Ab)"
  [pitch scale-name pitch-degree]
  (let [scale (specs/scales scale-name)
        n (.indexOf (:degrees scale) pitch-degree)
        interval (nth (:intervals scale) n)]
    (algo/+interval pitch interval -1)))

(defn- pitches->interval-seqs
  "Generate new intervals to supplied pitches from perspective of all pitches (i.e. test all roots)"
  [pitches]
  (reduce (fn [m root-pitch]
            (let [intervals (keep #(let [interval (algo/->interval root-pitch %)]
                                     (if (= :P8 interval) :P1 interval))
                                  pitches)]
              (if (= (count intervals) (count pitches))
                (assoc m root-pitch intervals)
                m)))
          {}
          specs/simple-pitch-keys))

(defn- intervals->scales
  "Find scale (names) by intervals"
  [intervals]
  (->> specs/intervals->scales
       (filter (fn [[scale-intervals _]]
                 (set/subset? (set intervals) (set scale-intervals))))
       (map second)))

(defn chord->scales
  "Find scales by chord
   Look for overlapping intervals based on pitches
   Optionally filter by desired degree"
  [{:keys [pitch pitches]} & {:keys [degree] :or {degree nil}}]
  (let [rotated-intervals (pitches->interval-seqs pitches)]
    (->> (concat
          (for [[_ intervals] rotated-intervals
                scale-name (intervals->scales intervals)
                :let [scale (get specs/scales scale-name)
                      chord-degree (nth (:degrees scale) (.indexOf (:intervals scale) (first intervals)))
                      tonic (degree->tonic pitch scale-name chord-degree)]
                :when (if (some? degree) (= degree chord-degree) true)]
            {:pitch tonic
             :name scale-name
             :degree chord-degree}))
         (sort-by (comp utils/parse-int name :degree)))))

; Find more deeply linked shapes

(defn contextualize
  "If input-shape is a chord, find degree in candidate-shape (scale)
   If input-shape is a scale, find the candidate-shape's (chord) degree"
  [input-shape candidate-shape]
  (let [base-scale (if (or (contains? input-shape :degrees) (contains? input-shape :degree)) input-shape candidate-shape)
        base-chord (if (= base-scale input-shape) candidate-shape input-shape)
        scale (algo/resolve-shape (:pitch base-scale) :scale (:name base-scale))
        chord (algo/resolve-shape (:pitch base-chord) :chord (:name base-chord))
        degree (nth (:degrees scale) (.indexOf (:pitches scale) (first (:pitches chord))))]
    (algo/degree-chord->roman-numeral degree (:name chord))))

(defn connect-shapes
  [shapes input-shape-type]
  (let [shape->comp-shapes (reduce (fn [m shape]
                                     (assoc m shape
                                            (set (remove #(nil? (:name %)) (map #(select-keys % [:pitch :name])
                                                                                ((if (= input-shape-type :chord)
                                                                                   chord->scales
                                                                                   scale->chords)
                                                                                 (algo/resolve-shape (:pitch shape) input-shape-type (:name shape))))))))
                                   {} shapes)
        comp-shape->shapes (utils/invert-map-of-sets shape->comp-shapes)]
    (into
     {}
     (for [[comp-shape shapes] comp-shape->shapes]
       [(assoc comp-shape :type (if (= input-shape-type :chord) :scale :chord))
        (map (fn [shape]
               {:found shape
                :context (contextualize (algo/resolve-shape (:pitch shape) input-shape-type (:name shape))
                                        (algo/resolve-shape (:pitch comp-shape) (if (= input-shape-type :chord) :scale :chord) (:name comp-shape)))})
             shapes)]))))

(defn connect
  "Given some note sets, find connective shapes
  1. note-sets -> proper shapes
  2. shapes -> complementary shapes (i.e. chord -> scales and vice versa)
  3. Show how the complementary shapes connect all the note sets and their proper shapes"
  [note-seqs input-shape-type & {:keys [max-shapes] :or {max-shapes 1}}]
  (let [note-seq-sets (set note-seqs)
        note-seq->shapes (reduce (fn [m note-seq]
                                   (assoc m note-seq
                                          (set (map #(select-keys % [:pitch :name :heuristics])
                                                    (notes->shapes note-seq input-shape-type :max-shapes max-shapes)))))
                                 {}
                                 note-seqs)
        shape->note-seqs (utils/invert-map-of-sets note-seq->shapes)
        shape->comp-shapes (reduce (fn [m shape]
                                     (assoc m shape
                                            (set (remove #(nil? (:name %)) (map #(select-keys % [:pitch :name])
                                                                                ((if (= input-shape-type :chord)
                                                                                   chord->scales
                                                                                   scale->chords)
                                                                                 (algo/resolve-shape (:pitch shape) input-shape-type (:name shape))))))))
                                   {} (keys shape->note-seqs))
        comp-shape->shapes (utils/invert-map-of-sets shape->comp-shapes)]
    (into
     {}
     (for [[comp-shape shapes] comp-shape->shapes
           ; See if complementary shape can account for all note-seqs
           :let [note-seqs-for-comp-shape (->> shapes
                                               (mapcat #(get shape->note-seqs %))
                                               set)]
           :when (= note-seq-sets note-seqs-for-comp-shape)]
       [(assoc comp-shape :type (if (= input-shape-type :chord) :scale :chord))
        (map (fn [shape]
               {:input (shape->note-seqs shape)
                :found shape
                :context (contextualize (algo/resolve-shape (:pitch shape) input-shape-type (:name shape))
                                        (algo/resolve-shape (:pitch comp-shape) (if (= input-shape-type :chord) :scale :chord) (:name comp-shape)))})
             shapes)]))))

(def memoize-connect (memoize connect))
(def memoize-connect-shapes (memoize connect-shapes))

(comment
  (let [scale (algo/resolve-shape :E :scale :harmonic-minor)]
    (scale->chords scale :num-thirds 4))
  (notes->shapes (:notes (algo/resolve-shape :C4 :chord :maj)) :scale)
  (intervals->chords [:P1 :M3 :P5 :M6])
  (scale->chords (algo/resolve-shape :C :scale :major))
  (scale->chords (algo/resolve-shape :Db :scale :diminished))
  (scale->chords (algo/resolve-shape :C# :scale :diminished))
  (scale->chords (algo/resolve-shape :G :scale :diminished))
  (scale-name->chords :ionian-pentatonic :num-thirds 3)
  (scale->chords (algo/resolve-shape :C :scale :ionian-pentatonic))
  (intervals->scales [:P1 :M3 :P5 :M6])
  (pitches->interval-seqs [:C :E :G])
  (chord->scales (algo/resolve-shape :C :chord :13sus4))
  (contextualize (algo/resolve-shape :B :chord :maj) (algo/resolve-shape :C :scale :major))
  (algo/resolve-shape :F :scale :mixolydian)
  (connect [[:C4 :E4 :G4] [:D4 :F4 :A4]] :chord)
  (connect-shapes [(algo/resolve-shape :C :chord :maj) (algo/resolve-shape :D :chord :m)] :chord)
  (connect [[:Gb4 :A4 :C#5 :E5] [:Gb4 :A4 :B4 :Eb5] [:E4 :G#4 :B4]] :chord)
  (connect [[:F4 :A4 :C5] [:Bb5 :D6 :F6]] :chord :max-shapes 10)
  (connect [[:C4 :E4 :G4 :B4] [:D4 :F4 :A4]] :scale :max-shapes 30))
