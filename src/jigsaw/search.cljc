"Functions to find and test compatibility between shapes (intervals) in the entire search space"

(ns jigsaw.search
  (:require
   [clojure.set :as set]
   [clojure.string :as s]
   [jigsaw.algo :as algo]
   [jigsaw.spec :as specs]
   [jigsaw.utils :as utils]))

; Based on pitch-index (i.e. :C => 0) semitones
(defn- resolve-all-shapes [shape-type]
  (for [pitch (keys specs/pitches)
        shape-name (keys (if (= shape-type :chord) specs/chords specs/scales))
        :when (and (not (s/includes? (name pitch) "bb")) (not (s/includes? (name pitch) "##")))]
    (let [shape (algo/resolve-shape (algo/pitch->note pitch) shape-type shape-name)
          semitones (map specs/pitches (:pitches shape))]
      (assoc (select-keys shape [:pitch :name])
             :semitones semitones))))

(def all-chords (resolve-all-shapes :chord))
(def all-scales (resolve-all-shapes :scale))

; Based on just semitones
(defn- resolve-all-shapes2 [shape-type]
  (for [[shape-name shape] (if (= shape-type :chord) specs/chords specs/scales)]
    (let [semitones (algo/intervals->semitones (:intervals shape))]
      {:name shape-name
       :semitones semitones})))

(def all-chords2 (resolve-all-shapes2 :chord))
(def all-scales2 (resolve-all-shapes2 :scale))

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

; Fuzzy-find any shape from notes (really semitones)

(defn notes->shapes
  [notes shape-type & {:keys [heuristic max-shapes selected-pitch]
                       :or {heuristic :overlap
                            max-shapes 10
                            selected-pitch nil}}]
  (let [semitones (map #(-> % algo/parts :pitch specs/pitches) notes)
        shapes (if (= shape-type :chord) all-chords all-scales)]
    (->> shapes
         (filter #(if (specs/pitch? selected-pitch) (= selected-pitch (:pitch %)) true))
         ; Match on semitones instead of pitches to capture enharmonic equivalents (best for input notes, not input chord/scales)
         ; TODO: check if enharmonics are the same for intersecting semitones
         (map (fn [shape]
                (-> shape
                    (assoc :heuristics (calculate-heuristics semitones (:semitones shape)))
                    (dissoc :semitones))))
         (remove #(and (not= :overlap heuristic) (not= 1 (get-in % [:heuristics heuristic]))))
         (sort-by (juxt (comp - :shares-root? :heuristics)
                        (comp - heuristic :heuristics)))
         (take max-shapes))))

(defn notes->shapes2
  [notes shape-type & {:keys [heuristic max-shapes selected-pitch]
                       :or {heuristic :overlap
                            max-shapes 10
                            selected-pitch nil}}]
  (let [semitones (algo/intervals->semitones (algo/->intervals notes))
        shapes (if (= shape-type :chord) all-chords2 all-scales2)]
    (->> shapes
         ; (filter #(if (specs/pitch? selected-pitch) (= selected-pitch (:pitch %)) true))
         ; Match on semitones instead of pitches to capture enharmonic equivalents (best for input notes, not input chord/scales)
         ; TODO: check if enharmonics are the same for intersecting semitones
         (map (fn [shape]
                (-> shape
                    (assoc :heuristics (calculate-heuristics semitones (:semitones shape))
                           ; TODO: wrong
                           :pitch (:pitch (algo/parts (first notes))))
                    ; (merge (algo/resolve-shape (first notes) shape-type (:name shape)))
                    (dissoc :semitones))))
         (remove #(and (not= :overlap heuristic) (not= 1 (get-in % [:heuristics heuristic]))))
         (sort-by (juxt (comp - :shares-root? :heuristics)
                        (comp - heuristic :heuristics)))
         (take max-shapes))))

(def notes->shapes-memo (memoize notes->shapes))

; Find chords from scales (via matching intervals)

(defn intervals->chord [intervals]
  (when (seq intervals)
    (let [interval-set (set intervals)]
      (specs/chords-by-intervals interval-set))))

(defn intervals->chords [intervals]
  (if (seq intervals)
    (let [interval-set (set intervals)]
      (->> specs/chords-by-intervals
           (filter (fn [[chord-interval-set _]] (clojure.set/subset? interval-set chord-interval-set)))
           vals))
    []))

(defn scale->chords
  "Harmonize; generate diatonic chords based on thirds; lines up with indexes of :pitches, :degrees, and :notes"
  [{pitches :pitches} & {:keys [num-thirds] :or {num-thirds 3}}]
  (for [rotation (range (count pitches))]
    (let [pitches (take num-thirds (take-nth 2 (cycle (utils/rotate pitches rotation))))
          intervals (algo/->intervals pitches)
          chord-name (specs/chords-by-intervals (set intervals))]
      {:pitch (first pitches) :name chord-name})))

; Find scales from chords
; I.e. re-evaluate chord as intervals from different possible roots; find scales with matching intervals

(defn- degree->tonic
  "Given some pitch (e.g. :Eb), scale (e.g. :major), and the pitch's degree (e.g. :5), find the original tonic pitch (e.g. :Ab)"
  [p scale-name degree]
  (let [scale (specs/scales scale-name)
        n (.indexOf (:degrees scale) degree)
        interval (nth (:intervals scale) n)]
    (algo/+interval p interval -1)))

(defn- pitches->interval-seqs
  "Generate new intervals to supplied pitches from perspective of all pitches (i.e. test all roots)"
  [pitches]
  (map (fn [root-pitch]
         (keep #(let [interval (algo/->interval root-pitch %)]
                  (if (= :P8 interval) :P1 interval))
               pitches))
       (keys specs/pitches)))

(defn- intervals->scales
  "Find scale (names) by intervals"
  [intervals]
  (->> specs/scales-by-intervals
       (filter (fn [[scale-intervals _]]
                 (set/subset? (set intervals) (set scale-intervals))))
       ; Return intervals passed in because they're used later to find degree
       (map #(vec [intervals (second %)]))))

(defn chord->scales
  "Find scales by chord
   Look for overlapping intervals based on pitches
   Optionally filter by desired degree"
  [{:keys [pitch pitches intervals]} & {:keys [degree] :or {degree nil}}]
  (let [interval-seqs (pitches->interval-seqs pitches)
        ; Only consider interval seqs with same count as chord intervals
        same-size-interval-seqs (filter #(= (count intervals) (count %)) interval-seqs)
        scale-names (mapcat intervals->scales same-size-interval-seqs)]
    (->> scale-names
         (map (fn [[chord-intervals-variant scale-name]]
                (let [scale (get specs/scales scale-name)
                      chord-degree (nth (:degrees scale) (.indexOf (:intervals scale) (first chord-intervals-variant)))
                      tonic (degree->tonic pitch scale-name chord-degree)]
                  {:pitch tonic
                   :name scale-name
                   :degree chord-degree})))
         (filter #(if (some? degree) (= degree (:degree %)) true))
         (sort-by (comp utils/parse-int name :degree)))))

(comment
  ; TODO:
  ; connect (chord/scale + chord/scale => what is the path between them?)
  ; contextualize (chord/scale + shape name => how does it fit? does connect provide this?)
  (let [scale (algo/resolve-shape :E :scale :harmonic-minor)]
    (scale->chords scale :num-thirds 4))
  (algo/resolve-shape :Gb4 :scale :major-pentatonic)
  (intervals->chords [:P1 :M3 :P5 :M6])
  (intervals->scales [:P1 :M3 :P5 :M6])
  (notes->shapes (:notes (algo/resolve-shape :C4 :chord :maj)) :scale)
  (scale->chords (algo/resolve-shape :G :scale :lydian))
  (chord->scales (algo/resolve-shape :C :chord :13sus4)))
