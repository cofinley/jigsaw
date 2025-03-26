"Functions to find and test compatibility between shapes (really intervals) in the entire search space"

(ns jigsaw.search
  (:require
   [clojure.set :as set]
   [clojure.string :as s]
   [jigsaw.algo :as algo]
   [jigsaw.spec :as specs]
   [jigsaw.utils :as utils]))

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

(defn- jaccard-index [set1 set2]
  (let [intersection (count (set/intersection set1 set2))
        union (count (set/union set1 set2))]
    (if (zero? union)
      0
      (float (/ intersection union)))))

(def heuristics
  ; Read as "<input> <heurstic> <possible shape>"
  {:contains? set/superset?
   :fully-contains? #(and (set/superset? %1 %2) (not= %1 %2))
   :contained-in? set/subset?
   :fully-contained-in? #(and (set/subset? %1 %2) (not= %1 %2))
   :overlap jaccard-index})

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
  [input-coll dest-coll]
  (reduce-kv (fn [m heuristic heuristic-fn]
               (assoc m heuristic (heuristic->float (heuristic-fn input-coll dest-coll)))) {} heuristics))

; Fuzzy-find any shape from notes (really semitones)

(defn notes->shapes
  [notes shape-type & {:keys [heuristic max-shapes selected-pitch]
                       :or {heuristic :overlap
                            max-shapes 10
                            selected-pitch nil}}]
  (let [semitones (map #(-> % algo/parts :pitch specs/pitches) notes)
        ; midis (map algo/note->midi notes)
        ; midi->note (zipmap midis notes)
        ; lowest-pitch (:pitch (algo/parts (get midi->note (first (sort midis)))))
        ; lowest-semitone (get specs/pitches lowest-pitch)
        shapes (if (= shape-type :chord) all-chords all-scales)]
    (->> shapes
         (filter #(if (specs/pitch? selected-pitch) (= selected-pitch (:pitch %)) true))
         ; Match on semitones instead of pitches to capture enharmonic equivalents (best for input notes, not input chord/scales)
         (map (fn [shape] (dissoc (assoc shape :heuristics (calculate-heuristics (set semitones) (set (:semitones shape))))
                                  :semitones))
                                      ; (assoc shape :heuristics (merge (calculate-heuristics (set semitones) (set (:semitones shape)))
                                                                      ; {:root-in-input? (heuristic->float (contains? (set semitones) (get specs/pitches (:pitch shape))))
                                                                      ;  :lowest-input-root? (heuristic->float (= lowest-semitone (get specs/pitches (:pitch shape))))
                                                                      ;  :root-pitches-match? (heuristic->float (= lowest-pitch (:pitch shape)))}))
              )
         ; TODO: check if enharmonics are the same for intersecting semitones
         (remove #(and (not= :overlap heuristic) (not= 1 (get-in % [:heuristics heuristic]))))
         (sort-by (juxt ;(comp - :root-in-input? :heuristics)
                        ;(comp - :lowest-input-root? :heuristics)
                        ;(comp - :root-pitches-match? :heuristics)
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
           (map val)))
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
  ; contextualize (chord/scale + shape name => how does it fit?)
  ; connect (chord/scale + chord/scale => what is the path between them?)
  (let [scale (algo/resolve-shape :E :scale :harmonic-minor)
        {pitches :pitches chord-lists :chords} (assoc scale :chords (scale->chords scale :num-thirds 4))
        chords (map first chord-lists)])
  (algo/resolve-shape :Gb4 :scale :major-pentatonic)
  (:pitches (algo/resolve-shape :C4 :chord :maj))
  (intervals->chords [:P1 :M3 :P5 :M6])
  (intervals->scales [:P1 :M3 :P5 :M6])
  (notes->shapes [:Eb4 :Gb4 :Ab4] :scale)
  (scale->chords (algo/resolve-shape :G :scale :lydian))
  (chord->scales (algo/resolve-shape :C :chord :13sus4)))
