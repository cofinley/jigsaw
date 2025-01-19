(ns jigsaw.search
  (:require
   [clojure.string :as s]
   [clojure.set :as set]
   [jigsaw.algo :as algo]
   [jigsaw.utils :as utils]
   [jigsaw.spec :as specs]))

(defn- resolve-all-shapes [shape-type]
  (for [pitch (keys specs/pitches)
        shape-name (keys (if (= shape-type :chord) specs/chords specs/scales))
        :when (and (not (s/includes? (name pitch) "bb")) (not (s/includes? (name pitch) "##")))]
    (let [shape (utils/strip-ns (algo/resolve-shape (algo/pitch->note pitch) shape-type shape-name))]
      (assoc shape :semitones (map #(get specs/pitches %) (:pitches shape))))))

(def all-chords (resolve-all-shapes :chord))
(def all-scales (resolve-all-shapes :scale))

(defn jaccard-index [set1 set2]
  (let [intersection (count (set/intersection set1 set2))
        union (count (set/union set1 set2))]
    (if (zero? union)
      0
      (float (/ intersection union)))))

(def heuristics
  ; Read as input <heurstic> possible shape
  {:contains? set/superset?
   :fully-contains? #(and (set/superset? %1 %2) (not= %1 %2))
   :contained-in? set/subset?
   :fully-contained-in? #(and (set/subset? %1 %2) (not= %1 %2))
   :overlap jaccard-index})

(defn- heuristic->float [x]
  (case x
    false 0
    true 1
    x))

(defn calculate-heuristics
  [input-coll dest-coll]
  (reduce-kv (fn [m heuristic heuristic-fn]
               (assoc m heuristic (heuristic->float (heuristic-fn input-coll dest-coll)))) {} heuristics))

(defn notes->shapes [notes shape-type & {:keys [heuristic max-shapes] :or {heuristic :overlap
                                                                           max-shapes 10}}]
  (let [pitches (into [] (map #(:pitch (algo/parts %)) notes))
        semitones (map specs/pitches pitches)
        midis (map algo/note->midi notes)
        midi->note (zipmap midis notes)
        lowest-pitch (:pitch (algo/parts (get midi->note (first (sort midis)))))
        lowest-semitone (get specs/pitches lowest-pitch)
        shapes (if (= shape-type :chord) all-chords all-scales)
        ; Match on semitones instead of pitches to capture enharmonic equivalents (best for input notes, not input chord/scales)
        shapes-with-heuristics (map (fn [shape]
                                      (assoc shape :heuristics (calculate-heuristics (set semitones) (set (:semitones shape)))))
                                    shapes)]
    (->> shapes-with-heuristics
         (map #(update % :heuristics merge {:root-in-input? (heuristic->float (contains? (set semitones) (get specs/pitches (:pitch %))))
                                            :lowest-input-root? (heuristic->float (= lowest-semitone (get specs/pitches (:pitch %))))
                                            :root-pitches-match? (heuristic->float (= lowest-pitch (:pitch %)))}))
         (remove #(and (not= :overlap heuristic) (not= 1 (get-in % [:heuristics heuristic]))))
         (sort-by (juxt (comp - :root-in-input? :heuristics)
                        ;(comp - :lowest-input-root? :heuristics)
                        (comp - :root-pitches-match? :heuristics)
                        (comp - heuristic :heuristics)))
         (take max-shapes))))

(defn scale-chords-exact
  [scale & {:keys [num-thirds] :or {num-thirds 4}}]
  (let [{start-pitch :pitch scale-name :name} scale
        scale (specs/scales scale-name)
        scale-intervals (::specs/intervals scale)
        scale-pitches (map (partial algo/+interval start-pitch) scale-intervals)]
    (for [rotation (range (count scale-pitches))]
      (let [pitches (take num-thirds (take-nth 2 (cycle (utils/rotate scale-pitches rotation))))
            notes (algo/pitches->notes pitches)
            intervals (conj (rest (map (partial algo/->interval (first notes)) notes)) :P1)]
        [(specs/chords-by-intervals (set intervals))]))))

(defn scale-chords
  [scale & {:keys [exact? num-thirds] :or {exact? false num-thirds 4}}]
  (if exact?
      ;; TODO: return flat sequence of maps of full shapes
      ;; TODO: for exact, don't just stack thirds, look for better measure
    (scale-chords-exact scale :num-thirds num-thirds)
    (let [{:keys [pitch name]} scale
          scale (specs/scales name)
          scale-intervals (::specs/intervals scale)
          scale-pitches (set (map (partial algo/+interval pitch) scale-intervals))]
      ;; TODO: return flat sequence of maps of full shapes
      (mapv (fn [interval]
              (let [pitch (algo/+interval pitch interval)]
                (mapv first (filter
                             (fn [[_ {chord-intervals ::specs/intervals}]]
                               (let [chord-pitches (set (map (partial algo/+interval pitch) chord-intervals))]
                                 ((if exact? utils/perfect-set? clojure.set/subset?) chord-pitches scale-pitches)))
                             specs/chords))))
            scale-intervals))))

(comment
  (let [scale (algo/resolve-shape :E :scale :harmonic-minor)
        {pitches ::specs/pitches chord-lists :chords} (assoc scale :chords (scale-chords scale :exact? true :num-thirds 4))
        chords (map first chord-lists)])
  (scale-chords-exact {:pitch :G :name :lydian})
  (algo/resolve-shape :Gb4 :scale :major-pentatonic)
  (:pitches (algo/resolve-shape :C4 :chord :maj))
  (resolve-all-shapes :chord)
  (notes->shapes :scale [:Eb4 :Gb4 :Ab4]))
