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

; Fuzzy-find any shape from any shape

(defn notes->shapes
  [notes shape-type & {:keys [heuristic max-shapes selected-pitch]
                       :or {heuristic :overlap
                            max-shapes 10
                            selected-pitch nil}}]
  (let [pitches (into [] (map #(:pitch (algo/parts %)) notes))
        semitones (map specs/pitches pitches)
        ; midis (map algo/note->midi notes)
        ; midi->note (zipmap midis notes)
        ; lowest-pitch (:pitch (algo/parts (get midi->note (first (sort midis)))))
        ; lowest-semitone (get specs/pitches lowest-pitch)
        shapes (if (= shape-type :chord) all-chords all-scales)
        filtered-shapes (filter #(if (specs/pitch? selected-pitch) (= selected-pitch (:pitch %)) true) shapes)
        ; Match on semitones instead of pitches to capture enharmonic equivalents (best for input notes, not input chord/scales)
        shapes-with-heuristics (map (fn [shape]
                                      (assoc shape :heuristics (calculate-heuristics (set semitones) (set (:semitones shape)))))
                                      ; (assoc shape :heuristics (merge (calculate-heuristics (set semitones) (set (:semitones shape)))
                                                                      ; {:root-in-input? (heuristic->float (contains? (set semitones) (get specs/pitches (:pitch shape))))
                                                                      ;  :lowest-input-root? (heuristic->float (= lowest-semitone (get specs/pitches (:pitch shape))))
                                                                      ;  :root-pitches-match? (heuristic->float (= lowest-pitch (:pitch shape)))})))
                                    filtered-shapes)]
    (->> shapes-with-heuristics
         ; TODO: check if enharmonics are the same for intersecting semitones
         (remove #(and (not= :overlap heuristic) (not= 1 (get-in % [:heuristics heuristic]))))
         (sort-by (juxt ;(comp - :root-in-input? :heuristics)
                        ;(comp - :lowest-input-root? :heuristics)
                        ;(comp - :root-pitches-match? :heuristics)
                   (comp - heuristic :heuristics)))
         (take max-shapes))))

(def notes->shapes-memo (memoize notes->shapes))

; Find chords from scales

(defn scale->chords
  "Diatonic chords based on thirds; lines up with indexes of :pitches, :degrees, and :notes"
  [scale & {:keys [num-thirds] :or {num-thirds 3}}]
  (let [{start-pitch :pitch scale-name :name} scale
        scale (specs/scales scale-name)
        scale-intervals (::specs/intervals scale)
        scale-pitches (map (partial algo/+interval start-pitch) scale-intervals)]
    (for [rotation (range (count scale-pitches))]
      (let [pitches (take num-thirds (take-nth 2 (cycle (utils/rotate scale-pitches rotation))))
            notes (algo/pitches->notes pitches)
            intervals (conj (rest (map (partial algo/->interval (first notes)) notes)) :P1)]
        (specs/chords-by-intervals (set intervals))))))

; Find scales from chords

(defn degree->tonic
  "Given some pitch (e.g. :Eb), scale, and degree (e.g. :5), find the tonic pitch (e.g. :)"
  [p scale-name degree]
  (let [scale (utils/strip-ns (specs/scales scale-name))
        n (.indexOf (:degrees scale) degree)
        interval (nth (:intervals scale) n)]
    (algo/+interval p interval -1)))

(defn chord->search-intervals
  "Generate chord intervals from perspective of all pitches"
  [{:keys [pitches intervals]}]
  (into
   []
   (comp
    (map (fn [base-pitch]
           (map (fn [chord-pitch]
                  (let [interval (algo/->interval base-pitch chord-pitch)]
                    (if (= :P8 interval) :P1 interval)))
                pitches)))
    (filter (fn [new-intervals] (= (count intervals) (count (remove nil? new-intervals))))))
   (keys specs/pitches)))

(defn intervals->scales [intervals]
  (reduce
   (fn [v [scale-intervals scale-name]]
     (if (set/subset? (set intervals) (set scale-intervals))
       (let [scale (utils/strip-ns (get specs/scales scale-name))]
         (conj v {:scale-name scale-name
                  :chord-degree (nth (:degrees scale) (.indexOf scale-intervals (first intervals)))}))
       v))
   []
   specs/scales-by-intervals))

(defn chord->scales [{pitch :pitch chord-name :name}]
  (let [chord (utils/strip-ns (algo/resolve-shape pitch :chord chord-name))
        search-intervals (chord->search-intervals chord)
        base-scales (mapcat intervals->scales search-intervals)]
    (map (fn [m]
           (let [{:keys [chord-degree scale-name]} m
                 tonic (degree->tonic (:pitch chord) scale-name chord-degree)]
             {:pitch tonic :degree chord-degree :name scale-name}))
         (sort-by (comp utils/parse-int name :chord-degree) base-scales))))

(comment
  (let [scale (algo/resolve-shape :E :scale :harmonic-minor)
        {pitches ::specs/pitches chord-lists :chords} (assoc scale :chords (scale->chords scale :num-thirds 4))
        chords (map first chord-lists)])
  (scale->chords {:pitch :G :name :lydian})
  (algo/resolve-shape :Gb4 :scale :major-pentatonic)
  (:pitches (algo/resolve-shape :C4 :chord :maj))
  (resolve-all-shapes :chord)
  (notes->shapes :scale [:Eb4 :Gb4 :Ab4])
  (map (juxt :pitch :degree :name) (chord->scales {:pitch :C :name :maj}))
  (chord->scales {:pitch :C :name :13sus4}))
