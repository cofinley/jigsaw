(ns jigsaw.search
  (:require
   [clojure.string :as s]
   [clojure.set :as set]
   [jigsaw.algo :as algo]
   [jigsaw.utils :as utils]
   [jigsaw.spec :as specs]))

(defn jaccard-index [set1 set2]
  (let [intersection (count (set/intersection set1 set2))
        union (count (set/union set1 set2))]
    (if (zero? union)
      0
      (float (/ intersection union)))))

(defn pitch-similarity
  "Get Jaccard Index of input pitches vs. chord/scale pitches (weighted more if root pitch in input)"
  [input-pitches dest-pitches dest-pitches-root]
  (let [jaccard-index (jaccard-index input-pitches dest-pitches)
        dest-root-in-pitches (contains? input-pitches dest-pitches-root)
        dest-root-in-input-pitches-weight 1
        dest-root-in-input-pitches-coefficient (if dest-root-in-pitches dest-root-in-input-pitches-weight 0)]
    (/ (+ jaccard-index dest-root-in-input-pitches-coefficient)
       (+ 1             dest-root-in-input-pitches-weight))))

(defn resolve-all-shapes [shape-type]
  (for [pitch (keys specs/pitches)
        shape-name (keys (if (= shape-type :chord) specs/chords specs/scales))
        :when (and (not (s/includes? (name pitch) "bb")) (not (s/includes? (name pitch) "##")))]
    (utils/strip-ns (algo/resolve-shape pitch shape-type shape-name))))

(def all-chords (resolve-all-shapes :chord))
(def all-scales (resolve-all-shapes :scale))

(defn notes->shapes [shape-type notes]
  (let [pitches (into [] (map #(:pitch (algo/parts %)) notes))
        midis (map algo/note->midi notes)
        midi->note (zipmap midis notes)
        lowest-pitch (:pitch (algo/parts (get midi->note (first (sort midis)))))
        shapes (map (fn [shape]
                      (assoc shape :similarity (pitch-similarity (set pitches)
                                                                 (set (:pitches shape))
                                                                 (:pitch shape))))
                    (if (= shape-type :chord) all-chords all-scales))
        max-similarity (:similarity (apply max-key :similarity shapes))]
    (->> shapes
         (filter #(= (:similarity %) max-similarity))
         (map #(assoc % :lowest-pitch-root? (if (= lowest-pitch (:pitch %)) 1 0)
                      :notes (map algo/pitch->note (:pitches %))))
         (sort-by (juxt (comp - :similarity) (comp - :lowest-pitch-root?))))))

(comment
  (:pitches (algo/resolve-shape :C4 :chord :maj))
  (notes->shapes :chord [:C4 :E4 :G4]))
