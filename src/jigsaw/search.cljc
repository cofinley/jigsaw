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

(defn subset-index [set1 set2]
  (let [intersection (count (set/intersection set1 set2))
        smallest (min (count set1) (count set2))]
    (float (/ intersection smallest))))

(defn- similarity
  [input-coll dest-coll dest-coll-root similarity-type]
  (let [index ((case similarity-type
                 :overlap jaccard-index
                 :subset subset-index)
               input-coll dest-coll)
        dest-root-in-input (contains? input-coll dest-coll-root)
        dest-root-in-input-weight 1
        dest-root-in-input-coefficient (if dest-root-in-input dest-root-in-input-weight 0)]
    (/ (+ index dest-root-in-input-coefficient)
       (+ 1             dest-root-in-input-weight))))

(defn- resolve-all-shapes [shape-type]
  (for [pitch (keys specs/pitches)
        shape-name (keys (if (= shape-type :chord) specs/chords specs/scales))
        :when (and (not (s/includes? (name pitch) "bb")) (not (s/includes? (name pitch) "##")))]
    (let [shape (utils/strip-ns (algo/resolve-shape (algo/pitch->note pitch) shape-type shape-name))]
      (assoc shape :semitones (map #(get specs/pitches %) (:pitches shape))))))

(def all-chords (resolve-all-shapes :chord))
(def all-scales (resolve-all-shapes :scale))

;; TODO: record multiple types of similarity measures, return all at once; convert UI to table?
(defn notes->shapes [notes shape-type & {:keys [similarity-type] :or {similarity-type :overlap}}]
  (let [pitches (into [] (map #(:pitch (algo/parts %)) notes))
        semitones (map specs/pitches pitches)
        midis (map algo/note->midi notes)
        midi->note (zipmap midis notes)
        lowest-pitch (:pitch (algo/parts (get midi->note (first (sort midis)))))
        lowest-semitone (get specs/pitches lowest-pitch)
        ; Match on semitones instead of pitches to capture enharmonic equivalents
        shapes (map (fn [shape]
                      (assoc shape :similarity (similarity (set semitones)
                                                           (set (:semitones shape))
                                                           (get specs/pitches (:pitch shape))
                                                           similarity-type)))
                    (if (= shape-type :chord) all-chords all-scales))
        max-similarity (:similarity (apply max-key :similarity shapes))]
    (->> shapes
         (filter #(= (:similarity %) max-similarity))
         (map #(assoc % :lowest-pitch-root? (if (= lowest-semitone (get specs/pitches (:pitch %))) 1 0)))
         (sort-by (juxt (comp - :similarity) (comp - :lowest-pitch-root?))))))

(comment
  (algo/resolve-shape :Gb4 :scale :major-pentatonic)
  (:pitches (algo/resolve-shape :C4 :chord :maj))
  (resolve-all-shapes :chord)
  (notes->shapes :scale [:Eb4 :Gb4 :Ab4]))
