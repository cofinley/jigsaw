(ns jigsaw.utils
  (:require [clojure.set :as set]))

(defn in?
  "Returns true if v in coll, else false."
  [coll v]
  (some? (some #(= v %) coll)))

(defn get-cyclic-distance [a b len]
  (let [distance (mod (- b a) len)
        reverse-distance (mod (- a b) len)]
    (min distance reverse-distance)))

(defn parse-int [x]
  (when-some [int-str (re-find #"\d+" (str x))]
    (Integer/parseInt int-str)))

(defn perfect-set?
  [set1 set2]
  (and
   (empty? (clojure.set/difference set1 set2))
   (empty? (clojure.set/difference set2 set1))))

(defn rotate [scale-sequence]
  (take (count scale-sequence)
        (drop 1 (cycle scale-sequence))))

