(ns jigsaw.utils
  (:require
   [clojure.string :as s]
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [clojure.walk :as walk]))

(defn in?
  "Returns true if v in coll, else false."
  [coll v]
  (some? (some #{v} coll)))

(defn get-cyclic-distance [a b len]
  (let [distance (mod (- b a) len)
        reverse-distance (mod (- a b) len)]
    (min distance reverse-distance)))

(defn parse-int [x]
  (when-some [int-str (re-find #"\d+" (str x))]
    (#?(:clj Integer/parseInt :cljs js/parseInt) int-str)))

(defn perfect-set?
  [set1 set2]
  (and
   (empty? (clojure.set/difference set1 set2))
   (empty? (clojure.set/difference set2 set1))))

(defn rotate [coll & [n]]
  (take (count coll)
        (drop (or n 1) (cycle coll))))

(defn pairs [coll] (partition 2 1 coll))

(defn strip-ns [m]
  (let [strip-ns-key (fn [k]
                       (if (keyword? k)
                         (keyword (name k))
                         k))]
    (walk/postwalk (fn [x]
                     (if (map? x)
                       (into {} (map (fn [[k v]] [(strip-ns-key k) v]) x))
                       x))
                   m)))

(defn pprint-aliases [shape]
  (let [aliases (:aliases shape)]
    (when (seq aliases) (str "Aliases:\n" (s/join "\n" (map #(str "- " %) aliases))))))

(defmacro prm [& more]
  `(prn ~(reduce #(assoc %1 (keyword (str %2)) %2) {} more)))
