(ns jigsaw.utils
  (:require
   [clojure.string :as s]
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

(defn distinct-by [f coll]
  (let [groups (group-by f coll)]
    (map #(first (groups %)) (distinct (map f coll)))))

(defn find-by-keys [mks coll]
  (some #(when (= (select-keys % (keys mks)) mks) %) coll))

(defn invert-map-of-sets
  "From
   {1 #{:a :b :c} 2 #{:b :c :d}}
   To
   {:c #{1 2}, :b #{1 2}, :a #{1}, :d #{2}}
  "
  [m]
  (reduce (fn [a [k v]]
            (assoc a k (conj (get a k #{}) v)))
          {}
          (for [[k s] m
                v s]
            [v k])))

(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn [v-seqs]
                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                    (if (= i -1) nil
                        (if-let [rst (next (v-seqs i))]
                          (assoc v-seqs i rst)
                          (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
              (cons (map first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))
