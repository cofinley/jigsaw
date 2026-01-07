(ns jigsaw.graph-impl
  (:require [jigsaw.utils :as utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn initial-state []
  {:nodes     {}
   :params    {}    ;; node-id -> map of function params
   :possible  {}    ;; node-id -> vector of possible results
   :values    {}    ;; node-id -> chosen value
   :dirty    #{}})  ;; nodes needing recomputation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependency helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dependents
  "Compute map of node-id -> set of nodes that depend on it."
  ([state]
   (reduce-kv
    (fn [m node-id {:keys [inputs]}]
      (reduce #(update %1 %2 conj node-id) m (or inputs [])))
    {}
    (:nodes state)))

  ([state node-id]
   (let [all-deps (dependents state)
         node-deps (set (get all-deps node-id))]
     (if (empty? node-deps)
       node-deps
       (reduce (fn [result dep]
                 (if (utils/in? result dep)
                   result
                   (apply conj result dep (dependents state dep))))
               []
               node-deps)))))

(defn topo-order
  "Compute topological order of nodes for safe recomputation.
   Returns a vector of node-ids in dependency order."
  [state]
  (let [nodes (:nodes state)
        ;; compute a map node -> incoming edges count
        deps (dependents state)
        indegree (reduce-kv
                  (fn [m node-id {:keys [inputs]}]
                    (assoc m node-id (count (or inputs []))))
                  {}
                  nodes)
        ;; set of nodes with zero indegree
        zero (into #?(:clj clojure.lang.PersistentQueue/EMPTY
                      :cljs cljs.core/PersistentQueue.EMPTY)
                   (filter #(zero? (indegree %)) (keys nodes)))]
    (loop [queue zero
           indegree indegree
           result []]
      (if (empty? queue)
        result
        (let [n (peek queue)
              queue (pop queue)
              result (conj result n)
              children (get deps n #{})
              [queue indegree] (reduce
                                (fn [[q indeg] c]
                                  (let [indeg (update indeg c dec)
                                        q (if (zero? (indeg c)) (conj q c) q)]
                                    [q indeg]))
                                [queue indegree]
                                children)]
          (recur queue indegree result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Node evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti compute-fn (fn [input-values params]))

(defn eval-node
  "Compute possible results for a node if all inputs are present."
  [state node-id]
  (let [{:keys [type fn-id inputs]} (get-in state [:nodes node-id])]
    (case type
      :input state
      :fn (let [input-values (select-keys (:values state) (or inputs []))
                params (get-in state [:params node-id])]
            (if (= (count input-values) (count (or inputs [])))
              (assoc-in state [:possible node-id]
                        (compute-fn fn-id input-values params))
              state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invalidation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn invalidate
  "Invalidate downstream nodes recursively when a node changes."
  [state node-id & {:keys [invalidate-self?]
                    :or {invalidate-self? false}}]
  (let [deps (dependents state node-id)]
    (reduce #(cond-> %1
               true (update :values dissoc %2)
               true (update :possible dissoc %2)
               true (update :dirty conj %2)
               invalidate-self? (update :values dissoc %2)
               invalidate-self? (update :possible dissoc %2)
               invalidate-self? (update :dirty conj %2))
            state
            deps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reactive updates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-input
  "Set an input node's value and mark downstream nodes dirty."
  [state node-id input-type value]
  (-> state
      (assoc-in [input-type node-id] value)
      (invalidate node-id (= input-type :params))))

(defn recompute
  "Recompute all dirty nodes in topological order."
  [state]
  (let [order (topo-order state)
        dirty (:dirty state)]
    (reduce
     (fn [s node-id]
       (if (contains? dirty node-id)
         (eval-node s node-id)
         s))
     (assoc state :dirty #{})
     order)))
