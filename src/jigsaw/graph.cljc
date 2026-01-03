(ns jigsaw.graph
  (:require [jigsaw.theory :as theory]
            [clojure.set :as set]
            [jigsaw.utils :as utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graph representation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def graph
  "Blank graph. Add nodes with stable IDs and their definitions."
  {:nodes {}})

;; Example node definition format:
;; {:node-id {:type :input} }
;; {:node-id {:type :fn
;;            :inputs [:node1 :node2]
;;            :fn-id :my-function}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn initial-state []
  {:values    {}   ;; node-id -> chosen value
   :possible  {}   ;; node-id -> vector of possible results
   :params    {}    ;; node-id -> map of function params
   :dirty    #{}})  ;; nodes needing recomputation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependency helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dependents
  "Compute map of node-id -> set of nodes that depend on it."
  [graph]
  (reduce-kv
   (fn [m node-id {:keys [inputs]}]
     (reduce #(update %1 %2 conj node-id) m (or inputs [])))
   {}
   (:nodes graph)))

(defn topo-order
  "Compute topological order of nodes for safe recomputation.
   Returns a vector of node-ids in dependency order."
  [graph]
  (let [nodes (:nodes graph)
        ;; compute a map node -> incoming edges count
        deps (dependents graph)
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

(defmulti compute-fn (fn [fn-id inputs params] fn-id))

(defmethod compute-fn :sum-or-prod [_ inputs _]
  [(+ (:a inputs) (:b inputs)) (* (:a inputs) (:b inputs))])

(defmethod compute-fn :scale [_ inputs params]
  [(:val inputs) (* (:val inputs) (:factor params))])

(defn eval-node
  "Compute possible results for a node if all inputs are present."
  [graph state node-id]
  (let [{:keys [type fn-id inputs]} (get-in graph [:nodes node-id])]
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
  "Invalidate downstream nodes when a node changes."
  [state deps node-id invalidate-self?]
  (reduce
   (fn [s dep]
     (cond-> s
       true (update :values dissoc dep)
       true (update :possible dissoc dep)
       true (update :dirty conj dep)
       invalidate-self? (update :values conj node-id)
       invalidate-self? (update :possible conj node-id)
       invalidate-self? (update :dirty conj node-id)))
   state
   (get deps node-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reactive updates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-input
  "Set an input node's value and mark downstream nodes dirty."
  [state graph node-id input-type value]
  (let [deps (dependents graph)]
    (-> state
        (assoc-in [input-type node-id] value)
        (invalidate deps node-id (= :params input-type)))))

(defn recompute
  "Recompute all dirty nodes in topological order."
  [graph state]
  (let [order (topo-order graph)
        dirty (:dirty state)]
    (reduce
     (fn [s node-id]
       (if (contains? dirty node-id)
         (eval-node graph s node-id)
         s))
     (assoc state :dirty #{})
     order)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  ;; Define a small example graph
  (def example-graph
    {:nodes
     {:a {:type :input}
      :b {:type :input}

      :c {:type :fn
          :inputs [:a :b]
          :fn-id :sum-or-prod}

      :d {:type :fn
          :inputs [:c]
          :fn-id :scale}}})

;; Initialize state
  (def state (initial-state))

  ;; Set input values
  (def state (-> state
                 (set-input example-graph :a :values 2)
                 (set-input example-graph :b :values 3)))

  ;; Recompute all downstream nodes automatically
  (def state (recompute example-graph state))
  ;; => :possible {:c [5 6]}

  ;; User chooses a result for :c
  (def state (-> state
                 (set-input example-graph :c :values 5)))

  ;; Recompute downstream nodes automatically
  (def state (recompute example-graph state))
  ;; => :possible {:a [5 10]}

  ;; Changing an input invalidates downstream nodes
  (def state (-> state
                 (set-input example-graph :a :values 4))))
