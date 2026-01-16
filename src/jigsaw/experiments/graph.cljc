(ns jigsaw.experiments.graph
  (:require
   [jigsaw.impl.graph :as g]))

(defmethod g/compute-fn :sum [input-values params]
  [(+ (first (vals input-values)) (second (vals input-values)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  ;; Define a small example state
  (def state
    {:nodes
     {:a {:type :input}
      :b {:type :input}

      :c {:type :fn
          :inputs [:a :b]
          :fn-id :sum}

      :d {:type :fn
          :inputs [:a :c]
          :fn-id :sum}

      :e {:type :fn
          :inputs [:c :d]
          :fn-id :sum}

      :f {:type :fn
          :inputs [:b :d]
          :fn-id :sum}}
     :params {}
     :possible {}
     :values {}
     :dirty #{}})

  ;; Set input values
  (def state (-> state
                 (g/set-input :a :values 2)
                 (g/set-input :b :values 3)))

  ;; Recompute all downstream nodes automatically
  (def state (g/recompute state))
  ;; => :possible {:c [5 6]}

  ;; User chooses a result for :c
  (def state (-> state
                 (g/set-input :d :params {:factor 5})
                 (g/set-input :c :values (first (get-in state [:possible :c])))))

;; Recompute downstream nodes automatically
  (def state (g/recompute state))
  ;; => :possible {:a [5 10]}

  ;; Changing an input invalidates downstream nodes
  (def state (-> state
                 (g/set-input :a :values 4)))

  (g/recompute state))
