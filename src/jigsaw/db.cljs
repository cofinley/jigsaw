(ns jigsaw.db)

(def default-db
  {:nodes #js []
   :edges #js []
   :node-data {}})

(defn ->node [props & [parent-props]]
  (merge
   {:id (str (random-uuid))
    :data {}}
   props
   {:zIndex (if-let [parent-z (:zIndex parent-props)]
              (inc parent-z)
              1)
    :position (cond
                ;; Prefer to base position off of parent position + offset
                (and (some? parent-props) (some? (:position parent-props)))
                (let [parent-pos (:position parent-props)]
                  {:x (+ 300 (:x parent-pos) (get-in parent-props [:measured :width]))
                   :y (:y parent-pos)})
                (:position props) (:position props)
                :else {:x 0 :y 0})}))
