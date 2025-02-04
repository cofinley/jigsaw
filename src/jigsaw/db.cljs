(ns jigsaw.db)

(def default-db
  {:nodes #js []
   :edges #js []
   :node-data {}})

(defn ->node [props & [parent-props]]
  (merge
   {:id (str (random-uuid))
    :type "default"
    :position (if-let [parent-pos (:position parent-props)]
                {:x (+ 200 (:x parent-pos) (get-in parent-props [:measured :width]))
                 :y (:y parent-pos)}
                {:x 0 :y 0})
    :data {}}
   props))
