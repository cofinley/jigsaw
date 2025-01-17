(ns jigsaw.db)

(def default-db
  {:nodes {"1" {:id "1"
                :position {:x 0 :y 0}
                :type :input-chord
                :data {:pitch :Eb :name :m}}
           "3" {:id "3"
                :position {:x 0 :y 450}
                :type :input-scale
                :data {:pitch :Eb :name :mixolydian}}}
   :edges {}})

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
