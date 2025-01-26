(ns jigsaw.db)

(def default-db
  {:nodes #js []
   :edges #js []
  ; :nodes (clj->js [{:id "1"
  ;                    :position {:x 0 :y 0}
  ;                    :type :input-chord
  ;                    :data {}}
  ;                   {:id "2"
  ;                    :position {:x 0 :y 750}
  ;                    :type :input-scale
  ;                    :data {}}])
  ;  :edges #js []
   :node-data {"1" {:pitch :Eb :name :m}
               "2" {:pitch :Eb :name :mixolydian}}
   :edges-clj {}})

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
