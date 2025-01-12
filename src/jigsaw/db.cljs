(ns jigsaw.db)

(def default-db
  {:nodes {"1" {:id "1"
                :position {:x 0 :y 50}
                :type :input-chord
                :data {:pitch :Eb :name :m}}
           "2" {:id "2"
                :position {:x 450 :y 0}
                :type :output-piano
                :data {}}
           "3" {:id "3"
                :position {:x 0 :y 350}
                :type :input-scale
                :data {:pitch :Eb :name :mixolydian}}
           "4" {:id "4"
                :position {:x 450 :y 300}
                :type :output-piano
                :data {}}}
   :edges {"1->2" {:id "1->2"
                   :source "1"
                   :target "2"}
           "3->4" {:id "3->4"
                   :source "3"
                   :target "4"}}})

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
