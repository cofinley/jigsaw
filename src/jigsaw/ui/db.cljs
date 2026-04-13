(ns jigsaw.ui.db
  (:require
   [cljs.reader]
   [re-frame.core :as re-frame]))

(def default-db
  {:nodes #js []
   :edges #js []
   :node-data {}
   :midi-access nil
   :settings {:midi-input nil
              :midi-output nil
              :midi-triggers {:new-node nil
                              :new-node-find-shapes nil
                              :toggle-clustering nil
                              :stop-recording nil}}
   :recording-id nil
   :clustering-id nil})

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

(def localstorage-key "jigsaw")

(defn data->local-store
  "Puts state into localStorage"
  [data]
  (let [clean-data (dissoc data :midi-access :function-results :node-loading)]
    (.setItem js/localStorage localstorage-key (str clean-data))))     ;; sorted-map written as an EDN map

(re-frame/reg-cofx
 :local-store-data
 (fn [cofx _]
   (assoc cofx :local-store-data
          (into (sorted-map)
                (some->> (.getItem js/localStorage localstorage-key)
                         (cljs.reader/read-string))))))
