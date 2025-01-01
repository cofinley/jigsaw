(ns jigsaw.db)

(def default-db
  {:nodes {"1" {:id "1"
                :position {:x 0 :y 0}
                :type :input-piano
                :data {:notes #{:C4 :E4 :G4}
                       :midis #{60 64 67}}}
           "2" {:id "2"
                :position {:x 800 :y 0}
                :type :output-piano
                :data {}}}
   :edges {"1->2" {:id "1->2"
                   :source "1"
                   :target "2"}}})

(defn ->node [props]
  (merge
   {:id (str (random-uuid))
    :type "default"
    :position {:x 200 :y 100}
    :data {}}
   props))

(defn ->input-piano-node []
  (->node {:type :input-piano
           :data {:notes #{}
                  :midis #{}}}))

(defn ->input-chord-node []
  (->node {:type :input-chord
           :data {:pitch nil
                  :name nil
                  :notes #{}
                  :midis #{}}}))

(defn ->input-scale-node []
  (->node {:type :input-scale
           :data {:pitch nil
                  :name nil
                  :notes #{}
                  :midis #{}}}))

(defn ->output-piano-node []
  (->node {:type :output-piano}))

(defn ->output-debug-node []
  (->node {:type :output-debug}))

