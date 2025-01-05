(ns jigsaw.db)

(def default-db
  {:nodes {"1" {:id "1"
                :position {:x 0 :y 0}
                :type :input-chord
                :data {:pitch :Eb :name :m}}
           "2" {:id "2"
                :position {:x 400 :y 0}
                :type :output-piano
                :data {}}
           "3" {:id "3"
                :position {:x 0 :y 300}
                :type :input-scale
                :data {:pitch :Eb :name :mixolydian}}
           "4" {:id "4"
                :position {:x 400 :y 300}
                :type :output-piano
                :data {}}}
   :edges {"1->2" {:id "1->2"
                   :source "1"
                   :target "2"}
           "3->4" {:id "3->4"
                   :source "3"
                   :target "4"}}})

(defn ->node [props]
  (merge
   {:id (str (random-uuid))
    :type "default"
    :position {:x 200 :y 100}
    :data {}}
   props))

(defn ->input-piano-node []
  (->node {:type :input-piano
           :data {:notes #{}}}))

(defn ->input-chord-node []
  (->node {:type :input-chord
           :data {:pitch nil
                  :name nil
                  :notes #{}}}))

(defn ->input-scale-node []
  (->node {:type :input-scale
           :data {:pitch nil
                  :name nil
                  :notes #{}}}))

(defn ->output-piano-node []
  (->node {:type :output-piano}))

(defn ->output-music-staff-node []
  (->node {:type :output-music-staff}))

(defn ->output-debug-node []
  (->node {:type :output-debug}))

