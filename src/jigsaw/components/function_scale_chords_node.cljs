(ns jigsaw.components.function-scale-chords-node
  (:require
   [clojure.string :as s]
   [re-frame.core :as re-frame]
   [jigsaw.algo :as algo]
   [jigsaw.subs :as subs]
   [jigsaw.events :as events]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.node :refer [node]]))

(defn function-scale-chords-node [{:keys [id]}]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming id])
        data (re-frame/subscribe [::subs/data id])]
    [node {:title "Scale Chords"
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if-let [incoming-node (first @incoming-nodes)]
       (if (= :input-scale (:type incoming-node))
         (let [selected-chord (:selected-chord @data)
               incoming-data (:data incoming-node)
               chords-list (algo/scale-chords incoming-data :exact? true)
               pitches (:pitches incoming-data)
               pitch->chord-list (zipmap pitches chords-list)]
           [select {:class "text-xl"
                    :value (or selected-chord "")
                    :on-change (fn [e]
                                 (let [value (-> e .-target .-value)
                                       [pitch name] (map keyword (s/split value #"_"))]
                                   (re-frame/dispatch [::events/set-selected-chord id value])
                                   (re-frame/dispatch [::events/set-pitch id pitch])
                                   (re-frame/dispatch [::events/set-name id name])))}
            (cons
             [:option {:disabled true :value ""} "(Select Chord)"]
             (for [pitch pitches
                   :let [chord-list (get pitch->chord-list pitch)]]
               [:optgroup {:label (name pitch)}
                (for [chord chord-list
                      :when (some? chord)]
                  ^{:key chord} [:option {:value (str (name pitch) "_" (name chord))} (str (name pitch) (name chord))])]))])
         [:p "Input is not a scale"])
       [:p "No input"])]))
