(ns jigsaw.components.function-scale-chords-node
  (:require
   [re-frame.core :as re-frame]
   [jigsaw.algo :as algo]
   [jigsaw.search :as search]
   [jigsaw.subs :as subs]
   [jigsaw.events :as events]
   [jigsaw.utils :as utils]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.components.node :refer [node]]))

(defn function-scale-chords-node [{:keys [id data]}]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming id])
        data (:data (events/js-node->clj-node {:data data}))]
    [node {:title "Scale Chords"
           :id id
           :data data
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if-let [incoming-data (first @incoming-nodes)]
       (if (contains? incoming-data :degrees)
         (let [selected-match-type (or (:match-type data) :diatonic)
               chords-list (search/scale-chords incoming-data :exact? (= :diatonic selected-match-type))
               pitches (:pitches incoming-data)
               pitch->chord-list (zipmap pitches chords-list)
               chord-shapes (flatten (map (fn [[pitch chord-names]]
                                            (map (fn [name] {:pitch pitch :name name}) chord-names))
                                          pitch->chord-list))
               pitch->degrees (zipmap pitches (:degrees incoming-data))]
           [:div {:class "flex flex-col text-xl items-start space-y-4"}
            [:label {:class "space-x-4"}
             [:span "Match Type"]
             [select {:class "w-max"
                      :value selected-match-type
                      :on-change #(re-frame/dispatch [::events/update-node-data id {:match-type (-> % .-target .-value keyword)}])}
              (map #(vector :option {} (name %)) [:diatonic :subset])]]
            [:p "Chord"]
            [table {:ms chord-shapes
                    :row-render {"Root" :pitch
                                 "Name" :name
                                 "Degree" #(algo/degree-chord->roman-numeral
                                            (get pitch->degrees (:pitch %))
                                            (:name %))}
                    :row-title-render utils/pprint-aliases
                    :row-selected? (fn [shape] (and (= (:pitch data) (:pitch shape)) (= (:name data) (:name shape))))
                    :on-row-click (fn [shape]
                                    (re-frame/dispatch [::events/update-node-data id shape])
                                    (re-frame/dispatch [::events/calculate-shape id]))}]])
         [:p "Input is not a scale"])
       [:p "No input"])]))
