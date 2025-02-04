(ns jigsaw.components.function-find-chords-node
  (:require
   [jigsaw.algo :as algo]
   [jigsaw.components.node :refer [node]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.events :as events]
   [jigsaw.search :as search]
   [jigsaw.spec :as specs]
   [jigsaw.subs :as subs]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]))

(defn function-find-chords-node [{:keys [id]}]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming id])
        data (re-frame/subscribe [::subs/data id])]
    [node {:title "Scale Chords"
           :id id
           :data @data
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if-let [incoming-data (first @incoming-nodes)]
       (if (contains? incoming-data :degrees)
         (let [num-thirds (or (:num-thirds @data) 3)
               chords (search/scale->chords incoming-data :num-thirds num-thirds)
               pitches (:pitches incoming-data)
               pitch->chord (zipmap pitches chords)
               chord-shapes (map (fn [[pitch chord-name]]
                                   {:pitch pitch
                                    :name chord-name
                                    :aliases (:aliases (utils/strip-ns (specs/chords chord-name)))})
                                 pitch->chord)
               pitch->degrees (zipmap pitches (:degrees incoming-data))]
           [:div {:class "flex flex-col text-xl items-start space-y-4"}
            [:label {:class "space-x-4"}
             [:span "Thirds"]
             [:input {:type "number"
                      :class "p-1 rounded-md border border-gray-400 nodrag text-black"
                      :size 2
                      :value num-thirds
                      :on-change #(re-frame/dispatch [::events/update-node-data id {:num-thirds (-> % .-target .-value int)}])}]]
            [:p "Chord"]
            [table {:ms chord-shapes
                    :row-render {"Root" :pitch
                                 "Name" :name
                                 "Degree" #(algo/degree-chord->roman-numeral
                                            (get pitch->degrees (:pitch %))
                                            (:name %))}
                    :row-title-render utils/pprint-aliases
                    :row-selected? (fn [shape] (and (= (:pitch @data) (:pitch shape)) (= (:name @data) (:name shape))))
                    :on-row-click (fn [shape]
                                    (re-frame/dispatch [::events/update-node-data id shape])
                                    (re-frame/dispatch [::events/calculate-shape id]))}]])
         [:p "Input is not a scale"])
       [:p "No input"])]))
