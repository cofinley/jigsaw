(ns jigsaw.components.function-find-scales-node
  (:require
   [jigsaw.algo :as algo]
   [jigsaw.components.node :refer [node]]
   [jigsaw.components.output-piano-node :refer [piano-preview]]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.events :as events]
   [jigsaw.search :as search]
   [jigsaw.spec :as specs]
   [jigsaw.subs :as subs]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]))

(defn function-find-scales-node [{:keys [id]}]
  (let [data (re-frame/subscribe [::subs/data id])
        parent-data (re-frame/subscribe [::subs/parent-data id])]
    [node {:title "Chord Scales"
           :id id
           :data @data
           :parent-data @parent-data
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if @parent-data
       (if (contains? @parent-data :intervals)
         (let [selected-degree (:selected-degree @data)
               scales (search/chord->scales @parent-data :degree selected-degree)]
           [:div {:class "flex flex-col text-xl items-start space-y-4"}
            [:label {:class "space-x-4"}
             [:span {:class "font-semibold"} "Degree"]
             [select {:value selected-degree
                      :on-change #(re-frame/dispatch [::events/update-node-data id {:selected-degree (let [value (-> % .-target .-value)]
                                                                                                       (when (not= "" value) (keyword value)))}])}
              (cons [:option {:value ""} "All"]
                    (for [deg (sort-by utils/parse-int specs/degrees)]
                      [:option {:value deg} deg]))]]
            [table {:ms scales
                    :row-render {"Tonic" :pitch
                                 "Name" :name
                                 "Chord's Degree" #(algo/degree-chord->roman-numeral
                                                    (:degree %)
                                                    (:name @parent-data))
                                 "Piano" (fn [shape] [piano-preview
                                                      (:notes (algo/resolve-shape (algo/pitch->note (:pitch shape)) :scale (:name shape)))
                                                      :parent-notes (:notes @parent-data)])}
                    :row-title-render (fn [shape] (utils/pprint-aliases (specs/scales (:name shape))))
                    :row-selected? (fn [shape] (and
                                                (= (:pitch @data) (:pitch shape))
                                                (= (:name @data) (:name shape))
                                                (= (:degree @data) (:degree shape))))
                    :on-row-click (fn [shape]
                                    (re-frame/dispatch [::events/update-node-data id
                                                        (merge shape (algo/resolve-shape (algo/pitch->note (:pitch shape)) :scale (:name shape)))])
                                    (re-frame/dispatch [::events/calculate-shape id]))}]])
         [:p "Input is not a chord"])
       [:p "No input"])]))
