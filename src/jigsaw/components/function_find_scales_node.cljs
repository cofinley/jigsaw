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
         (let [selected-degree (:selected-degree @data)]
           (when (not (:scale-shapes @data))
             (re-frame/dispatch [::events/compute-with-loading id
                                 (fn [_db]
                                   (let [scale-shapes (search/chord->scales @parent-data :degree selected-degree)]
                                     {:scale-shapes scale-shapes}))]))
           [:div {:class "flex flex-col text-xl items-start space-y-4"}
            [:label {:class "space-x-4"}
             [:span {:class "font-semibold"} "Degree"]
             [select {:value selected-degree
                      :on-change (fn [e]
                                   (let [degree (-> e .-target .-value)]
                                     (re-frame/dispatch [::events/compute-with-loading id
                                                         (fn [_db]
                                                           (let [scale-shapes (search/chord->scales @parent-data :degree selected-degree)]
                                                             {:selected-degree (when (not= "" degree) (keyword degree))
                                                              :scale-shapes scale-shapes}))])))}
              (cons [:option {:value ""} "All"]
                    (for [deg (sort-by utils/parse-int specs/degrees)]
                      [:option {:value deg} deg]))]]
            (when-let [scale-shapes (:scale-shapes @data)]
              [table {:ms scale-shapes
                      :row-render {"Tonic" :pitch
                                   "Name" :name
                                   "Chord's Degree" :degree
                                   "Piano" (fn [shape] [piano-preview
                                                        shape
                                                        :parent-notes (:notes @parent-data)])}
                      :row-title-render (fn [shape] (utils/pprint-aliases (specs/scales (:name shape))))
                      :row-selected? (fn [shape] (and
                                                  (= (:pitch @data) (:pitch shape))
                                                  (= (:name @data) (:name shape))))
                      :on-row-click (fn [shape]
                                      (re-frame/dispatch [::events/update-node-data id
                                                          (algo/->shape (assoc shape :note (algo/pitch->note (:pitch shape))))]))}])])
         [:p {:class "text-lg"} "Input is not a chord"])
       [:p {:class "text-lg"} "No input"])]))
