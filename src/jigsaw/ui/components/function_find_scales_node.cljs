(ns jigsaw.ui.components.function-find-scales-node
  (:require
   [jigsaw.core :as jigsaw]
   [jigsaw.impl.theory :as theory]
   [jigsaw.ui.components.node :refer [node]]
   [jigsaw.ui.components.output-piano-node :refer [piano-preview]]
   [jigsaw.ui.components.select :refer [select]]
   [jigsaw.ui.components.table :refer [table]]
   [jigsaw.ui.events :as events]
   [jigsaw.ui.subs :as subs]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]))

(defn function-find-scales-node [{:keys [id]}]
  (let [data (re-frame/subscribe [::subs/data id])
        parent-data (re-frame/subscribe [::subs/parent-data id])
        scales (re-frame/subscribe [::subs/function-result id])]
    [node {:title "Scales (from chord)"
           :id id
           :data @data
           :parent-data @parent-data
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if @parent-data
       (if (theory/chord? @parent-data)
         (let [selected-degree (:selected-degree @data)]
           [:div {:class "flex flex-col items-start space-y-4"}
            [:label {:class "space-x-4"}
             [:span {:class "font-semibold"} "Degree"]
             [select {:value selected-degree
                      :on-change (fn [e]
                                   (let [degree (-> e .-target .-value)]
                                     (re-frame/dispatch [::events/update-node-data id
                                                         {:selected-degree (when (not= "" degree) (js/parseInt degree))}])))}
              (cons [:option {:value ""} "All"]
                    (for [deg (range 1 8)]
                      [:option {:value deg} deg]))]]
            (when @scales
              [table {:ms @scales
                      :row-render {"Tonic" :pitch
                                   "Name" :name
                                   "Chord's Degree" :context
                                   "Piano" (fn [shape]
                                             [piano-preview
                                              shape
                                              :parent-notes (:notes @parent-data)])}
                      :row-title-render utils/pprint-aliases
                      :row-selected? (fn [shape] (and
                                                  (= (:pitch @data) (:pitch shape))
                                                  (= (:name @data) (:name shape))))
                      :on-row-click (fn [shape]
                                      (re-frame/dispatch [::events/update-node-data id
                                                          (jigsaw/->shape (assoc shape :note (theory/pitch->note (:pitch shape))))]))}])])
         [:p "Input is not a chord"])
       [:p "No input"])]))
