(ns jigsaw.components.function-find-chords-node
  (:require
   [jigsaw.algo :as algo]
   [jigsaw.components.node :refer [node]]
   [jigsaw.components.output-piano-node :refer [piano-preview]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.events :as events]
   [jigsaw.search :as search]
   [jigsaw.spec :as specs]
   [jigsaw.subs :as subs]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]))

(defn function-find-chords-node [{:keys [id]}]
  (let [data (re-frame/subscribe [::subs/data id])
        parent-data (re-frame/subscribe [::subs/parent-data id])]
    [node {:title "Scale Chords"
           :id id
           :data @data
           :parent-data @parent-data
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if @parent-data
       (if (contains? @parent-data :degrees)
         (let [num-thirds (or (:num-thirds @data) 3)
               shape-refs (search/scale->chords @parent-data :num-thirds num-thirds)
               chord-shapes (map (fn [shape]
                                   (assoc shape
                                          :aliases (:aliases (specs/chords (:name shape)))))
                                 shape-refs)]
           [:div {:class "flex flex-col text-xl items-start space-y-4"}
            [:label {:class "space-x-4"}
             [:span {:class "font-semibold"} "Thirds"]
             [:input {:type "number"
                      :class "p-1 rounded-md border border-gray-400 nodrag text-black"
                      :size 2
                      :value num-thirds
                      :on-change #(re-frame/dispatch [::events/update-node-data id {:num-thirds (-> % .-target .-value int)}])}]]
            [table {:ms chord-shapes
                    :row-render {"Root" :pitch
                                 "Name" :name
                                 "Degree" :degree
                                 "Piano" (fn [shape]
                                           (when (:name shape)
                                             [piano-preview shape]))}
                    :row-title-render utils/pprint-aliases
                    :row-selected? (fn [shape] (and (= (:pitch @data) (:pitch shape))
                                                    (= (:name @data) (:name shape))))
                    :on-row-click (fn [shape]
                                    (re-frame/dispatch [::events/update-node-data id
                                                        (algo/->shape (assoc shape :note (algo/pitch->note (:pitch shape))))]))}]])
         [:p "Input is not a scale"])
       [:p "No input"])]))
