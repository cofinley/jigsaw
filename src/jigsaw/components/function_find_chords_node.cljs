(ns jigsaw.components.function-find-chords-node
  (:require
   [jigsaw.algo :as algo]
   [jigsaw.components.node :refer [node]]
   [jigsaw.components.output-piano-node :refer [piano-preview]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.events :as events]
   [jigsaw.spec :as specs]
   [jigsaw.subs :as subs]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]))

(defn function-find-chords-node [{:keys [id]}]
  (let [data (re-frame/subscribe [::subs/data id])
        parent-data (re-frame/subscribe [::subs/parent-data id])
        chords (re-frame/subscribe [::subs/function-result id])]
    [node {:title "Chords (from scale)"
           :id id
           :data @data
           :parent-data @parent-data
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if @parent-data
       (if (specs/scale? @parent-data)
         [:div {:class "flex flex-col text-xl items-start space-y-4"}
          (when @chords
            [table {:ms @chords
                    :row-render {"Root" :pitch
                                 "Name" :name
                                 "Degree" :degree
                                 "Piano" (fn [shape]
                                           [piano-preview
                                            shape
                                            :parent-notes (:notes @parent-data)])}
                    :row-title-render utils/pprint-aliases
                    :row-selected? (fn [shape] (and (= (:pitch @data) (:pitch shape))
                                                    (= (:name @data) (:name shape))))
                    :on-row-click (fn [shape]
                                    (re-frame/dispatch [::events/update-node-data id
                                                        (algo/->shape (assoc shape :note (algo/pitch->note (:pitch shape))))]))}])]
         [:p {:class "text-lg"} "Input is not a scale"])
       [:p {:class "text-lg"} "No input"])]))
