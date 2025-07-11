(ns jigsaw.components.function-fit-shape-node
  (:require
   [jigsaw.components.node :refer [node]]
   [jigsaw.components.output-piano-node :refer [piano-preview]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.events :as events]
   [jigsaw.subs :as subs]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]))

(defn function-fit-shape-node [{:keys [id]}]
  (let [data (re-frame/subscribe [::subs/data id])
        parent-data (re-frame/subscribe [::subs/multi-parent-data id])
        fitted-shapes (re-frame/subscribe [::subs/function-result id])]
    [node {:title "Fit Notes to Shape"
           :id id
           :data @data
           :parent-data @parent-data
           :handles [{:type "target" :id "target-shape" :position "left"}
                     {:type "target" :id "candidate-shape" :position "left"}
                     {:type "source" :position "right"}]}
     (if (= (count @parent-data) 2)
       (let [max-shapes (or (:max-shapes @data) 1)]
         [:div {:class "flex flex-col space-y-2 items-start text-xl"}
          [:label {:class "space-x-4"}
           [:span {:class "font-semibold"} "Max shapes"]
           [:input {:class "p-1 rounded-md border-2 border-gray-400 nodrag"
                    :type "number"
                    :size 2
                    :value max-shapes
                    :on-change #(re-frame/dispatch [::events/update-node-data id {:max-shapes (int (-> % .-target .-value))}])}]]
          (when @fitted-shapes
            [table {:ms @fitted-shapes
                    :row-render {"Root" :pitch
                                 "Name" :name
                                 "Overlap" #(str (int (* 100 (get-in % [:heuristics :overlap]))) "%")
                                 "Piano" (fn [shape]
                                           (when (:name shape)
                                             [piano-preview
                                              shape
                                              :parent-notes (:notes @parent-data)]))}
                    :row-title-render utils/pprint-aliases
                    :row-selected? (fn [shape] (and (= (:pitch @data) (:pitch shape)) (= (:name @data) (:name shape))))
                    :on-row-click (fn [shape]
                                    (re-frame/dispatch [::events/update-node-data id shape]))}])])
       [:p {:class "text-lg"} "Need two inputs"])]))
