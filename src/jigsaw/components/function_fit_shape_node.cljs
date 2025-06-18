(ns jigsaw.components.function-fit-shape-node
  (:require
   [jigsaw.algo :as algo]
   [jigsaw.components.node :refer [node]]
   [jigsaw.components.output-piano-node :refer [piano-preview]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.events :as events]
   [jigsaw.search :as search]
   [jigsaw.subs :as subs]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]))

(defn function-fit-shape-node [{:keys [id]}]
  (let [data (re-frame/subscribe [::subs/data id])
        parent-data (re-frame/subscribe [::subs/multi-parent-data id])]
    [node {:title "Fit Notes to Shape"
           :id id
           :data @data
           :parent-data @parent-data
           :handles [{:type "target" :id "target-shape" :position "left" :style {:top "10%"}}
                     {:type "target" :id "candidate-shape" :position "left"}
                     {:type "source" :position "right"}]}
     (if (= (count @parent-data) 2)
       (let [target-shape (first (filter #(contains? % :name) @parent-data))
             candidate-input (first (filter #(not= % target-shape) @parent-data))
             max-shapes (or (:max-shapes @data) 1)
             shapes (search/fit target-shape (:notes candidate-input) :max-shapes max-shapes)
             resolved-shapes (map #(merge % (algo/->shape (algo/pitch->note (:pitch %)) (:name %))) shapes)]
         [:div {:class "flex flex-col space-y-2 items-start text-xl"}
          [:label {:class "space-x-4"}
           [:span {:class "font-semibold"} "Max shapes"]
           [:input {:class "p-1 rounded-md border border-gray-400 nodrag text-black"
                    :type "number"
                    :size 2
                    :value max-shapes
                    :on-change #(re-frame/dispatch [::events/update-node-data id {:max-shapes (int (-> % .-target .-value))}])}]]
          [table {:ms resolved-shapes
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
                                  (re-frame/dispatch [::events/update-node-data id shape]))}]])
       [:p {:class "text-lg"} "Need two inputs"])]))
