(ns jigsaw.components.function-find-shape-node
  (:require
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

(defn function-find-shape-node [{:keys [id]}]
  (let [data (re-frame/subscribe [::subs/data id])
        parent-data (re-frame/subscribe [::subs/parent-data id])
        resolved-shapes (re-frame/subscribe [::subs/function-result id])]
    [node {:title "Find Closest Shapes"
           :id id
           :data @data
           :parent-data @parent-data
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if @parent-data
       (if-let [notes (seq (get-in @parent-data [:notes]))]
         (let [incoming-shape-type (cond
                                     (contains? @parent-data :degrees) :scale
                                     (contains? @parent-data :intervals) :chord
                                     :else :notes)
               selected-shape-type (or (:selected-shape-type @data) (if (= :chord incoming-shape-type) :scale :chord))
               selected-pitch (or (:selected-pitch @data) "")
               heuristic (or (:heuristic @data) :overlap)
               max-shapes (or (:max-shapes @data) 10)]
           [:div {:class "flex flex-col space-y-2 items-start text-xl"}
            [:label {:class "space-x-4"}
             [:span {:class "font-semibold"} "Find"]
             [select {:class "w-max"
                      :on-change #(re-frame/dispatch [::events/update-node-data id {:selected-shape-type (keyword (-> % .-target .-value))}])
                      :value (or selected-shape-type "")}
              [[:option {:value :chord} "Chords"]
               [:option {:value :scale} "Scales"]]]]
            [:label {:class "space-x-4 inline-flex items-baseline"}
             [:span {:class "font-semibold"} "Where"]
             [:span "input"]
             [select {:class "w-max"
                      :on-change #(re-frame/dispatch [::events/update-node-data id {:heuristic (keyword (-> % .-target .-value))}])
                      :value heuristic}
              (for [[value label] search/heuristic-labels]
                [:option {:value value} label])]
             [:span (str "the " (name selected-shape-type))]]
            [:label {:class "space-x-4"}
             [:span {:class "font-semibold"} (if (= selected-shape-type :chord) "Root" "Tonic")]
             [select {:class "w-max"
                      :on-change #(re-frame/dispatch [::events/update-node-data id {:selected-pitch (keyword (-> % .-target .-value))}])
                      :value selected-pitch}
              (cons [:option {:value "all"} "(Show all)"]
                    (for [pitch specs/simple-pitch-keys]
                      [:option {:value pitch} (name pitch)]))]]
            [:label {:class "space-x-4"}
             [:span {:class "font-semibold"} "Max shapes"]
             [:input {:class "p-1 rounded-md border-2 border-gray-400 nodrag"
                      :type "number"
                      :size 2
                      :value max-shapes
                      :on-change #(re-frame/dispatch [::events/update-node-data id {:max-shapes (int (-> % .-target .-value))}])}]]
            (when @resolved-shapes
              [table {:ms @resolved-shapes
                      :row-render {(if (= selected-shape-type :chord) "Root" "Tonic") :pitch
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
         [:p {:class "text-lg"} "No notes in input"])
       [:p {:class "text-lg"} "No input"])]))
