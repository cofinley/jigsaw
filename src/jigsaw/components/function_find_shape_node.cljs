(ns jigsaw.components.function-find-shape-node
  (:require
   [clojure.string :as s]
   [re-frame.core :as re-frame]
   [jigsaw.search :as search]
   [jigsaw.subs :as subs]
   [jigsaw.events :as events]
   [jigsaw.utils :as utils]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.components.node :refer [node]]))

;; TODO: allow click-and-drag of table row into new input-shape node
(defn function-find-shape-node [{:keys [id data]}]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming id])
        data (:data (events/js-node->clj-node {:data data}))]
    [node {:title "Compatible Shapes"
           :id id
           :data data
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if-let [incoming-data (first @incoming-nodes)]
       (if-let [notes (seq (get-in incoming-data [:notes]))]
         (let [incoming-shape-type (cond
                                     (contains? incoming-data :degrees) :scale
                                     (contains? incoming-data :intervals) :chord ; Scales have intervals too, but we didn't find :degrees
                                     :else :notes)
               ; Recommend finding scales by default if incoming shape is a chord, otherwise find chords
               selected-shape-type (or (:selected-shape-type data) (if (= :chord incoming-shape-type) :scale :chord))
               heuristic (or (:heuristic data) :overlap)
               max-shapes (or (:max-shapes data) 10)]
           [:div {:class "flex flex-col space-y-2 items-start"}
            [:div {:class "flex items-center space-x-4"}
             [:label {:class "space-x-2"}
              [:span "Find"]
              [select {:class "w-max"
                       :on-change #(re-frame/dispatch [::events/update-node-data id {:selected-shape-type (keyword (-> % .-target .-value))}])
                       :value (or selected-shape-type "")}
               [[:option {:value :chord} "Chords"]
                [:option {:value :scale} "Scales"]]]]
             [:label {:class "space-x-2"}
              [:span "Heuristic"]
              [select {:class "w-max"
                       :on-change #(re-frame/dispatch [::events/update-node-data id {:heuristic (keyword (-> % .-target .-value))}])
                       :value heuristic}
               (for [heuristic-type (keys search/heuristics)]
                 [:option {:value heuristic-type} (s/replace (name heuristic-type) #"-" " ")])]]
             [:label {:class "space-x-2"}
              [:span "Max shapes"]
              [:input {:class "p-1 rounded-md border border-gray-400 nodrag text-black"
                       :type "number"
                       :size 2
                       :value max-shapes
                       :on-change #(re-frame/dispatch [::events/update-node-data id {:max-shapes (int (-> % .-target .-value))}])}]]]
            ;; TODO: find chord from scale - either combine with scale-chords node or this needs to not look for a chord with all scale notes (i.e. look for subsets)
            ;; TODO: make search an event and update data with results, otherwise it blocks event loop/animation
            (let [shapes (search/notes->shapes notes
                                               selected-shape-type
                                               :max-shapes max-shapes
                                               :heuristic (keyword heuristic))]
              [table {:ms shapes
                      :row-render {(if (= selected-shape-type :chord) "Root" "Tonic") :pitch
                                   "Name" :name
                                   "Overlap" #(str (int (* 100 (get-in % [:heuristics :overlap]))) "%")}
                      :row-title-render utils/pprint-aliases
                      :row-selected? (fn [shape] (and (= (:pitch data) (:pitch shape)) (= (:name data) (:name shape))))
                      :on-row-click (fn [shape]
                                      (re-frame/dispatch [::events/update-node-data id shape]))}])])
         [:p "No notes in input"])
       [:p "No input"])]))
