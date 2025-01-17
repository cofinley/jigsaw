(ns jigsaw.components.function-find-shape-node
  (:require
   [re-frame.core :as re-frame]
   [jigsaw.search :as search]
   [jigsaw.subs :as subs]
   [jigsaw.events :as events]
   [jigsaw.utils :as utils]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.components.node :refer [node]]))

(defn function-find-shape-node [{:keys [id data]}]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming id])
        data (:data (events/js-node->clj-node {:data data}))]
    [node {:title "Find Compatible Shapes"
           :id id
           :data data
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if-let [incoming-data (first @incoming-nodes)]
       (if-let [notes (seq (get-in incoming-data [:notes]))]
         (let [selected-shape-type (or (:selected-shape-type data) :chord)
               similarity-type (or (:similarity-type data) :overlap)]
           [:div {:class "flex flex-col space-y-2"}
            [select {:class "w-max"
                     :on-change #(re-frame/dispatch [::events/update-node-data id {:selected-shape-type (keyword (-> % .-target .-value))}])
                     :value (or selected-shape-type "")}
             [[:option {:value :chord} "Chord"]
              [:option {:value :scale} "Scale"]]]
            ;; TODO: find chord from scale - either combine with scale-chords node or this needs to not look for a chord with all scale notes (i.e. look for subsets)
            (let [shapes (search/notes->shapes notes selected-shape-type similarity-type)]
              [table {:ms shapes
                      :row-render {(if (= selected-shape-type :chord) "Root" "Tonic") :pitch
                                   "Name" :name
                                   "Similarity" #(str (int (* 100 (:similarity %))) "%")}
                      :row-title-render utils/pprint-aliases
                      :row-selected? (fn [shape] (and (= (:pitch data) (:pitch shape)) (= (:name data) (:name shape))))
                      :on-row-click (fn [shape]
                                      (re-frame/dispatch [::events/update-node-data id shape]))}])])
         [:p "No notes in input"])
       [:p "No input"])]))
