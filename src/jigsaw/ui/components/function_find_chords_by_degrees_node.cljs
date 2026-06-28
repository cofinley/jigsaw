(ns jigsaw.ui.components.function-find-chords-by-degrees-node
  (:require
   [clojure.string :as str]
   [jigsaw.core :as jigsaw]
   [jigsaw.impl.theory :as theory]
   [jigsaw.ui.components.node :refer [node]]
   [jigsaw.ui.components.output-piano-node :refer [piano-preview]]
   [jigsaw.ui.components.table :refer [table]]
   [jigsaw.ui.events :as events]
   [jigsaw.ui.subs :as subs]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]))

(defn function-find-chords-by-degrees-node [{:keys [id]}]
  (let [data (re-frame/subscribe [::subs/data id])
        parent-data (re-frame/subscribe [::subs/parent-data id])
        chords (re-frame/subscribe [::subs/function-result id])]
    [node {:title "Chords (from scale and degrees)"
           :id id
           :data @data
           :parent-data @parent-data
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if @parent-data
       [:div {:class "flex flex-col space-y-2 items-start"}
        [:p {:class "space-x-4"}
         [:span {:class "font-semibold"} "Scale"]
         [:span (str (name (:pitch @parent-data)) " " (name (:name @parent-data)))]]
        [:label {:class "space-x-4"}
         [:span {:class "font-semibold"} "Chord Degrees"]
         [:input {:class "p-1 rounded-md border-2 border-gray-400 nodrag"
                  :placeholder "ii V I ..."
                  :value (or (:chord-degrees-str @data) "")
                  :on-change #(re-frame/dispatch [::events/update-node-data id {:chord-degrees-str (-> % .-target .-value)}])}]]
        (if (theory/scale? @parent-data)
          [:div {:class "flex flex-col items-start space-y-4"}
           (when @chords
             [table {:ms @chords
                     :row-render {"Root" :pitch
                                  "Name" :name
                                  "Degree" :degree
                                  "Piano" (fn [shape]
                                            [piano-preview
                                             shape
                                             :parent-notes (:notes @parent-data)
                                             :show-subtracted? true])}
                     :row-title-render utils/pprint-aliases
                     :row-selected? (fn [shape] (and (= (:pitch @data) (:pitch shape))
                                                     (= (:name @data) (:name shape))))
                     :on-row-click (fn [shape]
                                     (re-frame/dispatch [::events/update-node-data id
                                                         (jigsaw/->shape (assoc shape :note (theory/pitch->note (:pitch shape))))]))}])]
          [:p "Input is not a scale"])]
       [:p "No input"])]))
