(ns jigsaw.ui.components.input-shape-node
  (:require
   [clojure.string :as s]
   [jigsaw.core :as jig]
   [jigsaw.impl.theory :as theory]
   [jigsaw.ui.components.node :refer [node]]
   [jigsaw.ui.components.output-piano-node :refer [piano-preview]]
   [jigsaw.ui.components.select :refer [select]]
   [jigsaw.ui.components.table :refer [table]]
   [jigsaw.ui.events :as events]
   [jigsaw.ui.subs :as subs]
   [jigsaw.utils :as utils]
   [reagent.core :as r]
   [re-frame.core :as re-frame]))

(defn input-shape-node [{:keys [id type]}]
  (let [data (re-frame/subscribe [::subs/data id])
        shape-type (if (= :input-chord (keyword type)) :chord :scale)
        shapes (map #(assoc (second %) :name (first %))
                    (if (= shape-type :chord) theory/chords theory/scales))
        title (if (= shape-type :chord) "Chord" "Scale")
        search (r/atom "")]
    (fn [{:keys [id]}]
      [node {:title title
             :id id
             :data @data
             :handles [{:type "source" :position "right"}]}
       [:div {:class "flex flex-col text-xl items-start space-y-4"}
        ;; Starting pitch
        [:label {:class "space-x-4"}
         [:span {:class "font-semibold"} (if (= shape-type :chord) "Root" "Tonic")]
         [select {:value (or (:pitch @data) "")
                  :on-change (fn [e]
                               (let [pitch (keyword (-> e .-target .-value))]
                                 (re-frame/dispatch [::events/update-node-data id {:pitch pitch}])
                                 (re-frame/dispatch [::events/calculate-shape id])))
                  :placeholder "Pitch"}
          (cons
           [:option {:disabled true :value ""} "Pitch"]
           (for [pitch theory/simple-pitch-keys]
             [:option {:value pitch} (name pitch)]))]]
        [:label {:class "space-x-4"}
         [:span {:class "font-semibold"} "Search"]
         [:input {:class "p-1 rounded-md border-2 border-gray-400 nodrag"
                  :on-change #(reset! search (-> % .-target .-value))}]]
        ;; Shape names
        [table {:ms (cond->> shapes
                      (some? (:pitch @data)) (map #(jig/->shape (theory/pitch->note (:pitch @data)) (:name %))))
                :row-render {"Name" :name
                             "Intervals" (fn [shape] (s/join " " (map name (:intervals shape))))
                             "Piano" (fn [shape]
                                       (when (and (:pitch shape) (:name shape))
                                         [piano-preview shape]))}
                :row-title-render utils/pprint-aliases
                :row-selected? (fn [shape] (= (:name @data) (:name shape)))
                :row-filter (fn [shape] (if (> (count @search) 0) (s/includes? (name (:name shape)) @search) true))
                :on-row-click (fn [shape]
                                (re-frame/dispatch [::events/update-node-data id shape]))}]]])))
