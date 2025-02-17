(ns jigsaw.components.input-shape-node
  (:require
   [clojure.string :as s]
   [re-frame.core :as re-frame]
   [jigsaw.spec :as specs]
   [jigsaw.events :as events]
   [jigsaw.utils :as utils]
   [jigsaw.subs :as subs]
   [jigsaw.components.node :refer [node]]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.components.output-piano-node :refer [output-piano-view piano-preview]]
   [reagent.core :as r]
   [jigsaw.algo :as algo]))

(defn input-shape-node [{:keys [id type]}]
  (let [data (re-frame/subscribe [::subs/data id])
        shape-type (if (= :input-chord (keyword type)) :chord :scale)
        shapes (into [] (map #(assoc (second %) :name (first %)))
                     (if (= shape-type :chord) specs/chords specs/scales))
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
         [:span (if (= shape-type :chord) "Root" "Tonic")]
         [select {:value (or (:pitch @data) "")
                  :on-change (fn [e]
                               (let [pitch (keyword (-> e .-target .-value))]
                                 (re-frame/dispatch [::events/update-node-data id {:pitch pitch}])
                                 (re-frame/dispatch [::events/calculate-shape id])))
                  :placeholder "Pitch"}
          (cons
           [:option {:disabled true :value ""} "Pitch"]
           (for [pitch (keys (sort-by val < specs/pitches))
                 :when (and (not (s/includes? (name pitch) "bb")) (not (s/includes? (name pitch) "##")))]
             [:option {:value pitch} (name pitch)]))]]
        [:label {:class "space-x-4"}
         [:span "Search"]
         [:input {:class "p-1 rounded-md border border-gray-400 nodrag text-black"
                  :on-change #(reset! search (-> % .-target .-value))}]]
        ;; Shape names
        [:p title]
        [table {:ms (cond->> shapes
                      (some? (:pitch @data)) (map #(algo/resolve-shape (algo/pitch->note (:pitch @data)) shape-type (:name %))))
                :row-render {"Name" :name
                             "Intervals" (fn [shape] (s/join " " (map name (:intervals shape))))
                             ; "Piano" (fn [shape] [output-piano-view {:data shape :key-width 20 :display-label-options? false}])
                             "Piano" (fn [shape] [piano-preview (:notes shape)])}
                :row-title-render utils/pprint-aliases
                :row-selected? (fn [shape] (= (:name @data) (:name shape)))
                :row-filter (fn [shape] (if (> (count @search) 0) (s/includes? (name (:name shape)) @search) true))
                :on-row-click (fn [shape]
                                (re-frame/dispatch [::events/update-node-data id shape])
                                (re-frame/dispatch [::events/calculate-shape id]))}]]])))
