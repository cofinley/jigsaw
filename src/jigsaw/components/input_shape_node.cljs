(ns jigsaw.components.input-shape-node
  (:require
   [clojure.string :as s]
   [jigsaw.components.node :refer [node]]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.events :as events]
   [jigsaw.spec :as specs]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]
   [reagent.core :as r]))

;; TODO: allow changing shape
(defn input-shape-node [{:keys [id data type]}]
  (let [shape-type (if (= :input-chord (keyword type)) :chord :scale)
        shapes (into [] (map #(assoc (utils/strip-ns (second %)) :name (first %)))
                     (if (= shape-type :chord) specs/chords specs/scales))
        title (if (= shape-type :chord) "Chord" "Scale")]
    (fn [{:keys [id data type]}]
      (let [data (:data (events/js-node->clj-node {:data data}))
            search (or (:search data) "")]
        [node {:title title
               :id id
               :data data
               :handles [{:type "source" :position "right"}]}
         [:div {:class "flex flex-col text-xl items-start space-y-4"}
          ;; Pitches
          [:label {:class "space-x-4"}
           [:span (if (= shape-type :chord) "Root" "Tonic")]
           [select {:value (or (:pitch data) "")
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
          ;; TODO: flush data to app-db only on blur, maybe xyflow.useOnSelectionChange ?
          [:label {:class "space-x-4"}
           [:span "Search"]
           [:input {:class "p-1 rounded-md border border-gray-400 nodrag text-black"
                    :value search
                    :on-change #(re-frame/dispatch [::events/update-node-data id {:search (-> % .-target .-value)}])}]]
          ;; Shape names
          [:p title]
          [table {:ms shapes
                  :row-render {"Name" :name
                               "Intervals" (fn [shape] (s/join " " (map name (:intervals shape))))}
                  :row-title-render utils/pprint-aliases
                  :row-selected? (fn [shape] (= (:name data) (:name shape)))
                  :row-filter (fn [shape] (s/includes? (name (:name shape)) search))
                  :on-row-click (fn [shape]
                                  (re-frame/dispatch [::events/update-node-data id shape])
                                  (re-frame/dispatch [::events/calculate-shape id]))}]]]))))
