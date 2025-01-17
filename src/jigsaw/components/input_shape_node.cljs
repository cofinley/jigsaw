(ns jigsaw.components.input-shape-node
  (:require
   [clojure.string :as s]
   [re-frame.core :as re-frame]
   [jigsaw.spec :as specs]
   [jigsaw.events :as events]
   [jigsaw.utils :as utils]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.components.node :refer [node]]))

(defn input-shape-node [{:keys [id data type]}]
  (let [shape-type (if (= :input-chord (keyword type)) :chord :scale)
        shapes (into [] (map #(assoc (utils/strip-ns (second %)) :name (first %)))
                     (if (= shape-type :chord) specs/chords specs/scales))
        title (if (= shape-type :chord) "Chord" "Scale")]
    (fn [{:keys [id data]}]
      (let [data (:data (events/js-node->clj-node {:data data}))]
        [node {:title title
               :id id
               :data data
               :handles [{:type "source" :position "right"}]}
         [:div {:class "flex flex-col text-xl items-start space-y-4"}
          ;; Pitches
          [:label {:class "space-x-4"}
           [:span (if (= shape-type :chord) "Root" "Tonic")]
           [select {:value (or (:pitch data) "")
                    :on-change #(re-frame/dispatch [::events/set-pitch id (keyword (-> % .-target .-value))])
                    :placeholder "Pitch"}
            (cons
             [:option {:disabled true :value ""} "Pitch"]
             (for [pitch (keys (sort-by val < specs/pitches))
                   :when (and (not (s/includes? (name pitch) "bb")) (not (s/includes? (name pitch) "##")))]
               [:option {:value pitch} (name pitch)]))]]
          ;; Shape names
          [:p title]
          [table {:ms shapes
                  :row-render {"Name" :name
                               "Intervals" (fn [shape] (s/join " " (map name (:intervals shape))))}
                  :row-title-render utils/pprint-aliases
                  :row= (fn [shape] (= (:name data) (:name shape)))
                  :on-row-click (fn [shape] (re-frame/dispatch [::events/set-name id (:name shape)]))}]
          (comment [:label {:class "space-x-2"}
                    [:span title]
                    [select {:value (or (:name data) "")
                             :class "text-black"
                             :on-change #(re-frame/dispatch [::events/set-name id (keyword (-> % .-target .-value))])
                             :placeholder (str title "Name")}
                     (cons [:option {:disabled true :value ""} title]
                           (for [[shape-name details] shapes
                                 :let [aliases (::specs/aliases details)]]
                             [:option {:value shape-name
                                       :title (when (seq aliases) (str "Aliases:\n" (s/join "\n" (map #(str "- " %) aliases))))}
                              (name shape-name)]))]])]]))))
