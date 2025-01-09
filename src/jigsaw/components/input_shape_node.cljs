(ns jigsaw.components.input-shape-node
  (:require
   [clojure.string :as s]
   [re-frame.core :as re-frame]
   [jigsaw.spec :as specs]
   [jigsaw.subs :as subs]
   [jigsaw.events :as events]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.node :refer [node]]))

(defn input-shape-node [{:keys [id type]}]
  (let [data (re-frame/subscribe [::subs/data id])
        shape-type (if (= :input-chord (keyword type)) :chord :scale)
        title (if (= shape-type :chord) "Chord" "Scale")
        shapes (if (= shape-type :chord) specs/chords specs/scales)]
    [node {:title title :handle {:type "source" :position "right"}}
     ;; Pitches
     [:div {:class "flex flex-col text-xl items-start space-y-4"}
      [:label {:class "space-x-4"}
       [:span "Pitch"]
       [select {:value (or (:pitch @data) "")
                :class "text-black"
                :on-change #(re-frame/dispatch [::events/set-pitch id (keyword (-> % .-target .-value))])
                :placeholder "Pitch"}
        (cons
         [:option {:disabled true :value ""} "Pitch"]
         (for [pitch (keys (sort-by val < specs/pitches))
               :when (and (not (s/includes? (name pitch) "bb")) (not (s/includes? (name pitch) "##")))]
           [:option {:value pitch} (name pitch)]))]]
       ;; Shape names
      [:label {:class "space-x-2"}
       [:span title]
       [select {:value (or (:name @data) "")
                :class "text-black"
                :on-change #(re-frame/dispatch [::events/set-name id (keyword (-> % .-target .-value))])
                :placeholder (str title "Name")}
        (cons [:option {:disabled true :value ""} title]
              (for [[shape-name details] shapes
                    :let [aliases (::specs/aliases details)]]
                [:option {:value shape-name
                          :title (when (seq aliases) (str "Aliases:\n" (s/join "\n" (map #(str "- " %) aliases))))}
                 (name shape-name)]))]]]]))
