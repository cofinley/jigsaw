(ns jigsaw.components.input-shape-node
  (:require
   [clojure.string :as s]
   [reagent.core :as r]
   [re-frame.core :as re-frame]
   [jigsaw.spec :as specs]
   [jigsaw.subs :as subs]
   [jigsaw.events :as events]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.node :refer [node handle]]))

(defn input-shape-node [props _]
  (let [id (:id props)
        shape-type (if (= :input-chord (keyword (:type props))) :chord :scale)
        title (if (= shape-type :chord) "Chord" "Scale")
        data (if (= shape-type :chord) specs/chords specs/scales)
        selected-pitch (re-frame/subscribe [::subs/pitch id])
        selected-name (re-frame/subscribe [::subs/name id])]
    [node {:title title}
       ;; Pitches
     [:div {:class "flex space-x-2 items-center"}
      [:label "Pitch"]
      [select {:value (or @selected-pitch "")
               :class "text-black"
               :on-change #(re-frame/dispatch [::events/set-pitch id (keyword (-> % .-target .-value))])
               :placeholder "Pitch"}
       (cons
        [:option {:disabled true :value ""} "Pitch"]
        (for [pitch (keys (sort-by val < specs/pitches))
              :when (and (not (s/includes? (name pitch) "bb")) (not (s/includes? (name pitch) "##")))]
          [:option {:value pitch} (name pitch)]))]
       ;; Shape names
      [:label title]
      [select {:value (or @selected-name "")
               :class "text-black"
               :on-change #(re-frame/dispatch [::events/set-name id (keyword (-> % .-target .-value))])
               :placeholder (str title "Name")}
       (cons [:option {:disabled true :value ""} title]
             (for [[shape-name details] data
                   :let [aliases (::specs/aliases details)]]
               [:option {:value shape-name
                         :title (when (seq aliases) (str "Aliases:\n" (s/join "\n" (map #(str "- " %) aliases))))}
                (name shape-name)]))]]
     [handle {:type "source" :position "right"}]]))
