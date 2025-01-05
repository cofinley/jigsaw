(ns jigsaw.components.input-piano-node
  (:require
   [reagent.core :as r]
   [re-frame.core :as re-frame]
   [jigsaw.algo :as algo]
   [jigsaw.subs :as subs]
   [jigsaw.events :as events]
   [jigsaw.components.node :refer [node handle]]
   ["react-piano" :refer [ControlledPiano]]))

(def key-width 30)

(defn input-piano-node [props _]
  (let [id (:id props)
        active-notes (re-frame/subscribe [::subs/active-notes id])]
    [node {:title "Piano"}
     [:div {:class "nodrag"}
      (let [first-midi 60
            octaves 2
            last-midi (dec (+ first-midi (* octaves 12)))
            width (* key-width (- last-midi first-midi))]
        [:> ControlledPiano
         {:class "nodrag"
          :noteRange {:first first-midi :last last-midi}
          :playNote (fn [midi] midi)
          :stopNote #()
          :activeNotes (map (comp algo/note->midi keyword) @active-notes)
          :onPlayNoteInput (fn [midi _] (re-frame/dispatch [::events/toggle-note id midi]))
          :onStopNoteInput #()
          :width width}])]
     [handle {:type "source" :position "right"}]]))

