(ns jigsaw.components.input-piano-node
  (:require
   [re-frame.core :as re-frame]
   [jigsaw.algo :as algo]
   [jigsaw.subs :as subs]
   [jigsaw.events :as events]
   [jigsaw.components.node :refer [node]]
   ["react-piano" :refer [ControlledPiano]]))

(def key-width 30)

(defn input-piano-node [{:keys [id]}]
  (let [active-notes (re-frame/subscribe [::subs/active-notes id])]
    [node {:title "Piano" :handles [{:type "source" :position "right"}]}
     [:div {:class "flex flex-col space-y-2"}
      [:button {:class "px-2 py-1 bg-gray-200 hover:bg-gray-100 cursor-pointer text-black rounded border self-end"
                :on-click #(re-frame/dispatch [::events/update-node-data id {:notes []}])}
       "Clear"]
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
           :width width}])]]]))

