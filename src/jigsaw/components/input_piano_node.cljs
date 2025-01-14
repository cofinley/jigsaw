(ns jigsaw.components.input-piano-node
  (:require
   [re-frame.core :as re-frame]
   [jigsaw.algo :as algo]
   [jigsaw.events :as events]
   [jigsaw.components.node :refer [node]]
   ["react-piano" :refer [ControlledPiano]]))

(def key-width 30)

(defn input-piano-node [{:keys [id data]}]
  (let [data (:data (events/js-node->clj-node {:data data}))]
    [node {:title "Piano"
           :id id
           :data data
           :handles [{:type "source" :position "right"}]}
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
           :activeNotes (map (comp algo/note->midi keyword) (:notes data))
           :onPlayNoteInput (fn [midi _] (re-frame/dispatch [::events/toggle-note id midi]))
           :onStopNoteInput #()
           :width width}])]]]))

