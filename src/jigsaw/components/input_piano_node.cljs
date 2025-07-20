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
  (let [data (re-frame/subscribe [::subs/data id])]
    [node {:title "Piano"
           :id id
           :data @data
           :handles [{:type "source" :position "right"}]}
     [:div {:class "flex flex-col gap-2"}
      [:div {:class "flex justify-between font-semibold text-xl"}
       [:button
        {:class "px-2 py-1 bg-gray-200 hover:bg-gray-100 cursor-pointer text-black rounded border cursor-pointer!"
         :title "Click to play"
         :on-click (fn [e]
                     (.stopPropagation e)
                     (re-frame/dispatch [::events/play-notes (:notes @data)]))}
        "Play"]
       [:button {:class "px-2 py-1 bg-gray-200 hover:bg-gray-100 cursor-pointer text-black rounded border"
                 :on-click #(re-frame/dispatch [::events/update-node-data id {:notes []}])}
        "Clear"]]
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
           :activeNotes (map (comp algo/note->midi keyword) (:notes @data))
           :onPlayNoteInput (fn [midi prev]
                              (let [midis (set (js->clj prev))
                                    new-midis ((if (some? (some #{midi} midis)) disj conj) midis midi)
                                    notes (set (map algo/midi->note new-midis))]
                                (re-frame/dispatch [::events/update-node-data id {:notes notes}])))
           :onStopNoteInput #()
           :width width}])]]]))

