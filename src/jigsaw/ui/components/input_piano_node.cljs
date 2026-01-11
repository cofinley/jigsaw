(ns jigsaw.ui.components.input-piano-node
  (:require
   [jigsaw.impl.theory :as theory]
   [jigsaw.ui.components.node :refer [node]]
   [jigsaw.ui.events :as events]
   [jigsaw.ui.subs :as subs]
   ["react-piano" :refer [ControlledPiano]]
   [re-frame.core :as re-frame]))

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
           :activeNotes (map (comp theory/note->midi keyword) (:notes @data))
           :onPlayNoteInput (fn [midi prev]
                              (let [midis (set (js->clj prev))
                                    new-midis ((if (some? (some #{midi} midis)) disj conj) midis midi)
                                    notes (set (map theory/midi->note new-midis))
                                    pcis (set (map #(-> % theory/parts :pci) notes))]
                                (re-frame/dispatch [::events/update-node-data id {:notes notes
                                                                                  :pcis pcis}])))
           :onStopNoteInput #()
           :width width}])]]]))

