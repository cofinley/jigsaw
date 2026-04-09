(ns jigsaw.ui.components.input-piano-node
  (:require
   [jigsaw.impl.theory :as theory]
   [jigsaw.ui.components.node :refer [node]]
   [jigsaw.ui.events :as events]
   [jigsaw.ui.subs :as subs]
   ["react-piano" :refer [ControlledPiano]]
   [re-frame.core :as re-frame]))

(def key-width 30)

(defn previous-c [midi]
  (- midi (rem midi 12)))

(defn next-c [midi]
  (+ midi (- 12 (rem midi 12))))

(defn input-piano-node [{:keys [id]}]
  (let [data (re-frame/subscribe [::subs/data id])
        recording? (re-frame/subscribe [::subs/recording? id])
        midi-input (re-frame/subscribe [::subs/midi-input])]
    [node {:title "Piano"
           :id id
           :data @data
           :handles [{:type "source" :position "right"}]}
     [:div {:class "flex flex-col gap-2"}
      [:div {:class "flex justify-between font-semibold text-xl"}
       [:div
        [:button
         {:class "px-2 py-1 bg-gray-200 hover:bg-gray-100 cursor-pointer text-black rounded border cursor-pointer!"
          :title "Record from MIDI"
          :on-click (fn [e]
                      (.stopPropagation e)
                      (if (and (not @recording?)
                               (or (not @midi-input) (= @midi-input "Select")))
                        (js/alert "Must select MIDI input from settings first")
                        (re-frame/dispatch [::events/toggle-recording id])))}
         (if @recording?
           "Recording MIDI..."
           "Record MIDI")]
        [:button
         {:class "px-2 py-1 bg-gray-200 hover:bg-gray-100 cursor-pointer text-black rounded border cursor-pointer!"
          :title "Click to play"
          :on-click (fn [e]
                      (.stopPropagation e)
                      (re-frame/dispatch [::events/play-notes (:notes @data)]))}
         "Play"]
        [:button
         {:class "px-2 py-1 bg-gray-200 hover:bg-gray-100 cursor-pointer text-black rounded border cursor-pointer!"
          :title "Click to play via MIDI"
          :on-click (fn [e]
                      (.stopPropagation e)
                      (re-frame/dispatch [::events/play-notes-midi (:notes @data)]))}
         "Play MIDI"]]
       [:button {:class "px-2 py-1 bg-gray-200 hover:bg-gray-100 cursor-pointer text-black rounded border"
                 :on-click #(re-frame/dispatch [::events/update-node-data id {:notes []}])}
        "Clear"]]
      [:div {:class "nodrag max-w-xl overflow-x-scroll"}
       (let [current-midis (map theory/note->midi (:notes @data))
             ; first-midi (if (empty? current-midis) 48 (previous-c (apply min current-midis)))
             first-midi 21
             ; octaves 2
             ; last-midi (if (empty? current-midis)
             ;             (dec (+ 48 (* octaves 12)))
             ;             (max (dec (+ 48 (* octaves 12))) (dec (next-c (apply max current-midis)))))
             last-midi 108
             ; width (* key-width (- last-midi first-midi))]
             width (* key-width (- last-midi first-midi) 1)]
         [:> ControlledPiano
          {:noteRange {:first first-midi :last last-midi}
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
           ; :keyWidthToHeight 1.4
           :renderNoteLabel (fn [opts]
                              (let [{:keys [keyboardShortcut midiNumber isActive isAccidental]} (js->clj opts :keywordize-keys true)]
                                (when (= 0 (mod midiNumber 12))
                                  (name (theory/midi->note midiNumber)))))
           :width width}])]]]))

