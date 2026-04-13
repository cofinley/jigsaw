(ns jigsaw.ui.components.function-transpose-node
  (:require
   [jigsaw.impl.theory :as theory]
   [jigsaw.ui.components.node :refer [node]]
   [jigsaw.ui.components.output-piano-node :refer [piano-preview]]
   [jigsaw.ui.components.select :refer [select]]
   [jigsaw.ui.components.table :refer [table]]
   [jigsaw.ui.events :as events]
   [jigsaw.ui.subs :as subs]
   [jigsaw.utils :as utils]
   ["react-piano" :refer [ControlledPiano]]
   [re-frame.core :as re-frame]))

(def key-width 30)

(defn function-transpose-node [{:keys [id]}]
  (let [data (re-frame/subscribe [::subs/data id])
        parent-data (re-frame/subscribe [::subs/parent-data id])
        result (re-frame/subscribe [::subs/function-result id])]
    [node {:title "Transpose"
           :id id
           :data @data
           :parent-data @parent-data
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if @parent-data
       (let [interval (or (:interval @data) "P1")
             multiplier (or (:multiplier @data) 1)]
         [:div {:class "flex flex-col space-y-2 items-start"}
          [:label {:class "space-x-4"}
           [:span {:class "font-semibold"} "Interval"]
           [select {:class "p-1 rounded-md border-2 border-gray-400 nodrag"
                    :value interval
                    :on-change (fn [e]
                                 (let [interval (-> e .-target .-value)]
                                   (re-frame/dispatch [::events/update-node-data id {:interval interval}])
                                   (when @result
                                     (re-frame/dispatch [::events/update-node-data id @result]))))}
            (for [[-interval details] (sort-by (fn [[-interval {semitones :semitones}]]
                                                 [(utils/parse-int (name -interval)) semitones])
                                               theory/intervals)]
              [:option
               {:value (name -interval)}
               (str (first (:aliases details)) " (" (name -interval) ")")])]

           [select {:class "p-1 rounded-md border-2 border-gray-400 nodrag"
                    :value multiplier
                    :on-change (fn [e]
                                 (let [multiplier (int (-> e .-target .-value))]
                                   (re-frame/dispatch [::events/update-node-data id {:multiplier multiplier}])
                                   (when @result
                                     (re-frame/dispatch [::events/update-node-data id @result]))))}
            [[:option {:value 1} "Up"]
             [:option {:value -1} "Down"]]]]

          (when @result
            (cond
              (theory/shape? @result) [table {:ms [@result]
                                              :row-render {"Root" :pitch
                                                           "Name" :name
                                                           "Piano" (fn [shape]
                                                                     (when (:name shape)
                                                                       [piano-preview
                                                                        shape]))}
                                              :row-title-render utils/pprint-aliases
                                              :row-selected? (fn [shape] (and (= (:pitch @data) (:pitch shape)) (= (:name @data) (:name shape))))
                                              :on-row-click (fn [shape]
                                                              (re-frame/dispatch [::events/update-node-data id shape]))}]
              :else [:div {:class "nodrag"}
                     (let [first-midi 60
                           octaves 2
                           last-midi (dec (+ first-midi (* octaves 12)))
                           width (* key-width (- last-midi first-midi))]
                       [:> ControlledPiano
                        {:class "nodrag"
                         :noteRange {:first first-midi :last last-midi}
                         :playNote (fn [midi] midi)
                         :stopNote #()
                         :activeNotes (map (comp theory/note->midi keyword) (:notes @result))
                         :onPlayNoteInput (fn [midi prev]
                                            (let [midis (set (js->clj prev))
                                                  new-midis ((if (some? (some #{midi} midis)) disj conj) midis midi)
                                                  notes (set (map theory/midi->note new-midis))
                                                  pcis (set (map #(-> % theory/parts :pci) notes))]
                                              (re-frame/dispatch [::events/update-node-data id {:notes notes
                                                                                                :pcis pcis}])))
                         :onStopNoteInput #()
                         :width width}])]))])
       [:p "Need an input"])]))
