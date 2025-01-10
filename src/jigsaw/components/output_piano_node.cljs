(ns jigsaw.components.output-piano-node
  (:require
   [reagent.core :as r]
   [re-frame.core :as re-frame]
   [jigsaw.algo :as algo]
   [jigsaw.subs :as subs]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.node :refer [node]]
   ["react-piano" :refer [Piano]]))

(def key-width 30)

(defn output-piano-node [{:keys [id]}]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming id])
        selected-label (r/atom :pitches)
        label-types [:pitches :intervals :degrees]]
    (fn [{:keys [id]}]
      [node {:title "Piano" :handles [{:type "target" :position "left"}]}
       (if-let [incoming-node (first @incoming-nodes)]
         (let [data (:data incoming-node)
               notes (:notes data)]
           (if (seq notes)
             (let [midis (map algo/note->midi notes)
                   midi->label (zipmap midis (get-in incoming-node [:data @selected-label]))
                   first-midi (first midis)
                   last-midi (last midis)
                   midi-range-start (- first-midi (mod first-midi 12))
                   midi-range-end (dec (+ last-midi (- 12 (mod last-midi 12))))
                   width (* key-width (- midi-range-end midi-range-start))]
               [:<>
                (when (some (partial contains? data) label-types)
                  [:div {:class "self-start flex space-x-2 items-center mb-2"}
                   [:label "Key Labels"]
                   [select {:value (or @selected-label "")
                            :class "text-black"
                            :on-change #(reset! selected-label (keyword (-> % .-target .-value)))
                            :placeholder "Key Labels"}
                    (for [label-type label-types
                          :when (contains? (:data incoming-node) label-type)]
                      [:option (name label-type)])]])
                [:div {:style {:pointerEvents "none"}}
                 [:> Piano
                  {:noteRange {:first midi-range-start :last midi-range-end}
                   :playNote #()
                   :stopNote #()
                   :renderNoteLabel (fn [_data]
                                      (let [{midi :midiNumber active? :isActive} (js->clj _data :keywordize-keys true)]
                                        (when active?
                                          (r/as-element
                                           [:b {:style {:font-size "1rem"}}
                                            (midi->label midi)]))))
                   :activeNotes midis
                   :width width}]]])
             [:p "No input"]))
         [:p "No input"])])))

