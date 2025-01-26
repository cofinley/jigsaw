(ns jigsaw.components.output-piano-node
  (:require
   [reagent.core :as r]
   [jigsaw.algo :as algo]
   [jigsaw.components.select :refer [select]]
   ["react-piano" :refer [Piano]]))

(def key-width 30)

(defn output-piano-view [props]
  (let [selected-label (r/atom :pitches)
        label-types [:pitches :intervals :degrees]]
    (fn [props]
      (let [data (:data props)
            notes (:notes data)]
        (if (seq notes)
          (let [midis (map algo/note->midi notes)
                midi->label (zipmap midis (get data @selected-label))
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
                       :when (contains? data label-type)]
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
          [:p "Nothing selected"])))))
