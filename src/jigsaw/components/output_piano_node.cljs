(ns jigsaw.components.output-piano-node
  (:require
   ["react-piano" :refer [Piano]]
   [clojure.set :as set]
   [jigsaw.algo :as algo]
   [jigsaw.components.select :refer [select]]
   [jigsaw.events :as events]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]
   [reagent.core :as r]
   [jigsaw.spec :as specs]))

(defn output-piano-view [props]
  (let [selected-label (r/atom :pitches)
        label-types [:pitches :intervals :degrees]]
    (fn [props]
      (let [data (:data props)
            notes (:notes data)
            ; parent-data (:parent-data props)
            ; parent-notes (:notes parent-data)
            key-width (or (:key-width props) 30)
            display-label-options? (if-some [a (:display-label-options? props)] a true)]
        (if (seq notes)
          (let [midis (map algo/note->midi notes)
                ; parent-midis (map algo/note->midi parent-notes)
                midi->label (zipmap midis (get data @selected-label))
                first-midi (first midis)
                midi-range-start (- first-midi (mod first-midi 12))
                ; last-midi (last midis)
                ; midi-range-end (dec (+ last-midi (- 12 (mod last-midi 12))))
                midi-range-end (+ 23 midi-range-start)
                width (* key-width (- midi-range-end midi-range-start))]
            [:<>
             (when (and (some (partial contains? data) label-types) display-label-options?)
               [:div {:class "self-start flex space-x-2 items-center mb-2 text-lg"}
                [:label "Key Labels"]
                [select {:value (or @selected-label "")
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
          [:p {:class "text-lg"} "Nothing selected"])))))

(def white-key-color "#CBCBCB")
(def white-key-border-color "#AAA")
(def black-key-color "#222")
(def black-key-border-color "#000")
(def white-key-color-played "#6366f1") ;; indigo-500
(def black-key-color-played "#4338CA") ;; indigo-700
(def white-key-color-overridden "#A855F7")  ;; purple-500
(def black-key-color-overridden "#9333EA")  ;; purple-600
(def key-kept-in-chord "gray")
(def key-added-in-chord "#16A34A")  ;; green-600
(def key-removed-in-chord "#EF4444")  ;; red-500

(def piano-keys
  (take 88
        (map
         (fn [index pitch color]
           {:index index
            :pitch pitch
            :midi (+ 21 index)
            :color color})
         (range)
         (cycle [:A :Bb :B :C :C# :D :Eb :E :F :F# :G :Ab])
         (cycle [:w :b  :w :w :b  :w :b  :w :w :b  :w :b]))))

(def margin-keys #{:A :B :D :E :G})

(defn piano-preview [shape & {:keys [parent-notes]
                              :or {parent-notes []}}]
  (let [full-shape (if (contains? shape :notes)
                     shape
                     (algo/->shape (assoc shape :note (algo/pitch->note (:pitch shape)))))
        notes (:notes full-shape)
        chromas (map specs/pitches (:pitches full-shape))
        midis (map algo/note->midi notes)
        parent-midis (map algo/note->midi parent-notes)
        parent-chromas (map #(-> % algo/parts :pitch specs/pitches) parent-notes)
        white-key-width 20
        first-midi (first midis)
        midi-range-start (- first-midi (mod first-midi 12))
        midi-range-end (+ 23 midi-range-start)
        piano-key-span (filter #(<= midi-range-start (:midi %) midi-range-end) piano-keys)
        num-white-keys (count (filter #(= :w (:color %)) piano-key-span))
        current-specific-notes (set/difference (set midis) (set parent-midis))
        current-specific-chromas (set/difference (set chromas) (set parent-chromas))
        parent-specific-notes (set/difference (set parent-midis) (set midis))
        parent-specific-chromas (set/difference (set parent-chromas) (set chromas))
        shared-notes (set/intersection (set midis) (set parent-midis))
        shared-pitches (set/intersection (set chromas) (set parent-chromas))
        piano-width (* white-key-width num-white-keys)
        piano-height (* 2.3 white-key-width)
        border-width (* 0.0015 piano-width)
        black-key-width (/ white-key-width 2)
        black-key-height (/ piano-height 1.6)
        black-key-offset (- (- (/ black-key-width 2)) border-width)
        margin (str "0 0 0 " black-key-offset "px")]
    [:div.flex.rounded.overflow-hidden.pl-2
     {:class "cursor-pointer!"
      :title "Click to play"
      :on-click (fn [e]
                  (.stopPropagation e)
                  (re-frame/dispatch [::events/play-shape full-shape]))}
     (for [key piano-key-span
           :let [pitch (:pitch key)
                 chroma (specs/pitches pitch)
                 white? (= :w (:color key))
                 highlighted? (utils/in? (if (seq parent-midis) parent-midis midis) (:midi key))
                 parent-specific-note? (utils/in? parent-specific-notes (:midi key))
                 parent-specific-chroma? (utils/in? parent-specific-chromas chroma)
                 current-specific-note? (if (seq parent-midis) (utils/in? current-specific-notes (:midi key)) false)
                 current-specific-chroma? (utils/in? current-specific-chromas chroma)
                 shared-note? (utils/in? shared-notes (:midi key))
                 shared-chroma? (utils/in? shared-pitches chroma)
                 key-color (cond
                             (and parent-specific-note? parent-specific-chroma?) key-removed-in-chord
                             (and current-specific-note? current-specific-chroma?) key-added-in-chord
                             shared-note? key-kept-in-chord
                             (and highlighted? shared-chroma?) key-kept-in-chord
                             highlighted? (if white? white-key-color-played black-key-color-played)
                             :else (if white? white-key-color black-key-color))
                 border (str border-width "px solid rgba(0,0,0,0.5)")]]

       ^{:key (:index key)}
       [:button.flex.flex-col.justify-end
        {:style (if white?
                  {:height piano-height
                   :width white-key-width
                   :border-top border
                   :border-bottom border
                   :border-left border
                   :background-color key-color
                   :margin (if (contains? margin-keys (:pitch key)) margin 0)}
                  {:height black-key-height
                   :width black-key-width
                   :z-index 2
                   :border border
                   :background-color key-color
                   :margin margin})}])]))
