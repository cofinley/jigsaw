(ns jigsaw.components.input-music-staff-node
  (:require
   [clojure.string :as string]
   [reagent.core :as r]
   [re-frame.core :as re-frame]
   [jigsaw.theory :as theory]
   [jigsaw.events :as events]
   [jigsaw.subs :as subs]
   [jigsaw.components.node :refer [node]]
   [jigsaw.components.select :refer [select]]
   ["abcjs" :as abcjs]))

(def SCALE 2)
(def mouse-height->note
  {0 :G5 1 :F5 2 :E5 3 :D5 4 :C5
   5 :B4 6 :A4 7 :G4 8 :F4 9 :E4 10 :D4 11 :C4
   12 :B3 13 :A3 14 :G3 15 :F3 16 :E3 17 :D3 18 :C3
   19 :B2 20 :A2 21 :G2 22 :F2})

(def abc-options #js {:staffwidth 300
                      :lineThickness 0.1
                      :scale SCALE
                      :jazzchords true
                      :add_classes true})

(defn- key-signature-impl [dom-id key-ref]
  (let [key-abc (theory/key-signature->abc key-ref)
        syntax (string/join "\n"
                            ["X:1"
                             (str "K:" key-abc)
                             "L:1/4"
                             "z"])]
    (.renderAbc abcjs dom-id syntax #js {:staffwidth 100
                                         :lineThickness 0.1})))

(defn key-signature [key-ref]
  (let [dom-id (str "key-sig-preview-" (random-uuid))]
    (r/create-class
     {:display-name "key-signature-preview"
      :component-did-mount
      (fn [_]
        (key-signature-impl dom-id key-ref))
      :component-did-update
      (fn [this _ _ _]
        (let [key-ref (second (r/argv this))]
          (key-signature-impl dom-id key-ref)))
      :reagent-render
      (fn []
        [:div {:id dom-id
               :class "inline-block"}])})))

(defn key-signature-options []
  (for [pitch theory/simple-pitch-keys
        key-type [:major :minor]]
    {:pitch pitch
     :name key-type
     :display-name (str (name pitch) " " (name key-type))}))

(defn key-signature-dropdown [current-key-sig on-change]
  (let [show-dropdown (r/atom false)]
    (fn [current-key-sig on-change]
      [:div {:class "relative"}
       ;; Current selection button
       [:button {:class "flex items-center space-x-2 px-3 py-2 border-2 border-neutral-400 rounded-md nodrag min-w-48"
                 :on-click (fn [e]
                             (.preventDefault e)
                             (.stopPropagation e)
                             (swap! show-dropdown not))}
        [:span {:class "flex-1 text-left"} (:display-name current-key-sig)]
        [:div {:class "flex items-center space-x-2"}
         [key-signature current-key-sig]
         [:span {:class "text-gray-500"} (if @show-dropdown "▲" "▼")]]]

       ;; Dropdown content
       (when @show-dropdown
         [:div {:class "nowheel absolute top-full left-0 mt-1 w-80 max-h-96 overflow-y-auto bg-neutral-900 border-2 border-neutral-400 rounded-md shadow-lg z-50 nodrag"}
          (for [option (key-signature-options)]
            [:div {:key (str (:pitch option) "|" (:name option))
                   :class (str "flex items-center justify-between px-3 py-2 cursor-pointer border-b border-gray-200 last:border-b-0 "
                               (if (and (= (:pitch current-key-sig) (:pitch option))
                                        (= (:name current-key-sig) (:name option)))
                                 "bg-indigo-500 hover:bg-indigo-400"
                                 "hover:bg-neutral-800"))
                   :on-click (fn [e]
                               (.preventDefault e)
                               (.stopPropagation e)
                               (on-change option)
                               (reset! show-dropdown false))}
             [:span {:class "font-medium flex-1"} (:display-name option)]
             [key-signature option]])])])))

(defn staff-impl [dom-id data]
  (let [key-ref (or (:key-signature data) {:pitch :C :name :major})
        notes (or (:notes data) #{})
        hover-note-val (get-in data [:hover-state :note])
        ;; Include hover note in display if it exists and is not already in notes
        all-notes (if (and hover-note-val (not (contains? notes hover-note-val)))
                    (conj notes hover-note-val)
                    notes)
        key-abc (theory/key-signature->abc key-ref)
        sorted-notes (sort-by theory/note->midi all-notes)
        ;; Separate notes into treble (C4 and above) and bass (below C4)
        treble-notes (filter #(>= (theory/note->midi %) (theory/note->midi :C4)) sorted-notes)
        bass-notes (filter #(< (theory/note->midi %) (theory/note->midi :C4)) sorted-notes)
        display-mode (or (:display-mode data) :melody)
        ;; Handle chord vs melody display
        treble-abc (if (seq treble-notes)
                     (if (= display-mode :chord)
                       (str "[" (string/join "" (map theory/note->abc treble-notes)) "]")
                       (string/join " " (map theory/note->abc treble-notes)))
                     "yyyy")
        bass-abc (if (seq bass-notes)
                   (if (= display-mode :chord)
                     (str "[" (string/join "" (map theory/note->abc bass-notes)) "]")
                     (string/join " " (map theory/note->abc bass-notes)))
                   "yyyy")
        syntax (string/join "\n"
                            ["X:1"
                             (str "K:" key-abc)
                             "L:1/4"
                             "%%staves (treble) (bass)"
                             "V:treble clef=treble"
                             "V:bass clef=bass"
                             (str "[V:treble] " treble-abc)
                             (str "[V:bass] " bass-abc)])
        rendering (.renderAbc abcjs dom-id syntax abc-options)]

    (when-let [svg (.querySelector (.getElementById js/document dom-id) "svg")]
      (when-let [g (.querySelector svg ".abcjs-staff-wrapper")]
        (.setAttribute svg "height" (+ 20 (-> g .getBoundingClientRect .-height)))))
    ;; Apply hover styling after rendering
    (when hover-note-val
      (js/setTimeout
       (fn []
         (when-let [svg (.querySelector (.getElementById js/document dom-id) "svg")]
           (let [notes-elements (.querySelectorAll svg (str ".abcjs-note" (if (= :chord display-mode)
                                                                            " .abcjs-notehead"
                                                                            "")))]
             (doseq [note-elem (js/Array.from notes-elements)]
               (let [note-name (str (name hover-note-val))
                     abc (first rendering)
                     ;; Match hovered elem to note to see if new or existing to color accordingly
                     ;; For chords, color the abcjs-notehead since the abcjs-note is shared between multiple pitches
                     note-elem-pitch-obj (if (= :chord display-mode)
                                           nil
                                           (->> (.findSelectableElement abc note-elem)
                                                .-element
                                                .-absEl
                                                .-abcelem
                                                .-pitches
                                                (filter #(= (name (theory/abc-pitch->note (.-name %))) note-name))
                                                first))
                     note-elem-name (if (= :chord display-mode)
                                      (name (theory/abc-pitch->note (.-name (.-dataset note-elem))))
                                      (when note-elem-pitch-obj
                                        (name (theory/abc-pitch->note (.-name note-elem-pitch-obj)))))]
                 (when (and note-elem-name note-name (= note-name note-elem-name))
                   (if (get-in data [:hover-state :exists?])
                     (.setAttribute note-elem "fill" "red")
                     (.setAttribute note-elem "fill" "lightgreen"))))))))
       0))))

(defn mouse-event->note [e]
  (when-let [svg (.querySelector (.-currentTarget e) "svg")]
    (let [svg-rect (.getBoundingClientRect svg)
          click-y (- (.-clientY e) (.-top svg-rect))
          ;; Use SVG coordinate system - find staff lines with abcjs classes
          staff-elements (.querySelectorAll svg ".abcjs-staff")
          ;; Calculate relative position to determine note
          note (when (> (.-length staff-elements) 0)
                 (let [svg-height (.-height svg-rect)
                       grid-size (/ svg-height (count (keys mouse-height->note)) SCALE) ; 23 note positions across staff height
                       grid-position (Math/round (/ click-y grid-size))]
                   ;; TODO: use existing key to transpose the final note
                   (get mouse-height->note grid-position)))]
      note)))

(defn staff [id data on-staff-click on-staff-clear]
  (let [dom-id (str "staff-" (random-uuid))]
    (r/create-class
     {:display-name "staff-component"
      :component-did-mount
      (fn [_]
        (staff-impl dom-id data))
      :component-did-update
      (fn [this _ _ _]
        (let [new-data (nth (r/argv this) 2)]
          (staff-impl dom-id new-data)))
      :reagent-render
      (fn [_ new-data]
        [:div {:class "flex flex-col items-center space-y-2 nodrag"}
         [:div {:id dom-id
                :style {:cursor "pointer"}
                :on-click
                (fn [e]
                  (when-let [note (mouse-event->note e)]
                    (on-staff-click note)))
                :on-mouse-move
                (fn [e]
                  (when-let [note (mouse-event->note e)]
                    (let [notes (or (:notes new-data) #{})
                          exists? (contains? notes note)]
                      (re-frame/dispatch [::events/update-node-data id {:hover-state {:note note :exists? exists?}}]))))
                :on-mouse-leave
                (fn [_]
                  (re-frame/dispatch [::events/update-node-data id {:hover-state {:note nil :exists? false}}]))}]
         [:button {:class "px-4 py-2 bg-red-500 text-white rounded hover:bg-red-600"
                   :on-click on-staff-clear}
          "Clear All Notes"]])})))

(defn input-music-staff-node [{:keys [id]}]
  (let [data (re-frame/subscribe [::subs/data id])]
    (fn [{:keys [id]}]
      [node {:title "Music Staff"
             :id id
             :data @data
             :handles [{:type "source" :position "right"}]}
       [:div {:class "flex flex-col space-y-4 nodrag text-xl"}
        ;; Key signature dropdown
        [:div {:class "flex items-center space-x-2"}
         [:label {:class "font-semibold"} "Key Signature:"]
         [key-signature-dropdown
          (let [current-key (or (:key-signature @data) {:pitch :C :name :major})]
            (assoc current-key :display-name (str (name (:pitch current-key)) " " (name (:name current-key)))))
          (fn [option]
            (let [key-sig {:pitch (:pitch option)
                           :name (:name option)}]
              (re-frame/dispatch [::events/update-node-data id {:key-signature key-sig}])))]]

        ;; Display mode toggle
        [:div {:class "flex items-center space-x-2"}
         [:label {:class "font-semibold"} "Display:"]
         [select {:value (name (or (:display-mode @data) :melody))
                  :on-change (fn [e]
                               (let [mode (keyword (-> e .-target .-value))]
                                 (re-frame/dispatch [::events/update-node-data id {:display-mode mode}])))}
          [[:option {:value "melody"} "Melody (spread out)"]
           [:option {:value "chord"} "Chord (stacked)"]]]]

        ;; Staff component
        [staff id @data
         (fn [note]
           (let [current-notes (or (:notes @data) #{})
                 new-notes (if (contains? current-notes note)
                             (disj current-notes note)
                             (conj current-notes note))]
             (re-frame/dispatch [::events/update-node-data id {:notes new-notes
                                                               :hover-state {:note note
                                                                             :exists? (not (contains? current-notes note))}}])))
         (fn []
           (re-frame/dispatch [::events/update-node-data id {:notes #{}}]))]]])))
