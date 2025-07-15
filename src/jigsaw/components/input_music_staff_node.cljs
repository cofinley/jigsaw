(ns jigsaw.components.input-music-staff-node
  (:require
   [clojure.string :as string]
   [reagent.core :as r]
   [re-frame.core :as re-frame]
   [jigsaw.spec :as specs]
   [jigsaw.events :as events]
   [jigsaw.subs :as subs]
   [jigsaw.algo :as algo]
   [jigsaw.components.node :refer [node]]
   [jigsaw.components.select :refer [select]]
   ["abcjs" :as abcjs]))

(defn key-signature-options []
  (for [pitch specs/simple-pitch-keys
        key-type [:major :minor]]
    {:pitch pitch
     :name key-type
     :display-name (str (name pitch) " " (name key-type))}))

(defn key-signature [dom-id key-ref]
  (let [key-shape (algo/->shape (assoc key-ref :note (algo/pitch->note (:pitch key-ref))))
        key-abc (str (name (:pitch key-shape))
                     " exp "
                     (string/join " " (map algo/note->abc (:notes key-shape))))
        syntax (string/join "\n"
                            ["X:1"
                             (str "K:" key-abc)
                             "L:1/4"
                             "z"])]
    (.renderAbc abcjs dom-id syntax #js {:staffwidth 100
                                         :lineThickness 0.1})))

(defn key-signature-preview [key-ref]
  (let [dom-id (str "key-sig-preview-" (random-uuid))]
    (r/create-class
     {:display-name "key-signature-preview"
      :component-did-mount
      (fn [_]
        (key-signature dom-id key-ref))
      :component-did-update
      (fn [this _ _ _]
        (let [key-ref (second (r/argv this))]
          (key-signature dom-id key-ref)))
      :reagent-render
      (fn []
        [:div {:id dom-id
               :class "inline-block"}])})))

(defn staff [dom-id data]
  (let [key-sig (or (:key-signature data) {:pitch :C :name :major})
        notes (or (:notes data) #{})
        hover-note-val (get-in data [:hover-state :note])
        ;; Include hover note in display if it exists and is not already in notes
        all-notes (if (and hover-note-val (not (contains? notes hover-note-val)))
                    (conj notes hover-note-val)
                    notes)
        key-shape (algo/->shape (assoc key-sig :note (algo/pitch->note (:pitch key-sig))))
        key-abc (str (name (:pitch key-shape))
                     " exp "
                     (string/join " " (map algo/note->abc (:notes key-shape))))
        sorted-notes (sort-by algo/note->midi all-notes)
        ;; Separate notes into treble (C4 and above) and bass (below C4)
        treble-notes (filter #(>= (algo/note->midi %) (algo/note->midi :C4)) sorted-notes)
        bass-notes (filter #(< (algo/note->midi %) (algo/note->midi :C4)) sorted-notes)
        display-mode (or (:display-mode data) :melody)
        ;; Handle chord vs melody display
        treble-abc (if (seq treble-notes)
                     (if (= display-mode :chord)
                       (str "[" (string/join "" (map algo/note->abc treble-notes)) "]")
                       (string/join " " (map algo/note->abc treble-notes)))
                     "z")
        bass-abc (if (seq bass-notes)
                   (if (= display-mode :chord)
                     (str "[" (string/join "" (map algo/note->abc bass-notes)) "]")
                     (string/join " " (map algo/note->abc bass-notes)))
                   "z")
        syntax (string/join "\n"
                            ["X:1"
                             (str "K:" key-abc)
                             "L:1/4"
                             "%%staves (treble) (bass)"
                             "V:treble clef=treble"
                             "V:bass clef=bass"
                             (str "[V:treble] " treble-abc)
                             (str "[V:bass] " bass-abc)])
        rendering (.renderAbc abcjs dom-id syntax #js {:staffwidth 1
                                                       :lineThickness 0.1
                                                       :add_classes true})]
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
                                                (filter #(= (name (algo/abc-pitch->note (.-name %))) note-name))
                                                first))
                     note-elem-name (if (= :chord display-mode)
                                      (name (algo/abc-pitch->note (.-name (.-dataset note-elem))))
                                      (when note-elem-pitch-obj
                                        (name (algo/abc-pitch->note (.-name note-elem-pitch-obj)))))]
                 (when (and note-elem-name note-name (= note-name note-elem-name))
                   (if (get-in data [:hover-state :exists?])
                     (.setAttribute note-elem "fill" "red")
                     (.setAttribute note-elem "fill" "lightgreen"))))))))
       0))))

(defn staff-component [id data on-staff-click on-staff-clear]
  (let [dom-id (str "staff-" (random-uuid))]
    (r/create-class
     {:display-name "staff-component"
      :component-did-mount
      (fn [_]
        (staff dom-id data))
      :component-did-update
      (fn [this _ _ _]
        (let [new-data (nth (r/argv this) 2)]
          (staff dom-id new-data)))
      :reagent-render
      (fn [_ new-data]
        [:div {:class "flex flex-col items-center space-y-2 nodrag"}
         [:div {:id dom-id
                :style {:cursor "pointer"}
                :on-click
                (fn [e]
                  ;; Find the SVG element created by abcjs
                  (when-let [svg (.querySelector (.-currentTarget e) "svg")]
                    (let [svg-rect (.getBoundingClientRect svg)
                          click-y (- (.-clientY e) (.-top svg-rect))
                          ;; Use SVG coordinate system - find staff lines with abcjs classes
                          staff-elements (.querySelectorAll svg ".abcjs-staff")
                          ;; Calculate relative position to determine note
                          note (when (> (.-length staff-elements) 0)
                                 ;; Simple grid approach - divide staff area into note positions
                                 (let [svg-height (.-height svg-rect)
                                       grid-size (/ svg-height 20) ; 20 note positions across staff height
                                       grid-position (Math/round (/ click-y grid-size))
                                       ;; Map grid positions to notes (treble to bass range)
                                       note-map {0 :G5, 1 :F5, 2 :E5, 3 :D5, 4 :C5, 5 :B4, 6 :A4, 7 :G4, 8 :F4, 9 :E4,
                                                 10 :D4, 11 :C4, 12 :B3, 13 :A3, 14 :G3, 15 :F3, 16 :E3, 17 :D3, 18 :C3, 19 :B2}]
                                   (get note-map grid-position)))]
                      (when note
                        (on-staff-click note)))))
                :on-mouse-move
                (fn [e]
                  ;; Find the SVG element created by abcjs
                  (when-let [svg (.querySelector (.-currentTarget e) "svg")]
                    (let [svg-rect (.getBoundingClientRect svg)
                          mouse-y (- (.-clientY e) (.-top svg-rect))
                          ;; Use SVG coordinate system - find staff lines with abcjs classes
                          staff-elements (.querySelectorAll svg ".abcjs-staff")
                          ;; Calculate relative position to determine note
                          note (when (> (.-length staff-elements) 0)
                                 ;; Simple grid approach - divide staff area into note positions
                                 (let [svg-height (.-height svg-rect)
                                       grid-size (/ svg-height 20) ; 20 note positions across staff height
                                       grid-position (Math/round (/ mouse-y grid-size))
                                       ;; Map grid positions to notes (treble to bass range)
                                       note-map {0 :G5, 1 :F5, 2 :E5, 3 :D5, 4 :C5, 5 :B4, 6 :A4, 7 :G4, 8 :F4, 9 :E4,
                                                 10 :D4, 11 :C4, 12 :B3, 13 :A3, 14 :G3, 15 :F3, 16 :E3, 17 :D3, 18 :C3, 19 :B2}]
                                   (get note-map grid-position)))]
                      (when note
                        (let [notes (or (:notes new-data) #{})
                              exists? (contains? notes note)]
                          (re-frame/dispatch [::events/update-node-data id {:hover-state {:note note :exists? exists?}}]))))))
                :on-mouse-leave
                (fn [_]
                  (re-frame/dispatch [::events/update-node-data id {:hover-state {:note nil :exists? false}}]))}]
         [:button {:class "px-4 py-2 bg-red-500 text-white rounded hover:bg-red-600"
                   :on-click on-staff-clear}
          "Clear All Notes"]])})))

(defn input-music-staff-node [{:keys [id]}]
  (let [data (re-frame/subscribe [::subs/data id])]
    (fn [{:keys [id]}]
      [node {:title "Music Staff Input"
             :id id
             :data @data
             :handles [{:type "source" :position "right"}]}
       [:div {:class "flex flex-col space-y-4 nodrag"}
        ;; Key signature dropdown
        [:div {:class "flex items-center space-x-2"}
         [:label {:class "font-semibold"} "Key Signature:"]
         [select {:value (str (name (get-in @data [:key-signature :pitch] :C))
                              "|"
                              (name (get-in @data [:key-signature :name] :major)))
                  :on-change (fn [e]
                               (let [value (-> e .-target .-value)
                                     [pitch-str name-str] (string/split value "|")
                                     key-sig {:pitch (keyword pitch-str)
                                              :name (keyword name-str)}]
                                 (re-frame/dispatch [::events/update-node-data id {:key-signature key-sig}])))}
          (for [option (key-signature-options)]
            [:option {:value (str (name (:pitch option)) "|" (name (:name option)))
                      :key (str (name (:pitch option)) "|" (name (:name option)))}
             (:display-name option)])]
         [key-signature-preview (or (:key-signature @data) {:pitch :C :name :major})]]

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
        [staff-component id @data
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
