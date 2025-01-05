(ns jigsaw.views
  (:require
   [clojure.string :as s]
   [cljs.pprint :as pprint]
   [reagent.core :as r]
   [reagent.debug :refer [log prn]]
   [re-frame.core :as re-frame]
   [jigsaw.algo :as algo]
   [jigsaw.spec :as specs]
   [jigsaw.subs :as subs]
   [jigsaw.events :as events]
   ["react" :refer [useMemo StrictMode]]
   ["react-piano" :refer [Piano ControlledPiano]]
   ["@xyflow/react" :refer [ReactFlow
                            Background
                            Controls
                            applyNodeChanges
                            applyEdgeChanges
                            addEdge
                            Handle
                            Panel]]
   ["abcjs" :as abcjs]))

(def key-width 30)

(defn select [props & body]
  [:select (r/merge-props {:class "p-1 rounded-md border border-gray-400 nodrag"} props)
   (for [child body]
     (with-meta child {:key (str (random-uuid))}))])

(defn node [m & body]
  [:div (r/merge-props {:class "react-flow__node-default w-full flex flex-col pb-4"} (:class (:props m)))
   [:div {:class "border-b border-gray-400 mb-4"}
    [:h4 {:class "w-max text-2xl"} (:title m)]]
   (for [child body]
     (with-meta child {:key (str (random-uuid))}))])

(defn handle [props & body]
  [:> Handle (r/merge-props {:class "h-3 w-3"} props)
   (for [child body]
     (with-meta child {:key (str (random-uuid))}))])

(defn input-piano-node [props _]
  (let [id (:id props)
        active-notes (re-frame/subscribe [::subs/active-notes id])]
    (r/as-element
     [node {:props props :title "Piano"}
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
           :activeNotes (map (comp algo/note->midi keyword) @active-notes)
           :onPlayNoteInput (fn [midi _] (re-frame/dispatch [::events/toggle-note id midi]))
           :onStopNoteInput #()
           :width width}])]
      [handle {:type "source" :position "right"}]])))

(defn input-shape-node [props _]
  (let [id (:id props)
        shape-type (if (= :input-chord (keyword (:type props))) :chord :scale)
        title (if (= shape-type :chord) "Chord" "Scale")
        data (if (= shape-type :chord) specs/chords specs/scales)
        selected-pitch (re-frame/subscribe [::subs/pitch id])
        selected-name (re-frame/subscribe [::subs/name id])]
    (r/as-element
     [node {:props props
            :title title
            :class "text-black"}
       ;; Pitches
      [:div {:class "flex space-x-2 items-center"}
       [:label "Pitch"]
       [select {:value (or @selected-pitch "")
                :class "text-black"
                :on-change #(re-frame/dispatch [::events/set-pitch id (keyword (-> % .-target .-value))])
                :placeholder "Pitch"}
        ^{:key ""} [:option {:disabled true :value ""} "Pitch"]
        (for [pitch (keys (sort-by val < specs/pitches))
              :when (and (not (s/includes? (name pitch) "bb")) (not (s/includes? (name pitch) "##")))]
          ^{:key pitch}
          [:option {:value pitch} (name pitch)])]
       ;; Shape names
       [:label title]
       [select {:value (or @selected-name "")
                :class "text-black"
                :on-change #(re-frame/dispatch [::events/set-name id (keyword (-> % .-target .-value))])
                :placeholder (str title "Name")}
        ^{:key ""} [:option {:disabled true :value ""} title]
        (for [[shape-name details] data
              :let [aliases (::specs/aliases details)]]
          ^{:key shape-name}
          [:option {:value shape-name
                    :title (when (seq aliases) (str "Aliases:\n" (s/join "\n" (map #(str "- " %) aliases))))}
           (name shape-name)])]]
      [handle {:type "source" :position "right"}]])))

(defn output-piano-node [props _]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming props])
        selected-label (r/atom :pitches)
        label-types [:pitches :intervals :degrees]]
    (fn [props _]
      (r/as-element
       [node {:props props :title "Piano"}
        [handle {:type "target" :position "left"}]
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
                       ^{:key label-type} [:option (name label-type)])]])
                 [:div {:style {:pointerEvents "none"}}
                  [:> Piano
                   {:noteRange {:first midi-range-start :last midi-range-end}
                    :playNote #()
                    :stopNote #()
                    :renderNoteLabel (fn [_data]
                                       (let [{midi :midiNumber active? :isActive} (js->clj _data :keywordize-keys true)]
                                         (when active?
                                           (r/as-element
                                            [:span {:style {:font-size "1rem"}}
                                             (midi->label midi)]))))
                    :activeNotes midis
                    :width width}]]])
              [:p "No input"]))
          [:p "No input"])]))))

(defn note->abc [n]
  (let [{:keys [letter octave accidental]} (algo/parts n)
        abc-accidental (case accidental
                         "bb" "__"
                         "b" "_"
                         "#" "^"
                         "##" "^^"
                         "")
        lowercase? (< 4 octave)
        commas (if lowercase? 0 (- 4 octave))
        apostrophes (if lowercase? (- octave 5) 0)]
    (str
     abc-accidental
     ((if lowercase? s/lower-case str) letter)
     (s/join (take commas (repeat ",")))
     (s/join (take apostrophes (repeat "'"))))))

(defn score [incoming-node]
  (let [dom-id (str (random-uuid))]
    (r/create-class
     {:display-name "score"
      :component-did-mount
      (fn [_]
        (let [notes (set (get-in incoming-node [:data :notes]))
              chord? (= :input-chord (:type incoming-node))
              scale? (= :input-scale (:type incoming-node))
              pitch (get-in incoming-node [:data :pitch])
              shape-name (get-in incoming-node [:data :name])
              key (if scale? (str (name pitch) (name shape-name)) "C")
              sorted-notes (sort-by algo/note->midi notes)
              pitches-str (s/join " " (map note->abc sorted-notes))
              syntax (s/join "\n"
                             ["X:1"
                              (str "K:" key)
                              "L:1/4"
                              (s/join " "
                                      [(when chord?
                                         (str "\"" (str (name pitch) (name shape-name)) "\""))
                                       (if chord?
                                         (str "[" pitches-str "]")
                                         pitches-str)])])]
          (.renderAbc abcjs dom-id syntax #js {:jazzchords true :lineThickness 0.1 :staffwidth (if chord? 100 (* 50 (count notes)))})))
      :reagent-render
      (fn []
        [:div {:id dom-id}])})))

(defn output-music-staff-node [props _]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming props])]
    (r/as-element
     [node {:props props :title "Staff"}
      [handle {:type "target" :position "left"}]
      (if-let [incoming-node (first @incoming-nodes)]
        (let [notes (get-in incoming-node [:data :notes])]
          (if (seq notes)
            [score incoming-node]
            [:p "No input"]))
        [:p "No input"])])))

(defn output-debug-node [props _]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming props])]
    (r/as-element
     [node {:props props :title "Debug"}
      [handle {:type "target" :position "left"}]
      (if-let [incoming-node (first @incoming-nodes)]
        [:pre {:class "text-left"} (with-out-str (pprint/pprint incoming-node))]
        [:p "No input"])])))

(def node-categories
  {:input "Input"
   :function "Function"
   :output "Output"})

(def node-types
  [{:type :input-piano
    :category :input
    :label "Piano"
    :component input-piano-node}
   {:type :input-chord
    :category :input
    :label "Chord"
    :component input-shape-node}
   {:type :input-scale
    :category :input
    :label "Scale"
    :component input-shape-node}
   {:type :output-piano
    :category :output
    :label "Piano"
    :component output-piano-node}
   {:type :output-music-staff
    :category :output
    :label "Music Staff"
    :component output-music-staff-node}
   {:type :output-debug
    :category :output
    :label "Debug"
    :component output-debug-node}])

(defn flow []
  (let [nodes (re-frame/subscribe [::subs/nodes])
        edges (re-frame/subscribe [::subs/edges])
        on-nodes-change (fn [changes]
                          (re-frame/dispatch [::events/set-nodes (js->clj (applyNodeChanges changes (clj->js @nodes)) :keywordize-keys true)]))
        on-edges-change (fn [changes]
                          (re-frame/dispatch [::events/set-edges (js->clj (applyEdgeChanges changes (clj->js @edges)) :keywordize-keys true)]))
        on-connect (fn [params]
                     (re-frame/dispatch [::events/set-edges (js->clj (addEdge params (clj->js @edges)) :keywordize-keys true)]))
        flow-node-types (useMemo #(clj->js (reduce (fn [m node-type]
                                                     (assoc m (:type node-type) (r/reactify-component (:component node-type))))
                                                   {} node-types)) #js [])]
    [:div {:style {:height "100%"}}
     [:> ReactFlow {:nodes (clj->js @nodes)
                    :edges (clj->js @edges)
                    :onNodesChange on-nodes-change
                    :onEdgesChange on-edges-change
                    :onConnect on-connect
                    :nodeTypes flow-node-types
                    :fitView true
                    :colorMode "dark"}
      [:> Panel {:position "top-right"}
       [select {:on-change #(re-frame/dispatch [::events/add-node (-> % .-target .-value)])
                :default-value ""}
        ^{:key "-1"} [:option {:disabled true :value ""} "(Add Node)"]
        (for [[cat-k cat-label] node-categories]
          ^{:key cat-label} [:optgroup {:label cat-label}
                             (for [node-type node-types
                                   :when (= cat-k (:category node-type))]
                               ^{:key node-type} [:option {:value (:type node-type)} (:label node-type)])])]]
      [:> Background]
      [:> Controls]]]))

(defn main-panel []
  [:> StrictMode
   [:f> flow]])
