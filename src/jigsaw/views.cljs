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
   ["react-piano" :refer [Piano ControlledPiano]]
   ["@xyflow/react" :refer [ReactFlow
                            Background
                            Controls
                            applyNodeChanges
                            applyEdgeChanges
                            addEdge
                            Handle
                            Panel]]
   ["vexflow" :refer [Factory]]))

(def key-width 20)

(defn select [props & body]
  [:select (r/merge-props {:class "p-1 rounded-md border border-gray-400"} props)
   (for [child body]
     (with-meta child {:key (str (random-uuid))}))])

(defn node [m & body]
  [:div (r/merge-props {:class "react-flow__node-default w-full flex flex-col pb-4"} (:props m))
   [:div {:class "border-b border-gray-400 mb-5"}
    [:h4 {:class "w-max text-2xl"} (:title m)]]
   (for [child body]
     (with-meta child {:key (str (random-uuid))}))])

(defn input-piano-node [props _]
  (let [id (:id props)
        active-notes (re-frame/subscribe [::subs/active-notes id])]
    (r/as-element
     [node {:title "Piano"}
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
      [:> Handle {:type "source" :position "right"}]])))

(defn input-shape-node [m]
  (let [id (get-in m [:props :id])
        shape-type (if (= :input-chord (keyword (get-in m [:props :type]))) :chord :scale)
        title (if (= shape-type :chord) "Chord" "Scale")
        data (if (= shape-type :chord) specs/chords specs/scales)
        selected-pitch (re-frame/subscribe [::subs/pitch id])
        selected-name (re-frame/subscribe [::subs/name id])]
    (r/as-element
     [node {:title title
            :class "text-black"}
       ;; Pitches
      [:div {:class "flex space-x-2 items-center"}
       [:label "Pitch"]
       [select {:value (or @selected-pitch "")
                :class "text-black"
                :on-change #(re-frame/dispatch [::events/set-pitch id (keyword (-> % .-target .-value))])}
        ^{:key ""} [:option {:value "" :disabled true} "Pitch"]
        (for [pitch (keys (sort-by val < specs/pitches))
              :when (and (not (s/includes? (name pitch) "bb")) (not (s/includes? (name pitch) "##")))]
          ^{:key pitch}
          [:option {:value pitch} (name pitch)])]
       ;; Shape names
       [:label title]
       [select {:value (or @selected-name "")
                :class "text-black"
                :on-change #(re-frame/dispatch [::events/set-name id (keyword (-> % .-target .-value))])}
        ^{:key ""} [:option {:value "" :disabled true} title]
        (for [[shape-name details] data
              :let [aliases (::specs/aliases details)]]
          ^{:key shape-name}
          [:option {:value shape-name
                    :title (when (seq aliases) (str "Aliases:\n" (s/join "\n" (map #(str "- " %) aliases))))}
           (name shape-name)])]]
      [:> Handle {:type "source" :position "right"}]])))

(defn input-chord-node [props _]
  (input-shape-node {:props props}))

(defn input-scale-node [props _]
  (input-shape-node {:props props}))

(defn output-piano-node [props _]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming props])]
    (r/as-element
     [node {:title "Piano"}
      [:> Handle {:type "target" :position "left"}]
      [:div {:style {:pointerEvents "none"}}
       (if-let [incoming-node (first @incoming-nodes)]
         (let [notes (get-in incoming-node [:data :notes])
               midis (map algo/note->midi notes)
               sorted-midis (sort midis)]
           (if (seq sorted-midis)
             (let [first-midi (first sorted-midis)
                   last-midi (last sorted-midis)
                   midi-range-start (- first-midi (mod first-midi 12))
                   midi-range-end (dec (+ last-midi (- 12 (mod last-midi 12))))
                   width (* key-width (- midi-range-end midi-range-start))]
               [:> Piano
                {:noteRange {:first midi-range-start :last midi-range-end}
                 :playNote #()
                 :stopNote #()
                 :renderNoteLabel (fn [_data]
                                    (let [{midi :midiNumber active? :isActive} (js->clj _data :keywordize-keys true)]
                                      (when active?
                                        (r/as-element
                                         [:span {:style {:font-size "1rem"}}
                                          (:pitch (algo/parts (algo/midi->note midi nil)))]))))
                 :activeNotes midis
                 :width width}])
             [:p "No input"]))
         [:p "No input"])]])))

(defn vexflow-score [incoming-node]
  (let [dom-id (str (random-uuid))
        notes (set (get-in incoming-node [:data :notes]))]
    (r/create-class
     {:display-name "vexflow-score"
      :component-did-mount
      (fn [_]
        (let [vf (Factory. (clj->js {:renderer {:elementId dom-id}
                                     :width 500
                                     :height 200}))
              score (.EasyScore vf)
              system (.System vf)
              stave {:voices [(.voice score (.notes score (str "(" (s/join " " (map name notes)) ")/w") {:stem "up"}))]}]
          (-> system
              (.addStave (clj->js stave))
              (.addClef "treble"))
          (.draw vf)))

      :reagent-render
      (fn []
        [:div {:id dom-id}])})))

(defn output-music-staff-node [props _]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming props])]
    (r/as-element
     [node {:title "Staff"}
      [:> Handle {:type "target" :position "left"}]
      (if-let [incoming-node (first @incoming-nodes)]
        (let [notes (get-in incoming-node [:data :notes])]
          (if (seq notes)
            [vexflow-score incoming-node]
            [:p "No input"]))
        [:p "No input"])])))

(defn output-debug-node [props _]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming props])]
    (r/as-element
     [node {:props props :title "Debug"}
      [:> Handle {:type "target" :position "left"}]
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
    :component input-chord-node}
   {:type :input-scale
    :category :input
    :label "Scale"
    :component input-scale-node}
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
                          (re-frame/dispatch [::events/set-edges (js->clj (applyEdgeChanges changes (clj->js @nodes)) :keywordize-keys true)]))
        on-connect (fn [params]
                     (re-frame/dispatch [::events/set-edges (js->clj (addEdge params (clj->js @edges)) :keywordize-keys true)]))]
    [:div {:style {:height "100%"}}
     [:> ReactFlow {:nodes (clj->js @nodes)
                    :edges (clj->js @edges)
                    :onNodesChange on-nodes-change
                    :onEdgesChange on-edges-change
                    :onConnect on-connect
                    :nodeTypes (clj->js (reduce (fn [m node-type]
                                                  (assoc m (:type node-type) (r/reactify-component (:component node-type))))
                                                {} node-types))
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
  [:f> flow])
