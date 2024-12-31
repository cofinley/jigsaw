(ns jigsaw.views
  (:require
   [reagent.core :as r]
   [re-frame.core :as re-frame]
   [re-com.core :as re-com :refer [at]]
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
                            Panel]]))

(def default-pitch-by-index
  {0 :C
   1 :C#
   2 :D
   3 :Eb
   4 :E
   5 :F
   6 :F#
   7 :G
   8 :Ab
   9 :A
   10 :Bb
   11 :B})

(defn midi->note [midi _key]
  (let [octave (dec (int (/ midi 12)))
        index (mod midi 12)
        p (get default-pitch-by-index index)]
    (keyword (str (name p) octave))))

(defn input-piano-node [props _]
  (let [node (js->clj props :keywordize-keys true)
        id (:id node)
        active-midis (re-frame/subscribe [::subs/active-midis id])]
    (r/as-element
     [:div {:class "react-flow__node-default" :style {:width "100%"}}
      [:div {:class "nodrag"}
       (let [first-midi 60
             octaves 2
             last-midi (dec (+ first-midi (* octaves 12)))
             key-width 10
             width (* key-width (- last-midi first-midi))]
         [:> ControlledPiano
          {:class "nodrag"
           :noteRange {:first first-midi :last last-midi}
           :playNote (fn [midi] midi)
           :stopNote #()
           :activeNotes @active-midis
           :onPlayNoteInput (fn [midi _] (re-frame/dispatch [::events/toggle-midi id midi]))
           :onStopNoteInput #()
           :width width}])]
      [:> Handle {:type "source" :position "right"}]])))

(defn output-piano-node [props _]
  (let [node (js->clj props :keywordize-keys true)
        incoming-nodes (re-frame/subscribe [::subs/incoming node])
        incoming-node (first @incoming-nodes)]
    (r/as-element
     [:div {:class "react-flow__node-default" :style {:width "100%"}}
      [:> Handle {:type "target" :position "left"}]
      [:div {:style {:pointerEvents "none"}}
       (if incoming-node
         (let [; notes (get-in incoming-node [:data :notes])
               ; midis (map #(.fromNote MidiNumbers %) notes)
               midis (get-in incoming-node [:data :midis])
               ; midi->note (zipmap midis notes)
               sorted-midis (sort midis)]
           (if (seq sorted-midis)
             (let [key-width 10
                   width (* key-width (- (last sorted-midis) (first sorted-midis)))
                   first-midi (first sorted-midis)
                   last-midi (last sorted-midis)
                   midi-range-start (- first-midi (mod first-midi 12))
                   midi-range-end (dec (+ last-midi (- 12 (mod last-midi 12))))]
               [:> Piano
                {:noteRange {:first midi-range-start :last midi-range-end}
                 :playNote #()
                 :stopNote #()
                 :renderNoteLabel (fn [_data]
                                    (let [{midi :midiNumber active? :isActive} (js->clj _data :keywordize-keys true)]
                                      (when active?
                                        (r/as-element [:span {:style {:font-size "0.6rem"}} (midi->note midi nil)]))))
                 :activeNotes midis
                 :width width}])
             [:p "No input"]))
         [:p "No input"])]])))

(def node-types
  #js {:input-piano input-piano-node
       :output-piano output-piano-node})

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
                    :defaultEdgeOptions {:type "step"}
                    :onConnect on-connect
                    :nodeTypes node-types
                    :fitView true
                    :colorMode "dark"}
      [:> Panel {:position "top-right"}
       [re-com/md-icon-button {:md-icon-name "material-symbols-out-home"}]
       [:select {:on-change #(re-frame/dispatch [::events/add-node (-> % .-target .-value)])
                 :default-value ""}
        [:option {:disabled true :value ""} "(Add Node)"]
        [:optgroup {:label "Input"}
         [:option {:value "input-piano"} "Piano"]]
        [:optgroup {:label "Function"}]
        [:optgroup {:label "Output"}
         [:option {:value "output-piano"} "Piano"]]]]
      [:> Background]
      [:> Controls]]]))

(defn main-panel []
  [re-com/v-box
   :src      (at)
   :height   "100%"
   :children [[:f> flow]]])
