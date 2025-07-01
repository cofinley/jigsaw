(ns jigsaw.components.flow
  (:require
   [reagent.core :as r]
   [re-frame.core :as re-frame]
   [jigsaw.subs :as subs]
   [jigsaw.events :as events]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.context-menu :refer [node-context-menu]]
   [jigsaw.components.node-types :refer [node-types node-categories]]
   ["react" :refer [useMemo useState useRef useCallback]]
   ["@xyflow/react" :refer [ReactFlow
                            ReactFlowProvider
                            Background
                            BaseEdge
                            getBezierPath
                            Controls
                            applyNodeChanges
                            applyEdgeChanges
                            addEdge
                            Panel
                            useReactFlow]]))

(defn memoized-node
  "Memoize node so it only re-renders if data changes"
  [props]
  (let [{:keys [id type]} (js->clj props :keywordize-keys true)]
    (useMemo
     (fn []
       (r/as-element
        [(:component (first (filter #(= (:type %) (keyword type)) node-types)))
         {:id id :type type}]))
     #js [])))

(defn custom-edge [props]
  (let [{:keys [id sourceX sourceY targetX targetY data]} (js->clj props :keywordize-keys true)
        edge-path (getBezierPath #js {:sourceX sourceX
                                      :sourceY sourceY
                                      :sourcePosition "right"
                                      :targetPosition "left"
                                      :targetX targetX
                                      :targetY targetY})
        edge [BaseEdge #js {:id id
                            :className (if (:highlighted? data) "stroke-yellow-400" "")
                            :path edge-path}]]
    (r/as-element edge)))

(defn flow-content []
  (let [nodes (re-frame/subscribe [::subs/nodes])
        edges (re-frame/subscribe [::subs/edges])
        flow-instance (useReactFlow)
        on-nodes-change (fn [changes]
                          (re-frame/dispatch [::events/set-nodes (applyNodeChanges changes @nodes)]))
        on-edges-change (fn [changes]
                          (re-frame/dispatch [::events/set-edges (applyEdgeChanges changes @edges)]))
        on-connect (fn [params]
                     (re-frame/dispatch [::events/set-edges (addEdge (clj->js (assoc (js->clj params)
                                                                                     :id (str (.-source params) "->" (.-target params))
                                                                                     :type :custom-edge)) @edges)])
                     (re-frame/dispatch [::events/recompute (.-target params)]))
        ref (useRef nil)
        [node-menu set-node-menu] (useState nil)
        on-node-context-menu (useCallback
                              (fn [e node]
                                (.preventDefault e)
                                (let [pane (-> ref .-current .getBoundingClientRect)]
                                  (set-node-menu
                                   (cond-> {:top (and (< (.-clientY e) (- (.-height pane) 200)) (.-clientY e))
                                            :left (and (< (.-clientX e) (- (.-width pane) 200)) (.-clientX e))
                                            :right (and (>= (.-clientX e) (- (.-width pane) 200)) (- (.-width pane) (.-clientX e)))
                                            :bottom (and (>= (.-clientY e) (- (.-height pane) 200)) (- (.-height pane) (.-clientY e)))
                                            :mouse-x (.-clientX e)
                                            :mouse-y (.-clientY e)
                                            :flow-instance flow-instance}
                                     (some? node) (merge {:id (.-id node)
                                                          :type (keyword (.-type node))})))))
                              #js [set-node-menu flow-instance])
        on-pane-click (useCallback #(set-node-menu nil) #js [set-node-menu])
        on-drag-over (useCallback
                      (fn [e]
                        (.preventDefault e)
                        (set! (.-dropEffect (.-dataTransfer e)) "copy"))
                      #js [])
        on-drop (useCallback
                 (fn [e]
                   (.preventDefault e)
                   (let [drag-data (try
                                     (js/JSON.parse (.getData (.-dataTransfer e) "application/json"))
                                     (catch :default _
                                       nil))]
                     (when drag-data
                       (let [flow-pos (.screenToFlowPosition flow-instance #js {:x (.-clientX e) :y (.-clientY e)})
                             shape-data (as-> (js->clj drag-data :keywordize-keys true) m
                                          (assoc m :pitch (keyword (:pitch m)))
                                          (assoc m :name (keyword (:name m))))]
                         (re-frame/dispatch [::events/create-node-from-drag shape-data
                                             {:x (.-x flow-pos) :y (.-y flow-pos)}])))))
                 #js [flow-instance])
        flow-node-types (useMemo
                         #(clj->js
                           (reduce (fn [m node-type]
                                     (assoc m (:type node-type) memoized-node))
                                   {} node-types))
                         #js [])
        edge-types (useMemo #(clj->js {:custom-edge custom-edge}) #js [])]
    [:div {:style {:height "100%"}}
     [:> ReactFlow {:ref ref
                    :nodes (or @nodes #js [])
                    :edges (or @edges #js [])
                    :onNodesChange on-nodes-change
                    :onEdgesChange on-edges-change
                    :onConnect on-connect
                    :onNodeContextMenu on-node-context-menu
                    :onPaneContextMenu on-node-context-menu
                    :onPaneClick on-pane-click
                    :onDragOver on-drag-over
                    :onDrop on-drop
                    :nodeTypes flow-node-types
                    :edgeTypes edge-types
                    :fitView true
                    :colorMode "dark"}
      [:> Panel {:position "top-right"}
       [select {:on-change #(re-frame/dispatch [::events/add-node {:type (-> % .-target .-value)}])
                :value ""}
        (cons
         [:option {:disabled true :value ""} "(Add Node)"]
         (for [[cat-k cat-label] node-categories]
           [:optgroup {:label cat-label}
            (for [node-type node-types
                  :when (= cat-k (:category node-type))]
              ^{:key (:type node-type)} [:option {:value (:type node-type)} (:label node-type)])]))]]
      [:> Background]
      (when node-menu
        [node-context-menu (merge {:on-click on-pane-click} node-menu)])
      [:> Controls]]]))

(defn flow []
  [:> ReactFlowProvider
   [:f> flow-content]])
