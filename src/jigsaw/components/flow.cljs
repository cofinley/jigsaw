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
                            Background
                            Controls
                            applyNodeChanges
                            applyEdgeChanges
                            addEdge
                            Panel]]))

(defn memoized-node
  "Memoize node so it only re-renders if data changes"
  [props]
  (let [{:keys [id type data]} (js->clj props :keywordize-keys true)]
    (useMemo
     (fn []
       (r/as-element
        [(:component (first (filter #(= (:type %) (keyword type)) node-types)))
         {:id id :type type :data data}]))
     #js [data])))

(defn flow []
  (let [nodes (re-frame/subscribe [::subs/nodes])
        edges (re-frame/subscribe [::subs/edges])
        on-nodes-change (fn [changes]
                          (re-frame/dispatch [::events/set-nodes (js->clj (applyNodeChanges changes (clj->js @nodes)) :keywordize-keys true)]))
        on-edges-change (fn [changes]
                          (re-frame/dispatch [::events/set-edges (js->clj (applyEdgeChanges changes (clj->js @edges)) :keywordize-keys true)]))
        on-connect (fn [params]
                     (re-frame/dispatch [::events/set-edges (js->clj (addEdge params (clj->js @edges)) :keywordize-keys true)]))
        ref (useRef nil)
        [node-menu set-node-menu] (useState nil)
        on-node-context-menu (useCallback (fn [e node]
                                            (.preventDefault e)
                                            (let [pane (-> ref .-current .getBoundingClientRect)]
                                              (set-node-menu {:id (.-id node)
                                                              :top (and (< (.-clientY e) (- (.-height pane) 200)) (.-clientY e))
                                                              :left (and (< (.-clientX e) (- (.-width pane) 200)) (.-clientX e))
                                                              :right (and (>= (.-clientX e) (- (.-width pane) 200)) (- (.-width pane) (.-clientX e)))
                                                              :bottom (and (>= (.-clientY e) (- (.-height pane) 200)) (- (.-height pane) (.-clientY e)))})))
                                          #js [set-node-menu])
        on-pane-click (useCallback #(set-node-menu nil) #js [set-node-menu])
        flow-node-types (useMemo #(clj->js (reduce (fn [m node-type]
                                                     (assoc m (:type node-type) memoized-node))
                                                   {} node-types)) #js [])]
    [:div {:style {:height "100%"}}
     [:> ReactFlow {:ref ref
                    :nodes (clj->js @nodes)
                    :edges (clj->js @edges)
                    :onNodesChange on-nodes-change
                    :onEdgesChange on-edges-change
                    :onConnect on-connect
                    :onNodeContextMenu on-node-context-menu
                    :onPaneClick on-pane-click
                    :nodeTypes flow-node-types
                    :fitView true
                    :colorMode "dark"}
      [:> Panel {:position "top-right"}
       [select {:on-change #(re-frame/dispatch [::events/add-node (-> % .-target .-value)])
                :value ""}
        (cons
         [:option {:disabled true :value ""} "(Add Node)"]
         (for [[cat-k cat-label] node-categories]
           [:optgroup {:label cat-label}
            (for [node-type node-types
                  :when (= cat-k (:category node-type))]
              ^{:key node-type} [:option {:value (:type node-type)} (:label node-type)])]))]]
      [:> Background]
      (when node-menu
        [node-context-menu (merge {:on-click on-pane-click} node-menu)])
      [:> Controls]]]))
