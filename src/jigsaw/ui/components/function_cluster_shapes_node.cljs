(ns jigsaw.ui.components.function-cluster-shapes-node
  (:require
   [jigsaw.core :as jigsaw]
   [jigsaw.impl.theory :as theory]
   [jigsaw.ui.components.button :refer [button]]
   [jigsaw.ui.components.node :refer [node]]
   [jigsaw.ui.components.output-piano-node :refer [piano-preview]]
   [jigsaw.ui.components.table :refer [table]]
   [jigsaw.ui.events :as events]
   [jigsaw.ui.subs :as subs]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]
   [reagent.core :as r]))

(defn matched-shape [parent-data id {:keys [input found context]}]
  ^{:key (str (:pitch found) (:name found) (name context))}
  [:div
   {:class "flex gap-2 justify-between items-center py-2 pl-2 hover:text-yellow-400"
    :onMouseOut (fn []
                  (re-frame/dispatch [::events/clear-edge-highlighting]))
    :onMouseOver (fn []
                   (let [matching-incoming-node-ids (if (seq input)
                                                      ; Coming from input-piano
                                                      (map :id (filter
                                                                (fn [parent-node-data]
                                                                  (= input (set (:notes parent-node-data))))
                                                                parent-data))
                                                      ; Coming from input-chord/scale
                                                      [(:id found)])]
                     ; Highlight matching nodes' edges
                     (doall
                      (for [incoming-node-id (map :id parent-data)
                            :let [edge-id (str incoming-node-id "->" id)
                                  highlighted? (utils/in? matching-incoming-node-ids incoming-node-id)]]
                        (re-frame/dispatch [::events/update-edge-props edge-id {:data #js {:highlighted? highlighted?}}])))))}
   [:p
    {:title (when-let [bass (:bass found)]
              (if-let [inversion (theory/bass->inversion (jigsaw/->shape found) bass)]
                (case inversion
                  1 "1st inversion"
                  2 "2nd inversion"
                  3 "3rd inversion"
                  4 "4th inversion"
                  "")
                (str "Slash chord; " (name bass) " not in chord")))}
    (str (name (:pitch found))
         (name (:name found))
         (if-let [bass (:bass found)]
           (str "/" (name bass)) "")
         " ("
         (name context)
         (when-let [overlap (get-in found [:heuristics :overlap])]
           (str ", "
                (int (* 100 overlap))
                "%"))
         ")")]
   [piano-preview found :parent-notes input]])

(defn function-cluster-shapes-node [props]
  (let [page (r/atom 1)]
    (fn [{:keys [id]}]
      (let [data (re-frame/subscribe [::subs/data id])
            parent-data (re-frame/subscribe [::subs/multi-parent-data id])
            results (re-frame/subscribe [::subs/function-result id])
            connecting? (re-frame/subscribe [::subs/connecting? id])]
        [node {:title "Cluster Shapes"
               :id id
               :data @data
               :parent-data @parent-data
               :handles [{:type "target" :position "left"}
                         {:type "source" :position "right"}]}
         (when @connecting?
           [:span "Connecting..."])
         [:div {:class "flex gap-4"}
          [:label {:class "flex gap-2 items-center mb-4 text-xl"}
           [:span {:class "font-semibold "} "Max Clusters"]
           [:input {:type "number"
                    :value (or (:max-clusters @data) 1)
                    :on-change #(re-frame/dispatch [::events/update-node-data id {:max-clusters (-> % .-target .-value int)}])
                    :class "p-1 rounded-md border-2 border-neutral-400 nodrag"
                    :size 2}]]
          [:label {:class "flex gap-2 items-center mb-4 text-xl"}
           [:span {:class "font-semibold "} "Max Shapes"]
           [:input {:type "number"
                    :value (or (:max-shapes @data) 1)
                    :on-change #(re-frame/dispatch [::events/update-node-data id {:max-shapes (-> % .-target .-value int)}])
                    :class "p-1 rounded-md border-2 border-neutral-400 nodrag"
                    :size 2}]]
          [:label {:class "flex gap-2 items-center mb-4 text-xl"}
           [:span {:class "font-semibold "} "Max Results"]
           [:input {:type "number"
                    :value (or (:max-results @data) 1)
                    :on-change #(re-frame/dispatch [::events/update-node-data id {:max-results (-> % .-target .-value int)}])
                    :class "p-1 rounded-md border-2 border-neutral-400 nodrag"
                    :size 2}]]]
         (if (> (count @parent-data) 1)
           (if (and @results (< 0 (count @results)))
             [:div {:class "flex flex-col gap-4"}
              [:div {:class "flex gap-2"}
               [button {:disabled (= @page 1)
                        :on-click #(swap! page dec)} "<"]
               [button {:disabled (= @page (count @results))
                        :on-click #(swap! page inc)} ">"]]
              (let [result (nth @results (dec @page))]
                (for [i (range (count (:clusters result)))
                      :let [cluster (nth (:clusters result) i)
                            connections (nth (:connections-by-cluster result) i)]]
                  [:div
                   [:div {:class "flex gap-4 text-2xl mb-4"}
                    [:span {:class "font-semibold"} "Cluster:"]
                    (map (fn [shape-ref]
                           [:span (str (name (:pitch shape-ref))
                                       (name (:name shape-ref)))]) cluster)]
                   [table {:ms connections
                           :row-render {"Pitch" :pitch
                                        "Name" :name
                                        "Piano" (fn [shape-ref]
                                                  [piano-preview shape-ref])
                                        "Inputs" (fn [shape-ref]
                                                   (let [shape (jigsaw/->shape shape-ref)
                                                         cluster-trace (filter (fn [[_ found]]
                                                                                 (utils/in? cluster (theory/->shape-ref found))) (:trace result))]
                                                     (doall
                                                      (for [[input found] cluster-trace]
                                                        (let [context (jigsaw/contextualize shape (jigsaw/->shape found))]
                                                          ^{:key (str i shape-ref input context)}
                                                          [matched-shape @parent-data id {:input input
                                                                                          :found found
                                                                                          :context context}])))))}
                           :on-row-click (fn [shape-ref] (re-frame/dispatch [::events/update-node-data id (jigsaw/->shape (theory/pitch->note (:pitch shape-ref)) (:name shape-ref))]))
                           :row-selected? (fn [shape-ref] (and (= (:pitch @data) (:pitch shape-ref))
                                                               (= (:name @data) (:name shape-ref))))}]]))]
             [:p {:class "text-lg"} "No results"])
           [:p {:class "text-lg"} "Connect more than one"])]))))
