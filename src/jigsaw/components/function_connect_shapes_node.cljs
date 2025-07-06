(ns jigsaw.components.function-connect-shapes-node
  (:require
   [jigsaw.algo :as algo]
   [jigsaw.components.node :refer [node]]
   [jigsaw.components.output-piano-node :refer [piano-preview]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.events :as events]
   [jigsaw.subs :as subs]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]))

(defn matched-shape [parent-data id {:keys [input found context]}]
  ^{:key (str (:pitch found) (:name found) (name context))}
  [:div
   {:class "flex gap-2 justify-between items-center py-2 pl-2 hover:text-yellow-400"
    :onMouseOut (fn []
                  (re-frame/dispatch [::events/clear-edge-highlighting]))
    :onMouseOver (fn []
                   (let [inputs (map set input)
                         matching-incoming-node-ids (if (seq inputs)
                                                      ; Coming from input-piano
                                                      (map :id (filter
                                                                (fn [parent-node-data]
                                                                  (utils/in? inputs (set (:notes parent-node-data))))
                                                                parent-data))
                                                      ; Coming from input-chord/scale
                                                      [(:id found)])]
                     ; Highlight matching nodes' edges
                     (doall
                      (for [incoming-node-id (map :id parent-data)
                            :let [edge-id (str incoming-node-id "->" id)
                                  highlighted? (utils/in? matching-incoming-node-ids incoming-node-id)]]
                        (re-frame/dispatch [::events/update-edge-props edge-id {:data #js {:highlighted? highlighted?}}])))))}
   [:p (str (name (:pitch found))
            (name (:name found))
            " ("
            (name context)
            (when-let [overlap (get-in found [:heuristics :overlap])]
              (str ", "
                   (int (* 100 overlap))
                   "%"))
            ")")]
   [piano-preview found :parent-notes (first input)]])

(defn function-connect-shapes-node [{:keys [id]}]
  (let [data (re-frame/subscribe [::subs/data id])
        parent-data (re-frame/subscribe [::subs/multi-parent-data id])
        connections (re-frame/subscribe [::subs/function-result id])]
    [node {:title "Connect Shapes"
           :id id
           :data @data
           :parent-data @parent-data
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     [:label {:class "flex gap-2 items-center mb-4 text-xl"}
      [:span {:class "font-semibold "} "Max Shapes"]
      [:input {:type "number"
               :value (or (:max-shapes @data) 1)
               :on-change #(re-frame/dispatch [::events/update-node-data id {:max-shapes (-> % .-target .-value int)}])
               :class "p-1 rounded-md border border-gray-400 nodrag text-black"
               :size 2}]]
     (if (> (count @parent-data) 1)
       (when @connections
         [table {:ms (sort-by (comp count second) > @connections)
                 :key-fn (fn [m] ((juxt (comp :pitch first) (comp :name first)) m))
                 :row-render {"Pitch" (comp :pitch first)
                              "Shape" (comp :name first)
                              "Piano" (fn [[comp-shape _]]
                                        (when (:name comp-shape)
                                          [piano-preview comp-shape]))
                              "Inputs" #(->> %
                                             second
                                             (map (partial matched-shape @parent-data id)))}
                 :on-row-click (fn [[comp-shape _]] (re-frame/dispatch [::events/update-node-data id (algo/->shape (algo/pitch->note (:pitch comp-shape)) (:name comp-shape))]))
                 :row-selected? (fn [[comp-shape _]] (and (= (:pitch @data) (:pitch comp-shape))
                                                          (= (:name @data) (:name comp-shape))))}])
       [:p {:class "text-lg"} "Connect more than one"])]))
