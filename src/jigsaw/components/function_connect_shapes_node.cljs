(ns jigsaw.components.function-connect-shapes-node
  (:require
   [jigsaw.algo :as algo]
   [jigsaw.components.node :refer [node]]
   [jigsaw.components.output-piano-node :refer [piano-preview]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.events :as events]
   [jigsaw.search :as search]
   [jigsaw.subs :as subs]
   [re-frame.core :as re-frame]
   [reagent.core :as r]))

(defn matched-shape [node-id shape]
  [:div
   {:class "flex gap-2 justify-between items-center"
    ;:onMouseOver #(re-frame/dispatch [::events/update-node-data node-id {:hovered-shape shape}])
    }
   [:p (str (name (get-in shape [:found :pitch]))
            (name (get-in shape [:found :name]))
            " ("
            (name (:context shape))
            ", "
            (int (* 100 (get-in shape [:found :heuristics :overlap])))
            "%"
            ")")]
   [piano-preview
    (:notes (algo/resolve-shape (algo/pitch->note (:pitch (:found shape))) :chord (:name (:found shape))))]])

(defn function-connect-shapes-node [{:keys [id]}]
  (let [max-shapes (r/atom 2)]
    (fn []
      (let [data (re-frame/subscribe [::subs/data id])
            parent-data (re-frame/subscribe [::subs/multi-parent-data id])]
        [node {:title "Connect Shapes"
               :id id
               :data @data
               :parent-data @parent-data
               :handles [{:type "target" :position "left"}
                         {:type "source" :position "right"}]}
         [:label {:class "flex gap-2 items-center mb-4 text-xl"}
          [:span {:class "font-semibold "} "Max Shapes"]
          [:input {:type "number"
                   :value @max-shapes
                   :on-change #(reset! max-shapes (-> % .-target .-value int))
                   :class "p-1 rounded-md border border-gray-400 nodrag text-black"
                   :size 2}]]
         (if (> (count @parent-data) 1)
           (let [connections (if (every? #(contains? % :name) @parent-data)
                               (search/memoize-connect-shapes @parent-data :chord)
                               (search/memoize-connect (map :notes @parent-data) :chord :max-shapes @max-shapes))]
             [table {:ms (sort-by (comp count second) > connections)
                     :key-fn (fn [m] ((juxt (comp :pitch first) (comp :name first)) m))
                     :row-render {"Pitch" (comp :pitch first)
                                  "Shape" (comp :name first)
                                  "Piano" (fn [[comp-shape _]]
                                            (when (:name comp-shape)
                                              [piano-preview
                                               (:notes (algo/resolve-shape (algo/pitch->note (:pitch comp-shape)) (:type comp-shape) (:name comp-shape)))]))
                                  "Inputs" #(->> %
                                                 second
                                                 (map (partial matched-shape id)))}
                     :on-row-click (fn [[comp-shape _]] (re-frame/dispatch [::events/update-node-data id (algo/resolve-shape (algo/pitch->note (:pitch comp-shape)) (:type comp-shape) (:name comp-shape))]))
                     :row-selected? (fn [[comp-shape _]] (and (= (:pitch @data) (:pitch comp-shape))
                                                              (= (:name @data) (:name comp-shape))))}])
           [:p "Connect more than one"])]))))
