(ns jigsaw.components.function-find-shape-node
  (:require
   [clojure.string :as s]
   [jigsaw.algo :as algo]
   [jigsaw.components.node :refer [node]]
   [jigsaw.components.output-piano-node :refer [piano-preview]]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.events :as events]
   [jigsaw.search :as search]
   [jigsaw.spec :as specs]
   [jigsaw.subs :as subs]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]))

;; TODO: allow click-and-drag of table row into new input-shape node
(defn function-find-shape-node [{:keys [id]}]
  (let [data (re-frame/subscribe [::subs/data id])
        parent-data (re-frame/subscribe [::subs/parent-data id])]
    [node {:title "Compatible Shapes"
           :id id
           :data @data
           :parent-data @parent-data
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if @parent-data
       (if-let [notes (seq (get-in @parent-data [:notes]))]
         (let [incoming-shape-type (cond
                                     (contains? @parent-data :degrees) :scale
                                     (contains? @parent-data :intervals) :chord ; Scales have intervals too, but we didn't find :degrees
                                     :else :notes)
               ; Recommend finding scales by default if incoming shape is a chord, otherwise find chords
               selected-shape-type (or (:selected-shape-type @data) (if (= :chord incoming-shape-type) :scale :chord))
               selected-pitch (or (:selected-pitch @data) "")
               heuristic (or (:heuristic @data) :overlap)
               max-shapes (or (:max-shapes @data) 10)
               shapes (search/notes->shapes-memo notes
                                                 selected-shape-type
                                                 :max-shapes max-shapes
                                                 :heuristic (keyword heuristic)
                                                 :selected-pitch (if (= selected-pitch :all) nil selected-pitch))]
           [:div {:class "flex flex-col space-y-2 items-start text-xl"}
            [:label {:class "space-x-4"}
             [:span {:class "font-semibold"} "Find"]
             [select {:class "w-max"
                      :on-change #(re-frame/dispatch [::events/update-node-data id {:selected-shape-type (keyword (-> % .-target .-value))}])
                      :value (or selected-shape-type "")}
              [[:option {:value :chord} "Chords"]
               [:option {:value :scale} "Scales"]]]]
            [:label {:class "space-x-4 inline-flex items-baseline"}
             [:span {:class "font-semibold"} "Where"]
             [:span "input"]
             [select {:class "w-max"
                      :on-change #(re-frame/dispatch [::events/update-node-data id {:heuristic (keyword (-> % .-target .-value))}])
                      :value heuristic}
              (for [[value label] search/heuristic-labels]
                [:option {:value value} label])]
             [:span (str "the " (name selected-shape-type))]]
            [:label {:class "space-x-4"}
             [:span {:class "font-semibold"} (if (= selected-shape-type :chord) "Root" "Tonic")]
             [select {:class "w-max"
                      :on-change #(re-frame/dispatch [::events/update-node-data id {:selected-pitch (keyword (-> % .-target .-value))}])
                      :value selected-pitch}
              (cons [:option {:value "all"} "(Show all)"]
                    (for [pitch (keys (sort-by val < specs/pitches))
                          :when (and (not (s/includes? (name pitch) "bb"))
                                     (not (s/includes? (name pitch) "##")))]
                      [:option {:value pitch} (name pitch)]))]]
            [:label {:class "space-x-4"}
             [:span {:class "font-semibold"} "Max shapes"]
             [:input {:class "p-1 rounded-md border border-gray-400 nodrag text-black"
                      :type "number"
                      :size 2
                      :value max-shapes
                      :on-change #(re-frame/dispatch [::events/update-node-data id {:max-shapes (int (-> % .-target .-value))}])}]]
            ;; TODO: make search an event and update data with results, otherwise it blocks event loop/animation
            [table {:ms shapes
                    :row-render {(if (= selected-shape-type :chord) "Root" "Tonic") :pitch
                                 "Name" :name
                                 "Overlap" #(str (int (* 100 (get-in % [:heuristics :overlap]))) "%")
                                 ; "Piano" (fn [shape] [output-piano-view {:data shape :key-width 20 :display-label-options? false}])
                                 "Piano" (fn [shape]
                                           (when (:name shape)
                                             [piano-preview
                                              (:notes shape)
                                              :parent-notes (:notes @parent-data)]))}
                    :row-title-render utils/pprint-aliases
                    :row-selected? (fn [shape] (and (= (:pitch @data) (:pitch shape)) (= (:name @data) (:name shape))))
                    :on-row-click (fn [shape]
                                    (re-frame/dispatch [::events/update-node-data id shape]))}]])
         [:p "No notes in input"])
       [:p "No input"])]))
