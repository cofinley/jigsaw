(ns jigsaw.components.function-find-shape-node
  (:require
   [clojure.string :as s]
   [re-frame.core :as re-frame]
   [jigsaw.algo :as algo]
   [jigsaw.search :as search]
   [jigsaw.subs :as subs]
   [jigsaw.events :as events]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.node :refer [node]]))

(defn function-find-shape-node [{:keys [id]}]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming id])
        data (re-frame/subscribe [::subs/data id])]
    [node {:title "Find Shape"
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if-let [incoming-node (first @incoming-nodes)]
       (if-let [notes (seq (get-in incoming-node [:data :notes]))]
         (let [selected-shape-type (:selected-shape-type @data)]
           [:<>
            [select {:on-change #(re-frame/dispatch [::events/update-node-data id {:selected-shape-type (keyword (-> % .-target .-value))}])
                     :value (or selected-shape-type "")}
             [[:option {:value :chord} "Chord"]
              [:option {:value :scale} "Scale"]]]
            (let [shapes (search/notes->shapes (:selected-shape-type @data) notes)
                  pitches (set (map :pitch shapes))
                  pitch->shapes (reduce (fn [m shape]
                                          (update m (:pitch shape) (fnil conj []) shape))
                                        {}
                                        shapes)]
              [select {:class "text-xl"
                       :value (or (str (name (:pitch @data)) "_" selected-shape-type "_" (name (:name @data))) "")
                       :on-change (fn [e]
                                    (let [value (-> e .-target .-value)
                                          [pitch shape-type name] (map keyword (s/split value #"_"))]
                                      (re-frame/dispatch [::events/set-selected-shape id (first (filter #(= (:name %) name) shapes))])
                                      (re-frame/dispatch [::events/set-pitch id pitch])
                                      (re-frame/dispatch [::events/set-name id name])))}
               (cons
                [:option {:disabled true :value ""} "(Select " selected-shape-type ")"]
                (for [pitch pitches
                      :let [shapes (get pitch->shapes pitch)]]
                  [:optgroup {:label (str (name pitch))}
                   (for [shape shapes
                         :when (some? shape)
                         :let [aliases (:aliases shape)]]
                     ^{:key shape} [:option {:value (str (name pitch) "_" selected-shape-type "_" (name (:name shape)))
                                             :title (when (seq aliases) (str "Aliases:\n" (s/join "\n" (map #(str "- " %) aliases))))}
                                    (str (name pitch) " " (name (:name shape)))])]))])])
         [:p "Need notes in input"])
       [:p "No input"])]))
