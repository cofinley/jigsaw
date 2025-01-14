(ns jigsaw.components.function-find-shape-node
  (:require
   [clojure.string :as s]
   [re-frame.core :as re-frame]
   [jigsaw.search :as search]
   [jigsaw.subs :as subs]
   [jigsaw.events :as events]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.node :refer [node]]))

(defn selected-shape-value [data]
  (let [{pitch :pitch selected-shape-type :selected-shape-type -name :name} data]
    (if (and (some? pitch) (some? selected-shape-type) (some? name))
      (str (name pitch) "_" (name selected-shape-type) "_" (name -name))
      "")))

(defn function-find-shape-node [{:keys [id]}]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming id])
        data (re-frame/subscribe [::subs/data id])]
    [node {:title "Find Shape"
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if-let [incoming-data (first @incoming-nodes)]
       (if-let [notes (seq (get-in incoming-data [:notes]))]
         (let [selected-shape-type (or (:selected-shape-type @data) :chord)
               similarity-type (or (:similarity-type @data) :overlap)]
           [:div {:class "flex flex-col space-y-2"}
            [select {:class "w-max"
                     :on-change #(re-frame/dispatch [::events/update-node-data id {:selected-shape-type (keyword (-> % .-target .-value))}])
                     :value (or selected-shape-type "")}
             [[:option {:value :chord} "Chord"]
              [:option {:value :scale} "Scale"]]]
            (let [shapes (search/notes->shapes notes selected-shape-type similarity-type)
                  pitches (set (map :pitch shapes))
                  pitch->shapes (reduce (fn [m shape]
                                          (update m (:pitch shape) (fnil conj []) shape))
                                        {}
                                        shapes)]
              [select {:class "text-xl"
                       :value (selected-shape-value (merge {:selected-shape-type selected-shape-type} @data))
                       :on-change (fn [e]
                                    (let [value (-> e .-target .-value)]
                                      (when (not= "" value)
                                        (let [[pitch shape-type shape-name] (map keyword (s/split value #"_"))]
                                          (re-frame/dispatch [::events/set-selected-shape
                                                              id
                                                              (keyword shape-type)
                                                              (first (filter #(and (= (:pitch %) pitch)
                                                                                   (= (:name %) shape-name))
                                                                             shapes))])))))}
               (cons
                [:option {:value ""} "(Select " selected-shape-type ")"]
                (for [pitch pitches
                      :let [shapes (get pitch->shapes pitch)]]
                  [:optgroup {:label (str (name pitch))}
                   (for [shape shapes
                         :when (some? shape)
                         :let [aliases (:aliases shape)]]
                     ^{:key shape} [:option {:value (selected-shape-value {:pitch pitch :selected-shape-type selected-shape-type :name (:name shape)})
                                             :title (when (seq aliases) (str "Aliases:\n" (s/join "\n" (map #(str "- " %) aliases))))}
                                    (str (name pitch) " " (name (:name shape)) " (" (int (* 100 (:similarity shape))) "% match)")])]))])])
         [:p "Need notes in input"])
       [:p "No input"])]))
