(ns jigsaw.components.function-scale-chords-node
  (:require
   [clojure.string :as s]
   [re-frame.core :as re-frame]
   [jigsaw.algo :as algo]
   [jigsaw.subs :as subs]
   [jigsaw.spec :as specs]
   [jigsaw.events :as events]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.node :refer [node]]))

(defn function-scale-chords-node [{:keys [id]}]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming id])
        data (re-frame/subscribe [::subs/data id])]
    [node {:title "Scale Chords"
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if-let [incoming-data (first @incoming-nodes)]
       (if (contains? incoming-data :degrees)
         (let [selected-chord (:selected-chord @data)
               selected-match-type (or (:match-type @data) :diatonic)
               chords-list (algo/scale-chords incoming-data :exact? (= :diatonic selected-match-type))
               pitches (:pitches incoming-data)
               pitch->chord-list (zipmap pitches chords-list)
               pitch->degrees (zipmap pitches (:degrees incoming-data))]
           [:div {:class "flex flex-col text-xl items-start space-y-4"}
            [:label {:class "space-x-4"}
             [:span "Match Type"]
             [select {:class "w-max"
                      :value selected-match-type
                      :on-change #(re-frame/dispatch [::events/update-node-data id {:match-type (-> % .-target .-value keyword)}])}
              (map #(vector :option {} (name %)) [:diatonic :subset])]]
            [:label {:class "space-x-4"}
             [:span "Chord"]
             [select {:class "text-xl"
                      :value (or selected-chord "")
                      :on-change (fn [e]
                                   (let [value (-> e .-target .-value)
                                         [pitch name] (map keyword (s/split value #"_"))]
                                     (re-frame/dispatch [::events/set-selected-chord id value])
                                     (re-frame/dispatch [::events/set-pitch id pitch])
                                     (re-frame/dispatch [::events/set-name id name])))}
              (cons
               [:option {:disabled true :value ""} "(Select Chord)"]
               (for [pitch pitches
                     :let [chord-list (get pitch->chord-list pitch)]]
                 [:optgroup {:label (str (name pitch))}
                  (for [chord chord-list
                        :when (some? chord)
                        :let [aliases (get-in specs/chords [chord ::specs/aliases])]]
                    ^{:key chord} [:option {:value (str (name pitch) "_" (name chord))
                                            :title (when (seq aliases) (str "Aliases:\n" (s/join "\n" (map #(str "- " %) aliases))))}
                                   (str (name pitch) (name chord) " (" (name (algo/degree-chord->roman-numeral (get pitch->degrees pitch) chord)) ")")])]))]]])
         [:p "Input is not a scale"])
       [:p "No input"])]))
