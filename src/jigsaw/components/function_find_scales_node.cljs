(ns jigsaw.components.function-find-scales-node
  (:require
   [jigsaw.algo :as algo]
   [jigsaw.components.node :refer [node]]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.table :refer [table]]
   [jigsaw.events :as events]
   [jigsaw.search :as search]
   [jigsaw.spec :as specs]
   [jigsaw.subs :as subs]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]))

(defn function-find-scales-node [{:keys [id data]}]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming id])
        data (:data (events/js-node->clj-node {:data data}))]
    [node {:title "Chord Scales"
           :id id
           :data data
           :handles [{:type "target" :position "left"}
                     {:type "source" :position "right"}]}
     (if-let [incoming-data (first @incoming-nodes)]
       (if (contains? incoming-data :intervals)
         (let [selected-degree (or (:selected-degree data) :i)
               scales (search/chord->scales incoming-data)]
           [:div {:class "flex flex-col text-xl items-start space-y-4"}
            [:label {:class "space-x-4"}
             [:span "Degree"]
             [select {:value selected-degree
                      :on-change #(re-frame/dispatch [::events/update-node-data id {:selected-degree (-> % .-target .-value keyword)}])}
              (cons [:option {:value :all} "All"]
                    (for [deg (sort-by utils/parse-int specs/degrees)]
                      [:option {:value deg} deg]))]]
            [:p "Scale"]
            [table {:ms scales
                    :row-render {"Tonic" :pitch
                                 "Name" :name
                                 "Chord's Degree"
                                 #(algo/degree-chord->roman-numeral
                                   (:degree %)
                                   (:name incoming-data))}
                    :row-title-render utils/pprint-aliases
                    :row-selected? (fn [shape] (and
                                                (= (:pitch data) (:pitch shape))
                                                (= (:name data) (:name shape))
                                                (= (:degree data) (:degree shape))))
                    :on-row-click (fn [shape]
                                    (re-frame/dispatch [::events/update-node-data id
                                                        (merge shape (algo/resolve-shape (algo/pitch->note (:pitch shape)) :scale (:name shape)))])
                                    (re-frame/dispatch [::events/calculate-shape id]))}]])
         [:p "Input is not a chord"])
       [:p "No input"])]))
