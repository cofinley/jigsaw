(ns jigsaw.ui.components.info-panel
  (:require
   [clojure.string :as str]
   [jigsaw.impl.theory :as theory]
   [jigsaw.ui.components.output-circle-of-fifths-node :refer [output-circle-of-fifths-view]]
   [jigsaw.ui.components.output-music-staff-node :refer [output-music-staff-view]]
   [jigsaw.ui.subs :as subs]
   [jigsaw.utils :as utils :refer [pprint-aliases]]
   [re-frame.core :as re-frame]))

(defn info-panel []
  (let [selected-node (re-frame/subscribe [::subs/selected-node])]
    [:div
     (if (and @selected-node (seq (keys @selected-node)))
       [:div {:class "flex flex-col gap-y-4"}
        (for [[k v] @selected-node
              :when (utils/in? [:name :pitch :bass :note :notes :pitches :intervals :degrees :intervals] k)]
          [:p {:class "flex gap-2"}
           [:span {:class "font-semibold capitalize"} (str (name k) ": ")]
           [:span {:title (if (= :name k)
                            (str (->> v theory/name->shape pprint-aliases))
                            "")}
            (if (coll? v)
              (str/join " " (if (every? keyword? v)
                              (map name v)
                              v))
              v)]])
        (when (:notes @selected-node)
          [:h3 {:class "font-semibold"} "Music Staff"
           [output-music-staff-view {:data @selected-node}]])
        (when (:notes @selected-node)
          [:h3 {:class "font-semibold"} "Circle of Fifths"
           [output-circle-of-fifths-view {:data @selected-node}]])]
       [:p "No node selected"])]))
