(ns jigsaw.ui.components.info-panel
  (:require
   [clojure.string :as str]
   [jigsaw.ui.components.output-circle-of-fifths-node :refer [output-circle-of-fifths-view]]
   [jigsaw.ui.components.output-music-staff-node :refer [output-music-staff-view]]
   [jigsaw.ui.subs :as subs]
   [jigsaw.utils :as utils]
   [re-frame.core :as re-frame]))

(defn info-panel []
  (let [selected-node (re-frame/subscribe [::subs/selected-node])]
    [:div
     (if (and @selected-node (seq (keys @selected-node)))
       [:div {:class "flex flex-col gap-y-4"}
        (for [[k v] @selected-node
              :when (not (utils/in? [:type :view-type :pcis] k))]
          [:p {:class "flex gap-2"}
           [:span {:class "font-semibold capitalize"} (str (name k) ": ")]
           [:span (if (coll? v)
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
