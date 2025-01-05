(ns jigsaw.components.output-debug-node
  (:require
   [cljs.pprint :as pprint]
   [reagent.core :as r]
   [re-frame.core :as re-frame]
   [jigsaw.subs :as subs]
   [jigsaw.components.node :refer [node handle]]))

(defn output-debug-node [props _]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming props])]
    [node {:title "Debug"}
     [handle {:type "target" :position "left"}]
     (if-let [incoming-node (first @incoming-nodes)]
       [:pre {:class "text-left"} (with-out-str (pprint/pprint incoming-node))]
       [:p "No input"])]))
