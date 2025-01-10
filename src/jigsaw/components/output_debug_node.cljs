(ns jigsaw.components.output-debug-node
  (:require
   [cljs.pprint :as pprint]
   [re-frame.core :as re-frame]
   [jigsaw.subs :as subs]
   [jigsaw.components.node :refer [node]]))

(defn output-debug-node [{:keys [id]}]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming id])]
    [node {:title "Debug" :handles [{:type "target" :position "left"}]}
     (if-let [incoming-node (first @incoming-nodes)]
       [:pre {:class "text-left"} (with-out-str (pprint/pprint incoming-node))]
       [:p "No input"])]))
