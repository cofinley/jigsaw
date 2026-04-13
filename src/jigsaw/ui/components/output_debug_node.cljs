(ns jigsaw.ui.components.output-debug-node
  (:require
   [cljs.pprint :as pprint]
   [reagent.core :as r]))

(defn output-debug-view [props]
  [:pre (r/merge-props {:class "text-left"} (dissoc props :data))
   (with-out-str (pprint/pprint (:data props)))])
