(ns jigsaw.ui.components.select
  (:require
   [reagent.core :as r]))

(defn select [props options]
  [:select (r/merge-props {:class "p-1 rounded-md border-2 border-neutral-400 nodrag"} props)
   (map #(with-meta % {:key (str %)}) options)])

