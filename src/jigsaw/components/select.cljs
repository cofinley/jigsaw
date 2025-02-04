(ns jigsaw.components.select
  (:require
   [reagent.core :as r]))

(defn select [props options]
  [:select (r/merge-props {:class "p-1 rounded-md border border-gray-400 nodrag text-black"} props)
   (map #(with-meta % {:key (str %)}) options)])

