(ns jigsaw.ui.components.button
  (:require [clojure.string :as str]))

(defn button [{:keys [on-click class title disabled]
               :or {title ""}} & children]
  [:button
   (cond-> {:class (str/join " " ["px-2 py-1 text-sm border cursor-pointer hover:dark:bg-neutral-100 hover:dark:text-neutral-900 rounded group disabled:pointer-events-none disabled:opacity-50" class])
            :on-click on-click
            :title title}
     disabled (assoc :disabled "disabled"))
   children])
