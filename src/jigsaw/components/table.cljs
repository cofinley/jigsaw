(ns jigsaw.components.table
  (:require [clojure.string :as s]))

(defn table [{:keys [ms col-render selected-row-fn on-row-hover on-row-click row-title-fn]}]
  [:div {:class "max-h-72 w-full overflow-scroll nowheel nodrag block text-left"}
   [:table
    [:thead
     [:tr {:class "sticky w-full top-0 bg-neutral-700"}
      (for [header (keys col-render)]
        ^{:key (str "header-" header)}
        [:th {:class "text-xl"} header])]]
    [:tbody
     (for [m ms]
       ^{:key m}
       [:tr {:class (s/join
                     " "
                     [(if (and (some? selected-row-fn) (selected-row-fn m))
                        "bg-indigo-500"
                        "bg-neutral-800 even:bg-neutral-900")
                      (if (and (some? selected-row-fn) (selected-row-fn m))
                        "hover:bg-indigo-400"
                        "hover:bg-neutral-700")
                      "hover:cursor-pointer"])
             :onMouseOver #(when on-row-hover (on-row-hover m))
             :onClick #(when on-row-click (on-row-click m))
             :title (when row-title-fn (row-title-fn m))}
        (for [[header col-render-fn] col-render]
          ^{:key (str "row-" header)}
          [:td {:class "text-xl"} (col-render-fn m)])])]]])
