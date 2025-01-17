(ns jigsaw.components.table
  (:require [clojure.string :as s]))

(defn table [{:keys [ms row-render row= on-row-hover on-row-click row-title-render]}]
  [:div {:class "max-h-72 w-full overflow-scroll nowheel nodrag"}
   [:table
    [:thead
     [:tr {:class "sticky w-full top-0 bg-neutral-700"}
      (for [header (keys row-render)]
        ^{:key (str "header-" header)}
        [:th {:class "text-xl"} header])]]
    [:tbody
     (for [m ms]
       ^{:key m}
       [:tr {:class (s/join
                     " "
                     [(if (and (some? row=) (row= m))
                        "bg-indigo-500"
                        "bg-neutral-800 even:bg-neutral-900")
                      (if (and (some? row=) (row= m))
                        "hover:bg-indigo-400"
                        "hover:bg-neutral-700")
                      "hover:cursor-pointer"])
             :onMouseOver #(when on-row-hover (on-row-hover m))
             :onClick #(when on-row-click (on-row-click m))
             :title (when row-title-render (row-title-render m))}
        (for [[header col-render-fn] row-render]
          ^{:key (str "row-" header)}
          [:td {:class "text-xl"} (col-render-fn m)])])]]])
