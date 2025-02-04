(ns jigsaw.components.table
  (:require [clojure.string :as s]))

(defn table [{:keys [ms
                     row-render
                     row-selected?
                     on-row-hover
                     on-row-click
                     row-title-render
                     row-filter]}]
  [:div {:class "max-h-72 w-full overflow-scroll nowheel nodrag"}
   [:table
    [:thead
     [:tr {:class "sticky w-full top-0 bg-neutral-700"}
      (doall (for [header (keys row-render)]
               ^{:key (str "header-" header)}
               [:th {:class "text-xl"} header]))]]
    [:tbody
     (for [m ms
           :when (if (some? row-filter)
                   (row-filter m)
                   true)]
       (let [selected? (and (some? row-selected?) (row-selected? m))]
         ^{:key m}
         [:tr {:class (s/join
                       " "
                       [(if selected?
                          "bg-indigo-500 hover:bg-indigo-400"
                          "bg-neutral-800 even:bg-neutral-900 hover:bg-neutral-700")
                        "hover:cursor-pointer"])
               :onMouseOver #(when on-row-hover (on-row-hover m))
               :onClick #(when on-row-click (on-row-click m))
               :title (when row-title-render (row-title-render m))}
          (for [[header col-render-fn] row-render]
            ^{:key (str "row-" header)}
            [:td {:class "text-xl"} (col-render-fn m)])]))]]])
