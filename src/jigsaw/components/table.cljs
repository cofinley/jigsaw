(ns jigsaw.components.table
  (:require
   [clojure.string :as s]
   [jigsaw.components.select :refer [select]]
   [reagent.core :as r]))

(defn table [{:keys [ms
                     row-render
                     row-selected?
                     on-row-hover
                     on-row-click
                     row-title-render
                     row-filter]}]
  (let [show-previews? (r/atom false)
        preview-type (r/atom nil)]
    [:div {:class "max-h-96 w-full overflow-scroll nowheel nodrag flex flex-col"}
     [:div {:class "self-start"}
      [:label {:class "space-x-1"}
       [:span "Show previews?"]
       [:input {:type "checkbox"
                :checked @show-previews?
                :on-change #(swap! show-previews? not)}]]
      (when @show-previews?
        [:label {:class "self-start space-x-1"}
         [:span "Preview type"]
         [select {:value @preview-type
                  :on-change #(reset! preview-type (-> % .-target .-value keyword))}
          [[:option {:value :piano} "Piano"]
           [:option {:value :staff} "Staff"]]]])]
     [:table
      [:thead
       [:tr {:class "sticky w-full top-0 bg-neutral-700 z-10"}
        (doall (for [header (keys row-render)]
                 ^{:key (str "header-" header)}
                 [:th {:class "text-xl"} header]))]]
      [:tbody
       (doall
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
             (doall (for [[header col-render-fn] row-render]
                      ^{:key (str "row-" header)}
                      [:td {:class "text-xl"} (col-render-fn m)]))])))]]]))
