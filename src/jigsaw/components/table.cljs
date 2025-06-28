(ns jigsaw.components.table
  (:require
   [clojure.string :as string]
   [jigsaw.spec :as specs]))

(defn is-draggable-row? [m]
  (specs/shape-ref? m))

(defn handle-drag-start
  [m e]
  (let [data-transfer (.-dataTransfer e)
        drag-data (js/JSON.stringify (clj->js m))]
    (.setData data-transfer "application/json" drag-data)
    (set! (.-effectAllowed data-transfer) "copy")
    ;; Add visual feedback classes
    (-> e .-target .-classList (.add "opacity-50"))
    (-> e .-target .-classList (.add "cursor-grabbing"))))

(defn handle-drag-end [e]
  (-> e .-target .-classList (.remove "opacity-50"))
  (-> e .-target .-classList (.remove "cursor-grabbing")))

(defn table [{:keys [ms
                     row-render
                     row-selected?
                     on-row-hover
                     on-row-click
                     row-title-render
                     key-fn
                     row-filter]}]
  [:div {:class "max-h-96 w-full overflow-scroll nowheel nodrag flex flex-col"}
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
        (let [selected? (and (some? row-selected?) (row-selected? m))
              draggable? (is-draggable-row? m)]
          ^{:key (or (and (some? key-fn) (key-fn m)) m)}
          [:tr (cond-> {:class (string/join
                                " "
                                [(if selected?
                                   "bg-indigo-500 hover:bg-indigo-400"
                                   "bg-neutral-800 even:bg-neutral-900 hover:bg-neutral-700")
                                 "hover:cursor-pointer"
                                 (when draggable? "cursor-grab")])
                        :onMouseOver #(when on-row-hover (on-row-hover m))
                        :onClick #(when on-row-click (on-row-click m))
                        :title (when row-title-render (row-title-render m))}
                 draggable? (assoc :draggable true
                                   :onDragStart #(handle-drag-start m %)
                                   :onDragEnd handle-drag-end))
           (doall (for [[header col-render-fn] row-render]
                    ^{:key (str "row-" header)}
                    [:td {:class "text-xl"} (col-render-fn m)]))])))]]])
