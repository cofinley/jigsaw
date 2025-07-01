(ns jigsaw.components.node
  (:require
   [reagent.core :as r]
   [goog.string :as gstr]
   [re-frame.core :as re-frame]
   [jigsaw.components.select :refer [select]]
   [jigsaw.components.loading :refer [loading-indicator]]
   [jigsaw.events :as events]
   [jigsaw.subs :as subs]
   [jigsaw.components.output-piano-node :refer [output-piano-view]]
   [jigsaw.components.output-music-staff-node :refer [output-music-staff-view]]
   [jigsaw.components.output-debug-node :refer [output-debug-view]]
   [jigsaw.components.output-circle-of-fifths-node :refer [output-circle-of-fifths-view]]
   ["@xyflow/react" :refer [Handle]]))

(def right-arrow "&#9658;")
(def down-arrow "&#9660;")

(def node-output-views
  [{:type :output-piano
    :label "Piano"
    :component output-piano-view}
   {:type :output-music-staff
    :label "Music Staff"
    :component output-music-staff-view}
   {:type :output-circle-of-fifths
    :label "Circle of Fifths"
    :component output-circle-of-fifths-view}
   {:type :output-debug
    :label "Debug"
    :component output-debug-view}])

(defn handle [props]
  [:> Handle (r/merge-props {:class "h-8 w-5 rounded-md"} props)])

(defn node [props & body]
  (let [open? (r/atom true)]
    (fn [{:keys [title id data parent-data handles class]} & body]
      (let [loading? @(re-frame/subscribe [::subs/node-loading? id])]
        (r/as-element
         [:div (merge {:class "react-flow__node-default w-full flex flex-col pb-5 pt-2 px-6 relative"} class)

          ;; Loading overlay
          (when loading?
            [:div {:class "absolute inset-0 bg-neutral-900 bg-opacity-70 flex items-center justify-center z-10 rounded-lg pointer-events-auto"}
             [loading-indicator {:message "Computing..."}]])

          [:div {:class "flex border-b border-neutral-400 mb-4 gap-2"}
           [:div {:class "cursor-pointer"
                  :on-click #(reset! open? (not @open?))}
            [:span {:class "text-lg cursor-pointer"}
             (gstr/unescapeEntities (if @open? down-arrow right-arrow))]]
           [:h4 {:class "w-max font-semibold text-2xl"} title]
           [:button {:class "text-neutral-400 cursor-pointer bg-transparent hover:bg-neutral-200 hover:text-neutral-900 rounded-lg text-sm w-8 h-8 ms-auto inline-flex justify-center items-center dark:hover:bg-neutral-600 dark:hover:text-white"
                     :on-click #(when (js/confirm "Delete?")
                                  (.stopPropagation %)
                                  (re-frame/dispatch [::events/delete-node id]))}
            [:svg {:class "w-3 h-3" :xmlns "http://www.w3.org/2000/svg" :fill "none" :viewBox "0 0 14 14"}
             [:path {:stroke "currentColor" :stroke-linecap "round" :stroke-linejoin "round" :stroke-width "2" :d "m1 1 6 6m0 0 6 6M7 7l6-6M7 7l-6 6"}]]]]

          (for [i (range (count handles))
                :let [h (nth handles i)
                      k (str "handle-target-" id "-" i)]
                :when (= "target" (:type h))]
            ^{:key k} [handle h])

          (when @open?
            [:<>
             (for [[i child] (map-indexed vector body)]
               (with-meta child {:key (str "node-body-" id "-" i)}))

             (let [view-type (:view-type data)]
               [:div {:class "flex flex-col space-y-4"}
                [:div {:class "flex space-x-2 items-center mt-4 text-xl"}
                 [:span {:class "font-semibold"} "View"]
                 [select {:value (or view-type "")
                          :on-change #(re-frame/dispatch [::events/update-node-data id {:view-type (let [value (-> % .-target .-value)]
                                                                                                     (when (not= "" value) (keyword value)))}])}
                  (cons
                   [:option {:value ""} "None"]
                   (for [view node-output-views]
                     [:option {:value (:type view)} (:label view)]))]]

                (when (some? view-type)
                  (let [view (:component (first (filter #(= view-type (:type %)) node-output-views)))
                        props {:data data :parent-data parent-data}]
                    [view props]))])])

          (for [i (range (count handles))
                :let [h (nth handles i)
                      k (str "handle-source-" id "-" i)]
                :when (= "source" (:type h))]
            ^{:key k} [handle h])])))))

