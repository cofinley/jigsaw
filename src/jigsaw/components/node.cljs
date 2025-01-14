(ns jigsaw.components.node
  (:require
   [reagent.core :as r]
   [goog.string :as gstr]
   [re-frame.core :as re-frame]
   [jigsaw.components.select :refer [select]]
   [jigsaw.events :as events]
   [jigsaw.components.output-piano-node :refer [output-piano-view]]
   [jigsaw.components.output-music-staff-node :refer [output-music-staff-view]]
   [jigsaw.components.output-debug-node :refer [output-debug-view]]
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
   {:type :output-debug
    :label "Debug"
    :component output-debug-view}])

(defn handle [props]
  [:> Handle (r/merge-props {:class "h-8 w-5 rounded-md"} props)])

(defn node [{:keys [title id data handles class]} & body]
  (let [open? (r/atom true)]
    (fn [{:keys [title id data handles class]} & body]
      (let [data (:data (events/js-node->clj-node {:data data}))]
        (r/as-element
         [:div (merge {:class "react-flow__node-default w-full flex flex-col pb-5 pt-2 px-6"} class)
          [:div {:class "border-b border-gray-400 mb-4 flex space-x-1"}
           [:span {:class "text-lg cursor-pointer"
                   :on-click #(reset! open? (not @open?))}
            (gstr/unescapeEntities (if @open? down-arrow right-arrow))]
           [:h4 {:class "w-max font-semibold text-2xl"} title]]

          (for [h handles
                :when (= "target" (:type h))]
            ^{:key (random-uuid)} [handle h])

          (when @open?
            (for [child body]
              (with-meta child {:key (str (random-uuid))})))

          (let [view-type (or (:view-type data) :output-piano)]
            [:div {:class "flex flex-col space-y-4"}
             [:div {:class "flex space-x-2 items-center mt-4"}
              [:span "View"]
              [select {:value view-type
                       :on-change #(re-frame/dispatch [::events/update-node-data id {:view-type (-> % .-target .-value keyword)}])}
               (for [view node-output-views]
                 [:option {:value (:type view)} (:label view)])]]

             (when (some? view-type)
               (let [view (:component (first (filter #(= view-type (:type %)) node-output-views)))
                     props {:data data}]
                 [view props]))])

          (for [h handles
                :when (= "source" (:type h))]
            ^{:key (random-uuid)} [handle h])])))))

