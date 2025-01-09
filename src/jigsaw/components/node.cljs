(ns jigsaw.components.node
  (:require
   [reagent.core :as r]
   [goog.string :as gstr]
   ["@xyflow/react" :refer [Handle]]))

(def right-arrow "&#9658;")
(def down-arrow "&#9660;")

(defn handle [props]
  [:> Handle (r/merge-props {:class "h-8 w-5 rounded-md"} props)])

(defn node [props & body]
  (let [open? (r/atom true)
        handle-props (:handle props)]
    (fn [props & body]
      (r/as-element
       [:div (r/merge-props {:class "react-flow__node-default w-full flex flex-col pb-4 pt-2"} (:class props))
        [:div {:class "border-b border-gray-400 mb-4 flex space-x-1"}
         [:span {:class "text-lg cursor-pointer"
                 :on-click #(reset! open? (not @open?))}
          (gstr/unescapeEntities (if @open? down-arrow right-arrow))]
         [:h4 {:class "w-max font-semibold text-2xl"} (:title props)]]
        (when (= "target" (:type handle-props))
          [handle handle-props])
        (when @open?
          (for [child body]
            (with-meta child {:key (str (random-uuid))})))
        (when (= "source" (:type handle-props))
          [handle handle-props])]))))

