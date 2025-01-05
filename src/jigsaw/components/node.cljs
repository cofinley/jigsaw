(ns jigsaw.components.node
  (:require
   [reagent.core :as r]
   ["@xyflow/react" :refer [Handle]]))

(defn node [props & body]
  (r/as-element
   [:div (r/merge-props {:class "react-flow__node-default w-full flex flex-col pb-4"} props)
    [:div {:class "border-b border-gray-400 mb-4"}
     [:h4 {:class "w-max text-2xl"} (:title props)]]
    (for [child body]
      (with-meta child {:key (str (random-uuid))}))]))

(defn handle [props & body]
  [:> Handle (r/merge-props {:class "h-3 w-3"} props)
   (for [child body]
     (with-meta child {:key (str (random-uuid))}))])

