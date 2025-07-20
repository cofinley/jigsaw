(ns jigsaw.components.context-menu
  (:require
   [reagent.core :as r]
   [re-frame.core :as re-frame]
   [jigsaw.components.node-types :refer [node-types]]
   [jigsaw.events :as events]
   [jigsaw.utils :as utils]))

(defn context-menu [{:keys [top right bottom left]} & body]
  [:div {:style {:top top :right right :bottom bottom :left left}
         :class "flex flex-col items-start py-2 bg-white rounded border border-black absolute z-30 shadow-lg"}
   body])

(defn menu-item [props label]
  [:button (r/merge-props
            {:class "p-2 w-full text-left hover:bg-black hover:text-white cursor-pointer"}
            props)
   label])

(defn add-node-menu-item [context-menu-props node-type label]
  [menu-item
   {:on-click (fn []
                (re-frame/dispatch [::events/add-node {:type node-type
                                                       :mouse-x (:mouse-x context-menu-props)
                                                       :mouse-y (:mouse-y context-menu-props)
                                                       :flow-instance (:flow-instance context-menu-props)}
                                    (:id context-menu-props)])
                ((:on-click context-menu-props)))}
   label])

(def node-type-allowed-children
  {nil (map :type node-types)
   :input-chord [:function-chord-scales :function-find-shape]
   :input-scale [:function-scale-chords :function-find-shape]
   :input-piano [:function-find-shape]
   :input-music-staff [:function-find-shape]
   :function-chord-scales [:function-scale-chords :function-find-shape]
   :function-scale-chords [:function-chord-scales :function-find-shape]
   :function-find-shape [:function-chord-scales :function-scale-chords :function-find-shape]
   :function-fit-shape [:function-chord-scales :function-scale-chords :function-find-shape]
   :function-connect-shapes [:function-scale-chords :function-find-shape]})

(defn node-context-menu [{:keys [type] :as props}]
  [context-menu props
   (for [node-type node-types
         :when (utils/in? (node-type-allowed-children type) (:type node-type))]
     ^{:key node-type}
     [add-node-menu-item props (:type node-type) (:label node-type)])])
