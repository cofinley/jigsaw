(ns jigsaw.components.context-menu
  (:require
   [reagent.core :as r]
   [re-frame.core :as re-frame]
   [jigsaw.events :as events]))

(defn context-menu [{:keys [top right bottom left]} & body]
  [:div {:style {:top top :right right :bottom bottom :left left}
         :class "flex flex-col items-start py-2 bg-white rounded border border-black absolute z-10 shadow-lg"}
   body])

(defn menu-item [props label]
  [:button (r/merge-props
            {:class "p-2 w-full text-left hover:bg-black hover:text-white cursor-pointer"}
            props)
   label])

(defn add-node-menu-item [context-menu-props node-type label]
  [menu-item {:on-click (fn []
                          (re-frame/dispatch [::events/add-node node-type (:id context-menu-props)])
                          ((:on-click context-menu-props)))}
   label])

(def root-node-options
  {:input-piano "New Piano"
   :input-chord "New Chord"
   :input-scale "New Scale"})

(def child-node-options
  {:function-scale-chords "Find chords"
   :function-chord-scales "Find scales"
   :function-find-shape "Find compatible shapes"})

(defn node-context-menu [{:keys [type] :as props}]
  [context-menu props
   (for [[node-type label] (if (some? type) child-node-options root-node-options)]
     ^{:key node-type} [add-node-menu-item props node-type label])])
