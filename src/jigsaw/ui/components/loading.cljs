(ns jigsaw.ui.components.loading)

(defn spinner [{:keys [size class]}]
  [:div {:class (str "animate-spin rounded-full border-2 border-t-transparent "
                     (or class "border-gray-300"))
         :style {:width (or size "20px") :height (or size "20px")}}])

(defn loading-indicator []
  [:div {:class "flex flex-col items-center justify-center p-4 text-gray-500"}
   [spinner {:size "24px" :class "border-blue-400"}]
   [:p {:class "mt-2 text-xl"} "Loading..."]])
