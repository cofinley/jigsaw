(ns jigsaw.components.loading)

(defn spinner
  "Simple CSS spinner component"
  [{:keys [size class]}]
  [:div {:class (str "animate-spin rounded-full border-2 border-t-transparent "
                     (or class "border-gray-300"))
         :style {:width (or size "20px") :height (or size "20px")}}])

(defn node-loading
  "Loading indicator specifically for node content"
  [{:keys [message]}]
  [:div {:class "flex flex-col items-center justify-center p-4 text-gray-500"}
   [spinner {:size "24px" :class "border-blue-400"}]
   (when message
     [:p {:class "mt-2 text-sm"} message])])
