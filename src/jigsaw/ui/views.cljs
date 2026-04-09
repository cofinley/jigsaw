(ns jigsaw.ui.views
  (:require
   ["react" :refer [StrictMode]]
   [jigsaw.ui.components.button :refer [button]]
   [jigsaw.ui.components.drawer :refer [drawer]]
   [jigsaw.ui.components.flow :refer [flow]]
   [jigsaw.ui.components.icons :refer [github-icon settings-icon]]
   [jigsaw.ui.components.settings-panel :refer [settings-panel]]
   [reagent.core :as r]))

(defn title []
  [:div
   [:div {:class "flex items-center gap-x-2"}
    [:h1.text-3xl.font-bold.tracking-tight
     "Jigsaw"]
    [:a
     {:class "ml-2" :href "https://github.com/cofinley/jigsaw" :target "_blank" :title "Source code"}
     [github-icon
      {:class "w-7 h-7"}]]]
   [:span.text-sm.dark:text-neutral-400
    "Music theory, explored"]])

(defn settings-button []
  (let [show-drawer? (r/atom false)]
    (fn []
      [:<>
       [drawer
        {:title "Settings"
         :side :right
         :show? @show-drawer?
         :on-close #(reset! show-drawer? false)}
        [settings-panel]]
       [button
        {:class "flex items-center gap-x-2"
         :on-click #(reset! show-drawer? true)}
        [settings-icon
         {:class "w-4 h-4"}]
        "Settings"]])))

(defn main-panel []
  [:> StrictMode
   [:div.flex.flex-col.dark:bg-neutral-800.dark:text-neutral-100.p-2.gap-y-2
    [:div.flex.justify-between.items-center
     [title]
     [settings-button]]]
   [:f> flow]])
