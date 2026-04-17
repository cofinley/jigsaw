(ns jigsaw.ui.views
  (:require
   ["react" :refer [StrictMode]]
   [jigsaw.ui.components.button :refer [button]]
   [jigsaw.ui.components.drawer :refer [drawer]]
   [jigsaw.ui.components.flow :refer [flow]]
   [jigsaw.ui.components.icons :refer [github-icon info-icon settings-icon]]
   [jigsaw.ui.components.info-panel :refer [info-panel]]
   [jigsaw.ui.components.settings-panel :refer [settings-panel]]
   [jigsaw.ui.events :as events]
   [jigsaw.ui.subs :as subs]
   [re-frame.core :as re-frame]))

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

(defn right-drawer []
  (let [component (re-frame/subscribe [::subs/drawer-component :right])]
    (when @component
      [drawer
       {:title (case @component
                 :settings "Settings"
                 :info "Info")
        :side :right
        :show? (some? @component)
        :on-close #(re-frame/dispatch [::events/update-drawer-component :right nil])}
       (case @component
         :settings [settings-panel]
         :info [info-panel])])))

(defn info-button []
  (let [component (re-frame/subscribe [::subs/drawer-component :right])]
    [button
     {:class "flex items-center gap-x-2 border-none"
      :on-click #(re-frame/dispatch [::events/update-drawer-component :right (if (= :info @component) nil :info)])}
     [info-icon
      {:class "w-4 h-4"}]
     "Info"]))

(defn settings-button []
  (let [component (re-frame/subscribe [::subs/drawer-component :right])]
    [button
     {:class "flex items-center gap-x-2 border-none"
      :on-click #(re-frame/dispatch [::events/update-drawer-component :right (if (= :settings @component) nil :settings)])}
     [settings-icon
      {:class "w-4 h-4"}]
     "Settings"]))

(defn main-panel []
  [:> StrictMode
   [:div {:class "flex flex-col dark:bg-neutral-800 dark:text-neutral-100 p-2 gap-y-2"}
    [:div {:class "flex justify-between items-center"}
     [title]
     [:div {:class "flex gap-4"}
      [info-button]
      [settings-button]]]]
   [right-drawer]
   [:f> flow]])
