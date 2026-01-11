(ns jigsaw.ui.core
  (:require
   [jigsaw.ui.config :as config]
   [jigsaw.ui.events :as events]
   [jigsaw.ui.views :as views]
   [reagent.dom :as rdom]
   [re-frame.core :as re-frame]))

(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/main-panel] root-el)))

(defn init []
  (re-frame/dispatch [::events/initialize-db])
  (dev-setup)
  (mount-root))
