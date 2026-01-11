(ns jigsaw.ui.views
  (:require
   [jigsaw.ui.components.flow :refer [flow]]
   ["react" :refer [StrictMode]]))

(defn main-panel []
  [:> StrictMode
   [:f> flow]])
