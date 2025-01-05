(ns jigsaw.views
  (:require
   [jigsaw.components.flow :refer [flow]]
   ["react" :refer [StrictMode]]))

(defn main-panel []
  [:> StrictMode
   [:f> flow]])
