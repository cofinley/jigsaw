(ns jigsaw.components.output-music-staff-node
  (:require
   [reagent.core :as r]
   [jigsaw.theory :as theory]
   ["abcjs" :as abcjs]))

(defn score [data]
  (let [dom-id (str (random-uuid))]
    (r/create-class
     {:display-name "score"
      :component-did-mount
      (fn [_]
        (let [notes (set (:notes data))
              syntax (theory/shape->abc data)
              scale? (contains? data :degrees)]
          (.renderAbc abcjs dom-id syntax #js {:jazzchords true
                                               :lineThickness 0.1
                                               :staffwidth (if scale? (* 50 (count notes)) 100)})))
      :should-component-update (fn [_ prev next]
                                 (let [prev-notes (:notes (second prev))
                                       next-notes (:notes (second next))]
                                   (not= prev-notes next-notes)))
      :component-did-update
      (fn [this _ _ _]
        (let [new-data (second (r/argv this))
              notes (set (:notes new-data))
              syntax (theory/shape->abc new-data)
              scale? (contains? new-data :degrees)]
          (.renderAbc abcjs dom-id syntax #js {:jazzchords true
                                               :lineThickness 0.1
                                               :staffwidth (if scale? (* 50 (count notes)) 100)})))
      :reagent-render
      (fn []
        [:div {:id dom-id
               :class "flex justify-center"}])})))

(defn output-music-staff-view [props]
  (let [data (:data props)
        notes (:notes data)]
    (if (and (every? data [:notes :pitch :name]) (seq notes))
      [score data]
      ;; TODO: allow just notes, maybe key/pitch override?
      [:p {:class "text-lg"} "Insufficient input; needs notes, pitch, and name"])))
