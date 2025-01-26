(ns jigsaw.components.node-types
  (:require
   [jigsaw.components.function-find-chords-node :refer [function-find-chords-node]]
   [jigsaw.components.function-find-scales-node :refer [function-find-scales-node]]
   [jigsaw.components.function-find-shape-node :refer [function-find-shape-node]]
   [jigsaw.components.input-piano-node :refer [input-piano-node]]
   [jigsaw.components.input-shape-node :refer [input-shape-node]]
   [reagent.core :as r]))

(def node-categories
  {:input "Input"
   :function "Function"})

(def node-types
  [{:type :input-piano
    :category :input
    :label "Piano"
    :component input-piano-node}
   {:type :input-chord
    :category :input
    :label "Chord"
    :component input-shape-node}
   {:type :input-scale
    :category :input
    :label "Scale"
    :component input-shape-node}
   {:type :function-scale-chords
    :category :function
    :label "Find Chords"
    :component function-find-chords-node}
   {:type :function-chord-scales
    :category :function
    :label "Find Scales"
    :component function-find-scales-node}
   {:type :function-find-shape
    :category :function
    :label "Compatible Shapes"
    :component function-find-shape-node}])

(def node-types-memo
  {:input-piano (r/reactify-component input-piano-node)
   :input-chord (r/reactify-component input-shape-node)
   :input-scale (r/reactify-component input-shape-node)
   :function-scale-chords (r/reactify-component function-find-chords-node)
   :function-chord-scales (r/reactify-component function-find-scales-node)
   :function-find-shape (r/reactify-component function-find-shape-node)})
