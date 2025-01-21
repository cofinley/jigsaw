(ns jigsaw.components.node-types
  (:require
   [jigsaw.components.input-piano-node :refer [input-piano-node]]
   [jigsaw.components.input-shape-node :refer [input-shape-node]]
   [jigsaw.components.function-scale-chords-node :refer [function-scale-chords-node]]
   [jigsaw.components.function-chord-scales-node :refer [function-chord-scales-node]]
   [jigsaw.components.function-find-shape-node :refer [function-find-shape-node]]))

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
    :label "Scale Chords"
    :component function-scale-chords-node}
   {:type :function-chord-scales
    :category :function
    :label "Chord Scales"
    :component function-chord-scales-node}
   {:type :function-find-shape
    :category :function
    :label "Compatible Shapes"
    :component function-find-shape-node}])

