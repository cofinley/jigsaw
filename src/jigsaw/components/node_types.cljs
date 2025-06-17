(ns jigsaw.components.node-types
  (:require
   [jigsaw.components.function-find-chords-node :refer [function-find-chords-node]]
   [jigsaw.components.function-find-scales-node :refer [function-find-scales-node]]
   [jigsaw.components.function-find-shape-node :refer [function-find-shape-node]]
   [jigsaw.components.function-connect-shapes-node :refer [function-connect-shapes-node]]
   [jigsaw.components.function-fit-shape-node :refer [function-fit-shape-node]]
   [jigsaw.components.input-piano-node :refer [input-piano-node]]
   [jigsaw.components.input-shape-node :refer [input-shape-node]]))

(def node-categories
  {:input "Input"
   :function "Function"})

(def node-types
  [{:type :input-piano
    :category :input
    :label "Input Piano"
    :component input-piano-node}
   {:type :input-chord
    :category :input
    :label "Input Chord"
    :component input-shape-node}
   {:type :input-scale
    :category :input
    :label "Input Scale"
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
    :label "Find Nearest Shapes"
    :component function-find-shape-node}
   {:type :function-connect-shapes
    :category :function
    :label "Connect Shapes"
    :component function-connect-shapes-node}
   {:type :function-fit-shape
    :category :function
    :label "Fit Notes to Shape"
    :component function-fit-shape-node}])
