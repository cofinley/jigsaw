(ns jigsaw.explorer
  (:require
   [clojure.core.protocols :as p]
   [clojure.datafy :as d]
   ; [portal.api :as portal]
   [jigsaw.spec :as specs]
   [jigsaw.algo :as algo]))

"
Use cases

- Given some shapes, find adjacent shapes
  - Can select one or more shapes as input
- Reference for chord/scale/transposing lookups
- Visualize shape(s) multiple ways
  - Piano roll
  - Circle of fifths
  - Grand staff

- Select one -> transformations available
- Select many -> search available
"

(declare make-node)
(def id (atom 0))

(defrecord Node [data id parents children]
  p/Datafiable
  (datafy [o] o)
  p/Navigable
  (nav [coll k v]
    (make-node
     (case k
       :+interval {::specs/pitches (map #(algo/+interval % v)
                                        (get-in coll [:data ::specs/pitches]))}
       nil)
     [(:id coll)])))

(defn make-node [data & [parents]]
  (->Node data (swap! id inc) (or parents []) []))

(def interval-data
  #::specs{:intervals [:P1 :M3 :P5]})

(def pitch-data
  #::specs{:pitches [:C :E :G]})

(defn nav-> [coll ks]
  (loop [ks ks
         result nil]
    (if-let [k (first ks)]
      (recur (rest ks) (d/nav coll k (get coll k)))
      result)))

; (portal/clear)

(-> pitch-data
    make-node
    (d/nav :+interval :M3)
    (d/nav :+interval :M3))
