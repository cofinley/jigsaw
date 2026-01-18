(ns jigsaw.experiments.logic
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}}}}
  (:require
   [clojure.core.logic :as l]
   [jigsaw.core :as jigsaw]
   [jigsaw.impl.theory :as theory]))

; (def all-shapes
;   (vec
;    (for [pitch theory/simple-pitch-keys
;          shape-name (concat (keys theory/chords) (keys theory/scales))]
;      (let [shape (jigsaw/->shape {:pitch pitch :name shape-name})]
;        (assoc
;         shape
;         ; :pitch-set (set (:pitches shape))
;         ; :pci-set (set (map theory/pitches (:pitches shape)))
;         ; :interval-set (set (:intervals shape))
;         ; :degree-set (set (:degrees shape))
;         :type (if (theory/chord? shape) :chord :scale))))))

(defn ->shapes [x]
  (cond
    (theory/shape? x) (jigsaw/shape->shapes x)
    (theory/shape-ref? x) (->shapes (jigsaw/->shape x))
    (and (map? x) (contains? x :notes)) (->shapes (jigsaw/->shape x))
    (keyword? x) (->shapes (jigsaw/->shape x))
    :else (lazy-seq)))

(defn neighboro [from to]
  (l/project [from]
             (l/membero to (->shapes from))))

(defmacro ? [m k v]
  `(l/featurec ~m {~k ~v}))

(comment
  (l/run 3 [q]
         (l/fresh [start a b path mode]
                  (l/== start :C_maj)
                  (neighboro start a)

                  (neighboro a b)
                  (? b :context :mode/II)

                  (l/== [start a b] path)
                  (l/== q path))))

; Connect from notes
(comment
  (let [; shapes-a (jigsaw/notes->shapes [:F4 :A4 :C5])
        ; shapes-b (jigsaw/notes->shapes [:Bb5 :D6 :F6])
        shapes-a (jigsaw/notes->shapes #{:Gb4 :A4 :C5 :E5})
        shapes-b (jigsaw/notes->shapes #{:Gb4 :A4 :B4 :Eb5})
        shapes-c (jigsaw/notes->shapes #{:E4 :G4 :B4})]

    (l/run 5 [q]
           (l/fresh [a a-pitch a-name a-overlap a-context
                     b b-pitch b-name b-overlap b-context
                     c c-pitch c-name c-overlap c-context
                     x x-pitch x-name y z]
                    (l/membero a shapes-a)
                    (l/membero b shapes-b)
                    (l/membero c shapes-c)

                    (neighboro a x)
                    ; (l/== x-pitch :E)
                    (l/featurec x {:pitch x-pitch :name x-name :context a-context})

                    (neighboro b y)
                    (l/featurec y {:pitch x-pitch :name x-name :context b-context})

                    (neighboro c z)
                    (l/featurec z {:pitch x-pitch :name x-name :context c-context})

                    (l/featurec a {:pitch a-pitch :name a-name :heuristics {:overlap a-overlap}})
                    (l/featurec b {:pitch b-pitch :name b-name :heuristics {:overlap b-overlap}})
                    (l/featurec c {:pitch c-pitch :name c-name :heuristics {:overlap c-overlap}})

                    (l/== q {{:pitch x-pitch :name x-name}
                             [{:pitch a-pitch :name a-name :context a-context :overlap a-overlap}
                              {:pitch b-pitch :name b-name :context b-context :overlap b-overlap}
                              {:pitch c-pitch :name c-name :context c-context :overlap c-overlap}]})))))

; Connect from shapes
(comment
  (l/run 3 [q]
         (l/fresh [a a-context x
                   x-pitch x-name
                   b b-context y]
                  (l/== a :C_maj)
                  (l/== b :D_m)

                  (neighboro a x)
                  (l/featurec x {:pitch x-pitch :name x-name :context a-context})

                  (neighboro b y)
                  (l/featurec y {:pitch x-pitch :name x-name :context b-context})

                   ; (l/== [:chord-degree/I :chord-degree/ii] [a-context b-context])

                  (l/== q {{:pitch x-pitch :name x-name} {a a-context
                                                          b b-context}}))))

; Fit (one hop)
(comment
  (l/run* [q]
          (l/fresh [start scale end]
                   ; Start at C major chord
                   (l/== start {:pitch :C :name :maj})

                   ; Find scale where it's a V chord
                   (neighboro start scale)
                   (l/featurec scale {:context :chord-degree/V})

                   ; Find the corresponding I chord (specifically major 7th) of the scale
                   (neighboro scale end)
                   (l/featurec end {:context :chord-degree/I
                                    :name :maj7})

                   ; Return the results
                   (l/== q {end scale}))))

; Fit (two hops)
(comment
  (l/run 1 [q]
         (l/fresh [start scale1 scale2 end]
                   ; Start at C major chord
                  (l/== start {:pitch :C :name :maj})

                   ; Find scale where it's a V chord
                  (neighboro start scale1)
                  (l/featurec scale1 {:context :chord-degree/V})

                  ; Find second mode of that scale
                  (neighboro scale1 scale2)
                  (l/featurec scale2 {:context :mode/II})

                   ; Find the corresponding I chord of the second mode
                  (neighboro scale2 end)
                  (l/featurec end {:context :chord-degree/I
                                   :name :maj})

                   ; Return the results
                  (l/== q [scale1 scale2 end]))))

; Secondary dominant (V/V or V7/V)
(comment
  (l/run 1 [q]
         (l/fresh [start-scale dom-chord dom-scale dom-chord2]
                   ; Start at C major scale
                  (l/== start-scale {:pitch :C :name :major})

                  ; Find the dominant (V) chord
                  (neighboro start-scale dom-chord)
                  (l/featurec dom-chord {:context :chord-degree/V})

                  ; Find the scale where the dominant chord is the I
                  (neighboro dom-chord dom-scale)
                  (l/featurec dom-scale {:context :chord-degree/I})

                  ; Find that scale's dominant (i.e. tonicized chord)
                  (neighboro dom-scale dom-chord2)
                  (l/featurec dom-chord2 {:context :chord-degree/V})

                  ; Return the secondary dominant
                  (l/== q dom-chord2))))

(defn shapeo [q shape-ref]
  (l/project [shape-ref]
             (l/== q (jigsaw/->shape shape-ref))))

(defn subo [q shape-ref sub-interval new-name & [multiplier]]
  (l/project [shape-ref]
             (l/fresh [shape pitch next-pitch next-shape next-name]
                      ; TODO: might be good to deal in whole shapes for algos to avoid recalcs, but refs/keywords for display
                      (shapeo shape shape-ref)
                      (l/featurec shape {:pitch pitch})
                      (l/is next-pitch pitch #(theory/+interval % sub-interval multiplier))
                      (l/== q {:pitch next-pitch :name new-name}))))

; Tritone substitution
(comment
  (l/run 1 [q]
         (l/fresh [scale ii v i sub]
                  (l/== scale {:pitch :C :name :major})

                  (neighboro scale ii)
                  (l/featurec ii {:context :chord-degree/ii :name :m7})

                  (neighboro scale v)
                  (l/featurec v {:context :chord-degree/V :name :7})

                  (neighboro scale i)
                  (l/featurec i {:context :chord-degree/I :name :maj7})

                  (subo sub v :d5 :7)
                  (l/== q [ii sub i]))))

; Coltrane changes
(comment
  (l/run 1 [q]
         (l/fresh [key1 ii v i
                   key2 v2 i2
                   key3 v3 i3]
                  (l/== key1 {:pitch :C :name :major})

                  ; Normal ii-V-I
                  (neighboro key1 ii)
                  (l/featurec ii {:context :chord-degree/ii :name :m7})

                  (neighboro key1 v)
                  (l/featurec v {:context :chord-degree/V :name :7})

                  (neighboro key1 i)
                  (l/featurec i {:context :chord-degree/I :name :maj7})

                  ; Key goes down a third
                  (subo key2 key1 :M3 :major -1)

                  ; New V-I
                  (neighboro key2 v2)
                  (l/featurec v2 {:context :chord-degree/V :name :7})

                  (neighboro key2 i2)
                  (l/featurec i2 {:context :chord-degree/I :name :maj7})

                  ; Key goes down another third
                  (subo key3 key2 :M3 :major -1)

                  ; New V-I
                  (neighboro key3 v3)
                  (l/featurec v3 {:context :chord-degree/V :name :7})

                  (neighboro key3 i3)
                  (l/featurec i3 {:context :chord-degree/I :name :maj7})

                  (l/== q [ii v2 i2 v3 i3 v i]))))
