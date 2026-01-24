(ns jigsaw.experiments.logic
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}}}}
  (:require
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as fd]
   [jigsaw.core :as jigsaw]
   [jigsaw.impl.theory :as theory]))

; (def all-shapes
;   (for [pitch theory/simple-pitch-keys
;         shape-name (concat (keys theory/chords) (keys theory/scales))]
;     (let [shape (jigsaw/->shape {:pitch pitch :name shape-name})
;           pitches (:pitches shape)
;           pcis (map #(theory/pitches %) pitches)]
;       (assoc
;        shape
;        :pcis pcis
;        :pitch-set (set pitches)
;        :pci-set (set pcis)
;         ; :interval-set (set (:intervals shape))
;         ; :degree-set (set (:degrees shape))
;        ;:type (if (theory/chord? shape) :chord :scale)
;        ))))

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

; Connect (from notes); given one or more starting points, find how they connect
(comment
  (let [shapes-a (jigsaw/notes->shapes #{:Gb4 :A4 :C5 :E5})
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

; Connect (from shapes)
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

                  (l/== q {{:pitch x-pitch :name x-name} {a a-context
                                                          b b-context}}))))

; Search nested relations
(comment
  (l/run 5 [q]
         (l/fresh [start scale end]
                   ; Start at C major chord
                  (l/== start {:pitch :C :name :maj})

                   ; Find scale where it's a V chord
                  (neighboro start scale)
                  (l/featurec scale {:context :chord-degree/V})

                   ; Find the corresponding I chord (specifically major 7th) of the scale
                  (neighboro scale end)
                  (l/featurec end {:context :chord-degree/IM7})

                   ; Return the results
                  (l/== q scale))))
; ({:pitch :F, :name :major, :context :chord-degree/V}
;  {:pitch :F, :name :bebop, :context :chord-degree/V}
;  {:pitch :F, :name :lydian, :context :chord-degree/V}
;  {:pitch :F, :name :harmonic-major, :context :chord-degree/V}
;  {:pitch :F, :name :bebop-major, :context :chord-degree/V})

; More searching of nested relations
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
                  (l/featurec end {:context :chord-degree/I})

                   ; Return the results
                  (l/== q [scale1 scale2 end]))))
; ([{:pitch :F, :name :lydian, :context :chord-degree/V}
;   {:pitch :G, :name :mixolydian, :context :mode/II}
;   {:pitch :G, :name :maj, :context :chord-degree/I}])

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
; => ({:pitch :D, :name :maj, :context :chord-degree/V})

(defn shapeo
  "Reify shape from (potential) shape ref"
  [q shape-ref]
  (l/is q shape-ref jigsaw/->shape))

(defn noteso
  "Potential shapes from notes"
  [q notes]
  (l/project [notes]
             (l/membero q (jigsaw/notes->shapes notes))))

(defn transposo
  "Transpose pitch/note/shape by interval"
  [q x interval & [multiplier]]
  (l/project [x]
             (l/== q (theory/transpose x interval multiplier))))

; Tritone substitution
(comment
  (l/run 1 [q]
         (l/fresh [scale ii v i sub]
                  (l/== scale {:pitch :C :name :major})

                  (neighboro scale ii)
                  (l/featurec ii {:context :chord-degree/ii7 :name :m7})

                  (neighboro scale v)
                  (l/featurec v {:context :chord-degree/V7 :name :7})

                  (neighboro scale i)
                  (l/featurec i {:context :chord-degree/IM7 :name :maj7})

                  ; Find the shape which is a tritone away from the V chord
                  (transposo sub v :d5)
                  (l/== q [ii sub i]))))
; ([{:pitch :D, :name :m7, :context :chord-degree/ii7}
;   {:pitch :Db, :name :7}
;   {:pitch :C, :name :maj7, :context :chord-degree/IM7}])

; Coltrane changes
(comment
  (let [key1 (jigsaw/->shape :C_major)]
    (l/run 1 [q]
           (l/fresh [ii v i
                     key2 v2 i2
                     key3 v3 i3]
                    ; Normal ii-V-I
                    (neighboro key1 ii)
                    (l/featurec ii {:context :chord-degree/ii7})

                    (neighboro key1 v)
                    (l/featurec v {:context :chord-degree/V7})

                    (neighboro key1 i)
                    (l/featurec i {:context :chord-degree/IM7})

                    ; Key goes down a third
                    (transposo key2 key1 :M3 -1)

                    ; New V-I
                    (neighboro key2 v2)
                    (l/featurec v2 {:context :chord-degree/V7})

                    (neighboro key2 i2)
                    (l/featurec i2 {:context :chord-degree/IM7})

                    ; Key goes down another third
                    (transposo key3 key2 :M3 -1)

                    ; New V-I
                    (neighboro key3 v3)
                    (l/featurec v3 {:context :chord-degree/V7})

                    (neighboro key3 i3)
                    (l/featurec i3 {:context :chord-degree/IM7})

                    (l/== q [ii v2 i2 v3 i3 v i])))))
; ([{:pitch :D, :name :m7, :context :chord-degree/ii7}
;   {:pitch :Eb, :name :7, :context :chord-degree/V7}
;   {:pitch :Ab, :name :maj7, :context :chord-degree/IM7}
;   {:pitch :B, :name :7, :context :chord-degree/V7}
;   {:pitch :E, :name :maj7, :context :chord-degree/IM7}
;   {:pitch :G, :name :7, :context :chord-degree/V7}
;   {:pitch :C, :name :maj7, :context :chord-degree/IM7}])

(defn heuristico [q shape1 shape2]
  (l/project [shape1 shape2]
             (l/== q (into {}
                           (map (fn [[k v]] (vector k (int (* 100 v))))
                                (theory/calculate-heuristics (:pitches (jigsaw/->shape shape1))
                                                             (:pitches (jigsaw/->shape shape2))))))))

(defn fuzzy-neighborc [from to]
  (l/fresh [h overlap pc]
           (heuristico h from to)
           (l/featurec h {:overlap overlap :same-pitch-count? pc})
           (l/conde
            ; Perfect pitch match
            [(fd/== overlap 100)]
            ; Same pitch count and decent pitch overlap
            [(fd/== pc 100) (fd/>= overlap 50)]
            ; Hacky sort?
            [(fd/>= overlap 95)]
            [(fd/>= overlap 90)]
            [(fd/>= overlap 80)]
            [(fd/>= overlap 70)]
            [(fd/>= overlap 60)])))

; Fit (find closest compatible shape, to a reference shape)
; i.e. <input> ~= ? <-> <shape>
(comment
  (let [start (jigsaw/->shape :C_m)
        parent (jigsaw/->shape :C_major)]
    (l/run 3 [q]
           (l/fresh [candidate]
                    ; Find which shape(s) in the C major scale...
                    (neighboro parent candidate)
                    ; ...are close to the C minor chord
                    (fuzzy-neighborc start candidate)
                    (l/== q candidate)))))
; ({:pitch :C, :name :maj, :context :chord-degree/I}
;  {:pitch :C, :name :sus4, :context :chord-degree/i}
;  {:pitch :C, :name :sus2, :context :chord-degree/i})

; (defn progressiono [chords]
;   (l/project [chords]))

;; TODO: autocomplete: based on inputs, see if you're playing a known progression
;; maybe account for fuzziness
(comment
  (let [; Start of a ii-V-I
        ii (jigsaw/->shape :D_m)
        v (jigsaw/->shape :G_7)]
    (l/run 1 [q]
           (l/fresh [scale progression]))))

; chords -> scales, using the degrees, then degrees -> progressions?
;   how to know if substitution?
;     need chord -> possible sub relation
; or
; progressions -> scale-progressions -> scale-progression-chords -> match on current chords?
; need scale to know current degrees

