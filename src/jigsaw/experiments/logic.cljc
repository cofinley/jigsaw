(ns jigsaw.experiments.logic
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}
                                 :invalid-arity {:level :off}}}}
  (:require
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as fd]
   [clojure.set :as set]
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

(defn noteso
  "Potential shapes from notes"
  [q notes]
  (l/project [notes]
             (l/membero q (jigsaw/notes->shapes notes))))

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

; Connect; given one or more starting points, find how they connect; finds common diatonic complementary shape (i.e. chord<->scale)

; Recursive goal to find shared neighbor shape; collect pitch & name of neighbor as well as the shapes contextualized to that neighbor
(l/defne connect-neighbors [shapes ?contextualized-shapes ?common-pitch ?common-name]
  ([() () _ _])
  ([[shape . rest-shapes] [?contextualized-shape . ?rest-contextualized-shapes] _ _]
   (l/fresh [?neighbor ?context]
            (neighboro shape ?neighbor)
            (l/featurec ?neighbor {:pitch ?common-pitch :name ?common-name :context ?context})
            (l/conjo shape {:context ?context} ?contextualized-shape)
            (connect-neighbors rest-shapes ?rest-contextualized-shapes ?common-pitch ?common-name))))

(defn connecto [q shapes]
  (l/fresh [?connection ?contextualized-shapes ?common-pitch ?common-name]
           (l/== ?connection {:pitch ?common-pitch :name ?common-name})
           (connect-neighbors shapes ?contextualized-shapes ?common-pitch ?common-name)
           (l/== q {:connection ?connection
                    :contextualized-shapes ?contextualized-shapes})))

(defn resolve-input [?shape x]
  (l/project [x]
             (if (theory/notes? x)
               (noteso ?shape x)
               (l/== ?shape x))))

; Support input of shapes or note-seqs which map to one or more shapes
(l/defne prepare-shapes [?shapes xs]
  ([() ()])
  ([[?shape . ?rest-shapes] [x . rest-xs]]
   (resolve-input ?shape x)
   (prepare-shapes ?rest-shapes rest-xs)))

; Find connection with shapes
(comment
  (let [; Start of a ii-V-I
        ii (jigsaw/->shape :D_m)
        v (jigsaw/->shape :G_maj)
        chords [ii v]]
    (l/run 5 [q]
           (l/fresh [?shapes ?conn ?chords ?prog]
                    (prepare-shapes ?shapes chords)
                    (connecto ?conn ?shapes)
                    (l/== q ?conn)))))

; Find connection with note seqs
(comment
  (let [ii (jigsaw/->shape :D4_m)
        ii-notes (:notes ii)
        v (jigsaw/->shape :G4_maj)
        v-notes (:notes v)
        note-seqs [ii-notes v-notes]]
    (l/run 1 [q]
           (l/fresh [?shapes ?conn ?contextualized-shapes ?prog]
                    (prepare-shapes ?shapes note-seqs)
                    (connecto ?conn ?shapes)
                    (l/== q ?conn)))))

; More connections from notes
(comment
  (let [note-seqs [[:Gb4 :A4 :C5 :E5]
                   [:Gb4 :A4 :B4 :Eb5]
                   [:E4 :G4 :B4]]]
    (l/run 5 [q]
           (l/fresh [?shapes ?conn ?contextualized-shapes]
                    (prepare-shapes ?shapes note-seqs)
                    (connecto ?conn ?shapes)
                    (l/== q ?conn)))))

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
                  (l/featurec ii {:context :chord-degree/ii7})

                  (neighboro scale v)
                  (l/featurec v {:context :chord-degree/V7})

                  (neighboro scale i)
                  (l/featurec i {:context :chord-degree/IM7})

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
                    ; Find the shape(s) in the C major scale...
                    (neighboro parent candidate)
                    ; ...which are close (pitch-wise) to the C minor chord
                    (fuzzy-neighborc start candidate)
                    (l/== q candidate)))))
; => ({:pitch :C, :name :maj, :context :chord-degree/I}
;     {:pitch :C, :name :sus4, :context :chord-degree/i}
;     {:pitch :C, :name :sus2, :context :chord-degree/i})

(defn progresso
  "Find compatible progresion, given some chord degrees"
  [q contextualized-chords & {:keys [p] :or {p set/subset?}}]
  (l/project [contextualized-chords]
             (l/membero q (->> theory/chord-progressions
                               (filter (fn [[prog-name details]]
                                         (p (set (map :context contextualized-chords))
                                            (set (:degrees details)))))))))

(defn progresso2
  "Find compatible progresion and resolve its chords, given some scale and chord degrees"
  [q scale-ref degrees & {:keys [p]
                          :or {p set/subset?}}]
  (l/project [scale-ref degrees]
             (l/membero q (->> theory/chord-progressions
                               (filter (fn [[prog-name details]]
                                         (p (set degrees) (set (:degrees details)))))
                               (map (fn [[prog-name details]]
                                      [prog-name (assoc details :resolved-degrees (map #(theory/resolve-chord-degree (jigsaw/->shape scale-ref) %) (:degrees details)))]))))))

;; Autocomplete: based on inputs, see if you're playing a known progression

; With shapes
(comment
  (let [; Start of a ii-V-I
        ii (jigsaw/->shape :D_m)
        v (jigsaw/->shape :G_maj)
        chords [ii v]]
    (l/run 5 [q]
           (l/fresh [?shapes ?conn ?chords ?prog]
                    (prepare-shapes ?shapes chords)
                    (connecto ?conn ?shapes)
                    (l/featurec ?conn {:contextualized-shapes ?chords})
                    (progresso ?prog ?chords)
                    (l/== q [?conn ?prog])))))

; With note seqs (i.e. account for fuzziness)
(comment (let [; Start of a ii-V-I
               ii (jigsaw/->shape :D4_m)
               ii-notes (:notes ii)
               v (jigsaw/->shape :G4_maj)
               v-notes (:notes v)
               i (jigsaw/->shape :C4_maj)
               i-notes (:notes i)
               #_#_note-seqs [ii-notes v-notes i-notes]
               note-seqs [[:Gb4 :A4 :C5 :E5]
                          [:Gb4 :A4 :B4 :Eb5]
                          [:E4 :G4 :B4]]]
           (l/run 1 [q]
                  (l/fresh [?shapes ?conn ?contextualized-shapes ?prog]
                           (prepare-shapes ?shapes note-seqs)
                           (connecto ?conn ?shapes)
                           (l/featurec ?conn {:contextualized-shapes ?contextualized-shapes})
                           (progresso ?prog ?contextualized-shapes)
                           (l/== q [?conn ?prog])))))
; ([{:connection {:pitch :G, :name :major-augmented},
;    :contextualized-shapes
;    ({:pitch :A,
;      :name :m6,
;      :bass :F#,
;      :heuristics
;      {:contains? 1,
;       :fully-contains? 0,
;       :contained-in? 1,
;       :fully-contained-in? 0,
;       :overlap 1.0,
;       :same-pitch-count? 1,
;       :shares-root? 0},
;      :input [:Gb4 :A4 :C5 :E5],
;      :context :chord-degree/ii}
;     {:pitch :E,
;      :name :M9sus4,
;      :bass :F#,
;      :heuristics
;      {:contains? 0,
;       :fully-contains? 0,
;       :contained-in? 1,
;       :fully-contained-in? 1,
;       :overlap 0.8,
;       :same-pitch-count? 0,
;       :shares-root? 0},
;      :input [:Gb4 :A4 :B4 :Eb5],
;      :context :chord-degree/vi}
;     {:pitch :E,
;      :name :m,
;      :heuristics
;      {:contains? 1,
;       :fully-contains? 0,
;       :contained-in? 1,
;       :fully-contained-in? 0,
;       :overlap 1.0,
;       :same-pitch-count? 1,
;       :shares-root? 1},
;      :input [:E4 :G4 :B4],
;      :context :chord-degree/vi})}
;   ["Circle progression"
;    {:degrees
;     [:chord-degree/vi
;      :chord-degree/ii
;      :chord-degree/V
;      :chord-degree/I],
;     :quality :major}]])
