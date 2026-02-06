(ns jigsaw.experiments.logic
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}
                                 :invalid-arity {:level :off}}}}
  (:require
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as fd]
   [clojure.set :as set]
   [clojure.string :as str]
   [jigsaw.core :as jigsaw]
   [jigsaw.impl.theory :as theory]))

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

; Thank you, Tim Baldridge
(defmacro with-fresh
  [& body]
  (let [lvars (->> body
                   flatten
                   (map #(if (map? %)
                           (seq %)
                           %))
                   flatten
                   (filter simple-symbol?)
                   (remove #(contains? &env %))
                   (filter #(str/starts-with? (name %) "?"))
                   distinct)]
    `(l/fresh [~@lvars]
              ~@body)))

(comment
  (l/run 3 [q]
         (with-fresh
           (l/== ?start :C_maj)
           (neighboro ?start ?a)

           (neighboro ?a ?b)
           (l/featurec ?b {:context :mode/II})

           (l/== q [?start ?a ?b]))))

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
  (with-fresh
    (l/== ?connection {:pitch ?common-pitch :name ?common-name})
    (connect-neighbors shapes ?contextualized-shapes ?common-pitch ?common-name)
    (l/== q {:connection ?connection
             :contextualized-shapes ?contextualized-shapes})))

(defn shapeo [?shape x]
  (l/project [x]
             (if (theory/notes? x)
               (noteso ?shape x)
               (l/== ?shape x))))

; Support input of shapes or note-seqs which map to one or more shapes
(l/defne prepare-shapes [?shapes xs]
  ([() ()])
  ([[?shape . ?rest-shapes] [x . rest-xs]]
   (shapeo ?shape x)
   (prepare-shapes ?rest-shapes rest-xs)))

; Find connection with shapes
(comment
  (let [; Start of a ii-V-I
        ii (jigsaw/->shape :D_m)
        v (jigsaw/->shape :G_maj)
        chords [ii v]]
    (l/run 5 [q]
           (with-fresh
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
           (with-fresh
             (prepare-shapes ?shapes note-seqs)
             (connecto ?conn ?shapes)
             (l/== q ?conn)))))

; More connections from notes
(comment
  (let [note-seqs [[:Gb4 :A4 :C5 :E5]
                   [:Gb4 :A4 :B4 :Eb5]
                   [:E4 :G4 :B4]]]
    (l/run 5 [q]
           (with-fresh
             (prepare-shapes ?shapes note-seqs)
             (connecto ?conn ?shapes)
             (l/== q ?conn)))))

; Search nested relations
(comment
  (l/run 5 [q]
         (with-fresh
           ; Start at C major chord
           (l/== ?start {:pitch :C :name :maj})

           ; Find scale where it's a V chord
           (neighboro ?start ?scale)
           (l/featurec ?scale {:context :chord-degree/V})

           ; Find the corresponding I chord (specifically major 7th) of the scale
           (neighboro ?scale ?end)
           (l/featurec ?end {:context :chord-degree/IM7})

           ; Return the results
           (l/== q ?scale))))
; ({:pitch :F, :name :major, :context :chord-degree/V}
;  {:pitch :F, :name :bebop, :context :chord-degree/V}
;  {:pitch :F, :name :lydian, :context :chord-degree/V}
;  {:pitch :F, :name :harmonic-major, :context :chord-degree/V}
;  {:pitch :F, :name :bebop-major, :context :chord-degree/V})

; More searching of nested relations
(comment
  (l/run 1 [q]
         (with-fresh
           ; Start at C major chord
           (l/== ?start {:pitch :C :name :maj})

           ; Find scale where it's a V chord
           (neighboro ?start ?scale1)
           (l/featurec ?scale1 {:context :chord-degree/V})

           ; Find second mode of that scale
           (neighboro ?scale1 ?scale2)
           (l/featurec ?scale2 {:context :mode/II})

           ; Find the corresponding I chord of the second mode
           (neighboro ?scale2 ?end)
           (l/featurec ?end {:context :chord-degree/I})

           ; Return the results
           (l/== q [?scale1 ?scale2 ?end]))))
; ([{:pitch :F, :name :lydian, :context :chord-degree/V}
;   {:pitch :G, :name :mixolydian, :context :mode/II}
;   {:pitch :G, :name :maj, :context :chord-degree/I}])

; Secondary dominant (V/V or V7/V)
(comment
  (l/run 1 [q]
         (with-fresh
           ; Start at C major scale
           (l/== ?start-scale {:pitch :C :name :major})

            ; Find the dominant (V) chord
           (neighboro ?start-scale ?dom-chord)
           (l/featurec ?dom-chord {:context :chord-degree/V})

            ; Find the scale where the dominant chord is the I
           (neighboro ?dom-chord ?dom-scale)
           (l/featurec ?dom-scale {:context :chord-degree/I})

            ; Find that scale's dominant (i.e. tonicized chord)
           (neighboro ?dom-scale ?dom-chord2)
           (l/featurec ?dom-chord2 {:context :chord-degree/V})

            ; Return the secondary dominant
           (l/== q ?dom-chord2))))
; => ({:pitch :D, :name :maj, :context :chord-degree/V})

(defn transposo
  "Transpose pitch/note/shape by interval"
  [q x interval & [multiplier]]
  (l/project [x]
             (l/== q (theory/transpose x interval multiplier))))

; Tritone substitution
(comment
  (l/run 1 [q]
         (with-fresh
           (l/== ?scale {:pitch :C :name :major})

           (neighboro ?scale ?ii)
           (l/featurec ?ii {:context :chord-degree/ii7})

           (neighboro ?scale ?v)
           (l/featurec ?v {:context :chord-degree/V7})

           (neighboro ?scale ?i)
           (l/featurec ?i {:context :chord-degree/IM7})

            ; Find the shape which is a tritone away from the V chord
           (transposo ?sub ?v :d5)
           (l/== q [?ii ?sub ?i]))))
; ([{:pitch :D, :name :m7, :context :chord-degree/ii7}
;   {:pitch :Db, :name :7}
;   {:pitch :C, :name :maj7, :context :chord-degree/IM7}])

; Coltrane changes
(comment
  (let [key1 (jigsaw/->shape :C_major)]
    (l/run 1 [q]
           (with-fresh
             ; Normal ii-V-I
             (neighboro key1 ?ii)
             (l/featurec ?ii {:context :chord-degree/ii7})

             (neighboro key1 ?v)
             (l/featurec ?v {:context :chord-degree/V7})

             (neighboro key1 ?i)
             (l/featurec ?i {:context :chord-degree/IM7})

              ; Key goes down a third
             (transposo ?key2 key1 :M3 -1)

              ; New V-I
             (neighboro ?key2 ?v2)
             (l/featurec ?v2 {:context :chord-degree/V7})

             (neighboro ?key2 ?i2)
             (l/featurec ?i2 {:context :chord-degree/IM7})

              ; Key goes down another third
             (transposo ?key3 ?key2 :M3 -1)

              ; New V-I
             (neighboro ?key3 ?v3)
             (l/featurec ?v3 {:context :chord-degree/V7})

             (neighboro ?key3 ?i3)
             (l/featurec ?i3 {:context :chord-degree/IM7})

             (l/== q [?ii ?v2 ?i2 ?v3 ?i3 ?v ?i])))))
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
  (with-fresh
    (heuristico ?h from to)
    (l/featurec ?h {:overlap ?overlap :same-pitch-count? ?pc})
    (l/conde
      ; Perfect pitch match
     [(fd/== ?overlap 100)]
      ; Same pitch count and decent pitch overlap
     [(fd/== ?pc 100) (fd/>= ?overlap 50)]
      ; Hacky sort?
     [(fd/>= ?overlap 95)]
     [(fd/>= ?overlap 90)]
     [(fd/>= ?overlap 80)]
     [(fd/>= ?overlap 70)]
     [(fd/>= ?overlap 60)])))

; Fit (find closest compatible shape, to a reference shape)
; i.e. <input> ~= ? <-> <shape>
(comment
  (let [start (jigsaw/->shape :C_m)
        parent (jigsaw/->shape :C_major)]
    (l/run 3 [q]
           (with-fresh
              ; Find the shape(s) in the C major scale...
             (neighboro parent ?candidate)
              ; ...which are close (pitch-wise) to the C minor chord
             (fuzzy-neighborc start ?candidate)
             (l/== q ?candidate)))))
; => ({:pitch :C, :name :maj, :context :chord-degree/I}
;     {:pitch :C, :name :sus4, :context :chord-degree/i}
;     {:pitch :C, :name :sus2, :context :chord-degree/i})

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

(defn progresso
  "Find compatible progresion, given some chords (with degree contexts)"
  [q contextualized-chords & {:keys [p] :or {p set/subset?}}]
  ; TODO: just pass in degrees, not full chords
  (l/project [contextualized-chords]
             (l/membero q (->> theory/chord-progressions
                               (filter (fn [[prog-name details]]
                                         (p (set (map :context contextualized-chords))
                                            (set (:degrees details)))))))))

;; Autocomplete: based on inputs, see if you're playing a known progression

; With shapes
(comment
  (let [; Start of a ii-V-I
        ii (jigsaw/->shape :D_m)
        v (jigsaw/->shape :G_maj)
        chords [ii v]]
    (l/run 5 [q]
           (with-fresh
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
                  (with-fresh
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

(def all-shapes
  (for [pitch theory/simple-pitch-keys
        shape-name (concat (keys theory/chords) (keys theory/scales))]
    (let [shape (jigsaw/->shape {:pitch pitch :name shape-name})
          pitches (:pitches shape)
          pcis (mapv #(theory/pitches %) pitches)]
      (assoc shape :pcis pcis))))

(defn shape-rel [q]
  (fn [a]
    (l/to-stream
     (map #(l/unify a % q) all-shapes))))

(defn heuristico2 [q p1 p2]
  (l/project [p1 p2]
             (l/== q (into {}
                           (map (fn [[k v]] (vector k (int (* 100 v))))
                                (theory/calculate-heuristics p1 p2))))))

(defn notes->shapes [q notes]
  (let [pcis (map #(-> % theory/parts :pci) notes)]
    (with-fresh
      (shape-rel ?shape)
      (l/featurec ?shape {:pcis ?shape-pcis})
      (heuristico2 ?h pcis ?shape-pcis)
      (l/featurec ?h {:overlap ?overlap :same-pitch-count? ?pc})
      (l/conde
      ; [(fd/== ?pc 100) (fd/>= ?overlap 70)]
      ; [(fd/== ?overlap 100)]
      ; [(fd/>= ?overlap 95)]
       [(fd/>= ?overlap 90) (l/conjo ?shape {:heuristics ?h} q)]
      ; [(fd/>= ?overlap 80)]
       #_[(fd/>= ?overlap 70)]
       #_[(fd/>= ?overlap 60)]))))

(comment
  (l/run 10 [q]
         (notes->shapes q [:C4 :E4 :G4])))

; Attempt to use logic for shape relations
(comment
  (let []
    (l/run 10 [q]
           (with-fresh
             (l/== ?pitch :C)
             (shape-rel ?shape)
             (? ?shape :pitch ?pitch)
             (? ?shape :name :major)
             #_(l/featurec ?shape {:pitch ?pitch :name :major})
             #_(l/project [?shape]
                          (fd/> (int (* 100 (theory/jaccard-index #{0 4 7} (set (:pcis ?shape))))) 50))
             (l/== q ?shape)))))

;; Different projections; find different ways to think about same notes (PCIs)

(defn alto
  "
  Given a shape, find alternative ways of thinking about that shape (what else could it be?), based on PCIs.
  Like noteso, but with disequality on the current shape or its enarmonic equivalent
  "
  [?q shape]
  (l/project [shape]
             (let [pcis (map theory/pitches (:pitches shape))]
               (with-fresh
                 (noteso ?shape (:notes shape))
                 (l/featurec ?shape {:pitch ?pitch :name ?name :pcis ?pcis})
                 (l/!= (:name shape) ?name)
                  ; Don't use same pitch or enharmonic equivalent
                 (l/firsto ?pcis ?first-pci)
                 (l/!= (first pcis) ?first-pci)
                 (l/== ?q ?shape)))))
(comment
  (let [shape (jigsaw/->shape :C4_maj)]
    (map #(assoc % :connection (jigsaw/->shape (:connection %)))
         (l/run 5 [q]
                (with-fresh
                  (alto ?shape shape)
                  (l/featurec ?shape {:pitch ?pitch})
                  (l/!= :Fb ?pitch)
                  (connecto ?conn [?shape])
                  ; (l/featurec ?conn {:contextualized-shapes ?cs})
                  #_(neighboro ?shape ?conn)
                  #_(l/is ?degs ?conn #(map :context (:contextualized-shapes %)))
                  (l/== q ?conn))))))
