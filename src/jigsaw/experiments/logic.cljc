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

(defn flatten-to-death
  [x]
  (cond
    (coll? x) (mapcat flatten-to-death x)
    :else (list x)))

; Thank you, Tim Baldridge
(defmacro with-fresh
  [& body]
  (let [lvars (->> body
                   flatten-to-death
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
             (cond
               (theory/shape? x) (l/== ?shape x)
               (theory/notes? x) (noteso ?shape x)
               (theory/shape-ref? x) (l/== ?shape (jigsaw/->shape x)))))

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
  [q contextualized-chords & {:keys [pred] :or {pred set/subset?}}]
  ; TODO: just pass in degrees, not full chords
  (l/project [contextualized-chords]
             (l/membero q (->> theory/chord-progressions
                               (filter (fn [[_ details]]
                                         (pred (set (map :context contextualized-chords))
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
; Slow; note-seqs * possible shapes * possible connections * possible progressions
; Good for getting whole picture, but only if each goal passes; intermediate goal results tossed
(comment (let [note-seqs [[:Gb4 :A4 :C5 :E5]
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

;; Different projections; find different ways to think about same notes (PCIs)
(defn alto
  "
  Given a shape, find alternative ways of thinking about that shape (what else could it be?), based on PCIs.
  Like noteso, but with disequality on the current shape and its enarmonic equivalent
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

; What else could the C major chord be?
(comment
  (let [shape (jigsaw/->shape :C4_maj)]
    (l/run* [q]
            (with-fresh
              (alto ?shape shape)
              (l/== q ?shape)))))
; => ({:pitch :Fb,
;      :name :m#5,
;      :bass :C,
;      :heuristics
;      {:contains? 1,
;       :fully-contains? 0,
;       :contained-in? 1,
;       :fully-contained-in? 0,
;       :overlap 1.0,
;       :same-pitch-count? 1,
;       :shares-root? 0},
;      :pcis [4 7 0],
;      :input [:C4 :E4 :G4]}
;     {:pitch :E,
;      :name :m#5,
;      :bass :B#,
;      :heuristics
;      {:contains? 1,
;       :fully-contains? 0,
;       :contained-in? 1,
;       :fully-contained-in? 0,
;       :overlap 1.0,
;       :same-pitch-count? 1,
;       :shares-root? 0},
;      :pcis [4 7 0],
;      :input [:C4 :E4 :G4]})

; Now find scales which work with above
(comment
  (let [shape (jigsaw/->shape :C4_maj)]
    (l/run 5 [q]
           (with-fresh
             (alto ?shape shape)
             (l/featurec ?shape {:pitch ?pitch :name ?name})
             (l/!= :Fb ?pitch)
             (neighboro ?shape ?conn)
             #_(connecto ?conn [?shape])
             #_(l/featurec ?conn {:connection ?scale})
             #_(l/is ?degs ?conn #(map :context (:contextualized-shapes %)))
             (l/== q {:alt {:pitch ?pitch :name ?name}
                      :alt-scale ?conn})))))

; Find path between two shapes
(l/defne not-membero [x l]
  ([_ []])
  ([_ [?y . ?r]]
   (l/!= x ?y)
   (not-membero x ?r)))

(l/defne reverseo [lst acum res]
  ([[] _ acum])
  ([[?x . ?y] ?z _]
   (l/fresh [w]
            (l/conso ?x ?z w)
            (reverseo ?y w res))))

(defn neighbor-refo
  "Stick with pitch+name keys to let map (dis)equality work below, otherwise :context will throw things off"
  [from to]
  (l/project [from]
             (l/membero to (map #(select-keys % [:pitch :name]) (->shapes from)))))

; Thank you, David Nolan
(l/defne travelo [a b visited max-depth path]
  ([?a ?b _ _ [b . visited]] (neighbor-refo ?a ?b))
  ([?a ?b ?v ?d ?p]
   (fd/<= 0 ?d)
   (l/fresh [c vis d]
            (neighbor-refo ?a c)
            (l/!= ?b c)
            (not-membero c ?v)
            (l/conso c ?v vis)
            (fd/- ?d 1 d)
            (travelo c ?b vis d ?p))))

(defn patho [start end depth res]
  (let [start-ref (select-keys (jigsaw/->shape start) [:pitch :name])
        end-ref (select-keys (jigsaw/->shape end) [:pitch :name])]
    (l/fresh [path]
             (travelo start-ref end-ref [start-ref] depth path)
             (reverseo path [] res))))

(defn contextualize-path [path]
  (reduce
   (fn [v current]
     (if (empty? v)
       (conj v current)
       (let [prev (last v)
             context (jigsaw/contextualize (jigsaw/->shape prev) (jigsaw/->shape current))]
         (conj v (assoc current :context context)))))
   [] path))

(defn find-paths [start end & {:keys [depth n]
                               :or {depth 1
                                    n 1}}]
  (->>
   (l/run n [p]
          (patho start end depth p))
   (map contextualize-path)))

(comment
  (find-paths :C_maj :A_m7b5 :n 2 :depth 2))
; => ([{:pitch :C, :name :maj}
;      {:pitch :C, :name :major-blues, :context :chord-degree/I}
;      {:pitch :A, :name :m7b5, :context :chord-degree/vi%}]
;     [{:pitch :C, :name :maj}
;      {:pitch :C, :name :major-blues, :context :chord-degree/I}
;      {:pitch :A, :name :minor-blues, :context :mode/VI}
;      {:pitch :A, :name :m7b5, :context :chord-degree/i%}])

(comment
  (->>
   (find-paths :C_maj7 :G_7 :depth 1)
   #_(map #(map jigsaw/->shape %))))

; Composing paths
(comment
  (l/run 1 [q]
         (with-fresh
           (patho :C_maj7 :G_7 1 ?p1)
           (patho :G_7 :A_m7 1 ?p2)
           (l/== q [?p1 ?p2]))))

(comment
  (l/run 1 [q]
         (with-fresh
           (patho :C_maj7 :G_7 1 ?p1)
           (l/resto ?p1 ?rest)
           (l/firsto ?rest ?second)
           (l/featurec ?second {:pitch ?pitch})
           (l/!= ?pitch :C)
           (l/!= ?pitch :D)
           (patho :G_7 :A_m7 1 ?p2)
           (l/== q [?p1 ?second ?p2]))))

; TODO: Neighboring paths; account for fuzziness
