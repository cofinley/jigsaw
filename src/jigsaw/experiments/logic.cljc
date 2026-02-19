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

(defn flatten-to-death
  "flatten, accounting for maps"
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
                   (filter #(and (str/starts-with? (name %) "?")
                                 (not (str/starts-with? (name %) "?="))))
                   distinct)]
    `(l/fresh [~@lvars]
              ~@body)))

(defn ->shapes [x]
  (cond
    (theory/shape? x) (jigsaw/shape->shapes x)
    (theory/shape-ref? x) (->shapes (jigsaw/->shape x))
    (and (map? x) (contains? x :notes)) (->shapes (jigsaw/->shape x))
    (keyword? x) (->shapes (jigsaw/->shape x))
    :else (lazy-seq)))

(defn neighboro
  "Shapes from shape"
  [from to]
  (l/project [from]
             (l/membero to (->shapes from))))

(defn noteso
  "Potential shapes from notes"
  [q notes]
  (l/project [notes]
             (l/membero q (jigsaw/notes->shapes notes :max-shapes 500))))

(defn shapeo [?shape x]
  (l/project [x]
             (cond
               (theory/shape? x) (l/== ?shape x)
               (theory/notes? x) (noteso ?shape x)
               :else (l/== ?shape (jigsaw/->shape x)))))

(defn shape-ido [?id ?shape]
  (l/project [?shape]
             (l/== ?id (select-keys ?shape [:pitch :name]))))

(defn shape==
  "Shape equality, based on pitch + name"
  [?a ?b]
  (with-fresh
    (shapeo ?shape1 ?a)
    (shapeo ?shape2 ?b)
    (shape-ido ?ref1 ?shape1)
    (shape-ido ?ref2 ?shape2)
    (l/== ?ref1 ?ref2)))

(defn shape!=
  "Shape disequality, based on pitch + name"
  [?a ?b]
  (with-fresh
    (shapeo ?shape1 ?a)
    (shapeo ?shape2 ?b)
    (shape-ido ?ref1 ?shape1)
    (shape-ido ?ref2 ?shape2)
    (l/!= ?ref1 ?ref2)))

(comment
  (l/run 1 [q]
         (with-fresh
           (l/== q {:pitch :C :name :maj :context :chord-degree/iii})
           (shape== q {:pitch :C :name :maj :context :chord-degree/I})
           (shape== q :C_maj))))

(comment
  (l/run 3 [q]
         (with-fresh
           (l/== ?start :C_maj)
           (neighboro ?start ?a)

           (neighboro ?a ?b)
           (l/featurec ?b {:context :mode/II})

           (l/== q [?a ?b]))))

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
             (connecto ?conn chords)
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
           (l/featurec ?end {:context :chord-degree/Imaj7})

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

; What's the V chord of A?
(comment
  (l/run 1 [q]
         (with-fresh
           (l/== ?scale :A_major)
           (neighboro ?scale ?v)
           (l/featurec ?v {:context :chord-degree/V})
           (l/== q ?v))))

; Secondary dominant (V/V or V7/V)
(comment
  (l/run 1 [q]
         (with-fresh
           ; Start at C major scale
           (l/== ?start-scale {:pitch :C :name :major})

            ; Find the tonic (I) chord
           (neighboro ?start-scale ?i)
           (l/featurec ?i {:context :chord-degree/I})

            ; Find the dominant (V) chord
           (neighboro ?start-scale ?v)
           (l/featurec ?v {:context :chord-degree/V})

            ; Find the scale where the dominant chord is the I (i.e. the now-tonicized chord)
           (neighboro ?v ?dom-scale)
           (l/featurec ?dom-scale {:context :chord-degree/I})

            ; Find that scale's dominant
           (neighboro ?dom-scale ?v-v)
           (l/featurec ?v-v {:context :chord-degree/V})

            ; Return the secondary dominant
           (l/== q [?v-v ?v ?i]))))
; => ([{:pitch :D,
;       :name :maj,
;       :context :chord-degree/V,
;       :parent-scale {:pitch :G, :name :major}}
;      {:pitch :G,
;       :name :maj,
;       :context :chord-degree/V,
;       :parent-scale {:pitch :C, :name :major}}
;      {:pitch :C,
;       :name :maj,
;       :context :chord-degree/I,
;       :parent-scale {:pitch :C, :name :major}}])

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
           (l/featurec ?ii {:context :chord-degree/iim7})

           (neighboro ?scale ?v)
           (l/featurec ?v {:context :chord-degree/V7})
            ; Find the shape which is a tritone away from the V chord
           (transposo ?sub ?v :d5)

           (neighboro ?scale ?i)
           (l/featurec ?i {:context :chord-degree/Imaj7})

           (l/== q [?ii ?sub ?i]))))
; ([{:pitch :D,
;    :name :m7,
;    :context :chord-degree/iim7,
;    :parent-shape {:pitch :C, :name :major}}
;   {:pitch :Db,
;    :name :7,
;    :context :interval/d5,
;    :parent-shape {:pitch :G, :name :7}}
;   {:pitch :C,
;    :name :maj7,
;    :context :chord-degree/Imaj7,
;    :parent-shape {:pitch :C, :name :major}}])

; Coltrane changes
(comment
  (let [key1 (jigsaw/->shape :C_major)]
    (l/run 1 [q]
           (with-fresh
             ; Normal ii-V-I
             (neighboro key1 ?ii)
             (l/featurec ?ii {:context :chord-degree/iim7})

             (neighboro key1 ?v)
             (l/featurec ?v {:context :chord-degree/V7})

             (neighboro key1 ?i)
             (l/featurec ?i {:context :chord-degree/Imaj7})

              ; Key goes down a third
             (transposo ?key2 key1 :M3 -1)

              ; New V-I
             (neighboro ?key2 ?v2)
             (l/featurec ?v2 {:context :chord-degree/V7})

             (neighboro ?key2 ?i2)
             (l/featurec ?i2 {:context :chord-degree/Imaj7})

              ; Key goes down another third
             (transposo ?key3 ?key2 :M3 -1)

              ; New V-I
             (neighboro ?key3 ?v3)
             (l/featurec ?v3 {:context :chord-degree/V7})

             (neighboro ?key3 ?i3)
             (l/featurec ?i3 {:context :chord-degree/Imaj7})

             (l/== q [?ii ?v2 ?i2 ?v3 ?i3 ?v ?i])))))
; ([{:pitch :D, :name :m7, :context :chord-degree/iim7}
;   {:pitch :Eb, :name :7, :context :chord-degree/V7}
;   {:pitch :Ab, :name :maj7, :context :chord-degree/Imaj7}
;   {:pitch :B, :name :7, :context :chord-degree/V7}
;   {:pitch :E, :name :maj7, :context :chord-degree/Imaj7}
;   {:pitch :G, :name :7, :context :chord-degree/V7}
;   {:pitch :C, :name :maj7, :context :chord-degree/Imaj7}])

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
    (l/run 1 [q]
           (with-fresh
             (prepare-shapes ?shapes chords)
             (connecto ?conn ?shapes)
             (l/featurec ?conn {:contextualized-shapes ?chords})
             (progresso ?prog ?chords)
             (l/== q [?conn ?prog])))))
; ([{:connection {:pitch :C, :name :major},
;    :contextualized-shapes
;    ({:intervals [:P1 :m3 :P5],
;      :pitch :D,
;      :name :m,
;      :pitches [:D :F :A],
;      :context :chord-degree/ii}
;     {:intervals [:P1 :M3 :P5],
;      :pitch :G,
;      :name :maj,
;      :pitches [:G :B :D],
;      :context :chord-degree/V})}
;   ["Montgomery–Ward bridge"
;    {:degrees
;     [:chord-degree/I
;      :chord-degree/IV
;      :chord-degree/ii
;      :chord-degree/V],
;     :quality :major}]])

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
  [?q ?shape-ref]
  (l/project [?shape-ref]
             (let [_shape (jigsaw/->shape ?shape-ref)
                   ; Fix shapes without notes
                   shape (if (contains? _shape :notes)
                           _shape
                           (jigsaw/->shape (theory/pitch->note (:pitch _shape)) (:name _shape)))
                   pcis (map theory/pitches (:pitches shape))]
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

; Now find scales which work with above (i.e. fuzzy neighbors)
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
             (let [shapes (->shapes from)]
               (l/membero to (map #(select-keys % [:pitch :name]) shapes)))))

; Thank you, David Nolen
(l/defne travelo2 [a b visited max-depth path]
  ([?a ?b _ _ [b . visited]] (neighbor-refo ?a ?b))
  ([?a ?b ?v ?d ?p]
   (fd/<= 0 ?d)
   (l/fresh [c vis d]
            (neighbor-refo ?a c)
            (l/!= ?b c)
            (not-membero c ?v)
            (l/conso c ?v vis)
            (fd/- ?d 1 d)
            (travelo2 c ?b vis d ?p))))

(l/defne travelo [a b visited-ids visited-shapes max-depth path]
  ([?a ?b _ _ _ [?b' . visited-shapes]]
   ; ?b' is the contextualized version of ?b
   (neighboro ?a ?b')
   ; need to check subset of keys for equality, but use contextualized version in path
   (shape== ?b ?b'))
  ([?a ?b ?v ?vs ?d ?p]
   (fd/<= 0 ?d)
   (l/fresh [?c ?cid ?v' ?vs' ?d']
            (neighboro ?a ?c)
            (shape!= ?b ?c)
            ; Base visited on pitch+names...
            (shape-ido ?cid ?c)
            (not-membero ?cid ?v)
            (l/conso ?cid ?v ?v')
            ; But store contextualized versions of shapes for path later
            (l/conso ?c ?vs ?vs')
            (fd/- ?d 1 ?d')
            (travelo ?c ?b ?v' ?vs' ?d' ?p))))

(defn patho [start end depth res]
  (let [start (jigsaw/->shape start)
        end (jigsaw/->shape end)]
    (l/fresh [path]
             (travelo start end [start] [start] depth path)
             (reverseo path [] res))))

(comment
  (l/run 1 [q]
         (with-fresh
           (patho :C_maj :D_m 1 q)
           #_(l/membero ?step q)
           #_(l/featurec ?step {:pitch :A}))))

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
           (patho :C_maj7 :C_m 1 q)
           (l/membero ?step q)
           (l/featurec ?step {:pitch :A}))))

; Borrowed chord, from parallel C minor
(comment
  (l/run 1 [q]
         (with-fresh
           (patho :C_maj7 :C_m 2 q))))
; (({:intervals [:P1 :M3 :P5 :M7],
;    :pitch :C,
;    :name :maj7,
;    :pitches [:C :E :G :B]}
;   {:pitch :C,
;    :name :major,
;    :context :chord-degree/Imaj7,
;    :parent-shape {:pitch :C, :name :major}}
;   {:pitch :C,
;    :name :minor,
;    :context :mode/parallel,
;    :parent-shape {:pitch :C, :name :major}}
;   {:pitch :C,
;    :name :m,
;    :context :chord-degree/i,
;    :parent-shape {:pitch :C, :name :minor}}))

(defn resolvo [?scale ?degree ?chord]
  (l/conde
   ; Need scale
   [(l/== true (l/lvar? ?scale))
    (neighboro ?chord ?scale)
    (l/featurec ?scale {:context ?degree})]
   ; Need degree
   [(l/== true (l/lvar? ?degree))
    (l/fresh [?chord-neighbor]
             (neighboro ?scale ?chord-neighbor)
             (shape== ?chord ?chord-neighbor)
             (l/featurec ?chord-neighbor {:context ?degree}))]
   ; Need chord
   [(l/== true (l/lvar? ?chord))
    (neighboro ?scale ?chord)
    (l/featurec ?chord {:context ?degree})]))

(l/defne resolvo* [?scale ?degrees ?chords]
  ([?s [] []])
  ([?s [?deg . ?rest-degs] [?chord . ?rest-chords]]
   ; (l/log "start" ?s ?deg ?chord)
   (l/conde
     ; Need scale
    [(l/lvaro ?s)
     (neighboro ?chord ?s)
     ; (l/trace-lvars "need scale" ?s ?chord)
     (l/featurec ?s {:context ?deg})
     (resolvo* ?s ?rest-degs ?rest-chords)]
     ; Need degrees
    [(l/lvaro ?deg)
     ; (l/trace-lvars "need deg" ?s ?deg ?chord)
     (l/fresh [?chord-neighbor]
              (neighboro ?s ?chord-neighbor)
              (shape== ?chord ?chord-neighbor)
              (l/featurec ?chord-neighbor {:context ?deg})
              (resolvo* ?s ?rest-degs ?rest-chords))]
     ; Need chords
    [(l/lvaro ?chord)
     ; (l/trace-lvars "need chord" ?s ?chord ?deg)
     (neighboro ?s ?chord)
     (l/featurec ?chord {:context ?deg})
     (resolvo* ?s ?rest-degs ?rest-chords)])))

(comment
  (l/run 2 [q]
         (with-fresh
           (resolvo q :chord-degree/V :E_maj))))

(comment
  (l/run 3 [q]
         (with-fresh
           (resolvo :A_major q :E_maj))))

(comment
  (l/run 3 [q]
         (with-fresh
           (resolvo :A_major :chord-degree/V ?v)
           (resolvo :A_major :chord-degree/I ?i)
           (l/== q [?v ?i]))))

(comment
  (l/run 1 [q]
         (with-fresh
           (resolvo* q [:chord-degree/V] [:E_maj]))))

(comment
  (l/run 3 [q]
         (with-fresh
           (resolvo* :A_major q [:E_maj]))))

(comment
  (l/run 3 [q]
         (with-fresh
           (resolvo* :A_major [:chord-degree/V :chord-degree/I] q))))

(comment
  (l/run 1 [q]
         (with-fresh
           (resolvo* :A_major [:chord-degree/V :chord-degree/I] ?chords)
           (l/is q ?chords (fn [chords] (map #(theory/transpose % :M3) chords))))))

(defn ?=
  "Fuzzy shape; get similar shapes to ?shape; can include ?shape"
  [?possible-shape ?shape-ref]
  (l/project [?shape-ref]
             (let [_shape (jigsaw/->shape ?shape-ref)
                   ; Fix shapes without notes
                   shape (if (contains? _shape :notes)
                           _shape
                           (jigsaw/->shape (theory/pitch->note (:pitch _shape)) (:name _shape)))]
               (noteso ?possible-shape (:notes shape)))))

(comment
  (l/run 2 [q]
         (with-fresh
           (?= q :C_maj))))

(comment
  (l/run 2 [q]
         (with-fresh
           (?= ?chord :C_maj)
           (resolvo ?scale :chord-degree/i ?chord)
           (l/== q ?scale))))

(comment
  (l/run 2 [q]
         (with-fresh
           (neighboro :C_maj ?neighbor)
           (resolvo ?neighbor :mode/II q))))

(comment
  (l/run 1 [q]
         (with-fresh
           (resolvo ?scale :chord-degree/V :C_maj)
           (resolvo ?scale :chord-degree/I q))))
; ({:pitch :F,
;   :name :maj,
;   :context :chord-degree/I,
;   :parent-shape {:pitch :F, :name :major}})

(comment
  (l/run 1 [q]
         (with-fresh
           (resolvo* ?scale [:chord-degree/V :chord-degree/I] [:C_maj q]))))
; ({:pitch :F,
;   :name :maj,
;   :context :chord-degree/I,
;   :parent-shape {:pitch :F, :name :major}})

(comment
  (l/run 1 [q]
         (with-fresh
           (resolvo* ?scale [:chord-degree/V :chord-degree/I] [:C_maj ?i])
           (resolvo* ?scale2 [:chord-degree/V :chord-degree/I] [?i q]))))
; ({:pitch :Bb,
;   :name :maj,
;   :context :chord-degree/I,
;   :parent-shape {:pitch :Bb, :name :major}})

(comment
  (l/run 2 [q]
         (with-fresh
           (?= ?chord :C_maj)
           (l/featurec ?chord {:pcis ?pcis})
           (not-membero 4 ?pcis)
           (l/membero 3 ?pcis)
           ; (resolvo ?scale :chord-degree/I ?chord)
           (l/== q [?chord #_?scale]))))

; (jig ...)               ; syntax wrapper
; C_maj                   ; realized chord
; C_major                 ; realized scale
; V I                     ; chord degrees
; mode/II                 ; mode (differentiated from degree roman numerals)
; M3 P5 A4                ; intervals
; (A_major V)             ; get chord from parent scale by degree(s)
; (== E_maj (?s V))       ; get scale from chord and degree(s)
; (- (A_major ii V I) M3) ; transpose chords (from a scale and degree(s)) by an interval
; (?s [C E G] [D F A])    ; find scale, chords from sets of notes (i.e. connect)
; (C_major ~[C Eb G])     ; fit notes to scale, get chord
; ~C_maj                  ; use fuzzy representation of chord (i.e. alto, maybe keep original too instead of disequality on it)
; [(C_major ii V) (Eb_major V I)] ; modulated progression

; C_maj : strict input
; [<pitches/notes] : strict input
; ~C_maj : fuzzy input (i.e. from shape notes)
; ~[<pitches/notes] : fuzzy input
; [C ~E G] : fuzzy input on specific note(s); other notes are strict
; ~C_maj !C : mix fuzzy with strict constraints (?)

; Connect
; (?s C_maj D_m)
; Proposed: (?s [C E G] [D F A])
; 
; Tritone sub:
; (C_major ii (t- v d5) i)
;   'v' is relative to parent
;
; Secondary
;   (C_major V-V)
;   Alternative possibility? (C_major (V V))  ; confusing when nested?
; Tertiary
;   (C_major V-V-V)
; ...ary
;   (C_major V-V-V-...)
;
; Coltrane
; [(C_major ii)
;  (t- (C_major V I) M3)
;  (t- (C_major V I) (t* M3 2))
;  (t- (C_major V I) (t* M3 3))
;  (C_major V I)] 
; 
; Fit
; (C_major ~[C Eb G])  ; fuzzy input, but outputs should conform to scale
;
; Progression-finding
; [(?s D_m G_maj) ?...]
; (progs D_m G_maj)
;
; Path-finding
; [C_maj --> ?x --> D_m]
; [C_maj --> ?x --> ?{:pitch :A}y]

; (defmacro jig [& body]
;   `(for [form# [~@body]]
;      ~form#))

; (jig
;  '[C_maj])

; TODO: Neighboring paths; account for fuzziness
; TODO: input piece of music, figure out the structure (i.e. progressions, modulations; most likely brute-force DFS/BFS)
