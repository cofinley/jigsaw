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

(defn neighboro
  "Shapes from shape"
  [from to]
  (l/project [from]
             (l/membero to (->shapes from))))

(defn ?=
  "Fuzzy shape; get similar shapes to ?shape; can include ?shape"
  [?possible-shape ?shape-ref]
  (l/project [?shape-ref]
             (let [_shape (jigsaw/->shape ?shape-ref)
                   ; Ensure shape has notes
                   shape (if (contains? _shape :notes)
                           _shape
                           (jigsaw/->shape (theory/pitch->note (:pitch _shape)) (:name _shape)))]
               (noteso ?possible-shape (:notes shape)))))

;; Different projections; find different ways to think about same notes (PCIs)
(defn alto
  "
  Given a shape, find alternative ways of thinking about that shape (what else could it be?), based on PCIs.
  Like noteso or ?=, but with disequality on the current shape and its enarmonic equivalent
  "
  [?possible-shape ?shape-ref]
  (l/project [?shape-ref]
             (let [_shape (jigsaw/->shape ?shape-ref)
                   ; Ensure shape has notes
                   shape (if (contains? _shape :notes)
                           _shape
                           (jigsaw/->shape (theory/pitch->note (:pitch _shape)) (:name _shape)))
                   pcis (map theory/pitches (:pitches shape))]
               (with-fresh
                 (noteso ?possible-shape (:notes shape))
                 (l/featurec ?possible-shape {:pitch ?pitch :name ?name :pcis ?pcis})
                 (l/!= (:name shape) ?name)
                 ; Don't use same pitch or enharmonic equivalent
                 (l/firsto ?pcis ?first-pci)
                 (l/!= (first pcis) ?first-pci)))))

(defn shape-refo [?id ?shape]
  (l/is ?id ?shape #(theory/->shape-ref %)))

(defn shape==
  "Shape equality, based on pitch + name"
  [a b]
  (with-fresh
    (l/conde
     ; Quick check if maps
     [(l/== true (map? a))
      (l/== true (map? b))
      (l/featurec a {:pitch ?pitch :name ?name})
      (l/featurec b {:pitch ?pitch :name ?name})]
     ; Alternative check if not
     [(shapeo ?shape1 a)
      (shapeo ?shape2 b)
      (shape-refo ?ref1 ?shape1)
      (shape-refo ?ref2 ?shape2)
      (l/== ?ref1 ?ref2)])))

(defn shape!=
  "Shape disequality, based on pitch + name"
  [a b]
  (with-fresh
    (l/conde
     ; Quick check if maps
     [(l/== true (map? a))
      (l/== true (map? b))
      (l/featurec a {:pitch ?pitch1 :name ?name1})
      (l/featurec b {:pitch ?pitch2 :name ?name2})
      (l/conde
       [(l/!= ?pitch1 ?pitch2)]
       [(l/!= ?name1 ?name2)])]
     [(shapeo ?shape1 a)
      (shapeo ?shape2 b)
      (shape-refo ?ref1 ?shape1)
      (shape-refo ?ref2 ?shape2)
      (l/!= ?ref1 ?ref2)])))

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

(defn transposo
  "Transpose pitch/note/shape by interval"
  [q x interval & [multiplier]]
  (l/project [x]
             (l/== q (theory/transpose x interval multiplier))))

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
               (l/membero to (map theory/->shape-ref shapes)))))

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
            (shape-refo ?cid ?c)
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
   (l/conde
    ; Need scale
    [(l/lvaro ?s)
     (neighboro ?chord ?s)
     (l/featurec ?s {:context ?deg})
     (resolvo* ?s ?rest-degs ?rest-chords)]
    ; Need degrees
    [(l/lvaro ?deg)
     (l/fresh [?chord-neighbor]
              (neighboro ?s ?chord-neighbor)
              (shape== ?chord ?chord-neighbor)
              (l/featurec ?chord-neighbor {:context ?deg})
              (resolvo* ?s ?rest-degs ?rest-chords))]
    ; Need chords
    [(l/lvaro ?chord)
     (neighboro ?s ?chord)
     (l/featurec ?chord {:context ?deg})
     (resolvo* ?s ?rest-degs ?rest-chords)])))

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

; TODO: Neighboring paths; account for fuzziness
; TODO: input piece of music, figure out the structure (i.e. progressions, modulations; most likely brute-force DFS/BFS)
