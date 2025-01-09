(ns jigsaw.components.output-music-staff-node
  (:require
   [clojure.string :as s]
   [reagent.core :as r]
   [re-frame.core :as re-frame]
   [jigsaw.algo :as algo]
   [jigsaw.subs :as subs]
   [jigsaw.components.node :refer [node]]
   ["abcjs" :as abcjs]))

(defn note->abc [n]
  (let [{:keys [letter octave accidental]} (algo/parts n)
        abc-accidental (case accidental
                         "bb" "__"
                         "b" "_"
                         "#" "^"
                         "##" "^^"
                         "")
        lowercase? (< 4 octave)
        commas (if lowercase? 0 (- 4 octave))
        apostrophes (if lowercase? (- octave 5) 0)]
    (str
     abc-accidental
     ((if lowercase? s/lower-case str) letter)
     (s/join (take commas (repeat ",")))
     (s/join (take apostrophes (repeat "'"))))))

(defn score [incoming-node]
  (let [dom-id (str (random-uuid))]
    (r/create-class
     {:display-name "score"
      :component-did-mount
      (fn [_]
        (let [notes (set (get-in incoming-node [:data :notes]))
              chord? (= :input-chord (:type incoming-node))
              scale? (= :input-scale (:type incoming-node))
              pitch (get-in incoming-node [:data :pitch])
              shape-name (get-in incoming-node [:data :name])
              key (if scale? (str (name pitch) (name shape-name)) "C")
              sorted-notes (sort-by algo/note->midi notes)
              pitches-str (s/join " " (map note->abc sorted-notes))
              syntax (s/join "\n"
                             ["X:1"
                              (str "K:" key)
                              "L:1/4"
                              (s/join " "
                                      [(when chord?
                                         (str "\"" (str (name pitch) (name shape-name)) "\""))
                                       (if chord?
                                         (str "[" pitches-str "]")
                                         pitches-str)])])]
          (.renderAbc abcjs dom-id syntax #js {:jazzchords true :lineThickness 0.1 :staffwidth (if chord? 100 (* 50 (count notes)))})))
      :reagent-render
      (fn []
        [:div {:id dom-id}])})))

(defn output-music-staff-node [{:keys [id]}]
  (let [incoming-nodes (re-frame/subscribe [::subs/incoming id])]
    [node {:title "Staff" :handle {:type "target" :position "left"}}
     (if-let [incoming-node (first @incoming-nodes)]
       (let [notes (get-in incoming-node [:data :notes])]
         (if (seq notes)
           [score incoming-node]
           [:p "No input"]))
       [:p "No input"])]))

