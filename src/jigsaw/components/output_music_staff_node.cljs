(ns jigsaw.components.output-music-staff-node
  (:require
   [clojure.string :as s]
   [reagent.core :as r]
   [jigsaw.algo :as algo]
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

(def note-length "1/4")

(defn shape->abc [data]
  (let [notes (set (:notes data))
        scale? (contains? data :degrees)
        pitch (:pitch data)
        shape-name (:name data)
        key (if scale? (str (name pitch) (name shape-name)) "C")
        sorted-notes (sort-by algo/note->midi notes)
        pitches-str (s/join " " (map note->abc sorted-notes))]
    (s/join "\n"
            ["X:1"
             (str "K:" key)
             (str "L:" note-length)
             (s/join " "
                     [(when-not scale?
                        (str "\"" (str (name pitch) (name shape-name)) "\""))
                      (if scale?
                        pitches-str
                        (str "[" pitches-str "]"))])])))

(defn score [data]
  (let [dom-id (str (random-uuid))]
    (r/create-class
     {:display-name "score"
      :component-did-mount
      (fn [_]
        (let [notes (set (:notes data))
              syntax (shape->abc data)
              scale? (contains? data :degrees)]
          (.renderAbc abcjs dom-id syntax #js {:jazzchords true
                                               :lineThickness 0.1
                                               :staffwidth (if scale? (* 50 (count notes)) 100)})))
      :should-component-update (fn [_ prev next]
                                 (let [prev-notes (:notes (second prev))
                                       next-notes (:notes (second next))]
                                   (not= prev-notes next-notes)))
      :component-did-update
      (fn [this _ _ _]
        (let [new-data (second (r/argv this))
              notes (set (:notes new-data))
              syntax (shape->abc new-data)
              scale? (contains? new-data :degrees)]
          (.renderAbc abcjs dom-id syntax #js {:jazzchords true
                                               :lineThickness 0.1
                                               :staffwidth (if scale? (* 50 (count notes)) 100)})))
      :reagent-render
      (fn []
        [:div {:id dom-id
               :class "flex justify-center"}])})))

(defn output-music-staff-view [props]
  (let [data (:data props)
        notes (:notes data)]
    (if (and (every? data [:notes :pitch :name]) (seq notes))
      [score data]
      ;; TODO: allow just notes, maybe key/pitch override?
      [:p "Insufficient input; needs notes, pitch, and name"])))
