(ns jigsaw.components.output-circle-of-fifths-node
  (:require
   [jigsaw.spec :as specs]))

(def circle-of-fifths-order
  [:C :G :D :A :E :B :F# :C# :G# :D# :A# :F])

(defn enharmonic-eq? [pitch1 pitch2]
  (= (specs/pitches pitch1) (specs/pitches pitch2)))

(defn pitch-matches-circle-position? [pitch circle-pitch]
  (enharmonic-eq? pitch circle-pitch))

(defn get-display-label-for-position [circle-pitch highlighted-pitches starting-pitch]
  (let [all-pitches (conj highlighted-pitches starting-pitch)
        matching-pitch (first (filter #(and % (enharmonic-eq? % circle-pitch)) all-pitches))]
    (if matching-pitch
      (name matching-pitch)
      (name circle-pitch))))

(defn draw-circle-of-fifths [highlighted-pitches starting-pitch]
  (let [center-x 150
        center-y 150
        radius 100
        inner-radius 60
        angle-step (/ (* 2 js/Math.PI) 12)]
    [:svg {:width 300 :height 300 :class "mx-auto"}

     ;; Section highlighting
     (for [i (range 12)
           :let [pitch (nth circle-of-fifths-order i)
                 angle (- (* i angle-step) (/ js/Math.PI 2))
                 highlighted? (some #(pitch-matches-circle-position? % pitch) highlighted-pitches)
                 is-starting-pitch? (and starting-pitch (pitch-matches-circle-position? starting-pitch pitch))]
           :when highlighted?]
       (let [start-angle (- angle (/ angle-step 2))
             end-angle (+ angle (/ angle-step 2))
             large-arc-flag (if (> (js/Math.abs (- end-angle start-angle)) js/Math.PI) 1 0)
             start-x (+ center-x (* radius (js/Math.cos start-angle)))
             start-y (+ center-y (* radius (js/Math.sin start-angle)))
             end-x (+ center-x (* radius (js/Math.cos end-angle)))
             end-y (+ center-y (* radius (js/Math.sin end-angle)))
             inner-start-x (+ center-x (* inner-radius (js/Math.cos start-angle)))
             inner-start-y (+ center-y (* inner-radius (js/Math.sin start-angle)))
             inner-end-x (+ center-x (* inner-radius (js/Math.cos end-angle)))
             inner-end-y (+ center-y (* inner-radius (js/Math.sin end-angle)))
             path-d (str "M " start-x " " start-y
                         " A " radius " " radius " 0 " large-arc-flag " 1 " end-x " " end-y
                         " L " inner-end-x " " inner-end-y
                         " A " inner-radius " " inner-radius " 0 " large-arc-flag " 0 " inner-start-x " " inner-start-y
                         " Z")]
         [:path {:key (str "highlight-" i)
                 :d path-d
                 :class (if is-starting-pitch? "fill-green-500" "fill-indigo-500")
                 :opacity (if is-starting-pitch? 0.8 0.6)}]))

     ;; Outer circle
     [:circle {:cx center-x :cy center-y :r radius
               :fill "none" :stroke "#ccc" :stroke-width 2}]

     ;; Inner circle
     [:circle {:cx center-x :cy center-y :r inner-radius
               :fill "none" :stroke "#ccc" :stroke-width 2}]

     ;; Section divider lines
     (for [i (range 12)
           :let [line-angle (- (* (+ i 0.5) angle-step) (/ js/Math.PI 2))
                 line-x1 (* inner-radius (js/Math.cos line-angle))
                 line-y1 (* inner-radius (js/Math.sin line-angle))
                 line-x2 (* radius (js/Math.cos line-angle))
                 line-y2 (* radius (js/Math.sin line-angle))]]
       [:line {:key (str "line-" i)
               :x1 (+ center-x line-x1) :y1 (+ center-y line-y1)
               :x2 (+ center-x line-x2) :y2 (+ center-y line-y2)
               :stroke "#fff" :stroke-width 2}])

     ;; Pitch labels
     (for [i (range 12)
           :let [pitch (nth circle-of-fifths-order i)
                 angle (- (* i angle-step) (/ js/Math.PI 2))
                 text-x (* (+ radius 20) (js/Math.cos angle))
                 text-y (* (+ radius 20) (js/Math.sin angle))
                 highlighted? (some #(pitch-matches-circle-position? % pitch) highlighted-pitches)
                 is-starting-pitch? (and starting-pitch (pitch-matches-circle-position? starting-pitch pitch))
                 display-label (get-display-label-for-position pitch highlighted-pitches starting-pitch)]]
       [:text {:key (str "label-" i)
               :x (+ center-x text-x) :y (+ center-y text-y)
               :text-anchor "middle" :dominant-baseline "middle"
               :class (str "text-lg font-semibold "
                           (if highlighted?
                             (if is-starting-pitch? "fill-green-400" "fill-indigo-400")
                             "fill-gray-500"))}
        display-label])]))

(defn output-circle-of-fifths-view [props]
  (let [data (:data props)
        notes (:notes data)
        pitches (:pitches data)
        starting-pitch (:pitch data)]
    (if (seq notes)
      (let [pitch-set (set pitches)]
        [draw-circle-of-fifths pitch-set starting-pitch])
      [:p {:class "text-lg"} "No notes to display"])))
