(ns jigsaw.explorer
  (:require
   [clojure.core.protocols :refer [nav]]
   [jigsaw.spec :as specs]
   [jigsaw.algo :as algo]))

(declare nav-home)
(declare nav-pitch)
(declare nav-interval)
(declare nav-chord)

(defn get-pitches []
  (with-meta
    (keys (sort-by val (remove (fn [[k _]]
                                 (let [{:keys [accidental]} (algo/parts k)]
                                   (algo/in? ["bb" "##"] accidental)))
                               specs/pitches)))
    {`nav #'nav-pitch
     :portal.viewer/default :portal.viewer/inspector}))

(defn get-intervals [x]
  (with-meta
    (keys (sort-by (comp ::specs/semitone val) specs/intervals))
    {:x x
     `nav #'nav-interval
     :portal.viewer/default :portal.viewer/inspector}))

(defn get-chords [x]
  (with-meta
    (keys specs/chords)
    {:x x
     `nav #'nav-chord
     :portal.viewer/default :portal.viewer/table}))

(defn nav-pitch [pitches k p]
  {:pitch p
   :intervals (with-meta
                (reduce (fn [m interval]
                          (assoc m interval (algo/+interval p interval)))
                        {} (get-intervals p))
                {`nav #'nav-pitch
                 :portal.viewer/default :portal.viewer/table})
   :chords (with-meta
             (reduce (fn [m chord-name]
                       (assoc m chord-name (algo/resolve-chord p chord-name)))
                     {} (get-chords p))
             {:portal.viewer/default :portal.viewer/table})})

(defn nav-interval [intervals k interval]
  (let [origin (:x (meta intervals))
        new-pitch (algo/+interval origin interval)]
    (nav-pitch nil nil new-pitch)))

(defn nav-chord [chords k chord-name]
  chord-name)

(def get-home
  (with-meta
    #{:pitches}
    {`nav #'nav-home
     :portal.viewer/default :portal.viewer/inspector}))

(defn nav-home [coll k v]
  (case v
    :pitches (get-pitches)))

get-home
