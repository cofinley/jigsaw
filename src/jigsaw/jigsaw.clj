;; ---------------------------------------------------------
;; jigsaw.jigsaw
;;
;; Jigsaw: determining a musical idea's potential harmonic functions by putting the multidimensional music theory jigsaw puzzle pieces together
;; ---------------------------------------------------------

(ns jigsaw.jigsaw
  (:gen-class)
  (:require
   [com.brunobonacci.mulog :as mulog]))

(defn greet [] "Hello, World")

(defn -main
  "Entry point into the application via clojure.main -M"
  [& args]
  (mulog/set-global-context!
   {:app-name "jigsaw" :version  "0.1.0-SNAPSHOT"})
  (mulog/log ::application-starup :arguments args)
  (greet))

;; Views
;; - multidimensional
;; - keyboard
;; - staff
;; - circle of fifths
;; - chromatic
