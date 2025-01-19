(ns jigsaw.test-utils
  (:require
   [clojure.test :refer [is]]
   [clojure.template :as temp]))

(defmacro are+
  "are but with assertion message like with `is`"
  [argv expr & args]
  (if (or
       (and (empty? argv) (empty? args))
       (and (pos? (count argv))
            (pos? (count args))
            (zero? (mod (count args) (count argv)))))
    `(temp/do-template ~argv (is ~expr (str '~expr " => " ~expr)) ~@args)
    (throw (IllegalArgumentException. "The number of args doesn't match are's argv."))))
