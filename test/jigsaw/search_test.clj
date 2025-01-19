(ns jigsaw.search-test
  (:require
   [clojure.test :refer [deftest testing]]
   [jigsaw.test-utils :refer [are+]]
   [jigsaw.search :as search]))

(deftest search-test
  (testing "Search"
    (testing "heuristics"
      (are+ [set1 set2 m] (= m (search/calculate-heuristics set1 set2))
        #{} #{} {:contained-in? 1
                 :fully-contained-in? 0
                 :contains? 1
                 :fully-contains? 0
                 :overlap 0}
        #{:C} #{} {:contained-in? 0
                   :fully-contained-in? 0
                   :contains? 1
                   :fully-contains? 1
                   :overlap 0.0}
        #{} #{:C} {:contained-in? 1
                   :fully-contained-in? 1
                   :contains? 0
                   :fully-contains? 0
                   :overlap 0.0}
        #{:C} #{:C} {:contained-in? 1
                     :fully-contained-in? 0
                     :contains? 1
                     :fully-contains? 0
                     :overlap 1.0}
        #{:C} #{:C :D} {:contained-in? 1
                        :fully-contained-in? 1
                        :contains? 0
                        :fully-contains? 0
                        :overlap 0.5}
        #{:C :D} #{:C} {:contained-in? 0
                        :fully-contained-in? 0
                        :contains? 1
                        :fully-contains? 1
                        :overlap 0.5}
        #{:C :D :E} #{:C} {:contained-in? 0
                           :fully-contained-in? 0
                           :contains? 1
                           :fully-contains? 1
                           :overlap (float (/ 1 3))}))))
