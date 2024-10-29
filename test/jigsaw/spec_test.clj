(ns jigsaw.spec-test
  (:require
   [clojure.test :refer [deftest testing are]]
   [clojure.spec.alpha :as s]
   [jigsaw.spec :as specs]))

(deftest spec-test
  (testing "Specs"
    (testing "with semitone"
      (are [value valid] (= valid (s/valid? ::specs/semitone value))
        -1 false
        0 true
        24 true
        :C  false))
    (testing "with pitch"
      (are [value valid] (= valid (s/valid? ::specs/pitch value))
        1 false
        "a" false
        :C true
        :c false
        :C# true))
    (testing "with interval"
      (are [value valid] (= valid (s/valid? ::specs/interval value))
        :1 true
        :13 false
        :M13 true))))

