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
        21 true
        22 false
        :C  false))
    (testing "with pitch"
      (are [value valid] (= valid (s/valid? ::specs/pitch value))
        1 false
        "a" false
        :C true
        :c false
        :C# true
        :C## true
        :Db true
        :Dbb true))
    (testing "with interval"
      (are [value valid] (= valid (s/valid? ::specs/interval value))
        :P1 true
        :13 false
        :M13 true))
    (testing "with note"
      (are [value valid] (= valid (s/valid? ::specs/note value))
        :C false
        :C2 true
        :c2 false
        :C#2 true
        :C##2 true
        :Db2 true
        :D11 false
        :T2 false))))
