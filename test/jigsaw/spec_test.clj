(ns jigsaw.spec-test
  (:require
   [clojure.test :refer [deftest testing are]]
   [clojure.spec.alpha :as s]
   [jigsaw.spec :as specs]
   [jigsaw.test-utils :refer [are+]]))

(deftest spec-test
  (testing "Specs"
    (testing "with semitones"
      (are [value valid] (= valid (s/valid? ::specs/semitones value))
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
    (testing "with chroma"
      (are [p1 p2] (= (::specs/pitches p1) (::specs/pitches p2))
        :C :C
        :C :Dbb
        :C :B#))
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
        :T2 false))
    (testing "with shape-ref"
      (are+ [m valid] (= valid (s/valid? ::specs/shape-ref m))
        {:pitch :C :type :chord :name :maj} true
        {:pitch :C :type :scale :name :major} true
        {:note :C4 :type :scale :name :major} true
        {:type :scale :name :major} false
        {:pitch :C :type :chord} false
        {:pitch :C :name :maj} false))
    (testing "with shape-blueprint"
      (are+ [m valid] (= valid (s/valid? ::specs/shape-blueprint m))
        {:name :maj :intervals [:P1 :M3 :P5]} true
        {:name :maj} false))
    (testing "with shape"
      (are+ [m valid] (= valid (s/valid? ::specs/shape m))
        {:pitch :C :type :chord :name :maj :intervals [:P1 :M3 :P5] :pitches [:C :E :G]} true
        {:note :C4 :type :chord :name :maj :intervals [:P1 :M3 :P5] :pitches [:C :E :G] :notes [:C4 :E4 :G4]} true
        {:name :maj} false))))
