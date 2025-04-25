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
        {:pitch :C :name :maj} true
        {:pitch :C :name :major} true
        {:note :C4 :name :major} true
        {:name :major} false
        {:pitch :C} false
        {:pitch :C :name :maj} true))
    (testing "with shape-blueprint"
      (are+ [m valid] (= valid (s/valid? ::specs/shape-blueprint m))
        {:name :maj :intervals [:P1 :M3 :P5]} true
        {:name :maj} false))
    (testing "with shape"
      (are+ [m valid] (= valid (s/valid? ::specs/shape m))
        {:pitch :C :name :maj :intervals [:P1 :M3 :P5] :pitches [:C :E :G]} true
        {:note :C4 :name :maj :intervals [:P1 :M3 :P5] :pitches [:C :E :G] :notes [:C4 :E4 :G4]} true
        {:name :maj} false))
    (testing "with chord"
      (are+ [m valid] (= valid (s/valid? ::specs/chord m))
        {:pitch :C :name :maj :intervals [:P1 :M3 :P5] :pitches [:C :E :G]} true
        {:pitch :C :name :major :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7] :pitches [:C :D :E :F :G :A :B]} false))
    (testing "with scale"
      (are+ [m valid] (= valid (s/valid? ::specs/scale m))
        {:pitch :C :name :maj :intervals [:P1 :M3 :P5] :pitches [:C :E :G]} false
        {:pitch :C :name :major :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7] :pitches [:C :D :E :F :G :A :B]} true))
    (testing "with context"
      (are+ [m valid] (= valid (s/valid? ::specs/context m))
        ; Single context link; i.e. current shape (not shown) came from this
        {:pitch :C
         :name :maj
         :intervals [:P1 :M3 :P5]
         :pitches [:C :E :G]
         ; Original shape not shown, degree of 1 is random here
         :degree :I} true
        ; Context chain, two links; i.e. current shape came from this which came from another shape
        {:pitch :C
         :name :maj
         :intervals [:P1 :M3 :P5]
         :pitches [:C :E :G]
         :degree :I  ; Cmaj = first degree of the C major scale
         :context {:pitch :C
                   :name :major
                   :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                   :pitches [:C :D :E :F :G :A :B]
                   :degrees [:1 :2 :3 :4 :5 :6 :7]
                   ; Original shape not shown, degree of 2 is random here
                   :degree :ii}} true
        ; Context chain, three links; i.e. current shape came from this which came from another shape
        {:pitch :C
         :name :maj
         :intervals [:P1 :M3 :P5]
         :pitches [:C :E :G]
         :degree :I  ; Cmaj is the first degree of the C major scale
         :context {:pitch :C
                   :name :major
                   :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                   :pitches [:C :D :E :F :G :A :B]
                   :degrees [:1 :2 :3 :4 :5 :6 :7]
                   :degree :ii  ; Dm is the second degree of the C major scale (:degree is always the chord's degree, even if the current context is a scale)
                   :context {:pitch :D
                             :name :m
                             :intervals [:P1 :m3 :P5]
                             :pitches [:D :F :A]
                             ; Original shape not shown, degree of 3 is random here
                             :degree :iii}}} true))
    (testing "with scale-chord"
      (are+ [m valid] (= valid (s/valid? ::specs/scale-chord m))
        ; Single context; no scale origin
        {:pitch :C
         :name :maj
         :intervals [:P1 :M3 :P5]
         :pitches [:C :E :G]
         ; Original shape not shown, degree of 1 is random here
         :degree :I} false
        ; Context chain; chord with scale origin
        {:pitch :C
         :name :maj
         :intervals [:P1 :M3 :P5]
         :pitches [:C :E :G]
         :degree :I
         :context {:pitch :C
                   :name :major
                   :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
                   :pitches [:C :D :E :F :G :A :B]
                   :degrees [:1 :2 :3 :4 :5 :6 :7]
                   ; Original shape not shown, degree of 2 is random here
                   :degree :ii}} true))
    (testing "with chord-scale"
      (are+ [m valid] (= valid (s/valid? ::specs/chord-scale m))
        ; Single context; no scale origin
        {:pitch :C
         :name :major
         :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
         :pitches [:C :D :E :F :G :A :B]
         :degrees [:1 :2 :3 :4 :5 :6 :7]
         ; Random degree
         :degree :I} false
        ; Context chain; scale with chord origin
        {:pitch :C
         :name :major
         :intervals [:P1 :M2 :M3 :P4 :P5 :M6 :M7]
         :pitches [:C :D :E :F :G :A :B]
         :degrees [:1 :2 :3 :4 :5 :6 :7]
         :degree :I
         :context {:pitch :C
                   :name :maj
                   :intervals [:P1 :M3 :P5]
                   :pitches [:C :E :G]
                   :degree :ii}} true))))
