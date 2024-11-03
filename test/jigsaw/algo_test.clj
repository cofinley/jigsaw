(ns jigsaw.algo-test
  (:require
   [clojure.test :refer [deftest testing are]]
   [jigsaw.algo :as algo]))

(deftest algo-test
  (testing "Algo"
    (testing "with flat?"
      (are [p want] (= want (algo/flat? p))
        :C false
        :C# false
        :Db true
        :Ab true))
    (testing "with natural?"
      (are [p want] (= want (algo/natural? p))
        :C true
        :C# false
        :Db false
        :A true))
    (testing "with sharp?"
      (are [p want] (= want (algo/sharp? p))
        :C false
        :C# true
        :Db false
        :Ab false))
    (testing "with enharmonic"
      (are [p notation want] (= want (algo/enharmonic p notation))
        :C :flat :C
        :C :sharp :B#
        :C# :sharp :C#
        :C# :flat :Db))
    (testing "with pitches->interval"
      (are [p1 p2 want] (= want (algo/pitches->interval p1 p2))
        :C :C  :P1
        :D :D  :P1
        :C :C# :m2
        :C :Db :m2
        :C :D  :M2
        :C :D# :A2
        :C :Eb :m3
        :C :E  :M3
        :C :F  :P4
        :C :F# :A4
        :C :Gb :d5
        :C :G  :P5
        :C :G# :A5
        :C :Ab :m6
        :C :A  :M6
        :C :A# :A6
        :C :Bb :m7
        :C :B  :M7))))
