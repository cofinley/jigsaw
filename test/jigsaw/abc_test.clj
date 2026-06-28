(ns jigsaw.abc-test
  (:require
   [clojure.test :refer [deftest testing]]
   [jigsaw.core :as jigsaw]
   [jigsaw.impl.abc :as abc]
   [jigsaw.test-utils :refer [are+]]))

(deftest core-test
  (testing "with note->abc"
    (are+ [note want] (= want (abc/note->abc note))
      :C4 "C"
      :C#4 "^C"
      :C##4 "^^C"
      :Db4 "_D"
      :Dbb4 "__D"
      :C5 "c"
      :C6 "c'"
      :C7 "c''"
      :C8 "c'''"
      :C3 "C,"
      :C2 "C,,"
      :C1 "C,,,"
      :C0 "C,,,,"))
  (testing "shape->abc"
    (are+ [shape-ref want] (= want (abc/shape->abc (jigsaw/->shape shape-ref)))
      :C4_maj "X:1
K:C exp C D E F G A B
L:1/4
\"Cmaj\" [C E G]"
      :C#4_m "X:1
K:C exp C D E F G A B
L:1/4
\"C#m\" [^C E ^G]"
      :C##6_sus4 "X:1
K:C exp C D E F G A B
L:1/4
\"C##sus4\" [^^c' ^^f' ^^g']"
      :C0_dim "X:1
K:C exp C D E F G A B
L:1/4
\"Cdim\" [C,,,, _E,,,, _G,,,,]")))
