(ns jigsaw.algo-test
  (:require
   [clojure.test :refer [deftest testing is are]]
   [jigsaw.algo :as algo]
   [jigsaw.spec :as specs]
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

(deftest algo-test
  (testing "Algo"
    (testing "parts"
      (testing "starting from a pitch"
        (are+ [p m] (= m (algo/parts p))
          :C   {:pitch :C   :letter \C :accidental ""}
          :C#  {:pitch :C#  :letter \C :accidental "#"}
          :C## {:pitch :C## :letter \C :accidental "##"}
          :Dbb {:pitch :Dbb :letter \D :accidental "bb"}))
      (testing "starting from a note"
        (are+ [n m] (= m (algo/parts n))
          :C4   {:pitch :C   :letter \C :accidental ""   :octave 4}
          :C#4  {:pitch :C#  :letter \C :accidental "#"  :octave 4}
          :C##4 {:pitch :C## :letter \C :accidental "##" :octave 4}
          :Dbb4 {:pitch :Dbb :letter \D :accidental "bb" :octave 4})))
    (testing "with flat?"
      (are+ [p want] (= want (algo/flat? p))
        :C  false
        :C# false
        :Db true
        :Ab true))
    (testing "with natural?"
      (are+ [p want] (= want (algo/natural? p))
        :C  true
        :C# false
        :Db false
        :A  true))
    (testing "with sharp?"
      (are+ [p want] (= want (algo/sharp? p))
        :C  false
        :C# true
        :Db false
        :Ab false))
    (testing "with enharmonic"
      (are+ [p notation want] (= want (algo/enharmonic p notation))
        :C :flat   :C
        :C :sharp  :B#
        :C# :sharp :C#
        :C# :flat  :Db))
    (testing "with pitches->interval"
      (are+ [p1 p2 want] (= want (algo/pitches->interval p1 p2))
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
        :C :B  :M7))
    (testing "with note->midi"
      (are+ [note want] (= want (algo/note->midi note))
        :C4  60
        :C#4 61
        :Db4 61
        :C0  12))
    (testing "with midi->note"
      (are+ [midi want] (= want (algo/midi->note midi))
        60 :C4
        61 :C#4
        62 :D4
        63 :Eb4
        12 :C0))
    (testing "with +interval"
      (testing "starting from a pitch"
        (testing "adding"
          (are+ [p interval want] (= want (algo/+interval p interval))
            :C :d2  :Dbb
            :C :m2  :Db
            :C :M2  :D
            :C :d3  :Ebb
            :C :m3  :Eb
            :C :A2  :D#
            :C :M3  :E
            :C :d4  :Fb
            :C :P4  :F
            :C :A3  :E#
            :C :d5  :Gb
            :C :A4  :F#
            :C :P5  :G
            :C :d6  :Abb
            :C :m6  :Ab
            :C :A5  :G#
            :C :M6  :A
            :C :d7  :Bbb
            :C :m7  :Bb
            :C :A6  :A#
            :C :M7  :B
            :C :P8  :C
            :C :P11 :F
            :C :M13 :A

            :C# :P1 :C#
            :C# :d2 :Db
            :C# :m2 :D
            :C# :M2 :D#
            :C# :d3 :Eb
            :C# :m3 :E
            :C# :A2 :D##
            :C# :M3 :E#
            :C# :d4 :F
            :C# :P4 :F#
            :C# :A3 :E##
            :C# :d5 :G
            :C# :A4 :F##
            :C# :P5 :G#
            :C# :d6 :Ab
            :C# :m6 :A
            :C# :A5 :G##
            :C# :M6 :A#
            :C# :d7 :Bb
            :C# :m7 :B
            :C# :A6 :A##
            :C# :M7 :B#

            :F :d4 :Bbb))
        (testing "subtracting"
          (are+ [p interval want] (= want (algo/+interval p interval -1))
            :C :d2  :B#
            :C :m2  :B
            :C :M2  :Bb
            :C :d3  :A#
            :C :m3  :A
            :C :A2  :Bbb
            :C :M3  :Ab
            :C :d4  :G#
            :C :P4  :G
            :C :A3  :Abb
            :C :d5  :F#
            :C :A4  :Gb
            :C :P5  :F
            :C :d6  :E#
            :C :m6  :E
            :C :A5  :Fb
            :C :M6  :Eb
            :C :d7  :D#
            :C :m7  :D
            :C :A6  :Ebb
            :C :M7  :Db
            :C :P8  :C
            :C :P11 :G
            :C :M13 :Eb

            :C# :P1 :C#
            :C# :d2 :B##
            :C# :m2 :B#
            :C# :M2 :B
            :C# :d3 :A##
            :C# :m3 :A#
            :C# :A2 :Bb
            :C# :M3 :A
            :C# :d4 :G##
            :C# :P4 :G#
            :C# :A3 :Ab
            :C# :d5 :F##
            :C# :A4 :G
            :C# :P5 :F#
            :C# :d6 :E##
            :C# :m6 :E#
            :C# :A5 :F
            :C# :M6 :E
            :C# :d7 :D##
            :C# :m7 :D#
            :C# :A6 :Eb
            :C# :M7 :D
            :C# :P8 :C#)))
      (testing "starting from a note"
        (testing "adding"
          (are+ [n interval want] (= want (algo/+interval n interval))
            :C4 :P1  :C4
            :C4 :P8  :C5
            :C4 :P11 :F5
            :F#4 :A5 :C##5))
        (testing "subtracting"
          (are+ [n interval want] (= want (algo/+interval n interval -1))
            :C4 :P1  :C4
            :C4 :P8  :C3
            :C4 :P11 :G3))))
    (testing "with +intervals"
      (testing "from pitch"
        (are+ [p intervals want] (= want (algo/+intervals p intervals))
          :C  [:P1]         [:C]
          :C  [:P1 :M3 :P5] [:C :E :G]
          :C  [:P1 :m3 :P5] [:C :Eb :G]
          :C# [:P1 :m3 :P5] [:C# :E :G#]
          :C# [:A3 :d5]     [:E## :G]))
      (testing "from note"
        (are+ [n intervals want] (= want (algo/+intervals n intervals))
          :C4  [:P1]                        [:C4]
          :C4  [:P8]                        [:C5]
          :C4  [:P1 :M3 :P5]                [:C4 :E4 :G4]
          :C4  [:P1 :m3 :P5]                [:C4 :Eb4 :G4]
          :C4  [:P1 :M3 :P5 :M7 :P11 :M13]  [:C4 :E4 :G4 :B4 :F5 :A5]
          :C#4 [:P1 :m3 :P5]                [:C#4 :E4 :G#4]
          :C#4 [:A3 :d5]                    [:E##4 :G4])))
    (testing "with clamp-pitch"
      (are+ [p want] (= want (algo/clamp-pitch p))
        :C      :C
        :C#     :C#
        :C##    :C##
        :C###   :D#
        :C####  :D##
        :C##### :E#
        :Db     :Db
        :Dbb    :Dbb
        :Dbbb   :Cb
        :Dbbbb  :Cbb
        :Dbbbbb :Bb))
    (testing "with resolve-chord"
      (testing "starting from a pitch"
        (are+ [pitch chord-name want] (= want (algo/resolve-chord pitch chord-name))
          :C  :maj #::specs{:name :maj :root :C  :interval-mapping {:P1 :C,  :M3 :E   :P5 :G}}
          :C# :m   #::specs{:name :m   :root :C# :interval-mapping {:P1 :C#, :m3 :E,  :P5 :G#}}
          :F# :aug #::specs{:name :aug :root :F# :interval-mapping {:P1 :F#, :M3 :A#, :A5 :C##}}))
      (testing "starting from a note"
        (are+ [note chord-name want] (= want (algo/resolve-chord note chord-name))
          :C3  :maj #::specs{:name :maj :root :C  :interval-mapping {:P1 :C3,  :M3 :E3,  :P5 :G3}}
          :C#4 :m   #::specs{:name :m   :root :C# :interval-mapping {:P1 :C#4, :m3 :E4,  :P5 :G#4}}
          :F#5 :aug #::specs{:name :aug :root :F# :interval-mapping {:P1 :F#5, :M3 :A#5, :A5 :C##6}})))))
