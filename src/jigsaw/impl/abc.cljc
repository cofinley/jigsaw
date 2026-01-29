(ns jigsaw.impl.abc
  (:require
   [clojure.string :as str]
   [jigsaw.impl.theory :as theory]))

(defn abc-pitch->note
  "
  Convert abc notation pitch to :jigsaw.note format
  \"C\" :C4
  \"^C\" :C#4
  \"^^C\" :C##4
  \"_D\" :Db4
  \"__D\" :Dbb4
  \"c\" :C5
  \"c'\" :C6
  \"c''\" :C7
  \"c'''\" :C8
  \"C,\" :C3
  \"C,,\" :C2
  \"C,,,\" :C1
  \"C,,,,\" :C0
  "
  [abc-pitch]
  (when abc-pitch
    (let [pitch-str (str abc-pitch)
          ;; Parse accidentals (^ for sharp, _ for flat)
          accidental-count (count (take-while #(or (= % \^) (= % \_)) pitch-str))
          accidental-char (when (pos? accidental-count) (first pitch-str))
          accidental-str (case [accidental-char accidental-count]
                           [\^ 1] "#"
                           [\^ 2] "##"
                           [\_ 1] "b"
                           [\_ 2] "bb"
                           "")
          ;; Get the base note letter (after accidentals)
          note-char (nth pitch-str accidental-count)
          base-letter (str/upper-case (str note-char))
          ;; Determine base octave (uppercase = 4, lowercase = 5)
          base-octave (if (= (str/upper-case note-char) note-char) 4 5)
          ;; Parse octave modifiers (' raises, , lowers)
          modifier-part (subs pitch-str (inc accidental-count))
          octave-offset (- (count (filter #(= % \') modifier-part))
                           (count (filter #(= % \,) modifier-part)))
          final-octave (+ base-octave octave-offset)
          ;; Construct the note keyword
          note-name (str base-letter accidental-str final-octave)]
      (keyword note-name))))

(defn key-signature->abc
  "
  B♭ - on the middle line (3rd line)
  E♭ - in the 4th space
  A♭ - in the 2nd space
  D♭ - on the 2nd line
  G♭ - on the 4th line
  C♭ - in the 3rd space
  F♭ - on the 1st line

  For sharps in treble clef:

  F♯ - on the 5th line
  C♯ - in the 3rd space
  G♯ - on the 4th line
  D♯ - in the 2nd space
  A♯ - on the 2nd line
  E♯ - in the 4th space
  B♯ - on the 3rd line
  "
  [key-ref]
  (let [accidental-pitches (theory/key-signature-accidentals key-ref)
        pitch->abc #(case %
                      ;; Flats
                      :Bb "_B"
                      :Eb "_e"
                      :Ab "_A"
                      :Db "_d"
                      :Gb "_G"
                      :Cb "_c"
                      :Fb "_F"

                      ;; Sharps
                      :F# "^f"
                      :C# "^c"
                      :G# "^g"
                      :D# "^d"
                      :A# "^a"
                      :E# "^e"
                      :B# "^B")]
    (str/join " " (map pitch->abc accidental-pitches))))

(comment (key-signature->abc {:pitch :C :name :minor}))

(defn pitch->abc [p]
  (let [{:keys [letter accidental]} (theory/parts p)
        abc-accidental (case accidental
                         "bb" "__"
                         "b" "_"
                         "#" "^"
                         "##" "^^"
                         "")]
    (str abc-accidental letter)))

(defn note->abc [n]
  (let [{:keys [letter octave accidental]} (theory/parts n)
        abc-accidental (case accidental
                         "bb" "__"
                         "b" "_"
                         "#" "^"
                         "##" "^^"
                         "")
        lowercase? (< 4 octave)
        commas (if lowercase? 0 (- 4 octave))
        apostrophes (if lowercase? (- octave 5) 0)]
    (str
     abc-accidental
     ((if lowercase? str/lower-case str) letter)
     (str/join (take commas (repeat ",")))
     (str/join (take apostrophes (repeat "'"))))))

(defn shape->abc
  [shape & {:keys [note-length selected-key]
            :or {note-length "1/4"}}]
  {:pre [(theory/shape? shape)]}
  (let [notes (set (:notes shape))
        scale? (theory/scale? shape)
        pitch (:pitch shape)
        shape-name (:name shape)
        key-ref (cond
                  (some? selected-key) selected-key
                  :else {:pitch :C :name :major})
        key-shape (theory/->shape (assoc key-ref :note (theory/pitch->note (:pitch key-ref))))
        key-abc (str (name (:pitch key-shape))
                     " exp "
                     (str/join " " (map note->abc (:notes key-shape))))
        sorted-notes (sort-by theory/note->midi notes)
        pitches-str (str/join " " (map note->abc sorted-notes))]
    (str/join "\n"
              ["X:1"
               (str "K:" key-abc)
               (str "L:" note-length)
               (str/join " "
                         [(when-not scale?
                            (str "\"" (name pitch) (name shape-name) "\""))
                          (if scale?
                            pitches-str
                            (str "[" pitches-str "]"))])])))

