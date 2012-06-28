(ns names.core
  (:use clojure.test))

(def vocals ["a" "e" "i" "o" "u"
             ;;"ä" "ö"
             ])

(def consonants [;;"b""c"
                 "d"
                 ;;"f"
                 "g" "h" "k" "l" "m" "n" "p"
                 ;;"q"
                 "r" "s" "t" "v"
                 ;;"w" "x" "z"
                 ])

(defn ensure-vector [value]
  (if (vector? value) value
      [value]))

(deftest ensure-vector-test
  (is (= (ensure-vector "a")
         ["a"]))
  (is (= (ensure-vector ["a"])
         ["a"])))

(defn names
  ([start letter-specifications]
     (if (seq letter-specifications)
       (for [letter (ensure-vector (first letter-specifications))]
         (names (str start letter) (rest letter-specifications)))
       start))
  ([specification]
     (flatten (names "" specification))))


(deftest one-letter
  (is (= (names [vocals])
         vocals))

  (is (= (names [consonants])
         consonants))

  (is (= (names ["a"])
         ["a"] ))

  (is (= (names [vocals vocals])
         '("aa" "ae" "ai" "ao" "au" "ea" "ee" "ei" "eo" "eu" "ia" "ie" "ii" "io" "iu" "oa" "oe" "oi" "oo" "ou" "ua" "ue" "ui" "uo" "uu"))))

(defn spit-names [spec]
  (spit "names.txt" (vec (names.core/names spec))))

(run-tests)

(comment
  (spit-names ["e" consonants vocals vocals consonants])
(spit-names ["aa" consonants vocals])
  )