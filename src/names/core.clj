(ns names.core
  (:use clojure.test))

(def vocals ["a" "e" "i" "o" "u" "ä" "ö"])
(def consonants ["b" "c" "d" "f" "g" "h" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x" "z"])

(defn ensure-seq [value]
  (cond (vector? value) value
        (seq? value) value
        (list? value) value
        :default [value]))

(deftest ensure-seq-test
  (is (= (ensure-seq "a")
         ["a"]))
  (is (= (ensure-seq ["a"])
         ["a"])))

(defn names
  ([start letter-specifications]
     (if (seq letter-specifications)
       (for [letter (ensure-seq (first letter-specifications))]
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
         '("aa" "ae" "ai" "ao" "au" "aä" "aö" "ea" "ee" "ei"
           "eo" "eu" "eä" "eö" "ia" "ie" "ii" "io" "iu" "iä"
           "iö" "oa" "oe" "oi" "oo" "ou" "oä" "oö" "ua" "ue"
           "ui" "uo" "uu" "uä" "uö" "äa" "äe" "äi" "äo" "äu"
           "ää" "äö" "öa" "öe" "öi" "öo" "öu" "öä" "öö"))))

(defn spit-names [spec]
  (spit "names.txt" (vec (names.core/names spec))))

(run-tests)

(comment
  (spit-names ["e" consonants vocals vocals consonants])
  (spit-names ["aa" consonants vocals])

  (let [v (remove #{"ä" "ö"} vocals)
        c (remove #{"b" "c" "f" "q" "w" "x" "z"} consonants)]
    (spit-names ["ak" c v])))