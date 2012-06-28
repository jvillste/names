(ns names.core
  (:use clojure.test))

(def vocals ["a" "e" "i" "o" "u" "y" "ä" "ö" "å"])
(def consonants ["b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x" "z"])

(defn ensure-seq [value]
  (if (sequential? value)
    value
    [value]))

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

  (is (= (names ["ak" ["b" "z"] ["k" "r"]])
         '("akbk" "akbr" "akzk" "akzr"))))

(defn format-name-list [values-per-line values]
  (str (apply str (for [line-values (interpose "\n" (partition-all values-per-line values))]
                       (apply str (interpose " " line-values)))) ))

(deftest format-name-list-test
  (is (= (format-name-list 2 [1 2 3 4 5 6 7])
         "1 2\n3 4\n5 6\n7")))

(defn spit-names [spec]
  (spit "names.txt" (format-name-list 20 (vec (names.core/names spec)))))

(run-tests)

(comment
  (spit-names ["e" consonants vocals vocals consonants])
  (spit-names ["aa" consonants vocals])

(let [v (remove #{"ä" "ö"} vocals)
        c (remove #{"b" "c" "f" "q" "w" "x" "z"} consonants)]
    (spit-names [v v v v])))