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


(deftest names-test
  (is (= (names [vocals])
         vocals))

  (is (= (names [consonants])
         consonants))

  (is (= (names ["a"])
         ["a"] ))

  (is (= (names ["ak" ["b" "z"] ["k" "r"]])
         '("akbk" "akbr" "akzk" "akzr"))))

(defn format-name-list [values-per-line values]
  (apply str (for [line-values (interpose "\n" (partition-all values-per-line values))]
               (apply str (interpose " " line-values)))))

(deftest format-name-list-test
  (is (= (format-name-list 2 [1 2 3 4 5 6 7])
         "1 2\n3 4\n5 6\n7")))

(defn extract-pattern [name letter-groups]
  (map (fn [letter]
         (first (filter (fn [letter-group]
                          (some (fn [group-letter]
                                  (= (str letter) group-letter))
                                letter-group))
                        letter-groups)))
       name))

(deftest extract-pattern-test
  (is (= (extract-pattern "abc" [["a" "b"] ["c" "d"]])
         '(["a" "b"] ["a" "b"] ["c" "d"]))))

(defn extract-pattern-signature [name letter-groups]
  (map (fn [letter]
         (first (filter (fn [index]
                          (some (fn [group-letter]
                                  (= (str letter) group-letter))
                                (letter-groups index)))
                        (range 0 (count letter-groups)))))
       name))

(defn increase-histogram-value [histogram key]
  (if (contains? histogram key)
    (update-in histogram [key] inc)
    (update-in histogram [key] (fn [_] 1))))

(deftest increase-histogram-value-test
  (is (= (increase-histogram-value {} :a)
         {:a 1}))
  (is (= (increase-histogram-value {:a 4} :a)
         {:a 5})))

(defn pattern-signature-histogram [names letter-groups]
  (reduce (fn [histogram name]
            (increase-histogram-value histogram
                                      (extract-pattern-signature name
                                                                 letter-groups)))
          {}
          names))


(deftest pattern-signature-histogram-test
  (is (= (pattern-signature-histogram ["aa" "ab"] [["a"] ["b"]])
         '{(0 1) 1, (0 0) 1})))

(defn format-pattern-signature-histogram [histogram]
  (for [key (reverse (sort-by histogram (keys histogram)))]
    (str (vec key) " : " (histogram key))))

(deftest format-pattern-signature-histogram-test
  (is (= (format-pattern-signature-histogram (pattern-signature-histogram ["aa" "ab"] [["a"] ["b"]]))
         '("[0 0] : 1" "[0 1] : 1"))))



(defn spit-names [spec]
  (spit "names.txt" (format-name-list (int (/ 100 (count spec))) (names spec))))

(run-tests)

(comment
  (spit-names ["e" consonants vocals vocals consonants])
  (spit-names ["aa" consonants vocals])

  (let [v (remove #{"ä" "ö" "å"} vocals)
        c (remove #{"b" "c" "f" "q" "w" "x" "z"} consonants)]
    (spit-names ["kl" v c v c v]))

  (let [front-vocals '("i" "o" "u" "y")
        back-vocals '("a" "e")
        consonants '("d" "g" "h" "j" "k" "l" "m" "n" "p" "r" "s" "t" "v")]
    (spit-names (extract-pattern "eliel" [front-vocals back-vocals consonants])))
  (spit-names (extract-pattern "esa" ['("i" "o" "u" "y")
                                      '("a" "e")
                                      '("h" "j" "s" "v")
                                      '("d" "g" "k" "l" "m" "n" "p" "r" "t")]))
  (spit-names (extract-pattern "klojure" ['("i" "o" "u" "y" "a" "e")
                                          '("h" "j" "s" "v" "d" "g" "k" "l" "m" "n" "p" "r" "t")]))

  (println (extract-pattern-signature "klojure" ['("i" "o" "u" "y" "a" "e")
                                                 '("h" "j" "s" "v" "d" "g" "k" "l" "m" "n" "p" "r" "t")]))
  )
