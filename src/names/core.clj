(ns names.core
  (:require [clojure.java.io :as io])
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


(defn coordinates-by-linear-ordering [index dimension-sizes]
  (loop [digits []
         index index
         dimension-sizes dimension-sizes]
    (if (seq dimension-sizes)
      (recur (conj digits (rem index (first dimension-sizes)))
             (quot index (first dimension-sizes))
             (rest dimension-sizes))
      digits)))




(deftest coordinates-by-linear-ordering-test
  (is (= (coordinates-by-linear-ordering 0 [2 2])
         [0 0]))

  (is (= (coordinates-by-linear-ordering 1 [2 2])
         [1 0]))

  (is (= (coordinates-by-linear-ordering 2 [2 2])
         [0 1]))

  (is (= (coordinates-by-linear-ordering 3 [2 2])
         [1 1]))

  (is (= (coordinates-by-linear-ordering 2 [3 2])
         [2 0]))

  (is (= (coordinates-by-linear-ordering 3 [3 2])
         [0 1]))

  (is (= (coordinates-by-linear-ordering 6 [3 2])
         [0 0])))


(defn index-by-linear-ordering [coordinates dimension-sizes]
  (loop [result 0
         multiplier 1
         coordinates coordinates
         dimension-sizes dimension-sizes]
    (if (seq coordinates)
      (recur (+ result
                (* multiplier
                   (first coordinates)))
             (* multiplier
                (first dimension-sizes))
             (rest coordinates)
             (rest dimension-sizes))
      result)))

(deftest index-by-linear-ordering-test
  (is (= (index-by-linear-ordering [1 0] [2 2])
         1))

  (is (= (index-by-linear-ordering [1 1] [3 2])
         4)))


(defn symbols-to-names [spec]
  (map (fn [letter-specification]
         (if (sequential? letter-specification)
           (map name letter-specification)
           (name letter-specification)))
       spec))

(defn word [coordinates dimensions]
  (loop [result ""
         coordinates coordinates
         dimensions dimensions]
    (if (seq coordinates)
      (recur (str result (nth (first dimensions) (first coordinates)))
             (rest coordinates)
             (rest dimensions))
      result)))

(deftest word-test
  (is (= (word [0 0] [["a" "b"] ["c" "d"]])
         "ac"))

  (is (= (word [0 1] [["a" "b"] ["c" "d"]])
         "ad"))

  (is (= (word [0 1] (symbols-to-names [['a "b"] ["c" "d"]]))
         "ad")))

(defn matching-word-part-index [word word-parts]
  (loop [index 0
         word-parts word-parts]
    (if (.startsWith word (first word-parts))
      index
      (if (seq word-parts)
        (recur (inc index)
               (rest word-parts))
        nil))))

(deftest matching-word-part-index-test
  (is (= (matching-word-part-index "word" ["o" "d" "w" "r"]))
      2))

(defn word-coordinates [word dimensions]
  (loop [coordinates []
         word word
         dimensions dimensions]
    (if (seq word)
      (let [word-part-index (matching-word-part-index word
                                                      (first dimensions))]
        (recur (conj coordinates word-part-index)
               (subs word (count (nth (first dimensions) word-part-index)))
               (rest dimensions)))
      coordinates)))

(deftest word-coordinates-test
  (is (= (word-coordinates "ab" [["a" "b"] ["a" "b"]])
         [0 1]))
  (is (= (word-coordinates "abcd" [["bc" "ab"] ["cd" "de"]])
         [1 0])))

(defn word-index [word dimensions]
  (index-by-linear-ordering (word-coordinates word dimensions)
                            (map count dimensions)))

(deftest word-index-test
  (is (= (word-index "ab" [["a" "b"] ["a" "b"]])
         2))
  (is (= (word-index "abcd" [["bc" "ab"] ["cd" "de"]])
         1)))

(defn maximum-index [dimensions]
  (reduce * (map count dimensions)))

(deftest maximum-index-test
  (is (= (maximum-index [["a" "b"] ["c" "d" "e"]])
         6)))

(defn names
  ([dimensions ordering]
     (names 0 dimensions ordering))

  ([index dimensions ordering]
     (lazy-seq (cons (word (ordering index (map count dimensions)) dimensions)
                     (names (inc index)
                            dimensions
                            ordering)))))

(deftest names-test
  (is (= (take (count vocals) (names [vocals] coordinates-by-linear-ordering))
         vocals))

  (is (= (take (count consonants)(names [consonants] coordinates-by-linear-ordering))
         consonants))

  (is (= (take 1 (names [["a"]] coordinates-by-linear-ordering))
         ["a"] ))

  (is (= (take 4 (names [["ak"] ["b" "z"] ["k" "r"]] coordinates-by-linear-ordering))
         '("akbk" "akzk" "akbr" "akzr"))))


(defn format-name-list [values-per-line values]
  (for [line-values (partition-all values-per-line values)]
    (apply str (interpose " " line-values))))

(deftest format-name-list-test
  (is (= (format-name-list 2 [1 2 3 4 5 6 7])
         '("1 2" "3 4" "5 6" "7"))))

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

(defn extract-pattern-signature [name letter-groups letter-group-names]
  (map (fn [letter]
         (let [letter-group-index (first (filter (fn [index]
                                                   (some (fn [group-letter]
                                                           (= (str letter) group-letter))
                                                         (letter-groups index)))
                                                 (range 0 (count letter-groups))))]
           (if letter-group-index
             (letter-group-names letter-group-index)
             letter)))
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

(defn pattern-signature-histogram [names letter-groups letter-group-names]
  (reduce (fn [histogram name]
            (increase-histogram-value histogram
                                      (extract-pattern-signature name
                                                                 letter-groups
                                                                 letter-group-names)))
          {}
          names))

(deftest pattern-signature-histogram-test
  (is (= (pattern-signature-histogram ["aa" "ab"] [["a"] ["b"]] ["first" "second"])
         '{("first" "second") 1, ("first" "first") 1})))

(defn format-pattern-signature-histogram [histogram]
  (for [key (reverse (sort-by histogram (keys histogram)))]
    (str "["(apply str (interpose " " key)) "] : " (histogram key))))

(deftest format-pattern-signature-histogram-test
  (is (= (format-pattern-signature-histogram (pattern-signature-histogram ["aa" "ab"] [["a"] ["b"]] ["first" "second"]))
         '("[first first] : 1" "[first second] : 1"))))

(defn name-specification-letter-count [specification]
  (apply + (map count (map first (map ensure-seq specification)))))

(deftest name-specification-letter-count-test
  (is (= (name-specification-letter-count ["ab" ["a" "b"]])
         3)))



(deftest symbols-to-names-test
  (is (= (symbols-to-names '[a b "c" [d "e"]])
         '("a" "b" "c" ("d" "e"))))

  (is (= (symbols-to-names [["ak"] ["b" "z"] ["k" "r"]])
         '(("ak") ("b" "z") ("k" "r")))))

(defn overflow [value maximum]
  (if (< value 0)
    (+ maximum (rem value maximum))
    (rem value maximum)))

(deftest overflow-test
  (is (= (overflow 10 8)
         2))

  (is (= (overflow -2 8)
         6)))

(defn spit-names [spec ordering middle-index number-of-names]
  (let [spec (symbols-to-names spec)]
    (with-open [writer (io/writer "names.txt")]
      (doseq [line (format-name-list (int (/ 100 (name-specification-letter-count spec)))
                                     (take number-of-names (names (overflow (- middle-index
                                                                               (int (/ number-of-names
                                                                                       2)))
                                                                            (maximum-index spec))
                                                                  spec
                                                                  ordering)))]
        (.write writer (str line "\n"))))))

(run-tests)



(comment

  (with-open [rdr (io/reader "/home/jukka/Downloads/kotus-sanalista_v1.xml")
              writer (io/writer "histogram.txt")]
    (let [words (for [line (line-seq rdr) :when (re-find #"<s>(.*)</s>" line)]
                  (.toLowerCase (second (re-find #"<s>(.*)</s>" line))))]
      (doseq [line (format-pattern-signature-histogram (pattern-signature-histogram words
                                                                                    [vocals
                                                                                     consonants]
                                                                                    ["v" "c"]))]
        (.write writer (str line "\n")))))


  (spit-names ["e" consonants vocals vocals consonants] coordinates-by-linear-ordering)
  (spit-names [["ak"] ["b" "z"] ["k" "r"]] coordinates-by-linear-ordering 0)
  (println (names (symbols-to-names [["ak"] ["b" "z"] ["k" "r"]]) coordinates-by-linear-ordering))

  (let [v (remove #{"ä" "ö" "å"} vocals)
        c (remove #{"b" "c" "f" "q" "w" "x" "z"} consonants)]

    (spit-names [c v c v c v] coordinates-by-linear-ordering)
    ;;[c v c c v] : 1595
    ;;[c v c v c v] : 901
    ;;[c v v c v] : 857
    ;;[c v c v] : 723
    ;;[c v v c c v] : 592
    ;;[c v c c v v] : 516
    )

  (let [v (remove #{"ä" "ö" "å"} vocals)
        c (remove #{"b" "c" "f" "q" "w" "x" "z"} consonants)]
    (spit-names ['(a e i u) ["m" "n" "l" "h" "v"] v ["on" "is" "os" "us"]]
                coordinates-by-linear-ordering
                0))

  (let [front-vocals '("i" "o" "u" "y")
        back-vocals '("a" "e")
        consonants '("d" "g" "h" "j" "k" "l" "m" "n" "p" "r" "s" "t" "v")]
    (spit-names (extract-pattern "eliel" [front-vocals back-vocals consonants])))

  (spit-names (extract-pattern "esa" ['("i" "o" "u" "y")
                                      '("a" "e")
                                      '("h" "j" "s" "v")
                                      '("d" "g" "k" "l" "m" "n" "p" "r" "t")])
              coordinates-by-linear-ordering
              0)

(let [dimensions ['("i" "o" "u" "y" "a" "e")
                  '("h" "j" "s" "v" "d" "g" "k" "l" "m" "n" "p" "r" "t")]
        name "elia"
        pattern (extract-pattern name dimensions)]
    (spit-names pattern
                coordinates-by-linear-ordering
                (word-index name pattern)
                1000))


  (println (extract-pattern-signature "klojure" ['("i" "o" "u" "y" "a" "e")
                                                 '("h" "j" "s" "v" "d" "g" "k" "l" "m" "n" "p" "r" "t")]))

  )
