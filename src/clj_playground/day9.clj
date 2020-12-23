(ns clj-playground.day9)
(require '[clojure.string :as str])

(defn read-file [filename]
  (->> filename
       slurp
       str/split-lines
       (map #(BigInteger. %))
       seq))


(def input-9(->> "resources/day9-input.txt"
               (read-file)))

(def sample (seq [35
                  20
                  15
                  25
                  47
                  40
                  62
                  55
                  65
                  95
                  102
                  117
                  150
                  182
                  127
                  219
                  299
                  277
                  309
                  576]))

(defn part-1 [input]
  (loop [l input]
    (let [preamble (take 25 l)]
      (if (<= (count l) 25)
        -1
        (let [x (last (take 26 l))
              r (for [a preamble
                      b preamble
                      :when (< a b)
                      :when (= x (+ a b))]
                      [a])]
          (if (empty? r)
            x
            (recur (rest l)))
          )))))

(part-1 input-9) ;; 556543474

(reduce + [])

(defn part-2 [input q]
  (loop [l input
         p input
         acc []]
    (let [s (reduce + acc)]
      (cond
        (= s q) acc
        (< s q) (recur (rest l) p (conj acc (first l)))
        (> s q) (recur (rest p) (rest p) [])
        ))))



(def x (part-2 input-9 556543474))


(apply max x)
(println
(+ (apply min x)
   (apply max x)))


