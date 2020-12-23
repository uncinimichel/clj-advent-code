(ns clj-playground.day10)
(require '[clojure.string :as str])

(defn read-file [filename]
  (->> filename
       slurp
       str/split-lines
       (map #(Integer/parseInt %))
       sort
       seq))

(def sample
  [16
  10
  15
  5
  1
  11
  7
  19
  6
   12
   4])


(def input-10(->> "resources/day10-input.txt"
                 (read-file)))


(defn part-1 [input]
  (loop [l input
         acc [0]]
    (if (empty? l)
      (conj acc (+ 3 (last acc)))
      (let [x (first l)
            y (last acc)]
        (if (<= (- x y) 3)
          (recur (rest l) (conj acc x))
          (recur (rest l) acc))))))

(defn part-2 [l acc]
  (if (empty? l)
    (let [solution (conj acc (+ 3 (last acc)))]
           (if (= (last solution) 22)
             [solution]
             []))
    (let [x (first l)
          y (last acc)
          diff (- x y)
          ;;_ (println diff)
          ]
      (cond
        (= diff 3) (part-2 (rest l) (conj acc x))
        (< diff 3) (concat (part-2 (rest l) (conj acc x))
                           (part-2 (rest l) acc))
      ;;  (< diff 2)(concat (part-2 (rest l) (conj acc x))
        ;;                 (part-2 (rest l) acc))
        :else [];;(part-2 (rest l) acc)
        ))))

(def adap-s (part-1 (sort sample)))
(def adap (part-1 input-10))

(println adap-s)

(defn part-1-l
  (loop [l adap-s
       acc {}]
  (if (< (count l) 2)
     acc
    (let [x (first l)
          y (second l)
          diff (- y x)
          k-diff diff
          _ (println x y)]
      (recur (rest l) (update acc k-diff #(if %
                                          (inc %)
                                          1))))))) ;; {1 65, 3 27}


(defn is-valid? [test]
  (loop [l test]
    (if (< (count l) 2)
      true
      (let [x (first l)
            y (second l)
            diff (- y x)]
        (if (> diff 3)
          false
          (recur (rest l)))))))


(defn comb [test]
  (loop [l test
         combs #{}]
    (if (< (count l) 3)
      (into #{} combs)
      (let [[x y z] (take 3 l)
            diff-1 (- y x)
            diff-2 (- z x)]
        (if (and (<= diff-1 3) (<= diff-2 3))
          (recur (rest l) (conj combs (remove #{y} test)))
          (recur (rest l) combs))))))


(clojure.set/union
 (comb adap-s)
 #{[1 1 4 5 7 10 11 12 15 16 19 22]})

(println adap)

(->>(loop [tests #{adap}
           solutions #{adap}]
      (if (empty? tests)
        solutions
        (let [test (first tests)
              combs (comb test)]
          (recur (concat (rest tests) combs)
                 (clojure.set/union solutions combs)))))
    count)
