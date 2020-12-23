(require '[clojure.string :as str])


(let [numbers (->> "resources/day1-input.txt"
    slurp
    str/split-lines
    (map #(Integer/parseInt %)))]

  (for [a numbers
        b numbers
        c numbers
        :when(< a b c)
        :when(= 20000000 (+ a b c))]
    [(* c a b)]))
