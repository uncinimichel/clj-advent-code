(ns clj-playground.day5)
(require '[clojure.string :as str])

(defn find-seat [min max id left right]
  (loop [mi min
         ma max
         c -1
         s id]
  (if s
    (let [half (quot (- ma mi) 2)
          l (first s)]
      (cond
        (= l left) (recur mi (- ma half 1) mi (next s))
        (= l right) (recur (+ mi half 1) ma ma (next s))
        ))
    c)))


;;max 926
;;min 80

(defn part-1-and-2 []
  (->> (read-file "resources/day5-input.txt")
       (map (fn [line]
              (+ (* 8 (find-seat 0 127 (take 7 line) \F \B))
                 (find-seat 0 7 (take-last 3 line) \L \R))))
       set
       (clojure.set/difference (set (range 80 926)))))

(part-1)

