(ns clj-playground.day6)
(require '[clojure.string :as str])


(defn read-file [filename]
  (->> filename
       slurp
       str/split-lines
       seq))



(defn part-1 []
  (->> (read-file "resources/day6-input.txt")
       (partition-by #(= "" %))
       (filter #(not= % [""]))
       (map #(str/join "" %))
       (map #(set %))
       (map count)
       (reduce +)
       ))

(defn part-2 []
  (->> (read-file "resources/day6-input.txt")
       (partition-by #(= "" %))
       (filter #(not= % [""]))
       (map #(map set %))
       (map #(reduce clojure.set/intersection %))
       (map count)
       (reduce +)
       ))

(part-2)

;;( f)
