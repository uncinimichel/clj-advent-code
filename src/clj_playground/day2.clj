(require '[clojure.string :as str])

(defn read-file [filename]
  (->> filename
       slurp
       str/split-lines
       seq))

(defn freq-char [word expect-c]
  (reduce (fn [acc c]
            (cond
              (= (str c) (str expect-c)) (inc acc)
              :else acc))
          0 word))

(defn char-in-pos? [p1 p2 s word]
  (let [s-word (seq word)
        c1 (nth s-word (- p1 1))
        c2 (nth s-word (- p2 1))]
    (cond
      (and (= s (str c1))
           (not= s (str c2))) true
      (and (not= s (str c1))
           (= s (str c2))) true
      :else false)))

(defn contains-string? [min max s word]
  (let [n (freq-char word s)]
    (if (and (>= n min)
             (<= n max))
      true
      false
     )))


(defn part1 []
  (->> (read-file "resources/day2-input.txt")
       (map #(re-find #"(\d+)-(\d+) (\S): (\S+)" %))
       (map #(contains-string? (Integer/parseInt (get % 1)) (Integer/parseInt (get % 2)) (get % 3) (get % 4)))
       frequencies
       ))

(defn part2 []
  (->> (read-file "resources/day2-input.txt")
       (map #(re-find #"(\d+)-(\d+) (\S): (\S+)" %))
       (map #(char-in-pos? (Integer/parseInt (get % 1)) (Integer/parseInt (get % 2)) (get % 3) (get % 4)))
       frequencies
       ))
