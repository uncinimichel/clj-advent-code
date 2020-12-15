(require '[clojure.string :as str])

(defn read-file [filename]
  (->> filename
       slurp
       str/split-lines
       seq))

(defn traverse-seq [s op times]
  (loop [x s
         candidate nil
         down times]
    (if (> down 0)
      (recur (rest x) (op x) (dec down))
      candidate)))

(defn is-tree? [w pos]
  (let [c (nth (seq w) pos)]
    (if (= \# c)
      true
      false)))

(traverse-seq (read-file "resources/day3-input.txt") next 1)

(defn slope [lines right down]
  (loop [xs lines
         p right
         trees 0]
    (if (and xs (>= (count xs) down))
      (let [x (traverse-seq xs first down)
            index (mod p (count x))
            is-true? (is-tree? x index)
            new-p (+ right p)
            _ (print x index)]
        (if is-true?
          (recur (traverse-seq xs next down) new-p (inc trees))
          (recur (traverse-seq xs next down) new-p trees)))
      trees)))

(defn part1 []
  (slope (read-file "resources/day3-input.txt") 3 1))

(defn part2 []
  (let [lines (read-file "resources/day3-input.txt")]
    (map (fn [{right :right down :down}]
           (slope lines right down)) [;;                                             {:right 1 :down 1}
        ;;                                           {:right 3 :down 1}
          ;;                                         {:right 5 :down 1}
            ;;                                       {:right 7 :down 1}
                                      {:right 1 :down 2}])))
(part2)
(part1)
;; 50, 148, 53, 64, 30
(* 50 148 53 64 30)
;;(50 148 53 64 29)
;;727923200
(def sample
  ["..##......."
   "#...#...#.."
   ".#....#..#."
   "..#.#...#.#"
   ".#...##..#."
   "..#.##....."
   ".#.#.#....#"
   ".#........#"
   "#.##...#..."
   "#...##....#"
   ".#..#...#.#"])






