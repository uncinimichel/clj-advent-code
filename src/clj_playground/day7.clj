(ns clj-playground.day7)
(require '[clojure.string :as str])

(defn read-file [filename]
   (->> filename
        slurp
        str/split-lines
        seq))

(defn find-first
  [f coll]
  (first (filter f coll)))

(def sample [
             "light red bags contain 1 bright white bag, 2 muted yellow bags."
             "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
             "bright white bags contain 1 shiny gold bag."
             "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
             "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
             "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
             "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
             "faded blue bags contain no other bags."
             "dotted black bags contain no other bags."])

(def sample2 ["shiny gold bags contain 2 dark red bags."
              "dark red bags contain 2 dark orange bags."
              "dark orange bags contain 2 dark yellow bags."
              "dark yellow bags contain 2 dark green bags."
              "dark green bags contain 2 dark blue bags."
              "dark blue bags contain 2 dark violet bags."
              "dark violet bags contain no other bags."])

(def p "faded green bags contain 2 dim cyan bags, 5 dim magenta bags, 2 wavy maroon bags, 2 faded white bags.")
(def pp "dotted black bags contain no other bags.")
(defn get-entry [s]
  (let [[o-b-raw others] (str/split s #" contain ")
        bags-raw (str/split others #", ")
        o-b (keyword (str/replace (second (re-find #"([a-zA-Z ]+) bag[s]?" o-b-raw)) " " "_"))
        bags (map #(keyword (str/replace (nth (re-find #"(no|\d+) ([a-zA-Z ]+) bag[s]?[\.]?" %) 2) " " "_")) bags-raw)]
    {o-b bags}
    ))

(defn get-entry-with-count [s]
  (let [[o-b-raw others] (str/split s #" contain ")
        bags-raw (str/split others #", ")
        o-b (keyword (str/replace (second (re-find #"([a-zA-Z ]+) bag[s]?" o-b-raw)) " " "_"))
        bags (map #(re-find #"(no|\d+) ([a-zA-Z ]+) bag[s]?[\.]?" %) bags-raw)
        bags-with-count (into (hash-map)
                              (map (fn [[_ n b]]
                               (if (= n "no")
                                 {(keyword (str/replace b " " "_")) 0}
                                 {(keyword (str/replace b " " "_")) (Integer/parseInt n)}))
                                   bags))
        ]
    {o-b bags-with-count}
    ))


(get-entry p)
(->> p
     get-entry-with-count
     first)

(def all-bag-part-1 (->> (read-file "resources/day7-input.txt")
                         (map get-entry)
                         (into (hash-map))))
(def all-bag-part-2 (->> (read-file "resources/day7-input.txt")
                         (map get-entry-with-count)
                         (into (hash-map))))

(println all-bag-part-2)

(def my-bag :shiny_gold)

(defn contain-mine? [bag my-bag all-bag]
  (loop [current bag]
    (let [_ (print current)]
    (cond
      (= current my-bag) true
      (= current :other) false
      :else (some #(contain-mine? % my-bag all-bag) (bag all-bag))))))


(defn part-1 [my-bag all-bag]
  (loop [bags (keys all-bag)
         acc 0]
    (if (empty? bags)
      acc
      (let [bag (first bags)
            is-there? (contain-mine? bag my-bag all-bag)]
        (cond
          (= bag my-bag) (recur (rest bags) acc)
          is-there? (recur (rest bags) (inc acc))
          :else (recur (rest bags) acc))))))

(defn part-2 [bag all-bag]
  (let []
    (if (nil? (:other (bag all-bag)))
      (reduce (fn [acc [k v]]
                (+ acc (* v (part-2 k all-bag))))
              1
              (bag all-bag))
      1)))

(let [_ (println "_______")]
  (- (part-2 my-bag all-bag-part-2)
     1))








