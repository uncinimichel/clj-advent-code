(ns clj-playground.day4
  (:require [clojure.string :as str]))

(require '[clojure.string :as str])

(def req (set ["byr"
              "iyr"
              "eyr"
              "hgt"
              "hcl"
              "ecl"
              "pid"]))

(def rules {
            "byr" #(and (>= (Integer/parseInt %) 1920)
                        (<= (Integer/parseInt %) 2020))
            "iyr" #(and (>= (Integer/parseInt %) 2010)
                        (<= (Integer/parseInt %) 2020))
            "eyr" #(and (>= (Integer/parseInt %) 2020)
                        (<= (Integer/parseInt %) 2030))
            "hgt" #(let [n (count %)
                        unit (subs % (- n 2) n)
                        high (Integer/parseInt (subs % 0 (- n 2)))]
                   (cond
                     (= "cm" unit) (and (>=  high 150)
                                        (<=  high 193))
                     (= "in" unit) (and (>=  high 59)
                                        (<=  high 76))
                     :else false))
            "hcl" #(some? (re-matches #"\#[0-9a-f]{6}" %))
            "ecl" #(contains? (set '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")) %)
            "pid" #(some? (re-matches #"[0-9]{9}" %))})

(defn read-file [filename]
  (->> filename
       slurp
       str/split-lines
       seq))

(defn reduce-to-set [s]
  (->> s
     (#(str/split % #" "))
     (reduce (fn [acc s]
               (conj acc (first (str/split s #":"))))
             (set []))))

(defn reduce-to-hash-map [s]
  (->> s
       (#(str/split % #" "))
       (reduce (fn [acc s]
                 (conj acc (apply hash-map (str/split s #":"))))
               (hash-map))))



(defn part-1 []
(->> (read-file "resources/day4-input.txt")
     (partition-by #(= "" %))
     (filter #(not= % [""]))
     (map #(str/join " " %))
     (map reduce-to-set)
     (filter #(clojure.set/subset? req %))
     count
     ))

(defn part-2 []
  (->> (read-file "resources/day4-sample.txt")
       (partition-by #(= "" %))
       (filter #(not= % [""]))
       (map #(str/join " " %))
       (map reduce-to-hash-map)
       (filter #(every? (fn [[k f]]
                          (let [_ (print (get % k))]
                             (and (contains? % k)
                                  (f (get % k))))) rules))))
(part-2)

(def t {"eyr" "1972" "cid" "100"
        "hcl" "#18171d" "ecl" "amb" "hgt" "170" "pid" "186cm" "iyr" "2018" "byr" "1926"})

(filter #(every? (fn [[k f] ]
                   (let [_ (print t)]
                   (and (contains? % k)
                       (f (get % k))))) rules) [t])
(part-1)
(part-2)

