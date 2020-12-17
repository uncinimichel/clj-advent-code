(ns clj-playground.day8) 
(require '[clojure.string :as str])

(defn read-file [filename]
  (->> filename
       slurp
       str/split-lines
       ))

(def ssss(->> "resources/day8-input.txt"
     (read-file)
     ))

(def sample ["nop +0"
             "acc +1"
             "jmp +4"
             "acc +3"
             "jmp -3"
             "acc -99"
             "acc +1"
             "jmp -4"
             "acc +6"])


(defn parse-op [op]
  (let [[all op sign v] (re-matches #"(jmp|acc|nop) ([\+|\-])(\d+)" op)]
       (cond
         (= sign "+") [all op + (Integer/parseInt v)]
         (= sign "-") [all op - (Integer/parseInt v)])))

(parse-op "jmp -32")

(str/replace "jmp -32" "jmp" "nop")



(defn generate-new-stack [stack index]
  ;; Return a new stack with the jmp/nop inverted. you start from the index
  (loop [s stack
         new-s []
         found false
         counter 1]
    (if (or found (> counter (count stack)))
      [counter (concat new-s s)]
      (let [e (first s)
            [all op sign x] (parse-op e)]
        (cond
          (< counter index) (recur (rest s) (conj new-s e) false (inc counter))
          (= op "nop") (recur (rest s) (conj new-s (str/replace all "nop" "jmp")) true (inc counter))
          (= op "jmp") (recur (rest s) (conj new-s (str/replace all "jmp" "nop")) true (inc counter))
          :else (recur (rest s) (conj new-s e) false (inc counter))
          )))))




(defn valid-stack [stack]
  (loop [index 1
         v #{}
         acc 0]
    (cond
      (contains? v index) -1
      (> index (count stack)) acc
      :else (let [[_ op sign x] (parse-op (nth stack (- index 1)))
                  vv (conj v index)]
              (cond
                (= op "nop") (recur (inc index) vv acc)
                (= op "acc") (recur (inc index) vv (sign acc x))
                (= op "jmp") (recur (sign index x) vv acc)
                )))))

()

(defn part-1 []
  (->> "resources/day8-input.txt"
       read-file
       valid-stack)
  )

(part-1)

(defn part-2 []
  (let [stack (read-file "resources/day8-input.txt")]
    (loop [s stack
           i 1
           x 1]
      (let [result (valid-stack s)]
        (cond
          (not= result -1) result
          (= result -1) (let [[new-i new-s] (generate-new-stack stack i)]
                          (recur new-s new-i (inc x)))
          )))))

(print "ciao")
(print (part-2))



