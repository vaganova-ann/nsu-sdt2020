(ns lab1.core
  (:gen-class))

(defn word-to-word
  [word alphabet]
  (map #(cons % word) (filter #(not= (first word) %) alphabet)))

(word-to-word `(:b) `(:b :c))

(defn concat-words-to-alphabet
  [words alphabet]
  (reduce concat (map #(word-to-word % alphabet) words)))

(concat-words-to-alphabet `((:a) (:b)) `(:a :b :c))

(defn create-all
  [alphabet n]
  (reduce
   (fn [result nn]
     (concat-words-to-alphabet result alphabet))
   (list ()) (range n)))

(create-all `(:a :b :c) 3)

(defn create-all-iterate
  [alphabet n]
  (let [nn (+ n 1)]
    (nth (iterate #(concat-words-to-alphabet % alphabet) `(())) (dec nn))))

(create-all-iterate `(:a :b :c) 3)