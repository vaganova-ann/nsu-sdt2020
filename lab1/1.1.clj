(ns lab1.core
  (:gen-class))


(defn add-lettes-to-letter
  [alphabet word] 
  (if (> (count alphabet) 0) 
    (if (not= (first alphabet) (first word))
      (cons (cons (first alphabet) word) 
            (add-lettes-to-letter (rest alphabet) word))
      (add-lettes-to-letter (rest alphabet) word))
    (list)))


(defn add-letters-to-words
  [alphabet words]
  (if (> (count words) 0)
    (concat (add-lettes-to-letter  alphabet (first words))
            (add-letters-to-words  alphabet (rest words)))
    (list)))

(defn alphabet-combinations
  [alphabet n]
  (if (> n 1)
    (add-letters-to-words alphabet (alphabet-combinations alphabet (- n 1)))
    (add-letters-to-words alphabet `(()))))

(alphabet-combinations `(:a :b :c) 3)