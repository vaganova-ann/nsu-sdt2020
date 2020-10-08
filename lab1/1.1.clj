(ns lab1.core
  (:gen-class))


(defn add-lettes-to-letter
  [alphabet word] 
  (if (> (count alphabet) 0) 
    (do
      (if (not= (first alphabet) (first word)) 
        (do
          (concat (list (cons (first alphabet) word)) 
                  (add-lettes-to-letter (rest alphabet) word)) 
          )
        (add-lettes-to-letter (rest alphabet) word))
      )
    (list)))

(defn add-letters-to-words
  [alphabet words]
  (println words)
  (if (> (count words) 0)
    (do
      (concat (add-lettes-to-letter  alphabet (first words))
              (add-letters-to-words  alphabet (rest words))))
    (list)))

(defn alphabet-combinations
  [alphabet n]
  (if (> n 0)
    (if (> n 1)
      (do
        (add-letters-to-words alphabet (alphabet-combinations alphabet (- n 1))))
      (add-letters-to-words alphabet `(())))
    (list)))

(alphabet-combinations `(:a :b :c) 3)