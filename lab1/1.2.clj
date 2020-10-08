(ns lab1.core
  (:gen-class))

(defn add-lettes-to-letter-recur
  [alphabet word]
  (loop [rest_alph alphabet my_word word new_word ()]
    (if  (and (> (count rest_alph) 0))
      (do
        (if (not= (first rest_alph) (first my_word))
          (do
            (recur (rest rest_alph) my_word (concat new_word (list (cons (first rest_alph) my_word)))))
          (recur (rest rest_alph) my_word new_word)))
      new_word)))

(defn add-letters-to-words-recur
  [alphabet words]
  (println words)
  (loop [alph alphabet one_word words result ()]
    (if (> (count one_word) 0)
      (do
        (recur alph
               (rest one_word)
               (concat result (add-lettes-to-letter-recur alph (first one_word)))))
      result)))

(defn alphabet-combinations-recur
  [alphabet n]
  (loop [alph alphabet nn n result `(())]
    (if (> nn 0)
      (recur alph (- nn 1) (add-letters-to-words-recur alph result))
      result)))

(alphabet-combinations-recur `(:a :b :c) 3)