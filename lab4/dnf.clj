(ns lab4.dnf
  (:use lab4.base_operations)
  (:use lab4.var_const)
  (:gen-class))
  
(defn remove-implication [expr]
  (let [[x left right] expr]
    (if  (> (count expr) 2)
      (cond
        (IMP? expr) (concat `(:lab4.base_operations/or) (list (NO (remove-implication left)) (remove-implication right)))
        (NO? expr) (if (variable? (rest expr))
                     expr
                     (concat `(:lab4.base_operations/neg) (remove-implication (rest expr))))
        :else (concat (list x) (list (remove-implication left) (remove-implication right))))
      expr)))
      

(remove-implication (AND (variable :y) (IMP (variable :x) (variable :z))))
(remove-implication  (IMP (variable :x) (AND (variable :y) (IMP (variable :x) (variable :y)))))
(remove-implication (AND (variable :y) (NO (IMP (variable :x) (variable :z)))))

(defn de-morgan [expr]
  (let [[x left right] (rest expr)]
    (if  (> (count expr) 2)
      (cond
        (and (NO? expr) (= x :lab4.base_operations/and)) (concat `(:lab4.base_operations/or) (list (de-morgan (NO left)) (de-morgan (NO right))))
        (and (NO? expr) (= x :lab4.base_operations/or)) (concat `(:lab4.base_operations/and) (list (de-morgan (NO left)) (de-morgan (NO right))))
        (NO? expr) (concat `(:lab4.base_operations/neg) (de-morgan (rest expr)))
        :else (concat (list (first expr)) (list (de-morgan x) (de-morgan left))))

      expr)))
      
(de-morgan (NO (AND (variable :x) (OR (variable :y) (variable :z)))))
(de-morgan (AND (NO (OR (variable :y) (variable :z))) (variable :x)))

(defn remove-double-no [expr]
  (let [[x left right] expr]
    (if  (> (count expr) 2)
      (cond
        (and (NO? expr) (= left :lab4.base_operations/neg)) (remove-double-no (rest (rest expr)))
        (NO? expr) (if (variable? (rest expr))
                     expr
                     (concat `(:lab4.base_operations/neg) (remove-implication (rest expr))))
        :else (concat (list x) (list (remove-double-no left) (remove-double-no right))))
      expr)))
      
(remove-double-no (NO (NO (variable :x))))
(remove-double-no (AND (OR (NO (variable :k)) (NO (NO (variable :l)))) (OR (NO (variable :x)) (NO (NO (variable :z))))))

(defn distributive [expr]
  ; рассматриваем только a*(b+c)=(a*b)+(a*c) потому что у нас приведение к ДНФ
  (let [[x left right] expr]
    (if  (> (count expr) 2)
      (cond
        (and (AND? expr) (OR? right))
        (concat `(:lab4.base_operations/or)
                (list
                 (AND (distributive left) (distributive (nth right 1)))
                 (AND (distributive left) (distributive (nth right 2)))))
        (NO? expr) (if (variable? (rest expr))
                     expr
                     (concat `(:lab4.base_operations/neg) (remove-implication (rest expr))))
        :else (concat (list x) (list (distributive left) (distributive right))))
      expr)))
      
(distributive (AND (variable :x) (OR (variable :y) (variable :z))))

(defn absorption [expr]
  ; x + x*y = x
  (let [[x left right] expr]
    (if  (> (count expr) 2)
      (cond
        (and (OR? expr) (AND? right) (= left (second right))) left
        (NO? expr) (if (variable? (rest expr))
                     expr
                     (concat `(:lab4.base_operations/neg) (remove-implication (rest expr))))
        :else (concat (list x) (list (absorption left) (absorption right))))
      expr)))
      
(absorption (OR (OR (variable :x) (variable :y)) (AND (OR (variable :x) (variable :y)) (variable :z))))     

(defn dnf [expr]
  (println expr)
  (->> expr
       (remove-implication)
       (de-morgan)
       (remove-double-no)
       (distributive)
       (absorption)))
       

(dnf (NO (OR (IMP (variable :x) (variable :y)) (IMP (variable :y) (NO (variable :z))))))
(dnf (IMP (OR (variable :a) (variable :b)) (OR (variable :c) (IMP (variable :d) (variable :e)))))
(dnf (AND (variable :y) (NO (IMP (variable :x) (variable :z)))))       
