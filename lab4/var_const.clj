(ns lab4.var_const
  (:gen-class))

(defn variable [name] ; объявляем переменную
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr] ; проверяем, что это переменная
  (= (first expr) ::var))

(defn variable-name [v] ; имя переменной
  (second v))


(defn same-variables? [v1 v2] ; сравниваем переменные
  (and
   (variable? v1) 
   (variable? v2)
   (= (variable-name v1)
      (variable-name v2))))

(defn variable-value [expr vr ]
  (get vr expr)
  )

(defn constant [num] ; объявляем константу
  {:predicate [(boolean? num)]}
  (list ::const num))

(defn constant? [expr] ; проверка на константу
  (= (first expr) ::const))

(defn constant-value [expr] ; получить значение константы
  (second expr))
