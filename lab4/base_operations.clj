(ns lab4.base_operations
  (:use lab4.var_const)
  (:gen-class))

(defn AND [expr & rest]
  (cons ::and (cons expr rest)))

(defn AND? [expr]
  (= (first expr) ::and))

(defn OR [expr & rest]
  (cons ::or (cons expr rest)))

(defn OR? [expr]
  (= (first expr) ::or))

(defn NO [expr]
  (cons ::neg expr))

(defn NO? [expr]
  (= (first expr) ::neg))

(defn IMP [expr & rest]
  (cons ::imp (cons expr rest)))

(defn IMP? [expr]
  (= (first expr) ::imp))
  
(defn args [expr]
  (rest expr))
  
(declare calc)

(defn collapse-OR [expr vr]
  (let [arguments (args expr)
        consts (distinct (filter constant? arguments))
        vars (distinct (filter variable? arguments))
        other (remove #(or (constant? %1) (variable? %1)) arguments)]
    (cond
      (some #(= (constant-value %) true) consts) (constant true)
      (some #(= (variable-value % vr) true) vars) (constant true)
      :else (concat (constant false) other))))

(defn collapse-AND [expr vr]
  (let [arguments (args expr)
        consts (distinct (filter constant? arguments))
        vars (distinct (filter variable? arguments))
        other (remove #(or (constant? %1) (variable? %1)) arguments)]
    (cond
      (some #(= (constant-value %) false) consts) (constant false)
      (some #(= (variable-value % vr) false) vars) (constant false)
      :else (concat (constant true) other))))
      
      
(defn calc-OR [expr vr]
  (let [normalized-exprs (collapse-OR expr vr)
        intermediate-result (take 2 normalized-exprs)]
    (cond
      (= (constant-value intermediate-result) true) (constant true)
      (= 2 (count normalized-exprs)) normalized-exprs
      :else (if (= true (constant-value (calc (first (drop 2 normalized-exprs)) vr)))
              (constant true)
              (constant false))
      )))


(defn calc-AND [expr vr]
  (let [normalized-exprs (collapse-AND expr vr)
        intermediate-result (take 2 normalized-exprs)]
    (cond
      (= (constant-value intermediate-result) false) (constant false)
      (= 2 (count normalized-exprs)) normalized-exprs
      :else (if (= false (constant-value (calc (first (drop 2 normalized-exprs)) vr)))
              (constant false)
              (constant true)))))

(defn calc-IMP [expr vr]
  (let [arguments (args expr)
        first_arg (first arguments)
        second_arg (second arguments)]
    (cond
      (and (constant? first_arg) (= (constant-value first_arg) false)) (constant true)
      (and (variable? first_arg) (= (variable-value first_arg vr) false)) (constant true)

      (and (constant? second_arg) (= (constant-value second_arg) true)) (constant false)
      (and (variable? second_arg) (= (variable-value second_arg vr) true)) (constant false)

      :else (if (= true (constant-value (calc second_arg vr)))
              (constant true)
              (constant false)))))
              


(defn calc-NO [expr vr]
  (let [arguments (args expr)]
    (cond
      (constant? arguments) (constant (not (constant-value arguments)))
      (variable? arguments) (calc (variable (variable-name arguments)) 
                                  {(variable (variable-name arguments)) (not (variable-value arguments vr)) })
      :else (
             cond
             (AND? arguments) (calc(cons ::or (map NO (rest arguments))) vr) 
             (OR? arguments) (calc (cons ::and (map NO (rest arguments))) vr)
             (IMP? arguments) (calc (cons ::neg (calc (cons ::imp (rest arguments)) vr)) vr)
             (NO? arguments) (calc arguments vr)
             ))))

(def rules_for_bo
  (list
   [(fn [expr vr] (constant? expr)) (fn [expr vr] (constant (constant-value vr)))]
   [(fn [expr vr] (variable? expr)) (fn [expr vr] (constant (variable-value expr vr)))]
   [(fn [expr vr] (AND? expr)) calc-AND]
   [(fn [expr vr] (OR? expr)) calc-OR]
   [(fn [expr vr] (IMP? expr)) calc-IMP]
   [(fn [expr vr] (NO? expr)) calc-NO]
   ))

(defn calc [expr vr]
  ((some (fn [rule]
           (if ((first rule) expr vr)
             (second rule)
             false))
         rules_for_bo)
   expr vr))
   
   
(calc (NO (AND (variable :x) (variable :x))) {(variable :x) false})
(calc (NO (OR (variable :x) (variable :x))) {(variable :x) true})
(calc (NO (IMP (variable :x) (variable :y))) {(variable :x) false (variable :y) true})
(calc (AND  (variable :x) (OR (constant true) (OR  (constant false) (constant false)))) {(variable :x) true})
