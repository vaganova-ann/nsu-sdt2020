(ns lab1.core
  (:gen-class))

(defn my-map
  [f data]
  (reduce
   (fn [result elem] (conj result (f elem)))
   [] data))

(defn my-map-anon
  [f data]
  (reduce
   #(conj %1 (f %2)) [] data))

(my-map-anon #(* % 3) `(1 2 3 4))

(defn my-filter
  [f data]
  (reduce
   (fn [result elem]
     (if (f elem)
       (conj result elem)
       result))
   [] data))

(defn my-filter-anon
  [f data]
  (reduce
   #(if (f %2)
      (conj %1 %2)
      %1)
   [] data))

(my-filter-anon #(> % 2) `(1 2 3))