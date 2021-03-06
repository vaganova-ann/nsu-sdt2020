(ns lab2.core
  (:gen-class))

;; 2.0
(defn trapezoid
  [f t1 t2]
  (* 0.5 (- t2 t1) (+ (f t1) (f t2))))

(trapezoid #(* %1 %1) 1 2)

(defn many_trapezoids
  [f a b n]
  (let [w (/ (- b a) n)
        b (+ b w)
        seq (range a b w)]
    (map #(trapezoid f %1 %2) seq (rest seq))))

(time (reduce + (many_trapezoids #(* %1 %1) 0 6 10)))

;; 2.1

(defn calc_tail 
  [f b w]
  (/ (* (- b (- b (mod b w)))
        (+ (f b)
           (f (- b (mod b w)))))
     2.0)
  )

(def integrate_no_tail
  (memoize
   (fn [f b w]
     (let [bb (- b (mod b w))]
       (if (> bb 0.0)
         (+ (integrate_no_tail f (- bb w) w)
            (/ (* w
                  (+ (f bb)
                     (f (- bb w))))
               2.0))
         (+ 0))))))

(defn integrate
  [f b w]
  (if (= 0 (mod b w))
    (integrate_no_tail f b w)
    (+ (calc_tail f b w) (integrate_no_tail f b w))))

(defn -main [& args]
  (let [f (fn [x] x)
        w 5]
    (time (integrate f 20 w))
    (time (integrate f 15 w))
    (time (integrate f 20 w))
    (time (integrate f 50 w))
  ))

(-main)


;; 2.2.0

(defn many_trapezoids_strange_range
  [f a b n]
  (let [w (/ (- b a) n)
        seq (map #(* w %) (range))]
    (map #(trapezoid f %1 %2) seq (rest seq))))

(time (take 10 (reductions + (many_trapezoids_strange_range #(* %1 %1) 0 6 10))))

(defn create_lazy_seq_integrate
  ([f a w] (create_lazy_seq_integrate f a w 0))
  ([f a w res]
   (lazy-seq (cons res (create_lazy_seq_integrate f (+ a w) w (+ res (trapezoid f a (+ a w))))))))

(nth (create_lazy_seq_integrate (fn [x] 1) 0 5) 5)

(defn lazy_integrate [f seq b w]
  (if  (= (mod b w) 0)
    (nth seq (/ b w))
    (+ (nth seq (/ (- b (mod b w)) w)) (trapezoid f (- b (mod b w)) b))))

(let [b 1000
      w 3
      f (fn [x] 1)
      seq (create_lazy_seq_integrate f 0 w)]
  (time (lazy_integrate f seq  b w))
  (time (lazy_integrate f seq b w))
  (time (lazy_integrate f seq b w)))

; 2.2.1
 
(defn get_lazy_integrate [f w]
  (let [seq (create_lazy_seq_integrate f 0 w)]
    (fn [b]
      (if  (= (mod b w) 0)
        (nth seq (/ b w))
        (+ (nth seq (/ (- b (mod b w)) w)) (trapezoid f (- b (mod b w)) b))))))


(let [w 3
      f (fn [x] 1)
      seq_integrate (get_lazy_integrate f w)]
  (time (seq_integrate 1000))
  (time (seq_integrate 1100)))