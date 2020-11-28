(ns lab3.core
  (:gen-class))

; 3.1

(defn my-partition
  [m seq]
  (loop [result `() sseq seq mm m]
    (if (empty? sseq)
      result
      (recur (conj result (take mm sseq)) (drop mm sseq) mm))))

(defn heavy_filter [number]
  (Thread/sleep 100)
  (neg? number))

(defn just_filter
  [seq]
  (let [chunk (my-partition 2 seq)]
    (mapcat #(filter heavy_filter %) chunk)
    ))

(defn parallel_filter
  [seq]
  (let [chunk (my-partition 2 seq)]
    (mapcat deref (doall (map #(future (doall (filter heavy_filter %))) chunk)))))

(defn parallel_filter_list
  [seq]
  (->>
   (my-partition 2 seq)
   (map #(future (doall (filter heavy_filter %))))
   (doall)
   (mapcat deref)))

(time (doall (just_filter `(1 -2 -3 -4 5 -6 7 -8 9))))
(time (doall (parallel_filter_list `(1 -2 -3 -4 5 -6 7 -8 9))))
(time (doall (parallel_filter `(1 -2 -3 -4 5 -6 7 -8 9))))

; 3.2 

(defn new_filter
  [f]
  (fn [number] 
    (Thread/sleep 100)
    (f number))
  )

(defn my_lazy_partition 
  [m my_seq]
  (lazy-seq 
   (when-let [not_empty_seq (seq my_seq)]
    (cons (take m not_empty_seq) (my_lazy_partition m (drop m not_empty_seq)))
     )
    )
  )

(my_lazy_partition 2 `(1 2 3 4 5 6))

(defn future_batch
  [filt batch]
  (let [ff (new_filter filt)]
    (->> batch
         (map #(future (doall (filter ff %))))
         (doall)
         (mapcat deref)
         )
    )
  )

(defn lazy_parallel_filter
  [filt len_chunk len_batch seq]
  (->> seq
       (my_lazy_partition len_chunk)
       (my_lazy_partition len_batch)
       (map #(future_batch filt %))
       (mapcat identity)
       ))


(time (doall (take 3 (lazy_parallel_filter neg? 2 2 `(-1 2 -3 -4 -5 6 -7 8 -9)))))

(time (doall (take 3 (lazy_parallel_filter neg? 2 2 (take 10 (range -10 10))))))

(time (doall (lazy_parallel_filter neg? 2 2 (take 10 (range -10 0)))))

(time (take 10 (lazy_parallel_filter even? 2 2 (range))))
