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

(defn my_lazy_partition
  ([m seq] (my_lazy_partition (drop m seq) m (take m seq)))
  ([seq m result]
   (lazy-seq (cons result (my_lazy_partition (drop m seq) m (take m seq))))))

(defn lazy_parallel_filter
  [seq len_chunk len_batch]
  (->> seq
       (my_lazy_partition len_chunk)
       (my_lazy_partition len_batch)
       
       ; (╯ ° □ °) ╯ (┻━┻)
       
       ))