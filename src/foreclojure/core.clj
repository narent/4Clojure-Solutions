(ns foreclojure.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn longest-seq [x] (->> 
	(partition 2 1 x) 
	(partition-by #(apply - %))
	(filter (fn [a] (every? #(= (apply - %) -1) a)))
	(reduce #(if (> (count %2) (count %1)) %2 %1) '())
	(flatten)
	(distinct)
	))

(defn my-partition [groupsize, thelist] (
	loop [l thelist partitions '()]
		(if (empty? l)
			partitions
			(recur 
				(drop groupsize l)
				(->> 
					(take groupsize l) 
					(list) 
					(concat partitions)
					(filter #(= (count %) groupsize))
					  )))
))

; Count Occurrences
(defn c-o [x] (reduce 
	#(merge %1 
		(hash-map %2 (inc (get %1 %2 0))))
	'{} x))

;56 Find Distinct Items
(defn fdi [x] (reduce 
	(fn [v i] (if (every? #(not= % i) v) (conj v i) v))
	[] x))

;58 Function Composition
(defn my-comp [& functions] 
	(let [[f & fs] (reverse functions)]
	(fn [& p] (reduce #(%2 %1) (apply f p) fs))))

;59 Juxt
(defn my-juxt [& fns] (fn [& p] (map #(apply %1 p) fns)))

;60 Sequence Reductions
; (defn my-reductions
; 	([f coll] (reduce #(lazy-cat %1 [(f (last %1) %2)]) (vector (first coll)) (rest coll)))
; 		)
(defn my-reductions
	([f coll] 
		(my-reductions f (first coll) (rest coll)))
	([f seed coll] 
		(if (empty? coll) [seed] (cons seed (lazy-seq (my-reductions f (f seed (first coll)) (rest coll)))))))

;61 Map Construction
#(into {} (map vector %1 %2))

;62 Iterate
(defn my-iterate [f x] 
	(cons x  (lazy-seq (my-iterate f  (f x)))))
(defn my-iterate2 [f x] 
	(reductions (fn [v i] (f v)) x (range)))

;63 Group a Sequence
(defn my-group-seq [f coll] 
	(reduce #(let [h %1 k (f %2) v (conj (or (h k) []) %2)] (into h [[k  v]])) 
		{} coll))

;135 Infix
(defn infix [op1 f op2 & others] (if (empty? others) (f op1 op2) (apply infix (cons (f op1 op2) others))))

;66 GCD
(defn gcd [x y] 
	(let [r (rem x y)] (if (= r 0) y (recur y r))))

;156 Map Defaults
(defn make-map [n ks] (reduce (fn [m k] (into m [[k n]])) {} ks))
(defn make-map2 [n ks] (zipmap ks (repeat (count ks) n)))

;40 Interpose
(defn my-interpose [n l] (butlast (reduce #(into %1 [%2 n]) [] l)))

;31 Pack a sequence
(defn my-pack-a-sequence [l] (partition-by identity l))

;166 Comparisions
(defn my-comp [op x y] 
	(cond 
		(= (op x y) (op y x)) :eq)
	)

;81 Set intersection
(defn my-set-intersection [s1 s2] 
	(if (> (count s2) (count s1)) 
		(recur s2 s1) 
		(reduce 
			#(if (contains? s2 %2) (conj %1 %2) %1) 
			#{} s1)
		)
	)
;Sets can be treated as functions
;So, the below will also work
(defn my-set-intersection2 [s1 s2] ((comp set filter) s1 s2))

;74 Filter perfect squares
(defn my-filter-perfect-squares [s] (
	let [ns (read-string (str "(" s ")"))]
	(->> ns 
		(filter #(let [sq  (int (Math/sqrt %))] (= % (* sq sq))))
		(interpose ",")
		(apply str))
))

;69 Merge with a function
(defn my-merge-with-function [f & ms] 
	()
	)

