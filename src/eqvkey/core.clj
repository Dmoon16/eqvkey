(ns eqvkey.core)

(defn abs 
	"Absolute value of a number"
	[n] 
	(if (not (number? n)) 
		(throw (IllegalArgumentException. "abs requires a number"))
		(max n (- n))))

(defn byte-list 
	"Transforms a string into a byte list"
	[key]
	(map (comp byte int) key))

(defn make-bit-seq 
	"Transforms a string into a list of bits"
	[key]
	(mapcat 
		#(let [bt %] 
			(for [i (range 8)] 
				(if (bit-test bt i) 1 0))) 
		(byte-list key)))

(defn eqv 
	"Computes the equivalent key to a string as a list of bits"
	[key threshold] 
	(defn alternate [acc el] 
		(let [{:keys [n0 n1 flip ekey]} acc]
			; (println n0 n1 flip ekey)
			(let [over? (>= (abs (- n0 n1)) threshold)
					flag (if over? (not flip) flip)
					bit1? (= el 1)
					newbit (if flag (if bit1? 0 1) el)
					new0 (if over? 0 (if bit1? n0 (inc n0)))
					new1 (if over? 0 (if bit1? (inc n1) n1))]
					{:n0 new0 :n1 new1 :flip flag :ekey (conj ekey newbit)})))
	(reverse 
		(get 
			(reduce 
				alternate 
				{:n0 0 :n1 0 :flip false :ekey '()} 
				(make-bit-seq key)) 
		:ekey)))