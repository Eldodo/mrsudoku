(ns mrsudoku.dpll)


(declare find-1-literal)

(defn find-1-literal [phi]
  (some #(when (= 1 (count %))
           (first %)) phi))

(declare make-true-1)

(defn make-true-1 [clause x]
  (let [not-x (list 'not x)]
    (cond
      (contains? clause x) true
      (contains? clause not-x)
        (let [clause' (disj clause not-x)]
          (if (empty? clause')
            nil
            clause'))
      :else clause)))


(declare make-var-true)

(defn make-var-true [phi x]
  (reduce (fn [phi' clause']
            (case clause'
              nil (reduced nil)
              true phi'
              ;;else
              (conj phi' clause')))
          #{}
          (map #(make-true-1 % x) phi)))




(declare make-false-1)

(defn make-false-1 [clause x]
  (let [not-x (list 'not x)]
    (cond
      (contains? clause not-x) true
      (contains? clause x)
        (let [clause' (disj clause x)]
          (if (empty? clause')
            nil
            clause'))
      :else clause)))


(declare make-var-false)

(defn make-var-false [phi x]
  (reduce (fn [phi' clause']
            (case clause'
              nil (reduced nil)
              true phi'
              ;;else
              (conj phi' clause')))
         #{}
         (map #(make-false-1 % x) phi)))


(defn rule-1-literal [phi]
  (if-let [litt (find-1-literal phi)]
    (if-let [phi' (if (symbol? litt)
                    (make-var-true phi litt)
                    (make-var-false phi (second litt)))]
      [phi', (if (symbol? litt) litt (second litt)), (symbol? litt)]
      [nil, nil, nil])
    nil))

(declare find-neg-pos-1)

(defn find-neg-pos-1 [m clause]
  (loop [clause clause m m]
    (if (seq clause)
      (let [ [x, xsigne] (if (symbol? (first clause))
                           [(first clause) :positif]
                           [(second (first clause)) :negatif])
             signe (get m x)]
        (case signe
          nil (recur (rest clause) (assoc m x xsigne))
          :positif (recur (rest clause) (if (= signe :positif)
                                          m
                                          (assoc m x :suppr)))
          :negatif (recur (rest clause) (if (= signe :negatif)
                                          m
                                          (assoc m x :suppr)))
          :suppr (recur (rest clause) m)))
      m)))


(declare find-neg-pos)

(defn find-neg-pos [phi]
  (some (fn [[x, signe]]
          (if (not= signe :suppr)
            [x signe]
            false))
        (reduce find-neg-pos-1 {} phi)))


(declare rule-affirmative-negative)

(defn rule-affirmative-negative [phi]
  (if-let [[x, signe] (find-neg-pos phi)]
    (case signe
      :positif (make-var-true phi x)
      :negatif (make-var-false phi x))
    nil))

(declare get-var)

(defn get-var [litt]
  (if (symbol? litt)
    litt
    (second litt)))


;;premier splitter
(declare trivial-splitter)

(defn trivial-splitter [phi]
  (get-var (first (seq (first phi)))))


;;deuxième splitter
(declare varfreq1)

(defn varfreq1 [m clause]
  (reduce (fn [m litt]
            (let [x (get-var litt)]
              (update m x (fn [xnb]
                            (if xnb
                              (inc xnb)
                              1))))) m clause))


(declare varfreqs)

(defn varfreqs [phi]
  (loop [phi phi, m {}]
    (if (seq phi)
      (recur (rest phi) (varfreq1 m (first phi)))
      m)))

(declare max-val)

(defn max-val [m]
  (loop [m m, x nil, maxnb 0]
    (if (seq m)
      (let [[y, ynb] (first m)]
        (if (> ynb maxnb)
          (recur (rest m) y ynb)
          (recur (rest m) x maxnb)))
      x)))

(declare max-splitter)

(defn max-splitter [phi]
  (max-val (varfreqs phi)))

(declare dpll)

(defn dpll
  ([phi] (dpll phi {} max-splitter))
  ([phi splitter] (dpll phi {} splitter))
  ([phi sat splitter]
     (loop [phi phi, sat sat]
       (if (empty? phi)
         sat;;solution trouvée
         (if-let [[phi', x, xval] (rule-1-literal phi)]
           (if (nil? phi')
             false
             (recur phi' (assoc sat x xval)))
           (if-let [[phi', x, xval] (rule-affirmative-negative phi)]
             (recur phi' (assoc sat x xval))
             (let [x (splitter phi)]
               (or (let [phi-true (make-var-true phi x)]
                     (and phi-true (dpll phi-true (assoc sat x true) splitter)))
                   (let [phi-false (make-var-false phi x)]
                     (and phi-false (dpll phi-false (assoc sat x false) splitter)))
                   nil))))))))
