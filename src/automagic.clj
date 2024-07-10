(ns automagic)

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))


(defn cons [x y]
  (defn dispatch [m]
    (cond (= m 0) x
          (= m 1) y
          :else (throw (Exception. "Argument not 0 or 1"))))
  dispatch)

(defn car [z]
  (z 0))

(defn cdr [z]
  (z 1))

(defn make-rat [n d]
  (let [g (gcd n d)]
    (cons (/ n g) (/ d g))))

(defn numer [x]
  (car x))

(defn denom [x]
  (cdr x))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn print-rat [x] 
  (print (numer x))
  (print "/")
  (println (denom x)))

(defn str-rat [x]
  (str (numer x) "/" (denom x)))

(def one-half (make-rat 1 2))

(print-rat one-half)

(def one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(def neg-one-third (make-rat 1 -3))

(print-rat neg-one-third)

(defn memq [item x] 
  (cond (empty? x) false
        (= item (first x)) x
        :else (memq item (rest x))))

(memq 'apple '(x (apple suace) y apple pear))

(defn variable? [e])

(defn same-variable? [v1 v2])

(defn sum? [e])

(defn addend [e])

(defn augend [e])

(defn make-sum [a1 a2])

(defn product? [e])

(defn multiplier [e])

(defn multiplicand [e])

(defn make-product [m1 m2])

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))
        (product? exp) (make-sum
                        (make-product (multiplier exp)
                                      (deriv (multiplicand exp) var))
                        (make-product (deriv (multiplier exp) var)
                                      (multiplicand exp)))
        :else (throw (Exception. "unknown expression type: DERIV"))))