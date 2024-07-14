(ns automagic)

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))

(defn cons [x y]
  (list x y))

(defn pair? [x]
  (list? x))

(defn car [z]
  (nth z 0))

(defn cdr [z]
  (nth z 1))

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

(defn variable? [x]
  (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn sum? [x]
  (and (pair? x) (= (car x) '+)))

(defn addend [s]
  (second s))

(defn augend [s]
  (nth s 2))

(defn make-sum [a1 a2]
  (cond (= a1 0) a2
        (= a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn product? [x]
  (and (pair? x) (= (car x) '*)))

(defn multiplier [p]
  (second p))

(defn multiplicand [p]
  (nth p 2))

(defn make-product [m1 m2]
  (cond
    (or (= m1 0) (= m2 0)) 0
    (= m1 1) m2
    (= m2 1) m1
    (and (number? m1) (number? m2)) (* m1 m2)
    :else (list '* m1 m2)))

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
        (exponentiation? exp) (make-product
                               (make-product (exponent exp)
                                             (make-exponentiation 
                                              (base exp) 
                                              (make-sum (exponent exp) -1)))
                               (deriv (base exp) var))
        :else (throw (Exception. "unknown expression type: DERIV"))))

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)

(defn exponentiation? [x]
  (and (pair? x) (= (car x) '**)))

(defn make-exponentiation [b e]
  (cond (= e 0) 1
        (= e 1) b
        (and (number? b) (number? e)) (Math/pow b e)
        :else (list '** b e)))

(defn base [e]
  (second e))

(defn exponent [e]
  (nth e 2))

(def x-cubed (make-exponentiation 'x 3))
(def x-zero (make-exponentiation 'x 1))

(exponent x-cubed)

(deriv x-cubed 'x)

(defn f [a b & as]
  (println a)
  (println b)
  (println as))

(f 1 2 3 4)