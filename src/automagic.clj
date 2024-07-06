(ns automagic)

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
  (cons n d))

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
  (println)
  (print (numer x))
  (print "/")
  (print (denom x)))

(print-rat (div-rat (make-rat 3 2) (make-rat 1 4)))

(defn str-rat [x]
  (str (numer x) "/" (denom x)))

(print-rat (sub-rat (make-rat 1 2) (make-rat 1 3)))