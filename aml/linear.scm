
(define-module (aml linear))

(define-public (column mat col)
  (letrec ((extract-col (lambda (vec  row)
		       [if (= row (car (array-dimensions mat)))
			   vec
			   (begin
			     (array-set! vec (array-ref mat row col) row 0)
			     (extract-col vec (+ row 1)))])))
    (extract-col (make-array #t (car (array-dimensions mat)) 1) 0)))

(define-public (transpose mat)
  (let ((trans-mat (make-array 0 (cadr (array-dimensions mat))
				(car (array-dimensions mat)))))
     (array-index-map! trans-mat (lambda (i j)
				   (array-ref mat j i)))
     trans-mat))

(define-public (trace mat)
  (letrec ((sum-trace (lambda (current-total row)
			[if (= row (car (array-dimensions mat)))
			    current-total
			    (sum-trace (+ current-total (array-ref mat row row)) (+ row 1))])))
    (sum-trace 0 0)))

(define-public (multiply mat_1 mat_2)
  (letrec ((result (make-array 0 (car (array-dimensions mat_1)) (cadr (array-dimensions mat_2))))
	   (single-multiply-entry (lambda (i j k total)
			   [if (= k (car (array-dimensions mat_2)))
			       total
			       (single-multiply-entry i j (+ k 1)
						      (+ total (* (array-ref mat_1 i k)
								  (array-ref mat_2 k j))))])))
    (array-index-map! result (lambda (i j)
			       (single-multiply-entry i j 0 0)))
    result))

(define-public (scalar-multiply scalar mat_1)
  (let ((result (make-array 0 (array-dimensions mat_1))))
    (array-index-map! result (lambda (i j)
			       (* scalar (array-ref mat_1 i j))))
    result))

(define (sub-add-map proc mat_1 mat_2)
  (let ((result (make-array 0 (array-dimensions mat_1))))
    (array-index-map! result (lambda (i j)
			       (proc (array-ref mat_1 i j) (array-ref mat_2 i j))))
    result))

(define-public (add mat_1 mat_2)
  (sub-add-map + mat_1 mat_2))

(define-public (subtract mat_1 mat_2)
  (sub-add-map - mat_1 mat_2))

(define-public (dot-product vec_1 vec_2)
  (letrec ((sum-product (lambda (i total)
			   [if (= i (cadr (array-dimensions vec_1)))
			       total
			       (sum-product (+ i 1) (+ total (* (array-ref vec_1 0 i) (array-ref vec_2 0 i))))])))
    (sum-product 0 0)))

;;cross product
