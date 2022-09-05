#! /usr/bin/guile3.0 \
-e test -s
!#

(define path-lst (string-split (dirname (current-filename)) #\/ ))
(list-cdr-set! path-lst (- (length path-lst) 2) '())
(add-to-load-path (string-join path-lst "/"))

(use-modules (ice-9 format) (aml linear) (srfi srfi-64))

(define (test args)
  (test-column)
  (test-transpose)
  (test-trace)
  (test-mult)
  )

(define (test-column)
  (test-begin "column-test_2x3")
  (let* ((x (make-array 0 2 3))
	 (y (make-array 0 (array-rank x) 1)))
    
    (array-set! x 1 0 1)
    (array-set! x 2 0 2)
    (array-set! x 3 1 0)
    (array-set! x 4 1 1)
    (array-set! x 5 1 2)

    (array-set! y 0 0 0)
    (array-set! y 3 1 0)

    (test-equal y (column x 0))

    (array-set! y 1 0 0)
    (array-set! y 4 1 0)

    (test-equal y (column x 1))

    (array-set! y 2 0 0)
    (array-set! y 5 1 0)

    (test-equal y (column x 2)))
  (test-end "column-test_2x3"))

(define (test-transpose)
  (test-begin "transpose-test_2x3")
  (let* ((x (make-array 0 2 3))
	 (y (make-array 0 (cadr (array-dimensions x))
			(car (array-dimensions x)))))

    (array-set! x 1 0 1)
    (array-set! x 2 0 2)
    (array-set! x 3 1 0)
    (array-set! x 4 1 1)
    (array-set! x 5 1 2)

    (array-set! y 3 0 1)
    (array-set! y 1 1 0)
    (array-set! y 4 1 1)
    (array-set! y 2 2 0)
    (array-set! y 5 2 1)

    (test-equal y (transpose x)))
  (test-end "transpose-test_2x3"))

(define (test-trace)
  (test-begin "trace-test")
  (let ((x (make-array 0 3 3))
	(single (make-array 0 1 1)))

    (array-set! x 1 0 0)
    (array-set! x 2 0 1)
    (array-set! x 3 0 2)
    (array-set! x 4 1 0)
    (array-set! x 5 1 1)
    (array-set! x 6 1 2)
    (array-set! x 7 2 0)
    (array-set! x 8 2 1)
    (array-set! x 9 2 2)

    (test-equal 15 (trace x))
    
    (let ((value 25))
      (array-set! single value 0 0)
      (test-equal value (trace single)))
    
  (test-end "trace-test")))

(define (test-mult)
  (test-begin "mult-test")
  
  (let ((a (make-array 1 1 3))
	(b (make-array 0 2 3))
	(c (make-array 0 3 3)))

    (array-set! a 2 0 1)
    (array-set! a 3 0 2)

    (test-equal (make-array 14 1 1) (multiply a (transpose a)))
    (test-equal (make-array 4 1 1) (multiply (make-array 2 1 1) (make-array 2 1 1)))

    (array-set! b 1 0 0)
    (array-set! b 2 0 1)
    (array-set! b 3 0 2)
    (array-set! b 4 1 0)
    (array-set! b 5 1 1)
    (array-set! b 6 1 2)

    (let ((result (make-array 14 2 2)))
      (array-set! result 32 0 1)
      (array-set! result 32 1 0)
      (array-set! result 77 1 1)
      (test-equal result (multiply b (transpose b))))

    (let ((result (make-array 14 1 2)))
      (array-set! result 32 0 1)
      (test-equal result (multiply a (transpose b))))

    (array-set! c 1 0 0)
    (array-set! c 2 0 1)
    (array-set! c 3 0 2)
    (array-set! c 4 1 0)
    (array-set! c 5 1 1)
    (array-set! c 6 1 2)
    (array-set! c 7 2 0)
    (array-set! c 8 2 1)
    (array-set! c 9 2 2)

    (let ((result (make-array 30 3 3)))
      (array-set! result 36 0 1)
      (array-set! result 42 0 2)
      (array-set! result 66 1 0)
      (array-set! result 81 1 1)
      (array-set! result 96 1 2)
      (array-set! result 102 2 0)
      (array-set! result 126 2 1)
      (array-set! result 150 2 2)
      (test-equal result (multiply c c))))    
  
  (test-end "mult-test"))
  
