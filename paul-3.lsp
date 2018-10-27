
;Solution to Problem 1
(DEFUN MIN-2 (A B)
   (IF (AND (NUMBERP A) (NUMBERP B))
       (if (<= A B) A B)
       'ERROR))

;Solution to Problem 2
(DEFUN SAFE-AVG (A B)
   (IF (AND (NUMBERP A) (NUMBERP B))
       (/ (+ A B) 2)
       NIL))

;Solution to Problem 3
(DEFUN ODD-GT-MILLION (A)
   (IF (AND (INTEGERP A) (> A 1000000))
       (ODDP A)
       NIL))

;Solution to Problem 4
(DEFUN MULTIPLE-MEMBER (A B)
	MEMBER A (CDR (MEMBER A B))))

;Solution to Problem 5
(DEFUN MONTH->INTEGER(M)
	(COND ((EQL M 'JANUARY) 1)
		  ((EQL M 'FEBRUARY) 2)
		  ((EQL M 'MARCH) 3)
		  ((EQL M 'APRIL) 4)
		  ((EQL M 'MAY) 5)
		  ((EQL M 'JUNE) 6)
		  ((EQL M 'JULY) 7)
		  ((EQL M 'AUGUST) 8)
		  ((EQL M 'SEPTEMBER) 9)
		  ((EQL M 'OCTOBER) 10)
		  ((EQL M 'NOVEMBER) 11)
		  ((EQL M 'DECEMBER) 12)))

;Solution to Problem 6
(DEFUN SCORE->GRADE (G)
	(COND ((AND (NUMBERP G) (< G 60)) 'F)
	      ((AND (NUMBERP G) (>= 60 G) (< G 70)) 'D)	
	      ((AND (NUMBERP G) (>= 70 G) (< G 73)) 'C)
	      ((AND (NUMBERP G) (>= 73 G) (< G 77)) 'C+)
	      ((AND (NUMBERP G) (>= 77 G) (< G 80)) 'B-)
	      ((AND (NUMBERP G) (>= 80 G) (< G 83)) 'B)
	      ((AND (NUMBERP G) (>= 83 G) (< G 87)) 'B+)
	      ((AND (NUMBERP G) (>= 87 G) (< G 90)) 'A-)
	      ((AND (NUMBERP G) (>= G 90)) 'A)))

;Solution to Problem 7
(DEFUN GT (A B)
   (AND (NUMBERP A) (NUMBERP B) (> A B)))

;Solution to Problem 8
(DEFUN SAME-PARITY (A B) 
	(OR (AND (NUMBERP A) (NUMBERP B) (ZEROP A) (ZEROP B))
		(AND (NUMBERP A) (NUMBERP B) (PLUSP A) (PLUSP B))
		(AND (NUMBERP A) (NUMBERP B) (MINUSP A) (MINUSP B))))

;Solution to Problem 9
(DEFUN SAFE-DIV (A B)
	(AND (NUMBERP A) (NUMBERP B) (NOT(ZEROP B)) (/ A B)))