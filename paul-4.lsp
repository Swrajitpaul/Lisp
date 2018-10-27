;Solution to Problem 1
(defun sum(L)
	(if (null L)
		0
		(+ (car L) (sum(cdr L)))))

;Solution to Problem 2
(defun neg-nums (L)
	(if (null L)
		nil
		(if (plusp (car L))
			(neg-nums (cdr L))
			(cons (car L) (neg-nums (cdr L))))))

;Solution to Problem 3
(defun inc-list-2 (L N)
	(if (null L)
		nil
		(cons (+ (car L) N) (inc-list-2 (cdr L) N))))

;Solution to Problem 4
(defun insert (N L)
	(if (null L)
	    (cons N L)
	    (cond ((<= (car L) N) (cons N L))
    		  (T (cons (car L) (insert N (cdr L)))))))

;Solution to Problem 5
(defun isort(L)
	(if (endp L)
		(cons nil nil)
		(insert (car L) (isort (cdr L)))))

;Solution to Problem 6
(defun split-list (L)
	(if (endp L)
		(cons nil (cons nil nil))
		(list (cons (car L) (cadr (split-list (cdr L))))
			  (car (split-list (cdr L))))))

;Solution to Problem 7
(defun partition(L P)
	(if (endp L)
		(cons nil (cons nil nil))
		(cond ((< (car L) P) (list (cons (car L) (car (partition(cdr L) P))) (cadr (partition (cdr L) P))))
			  (T (list (car (partition (cdr L) P)) (cons (car L) (cadr (partition (cdr L) P))))))))

;Solution to Problem 8
(defun pos(E L)
	(cond ((endp L) 0)
		  ((equal E (car L)) 1)
		  (T (let ((X (pos E (cdr L))))
		  	 (if(zerop X)
		  	 	0
		  	 	(+ 1 X))))))

;Solution to Problem 9
(defun split-nums (N)
	(if (zerop N)
		(cons '(0) (cons '() nil))
		(let ((X (split-nums (- n 1))))
			(if (evenp N)
				(list (cons N (car X))
					  (cadr X))
				(list (car X)
					  (cons N (cadr X)))))))

;Solution to Problem 10
(defun set-union (s1 s2)
	(cond ((null s1) s2)
		  ((member (car s1) s2) (set-union (cdr s1) s2))
		  (T (cons (car s1) (set-union (cdr s1) s2)))))

;Solution to Problem 11
(defun set-remove (X S)
	(cond ((null S) ())
		  ((equal X (car S)) (cdr S))
		  (cons (car S) (set-remove X (cdr S)))))

;Solution to Problem 12
(defun set-excl-union (s1 s2)
	(cond ((null s1) s2)
		  ((member (car s1) s2) (set-remove (car s1) (set-excl-union (cdr s1) s2))
		  (cons (car s1) (set-excl-union (cdr s1) s2)))))

;Solution to Problem 13
(defun singletons (e)
	(cond ((null e) ())
		  ((member (car e) (cdr e)) (set-remove (car e) (singletons (cdr e))))
		  (cons (car e) (singletons (cdr e)))))