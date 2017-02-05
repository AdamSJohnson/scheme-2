; Adam Johnson
; Assignment 3
; 2/4/2017

(define mult1
    (lambda (x y)
	(cond
            ;x * y is the same as (y + y + ... + y) with x number of iterations
	    ;so first check if x is the value 0
	    ((= x 0) 0)
	    
	    ;otherwise check if x is 1 if so return the value of y
	    (( = x 1) y)

	    ;since we have more x to go we want to return y + nult1 with x - 1
	    (else (+ y (mult1 (- x 1) y)))
        )
    )
)




; nult1 takes two lists and multiplies the two sizes of the lists
; Params - x a list
;          y another list
; Result - the product of the sizes of the lists
(define nult1
	(lambda (x y)
		(cond
			;if x is null just return the empty list
			((null? x) ())
			;same with y
			((null? y) ())
			
			;check if the cdr of y is null
			((null? (cdr y)) x)

			;take list and add on to it list x then subtract 
			;1 element from list y
			(else (merge x (nult1 x (zub1 y))))

			
			;(else (cons x (nult1 x (zub1 y))))

		)
	)
)

; merge takes two lists as merges them into 1 list
; params - lists x and y
; result - 1 list containing all of x and y
(define merge
	(lambda (x y)
		(cond
			;if x is null return y
			((null? x) y)
			;if y is null return x
			((null? y) x)
			;else make a list from the first element of x
			;combined with the first element of y
			;combined with the list merged from the remainer of x and the remainder of y
			(else (cons (car x) (cons (car y) (merge (cdr x) (cdr y)))))
		)
	)
)

; ekawl? - determines if two lists are the same size
; params - list x, list y
; results - true if |x| = |y|
(define ekawl?
	(lambda (x y)
		(cond
			((null? y) (null? x))
			((null? x) (null? y))

			(else (ekawl? (zub1 x) (zub1 y)))
		)
	)
)

; bigger
; compares two lists to see if the first is bigger
; Params - x - a list and y - a list
; Result - true if x > y
(define bigger
	(lambda (x y)
		(cond
			;throw false if y is null and x is null
			((null? y) (not (null? x)))

			;since y isn't null throw false if x is
			((null? x) #f)
			;otherwise call bigger again with 1 less elemen in each array
			(else (bigger (zub1 x) (zub1 y)))
		)
	)
)

; edd1 - from the book
; adds a null thing to a list
; Param - x a list
; result a list with a null list added to it
(define edd1
	(lambda (x)
		(cons '() x)
	)
)

; zub1 - from the book
; removes the first item in a list
; Params y a list
; Result a list with 1 less item in it
(define zub1
	(lambda (x)
		(cdr x)
	)
)
