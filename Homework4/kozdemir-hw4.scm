;- Procedure: check-triple?
;- Input : Takes only one parameter named tripleList
;- Output : Returns true if the given parameter consists of triples,
; returns false if otherwise.
;- Hint: You can assume that the given input to this procedure is
; a list and it is not null.
;- Examples :
; (check-triple? ’((1 2) (3 4 5))) ------> evaluates to false since ’(1 2) has only 2 elements
; (check-triple? ’((5 12 12) (6 6 6) ())) ----> evaluates to false since ’() is empty
; (check-triple? ’((5 3 9) (9 55 32) (’a 28 67))) -----> evaluates to false since ’a is not a number
; (check-triple? ’((5 12 13) (3 4 5) (16 63 65) (12 35 37))) --> evaluates to true
(define check-triple? 
  (lambda (tripleList)
    (cond ((null? tripleList) #t) ; Base case
      ((and (list? (car tripleList)) ; Check if the first element of the list is a sublist
          (= (length (car tripleList)) 3) ; Check if the sublist has exactly three elements
          (integer? (car (car tripleList))) ; Check if the first element of the sublist is an integer
          (integer? (cadr (car tripleList))) ; Check if the second element of the sublist is an integer
          (integer? (caddr (car tripleList)))) ; Check if the third element of the sublist is an integer
        (check-triple? (cdr tripleList))) ; Recurse on the rest of the list
      (else #f)))) ; Return false if the conditions are not met

;- Procedure: check-length?
;- Input : Takes two parameters as inTriple and count
;- Output : Returns true if the length of the given list
; equals to count, returns false if otherwise.
;- Hint: You can assume that the given input (inTriple) to this procedure is
; a list.
;- Examples :
; (check-length? ’(’a ’bc) 2) --> evaluates to true
; (check-length? ’(3 4 5) 3) --> evaluates to true
; (check-length? ’(2 3 4 5) 3) --> evaluates to false since the length of the list is not 3
; (check-length? ’(3 4 5) 4) --> evaluates to false since the length of the list is not 4
(define check-length?
  (lambda (inTriple count) 
    (= (length inTriple) count)))

;- Procedure: check-sides?
;- Input : Takes only one parameter named inTriple
;- Output : It returns true if all of the elements in the given
; list are numbers and each of the numbers is greater than zero.
; It returns false if otherwise.
;- Hint: You can assume that the given input to this procedure is
; a list and it has 3 elements.
;- Examples :
; (check-sides? ’(6 4 27)) --> evaluates to true
; (check-sides? ’(6 0 27)) --> evaluates to false since not all elements are greater than 0
; (check-sides? ’(() ’c 3)) -------> evaluates to false since ’() and ’c are not numbers
; (check-sides? ’(#t 10 14)) -------> evaluates to false since #t is not a number
(define check-sides?
  (lambda (inTriple)
    (and (integer? (car inTriple))
      (integer? (cadr inTriple))
      (integer? (caddr inTriple))
      (> (car inTriple) 0)
      (> (cadr inTriple) 0)
      (> (caddr inTriple) 0))))

;- Procedure: sort-triple
;- Input : Takes only one parameter named inTriple
;- Output : It returns the sorted inTriple in ascending order.
;- Hint: You can assume that the given input to this procedure is
; a triple (see section 3).
;- Examples :
; (sort-triple ’(4 3 5)) --> evaluates to (3 4 5)
; (sort-triple ’(8 8 8)) -------> evaluates to (8 8 8)
; (sort-triple ’(6 10 14)) -------> evaluates to (6 10 14)
(define sort-triple
  (lambda (inTriple)
    (if (> (car inTriple) (cadr inTriple))
        (if (> (car inTriple) (caddr inTriple))
            (if (> (cadr inTriple) (caddr inTriple))
                (list (caddr inTriple) (cadr inTriple) (car inTriple))
                (list (cadr inTriple) (caddr inTriple) (car inTriple)))
            (list (cadr inTriple) (car inTriple) (caddr inTriple)))
        (if (> (cadr inTriple) (caddr inTriple))
            (if (> (car inTriple) (caddr inTriple))
                (list (caddr inTriple) (car inTriple) (cadr inTriple))
                (list (car inTriple) (caddr inTriple) (cadr inTriple)))
            (list (car inTriple) (cadr inTriple) (caddr inTriple)))))) 

;- Procedure: sort-all-triples
;- Input : Takes only one parameter named tripleList
;- Output : Returns the list of triples given as the parameter in which
; all triples are sorted internally in ascending order.
;- Hint: You can assume that the given input to this procedure is
; a list of triples (see section 4).
;- Examples :
; (sort-all-triples ’((4 3 5) (9 4 6) (13 12 1) (6 6 6))) --> evaluates to ((3 4 5) (4 6 9) (1 12 13) (6 6 6))
; (sort-all-triples ’((4 7 9) (15 36 9))) --> evaluates to ((4 7 9) (9 15 36))
(define sort-all-triples
  (lambda (tripleList)
    (map sort-triple tripleList)))

;- Procedure: triangle?
;- Input : Takes only one parameter named triple
;- Output : It returns true if the given triple satisfies the triangle rule,
; returns false if otherwise.
; The Triangle rule is explained in section 3.
;- Hint: You can assume that the given input to this procedure is
; a triple (see section 3) in which all elements are sorted in ascending order.
;- Examples :
; (triangle? ’(4 6 8)) ---> evaluates to #t
; (triangle? ’(12 21 34)) ---> evaluates to #f
; (triangle? ’(9 10 18)) ---> evaluates to #t
(define triangle?
  (lambda (triple)
    (and
      (> (+ (car triple) (cadr triple)) (caddr triple)) ; 1 + 2 > 3
      (> (+ (car triple) (caddr triple)) (cadr triple)) ; 1 + 3 > 2
      (> (+ (cadr triple) (caddr triple)) (car triple))))) ; 2 + 3 > 1

;- Procedure: filter-triangle
;- Input : Takes only one parameter named tripleList
;- Output : It returns tripleList consists of triples that each triple represents
; a triangle. So, it filters triangles in intripleList and discards other triples.
; The Triangle rule is explained in section 3.
;- Hint: You can assume that the given input to this procedure is
; a list of triples that each of the triples is sorted internally in ascending order.
;- Examples :
; (filter-triangle ’((3 4 5) (4 6 9) (1 12 13))) --> evaluates to ((3 4 5) (4 6 9))
; (filter-triangle ’((8 10 21) (22 31 53))) --> evaluates to ()
(define filter-triangle
  (lambda (tripleList)
    (cond ((null? tripleList) '())
      ((triangle? (car tripleList))
        (cons (car tripleList) (filter-triangle (cdr tripleList))))
      (else (filter-triangle (cdr tripleList))))))

; Helper procedure for Pythagorean theorem
; Returns the square of the given number
(define square
  (lambda (x)
  (cond ((= x 0) 0)
        (else (* x x)))))

;- Procedure: pythagorean-triangle?
;- Input : Takes only one parameter named triple
;- Output : It returns true if the given triple satisfies the Pythagorean theorem,
; returns false if otherwise.
; The Pythagorean theorem is explained in section 3.
;- Hint: You can assume that the given input to this procedure is
; a triple (see section 3) in which all elements are sorted in ascending order.
; Also, the triple itself satisfies the Triangle rule.
;- Examples :
; (pythagorean-triangle? ’(4 6 8)) ---> evaluates to #f
; (pythagorean-triangle? ’(5 12 13)) ---> evaluates to #t
; (pythagorean-triangle? ’(7 24 25)) ---> evaluates to #t
; (pythagorean-triangle? ’(9 10 18)) ---> evaluates to #f
(define pythagorean-triangle?
  (lambda (triple)
    (if (= (+ (square (car triple)) (square (cadr triple))) (square (caddr triple))) #t #f)))

;- Procedure: filter-pythagorean
;- Input : Takes only one parameter named tripleList
;- Output : It returns tripleList consists of triples that each triple represents
; a pythagorean triangle. So, it filters pythagorean triangles in intripleList
; and discards other triples.
; The Pythagorean theorem is explained in section 3.
;- Hint: You can assume that the given input to this procedure is
; a list of triples that each of the triples is sorted internally in ascending order,
; and satisfies the Triangle rule (see section 3).
;- Examples :
; (filter-pythagorean ’((3 4 5) (4 6 8))) ---> evaluates to ((3 4 5))
; (filter-pythagorean ’((3 4 5) (13 18 30) (5 12 13) (8 8 8))) ---> evaluates to ((3 4 5) (5 12 13))
; (filter-pythagorean ’((7 11 16) (9 11 12))) --------> evaluates to ()
(define filter-pythagorean
  (lambda (tripleList)
    (cond ((null? tripleList) '())
      ((pythagorean-triangle? (car tripleList))
        (cons (car tripleList) (filter-pythagorean (cdr tripleList))))
      (else (filter-pythagorean (cdr tripleList))))))

;- Procedure: get-area
;- Input : Takes only one parameter named triple
;- Output : It returns the area of the given pythagorean triangle.
;- Hint: You can assume that the given input to this procedure is
; a triple (see section 3) in which all elements are sorted in ascending order.
; Also, the triple itself satisfies the Pythagorean theorem (see section 3).
;- Examples :
; (get-area ’(3 4 5)) ---------> evaluates to 6
; (get-area ’(5 12 13)) -------> evaluates to 30
; (get-area ’(12 35 37)) ------> evaluates to 210
(define get-area
  (lambda (triple)
    (/ (* (car triple) (cadr triple)) 2)))

; Helper procedures for sorting
; Bubble sort
(define sort-one
  (lambda (lst)
    (if (null? (cdr lst)) lst
      (if (> (get-area (car lst)) (get-area (cadr lst)))
        (cons (cadr lst) (sort-one (cons (car lst) (cddr lst))))
        (cons (car lst) (sort-one (cdr lst)))))))

(define sort-list
  (lambda (lst len)
    (if (= 1 len) lst (sort-list (sort-one lst) (- len 1)))))

;- Procedure: sort-area
;- Input : Takes only one parameter named tripleList
;- Output : Returns the list of triples given as the parameter in which
; all triples are sorted according to the areas of the pythagorean triangles
; in ascending order.
;- Hint: You can assume that the given input to this procedure is
; a list of triples that each of the triples is sorted internally in ascending order,
; and satisties the Pythagorean theorem (see section 3).
; Examples :
; (sort-area ’((5 12 13) (3 4 5) (16 63 65) (12 35 37))) --> evaluates to ((3 4 5) (5 12 13) (12 35 37) (16 63 65))
; (sort-area ’((5 12 13) (16 63 65) (3 4 5))) ------>  evaluates to ((3 4 5) (5 12 13) (16 63 65))
(define sort-area
  (lambda (tripleList)
    (sort-list tripleList (length tripleList))))

;- Procedure: main-procedure
;- Input : Takes only one parameter named tripleList
;- Output : Returns list of triples according to the scenario described in section 3.
; Returns an error if the given parameter is not a tripleList.
(define main-procedure
  (lambda (tripleList)
    (if (or (null? tripleList) (not (list? tripleList)))
      (error "ERROR305: the input should be a list full of triples")
      (if (check-triple? tripleList)
        (sort-area (filter-pythagorean (filter-triangle
        (sort-all-triples tripleList))))
        (error "ERROR305: the input should be a list full of triples")))))