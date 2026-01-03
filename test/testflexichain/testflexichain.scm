(module testflexichain
   (library srfi64
            flexichain))


(test-begin "flexichain-test")

(define gb (make-gapbuffer))

(test-assert "vector is not a flexichain" (not (flexichain? (vector))))

(test-assert "gapbuffer is a flexichain" (flexichain? gb))

(test-assert "vector is not a gapbuffer" (not (gapbuffer? (vector))))

(test-assert "gapbuffer is a gapbuffer" (gapbuffer? gb))

(define vgb (vector->gapbuffer (vector 1 2 3 4 5)))

(test-assert "vector->gapbuffer returns a gapbuffer" (gapbuffer? vgb))

(test-assert "flexi-length on #fc(1 2 3 4 5) => 5" (= (flexi-length vgb) 5))

(test-assert "flexi-ref 2 on #fc(1 2 3 4 5) => 3" (= 3 (flexi-ref vgb 2)))

(test-assert "flexi-set! #fc(1 2 3 4 5) 0 6 => #fc(6 1 2 3 4 5)"
   (begin (flexi-set! vgb 0 6)
          (and (= 6 (flexi-ref vgb 0))
               (= 5 (flexi-length vgb)))))

(test-assert "flexi-delete! #fc(6 2 3 4 5) => #fc(2 3 4 5)"
   (begin (flexi-delete! vgb 0)
          (and (= 2 (flexi-ref vgb 0))
               (= 4 (flexi-length vgb)))))

(test-assert "flexi-insert! #fc(2 3 4 5) 0 1 => #fc(1 2 3 4 5)"
   (begin (flexi-insert! vgb 0 1)
          (and (= 1 (flexi-ref vgb 0))
               (= 5 (flexi-length vgb)))))

(test-assert "flexi-insert-vector! #fc(1 2 3 4 5) 0 #(-2 -1 0)
              => #fc(-2 -1 0 1 2 3 4 5)"
   (begin (flexi-insert-vector! vgb 0 '#(-2 -1 0))
          (and (= -2 (flexi-ref vgb 0))
               (= -1 (flexi-ref vgb 1))
               (= 0 (flexi-ref vgb 2)))))

(test-assert "flexi-delete-items! #fc(-2 -1 0 1 2 3 4 5) 3 5
              => #fc(-2 -1 0)"
   (begin (flexi-delete-items! vgb 3 5)
          (and  (= -2 (flexi-ref vgb 0))
                (= -1 (flexi-ref vgb 1))
                (= 0 (flexi-ref vgb 2)))))


(define (string->vector str::bstring)
   (do ((i 0 (+ i 1))
        (vec (make-vector (string-length str))))
       ((= i (string-length str)) vec)
       (vector-set! vec i (string-ref str i))))


(define (flexi-for-each proc fc::<flexichain>)
   (do ((i 0 (+ i 1)))
       ((= i (flexi-length fc)))
       (proc (flexi-ref fc i))))


(define s (make-gapbuffer))
(gapbuffer-print-rep s)
(flexi-insert-vector! s 0 (string->vector "abcdefghklm"))
(gapbuffer-print-rep s)
(flexi-delete! s 3)
(gapbuffer-print-rep s)
; (define lmgb (make-gapbuffer :initial-capacity 16))
; (flexi-insert-vector! lmgb 0 (string->vector "abcdefghklm"))
; (flexi-delete! lmgb 3)

; (flexi-for-each (lambda (i) (printf " ~a " i)) lmgb)
; (newline)
; (flexi-insert! lmgb 3 #\d)

; (print "after insert #\d")
; (flexi-for-each (lambda (i) (printf " ~a " i)) lmgb)
; (newline)



; (gapbuffer-print-rep lmgb)

;;; Additional comprehensive tests
(test-group "Constructor Tests"
   (test-assert "make-gapbuffer with parameters"
      (gapbuffer? (make-gapbuffer :initial-capacity 10 :fill #\space 
                                  :min-capacity 8 :expand-factor 2.0)))
   
   (test-assert "list->gapbuffer conversion"
      (let ((gb (list->gapbuffer '(a b c d))))
         (and (gapbuffer? gb) (= (flexi-length gb) 4))))
   
   (test-assert "gapbuffer variadic constructor"
      (let ((gb (gapbuffer #\a #\b #\c)))
         (and (gapbuffer? gb) (= (flexi-length gb) 3)))))

(test-group "Parameter Validation Tests"
   ;; Test that proper exception types are thrown
   (test-assert "expand-factor <= 1.0 throws flexi-initialization-error"
      (with-handler 
         (lambda (e) (isa? e &flexi-initialization-error))
         (make-gapbuffer :expand-factor 0.5)
         #f))
   
   (test-assert "expand-factor = 1.0 throws flexi-initialization-error"
      (with-handler 
         (lambda (e) (isa? e &flexi-initialization-error))
         (make-gapbuffer :expand-factor 1.0)
         #f))
   
   (test-assert "min-capacity <= 0 throws flexi-initialization-error"
      (with-handler 
         (lambda (e) (isa? e &flexi-initialization-error))
         (make-gapbuffer :min-capacity 0)
         #f))
   
   (test-assert "negative min-capacity throws flexi-initialization-error"
      (with-handler 
         (lambda (e) (isa? e &flexi-initialization-error))
         (make-gapbuffer :min-capacity -5)
         #f))
   
   (test-assert "negative initial-capacity throws flexi-initialization-error"
      (with-handler 
         (lambda (e) (isa? e &flexi-initialization-error))
         (make-gapbuffer :initial-capacity -1)
         #f)))

(test-group "Stack Operations Tests"
   (let ((gb (make-gapbuffer)))
      ;; Push operations
      (flexi-push-start! gb 'start1)
      (flexi-push-end! gb 'end1)
      (flexi-push-start! gb 'start2)
      (flexi-push-end! gb 'end2)
      
      (test-equal "stack length" 4 (flexi-length gb))
      (test-equal "start element" 'start2 (flexi-ref gb 0))
      (test-equal "end element" 'end2 (flexi-ref gb 3))
      
      ;; Pop operations
      (test-equal "pop start" 'start2 (flexi-pop-start! gb))
      (test-equal "pop end" 'end2 (flexi-pop-end! gb))
      (test-equal "after pops length" 2 (flexi-length gb))))

(test-group "Rotation Tests"
   ;; Test right rotation
   (let ((gb (vector->gapbuffer (vector 'a 'b 'c 'd 'e))))
      (flexi-rotate! gb 2)
      (test-equal "rotate right element 0" 'c (flexi-ref gb 0))
      (test-equal "rotate right element 1" 'd (flexi-ref gb 1))
      (test-equal "rotate right wrapped" 'a (flexi-ref gb 3)))
   
   ;; Test left rotation with fresh buffer
   (let ((gb (vector->gapbuffer (vector 'a 'b 'c 'd 'e))))
      (flexi-rotate! gb -1)
      (test-equal "rotate left element 0" 'e (flexi-ref gb 0))
      (test-equal "rotate left element 4" 'd (flexi-ref gb 4))))

(test-group "Cursor Tests"
   (let* ((gb (vector->gapbuffer (vector 'a 'b 'c 'd 'e)))
          (cursor (make-left-sticky-flexicursor gb :position 2)))
      
      (test-equal "cursor position" 2 (cursor-pos cursor))
      (test-assert "cursor not at beginning" (not (cursor-at-beginning? cursor)))
      (test-assert "cursor not at end" (not (cursor-at-end? cursor)))
      
      ;; Cursor movement (with explicit parameter)
      (cursor-move> cursor 1)
      (test-equal "cursor move right" 3 (cursor-pos cursor))
      
      (cursor-move< cursor 2)
      (test-equal "cursor move left" 1 (cursor-pos cursor))
      
      ;; Cursor access
      (test-equal "cursor ref before" 'a (cursor-ref< cursor))
      (test-equal "cursor ref after" 'b (cursor-ref> cursor))))

(test-group "Cursorchain Tests"
   (let ((cc (make-cursorchain :fill #\space)))
      (test-assert "cursorchain is flexichain" (flexichain? cc))
      (test-assert "cursorchain is gapbuffer" (gapbuffer? cc))
      
      ;; Test with cursors
      (let ((cursor1 (make-left-sticky-flexicursor cc :position 0))
            (cursor2 (make-right-sticky-flexicursor cc :position 0)))
         
         (flexi-insert-vector! cc 0 (vector #\H #\e #\l #\l #\o))
         
         ;; Cursors should adjust automatically
         (test-equal "cursor1 after insert" 0 (cursor-pos cursor1))
         (test-equal "cursor2 after insert" 5 (cursor-pos cursor2)))))

(test-group "Error Handling Tests"
   (let ((gb (vector->gapbuffer (vector 1 2 3))))
      
      ;; Position errors should throw flexi-position-error
      (test-assert "negative position access throws flexi-position-error"
         (with-handler 
            (lambda (e) (isa? e &flexi-position-error))
            (flexi-ref gb -1)
            #f))
      
      (test-assert "out of bounds access throws flexi-position-error"
         (with-handler 
            (lambda (e) (isa? e &flexi-position-error))
            (flexi-ref gb 5)
            #f))
      
      (test-assert "negative position insert throws flexi-position-error"
         (with-handler 
            (lambda (e) (isa? e &flexi-position-error))
            (flexi-insert! gb -1 'x)
            #f))
      
      (test-assert "out of bounds insert throws flexi-position-error"
         (with-handler 
            (lambda (e) (isa? e &flexi-position-error))
            (flexi-insert! gb 10 'x)
            #f))
      
      (test-assert "negative position delete throws flexi-position-error"
         (with-handler 
            (lambda (e) (isa? e &flexi-position-error))
            (flexi-delete! gb -1)
            #f))
      
      (test-assert "out of bounds delete throws flexi-position-error"
         (with-handler 
            (lambda (e) (isa? e &flexi-position-error))
            (flexi-delete! gb 5)
            #f))))

(test-group "Ranked Flexichain Tests"
   ;; Define a test element class that uses rank mixin
   (define-class <test-element>::<element-rank-mixin>
      content::obj)
   
   (let ((chain (make-ranked-gapbuffer)))
      
      ;; Create ranked elements
      (define elem1 (instantiate::<test-element> (content "first") (index 0) (chain chain)))
      (define elem2 (instantiate::<test-element> (content "second") (index 1) (chain chain)))
      (define elem3 (instantiate::<test-element> (content "third") (index 2) (chain chain)))
      
      ;; Test insertion and automatic rank assignment
      (flexi-insert! chain 0 elem1)
      (test-equal "elem1 rank after insert" 0 (rank elem1))
      (test-assert "elem1 is first" (flexi-first? elem1))
      (test-assert "elem1 is last" (flexi-last? elem1))
      
      (flexi-insert! chain 1 elem2)
      (test-equal "elem2 rank after insert" 1 (rank elem2))
      (test-assert "elem1 still first" (flexi-first? elem1))
      (test-assert "elem2 is last" (flexi-last? elem2))
      (test-assert "elem1 not last anymore" (not (flexi-last? elem1)))
      
      (flexi-insert! chain 1 elem3)  ; Insert in middle
      (test-equal "elem1 rank unchanged" 0 (rank elem1))
      (test-equal "elem3 rank in middle" 1 (rank elem3))
      (test-equal "elem2 rank shifted" 2 (rank elem2))
      
      ;; Test navigation
      (test-equal "elem1 next is elem3" elem3 (flexi-next elem1))
      (test-equal "elem3 next is elem2" elem2 (flexi-next elem3))
      (test-equal "elem3 prev is elem1" elem1 (flexi-prev elem3))
      (test-equal "elem2 prev is elem3" elem3 (flexi-prev elem2))
      
      ;; Test position queries
      (test-assert "elem3 not first" (not (flexi-first? elem3)))
      (test-assert "elem3 not last" (not (flexi-last? elem3)))
      
      ;; Test element replacement
      (define elem4 (instantiate::<test-element> (content "replacement") (index 3) (chain chain)))
      (flexi-set! chain 1 elem4)
      (test-equal "elem4 rank after set" 1 (rank elem4))
      (test-equal "elem1 next is now elem4" elem4 (flexi-next elem1))))

(test-end "flexichain-test")
