;;; Circular gapbuffer implementation
;;; Bigloo port
;;; Copyright (C) 2024 Joseph Donaldson (donaldsonjw@gmail.com)
;;;
;;; Original Common Lisp implementation 
;;; Flexichain -- https://github.com/robert-strandh/Flexichain
;;; Copyright (C) 2003-2004  Robert Strandh (strandh@labri.fr)
;;; Copyright (C) 2003-2004  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(module flexichain.gapbuffer
   (import flexichain.flexichain
           flexichain.exceptions)
   
   (export
      (class <gapbuffer>::<flexichain>
         (fill-element (default #\u))
         (expand-factor::double (default 1.5))
         (min-capacity::long (default 5))
         buffer
         gap-start::long
         gap-end::long
         data-start::long)
      
      (make-gapbuffer #!key (initial-capacity 0) (fill #\_)
         (min-capacity 5) (expand-factor 1.5))
      (vector->gapbuffer vec::vector)
      (list->gapbuffer lst::pair-nil)
      (gapbuffer . items)
      (gapbuffer? obj)
      (gapbuffer-print-rep gb)
      (generic move-elements chain::<flexichain> to::vector from::vector
                   start1::long start2::long end2::long)
      (calculate-space::long expand-factor::double min-capacity::long n::long)
      (index-position chain::<gapbuffer> index::long))

   (option
      (set! *arithmetic-genericity* #t)
      (set! *arithmetic-overflow* #f)))

;; Return whether an OBJ is a gapbuffer.
(define (gapbuffer? obj)
   (isa? obj <gapbuffer>))

;; calculate the needed space given EXPAND-FACTOR, MIN-CAPACITY, and N
;; items
(define (calculate-space::long expand-factor::double min-capacity::long n::long)
   (+ 2 (max (flonum->fixnum (ceiling (* n expand-factor))) min-capacity)))

;; Construct a new gapbuffer 
(define (make-gapbuffer #!key (initial-capacity 0) (fill #\_)
           (min-capacity 5) (expand-factor 1.5))
   (when (<= expand-factor 1.0)
      (raise-flexi-initialization-error :proc "make-gapbuffer" 
                                        :cause "expand-factor must be > 1.0"))
   (when (<= min-capacity 0)
      (raise-flexi-initialization-error :proc "make-gapbuffer" 
                                        :cause "min-capacity must be > 0"))
   (when (< initial-capacity 0)
      (raise-flexi-initialization-error :proc "make-gapbuffer" 
                                        :cause "initial-capacity must be >= 0"))
   (let* ((capacity (calculate-space expand-factor min-capacity initial-capacity))
          (buffer (make-vector capacity fill)))
      (instantiate::<gapbuffer> (fill-element fill)
                                (expand-factor expand-factor)
                                (min-capacity min-capacity)
                                (buffer buffer)
                                (gap-start 2)
                                (gap-end 0)
                                (data-start 1))))

;; Create a gapbuffer from vector VEC 
(define (vector->gapbuffer vec::vector)
   (let* ((size (vector-length vec))
          (gapbuffer (make-gapbuffer :initial-capacity size)))
      (flexi-insert-vector! gapbuffer 0 vec)
      gapbuffer))

;; Create a gapbuffer from list LST 
(define (list->gapbuffer lst::pair-nil)
   (let* ((size (length lst))
          (gapbuffer (make-gapbuffer :initial-capacity size)))
      (for-each (lambda (item) (flexi-push-end! gapbuffer item)) lst)
      gapbuffer))

;; Create a gapbuffer from the ITEMS items 
(define (gapbuffer . items)
   (list->gapbuffer items))

(define-expander inc!
   (lambda (x e)
      (match-case x
         ((?- ?p)
          (e `(begin (set! ,p (+ ,p 1)) ,p) e))
         ((?- ?p ?v)
          (e `(begin (set! ,p (+ ,p ,v)) ,p) e))
         (else
          (error "inc!" "invalid form" x)))))

(define-expander dec!
   (lambda (x e)
      (match-case x
         ((?- ?p)
          (e `(begin (set! ,p (- ,p 1)) ,p) e))
         ((?- ?p ?v)
          (e `(begin (set! ,p (- ,p ,v)) ,p) e))
         (else
          (error "dec!" "invalid form" x)))))

(define-expander with-virtual-gap
   (lambda (x e)
      (match-case x
            ((?- (?bl ?ds ?gs ?ge) ?chain . ?body)
             (e (let ((c (gensym)))
                   `(let* ((,(symbol-append c '::<gapbuffer>) ,chain)
                           (,bl (vector-length (-> ,c buffer)))
                           (,ds (-> ,c data-start))
                           (,gs (-> ,c gap-start))
                           (,ge (-> ,c gap-end)))
                       (when (< ,gs ,ds) (inc! ,gs ,bl))
                       (when (< ,ge ,ds) (inc! ,ge ,bl))
                       ,@body)) e))
         (else
          (error "with-virtual-gap" "invalid form" x)))))

(define-method (flexi-length chain::<gapbuffer>)
   (with-virtual-gap (bl ds gs ge) chain
      (- bl (- ge gs) 2)))

(define-method (flexi-empty? chain::<gapbuffer>)
   (zero? (flexi-length chain)))

;; Returns the position corresponding to the INDEX in the CHAIN.
(define (index-position chain::<gapbuffer> index::long)
   (with-virtual-gap (bl ds gs ge) chain
      (when (< index ds)
         (inc! index bl))
      (when (>= index ge)
         (dec! index (- ge gs)))
      (- index ds 1)))

(define-method (flexi-position-index chain::<flexichain> position::long)
   (position-index chain position))

;; Returns the (0 indexed) index of the POSITION-th item
;; of the CHAIN in the buffer.
(define (position-index chain::<gapbuffer> position::long)
   (with-virtual-gap (bl ds gs ge) chain
      (let ((index (+ ds position 1)))
         (when (>= index gs)
            (inc! index (- ge gs)))
         (when (>= index bl)
            (dec! index bl))
         index)))

(define (ensure-gap-position chain::<gapbuffer> position::long)
   (move-gap chain (position-index chain position)))

(define (ensure-room chain::<gapbuffer> n::long)
   (when (> n (- (vector-length (-> chain buffer)) 2))
      (increase-buffer-size chain n)))

(define (1+ v) (+ v 1))

(define (1- v) (- v 1))

(define-method (flexi-insert! chain::<gapbuffer> position::long item::obj)
   (unless (<= 0 position (flexi-length chain))
      (flexi-position-error "flexi-insert!" "invalid position" chain position))
   (ensure-gap-position chain position)
   (ensure-room chain (1+ (flexi-length chain)))
   (vector-set! (-> chain buffer) (-> chain gap-start) item)
   (set! (-> chain gap-start) (+ (-> chain gap-start) 1))
   (when (= (-> chain gap-start) (vector-length (-> chain buffer)))
      (set! (-> chain gap-start) 0)))

(define-method (flexi-insert-vector! chain::<gapbuffer> position::long vec::vector)
   (unless (<= 0 position (flexi-length chain))
      (flexi-position-error "flexi-insert-vector!" "invalid position" chain position))
   (ensure-gap-position chain position)
   (ensure-room chain (+ (flexi-length chain) (vector-length vec)))
   (do ((i 0 (+ i 1)))
       ((= i (vector-length vec)))
       (vector-set! (-> chain buffer) (-> chain gap-start) (vector-ref vec i))
       (set! (-> chain gap-start) (+ (-> chain gap-start) 1))
       (when (= (-> chain gap-start) (vector-length (-> chain buffer)))
          (set! (-> chain gap-start) 0))))     

(define-method (flexi-delete! chain::<gapbuffer> position::long)
   (unless (<= 0 position (flexi-length chain))
      (flexi-position-error "flexi-delete!" "invalid position" chain position))
   (ensure-gap-position chain position)
   (vector-set! (-> chain buffer) (-> chain gap-end) (-> chain fill-element))
   (set! (-> chain gap-end) (+ (-> chain gap-end) 1))
   (when (= (-> chain gap-end) (vector-length (-> chain buffer)))
      (set! (-> chain gap-end) 0))
   (when (and (> (vector-length (-> chain buffer)) (+ (-> chain min-capacity) 2))
              (< (+ (flexi-length chain) 2)
                 (/ (vector-length (-> chain buffer)) (square (-> chain expand-factor)))))
      (decrease-buffer-size chain)))

(define-inline (square x) (* x x))

(define-method (flexi-delete-items! chain::<gapbuffer> position::long n::long)
   (unless (zero? n)
     (with-access::<gapbuffer> chain (buffer expand-factor min-capacity gap-end data-start)
        (when (negative? n)
           (inc! position n)
           (set! n (* -1 n)))
        (unless (<= 0 (+ position n) (flexi-length chain))
           (flexi-position-error "flexi-delete-items!" "invalid position" chain position))
        (ensure-gap-position chain position)
        ;; Two cases to consider - one where position+n is wholly on
        ;; this side of the gap in buffer, and one where part of it is
        ;; "wrapped around" to the beginning of buffer.
        (cond ((>= (vector-length buffer) (+ gap-end n))
               (fill-gap chain gap-end (+ gap-end n))
               (inc! gap-end n))
              (else (let ((surplus-elements (- n (- (vector-length buffer) gap-end))))
                       (fill-gap chain gap-end (vector-length buffer))
                       (fill-gap chain 0 surplus-elements)
                       (set! gap-end surplus-elements))))
        (when (= gap-end (vector-length buffer))
           (set! gap-end 0))
        (when (and (> (vector-length buffer) (+ min-capacity 2))
                   (< (+ (flexi-length chain) 2)
                      (/ (vector-length buffer) (square expand-factor))))
           (decrease-buffer-size chain)))))

(define-method (flexi-ref chain::<gapbuffer> position::long)
  (with-access::<gapbuffer> chain (buffer)
     (unless (<= 0 position (flexi-length chain))
        (flexi-position-error "flexi-ref" "invalid position" chain position))
     (vector-ref buffer (position-index chain position))))

(define-method (flexi-set! chain::<gapbuffer> position::long item::obj)
  (with-access::<gapbuffer> chain (buffer)
     (unless (<= -1 position (flexi-length chain))
        (flexi-position-error "flexi-ref" "invalid position" chain position))
     (vector-set! buffer (position-index chain position) item)))

(define-method (flexi-push-start! chain::<gapbuffer> item::obj)
  (flexi-insert! chain 0 item))

(define-method (flexi-push-end! chain::<gapbuffer> item::obj)
   (flexi-insert! chain (flexi-length chain) item))

(define-method (flexi-pop-start! chain::<gapbuffer>)
  (let ((res (flexi-ref chain 0)))
     (flexi-delete! chain 0)
     res))

(define-method (flexi-pop-end! chain::<gapbuffer>)
  (let* ((position (1- (flexi-length chain)))
         (res (flexi-ref chain position)))
     (flexi-delete! chain position)
     res))

(define-method (flexi-rotate! chain::<gapbuffer> #!optional (n 1))
   (when (> (flexi-length chain) 1)
      (cond ((positive? n)
             ;; To move element at position n to position 0,
             ;; we need to move n elements from start to end
             (do ((i 0 (+ i 1)))
                 ((= i n))
                 (flexi-push-end! chain (flexi-pop-start! chain))))
            ((negative? n)
             ;; To move element at position 0 to position |n|,
             ;; we need to move |n| elements from end to start
             (do ((i 0 (+ i 1)))
                 ((= i (- n)))
                 (flexi-push-start! chain (flexi-pop-end! chain))))
            (else #f))))

;; Moves the elements and gap inside the buffer so that
;; the element currently at HOT-SPOT becomes the first element following
;; the gap, or does nothing if there are no elements.
(define  (move-gap chain::<gapbuffer> hot-spot::long)
   (with-access::<gapbuffer> chain (gap-start gap-end)
      (unless (= hot-spot gap-end)
         (case (gap-location chain)
            ((:gap-empty) (move-empty-gap chain hot-spot))
            ((:gap-left) (move-left-gap chain hot-spot))
            ((:gap-right) (move-right-gap chain hot-spot))
            ((:gap-middle) (move-middle-gap chain hot-spot))
            ((:gap-non-contiguous) (move-non-contiguous-gap chain hot-spot))))
      (values gap-start gap-end)))

;; Moves the gap. Handles the case where the gap is empty.
(define (move-empty-gap chain::<gapbuffer> hot-spot::long)
   (with-access::<gapbuffer> chain (gap-start gap-end)
      (set! gap-start hot-spot)
      (set! gap-end hot-spot)))

;; Moves the gap. Handles the case where the gap is
;; on the left of the buffer.
(define (move-left-gap chain::<gapbuffer> hot-spot::long)
  (with-access::<gapbuffer> chain (buffer gap-start gap-end data-start)
    (let ((buffer-capacity (vector-length buffer)))
      (cond ((< (- hot-spot gap-end) (- buffer-capacity hot-spot))
             (push-elements-left chain (- hot-spot gap-end)))
            ((<= (- buffer-capacity hot-spot) gap-end)
             (hop-elements-left chain (- buffer-capacity hot-spot)))
            (else
             (hop-elements-left chain (- gap-end gap-start))
             (push-elements-right chain (- gap-start hot-spot)))))))

;; Moves the gap. Handles the case where the gap is
;; on the right of the buffer.
(define (move-right-gap chain::<gapbuffer> hot-spot::long)
   (with-access::<gapbuffer> chain (buffer gap-start gap-end)
      (let ((buffer-capacity (vector-length buffer)))
         (cond ((< (- gap-start hot-spot) hot-spot)
                (push-elements-right chain (- gap-start hot-spot)))
               ((<= hot-spot (- buffer-capacity gap-start))
                (hop-elements-right chain hot-spot))
               (else
                (hop-elements-right chain (- buffer-capacity gap-start))
                (push-elements-left chain (- hot-spot gap-end)))))))

;; Moves the gap. Handles the case where the gap is
;; in the middle of the buffer.
(define (move-middle-gap chain::<gapbuffer> hot-spot::long)
   (with-access::<gapbuffer> chain (buffer gap-start gap-end)
      (let ((buffer-capacity (vector-length buffer)))
         (cond ((< hot-spot gap-start)
                (cond ((<= (- gap-start hot-spot)
                          (+ (- buffer-capacity gap-end) hot-spot))
                       (push-elements-right chain (- gap-start hot-spot)))
                      (else
                    (push-elements-left chain (- buffer-capacity gap-end))
                    (move-right-gap chain hot-spot))))
               (else
                (cond ((< (- hot-spot gap-end)
                          (+ (- buffer-capacity hot-spot) gap-start))
                       (push-elements-left chain (- hot-spot gap-end)))
                      (else
                    (push-elements-right chain gap-start)
                    (move-left-gap chain hot-spot))))))))

;; Moves the gap. Handles the case where the gap is in 2 parts,
;; on both ends of the buffer.
(define (move-non-contiguous-gap chain::<gapbuffer> hot-spot::long)
  (with-access::<gapbuffer> chain (buffer gap-start gap-end)
    (let ((buffer-capacity (vector-length buffer)))
      (cond ((< (- hot-spot gap-end) (- gap-start hot-spot))
             (hop-elements-right chain (min (- buffer-capacity gap-start)
                                           (- hot-spot gap-end)))
             (let ((nb-left (- hot-spot gap-end)))
               (unless (zero? nb-left)
                 (push-elements-left chain nb-left))))
            (else
             (hop-elements-left chain (min gap-end (- gap-start hot-spot)))
             (let ((nb-right (- gap-start hot-spot)))
               (unless (zero? nb-right)
                 (push-elements-right chain nb-right))))))))

;; move elements of a flexichain and adjust data-start
(define-generic (move-elements chain::<flexichain> to::vector from::vector
                   start1::long start2::long end2::long))

(define-method (move-elements chain::<gapbuffer> to::vector from::vector
                  start1::long start2::long end2::long)
   (vector-copy! to start1 from start2 end2) 
   (with-access::<gapbuffer> chain (data-start)
      (when (and (<= start2 data-start) (< data-start end2))
         (inc! data-start (- start1 start2)))))

;; fill part of gap with the fill element
(define-generic (fill-gap chain::<flexichain> start::long end::long))

(define-method (fill-gap chain::<gapbuffer> start::long end::long)
  (with-access::<gapbuffer> chain (buffer fill-element)
     (vector-fill! buffer fill-element start end)))

;; Pushes the COUNT elements of CHAIN at the right of the gap,
;; to the beginning of the gap. The gap must be continuous. Example:
;; PUSH-ELEMENTS-LEFT abcd-----efghijklm 2  => abcdef-----ghijklm
(define (push-elements-left chain::<gapbuffer> count::long)
   (with-access::<gapbuffer> chain (buffer gap-start gap-end)
      (move-elements chain buffer buffer gap-start gap-end (+ gap-end count))
      (fill-gap chain (max gap-end (+ gap-start count)) (+ gap-end count))
      (inc! gap-start count)
      (inc! gap-end count)
      (normalize-indices chain)))

;; Pushes the COUNT elements of CHAIN at the left of the gap,
;; to the end of the gap. The gap must be continuous. Example:
;; PUSH-ELEMENTS-RIGHT abcd-----efghijklm 2  =>  ab-----cdefghijklm
(define (push-elements-right  chain::<gapbuffer> count::long)
   (with-access::<gapbuffer> chain (buffer gap-start gap-end)
      (let* ((buffer-capacity (vector-length buffer))
             (rotated-gap-end (if (zero? gap-end) buffer-capacity gap-end)))
         (move-elements chain buffer buffer
            (- rotated-gap-end count) (- gap-start count) gap-start)
         (fill-gap chain (- gap-start count) (min gap-start (- rotated-gap-end count)))
         (dec! gap-start count)
         (set! gap-end (- rotated-gap-end count))
         (normalize-indices chain))))

;; Moves the COUNT rightmost elements to the end of the gap,
;; on the left of the data. Example:
;; HOP-ELEMENTS-LEFT ---abcdefghijklm--- 2  =>  -lmabcdefghijk-----
(define (hop-elements-left chain::<gapbuffer> count::long)
   (with-access::<gapbuffer> chain (buffer gap-start gap-end)
      (let* ((buffer-capacity (vector-length buffer))
             (rotated-gap-start (if (zero? gap-start) buffer-capacity gap-start)))
         (move-elements chain buffer buffer
            (- gap-end count) (- rotated-gap-start count) rotated-gap-start)
         (fill-gap chain (- rotated-gap-start count) rotated-gap-start)
         (set! gap-start (- rotated-gap-start count))
         (dec! gap-end count)
         (normalize-indices chain))))

;; Moves the COUNT leftmost elements to the beginning of the gap,
;; on the right of the data. Example:
;; HOP-ELEMENTS-RIGHT ---abcdefghijklm--- 2  =>  -----cdefghijklmab-
(define (hop-elements-right chain::<gapbuffer> count::long)
   (with-access::<gapbuffer> chain (buffer gap-start gap-end)
      (move-elements chain buffer buffer gap-start gap-end (+ gap-end count))
      (fill-gap chain gap-end (+ gap-end count))
      (inc! gap-start count)
      (inc! gap-end count)
      (normalize-indices chain)))


(define (increase-buffer-size  chain::<gapbuffer> n::long)
   (resize-buffer chain (calculate-space (-> chain expand-factor) (-> chain min-capacity) n)))

(define (decrease-buffer-size chain::<gapbuffer>)
  (resize-buffer chain (calculate-space (-> chain expand-factor) (-> chain min-capacity)
                          (flexi-length chain))))
; allocate a new buffer with the size indicated
(define-generic (resize-buffer chain::<flexichain> new-buffer-size::long))

(define-method (resize-buffer chain::<gapbuffer> new-buffer-size::long)
  (with-access::<gapbuffer> chain (buffer gap-start gap-end
               fill-element expand-factor)
    (let ((buffer-capacity (vector-length buffer))
          (buffer-after (make-vector new-buffer-size fill-element)))
      (case (gap-location chain)
         ((:gap-empty :gap-middle)
          (move-elements chain buffer-after buffer 0 0 gap-start)
          (let ((gap-end-after (- new-buffer-size (- buffer-capacity gap-end))))
             (move-elements chain buffer-after buffer gap-end-after gap-end buffer-capacity)
             (set! gap-end gap-end-after)))
        ((:gap-right)
         (move-elements chain buffer-after buffer 0 0 gap-start))
        ((:gap-left)
         (let ((gap-end-after (- new-buffer-size (+ 2 (flexi-length chain)))))
            (move-elements chain buffer-after buffer gap-end-after gap-end buffer-capacity)
            (set! gap-end gap-end-after)))
        ((:gap-non-contiguous)
         (move-elements chain buffer-after buffer 0 gap-end gap-start)
         (dec! gap-start gap-end)
         (set! gap-end 0)))
      (set! buffer buffer-after)))
  (normalize-indices chain))

;; Sets gap limits to 0 if they are at the end of the buffer.
(define (normalize-indices chain::<gapbuffer>)
   (with-access::<gapbuffer> chain (buffer gap-start gap-end data-start)
      (let ((buffer-capacity (vector-length buffer)))
         (when (>= data-start buffer-capacity)
            (set! data-start 0))
         (when (>= gap-start buffer-capacity)
            (set! gap-start 0))
         (when (>= gap-end buffer-capacity)
            (set! gap-end 0)))))

;; Returns a keyword indicating the general location of the gap.
(define (gap-location chain::<gapbuffer>)
  (with-access::<gapbuffer> chain (buffer gap-start gap-end)
     (cond ((= gap-start gap-end) :gap-empty)
           ((and (zero? gap-start) (>= gap-end 0)) :gap-left)
           ((and (zero? gap-end) (> gap-start 0)) :gap-right)
           ((> gap-end gap-start) :gap-middle)
           (else :gap-non-contiguous))))


(define-expander dotimes
   (lambda (x e)
      (match-case x
         ((?- (?i (and ?count)) . ?body)
          (let ((l (gensym 'loop)))
             (e `(let ,l ((,i 0))
                      (if (< ,i ,count)
                          (begin ,@body  (,l (+ ,i 1))))
                      #unspecified) e)))
         (else
          (error "dotimes" "invalid form" x)))))

(define (box str #!optional (start #t) (end #t))
   (let ((len (string-length str)))
      (with-output-to-string
         (lambda ()
            (when start
               (printf "┏"))
            (dotimes (n len)
               (printf "━"))
            (when end (printf "┓"))
            (newline)
            (when start
               (printf "┃"))
            (printf "~a" str)
            (when end
               (printf "┃"))
            (newline)
            (when start
               (printf "┗"))
            (dotimes (n len)
               (printf "━"))
            (when end
               (printf "┛"))))))

;; print internal storage vector
(define (gapbuffer-print-rep gb)

   (define (vector-fold-left f seed vec)
      (let loop ((i 0)
                 (s seed))
         (if (= i (vector-length vec))
             s
             (loop (+ i 1)
                (f s (vector-ref vec i))))))
   
   (define (->vector-of-str v)
      (vector-map (lambda (item)
                     (format "~a" item)) v))

   (define (max-width vos)
      (vector-fold-left (lambda (s v)
                           (max s (string-length v)))
         0
         vos))
   (define (max-width-of-number n)
      (let loop ((n n)
                 (i 1))
         (if (= (/fx n 10) 0)
             i
             (loop (/fx n 10)
                (+ i 1)))))  
   (define (print-box-top width)
      (if (<= width 2)
          (error " print-box-top" "invalid width; width must be greater than or equal to 2"
             width)
          (begin
             (printf "~a" "┏")
             
             (do ((i 0 (+ i 1)))
                 ((= i width))
                 (printf "~a" "━")
                 (when (and (> i 0)
                            (< i width))
                    (printf "~a" "┳")))
             
             (printf "~a" "┓"))))
   
   (let* ((gb::<gapbuffer> gb)
          (vos (->vector-of-str (-> gb buffer)))
          (mw (max (+ 1 (max-width vos))
                 (+ 1 (max-width-of-number (- (vector-length (-> gb buffer)) 1))))))

      (newline )
      (printf "#fc(")
      (vector-for-each (lambda (itm)
                          (printf "~a~a" itm (make-string (- mw (string-length itm)) #\space)))
         vos)
      (print ")")
      (printf "----")
      (do ((i 0 (+ i 1)))
          ((= i (vector-length vos)))
          (printf "~a~a"
             (cond ((= i (-> gb data-start))
                       #\d )
                    ((= i (-> gb gap-start))
                       #\s)
                    ((= i (-> gb gap-end))
                     #\e)
                    (else
                     #\space))
             (make-string (- mw 1) #\space)))
      (newline)
      (printf "i---")
      
      (do ((i 0 (+ i 1)))
          ((= i (vector-length vos)))
          
          (let ((ns (number->string i)))
             (printf "~a~a" (number->string i)
                (make-string (- mw (max-width-of-number i)) #\space))))
      (newline)))