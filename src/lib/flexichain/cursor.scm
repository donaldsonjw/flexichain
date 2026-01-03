(module flexichain.cursor
   (export
      (abstract-class <cursor>)
      (generic cursor-clone cursor::<cursor>)
      (generic cursor-pos::long cursor::<cursor>)
      (generic cursor-pos-set! cursor::<cursor> pos::long)
      (generic cursor-at-beginning? cursor::<cursor>)
      (generic cursor-at-end? cursor::<cursor>)
      (generic cursor-move> cursor::<cursor> n::long)
      (generic cursor-move< cursor::<cursor> n::long)
      (generic cursor-insert! cursor::<cursor> itm::obj)
      (generic cursor-delete<! cursor::<cursor> n::long)
      (generic cursor-delete>! cursor::<cursor> n::long)
      (generic cursor-ref< cursor::<cursor>)
      (generic cursor-set<! cursor::<cursor> item::obj)
      (generic cursor-ref> cursor::<cursor>)
      (generic cursor-set>! cursor::<cursor> item::obj)))

;; Creates a cursor that is initially at the same location
;; as the one given as argument.
(define-generic (cursor-clone cursor::<cursor>))

;; Returns the position of the cursor.
(define-generic (cursor-pos::long cursor::<cursor>))

;; Set the position of the cursor. 
(define-generic (cursor-pos-set! cursor::<cursor> pos::long))

;; Returns true if the cursor is at the beginning. 
(define-generic (cursor-at-beginning? cursor::<cursor>))

;; Returns true if the cursor is at the end.
(define-generic (cursor-at-end? cursor::<cursor>))

;; Moves cursor forward N positions.
(define-generic (cursor-move> cursor::<cursor> n::long))

;; Moves cursor backward N positions.
(define-generic (cursor-move< cursor::<cursor> n::long))

;; Inserts item at the cursor.
(define-generic (cursor-insert! cursor::<cursor> itm::obj))

;; Deletes N objects before the cursor.
(define-generic (cursor-delete<! cursor::<cursor> n::long))

;; Deletes N objects after the cursor.
(define-generic (cursor-delete>! cursor::<cursor> n::long))

;; Returns the item immediately before the cursor 
(define-generic (cursor-ref< cursor::<cursor>))

;; Replaces the item immediately before the cursor
(define-generic (cursor-set<! cursor::<cursor> item::obj))

;; Returns the item immediately after the cursor 
(define-generic (cursor-ref> cursor::<cursor>))

;; Replaces the item immediately after the cursor
(define-generic (cursor-set>! cursor::<cursor> item::obj))

