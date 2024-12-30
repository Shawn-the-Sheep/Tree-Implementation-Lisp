(defun hparent (i)
  ;gives the index of the parent
  (floor (- i 1) 2))

(defun hrt (i)
  ;gives the index of the right child of the node at index i
  (* (+ i 1) 2))

(defun hlt (i)
  ;gives the index of the left child of the node at index i
  (- (hrt i) 1))

(defun heap-up (vec i)
  ;note that this function is for max-heaps
  (when (and (> i 0) (> (aref vec i) (aref vec (hparent i))))
    (rotatef (aref vec i) (aref vec (hparent i)))
    (heap-up vec (hparent i)))
  vec)

(defun heap-push (node vec)
  (vector-push-extend node vec)
  (heap-up vec (1- (length vec))))

(defun heap-down (vec beg &optional (end (length vec)))
  (let ((l (hlt beg))
        (r (hrt beg)))
    (when (< l end)
      (let ((child (if (or (>= r end) (> (aref vec l) (aref vec r)))
                       l
                       r)))
        (when (< (aref vec beg) (aref vec child))
          (rotatef (aref vec beg) (aref vec child))
          (heap-down vec child end)))))
  vec)

(defun heap-pop (vec)
  (rotatef (aref vec 0) (aref vec (- (length vec) 1)))
  (prog1
      (vector-pop vec)
      (heap-down vec 0)))

(defun heapify (vec)
  (let ((mid (floor (length vec) 2))) ;mid is a leaf node
    (dotimes (i mid)
      (heap-down vec (- mid i 1))))
  vec)

(defun heap-sort (vec)
  (heapify vec)
  (dotimes (i (1- (length vec)) vec)
    (let ((currNode (- (length vec) i 1)))
      (rotatef (aref vec 0) (aref vec currNode))
      (heap-down vec 0 currNode))))