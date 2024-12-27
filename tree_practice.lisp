(defstruct (binary-tree-node (:conc-name NIL))
  key
  (lc NIL)
  (rc NIL))

(defun insert-left (node item)
  (if (null (lc node))
      (setf (lc node) (make-binary-tree-node :key item))
      (setf (lc node) (make-binary-tree-node :key item :lc (lc node)))))

(defun insert-right (node item)
  (if (null (rc node))
      (setf (rc node) (make-binary-tree-node :key item))
      (setf (rc node) (make-binary-tree-node :key item :rc (rc node)))))

(defun sumEven (node)
  (cond
    ((null node) 0)
    ((evenp (key node)) (+ (key node) (sumEven (lc node)) (sumEven (rc node))))
    (t (+ (sumEven (lc node)) (sumEven (rc node))))))

(defun bst-insert (item node)
  (if (null node)
      (make-binary-tree-node :key item)
      (let ((root (key node))
            (left (lc node))
            (right (rc node)))
        (cond
          ((< item root) (setf (lc node) (bst-insert item left)))
          ((> item root) (setf (rc node) (bst-insert item right)))
          (t node))
        node)))

(defun bst-find (item node)
  (when node
    (cond
      ((< item (key node)) (bst-find item (lc node)))
      ((> item (key node)) (bst-find item (rc node)))
      (t node))))

(defun path-to-item (item node &optional (list_acc NIL))
  (when node
    (cond
      ((< item (key node)) (path-to-item item (lc node) (append list_acc (list (key node)))))
      ((> item (key node)) (path-to-item item (rc node) (append list_acc (list (key node)))))
      (t (append list_acc (list (key node)))))))

(defun bst-min (node)
  (if (null (lc node))
      (key node)
      (bst-min (lc node))))

(defun bst-max (node)
  (if (null (rc node))
      (key node)
      (bst-max (rc node))))

(defun bst-min-node (node)
  (and node
       (or (bst-min-node (lc node)) node)))

(defun bst-max-node (node)
  (and node
       (or (bst-max-node (rc node)) node)))

(defun tree-rotate (node parent grandparent)
  ;we need two cond forms. The first 1 serves the purpose of altering the node to the correct form
  (cond
    ((eql node (lc parent)) (setf (lc parent) (rc node) (rc node) parent))
    ((eql node (rc parent)) (setf (rc parent) (lc node) (lc node) parent))
    (t (error "The Node provided is not a child of the parent")))
  ;the second one serves the purpose of altering the grandparent (the overall tree) to have the final rotated form
  (cond
    ((null grandparent) node)
    ((eql parent (lc grandparent)) (setf (lc grandparent) node))
    ((eql parent (rc grandparent)) (setf (rc grandparent) node))
    (t (error "The parent provided is not a child of the grandparent"))))

(defun node-chain (item root &optional chain)
  (if root
      (let ((key (key root))
            (lc (lc root))
            (rc (rc root))
            (chain (cons root chain)))
        (cond
          ((< item key) (node-chain item lc chain))
          ((> item key) (node-chain item rc chain))
          (t (values root chain))))
      (values nil chain)))

(defun bst-splay (node chain)
  (loop :for (parent grandparent) :on chain :do
       (tree-rotate node parent grandparent))
  node)

(defun st-search (item root)
  (multiple-value-bind (node chain) (node-chain item root)
    (when node
      (bst-splay node (rest chain)))))

(defun st-insert (obj root)
  (if (null root)
      (make-binary-tree-node :key obj)
      (multiple-value-bind (node chain) (node-chain obj root)
        (unless node
          (let ((parent (first chain))
                (new (make-binary-tree-node :key obj)))
            (if (< obj (key parent))
                (setf (lc parent) new)
                (setf (rc parent) new))
            (bst-splay new chain))))))

(defstruct (general-tree-node (:conc-name NIL))
  key
  (children NIl))

(defun search-tree (item node)
  (when node
    (if (equal item (key node))
        item
        (dolist (child (children node))
          (let ((res (search-tree item child)))
            (when res
              (return res)))))))

(defun count-elements (node)
  (if (null node)
      0
      (let ((acc 1))
        (dolist (p (children node) acc)
          (setf acc (+ (count-elements p) acc))))))