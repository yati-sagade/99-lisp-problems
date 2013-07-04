;; Utility functions
(defun my-last-n (n elems)
  "Get the last n elements of a list"
  (if (<= (length elems) n)
      elems
      (loop for i from (- (length elems) n) to (1- (length elems))
	 collect (elt elems i))))

;; 1
(defun my-last (elems)
  "Find the last box of a list"
  (my-last-n (1 elems)))

;; 2
(defun my-but-last (elems)
  "Find the last but one box of a list"
  (my-last-n (2 elems)))


;; 3
(defun element-at (elems idx)
  "Find the element at index idx in elems"
  (if (>= idx (length elems))
      nil
      (do* ((i 0 (1+ i))
	    (rst elems (rest rst))
	    (fst (first elems) (first rst)))
	   ((>= i idx) fst))))


;; 4
(defun my-length (elems)
  "Find the length of a list"
  (labels ((my-length-aux (elems acc)
	     (if (null elems)
		 acc
		 (my-length-aux (rest elems) (1+ acc)))))
    (my-length-aux elems 0)))

;; 5
(defun my-reverse (elems)
  "Reverse a list"
  (labels ((my-reverse-aux (elems result)
	     (if (null elems)
		 result
		 (my-reverse-aux (rest elems) (cons (first elems) result)))))
    (my-reverse-aux elems nil)))

;; 6
(defun is-palindrome (elems)
  "Find out if a list is a palindrome"
  (if (null elems) T
      (do ((ret T)
	   (left 0 (1+ left))
	   (right (1- (length elems)) (1- right)))
	  ((not (eql (elt elems left) (elt elems right))) nil)
	(if (>= left right) (return T)))))

