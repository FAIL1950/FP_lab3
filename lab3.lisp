(defun insert (x sorted-list)
  (cond ((null sorted-list) (list x))
        ((<= x (car sorted-list)) (cons x sorted-list))
        (t (cons (car sorted-list) (insert x (cdr sorted-list))))))

(defun insertion-sort1 (lst)
  (when lst
      (insert (car lst) (insertion-sort1 (cdr lst)))))


(defun insertion-sort2 (lst)
  (when lst
    (let ((sorted-list (copy-list lst)) (B))
      (loop for i from 1 below (length sorted-list)
            do (setq B (nth i sorted-list))
               (let ((j 0))
                 (do ()
                     ((<= B (nth j sorted-list)))
                   (incf j))


                 (loop for k from (- i 1) downto j
                       do (rotatef (nth (+ k 1) sorted-list) (nth k sorted-list))))

            )
      
      sorted-list)
   )
  )

(defun check-function (name func args expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (apply func args) expected)
          name))

(defun test-insertion-sort1 ()
  (check-function "Test1" 'insertion-sort1 '(()) nil)
  (check-function "Test2" 'insertion-sort1 '((4 3 5 1 2)) '(1 2 3 4 5))
  (check-function "Test3" 'insertion-sort1 '((10 3 5 9 2)) '(2 3 5 9 10)))


(defun test-insertion-sort2 ()
  (check-function "Test1" 'insertion-sort2 '(()) nil)
  (check-function "Test2" 'insertion-sort2 '((4 3 5 1 2)) '(1 2 3 4 5))
  (check-function "Test3" 'insertion-sort2 '((10 3 5 9 2)) '(2 3 5 9 10)))

