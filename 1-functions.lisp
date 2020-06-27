(defun take-n (lst n)
  (cond ((< n 1) nil)
        ((> n (list-length lst)) lst)
        ((null lst) '())
        ((> n 0) (cons (car lst) (take-n (cdr lst) (- n 1)) )))) 

(defun take-n-deep (lst n)
  (take-n lst n)) 

(defun cut-in-half (lst)
  (cond ((null lst) '())
        ((= 1 (list-length lst)) (list (car lst)))
        ((evenp (list-length lst)) (let ((x (/ (list-length lst) 2 ))) (cut lst x)))
        ((oddp (list-length lst)) (let ((x (/ (+ (list-length lst) 1) 2 ))) (cut lst x)))))

(defun cut (lst x)
  (list (take-n-deep lst x) (subseq lst x (list-length lst)))) 

(defun make-tree (lst)
  (cond ((null lst) '())
        ((= 1 (list-length lst)) (list lst))
        ((> (list-length lst) 2) (cons (cut-in-half (subseq lst 0 2)) (make-tree (subseq lst 2 (list-length lst)))))
        ((= (list-length lst) 2) (list (cut-in-half (subseq lst 0 2)))))) 

(defun tree-height (lst)
  (cond ((null lst) nil)
        ((is-leaf-node lst) 1)
        ((are-siblings lst) (+ (tree-height (car lst)) (tree-height (cdr lst)))))) 

(defun are-siblings (lst)
  (if (= (list-length lst) 2)
      t
      nil)) 

(defun is-leaf-node (lst)
  (if (= (list-length lst) 1)
      t
      nil))

;; (take-n '(1 2 3) 2)
;; (take-n '(1 2 3 4 5 6 7) 5)
;; (take-n-deep '((1 2) 3) 2)
;; (cut-in-half '())
;; (cut-in-half '(1))
;; (cut-in-half '(1 2))
;; (cut-in-half '(1 2 3))
;; (cut-in-half '(1 2 3 4))
;; (cut-in-half '(1 2 3 4 5))
;; (cut-in-half '(1 2 3 4 5 6))
;; (make-tree '(1))
;; (make-tree '(1 2 3))
;; (make-tree '(1 2 3 4))
;; (tree-height '())
;; (tree-height '(1))
;; (tree-height '((2) (3)))
;; (tree-height (make-tree '(1 2 3)))
;; (tree-height (make-tree '(1 2 3 4)))