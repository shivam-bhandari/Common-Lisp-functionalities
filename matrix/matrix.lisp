
(defun matrix-add (mat1 mat2)
  (if (null mat1)
      '()
      (cons 
          ; Adding row-wise
          (add-row (car mat1) (car mat2))
          (matrix-add (cdr mat1) (cdr mat2))
      )
  )
)

(defun add-row (row1 row2)
  (if (null row1)
      '()
      (cons (+ (car row1) (car row2))
            (add-row (cdr row1) (cdr row2))
      )
  )
)

(defun matrix-transpose (mat)
  (cond ((null (car mat)) '())
        ((cons 
            (mapcar #'car mat)
            (matrix-transpose (mapcar #'cdr mat)))
        )
  )
)

(defun dot-product (a b)
  (cond ((or (null a) (null b)) 0)
        (t (+ (* (car a) (car b)) (dot-product (cdr a) (cdr b))))
  )
)

(defun matrix-multiply (mat1 mat2)
  (cond ((null mat1) nil)
        (
          (cons 
          (mapcar #'(lambda (row2) (dot-product (car mat1) row2))(matrix-transpose mat2)
          )
          (matrix-multiply (cdr mat1) mat2)
          )
        )
  )
)


