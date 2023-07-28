(defun match(pattern assertion)
  (setq q '?)
  (setq ex '!)
  (cond
    ( 
      (or (null pattern) (null assertion)) 
      (equal pattern assertion)
    )
    (
      (equal (car pattern) q) 
      (match (cdr pattern) (cdr assertion))
    )
    (
      (and (equal (car pattern) ex) (match (cdr pattern) (cdr assertion)))
      T
    )
    (
      (equal (car pattern) ex) 
      (match pattern (cdr assertion))
    )
    (
      (and (equal (car pattern) (car assertion)) (match (cdr pattern) (cdr assertion)))
    )
  )
)