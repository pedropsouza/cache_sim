(defstruct cache-sim
  addr-bits
  cache-bits
  mem-cells
  cache-cells
  (op-count 0)
  (hit-count 0)
  )

(defun new-cache-sim (&key addr-bits (cache-bits (floor (/ addr-bits 2))))
  (make-cache-sim 
    :addr-bits addr-bits
    :cache-bits cache-bits
    :mem-cells (let ((size (expt 2 addr-bits)))
                 (make-array
                   size
                   :initial-contents (loop for i from 0 below size collect i)
                   )
                 )
    :cache-cells (let ((size (expt 2 cache-bits)))
                   (make-array
                     size
                     :initial-element nil
                     )
                   )
    )
  )

(defmethod cache-sim-cachemask ((sim cache-sim))
  (1- (ash 1 (cache-sim-cache-bits sim)))
  )

(defmethod cache-sim-lowaddr ((sim cache-sim) (address integer))
  (logand (cache-sim-cachemask sim) address)
  )

(defmethod cache-sim-tag ((sim cache-sim) (address integer))
  (logand (lognot (cache-sim-cachemask sim)) address)
  )

(defmethod cache-sim-query-cache ((sim cache-sim) (address integer))
  (let* ((cache-cells (cache-sim-cache-cells sim))
         (cell (aref cache-cells (cache-sim-lowaddr sim address)))
         )
    (and
      (equal (car cell) (cache-sim-tag sim address))
      (cdr cell)
      )
    )
  )

(defmethod cache-sim-access ((sim cache-sim) (address integer))
  (progn 
    (incf (cache-sim-op-count sim))
    (let ((hit (cache-sim-query-cache sim address)))
      (if hit
        (progn
          (incf (cache-sim-hit-count sim))
          (list 'hit hit)
          )
        (let ((cell (aref (cache-sim-mem-cells sim) address)))
          (setf
            (aref (cache-sim-cache-cells sim) (cache-sim-lowaddr sim address))
            (cons (cache-sim-tag sim address) cell)
            )
          (list 'miss cell)
          )
        )
      )
    )
  )

(defmethod test-op-eqv ((sim cache-sim))
  (map 'list
    (lambda (x)
      (let ((cache-size (length (cache-sim-cache-cells sim))))
        (let ((cache-mask (1- cache-size)))
          (list x (mod x cache-size) (logand x cache-mask))
          )
        )
      )
    (cache-sim-mem-cells sim)
    )
  )
