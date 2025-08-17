(defstruct cache-sim
  addr-bits
  cache-bits
  assoc-bits
  mem-cells
  lines ;; has (expt 2 assoc-bits) cache cells per line
  (op-count 0)
  (hit-count 0)
  (tick 0)
  )

(defun new-cache-sim (
        &key addr-bits
        (cache-bits (floor (/ addr-bits 2)))
        (assoc-bits 0)
      )
  (make-cache-sim 
    :addr-bits addr-bits
    :cache-bits cache-bits
    :assoc-bits assoc-bits
    :mem-cells
    (let ((size (expt 2 addr-bits)))
      (make-array
        size
        :initial-contents
        (loop for i from 0 below size collect i)
        )
      )
    :lines
    (let ((size (expt 2 (- cache-bits assoc-bits)))
          (block-size (expt 2 assoc-bits)))
      (make-array
        size
        :initial-contents
        (loop for i from 0 below size collect
          (make-array
            block-size
            :initial-contents
            (loop for i from 0 below block-size collect (list nil 0 nil))
            )
          )
        )
      )
    )
  )

(defmethod cache-sim-cachemask ((sim cache-sim))
  "bitmask for normalizing to cache address space"
  (1- (ash 1 (cache-sim-cache-bits sim)))
  )

(defmethod cache-sim-lowaddr ((sim cache-sim) (address integer))
  "normalize phys address to cache address"
  (logand (cache-sim-cachemask sim) address)
  )

(defmethod cache-sim-index-num ((sim cache-sim) (address integer))
  "get associated line number for given phys address"
  (mod address
    (expt 2
      (- (cache-sim-cache-bits sim)
         (cache-sim-assoc-bits sim)
         )
      )
    )
  )
(defmethod cache-sim-tag ((sim cache-sim) (address integer))
  (logand (lognot (cache-sim-cachemask sim)) address)
  )

(defmethod cache-sim-get-line ((sim cache-sim) (address integer))
  (let ((lines (cache-sim-lines sim)))
    (aref lines (cache-sim-index-num sim address))
    )
  )
(defmethod cache-sim-query-cache ((sim cache-sim) (address integer))
  (find-if
    (lambda (c) (equal (car c) (cache-sim-tag sim address)))
    (cache-sim-get-line sim address)
    )
  )
(defmethod cache-sim-lru-in-block ((sim cache-sim) (address integer))
  (aref (sort (cache-sim-get-line sim address)
              #'<
              :key (lambda (c) (cadr c))
          )
        0
        )
  )

(defmethod cache-sim-access ((sim cache-sim) (address integer))
  (progn 
    (incf (cache-sim-op-count sim))
    (let ((hit (cache-sim-query-cache sim address)))
      (if hit
        (progn
          (incf (cache-sim-hit-count sim))
          (setf (cadr hit) (cache-sim-tick sim)) ;; set access tick
          (list 'hit hit)
          )
        (let ((mem-cell (aref (cache-sim-mem-cells sim) address))
              (cache-cell (cache-sim-lru-in-block sim address)))
          (format t "~&miss is writing ~a to ~a" mem-cell cache-cell)
          (setf (car cache-cell) (cache-sim-tag sim address))
          (setf (cadr cache-cell) (cache-sim-tick sim))
          (setf (caddr cache-cell) mem-cell)
          (list 'miss mem-cell)
          )
        )
      )
    )
  )
