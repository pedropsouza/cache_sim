(in-package #:cache-sim)

(defstruct cache-sim
  addr-bits
  cache-bits
  assoc-bits
  block-bits
  lines ;; has (expt 2 assoc-bits) cache cells per line
  (op-count 0)
  (hit-count 0)
  (tick 0)
  )

(defun new-cache-sim (
        &key addr-bits
        (cache-bits (floor (/ addr-bits 2)))
        (assoc-bits 0)
        (block-bits 0)
      )
  (make-cache-sim 
    :addr-bits addr-bits
    :cache-bits cache-bits
    :assoc-bits assoc-bits
    :block-bits block-bits
    :lines
    (let ((size (expt 2 (- cache-bits assoc-bits block-bits)))
          (line-element-count (expt 2 assoc-bits)))
      (make-array
        size
        :initial-contents
        (loop for i from 0 below size collect
          (make-array
            line-element-count
            :initial-contents
            (loop for i from 0 below line-element-count collect
                  (list nil 0 nil)); don't use quoting here!
                                   ; quoting reuses earlier symbols
            ))))))

(defun bitmask (n)
  "returns a bitmask with lowest n bits set"
  (1- (ash 1 n)))

(defmethod cache-sim-lowaddr ((sim cache-sim) (address integer))
  "normalize phys address to cache address"
  (logand (bitmask (cache-sim-cache-bits sim)) address))

(defmethod cache-sim-address-offset ((sim cache-sim) (address integer))
  "extract offset from an address"
  (logand (1- (ash 1 (cache-sim-block-bits sim))) address))

(defmethod cache-sim-index-num ((sim cache-sim) (address integer))
  "get associated line number for given phys address"
  (mod (ash address (- (cache-sim-block-bits sim)))
    (expt 2
      (- (cache-sim-cache-bits sim)
         (cache-sim-assoc-bits sim)
         (cache-sim-block-bits sim)
         ))))

(defmethod cache-sim-tag ((sim cache-sim) (address integer))
  "get tag part of address"
  (logand (lognot (bitmask (cache-sim-block-bits sim))) address))

(defmethod cache-sim-get-line ((sim cache-sim) (address integer))
  "get the mapped cache line for a given memory address"
  (let ((lines (cache-sim-lines sim)))
    (aref lines (cache-sim-index-num sim address))
    )
  )
(defmethod cache-sim-query-cache ((sim cache-sim) (address integer))
  "try to recover cached data for a given address (nil on miss, block on hit)"
  (flet ((tag-match (cblock)
            (equal (car cblock) (cache-sim-tag sim address))))
    (let ((hit (find-if #'tag-match (cache-sim-get-line sim address))))
      hit)))

(defmethod cache-sim-lru-in-line ((sim cache-sim) (address integer))
  "get the least recently used block in the mapped cache line for a given address"
  (aref (sort (copy-seq (cache-sim-get-line sim address)) #'< :key #'cadr) 0))

(defmethod cache-sim-access ((sim cache-sim) (address integer))
  "public interface for the cache simulation"
  (progn 
    (incf (cache-sim-op-count sim))
    (incf (cache-sim-tick sim))
    (let ((hit (cache-sim-query-cache sim address)))
      (if hit
        (progn
          (incf (cache-sim-hit-count sim))
          (setf (cadr hit) (cache-sim-tick sim)) ;; set access tick
          (values 'hit (nth (cache-sim-address-offset sim address) (caddr hit)) hit nil))
        ; else
        (let* ((cache-block (cache-sim-lru-in-line sim address))
               (cache-block-backup (copy-seq cache-block))
               (line-address
                 (logxor (cache-sim-address-offset sim address) address)))
          ; update the block with the miss data
          (setf (car cache-block) (cache-sim-tag sim address))
          (setf (cadr cache-block) (cache-sim-tick sim))
          (values 'miss cache-block cache-block-backup))))))

(defmethod cache-sim-miss-count ((sim cache-sim))
  (- (cache-sim-op-count sim) (cache-sim-hit-count sim)))

(defmethod cache-sim-miss-ratio ((sim cache-sim))
  (/ (cache-sim-miss-count sim) (cache-sim-op-count sim)))

(defmethod cache-sim-debug-info ((sim cache-sim))
  "list with most relevant info for debugging purposes"
  `(
           ;:addr-bits  ,(cache-sim-addr-bits sim)
           ;:cache-bits ,(cache-sim-cache-bits sim)
           ;:assoc-bits ,(cache-sim-assoc-bits sim)
           ;:block-bits ,(cache-sim-block-bits sim)
           :op-count ,(cache-sim-op-count sim)
           :hit-count ,(cache-sim-hit-count sim)
           :lines ,(cache-sim-lines sim)))
