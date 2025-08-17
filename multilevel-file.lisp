(load "full.lsp")
(defparameter *seen-addrs* (make-hash-table))
(defparameter *addr-bits* 6)
(defparameter *cache-bits-list*
  '((l1 . (3 0 1))
    (l2 . (4 1 2))
    (l3 . (5 3 2))
    ))
(defparameter *fmtstr*
  (format nil "~~&~~~dd = ~~~d,'0b, ~~a @ ~~a";~~%~~a"
    (ceiling (log (expt 2 *addr-bits*)) (log 10))
    (ceiling (log (expt 2 *addr-bits*)) (log 2))))

(defparameter *main-mem* (make-array (expt 2 *addr-bits*)))
(defparameter *cache*
  (mapcar
    (lambda (entry)
      (cons (car entry)
            (new-cache-sim
              :addr-bits *addr-bits*
              :cache-bits (cadr entry)
              :assoc-bits (caddr entry)
              :block-bits (cadddr entry)
              :backing *main-mem*)))
    *cache-bits-list*))

(prog ((lines (uiop:read-file-lines (or (nth 1 sb-ext:*posix-argv*) "input.txt"))))
  (setf *random-state* (seed-random-state 42))
  (princ "###")
  (mapcar
    (lambda (addr-str)
      (let ((addr (parse-integer addr-str)))
        (labels
          ((lookup (levels)
            (and levels
              (multiple-value-bind (outcome value blk old-blk)
                (cache-sim-access (cdar levels) addr)
                (case outcome
                  (hit
                    (progn
                      (format t *fmtstr* addr addr "hit" (caar levels))))
                  (miss
                    (prog ((kind "conflict"))
                      (format t *fmtstr* addr addr "miss" (caar levels))
                      (if (not (gethash addr *seen-addrs*)) (princ " (compulsory)"))
                      (format t " replace ~a -> ~a" old-blk blk)
                      (lookup (cdr levels))
                      (setf (gethash addr *seen-addrs*) t)))
                  )))))
          (progn
            (lookup *cache*)
            (mapcar (lambda (x) (print (cons (car x) (cache-sim-debug-info (cdr x))))) *cache*)))))
    lines)
  (flet ((print-stats (level)
          (let ((h (cache-sim-hit-count (cdr level)))
                (c (cache-sim-op-count (cdr level))))
            (format t "~&the hit ratio for ~a is ~d/~d = ~,4f"
              (string (car level)) h c (/ h c)))))
    (mapcar #'print-stats *cache*))
  (let ((all-hits (reduce #'+ *cache* :key (lambda (x) (cache-sim-hit-count (cdr x)))))
        (all-ops (cache-sim-op-count (cdr (assoc 'l1 *cache*)))))
    (format t "~&overall hit rate: ~d/~d = ~,4f" all-hits all-ops (/ all-hits all-ops)))
  (format t "~&Done!"))
