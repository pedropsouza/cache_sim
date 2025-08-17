(load "full.lsp")
(defparameter *seen-addrs* (make-hash-table))
(defparameter *addr-bits* 6)
(defparameter *cache-bits-list*
  '((l1 . (2 0 1))
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
            (list
              (new-cache-sim
                :addr-bits *addr-bits*
                :cache-bits (cadr entry)
                :assoc-bits (caddr entry)
                :block-bits (cadddr entry)
                :backing *main-mem*)
              (new-cache-sim ; fully associative mirror
                :addr-bits *addr-bits*
                :cache-bits (cadr entry)
                :assoc-bits (- (cadr entry) (cadddr entry))
                :block-bits (cadddr entry)
                :backing *main-mem*)
              (new-cache-sim ; compulsory mirror
                :addr-bits *addr-bits*
                :cache-bits *addr-bits*
                :assoc-bits 0
                :block-bits (cadddr entry)
                :backing *main-mem*))))
    *cache-bits-list*))

(defun run-sim (&optional (input-file "input-teste-assoc.txt"))
  (prog ((lines (uiop:read-file-lines (or (nth 1 sb-ext:*posix-argv*) input-file))))
        (setf *random-state* (seed-random-state 42))
        (princ "###")
        (mapcar
          (lambda (addr-str)
            (let ((addr (parse-integer addr-str)))
              (labels
                ((lookup (levels)
                         (and levels
                              (multiple-value-bind (outcome value blk old-blk)
                                (cache-sim-access (cadar levels) addr)
                                (cache-sim-access (caddar levels) addr)
                                (cache-sim-access (nth 3 (car levels)) addr)
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
                  (mapcar
                    (lambda (x)
                      (progn
                        (pprint (cons (car x) (cache-sim-debug-info (cadr x))))
                        (pprint (cons (intern (concatenate 'string (string (car x)) "-fa")) (cache-sim-debug-info (caddr x))))))
                    *cache*)))))
          lines)
        (flet ((print-stats (level)
                            (let* ((sim (cadr level))
                                   (h (cache-sim-hit-count sim))
                                   (c (cache-sim-op-count sim))
                                   (m (- c h)))
                              (format t "~&for ~a:" (string (car level)))
                              (format t "~&~4thit ratio is ~d/~d = ~,4f" h c (/ h c))
                              (format t "~&~4tmiss ratio is ~d/~d = ~,4f" m c (cache-sim-miss-ratio sim))
                              (let* ((fa (nth 2 level))
                                     (co (nth 3 level))
                                     (compulsory-miss-ratio (cache-sim-miss-ratio co))
                                     (conflict-miss-ratio   (cache-sim-miss-ratio fa))
                                     (capacity-miss-ratio   (- conflict-miss-ratio compulsory-miss-ratio)))
                                ; conflict miss ratio = miss ratio - miss ratio fully associative
                                ; capacity miss ratio = miss ratio fully associative - miss ratio infinite capacity
                                ; compulsory miss ratio = miss ratio infinite capacity
                                (format t "~&~4tconflict misses for ~s = ~,4f"
                                        (car level) conflict-miss-ratio)
                                (format t "~&~4tcapacity misses for ~s = ~,4f"
                                        (car level) capacity-miss-ratio)
                                (format t "~&~4tcompulsory misses for ~s = ~,4f"
                                        (car level) compulsory-miss-ratio)))))

          (progn
            (mapcar #'print-stats *cache*))
          (let ((all-hits (reduce #'+ *cache* :key (lambda (x) (cache-sim-hit-count (cadr x)))))
                (all-ops (cache-sim-op-count (cadr (assoc 'l1 *cache*)))))
            (format t "~&overall hit rate: ~d/~d = ~,4f" all-hits all-ops (/ all-hits all-ops)))
          (format t "~&Done!"))))
