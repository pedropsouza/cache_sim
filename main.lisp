(in-package #:cache-sim)
(require :uiop)
;(load "noback.lsp")
(defun init-globals (&key addr-bits cache-bits-list)
  (defparameter *seen-addrs* (make-hash-table))
  (defparameter *fmtstr*
    (format nil "~~&~~~dd = ~~~d,'0b, ~~a @ ~~a";~~%~~a"
      (ceiling (log (expt 2 addr-bits)) (log 10))
      (ceiling (log (expt 2 addr-bits)) (log 2))))

  (defparameter *cache*
    (mapcar
      (lambda (entry)
        (cons (car entry)
              (list
                (new-cache-sim
                  :addr-bits addr-bits
                  :cache-bits (cadr entry)
                  :assoc-bits (caddr entry)
                  :block-bits (cadddr entry))
                (new-cache-sim ; fully associative mirror
                  :addr-bits addr-bits
                  :cache-bits (cadr entry)
                  :assoc-bits (- (cadr entry) (cadddr entry))
                  :block-bits (cadddr entry)))))
      cache-bits-list)))

(defun run-sim (&optional (input-file "input-teste-assoc.txt"))
  (prog ((lines (uiop:read-file-lines input-file))
         (miss-kind-alist (mapcar (lambda (entry) (list (car entry) 0 0 0)) *cache*)))
        ;(setf *random-state* (seed-random-state 42))
        (princ "###")
        (mapcar
          (lambda (addr-str)
            (let ((addr (parse-integer addr-str)))
              (labels
                ((lookup (levels)
                         (and levels
                              (multiple-value-bind (outcome blk old-blk)
                                (cache-sim-access (cadar levels) addr)
                                (let ((fully-assoc-outcome (cache-sim-access (caddar levels) addr)))
                                  (case outcome
                                    (hit
                                      ;(progn
                                        ;(format t *fmtstr* addr addr "hit" (caar levels))
                                        ;)
                                      nil)
                                    (miss
                                      (prog ((kind "capacity"))
                                            (format t *fmtstr* addr addr "miss" (caar levels))
                                            (cond
                                              ((not (gethash addr *seen-addrs*)) 
                                                (setq kind "compulsory")
                                                (incf (cadr (assoc (caar levels) miss-kind-alist))))
                                              ((eq fully-assoc-outcome 'hit)
                                                (setq kind "conflict")
                                                (incf (caddr (assoc (caar levels) miss-kind-alist))))
                                              (t (incf (cadddr (assoc (caar levels) miss-kind-alist)))))

                                            (format t " (~a) replace ~a -> ~a" kind old-blk blk)
                                            (lookup (cdr levels))
                                            (setf (gethash addr *seen-addrs*) t)))
                                    ))))))
                (progn (lookup *cache*)))))
          lines)
        (flet ((print-stats (level)
          (let ((sim (cadr level)))
            (and sim
              (let* ((h (cache-sim-hit-count sim))
                     (c (cache-sim-op-count sim))
                     (m (- c h)))
                (format t "~&for ~a:" (string (car level)))
                (format t "~&~4thit ratio is ~d/~d = ~,4f" h c (/ h c))
                (format t "~&~4tmiss ratio is ~d/~d = ~,4f" m c (cache-sim-miss-ratio sim))
                (let* ((fa (nth 2 level))
                       (miss-kinds (cdr (assoc (car level) miss-kind-alist)))
                       (compulsory-misses (nth 0 miss-kinds))
                       (conflict-misses (nth 1 miss-kinds))
                       (capacity-misses (nth 2 miss-kinds)))
                  (flet ((report-misskind (name val)
                          (format t "~&~4t~s misses for ~s = ~d/~d = ~,4f"
                          name (car level) val m (/ val m))))
                  ; conflict miss ratio = miss ratio - miss ratio fully associative
                  ; capacity miss ratio = miss ratio fully associative - miss ratio infinite capacity
                    (report-misskind "compulsory" compulsory-misses)
                    (report-misskind "conflict" conflict-misses)
                    (report-misskind "capacity" capacity-misses)
                  )))))))
          (progn
            (mapcar #'print-stats *cache*))
          (let* ((all-hits (reduce #'+ *cache* :key (lambda (x) (cache-sim-hit-count (cadr x)))))
                 (num-accesses (cache-sim-op-count (cadr (assoc :l1 cache-sim::*cache*))))
                )
            (format t "~&Overral hit rate: ~d/~d = ~,4f"
                    all-hits num-accesses (/ all-hits num-accesses))
            (format t "~&Overral miss rate: ~d/~d = ~,4f"
                    (- num-accesses all-hits) num-accesses (/ (- num-accesses all-hits) num-accesses))
            (format t "~&Done!")))))

(defun sufficient-bits (num &optional change-notice-f)
        (let* ((bits (ceiling (log num 2)))
               (nearest (expt 2 bits)))
          (if (not (equal nearest num))
            (funcall change-notice-f num nearest))
          bits))

(defun alert-value-change (which)
        (lambda (old new)
          (format t "~&alert: ~s was rounded up from ~d to ~d, the nearest power of two."
            which old new)))

(defun parse-cache-definition (c-def)
  (destructuring-bind
    (num-sets block-size associativity)
    (mapcar #'parse-integer (uiop:split-string c-def :separator ":"))
    (list (sufficient-bits (* num-sets associativity (/ block-size 8))
                           (alert-value-change "number of sets"))
          (sufficient-bits associativity (alert-value-change "associativity"))
          (sufficient-bits (/ block-size 8) (alert-value-change "block size")))))

(defun give-level-names (c-defs)
  (mapcar
    (lambda (num def)
      (cons
        (intern (concatenate 'string "L" (prin1-to-string num)) (find-package :keyword))
        def))
    (alexandria:iota (length c-defs) :start 1)
    c-defs))

(defun read-eval-print-loop ()
  (loop (print (eval (read)))))

(defun main (&key (filepath nil) (cache-defs-str nil))
  (let*
    ((args (uiop:command-line-arguments))
     (filepath (or filepath (car (last args))))
     (cache-defs-str-actual
       (or
         (uiop:split-string
           cache-defs-str
           :separator " ")
         (if (> (length (butlast args)) 1) (butlast args) '("1024:32:1"))))
     (c-defs
       (give-level-names
         (mapcar #'parse-cache-definition
                 cache-defs-str-actual)))
     (addr-bits
       (sufficient-bits
         (reduce #'max 
           (mapcar #'parse-integer 
             (remove-if #'alexandria:emptyp
               (uiop:read-file-lines filepath))))
         (alert-value-change "address bits"))))
    (format t "~&using ~d bits for addressing~%" addr-bits)
    (init-globals
      :addr-bits addr-bits
      :cache-bits-list c-defs)
    (run-sim filepath)
    (read-eval-print-loop)))
