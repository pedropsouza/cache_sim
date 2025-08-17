(load "cache-sim-assoc.lsp")
(defparameter *sim* (new-cache-sim :addr-bits 5 :cache-bits 3 :assoc-bits 1))
(loop for addr in '(
                0
                8
                0
                6
                8
                20
                24
                28
                20
              ) do
  (let ((res (cache-sim-access *sim* addr))
        (fmtstr
          (format nil "~~&~~~dd = ~~~d,'0b got a ~~a with value ~~d~~%~~a"
                (ceiling (log (expt 2 (cache-sim-addr-bits *sim*))) (log 10))
                (ceiling (log (expt 2 (cache-sim-addr-bits *sim*))) (log 2))
            )
          )
        )
    (progn
      (format t fmtstr addr addr (car res) (cddr res) (cache-sim-lines *sim*))
      )
    )
  )
(let ((h (cache-sim-hit-count *sim*))
      (c (cache-sim-op-count *sim*)))
  (format t "~&the hit ratio is ~d/~d = ~,4f"
    h c (/ h c)
    )
  )
