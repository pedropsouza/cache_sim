(load "cache-sim.lsp")
(defparameter *sim* (new-cache-sim :addr-bits 5 :cache-bits 3))
(loop for addr in '(
               #2r11000
               #2r10110
               #2r11010
               #2r10110
               #2r10000
               #2r00011
               #2r10010
               #2r10110
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
      (fresh-line)
      (format t fmtstr addr addr (car res) (cdr res) (cache-sim-cache-cells *sim*))
      )
    )
  )
(let ((h (cache-sim-hit-count *sim*))
      (c (cache-sim-op-count *sim*)))
  (format t "~&the hit ratio is ~d/~d = ~,4f"
    h c (/ h c)
    )
  )
