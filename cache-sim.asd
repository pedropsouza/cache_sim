(asdf:defsystem #:cache-sim
  :depends-on (#:asdf
               #:alexandria
               #:uiop)
  :components ((:file "package")
               (:file "noback")
               (:file "main")))
