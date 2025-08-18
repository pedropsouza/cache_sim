(push "./" asdf:*central-registry*)
(ql:quickload "cache-sim")
;(declaim (optimize (speed 3) (safety 0) (debug 0))) ; doesn't do jack to reduce file size
(load "main.lisp")
(sb-ext:save-lisp-and-die "cache_simulator" :toplevel #'cache-sim:main :executable t)
