(declaim (optimize (speed 3) (safety 0) (debug 0)))
(load "main.lsp")
(sb-ext:save-lisp-and-die "cache_simulator" :toplevel #'main :executable t)
