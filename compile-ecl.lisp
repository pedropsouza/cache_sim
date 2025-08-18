(load "~/quicklisp/setup.lisp")
(push "./" asdf:*central-registry*)
(ql:quickload "cache-sim")
(asdf:make-build :cache-sim :type :program :move-here #P"./cache_simulator_ecl" :epilogue-code '(cache-sim:main))
