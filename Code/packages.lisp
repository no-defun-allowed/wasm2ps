(defpackage #:wasm2ps
  (:use :cl)
  (:export #:wasm2ps))

(defpackage #:wasm2ps-user
  (:use)
  (:export #:module
           #:type #:i32 #:funcref #:memory
           #:func #:param #:result #:local
           #:block #:loop #:call #:return
           #:import
           #:table

           #:br_if
           #:local.get #:local.set #:local.tee
           #:add #:mul #:const
           #:gt_s #:ge_s #:eq_s #:le_s #:lt_s))
