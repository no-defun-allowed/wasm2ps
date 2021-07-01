(asdf:defsystem :wasm2ps
  :depends-on (:alexandria :external-program)
  :serial t
  :components ((:file "packages")
               (:file "reader")
               (:file "convert")
               (:file "toplevel-convert")))
