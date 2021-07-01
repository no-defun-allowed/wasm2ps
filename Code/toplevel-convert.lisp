(in-package :wasm2ps)

(defun toplevel-convert (form)
  (case (first form)
    (wasm2ps-user:func
     (destructuring-bind (func name &rest body)
         form
       (declare (ignore func))
       (convert-function name body)))
    (otherwise
     (let ((*print-depth* 1))
       (format nil "% ~a" form)))))

(defun convert-function (name body)
  (loop with local-count = 0
        with param-count = 0
        for part on body
        for ((declaration . values) . nil) = part
        do (case declaration
             (wasm2ps-user:local
              (setf local-count (length values)))
             (wasm2ps-user:param
              (setf param-count (length values)))
             ((wasm2ps-user:type wasm2ps-user:result))
             (otherwise
              (return
                (emit-function-code name local-count param-count part))))))

(defun emit-function-code (name locals params body)
  (format nil "/~a { { ~%~a~{~%~a~} } ~d function-body } bind def"
          name
          (install-params params (+ params locals))
          (mapcar #'convert body)
          locals))

(defun install-params (params locals)
  (format nil "~{~d exch def~%~}~{~d 0 def~^~%~}"
          (loop for n from (1- params) downto 0
                collect n)
          (loop for n from (1- locals) downto params
                collect n)))

(defun emit-from-ps-source (pathname)
  (write-line
   (alexandria:read-file-into-string
    (asdf:system-relative-pathname '#:wasm2ps pathname))))

(defun wasm2ps (pathname &key (output *standard-output*))
  (etypecase output
    (stream
     (let ((*standard-output* output))
       (%wasm2ps pathname)))
    ((or pathname string)
     (with-open-file (s output
                        :direction :output
                        :if-exists :overwrite
                        :if-does-not-exist :create)
       (let ((*standard-output* s))
         (%wasm2ps pathname))))))

(defun %wasm2ps (pathname)
  (destructuring-bind (module &rest forms)
      (read-module-from-wasm-file pathname)
    (assert (eql module 'wasm2ps-user:module))
    (emit-from-ps-source "runtime.ps")
    (loop for form in forms
          do (write-line (toplevel-convert form)))
    (emit-from-ps-source "execute.ps")
    (values)))
