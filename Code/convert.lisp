(in-package :wasm2ps)

(defvar *type-prefixes* '("I32." "I64."))

(defun starts-with-p (prefix string)
  (= (length prefix) (mismatch prefix string)))

(defgeneric convert (value)
  (:method ((value list))
    (convert-list (first value) (rest value))))

(defgeneric convert-list (name arguments)
  (:method (name arguments)
    "Delete type prefixes if we find any."
    (loop with symbol-name = (symbol-name name)
          for prefix in *type-prefixes*
          when (starts-with-p prefix symbol-name)
            do (return-from convert-list
                 (convert-list
                  (intern (subseq symbol-name (length prefix))
                          '#:wasm2ps-user)
                  arguments)))
    (call-next-method)))

(defun convert-body (forms)
  (format nil "~{~a~^ ~}" (mapcar #'convert forms)))

(defmacro define-converter ((name &rest arguments) &body body)
  `(defmethod convert-list ((.name. (eql ',name)) .arguments.)
     (destructuring-bind ,arguments .arguments.
       ,@body)))

(define-converter (wasm2ps-user:const n)
  (prin1-to-string n))

(define-converter (wasm2ps-user:add x y)
  (format nil "~a ~a add"
          (convert x)
          (convert y)))

(define-converter (wasm2ps-user:local.get n)
  (format nil "~d load" n))

(define-converter (wasm2ps-user:local.set n value)
  ;; I'd rather keep the position next to the store word.
  (format nil "~a ~d exch store" (convert value) n))

(define-converter (wasm2ps-user:local.tee n value)
  (format nil "~a dup ~d exch store" (convert value) n))

(define-converter (wasm2ps-user:block &body body)
  (format nil "{ ~a } block" (convert-body body)))

(define-converter (wasm2ps-user:loop &body body)
  (format nil "{ ~a } wasm-loop" (convert-body body)))

(define-converter (wasm2ps-user:br_if depth test)
  (format nil "~a 0 ne { ~d throw } if"
          (convert test) depth))

(macrolet ((def (name)
             `(define-converter (,(alexandria:format-symbol '#:wasm2ps-user
                                                            "~a_S"
                                                            name)
                                 a b)
                (format nil "~a ~a ~a boolean-to-int"
                        (convert a) (convert b) ,(string-downcase name)))))
  (def lt)
  (def le)
  (def eq)
  (def ge)
  (def gt))

(define-converter (wasm2ps-user:return value)
  (format nil "~a return"
          (convert value)))

(define-converter (wasm2ps-user:call function-name &rest values)
  (format nil "~a ~a"
          (convert-body values) function-name))
