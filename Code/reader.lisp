(in-package :wasm2ps)

(defvar *original-left-paren-reader*
  (get-macro-character #\( ))
(defun read-left-paren (stream character)
  (cond
    ((char= (peek-char nil stream) #\; )
     (read-char stream)
     (read-rest-of-block-comment stream)
     (values))
    (t
     (funcall *original-left-paren-reader* stream character))))

(defun read-rest-of-block-comment (stream)
  (let ((last-char nil))
    (loop
      (let ((this-char (read-char stream)))
        (cond
          ((and (eql last-char #\; ) (eql this-char #\) ))
           (return))
          ((and (eql last-char #\( ) (eql this-char #\; ))
           (read-rest-of-block-comment stream)
           (setf last-char nil))
          (t
           (setf last-char this-char)))))))

(defvar *wast-readtable* (copy-readtable))
(set-macro-character #\( 'read-left-paren nil *wast-readtable*)

(defun read-module (stream)
  (with-standard-io-syntax
    (let ((*read-eval* nil)
          (*readtable* *wast-readtable*)
          (*package* (find-package '#:wasm2ps-user)))
      (read stream))))

(defun read-module-from-wasm-file (pathname)
  (etypecase pathname
    (string)
    (pathname (setf pathname (namestring pathname))))
  (let ((process
          (external-program:start "wasm2wat"
                                  `("-f" ,pathname)
                                  :output :stream)))
    (read-module (external-program:process-output-stream process))))
