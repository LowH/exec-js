;;
;;  exec-js  -  Execute Javascript from Common Lisp
;;
;;  Copyright 2012 Thomas de Grivel <billitch@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :exec-js)

;;  Execute JavaScript

(defvar *node* "/usr/local/bin/node")
(defvar *node-arguments* ())

(defun safe-read-from-string (string)
  (let ((*read-eval* nil))
    (read-from-string string)))

(define-condition execjs-error (simple-error) ())

(define-condition js-compile-error (execjs-error) ()
  (:default-initargs
   :format-control
      "Error from Javascript compiler :~%~A~&Exit code : ~D"))

(define-condition js-program-error (execjs-error) ()
  (:default-initargs
   :format-control
      "Error in Javascript program : ~A~&~A"))

(defun run (program args &optional in)
  (let ((out (make-string-output-stream))
	(err (make-string-output-stream)))
    (unwind-protect
	 (multiple-value-bind (status code)
	     (external-program:run program
				   args
				   :input in
				   :output out
				   :error err
				   :environment nil
				   :replace-environment-p t
				   :external-format :utf-8)
	   (unless (and (eq :exited status) (= 0 code))
	     (error 'js-compile-error
		    :format-arguments `(,(get-output-stream-string err)
					 ,code))))
      (close out)
      (close err))
    (values (get-output-stream-string out)
	    (get-output-stream-string err))))

(defun parse-output (out err)
  (destructuring-bind (&optional assert js) (safe-read-from-string out)
    (unless assert
      (error 'js-program-error :format-arguments `(,js ,err)))
    (when js
      (json:decode-json-from-string js))))

(defun from-stream (js)
  (multiple-value-call #'parse-output
    (with-input-from-string (first +node-runner-prefix+)
      (with-input-from-string (third +node-runner-suffix+)
	(let ((js (make-concatenated-stream first js third)))
	  (unwind-protect
	       (run *node* *node-arguments* js)
	    (close js)))))))

(defun from-string (js)
  (with-input-from-string (js js)
    (from-stream js)))

(defun from-file (js)
  (with-open-file (js js
		      :element-type 'character
		      :external-format :utf-8)
    (from-stream js)))

(defun from (js)
  (typecase js
    (stream   (from-stream js))
    (pathname (from-file   js))
    (string   (from-string js))))
