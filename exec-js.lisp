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

;;  Config

(defvar *node* "/usr/local/bin/node")
(defvar *node-arguments* '())

;;  Conditions

(define-condition execjs-error (simple-error) ())

(define-condition js-compile-error (execjs-error) ()
  (:default-initargs
   :format-control
      "Error from Javascript compiler :~%~A~&Exit code : ~D"))

(define-condition js-program-error (execjs-error) ()
  (:default-initargs
   :format-control
      "Error in Javascript program : ~A~&~A"))

;;  Call node

(defun run/stream (program args in out err)
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
				  ,code)))))

(defun run (program args &key in out)
  (let ((stream (unless out
		  (make-string-output-stream)))
	(err (make-string-output-stream)))
    (unwind-protect (run/stream program args in (or out stream) err)
      (when stream
	(close stream))
      (close err))
    (values (or out (get-output-stream-string stream))
	    (get-output-stream-string err))))

;;  Safely

(defun safe-read-from-string (string)
  (let ((*read-eval* nil))
    (read-from-string string)))

(defun parse-output (out err)
  (destructuring-bind (&optional assert js) (safe-read-from-string out)
    (unless assert
      (error 'js-program-error :format-arguments `(,js ,err)))
    (when js
      (json:decode-json-from-string js))))

;;  Exec

(defun from-stream (js &key (safely t) out)
  (if safely
      (with-input-from-string (first +node-runner-prefix+)
	(with-input-from-string (third +node-runner-suffix+)
	  (let ((js (make-concatenated-stream first js third)))
	    (unwind-protect
		 (multiple-value-call #'parse-output
		   (run *node* *node-arguments* :in js :out out))
	      (close js)))))
      (run *node* *node-arguments* :in js :out out)))

(defun from-string (js &key (safely t) out)
  (with-input-from-string (js js)
    (from-stream js :safely safely :out out)))

(defun from-file (js &key (safely t) out)
  (with-open-file (js js
		      :element-type 'character
		      :external-format :utf-8)
    (from-stream js :safely safely :out out)))

(defun from (js &key (safely t) out)
  (typecase js
    (stream   (from-stream js :safely safely :out out))
    (pathname (from-file   js :safely safely :out out))
    (string   (from-string js :safely safely :out out))))
