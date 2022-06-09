;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun initialize-lparallel-kernel (&key (worker-count (sera:count-cpus)))
  "Initialize the lparallel kernel with WORKER-COUNT, if not supplied set it to
the amount of CPU cores."
  (unless lpara:*kernel*
    (setf lpara:*kernel* (lpara:make-kernel worker-count))))

(defun restart-browser (c)
  "Restart browser reporting condition C."
  (restart-with-message
   :condition c
   :backtrace (with-output-to-string (stream)
                (uiop:print-backtrace :stream stream :condition c))))

(export-always 'with-protect)
(defmacro with-protect ((format-string &rest args) &body body)
  "Run body with muffled conditions when `*run-from-repl-p*' is nil, run normally otherwise.
When the condition is muffled, a warning is reported to the user as per
FORMAT-STRING and ARGS.
As a special case, the first `:condition' keyword in ARGS is replaced with the
raised condition."
  (alex:with-gensyms (c sub-c)
    `(if (or *run-from-repl-p* *debug-on-error*)
         (handler-case (progn ,@body)
           (nyxt-prompt-buffer-canceled ()
             (log:debug "Prompt buffer interrupted")))
         (ignore-errors
          (handler-bind
              ((error
                 (lambda (,c)
                   (declare (ignorable ,c))
                   (if *restart-on-error*
                       (restart-browser ,c)
                       ,(let ((condition-index (position :condition args)))
                          (flet ((new-args (condition condition-index &optional escaped-p)
                                   (if condition-index
                                       (append (subseq args 0 condition-index)
                                               (list (if escaped-p
                                                         `(plump:encode-entities (princ-to-string ,condition))
                                                         `,condition))
                                               (subseq args (1+ condition-index)))
                                       'args)))
                            `(handler-bind ((t (lambda (,sub-c)
                                                 (declare (ignore ,sub-c))
                                                 (log:error ,format-string ,@(new-args c condition-index))
                                                 (invoke-restart 'continue))))
                               (echo-warning ,format-string ,@(new-args c condition-index :escaped-p)))))))))
            ,@body)))))

(defun make-channel (&optional size)
  "Return a channel of capacity SIZE.
If SIZE is 0, channel is always blocking.
If SIZE is NIL, capacity is infinite."
  (cond
    ((null size)
     (make-instance 'calispel:channel
                    :buffer (make-instance 'jpl-queues:unbounded-fifo-queue)))
    ((zerop size)
     (make-instance 'calispel:channel))
    ((plusp size)
     (make-instance 'calispel:channel
                    :buffer (make-instance 'jpl-queues:bounded-fifo-queue :capacity size)))))

(defun drain-channel (channel &optional timeout)
  "Listen to CHANNEL until a value is available, then return all CHANNEL values
as a list.
TIMEOUT specifies how long to wait for a value after the first one.
This is a blocking operation."
  (labels ((fetch ()
             (multiple-value-bind (value received?)
                 (calispel:? channel timeout)
               (if received?
                   (cons value (fetch))
                   nil))))
    (cons (calispel:? channel)
          (nreverse (fetch)))))

(defun wrap-thunk-for-thread (thunk channel error-channel)
  (lambda ()
    (if (or *run-from-repl-p* *restart-on-error*)
        (let ((current-condition nil))
          (restart-case
              (handler-bind ((condition (lambda (c) (setf current-condition c))))
                (calispel:! channel (funcall thunk)))
            (abort-ffi-method ()
              :report "Pass condition to calling thread."
              (calispel:! error-channel current-condition))))
        (handler-case (calispel:! channel (funcall thunk))
          (condition (c)
            (calispel:! error-channel c))))))

(defun wait-for-thread (thread channel error-channel)
  (calispel:fair-alt
    ((calispel:? channel result)
     result)
    ((calispel:? error-channel condition)
     (with-protect ("Error in thread ~a: ~a" (bt:thread-name thread) :condition)
       (error condition)))))

(defun call-with-thread (name thunk)
  "See also `with-thread-async' and `call-with-thread-wait'."
  ;; TODO: Explain why we rely on 2 channels, instead of using bt:join-thread to
  ;; get the thread value.  It's because we need `calispel:fair-alt'.  With
  ;; `bt:join-thread' alone we cannot get the condition.  Returning the
  ;; condition is not an option, otherwise how do we tell the difference between
  ;; a raised condition and the instance of a condition returned like any value?
  ;; BUT! Do we need to know the condition of the failed thread?
  ;; Maybe not, seems that we are not using it at all when *run-from-repl-p* is nil.
  (let ((channel (make-channel 1))
        (error-channel (make-channel 1)))
    (values
     (bt:make-thread
      (wrap-thunk-for-thread thunk channel error-channel)
      :name (str:concat "Nyxt " name))
     channel
     error-channel)))

(defun call-with-thread-wait (name thunk)
  "See also `call-with-thread' and `with-thread'."
  (multiple-value-call #'wait-for-thread
    (call-with-thread name thunk)))

(export-always 'with-thread)
(defmacro with-thread (name &body body)
  "Run BODY in a new protected thread.
Return 3 values:
- The newly thread.
- A result channel.
- An error channel.

Example:

\(multiple-value-match (with-thread \"My thread\" BODY)
  ((thread channel error-channel)
   (calispel:fair-alt
     ((calispel:? channel result)
      result)
     ((calispel:? error-channel condition)
      (with-protect (\"Error in ~a: ~a\" (bt:thread-name thread) :condition)
        (error condition))))))

This supersedes `bt:make-thread' in Nyxt.  Don't use the latter unless you know
what you are doing!"
  (sera:with-thunk (body)
    `(call-with-thread ,name ,body)))

(export-always 'with-thread-wait)
(defmacro with-thread-wait (name &body body)
  "Run BODY in a new protected thread and block until it can return the result."
  (sera:with-thunk (body)
    `(call-with-thread-wait ,name ,body)))

(defun call-with-renderer-thread-maybe-wait (name thunk) ; TODO: Poorly named, it's not blocking!
  "Like `call-with-thread' but run THUNK from the GTK loop.
If already on the loop, return result of THUNK.
If not on the loop, return 3 values, as in `call-with-thread'."
  (if (renderer-thread-p)
      (funcall thunk)
      (let ((channel (make-channel 1))
            (error-channel (make-channel 1)))
        (values
         (ffi-with-renderer-thread
          (wrap-thunk-for-thread thunk channel error-channel))
         channel
         error-channel))))

(defun call-with-renderer-thread-wait (name thunk)
  "See also `call-with-renderer-thread-maybe-wait'."
  (if (renderer-thread-p)
      (funcall thunk)
      ;; TODO: Override `wait-for-thread' message with "Error in FFI method: ~a".
      (multiple-value-call #'wait-for-thread
        (call-with-renderer-thread-maybe-wait name thunk))))

(defmacro with-renderer-thread-maybe-wait (name &body body)
  ""
  (sera:with-thunk (body)
    `(call-with-renderer-thread-maybe-wait ,name ,body)))

(defmacro with-renderer-thread-wait (name &body body)
  ""
  (sera:with-thunk (body)
    `(call-with-renderer-thread-wait ,name ,body)))

(export-always 'run-thread)
(defmacro run-thread (name &body body)  ; TODO: Deprecate!  Or use previous definitions for those who do not care about the channels?
  "Obsoleted by `with-thread'."
  `(with-thread ,name ,@body))

(defun evaluate-wait (string &key interactive-p)
  "Evaluate all expressions in STRING and return the last result as a list of values.
The list of values is useful when the last result is multi-valued, e.g. (values 'a 'b).
You need not wrap multiple values in a PROGN, all top-level expressions are
evaluated in order."
  (multiple-value-call #'wait-for-thread
    (evaluate string :interactive-p interactive-p)))

(setf (fdefinition 'evaluate) #'evaluate-wait) ; TODO: Mark as obsolete.

(defun evaluate (string &key interactive-p)
  "Like `evaluate' but does not block."
  (with-thread "evaluator"
    (let ((interactive-p interactive-p))
      (with-input-from-string (input string)
        (first
         (last
          (loop for object = (read input nil :eof)
                until (eq object :eof)
                collect (multiple-value-list
                         (handler-case (let ((*interactive-p* interactive-p))
                                         (eval object))
                           (error (c) (format nil "~a" c)))))))))))
