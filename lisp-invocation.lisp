;;; Lisp implementations
#+xcvb (module (:build-depends-on ("/asdf")))

(uiop:define-package :lisp-invocation/lisp-invocation
  (:nicknames :lisp-invocation)
  (:use :cl :uiop)
  (:export
   #:define-lisp-implementation
   #:get-lisp-implementation
   #:ensure-path-executable
   #:lisp-implementation-fullname
   #:lisp-implementation-name
   #:lisp-implementation-feature
   #:lisp-implementation-flags
   #:lisp-implementation-eval-flag
   #:lisp-implementation-load-flag
   #:lisp-implementation-arguments-end
   #:lisp-implementation-environment-variable
   #:lisp-implementation-image-flag
   #:lisp-implementation-image-executable-p
   #:lisp-implementation-standalone-executable
   #:lisp-implementation-argument-control
   #:lisp-implementation-disable-debugger
   #:lisp-implementation-directory-variable
   #:lisp-implementation-invoker
   #:lisp-environment-variable-name
   #:lisp-invocation-arglist
   #:invoke-lisp
   #:register-lisp-implementation
   #:register-lisp-implementation*
   #:quit-form
   #:save-image-form))

(in-package :lisp-invocation)

(defvar *lisp-implementations* (make-hash-table :test 'equal)
  "Dictionary of known Lisp implementations")

(defstruct (lisp-implementation)
  identifiers ;; the first also names the environment variable for the lisp-path, as per cl-launch
  fullname
  name
  feature
  environment-variable
  flags
  eval-flag
  load-flag
  arguments-end
  image-flag
  image-executable-p
  standalone-executable
  argument-control
  disable-debugger
  directory-variable
  ;; fasl-type cfasl-type
  invoker
  quit-format
  dump-format)

(defmacro define-lisp-implementation (key () &rest keys)
  `(apply 'register-lisp-implementation ',key ',keys))

(defun register-lisp-implementation (identifiers &rest keys)
  "Register the lisp implementation identified by the IDENTIFIERS argument (a
keyword or list of keywords), with given option KEYS."
  (let* ((identifiers (ensure-list identifiers))
         (implementation (apply #'make-lisp-implementation :identifiers identifiers keys)))
    (dolist (id identifiers)
      (assert (keywordp id))
      (setf (gethash id *lisp-implementations*) implementation))))

(defun register-lisp-implementation* (x)
  "Register the lisp implementation described by the list X, which consists of a name
followed by a plist of keywords and arguments."
  (apply 'register-lisp-implementation x))



(defun get-lisp-implementation (&optional (implementation-type (implementation-type)))
  (or (gethash implementation-type *lisp-implementations*)
      (error "Unknown Lisp implementation type ~S" implementation-type)))

(defun ensure-path-executable (x)
  (when x
    (let ((n (native-namestring x)))
      (cond
	((asdf::absolute-pathname-p x) n)
	((asdf::os-unix-p) (format nil "./~A" n))
	(t n)))))

(defun lisp-environment-variable-name (&key (type (implementation-type)) prefix suffix)
  (let* ((implementation (get-lisp-implementation type))
         (name (or (lisp-implementation-environment-variable implementation)
                (first (lisp-implementation-identifiers implementation)))))
    (when (eq prefix t) (setf prefix "X"))
    (when (eq suffix t) (setf prefix "_OPTIONS"))
    (format nil "~@[~A~]~:@(~A~)~@[~A~]" prefix name suffix)))

(defun lisp-invocation-arglist
    (&key (implementation-type (implementation-type))
       lisp-path
       (lisp-flags :default)
       image-path
       load
       eval
       arguments
       debugger
       cross-compile)
  (with-slots (name flags disable-debugger load-flag eval-flag
	       image-flag image-executable-p standalone-executable
	       arguments-end argument-control)
      (get-lisp-implementation implementation-type)
    (append
     (when (or (null image-path) (not image-executable-p))
       (ensure-list
        (or
         (when (consp lisp-path) lisp-path)
         (ensure-path-executable lisp-path)
         (getenvp (lisp-environment-variable-name
                   :type implementation-type :prefix (when cross-compile "X")))
         name)))
     (when (and image-path (not image-executable-p))
       (list image-flag))
     (when image-path
       (list
        (if image-executable-p
          (ensure-path-executable image-path)
          image-path)))
     (if (eq lisp-flags :default)
	 flags
	 lisp-flags)
     (unless debugger
       disable-debugger)
     (mapcan (if load-flag
                 (lambda (x) (list load-flag (native-namestring x)))
                 (lambda (x) (list eval-flag (format nil "(load ~S)" (native-namestring x)))))
             (ensure-list load))
     (when eval
       (list eval-flag eval))
     (when arguments
       (unless argument-control
	 (error "Can't reliably pass arguments to Lisp implementation ~A" implementation-type))
       (cons arguments-end arguments)))))

(defun lisp-invoker (&optional (implementation-type (implementation-type)))
  (or (lisp-implementation-invoker (get-lisp-implementation implementation-type))
      'invoke-lisp-directly))

(defun invoke-lisp
    (&rest keys
     &key (implementation-type (implementation-type))
       lisp-path
       (lisp-flags :default)
       image-path
       load
       eval
       arguments
       debugger
       cross-compile
       (run-program 'run-program)
       run-program-args)
  (declare (ignore lisp-path lisp-flags image-path load eval arguments debugger cross-compile
                   run-program run-program-args))
  (apply (lisp-invoker implementation-type)
         keys))

(defun invoke-lisp-directly
    (&rest keys
     &key (implementation-type (implementation-type))
       lisp-path
       (lisp-flags :default)
       image-path
       load
       eval
       arguments
       debugger
       cross-compile
       (run-program 'run-program)
       run-program-args)
  (declare (ignore implementation-type lisp-path lisp-flags image-path
                   load eval arguments debugger cross-compile))
  (apply run-program
         (apply 'lisp-invocation-arglist (remove-plist-keys '(:run-program :run-program-args) keys))
         run-program-args))

(defun invoke-lisp-via-script
    (&rest keys
     &key implementation-type
       lisp-path
       lisp-flags
       image-path
       load
       eval
       arguments
       debugger
       cross-compile
       (run-program 'run-program)
       run-program-args)
  (declare (ignore implementation-type lisp-path lisp-flags image-path debugger cross-compile
                   run-program run-program-args))
  (with-temporary-file (:stream s :pathname p :type "lisp")
    (when arguments
      (format s "(unless (find-package :uiop/image) (defpackage :uiop/image (:use :cl)))~%~
                 (defparameter uiop/image::*command-line-arguments* '~S)~%"
              arguments))
    (loop :for l :in (ensure-list load) :do (format s "(cl:load ~S)~%" l))
    (format s "~@[~A~]~%" eval)
    :close-stream
    (apply 'invoke-lisp-directly
           :load (native-namestring p)
           (remove-plist-keys '(:load :eval) keys))))


;;; Avoiding use of a compiled-in driver in the build process

(defun quit-form (&key code (implementation-type (implementation-type)))
  "Returns the correct form to quit lisp, based on the value of lisp-implementation.
Can optionally be given a unix status CODE to exit with"
  (format nil (slot-value (get-lisp-implementation implementation-type) 'quit-format)
	  (or code 0)))

(defun save-image-form (filepath &optional (implementation-type (implementation-type)))
  "Returns the lisp form to save the lisp image to the given filepath"
  (format nil (slot-value (get-lisp-implementation implementation-type) 'dump-format)
	  filepath))
