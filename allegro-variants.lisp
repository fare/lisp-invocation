(defpackage :lisp-invocation/allegro-variants
  (:use :common-lisp :fare-utils :uiop :lisp-invocation/lisp-invocation)
  (:export #:all-allegro-variants))

(in-package :lisp-invocation/allegro-variants)

#|
Allegro is supported in the following flavors:
   allegro, allegro8, allegromodern, allegromodern8,
   allegro_s, allegro8_s, allegromodern_s, allegromodern8_s (SMP variants)
   allegro_64, allegro8_64, allegromodern_64, allegromodern8_64 (64-bit variants),
   allegro_64_s, allegro8_64_s, allegromodern_64_s, allegromodern8_64_s, (SMP, 64-bit variants)
Allegro CL is a special case: instead of setting environment variables for the specific runtime
locations, you may simply specify the Allegro install directories using these variables:
    ALLEGRO64DIR, ALLEGRO64SDIR (64-bit Allegro and SMP Allegro, respectively),
    ALLEGRODIR, and ALLEGROSDIR.
|#

(defun all-allegro-variants ()
  "Return a list of possible Allegro variants based on built-in information
and environment variables.  The returned list is made up of argument lists for
REGISTER-LISP-IMPLEMENTATION."
  ;; Beware that on Windows, we use the non-multithreaded console application build.exe
  ;; so that we may have console I/O on the existing stdin and stdout, that we may capture.
  (while-collecting (c)
    (loop
      :with windowsp = (os-windows-p)
      :for (smpvar smpname smpfullname) :in `(("" "" "") ("S" :_s " (SMP)")) :do
      (loop
        :for (bitsvar bitsname bitsfullname) :in '(("" "" "") ("64" "_64" " (64-bit words)"))
        :for dirvar = (format nil "~:@(ALLEGRO~A~A~)" bitsvar smpvar)
        :for dir = (getenv-pathname dirvar :want-absolute t :ensure-directory t) :do
          (loop :for (charname charfullname) :in '(("" "") ("8" " (8-bit chars)")) :do
            (loop
              :for (caseexe casename casefullname) :in
              '(("a" "" "") ("m" :modern " (modern syntax)"))
              :for allegro-variant = (conc-keyword :allegro casename charname bitsname smpname)
              :for fullname = (strcat "Allegro CL"
                                      casefullname charfullname bitsfullname smpfullname)
              :for executable = (format nil "~(~alisp~a~)" caseexe charname)
              :for (exepath imgpath) = (if windowsp
                                           (list (subpathname dir (if (emptyp charfullname) "buildi.exe" "build.exe"))
                                                 (subpathname dir executable :type "dxl"))
                                           (list (subpathname dir executable) nil)) :do
                (c `(,allegro-variant
                     :fullname ,fullname
                     :name ,(native-namestring exepath)
                     :default-image ,(native-namestring imgpath)
                     :feature :allegro ;; do we want a more discriminating feature expression?
                     :flags ("-qq")
                     :eval-flag "-e"
                     :load-flag "-L"
                     ;; :quit-flags ("-kill")
                     :arguments-end "--"
                     :image-flag "-I"
                     :image-executable-p nil
                     :standalone-executable nil
                     :argument-control t
                     :disable-debugger ("-batch") ; see also -#D -#C -#!
                     :quit-format "(excl:exit ~A :quiet t)"
                     :dump-format "(progn (sys:resize-areas :global-gc t :pack-heap t :sift-old-areas t :tenure t) (excl:dumplisp :name ~A :suppress-allegro-cl-banner t))"))))))))

(map () 'register-lisp-implementation* (all-allegro-variants))
