;;;-----------------------------------------------------------------------------
;;; 
;;; mini-argparse.lisp -- Common Lisp argument parser inspired by Python's argparse.
;;;
;;; Copyright (C) 2019 Severin Kempf skempf@indyeng.com
;;; 
(in-package #:mini-argparse)

(defparameter *default-width* 80)

;;;-----------------------------------------------------------------------------
;;; utilities
(defparameter *whitespace* '(#\Space #\Newline #\Backspace #\Tab 
                             #\Linefeed #\Page #\Return #\Rubout))

(defun whitespace-p (c)
  (member c *whitespace* :test #'char=))

(defun break-line (line &key (width *default-width*))
  (let ((length (length line)))
    (if (< length width)
        (list line)
        (labels ((split (p0 acc)
                   (if (> p0 length)
                       (reverse acc)
                       (let* ((p2 (min (+ p0 width) length))
                              (p1 (or (if (= p2 length) length) ; take the rest of the line
                                      (position-if #'whitespace-p line ; search backward a bit
                                                   :from-end t
                                                   :start p0 :end p2)
                                      (position-if #'whitespace-p line ; search forward a bit
                                                   :from-end nil
                                                   :start p2 :end length)
                                      length))) ; no more whitespace, just terminate at end
                         (split (1+ p1) (cons (subseq line p0 p1) acc))))))
          (split 0 nil)))))

;;;-----------------------------------------------------------------------------
(defclass parser ()
  ((name :initarg :name)
   (description :initarg :description)
   (optional-keys :initform (make-hash-table :size 5 :test 'equal))
   (optional :initform nil)
   (positional-keys :initform nil)
   (positional :initform nil)
   (unknowns :initform nil)
   (values :initform nil)))

(define-condition <parser-error> (error) ())
(define-condition <option-form-not-supplied> (<parser-error>) ())
(define-condition <option-form-not-unique> (<parser-error>) ())
(define-condition <option-does-not-exist> (<parser-error>) ())
(define-condition <option-invalid-count> (<parser-error>) ())
(define-condition <option-invalid-flag-name> (<parser-error>) ())
(define-condition <option-missing-argument> (<parser-error>) ())

(defun short-form-p (flag)
  (and (= (length flag) 2)
       (char= #\- (elt flag 0))
       (char/= #\- (elt flag 1))))

(defun long-form-p (flag)
  (and (> (length flag) 3)
       (char= #\- (elt flag 0))
       (char= #\- (elt flag 1))))

(defun check-count (count)
  (assert (or (null count) 
              (numberp count)
              (and (characterp count) (char= count #\+)))
          nil
          '<option-invalid-count>))

(defun add-optional (parser name short-form long-form description
                     &key (count 0) type default (show-default t))
  "Add an optional argument to parser.

The option name, short-form, long-form, and description are all
required. One of the forms may be nil. The name is a keyword to set and
retreive the values. The description is printed in the help message.

The count (number of arguments this option holds) is assumed to be 0;
i.e. a flag. The type and default both are assumed nil. Show-default,
which is assumed t, is checked when printing the default value in the
help message.
"
  (check-type name keyword)
  (check-count count)
  (assert (or short-form long-form) nil '<option-form-not-supplied>)
  (with-slots (optional optional-keys values) parser
    (assert (not (getf optional name)) nil '<option-not-unique>)
    (when short-form
      (assert (short-form-p short-form) nil '<option-invalid-flag-name>)
      (assert (not (gethash short-form optional-keys)) nil '<option-not-unique>)
      (setf (gethash short-form optional-keys) name))
    (when long-form
      (assert (long-form-p long-form) nil '<option-invalid-flag-name>)
      (assert (not (gethash long-form optional-keys)) nil '<option-not-unique>)
      (setf (gethash long-form optional-keys) name))
    (setf (getf optional name) (list :name name
                                     :opt (or short-form long-form)
                                     :alt-opt (and short-form long-form)
                                     :description description
                                     :count count
                                     :type type
                                     :default default
                                     :show-default show-default)
          (getf values name) default))
  (values))

(defun add-positional (parser name description
                       &key (count 1) type)
  "Add a positional option to parser.

The option name and description are required. The name is a keyword to
set and retreive the values. The description is printed in the help
message.

The count (number of arguments this option holds) defaults to 1. The
type defaults to nil.
"
  (check-type name keyword)
  (check-count count)
  (when (numberp count) (assert (> count 0) nil '<option-invalid-count>))
  (with-slots (positional-keys positional values) parser
    (assert (not (getf positional name)) nil '<option-not-unique>)
    (push name positional-keys)
    (setf (getf positional name) (list :name name
                                       :description description
                                       :count count
                                       :type type)
          (getf values name) nil))
  (values))

(defun print-help (parser &key (stream *standard-output*) (screen-width *default-width*))
  (let ((opt-header) (opt-body) (pos-header) (pos-body))
    (with-slots (name description optional positional) parser
      ;; collect lines for header and body
      (loop for (name plst) on optional by #'cddr
            do (let ((opt (getf plst :opt))
                     (alt-opt (getf plst :alt-opt))
                     (description (getf plst :description))
                     (count (getf plst :count))
                     (type (getf plst :type))
                     (default (getf plst :default))
                     (show-default (getf plst :show-default)))
                   (cond
                     ((numberp count)
                      (push (format nil " [~A~@[,~A~]~V{ ~A~:*~}]" opt alt-opt count (list type))
                            opt-header)
                      (push (format nil "  ~A~@[, ~A~]~V{ ~A~:*~}~
                                        ~{~%    ~A~}~:[~; Default value ~A.~]~%"
                                    opt alt-opt count (list type) 
                                    (break-line description :width screen-width)
                                    show-default default)
                            opt-body))
                     ((and (characterp count) (char= #\+))
                      (push (format nil " [~A~@[,~A~] ~A [~A ...]]" opt alt-opt type type)
                            opt-header)
                      (push (format nil "  ~A~@[, ~A~] ~A [~A ...]~
                                        ~{~%    ~A~}~:[~; Default value ~A.~]~%"
                                    opt alt-opt type type (break-line description :width screen-width)
                                    show-default default)
                            opt-body)))))

      (loop for (name plst) on positional by #'cddr ;; positional
            do (let ((type (getf plst :type))
                     (description (getf plst :description))
                     (count (getf plst :count)))
                 (cond
                   ((numberp count)
                    (push (format nil "~V{ ~A~:*~}" count (list name)) pos-header)
                    (push (format nil "  ~A [~A]~{~%    ~A~} Takes ~r argument~:p.~%"
                                  name type (break-line description :width screen-width) count)
                          pos-body))
                   ((and (characterp count) (char= #\+))
                    (push (format nil "~A [~A ...]" name name) pos-header)
                    (push (format nil "  ~A [~A]~{~%    ~A~} Takes multiple arguments.~%"
                                  name type (break-line description :width screen-width))
                          pos-body)))))

      ;; print header
      (let ((ip 0)
            (indent (length name)))
        (flet ((print-header (lst)
                 (loop for str in lst
                       for width = (length str)
                       when (> (+ ip width) screen-width)
                         do (format stream "~%~V@{~A~:*~}" indent #\Space)
                            (setf ip indent)
                       do (format stream "~A" str)
                          (incf ip width))))
          (format stream "~%~A" name)
          (print-header opt-header)
          (print-header pos-header)))

      ;; description
      (format stream "~2%~{~A~%~}~%" (break-line description :width screen-width))

      ;; body
      (when pos-body
        (format stream "~%Positional Arguments:~2%")
        (format stream "~{~A~%~}" pos-body))

      (when opt-body
        (format stream "~%Optional Arguments:~2%")
        (format stream "~{~A~%~}" opt-body))

      ;; footer
      (format stream "~%"))))

(defun print-help-and-exit (parser &key (stream *standard-output*))
  (print-help parser :stream stream)
  (uiop:quit 0 t))

(defun print-error-and-exit (parser errstr &key (stream *error-output*) (code 1))
  (print-help parser :stream stream)
  (format stream "Error: ~A~2%" errstr)
  (uiop:quit code t))

;;;-----------------------------------------------------------------------------
(defun parser (name description)
  "Return `parser` instance.

The name and description are required. Both are used in the help
message.
"
  (let ((parser (make-instance 'parser 
                               :name name 
                               :description description)))
    (add-optional parser :help "-h" "--help" "Display this help and exit."
                         :count 0
                         :type nil
                         :show-default nil)
    parser))

(defun read-arg (str type &key end (max-length 80))
  (let ((*read-eval* nil)
        (end (if end end (min max-length (length str)))))
    (multiple-value-bind (value p1)
        (read-from-string str nil nil :start 0 :end end)
      (if type
          (values (coerce value type) p1)
          (values value p1)))))

(defun convert-arg (arg type &key end)
  (case type
    (integer (parse-integer arg :junk-allowed t))
    (string arg)
    (null arg)
    (pathname (merge-pathnames arg))
    (otherwise (read-arg arg type :end end))))

(defun set-option (parser iarg args)
  (flet ((fetch-args (count type)
           (let ((n-available (- (length args) iarg))
                 (vlst))
             (cond
               ((and (numberp count) (zerop count)) (setf vlst t))
               ((numberp count)
                (assert (>= n-available count) nil '<opt-missing-argument>)
                (if (= count 1)
                    (prog1
                      (setf vlst (convert-arg (svref args iarg) type))
                      (incf iarg))
                    (setf vlst 
                          (loop for i from 0 below count
                                for a = (svref args iarg)
                                collect (convert-arg a type)
                                do (incf iarg)))))
               (t
                (assert (> n-available 0) nil '<opt-missing-argument>)
                (setf vlst
                      (loop for i from 0 below n-available
                            for a = (svref args iarg)
                            until (char= #\- (char a 0))
                            collect (convert-arg a type)
                            do (incf iarg)))))
             vlst)))
    (with-slots (optional-keys optional positional-keys positional values unknowns) parser
      (let ((arg (svref args iarg)))
        (incf iarg)
        (if (or (short-form-p arg) (long-form-p arg))
            ;; optional
            (let ((name (gethash arg optional-keys)))
              (if name
                  (let* ((plst (getf optional name))
                         (count (getf plst :count))
                         (type (getf plst :type))
                         (val (fetch-args count type)))
                    (setf (getf values name) val))
                  (push arg unknowns)))
            ;; positional
            (loop for name in (reverse positional-keys)
                  for plst = (getf positional name)
                  for count = (getf plst :count) 
                  and type = (getf plst :type)
                  and vals = (getf values name)
                  when (or (and (numberp count) (< (length vals) count))
                           (characterp count))
                    do (return 
                         (if (and (numberp count) (= 1 count))
                             (setf (getf values name) (convert-arg arg type))
                             (push (convert-arg arg type) (getf values name))))
                  finally (push arg unknowns))))
        iarg)))

(defun process-args (parser argv)
  "Process the command line arguments and return a property list with the option names as keys.

The command line arguments are assumed to be collected into the supplied
list `argv`.
"
  (let ((nargs (length argv))
        (args (coerce argv 'vector)))
    (loop with iarg = 0
          while (< iarg nargs)
          do (setf iarg (set-option parser iarg args)))
    (with-slots (values unknowns) parser
      (cond
        (unknowns
         (print-error-and-exit parser 
                               (format t "Error! Unknown arguments: ~A~%" unknowns)))
        ((getf values :help)
         (print-help-and-exit parser))
        (t
         values)))))

;;;-----------------------------------------------------------------------------
;;; End
