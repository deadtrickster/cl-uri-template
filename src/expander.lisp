(in-package #:uri-template)

(define-constant +operator+ "+#./;?&|!@" :test #'equalp)
(define-constant +reserved+ ":/?#[]@!$&'()*+,;=" :test #'equalp)

(unless (boundp '+undef+)
  (defconstant +undef+ (gensym)))

(unless (boundp '+template+)
  (defconstant +template+ (load-time-value (ppcre:create-scanner "{([^\\}]+)}"))))

(defun non-empty-collection-p (value)
  (and (or (typep value 'list)
           (typep value 'vector))
       (not (stringp value))
       (> (length value) 0)))

(defun all-registers-as-strings (regex string)
  (with-collector-output (add-register)
    (ppcre:do-scans (s e rs re regex string)
      (add-register (subseq string (aref rs 0) (aref re 0))))))

;; FROM CL-PPCRE
(declaim (inline nsubseq))
(defun nsubseq (sequence start &optional (end (length sequence)))
  "Returns a subsequence by pointing to location in original sequence."
  (make-array (- end start)
              :element-type (array-element-type sequence)
              :displaced-to sequence
              :displaced-index-offset start))

(all-registers-as-strings +template+ "http:www{.domain*}{/top,next}{?q:20}")

(defun variables (template)
  (with-collector-output (add-var)
    (loop for varlist in (all-registers-as-strings +template+ template) do
             (when (find (aref varlist 0) +operator+)
               (setf varlist (subseq varlist 1)))
             (loop for var in (split-sequence #\, varlist :remove-empty-subseqs t) do
                  (setf var (first (split-sequence #\: var :remove-empty-subseqs t)))

                  (when (ends-with #\* var)
                    (setf var (subseq var 0 (1- (length var)))))

                  (add-var var)))))

(assert (equalp '("domain" "top" "next" "q") (variables "http:www{.domain*}{/top,next}{?q:20}")))

(defmethod escape ((str string) safe &optional prefix)
  "URI encodes/escapes the given string."
  (with-output-to-string (s)
    (loop for c across (flexi-streams:string-to-octets (if (and prefix (< prefix (length str))) (subseq str 0 prefix) str) :external-format :utf-8)
          do (if (or (find (code-char c) safe)
                     (<= 48 c 57)
                     (<= 65 c 90)
                     (<= 97 c 122)
                     (find c '(45 95 46 126)))
              (write-char (code-char c) s)
              (format s "%~2,'0x" c)))))

(defmethod escape ((value number) safe &optional prefix)
  (princ-to-string value))

(defun join (delimiter strings)
  (when strings
    (reduce (lambda (a b)
              (concatenate 'string a (string delimiter) b))
            strings)))

(defun varlist-from-register (target-string reg-start reg-end)
  (if (find (aref target-string reg-start) +operator+)
      (values (aref target-string reg-start)
              (subseq target-string (1+ reg-start) reg-end))
      (values nil
              (subseq target-string reg-start reg-end))))

(defun safe-operator-p (operator)
  (case operator
    (#\+ t)
    (#\# t)))

(defun varlist-varspecs (varlist)
  (split-sequence #\, varlist :remove-empty-subseqs t))

(defun parse-varspec (varspec)
  (let* ((explode (eql #\* (aref varspec (1- (length varspec)))))
         (without-explode (if explode (subseq varspec 0 (1- (length varspec))) varspec))
         (colon-pos (position #\: varspec))
         (prefix (when colon-pos
                   (parse-integer without-explode :start (1+ colon-pos))))
         (without-colon (if colon-pos
                            (subseq without-explode 0 colon-pos)
                            without-explode)))
    (values without-colon explode prefix)))

(defun start-and-joiner-for-operator (operator)
  (case operator
    (#\+ (values "" #\,))
    (#\# (values (string operator) #\,))
    (#\? (values (string operator) #\&))
    (#\& (values "&" operator))
    ((()) (values "" #\,))
    (t (values (string operator) operator))))

(defun quote-collection (collection safe &optional prefix)
  (map 'list (lambda (x)
               (escape x safe prefix))
       collection))

(defun quote-hash-table (ht safe key-value-del &optional delimiter)
  (join (or delimiter key-value-del)
        (with-collector-output (add-kv)
          (maphash (lambda (key value)
                     (when value
                       (add-kv
                        (format nil "~a~a~a"
                                (escape key safe)
                                key-value-del
                                (escape value safe)))))
                   ht))))

(defmethod empty-value-p ((value sequence))
  (= 0 (length value)))

(defmethod empty-value-p ((value hash-table))
  (= 0 (hash-table-count value)))

(defmethod empty-value-p (value)
  nil)

(defun to-string-path (value explode prefix operator safe)
  (cond
    ((eql value +undef+) nil)
    ((eql value :null) nil)
    ((equal "" value) "")
    ((empty-value-p value) nil)
    ((non-empty-collection-p value)
     (join (if explode operator #\,) (quote-collection value safe)))
    ((hash-table-p value)
     (if explode
         (quote-hash-table value safe #\= operator)
         (quote-hash-table value safe #\,)))
    (t (escape value safe prefix))))

(defun to-string-query (varname value explode prefix safe)
  (cond
    ((eql value +undef+) nil)
    ((eql value :null) nil)
    ((empty-value-p value)
     (concatenate 'string varname "="))
    ((non-empty-collection-p value)
     (if explode
         (join "&" (map 'list (lambda (v) (concatenate 'string varname "=" (escape v safe))) value))
         (concatenate 'string varname "=" (join #\, (quote-collection value safe)))))
    ((hash-table-p value)
     (if explode
         (quote-hash-table value safe #\= #\&)
         (concatenate 'string varname "=" (quote-hash-table value safe #\,))))
    (t (concatenate 'string varname "=" (escape value safe prefix)))))

(defun to-string-semi (varname value explode prefix safe)
  (cond
    ((eql value +undef+) nil)
    ((eql value :null) nil)
    ((empty-value-p value)
     varname)
    ((non-empty-collection-p value)
     (if explode
         (join ";" (map 'list (lambda (v) (concatenate 'string varname "=" (escape v safe))) value))
         (concatenate 'string varname "=" (join #\, (quote-collection value safe)))))
    ((hash-table-p value)
     (if explode
         (quote-hash-table value safe #\= #\;)
         (concatenate 'string varname "=" (quote-hash-table value safe #\,))))
    (t (concatenate 'string varname "=" (escape value safe prefix)))))

(defun to-string (value explode prefix safe)
  (cond
    ((eql value +undef+) nil)
    ((eql value :null) nil)
    ((equal "" value) "")
    ((empty-value-p value) nil)
    ((non-empty-collection-p value)
     (join #\, (quote-collection value safe)))
    ((hash-table-p value)
     (if explode
         (quote-hash-table value safe #\= #\,)
         (quote-hash-table value safe #\,)))
    (t (escape value safe prefix))))

(defgeneric expand-operator (operator varname value explode prefix))

(defmethod expand-operator ((operator (eql #\.)) varname value explode prefix)
  (to-string-path value explode prefix operator ""))
(defmethod expand-operator ((operator (eql #\/)) varname value explode prefix)
  (to-string-path value explode prefix operator ""))

(defmethod expand-operator ((operator (eql #\?)) varname value explode prefix)
  (to-string-query varname value explode prefix ""))
(defmethod expand-operator ((operator (eql #\&)) varname value explode prefix)
  (to-string-query varname value explode prefix ""))

(defmethod expand-operator ((operator (eql #\;)) varname value explode prefix)
  (to-string-semi varname value explode prefix ""))

(defmethod expand-operator ((operator (eql nil)) varname value explode prefix)
  (to-string value explode prefix ""))
(defmethod expand-operator ((operator (eql #\+)) varname value explode prefix)
  (to-string value explode prefix +reserved+))
(defmethod expand-operator ((operator (eql #\#)) varname value explode prefix)
  (to-string value explode prefix +reserved+))

(defun expand (template variables)
  (flet ((sub (target-string start end match-start match-end reg-starts reg-ends)
           (declare (ignore start end match-start match-end))
           (multiple-value-bind (operator varlist)
               (varlist-from-register target-string (aref reg-starts 0) (aref reg-ends 0))
             (let ((varspecs (varlist-varspecs varlist))
                   retval)
               (multiple-value-bind (start joiner) (start-and-joiner-for-operator operator)
                 (loop for varspec in varspecs do
                       (multiple-value-bind (varname explode prefix) (parse-varspec varspec)
                         (let* ((value (gethash varname variables +undef+))
                                (expanded (expand-operator operator varname value explode prefix)))
                           (when expanded
                             (push expanded retval)))))
                 (if retval
                     (concatenate 'string start (join joiner (reverse retval)))
                     ""))))))
    (ppcre:regex-replace-all +template+ template #'sub)))
