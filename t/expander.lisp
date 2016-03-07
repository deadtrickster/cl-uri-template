(in-package :uri-template.test)

(defun cases-file-path (name)
  (concatenate 'string
               (namestring (asdf:component-pathname (asdf:find-system :uri-template.test)))
               "/t/cases/"
               name))

(defun load-cases-file (name)
  (with-open-file (stream (cases-file-path name))
    (let ((yason:*parse-object-as* :alist)
          (yason:*parse-json-arrays-as-vectors* t)
          (yason:*parse-json-null-as-keyword* t))
      (ia-hash-table:alist-ia-hash-table (yason:parse stream)))))

(defmethod try-match-test-case (expanded (expansions string))
  (equal expanded expansions))

(defmethod try-match-test-case (expanded (expansions vector))
  (find expanded expansions :test #'equal))

(defun run-cases (cases)
  (maphash (lambda (level-title cases)
             (subtest level-title
               (let ((variables (gethash "variables" cases))
                     (testcases (gethash "testcases" cases)))
                 (loop for testcase across testcases
                       as expanded = (uri-template:expand (aref testcase 0) variables) do
                          (ok (try-match-test-case expanded (aref testcase 1)) (format nil "~s expanded to ~s" (aref testcase 0) expanded))))))
           cases))

(plan 2)

(subtest "Testing with spec-examples.json"
  (let ((cases (load-cases-file "spec-examples.json")))
    (run-cases cases)))

(subtest "Testing with spec-examples-by-section.json"
  (let ((cases (load-cases-file "spec-examples-by-section.json")))
    (run-cases cases)))

(subtest "Expand using alist variables"
  (is (uri-template:expand "https://api.github.com/users/{user}/repos{?type,page,per_page,sort}" '(("user" . "deadtrickster") ("type" . "public")))
      "https://api.github.com/users/deadtrickster/repos?type=public"))

;; (subtest "Testing with extended-tests.json"
;;   (let ((cases (load-cases-file "extended-tests.json")))
;;     (run-cases cases)))

(finalize)
