(in-package :cl-user)

(defpackage :uri-template.test.system
  (:use :cl :asdf))

(in-package :uri-template.test.system)

(defsystem :uri-template.test
  :version "0.0.1"
  :description "Tests for uri-template"
  :maintainer "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :author "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :licence "MIT"
  :depends-on ("uri-template"
               "prove"""
               "cl-interpol")
  :serial t
  :components ((:module "t"
                :serial t
                :components
                ((:file "package")
                 (:test-file "dummy")
                 (:test-file "variables")
                 (:test-file "expander"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
