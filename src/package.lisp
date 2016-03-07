(in-package #:cl-user)

(defpackage #:uri-template
  (:use :cl :alexandria #:split-sequence #:collectors)
  (:export #:variables
           #:expand))
