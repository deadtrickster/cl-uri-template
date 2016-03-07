(asdf:defsystem :uri-template
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :depends-on ("alexandria" "split-sequence" "cl-ppcre" "collectors" "ia-hash-table")
  :author "Ilya Khaprov <ilya.kharpov@publitechs.com>"
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "expander"))))
  :description "RFC6570 templates")
