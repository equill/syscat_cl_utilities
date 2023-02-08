;    Copyright James Fleming <james@electronic-quill.net>
;

(asdf:defsystem #:syscat-cl-utilities
  :serial t
  :author "James Fleming <james@electronic-quill.net>"
  :description "CL utilities for Syscat."
  :depends-on (#:log4cl
               #:drakma
               #:cl-json)
  :components ((:file "package")
               (:file "model-dependencies")))
