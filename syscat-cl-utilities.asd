;    Copyright James Fleming <james@electronic-quill.net>
;

(asdf:defsystem #:syscat-cl-utilities
  :author "James Fleming <james@electronic-quill.net>"
  :description "CL utilities for Syscat."
  :depends-on (#:log4cl
               #:drakma
               #:shasht)
  :components ((:file "package")
               (:file "utilities")
               (:file "model-dependencies" :depends-on ("package"))
               (:file "import-spdx-licenses" :depends-on ("package"
                                                          "utilities"))))
