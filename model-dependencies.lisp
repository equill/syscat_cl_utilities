(in-package #:syscat-cl-utilities)

;; Walk an ASDF system's dependencies, and make sure they're all in Syscat as libraries, plus
;; - connection to the relevant SPDX license definition
;; - connection to their author
;; - bidirectional links between each dependency pair


;; Hints to self:
;; - asdf:find-system
;; - asdf:system-depends-on
;; - asdf:system-license

(defun upload-system (sysdef syscat-url)
  "Take an ASDF system, and upload its details to the supplied Syscat base URL. Does not currently deal with authentication."
  (declare (type string syscat-url)
           (type asdf/system:system sysdef)))
