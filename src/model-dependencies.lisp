(in-package #:syscat-cl-utilities)

;; Walk an ASDF system's dependencies, and make sure they're all in Syscat as libraries, plus
;; - connection to the relevant SPDX license definition
;; - connection to their author
;; - bidirectional links between each dependency pair


;; Hints to self:
;; - asdf:find-system
;; - asdf:system-depends-on
;; - asdf:system-license

(defun ensure-resource-exists (syscat-url resourcetype attributes)
  "Ensure that the specified resource exists, and returns t on success.
   This function assumes no authentication is required.
   If the specified resource already exists, this function does not update any of its attributes.
   `syscat-url` should be in the form http://syscat.local:4952/, complete with the trailing `/`."
  (declare (type string syscat-url resourcetype)
           (type list attributes))
  ;; Pre-extract the UID, because it's referred to seeral times,
  ;; and this code is not performance-critical.
  (let ((uid (cdr (assoc "uid" attributes :test #'equal)))
        (url (format nil "~A/raw/v1/~A" syscat-url resourcetype)))
    (log:info "Ensuring that ~A resource '~A' is present." resourcetype uid)
    (log:debug "Using URL ~A" url)
    ;; Try to upload it, and store the responses for checking afterward
    (make-http-request url :parameters attributes)))

(defun ensure-relationship-exists (syscat-url source-path relationship target-path)
  "Ensure this relationship exists between those resources."
  (declare (type string source-path relationship target-path))
  (log:debug "Ensuring relationship ~A exists from resource ~A to resource ~A"
             relationship source-path target-path)
  (make-http-request (format nil "~A/raw/v1~A/~A" syscat-url source-path relationship)
                     :parameters `(("target" . ,target-path))))

(defun upload-system (syscat-url sysdef)
  "Take an ASDF system, and upload its details to the supplied Syscat base URL.
   Does not currently deal with authentication."
  (declare (type string syscat-url)
           (type asdf/system:system sysdef))
  (let ((system-name (asdf:primary-system-name sysdef))
        (license (asdf:system-license sysdef))
        (author (asdf:system-author sysdef)))
    (log:debug "Attempting to update ~A with system ~A, with license ~A and author '~A'"
               syscat-url
               system-name
               license
               author)
    (ensure-resource-exists syscat-url
                            "ASDFsystems"
                            `(("uid" . ,system-name)))))

(defun import-dependencies (syscat-url packagename)
  "Walk a package's dependencies, and ensure they're all imported into Syscat.
   Assumes the parent package is an application, unless :library-p is set to t.
   Assumes all its dependencies are libraries."
  (declare (type string syscat-url)
           (type (or string keyword) packagename))
  (log:debug "Ensuring CommonLisp is recorded as a programming language.")
  (ensure-resource-exists syscat-url "ProgrammingLanguages" '(("uid" . "CommonLisp")))
  (let ((sysdef (asdf:find-system packagename)))
    (log:debug "Attempting to import dependencies for system ~A" (asdf:primary-system-name sysdef))
    (upload-system syscat-url sysdef)))
