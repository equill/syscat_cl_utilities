(in-package #:syscat-cl-utilities)

;; Walk an ASDF system's dependencies, and make sure they're all in Syscat as libraries, plus
;; - connection to the relevant SPDX license definition
;; - connection to their author
;; - bidirectional links between each dependency pair

;; Assumes the SPDX definitions have already been installed,
;; e.g. via `import-spdx-licenses`.


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
    (log:debug "Ensuring that ~A resource '~A' is present." resourcetype uid)
    (log:debug "Using URL ~A" url)
    ;; Try to upload it
    (make-http-request url :parameters attributes)))

(defun ensure-relationship-exists (syscat-url source-path relationship target-path)
  "Ensure this relationship exists between those resources."
  (declare (type string source-path relationship target-path))
  (log:debug "Ensuring relationship ~A exists from resource ~A to resource ~A"
             relationship source-path target-path)
  (make-http-request (format nil "~A/raw/v1~A/~A" syscat-url source-path relationship)
                     :parameters `(("target" . ,target-path))))

(defun find-dependencies-and-licenses (sysdef dependency reporthash)
  "Recursively add systems to a reporting hash, associated with their licences.
   The purpose is to fetch the set of systems, and identify which (if any) license applies to each,
   leaving it to the client function to decide what to do with the information.
   Destructively modifies the supplied hash-table, and does not directly return a useful value."
  (declare (type hash-table reporthash))
  (log:debug "Reporting license information for system ~A" (asdf:primary-system-name sysdef))
  ;; Catch the case where no license is recorded
  (let ((licensename (or (asdf:system-license (asdf:find-system sysdef)) "None")))
    (log:debug "License for system ~A: ~A" (asdf:primary-system-name sysdef) licensename)
    ;; Add this system to the list of those using its license
    (setf (gethash licensename reporthash)
          ;; The most practical way to do this is to re-request the value,
          ;; but default to the empty list, to ensure there's something to append to.
          (pushnew (asdf:primary-system-name sysdef)
                   (gethash licensename reporthash (list))
                   :test #'equal))
    ;; Now work through the dependencies, depth first.
    ;; The following section is derived from code published by Shinmera
    ;;
    ;; First, resolve the more complex specifications, for your current environment:
    (let ((dep-spec (asdf::resolve-dependency-spec sysdef dependency)))
      (when dep-spec
        ;; Recurse through its dependencies according to asdf:system-depends-on
        (dolist (dep (asdf:system-depends-on dep-spec))
          (find-dependencies-and-licenses dep-spec dep reporthash))
        ;; Recurse through its dependencies according to asdf:system-defsystem-depends-on
        (dolist (dep (asdf:system-defsystem-depends-on dep-spec))
          (find-dependencies-and-licenses dep-spec dep reporthash))))))

(defun upload-spdx-system (syscat-url sysdef)
  "Helper function to extract the relevant bits of an SPDX system,
   and ensure its definition exists in Syscat."
  (declare (type string syscat-url)
           (type asdf:system sysdef))
  (log:debug "Ensuring ASDF system ~A is present" (asdf:primary-system-name sysdef))
  ;; Initialise the system details with what we know will be there
  (let ((details `(("uid" . ,(sanitise-uid (asdf:primary-system-name sysdef))))))
    ;; Now add the may-be-there details
    (when (asdf:system-description sysdef)
      (nconc details `(("description" . ,(asdf:system-description sysdef)))))
    (when (asdf:system-long-name sysdef)
      (nconc details `(("description" . ,(asdf:system-long-name sysdef)))))
    ;; Fire ze missiles!
    (ensure-resource-exists syscat-url "ASDFsystems" details)))

(defun connect-systems-to-licenses (syscat-url license systems)
  "If the license in question is present in Syscat, connect these systems to it.
   If it's not, complain."
  (declare (type string syscat-url license)
           (type list systems))
  (log:debug "Connecting license ~A to systems ~{~A~^, ~}" license systems)
  (let ((sanitised-license (sanitise-uid license)))
    (if (make-http-request (format nil "~A/raw/v1/SPDXlicenses/~A" syscat-url sanitised-license)
                           :request-method :GET)
        (dolist (sysname systems)
          (let ((sanitised-sysname (sanitise-uid sysname)))
            ;; Connect the system to the license
            (log:debug "Attempting to connect ASDF system ~A to license ~A"
                      sanitised-sysname sanitised-license)
            (ensure-relationship-exists syscat-url
                                        (format nil "/ASDFsystems/~A" sanitised-sysname)
                                        "GOVERNED_BY"
                                        (format nil "/SPDXlicenses/~A" sanitised-license))
            ;; Connect the license to the system
            (log:debug "Attempting to connect license ~A to ASDF system ~A"
                      sanitised-license sanitised-sysname)
            (ensure-relationship-exists syscat-url
                                        (format nil "/SPDXlicenses/~A" sanitised-license)
                                        "GOVERNS"
                                        (format nil "/ASDFsystems/~A" sanitised-sysname))))
        (log:warn "License ~A not present in Syscat" sanitised-license))))

(defun import-dependencies (syscat-url packagename)
  "Walk a package's dependencies, and ensure they're all imported into Syscat.
   Assumes the parent package is an application, unless :library-p is set to t.
   Assumes all its dependencies are libraries."
  (declare (type string syscat-url)
           (type (or string keyword) packagename))
  ;; Accumulator for package/license associations
  (let ((reporthash (make-hash-table :test #'equal)))
    (log:debug "Attempting to import dependencies for system ~A" packagename)
    ;; Populate the accumulator
    (find-dependencies-and-licenses packagename packagename reporthash)
    ;; Get the information into the database
    (maphash (lambda (license systems)
               ;; Ensure each ASDF system is in there
               (dolist (sysname systems)
                 (upload-spdx-system syscat-url (asdf:find-system sysname)))
               ;; Connect each ASDF system in the list with this license.
               ;; ...except when the system didn't specify a license.
               (when (not (equal "None" license))
                 (connect-systems-to-licenses syscat-url license systems)))
             reporthash)))
