(in-package #:syscat-cl-utilities)

;; Create ASDFsystems entries for each license definition in the SPDX repo
;;
;; Works by iterating over the JSON definition files in the `json/details` subdirectory of
;; the SPDX repo at `https://github.com/spdx/license-list-data`.
;;
;; Currently assumes no authentication is required.

(defclass licensedef ()
  ((license-id :initarg :license-id
               :reader license-id
               :type string
               :initform (error ":license-id arg is mandatory.")
               :documentation "The canonical short form of the license's name, which is used in Syscat as its UID.")
   (fullname :initarg :fullname
             :reader fullname
             :type (or null string)
             :documentation "The full name should use the title found in the license file or be consistent with naming from other well-known sources.")
   (standard-header :initarg :standard-header
                    :reader standard-header
                    :type (or null string)
                    :documentation "Should only include text intended to be put in the header of source files or other files as specified in the license or license appendix when specifically delineated.")
   (osi-approved :initarg :osi-approved
                 :reader osi-approved
                 :type boolean
                 :documentation "If the license is OSI-approved, this field will indicate 'Y' and otherwise left blank. Thus, `nil` means only that it's not positively known to have been approved.")
   (text :initarg :text
         :reader text
         :type string
         :documentation "The full text of the license or exception"))
  (:documentation "Intermediate representation of an SPDX license,
                   from which details are uploaded into Syscat.
                   The value of the license-id slot is used as the UID;
                   its other fields should match the attributes in Syscat."))


(defun enumerate-license-files (path)
  "Take a path to the parent directory of the SPDX repo,
   and return a list of paths to the JSON definition files.
   Note that `~` does _not_ get auto-rendered to the user's home directory."
  (declare (type (or string pathname) path))
  (log:debug "Enumerating JSON license files under ~A" path)
  (directory
    (make-pathname :name :wild
                   :type "json"
                   :directory (format nil "~A/json/details/"
                                      (string-right-trim "/" path)))))

(defun digest-license-file (path)
  "Given the path to an SPDX license file in JSON format,
   returns an instance of `licensedef` populated with details from that file."
  (declare (type (or string pathname) path))
  (log:debug "Attempting to digest SPDX license file at path ~A" path)
  (let ((parsed-file (with-open-file (infile path)
                       (shasht:read-json infile))))
    ;; Wrap this in some sanity-checks
    (if (and parsed-file
             (hash-table-p parsed-file))
        (if (stringp (gethash "licenseId" parsed-file))
            ;; The happy path: this file contains a valid (enough) definition
            (make-instance 'licensedef
                           :license-id (gethash "licenseId" parsed-file)
                           :fullname (gethash "fullName" parsed-file)
                           :standard-header (gethash "standardLicenseHeader" parsed-file)
                           :osi-approved (gethash "isOsiApproved" parsed-file)
                           :text (gethash "licenseText" parsed-file))
            ;; The file is valid JSON, but there's no licenseId field
            (log:warn "No valid licenseId field found in file ~A" path))
        ;; The file contained no sensible JSON object definition
        (log:warn "No JSON object definition found in file ~A" path))))

(defun ensure-license-is-present (syscat-url license)
  "Ensure that an SPDXlicenses resource exists with a UID corresponding to this one's license-id.
   Does not update attributes of existing resources.
   Assumes no authentication is required."
  (declare (type string syscat-url)
           (type licensedef license))
  (log:debug "Ensuring license '~A' is defined in Syscat at '~A'"
             (license-id license)
             syscat-url)
  ;; Initialise the parameters with those we know we'll have
  (let ((params `(("uid" . ,(license-id license))
                  ("OSIapproved" . ,(if (osi-approved license)
                                        "True"
                                        "False")))))
    ;; Add any other parameters we also have
    (when (not (null (fullname license)))
      (nconc params `(("FullName" . ,(fullname license)))))
    (when (not (null (standard-header license)))
      (nconc params `(("StandardLicenseHeader" . ,(standard-header license)))))
    (when (not (null (text license)))
      (nconc params `(("Text" . ,(text license)))))
    ;; Make the HTTP request
    (multiple-value-bind (body status-code)
      (drakma:http-request (format nil "~A/raw/v1/SPDXlicenses" syscat-url)
                           :method :POST
                           :content (post-encode-payload params)
                           :form-data nil
                           :external-format-in :UTF-8
                           :external-format-out :UTF-8)
      ;; Handle the response parameters accordingly
      (cond
        ;; Success!
        ((= 201 status-code)
         (log:info "SPDX License ~A created" (license-id license)))
        ((= 200 status-code)
         (log:info "SPDX License ~A already present" (license-id license)))
        ;; Anything else is an unexpected condition
        (t (error (format nil "Unexpected response from server: ~D - ~A"
                          status-code body)))))))


;; Pull it all together
(defun import-spdx-licenses (syscat-url path)
  "Given the path to the top-level directory of the SPDX license-list-data repo,
   ensure there's an SPDXlicenses resource corresponding to each definition in the repo."
  (declare (type string syscat-url)
           (type (or string pathname) path))
  (mapcar (lambda (filepath)
            (ensure-license-is-present (string-right-trim "/" syscat-url)
                                       (digest-license-file filepath)))
          (enumerate-license-files path)))
