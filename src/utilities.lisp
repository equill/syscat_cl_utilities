(in-package #:syscat-cl-utilities)

(defun post-encode-payload (payload)
  "Transform a list of dotted pairs of strings into a POST-encoded string.
  The intent is to provide Drakma with pre-encoded content,
  instead of having it URL-encode the content of a PUT request.
  This also bypasses the 1024-character limit that comes with URL-encoding."
  (format nil "~{~A~^&~}"
          (mapcar #'(lambda (pair)
                      (format nil "~A=~A"
                              (drakma:url-encode (car pair) :UTF-8)
                              (drakma:url-encode (cdr pair) :UTF-8)))
                  payload)))

(defun make-http-request (url &key (request-method :POST) parameters)
  "Make an HTTP request, defaulting to the POST method."
  (declare (type string url)
           (type keyword request-method)
           (type list parameters))
  (log:debug "Making HTTP request ~A" url)
  (multiple-value-bind (body status-code)
    (drakma:http-request url
                         :method request-method
                         :parameters parameters)
    (cond
      ;; Successful upload
      ((= status-code 201)
       (log:debug "Created successfully")
       t)
      ;; Resource is already there
      ((= status-code 200)
       (log:debug "Resource or relationship is already present")
       t)
      ;; Something else
      (t (log:warn "Unexpected response: ~A - ~A" status-code body)
         nil))))

(defun sanitise-uid (uid)
  "Replace UID-unfriendly characters in UIDs with something safe.
   Expects a string and returns another string."
  (cl-ppcre:regex-replace-all "[/ ]" uid "_"))
