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
