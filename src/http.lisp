;;;; A minimal implementation of the subset of HTTP request and response
;;;; processing needed by the game server.

(in-package :jade)

(defstruct http-request
  (method "GET")
  (path "/")
  (version "HTTP/1.1")
  (headers nil))

(defun http-request-get-header (request header-name)
  (cdr (assoc header-name (http-request-headers request) :test #'string-equal)))

(defun split-names-values (items separator)
  (mapcar #'(lambda (item)
              (destructuring-bind (name value)
                  (cl-ppcre:split separator item :limit 2)
                (cons name value)))
          items))

(defun parse-http-request (data)
  "Given a byte vector representing a raw http request, parses the request and
returns an http-request instance."
  (let* ((text (babel:octets-to-string data :encoding :iso-8859-1))
         (lines (cl-ppcre:split "\\r\\n" text :sharedp t)))
    (destructuring-bind (method path version) (cl-ppcre:split " " (first lines))
      (make-http-request :method method
                         :path path
                         :version version
                         :headers (split-names-values (rest lines) ":\\s*")))))

(defun http-request-get-cookies (request)
  "Returns an alist mapping cookie names to values, as found in the Cookie
header for `request`."
  (let ((cookies (http-request-get-header request "Cookie")))
    (split-names-values (cl-ppcre:split ";\\s+" cookies) "=")))

(defstruct http-response
  (version "HTTP/1.1")
  (status 200)
  (reason "OK")
  (headers nil)
  (body nil))

(defun http-response-add-header (response name value)
  (push (cons name value) (http-response-headers response)))

(defun http-response-set-cookie (response name value)
  (http-response-add-header response "Set-Cookie" (format nil "~a=~a" name value)))

(defun encode-http-response (response)
  (let ((eol (coerce #(#\Return #\Linefeed) 'string))
        (body (http-response-body response)))
    (babel:string-to-octets
     (with-output-to-string (result)
       (format result "~a ~a ~a~a"
               (http-response-version response)
               (http-response-status response)
               (http-response-reason response)
               eol)
       (dolist (header (http-response-headers response))
         (format result "~a: ~a~a" (car header) (cdr header) eol))
       ;; FIXME: The body content length doesn't handle changes due to encoding.
       (format result "Content-Length: ~d~a" (if body (length body) 0) eol)
       (write-string eol result)
       (when body
         (write-string body result))))))
