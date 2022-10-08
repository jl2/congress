;; congress.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :congress)

;; Inline functions that just call a single other function
(declaim (inline get-headers
                 api-url
                 api-key))

(defparameter *api-key-file* (asdf:system-relative-pathname :congress ".api-key")
  "Path to the config JSON file with the API key.")

(defvar *debug-stream* nil
  "Set to non-nil to log to a stream.")

(defvar *api-key* nil
  "The API key.")

(defparameter *api-host* "api.congress.gov"
  "Default URL, can be changed for testing.")

(defvar *cookies* (cl-cookie:make-cookie-jar)
  "Holds cookies exchanged with server.")

(defun api-key ()
  "Return the RideWithGPS API key, reading from the config file if necessary."
  (when (null *api-key*)
   (with-input-from-file (ins *api-key-file*)
      (setf *api-key*
            (st-json:getjso "api-key" (st-json:read-json ins)))))
  *api-key*)

(defun api-url (&rest parameters)
  (quri:make-uri :scheme "https"
                 :host *api-host*
                 :path (let ((*print-case* :downcase))
                         (format nil "/v3/~{~a~^/~}" parameters))
                 :query (list (cons "api_key" (api-key))
                              (cons "format" "json"))))

(defun get-headers ()
  "Returns headers used to make API requests."
  '(("content-type" . "application/json")))

(defun cget (&rest parameters)
  "Make a GET request to RideWithGPS and parses the JSON result.
   Calls (connect) when authentication fails (401 error)."

  (multiple-value-bind (body status response-headers uri)
      (handler-bind (
                     ;; Try reconnecting on 401 errors
                     (dex:http-request-unauthorized
                       #'dex:ignore-and-continue)

                     ;; Ignore 404 errors because they indicate empty result sets
                     (dex:http-request-not-found
                      #'dex:ignore-and-continue))
        (let ((url (apply #'api-url parameters)))
          (dex:get url
                   :headers (get-headers)
                   :cookie-jar *cookies*)))
    (values (st-json:read-json-from-string body) status response-headers uri)))

(defun fetch (url)
  (dex:get url))

(defun bill-text (congress s-or-hr number)
  (let ((text-data (congress:cget :bill congress s-or-hr number :text)))
    (congress:fetch
     (getjso "url"
             (find "Formatted Text"
                   (getjso "formats"
                           (car (getjso "textVersions" text-data)))
                   :key (alexandria:curry #'getjso "type")
                   :test #'string=)))))

(defun add-bill-to-markov-table (mtable congress s-or-hr number)
  (let* ((pre-text (bill-text congress s-or-hr number))
         (head "<html><body><pre>")
         (tail "</pre></body></html>")
         (head-len (length head))
         (tail-len (length tail)))
    (markov:add-to-table (subseq pre-text head-len (- (length pre-text) tail-len))
                         mtable)))
