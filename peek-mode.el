;;; peek-mode.el --- Serve buffers live over HTTP with elnode backend
;;; (i.e., impatient-mode ported to elnode)
 
;; LICENSE
 
;; Author: Erik Iverson <erik@sigmafield.org>
;; Version: 0.1
;; URL: https://github.com/erikriverson/peek-mode
;; Package-Requires: ((elnode "0.9.8.1"))
 
;;; Commentary:
 
;; peek-mode is a minor mode that publishes live Emacs buffers through
;; an elnode server under
;; http://<server>:<port>/peek/<buffer>. Turning on peek-mode in a
;; buffer will publish it. To unpublish a buffer, toggle peek-mode
;; off.
 
;; peek-mode is (very very!) largely based on Brian Taylor's
;; <el.wubo@gmail.com> impatient-mode
;; <https://github.com/netguy204/imp.el> However, impatient-mode does
;; not use elnode, but rather a different emacs httpd backend
;; <https://github.com/skeeto/emacs-http-server>. I consider peek-mode
;; as "impatient-mode ported to elnode".
 
;; Start the elnode server (`elnode-start') and visit
;; http://<server>:<port>/peek/. There will be a listing of all the
;; buffers that currently have peek-mode enabled. You can evaluate the
;; line below to start the elnode server on localhost:8008 with the
;; proper dispatcher, assuming the code in this file is available by
;; having loaded it. 
;; (elnode-start 'peek-mode-dispatcher-handler :port 8008 :host "localhost")

(defun peek-serve-buffer-list (httpcon)
  "Serve a list of peekable buffers."
  (elnode-send-html httpcon (concat "<html><head>\n
     <title>peek-mode buffer list</title>
     </head><body><h1>Peekable Buffers</h1>\n<hr/><ul>"
                    (mapconcat
                     (lambda(buffer)
                       (format "<li><a href=\"live/%s/\">%s</a></li>\n"
                                       (url-hexify-string (buffer-name buffer))
                                       (url-insert-entities-in-string (buffer-name buffer))))
                     (peek-buffer-list) "\n")
                    "</ul>\n<hr/>"
                    "Enable <code>peek-mode</code> in buffers to add them to this list."
                    "</body></html>")))
 
(defun peek-live-buffer (httpcon)          
  "Serve up the shim that lets us watch a buffer change"
  (let* ((path (elnode-http-pathinfo httpcon))
                (index (expand-file-name "index.html" peek-shim-root))
         (parts (cdr (split-string path "/")))
         (buffer-name (nth 2 parts))
;;         (file (httpd-clean-path (mapconcat 'identity (nthcdr 3 parts) "/")))
         (file (mapconcat 'identity (nthcdr 3 parts) "/"))
         (buffer (get-buffer buffer-name))
         (buffer-file (buffer-file-name buffer))
         (buffer-dir (and buffer-file (file-name-directory buffer-file))))
 
    (cond
     ((equal (file-name-directory path) "/peek/live/")
      (elnode-send-redirect httpcon (concat path "/")))
     ((not (peek-buffer-enabled-p buffer)) (peek-private httpcon buffer-name))
     ((and (not (string= file "")) buffer-dir)
      (let* ((full-file-name (expand-file-name file buffer-dir))
             (live-buffer (remove-if-not
                           (lambda (buf) (equal full-file-name (buffer-file-name buf)))
                           (peek-buffer-list))))
        (add-to-list 'peek-related-files full-file-name)
        (if (not live-buffer)
                    (elnode-send-file httpcon full-file-name))))
     (t (peek-buffer-enabled-p buffer)
                (elnode-send-file httpcon index)))))
 
(defconst peek-mode-urls
  '(("peek/$" . peek-buffer-list-handler)
    ("peek/buffer/.*$" . peek-long-poll-handler)
    ("peek/live/.*$" . peek-live-buffer-handler)))
 
(defun peek-buffer-list-handler (httpcon)
   (peek-serve-buffer-list httpcon))
 
(defun peek-long-poll-handler (httpcon)
   (peek-long-poll-receive httpcon))
 
(defun peek-live-buffer-handler (httpcon)
   (peek-live-buffer httpcon))
 
(defun peek-mode-dispatcher-handler (httpcon)
  (elnode-dispatcher httpcon peek-mode-urls))
 
(defun peek-send-state (httpcon)
  (let ((id (number-to-string peek-last-state))
        (buffer (current-buffer)))
    (elnode-http-start httpcon 200 '("Cache-Control" . "no-cache")
                                       '("Content-Type" . "text/plain")
                       '("Connection" . "keep-alive" ))
    (elnode-http-return httpcon
                                                (concat id " " (buffer-substring-no-properties
                                                                                (point-min) (point-max))))))
                                       (or (cdr (assoc "id" query)) "0"))))
    (if (peek-buffer-enabled-p buffer)
        (with-current-buffer buffer
          (if (equal req-last-id peek-last-state)
                      (push httpcon peek-client-list)
            (peek-send-state-ignore-errors httpcon)))
      (peek-private httpcon buffer-name)))
  (elnode-defer-now 'peek-long-poll-receive))
 
(provide 'peek-mode)
 
;;; peek-mode.el ends here
