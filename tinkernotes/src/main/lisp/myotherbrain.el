(eval-when-compile (require 'cl))
(require 'json)

;; Required global variables: tinkernotes-rexster-host, tinkernotes-rexster-port, tinkernotes-rexster-graph
;;
;; For example:
;;
;;     (defun tinkernotes ()
;;         (defvar tinkernotes-rexster-host "localhost")
;;         (defvar tinkernotes-rexster-port "8182")
;;         (defvar tinkernotes-rexster-graph "tinkernotes"))

;; from Emacs-w3m
(defun w3m-url-encode-string (str &optional coding)
  (apply (function concat)
         (mapcar
          (lambda (ch)
            (cond
             ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
              (char-to-string ch))      ; printable
             (t
              (format "%%%02X" ch))))   ; escape
          ;; Coerce a string to a list of chars.
          (append (encode-coding-string str (or coding 'iso-2022-jp))
                  nil))))

(defun current-line ()
    (interactive)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun find-id ()
    (let ((line (current-line)))
        (if (string-match "^\([0-9A-Za-z+/]*:[0-9A-Za-z+/]*\)" line)
            (let (
                (i1 (string-match "\(" line))
                (i2 (string-match ":" line))
                (i3 (string-match "\)" line)))
                (let (
                    (s1 (substring line (+ 1 i1) i2))
                    (s2 (substring line (+ 1 i2) i3)))
                    (let (
                        (assoc-id (if (< 0 (length s1)) s1 nil))
                        (atom-id (if (< 0 (length s2)) s2 nil)))
                        (list assoc-id atom-id))))
            (list nil nil))))

(defun base-url ()
    (concat "http://" tinkernotes-rexster-host ":" tinkernotes-rexster-port "/" tinkernotes-rexster-graph "/myotherbrain/"))

(defun visit-item ()
    (interactive)
    (let ((atom-id (car (last (find-id)))))
        (if atom-id
            (url-retrieve
                (concat (base-url) "view-notes?root=" (w3m-url-encode-string atom-id)) 'receive-view))))


(defun visit-meta ()
    (interactive)
    (let ((assoc-id (car (find-id))))
        (if assoc-id
            (url-retrieve
                (concat (base-url) "view-notes?root=" (w3m-url-encode-string assoc-id)) 'receive-view))))

(defun http-post (url args callback)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (mapconcat (lambda (arg)
                      (concat (w3m-url-encode-string (car arg))
                              "="
                              (w3m-url-encode-string (car (last arg)))))
;;                      (concat (url-hexify-string (car arg))
;;                              "="
;;                              (url-hexify-string (cdr arg))))
                    args
                    "&")))
    (url-retrieve url callback)))

(defun push-view ()
    (interactive)
    (let (
        (root-id (find-root-id (buffer-name)))
        (entity (buffer-string)))
        (http-post
            (concat (base-url) "update-notes")
            (list (list "root" root-id) (list "view" entity))
            'receive-view)))

(defun my-debug ()
    (interactive)
    (message (find-id)))


(defun strip-http-headers (entity)
    (let ((i (string-match "\n\n" entity)))
        (if (>= i 0)
            (substring entity (+ i 2))
            entity)))


(defun view-name (root-id)
    (concat "view-" root-id))

(defun find-root-id (viewname)
    (substring viewname (+ 1 (string-match "\-" viewname))))


(defun info-message (msg)
    (message (concat "Info: " msg)))

(defun error-message (msg)
    (message (concat "Error: " msg)))


(defun receive-view (status)
    (let ((json-object-type 'hash-table))
        (let ((json (json-read-from-string (strip-http-headers (buffer-string)))))
            (if status
                (let ((msg (gethash "message" json))
                    (error (gethash "error" json)))
                        (if error
                            (error-message error)
                            (error-message msg)))
                (let (
                    (root (gethash "root" json))
                    (view (gethash "view" json)))
                        (switch-to-buffer (view-name root))
                        (tinkernotes-mode)
                        (erase-buffer)
                        (insert view)
                        (beginning-of-buffer)
                        (info-message (concat "updated to view (root: '" root "')")))))))


(global-set-key (kbd "C-c i") 'visit-item)
(global-set-key (kbd "C-c m") 'visit-meta)
(global-set-key (kbd "C-c p") 'push-view)
(global-set-key (kbd "C-c d") 'my-debug)


(setq syntax-keywords
 '(
   ("^\([0-9A-Za-z+/]*:[0-9A-Za-z+/]*\)" . font-lock-doc-face)
  ))

(define-derived-mode tinkernotes-mode fundamental-mode
  (setq font-lock-defaults '(syntax-keywords))
  (setq mode-name "tinkernotes")
)


;; Uncomment only when debugging
(add-hook 'after-init-hook '(lambda () (setq debug-on-error t)))

(provide 'myotherbrain)
