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


;; HELPER CODE ;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun strip-http-headers (entity)
    (let ((i (string-match "\n\n" entity)))
        (if (>= i 0)
            (substring entity (+ i 2))
            entity)))


;; BUFFERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-line ()
    (interactive)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

;; Buffer-local variables. Given them initial, global bindings.
(setq view-depth nil)
(setq view-root nil)
(setq view-title nil)
(make-local-variable 'view-root)
(make-local-variable 'view-depth)
(make-local-variable 'view-title)

(defun get-view-depth ()
    (if view-depth view-depth 3))

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

(defun view-name (root-id)
    (concat "view-" root-id))


;; COMMUNICATION ;;;;;;;;;;;;;;;;;;;;;;;

(defun base-url ()
    (concat "http://" tinkernotes-rexster-host ":" tinkernotes-rexster-port "/" tinkernotes-rexster-graph "/myotherbrain/"))

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
                    (view (gethash "view" json))
                    (depth (string-to-number (gethash "depth" json)))
                    (title (gethash "title" json)))
                        (switch-to-buffer (view-name root))
                        (tinkernotes-mode)
                        (erase-buffer)
                        (insert view)
                        (beginning-of-buffer)
                        (setq view-root root)
                        (setq view-depth depth)
                        (setq view-title title)
                        (info-message (concat "updated to view " (view-info))))))))


;; VIEWS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun view-info ()
    (concat
        "(root: " view-root
         " :depth " (number-to-string view-depth)
         " :title \"" view-title "\")"))  ;; TODO: actuallly escape the title string

(defun request-view (root depth)
    (url-retrieve
        (concat (base-url) "view-notes?root=" (w3m-url-encode-string root) "&depth=" (number-to-string depth)) 'receive-view))

(defun visit-item ()
    (interactive)
    (let ((atom-id (car (last (find-id)))))
        (if atom-id
            (request-view atom-id (get-view-depth)))))

(defun visit-meta ()
    (interactive)
    (let ((link-id (car (find-id))))
        (if link-id
            (request-view link-id (get-view-depth)))))

(defun refresh-view ()
    (interactive)
    (if view-root
        (request-view view-root (get-view-depth))))

(defun decrease-depth ()
    (interactive)
    (if view-root
        (request-view view-root (- (get-view-depth) 1))))

(defun increase-depth ()
    (interactive)
    (if view-root
        (request-view view-root (+ (get-view-depth) 1))))


;; UPDATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun push-view ()
    (interactive)
    (let (
        (entity (buffer-string)))
        (http-post
            (concat (base-url) "update-notes")
            (list (list "root" view-root) (list "view" entity) (list "depth" (number-to-string (get-view-depth))))
            'receive-view)))


;; INTERFACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun info-message (msg)
    (message (concat "Info: " msg)))

(defun error-message (msg)
    (message (concat "Error: " msg)))

(defun my-debug ()
    (interactive)
    (message (find-id)))


(global-set-key (kbd "C-c i") 'visit-item)
(global-set-key (kbd "C-c m") 'visit-meta)
(global-set-key (kbd "C-c r") 'refresh-view)
(global-set-key (kbd "C-c -") 'decrease-depth)
(global-set-key (kbd "C-c +") 'increase-depth)
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
