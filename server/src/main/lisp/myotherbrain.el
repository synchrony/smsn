(eval-when-compile (require 'cl))
(require 'json)


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

(defun visit-item ()
    (interactive)
    (let ((atom-id (car (last (find-id)))))
        (if atom-id
            (url-retrieve
                (concat "http://localhost:8182/josh/myotherbrain/read-notes?root=" atom-id) 'receive-view))))


(defun visit-meta ()
    (interactive)
    (let ((assoc-id (car (find-id))))
        (if assoc-id
            (url-retrieve
                (concat "http://localhost:8182/josh/myotherbrain/read-notes?root=" assoc-id) 'receive-view))))

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

(defun receive-view (status)
    (let ((json-object-type 'hash-table))
        (let ((json (json-read-from-string (strip-http-headers (buffer-string)))))
            (let (
                (id (gethash "root" json))
                (view (gethash "view" json)))
                    (switch-to-buffer (view-name id))
                    (erase-buffer)
                    (insert view)
                    (beginning-of-buffer)))))


(global-set-key (kbd "C-c i") 'visit-item)
(global-set-key (kbd "C-c m") 'visit-meta)
(global-set-key (kbd "C-c d") 'my-debug)


(add-hook
     'after-save-hook
     (lambda () (message "you have saved!")))


(provide 'myotherbrain)
