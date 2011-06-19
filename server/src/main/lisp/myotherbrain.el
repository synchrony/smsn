(eval-when-compile (require 'cl))
(require 'json)


(defun visit-item ()
  (interactive)
  (let ((root (current-word)))
    (url-retrieve
        (concat "http://localhost:8182/josh/myotherbrain/read-notes?root=" root) 'receive-view)))

(defun visit-meta ()
  (interactive)
  (let ((root (current-word)))
    (url-retrieve
        (concat "http://localhost:8182/josh/myotherbrain/read-notes?root=" root) 'my-switch-to-url-buffer)))

(global-set-key (kbd "C-c i") 'visit-item)
(global-set-key (kbd "C-c m") 'visit-meta)

  
(add-hook 
     'after-save-hook 
     (lambda () (message "you have saved!")))


(defun skip-http-headers (buffer)
  "Remove HTTP headers from BUFFER, and return it. Assumes headers are indeed present!"
  (with-current-buffer buffer
    (widen)
    (goto-char (point-min))
    (search-forward "\n\n")
    (delete-region (point-min) (point))
    buffer)) 

(defun receive-view (status)
    "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
    (switch-to-buffer (skip-http-headers (current-buffer)))
    (let ((json-object-type 'hash-table))
        (let ((view (gethash "view" (json-read-from-string (buffer-string)))))
            (erase-buffer)
            (insert view))))


(provide 'myotherbrain)
