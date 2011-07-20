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
    ;;(interactive)(read-from-minibuffer (concat "arg: " str))
    (apply (function concat)
        (mapcar (lambda (ch) (cond
                    ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
                        (char-to-string ch))      ; printable
                    (t
                        (format "%%%02X" ch))))   ; escape
          ;; Coerce a string to a list of chars.
          (append (encode-coding-string str (or coding 'utf-8))
                  nil))))

(defun http-post (url args callback)
    "Send ARGS to URL as a POST request."
    (let ((url-request-method "POST")
        (url-request-extra-headers
            '(("Content-Type" . "application/x-www-form-urlencoded;charset=UTF-8")))
        (url-request-data
            (mapconcat (lambda (arg)
                (concat
                    (w3m-url-encode-string (car arg))
                    "="
                    (w3m-url-encode-string (car (last arg)))))
;;                      (concat (url-hexify-string (car arg))
;;                              "="
;;                              (url-hexify-string (cdr arg))))
                    args
                    "&")))
    (url-retrieve url callback)))

(defun http-get (url callback)
    (url-retrieve url callback))

(defun strip-http-headers (entity)
    (let ((i (string-match "\n\n" entity)))
            (decode-coding-string (substring entity (+ i 2)) 'utf-8)))


;; BUFFERS / VARIABLES ;;;;;;;;;;;;;;;;;

;; Buffer-local variables. Given them initial, global bindings so they're defined before there are actual view buffers.
(setq view-depth 3)
(setq view-root nil)
(setq view-title nil)
(setq view-style "hybrid")
(setq view-min-sharability 0.25)
(setq view-max-sharability 1)
(setq view-min-weight 0.25)
(setq view-max-weight 1)
(setq view-atoms nil)
(setq view-current-line 1)

(defun current-line ()
    (interactive)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun find-id ()
    (let ((line (current-line)))
        (if (string-match "^[0-9A-Za-z@&#]*:[0-9A-Za-z@&]*: " line)
            (let (
                (i2 (string-match ":" line))
                (i3 (string-match ": " line)))
                (let (
                    (s1 (substring line 0 i2))
                    (s2 (substring line (+ 1 i2) i3)))
                    (let (
                        (assoc-id (if (< 0 (length s1)) s1 nil))
                        (atom-id (if (< 0 (length s2)) s2 nil)))
                        (list assoc-id atom-id))))
            (list (get-text-property (line-beginning-position) 'link-key) (get-text-property (line-beginning-position) 'target-key))
            )))

;;            (list nil nil))))

(defun get-key (atom)
    (cdr (assoc 'key atom)))

(defun get-value (atom)
    (cdr (assoc 'value atom)))

(defun get-weight (atom)
    (cdr (assoc 'weight atom)))

(defun get-sharability (atom)
    (cdr (assoc 'sharability atom)))

(defun view-name (root-id)
    (if root-id
        (concat "view-" root-id)
        (concat "anonymous-" (number-to-string (random 1000)))))

(defun current-target-key ()
    (car (last (find-id))))

(defun current-link-key ()
    (car (find-id)))

(defun current-link ()
    (get-atom (current-link-key)))

(defun current-target ()
    (get-atom (current-target-key)))
    
(defun get-atom (key)
    (if key
        (if view-atoms
            (gethash key view-atoms)
            nil)
        nil))

(defun show-info (atom)
    (let (
        (created (cdr (assoc 'created atom)))
        (value (cdr (assoc 'value atom)))
        (weight (cdr (assoc 'weight atom)))
        (sharability (cdr (assoc 'sharability atom))))
            (message (concat
                 "weight: " (number-to-string weight)
                 " sharability: " (number-to-string sharability)
                 " created: " (format-time-string "%Y-%m-%dT%H:%M:%S%z" (seconds-to-time (/ created 1000.0)))
                 " value: " value))))

(defun link-info ()
    (interactive)
    (let ((link (current-link)))
        (if link
            (show-info link)
            (no-link))))

(defun target-info()
    (interactive)
    (let ((target (current-target)))
        (if target
            (show-info target)
            (no-target))))


;; COMMUNICATION ;;;;;;;;;;;;;;;;;;;;;;;

(defun base-url ()
    (concat "http://" tinkernotes-rexster-host ":" tinkernotes-rexster-port "/" tinkernotes-rexster-graph "/tinkernotes/"))

(defun receive-view-debug (status)
    (message (buffer-string)))

(defun receive-view (status)
    (let ((json (json-read-from-string (strip-http-headers (buffer-string)))))
        (if status
            (let ((msg (cdr (assoc 'message json)))
                (error (cdr (assoc 'error json))))
                    (if error
                        (error-message error)
                        (error-message msg)))
            (let (
                (root (cdr (assoc 'root json)))
                (view (cdr (assoc 'view json)))
                (depth (cdr (assoc 'depth json)))
                (min-sharability (string-to-number (cdr (assoc 'minSharability json))))
                (max-sharability (string-to-number (cdr (assoc 'maxSharability json))))
                (min-weight (string-to-number (cdr (assoc 'minWeight json))))
                (max-weight (string-to-number (cdr (assoc 'maxWeight json))))
                (style (cdr (assoc 'style json)))
                (title (cdr (assoc 'title json))))
                    (switch-to-buffer (view-name root))
                    (make-local-variable 'view-root)
                    (make-local-variable 'view-depth)
                    (make-local-variable 'view-style)
                    (make-local-variable 'view-title)
                    (make-local-variable 'view-min-sharability)
                    (make-local-variable 'view-max-sharability)
                    (make-local-variable 'view-min-weight)
                    (make-local-variable 'view-max-weight)
                    (make-local-variable 'view-atoms)
                    (make-local-variable 'view-current-line)
                    (setq view-root root)
                    (if depth (setq view-depth (string-to-number depth)))
                    (setq view-min-sharability min-sharability)
                    (setq view-max-sharability max-sharability)
                    (setq view-min-weight min-weight)
                    (setq view-max-weight max-weight)
                    (setq view-style style)
                    (setq view-title title)
                    (setq view-atoms (make-hash-table :test 'equal))
                    ;;(tinkernotes-mode)
                    (erase-buffer)
                    (let ((view-json (json-read-from-string view)))
                        (write-view (cdr (assoc 'children view-json)) 0))
                    (beginning-of-buffer)
                    (setq visible-cursor t)
                    ;; Try to move to the corresponding line in the previous view.
                    ;; This is not always possible and not always helpful, but it is often both.
                    (beginning-of-line view-current-line)
                    (info-message (concat "updated to view " (view-info)))))))

(setq full-colors '(
    "#330000" "#660000" "#990000" "#CC0000"  ;; private:   red
    "#332600" "#664C00" "#997200" "#CC9900"  ;; protected: orange
    "#003300" "#006600" "#009900" "#00CC00"  ;; public:    green
    "#000066" "#000099" "#0000CC" "#0000FF"  ;; demo:      blue
    ))

(setq reduced-colors '("red" "red" "blue" "blue"))

(setq full-colors-supported (> (length (defined-colors)) 8))

(defun colorize (text weight sharability bold)
    (let (
        (i (- (ceiling (* sharability 4)) 1))
        (j (- (ceiling (* weight 4)) 1)))
            (let ((color
                (if full-colors-supported
                    (elt full-colors (+ j (* i 4)))
                    (elt reduced-colors i))))
	    (if bold
        (propertize text 'face (list 'bold 'italic  :foreground color))
        (propertize text 'face (list :foreground color))))))

(defun unescape-link-value (value)
    (replace-regexp-in-string "[ ]" "\\\\ "
        (replace-regexp-in-string "[)]" "\\\\)"
            (replace-regexp-in-string "[(]" "\\\\(" value))))

(defun write-view (children indent)
    (loop for json across children do
    (let (
        (meta (eq t (cdr (assoc 'meta json))))
        (link (cdr (assoc 'link json)))
        (target (cdr (assoc 'target json)))
        (children (cdr (assoc 'children json))))
            (let (
                (link-key (get-key link))
                (link-value (get-value link))
		        (link-weight (get-weight link))
	            (link-sharability (get-sharability link))
                (target-key (cdr (assoc 'key target)))
                (target-value (cdr (assoc 'value target)))
		        (target-weight (cdr (assoc 'weight target)))
		        (target-sharability (cdr (assoc 'sharability target))))
		            (if link-key (puthash link-key link view-atoms))
		            (if target-key (puthash target-key target view-atoms))
		            ;;(if (not link-key) (error "missing link key"))
		            ;;(if (not link-value) (error (concat "missing value for link with key " link-key)))
		            (if (not link-weight) (error (concat "missing weight for link with key " link-key)))
		            (if (not link-sharability) (error (concat "missing sharability for link with key " link-key)))
		            (if (not target-key) (error "missing target key"))
		            (if (not target-value) (error (concat "missing value for target with key " target-key)))
		            (if (not target-weight) (error (concat "missing weight for target with key " target-key)))
		            (if (not target-sharability) (error (concat "missing sharability for target with key " target-key)))
                    (insert
                        (propertize
                            (concat
		                        (propertize (concat link-key ":" target-key ":")
					                'face (if full-colors-supported
					                     '(:foreground "grey80" :background "grey97")
					                     '(:foreground "black"))) " ")
					         ;;'invisible t
			                'link-key link-key
			                'target-key target-key))
			        ;; TODO: also propertize the whitespace. Just propertize once for the whole "line"
			        (loop for i from 1 to indent do (insert "    "))
			        ;; Also propertize target value, for the sake of {{{block text}}}
			        (let ((link-text
			            (if meta (concat "(" (unescape-link-value link-value) ")") (unescape-link-value link-value))))
                            (insert (propertize (concat
                                (colorize link-text link-weight link-sharability t) "  "
                                (colorize target-value target-weight target-sharability nil) "\n")
                                'link-key link-key
                                'target-key target-key)))
                    (write-view children (+ indent 1))
                    ))))


;; VIEWS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun view-info ()
    (concat
        "(root: " view-root
         " :depth " (number-to-string view-depth)
         " :style " view-style
         " :sharability [" (number-to-string view-min-sharability) ", " (number-to-string view-max-sharability) "]"
         " :weight [" (number-to-string view-min-weight) ", " (number-to-string view-max-weight) "]"
         " :title \"" view-title "\")"))  ;; TODO: actuallly escape the title string

(defun to-forward-style (style)
    (cond
        ((string-equal style "hybrid") "hybrid")
        ((string-equal style "targets") "targets")
        ((string-equal style "links") "links")
        ((string-equal style "hybrid-inverse") "hybrid")
        ((string-equal style "targets-inverse") "targets")
        ((string-equal style "links-inverse") "links")))

(defun to-backward-style (style)
    (cond
        ((string-equal style "hybrid") "hybrid-inverse")
        ((string-equal style "targets") "targets-inverse")
        ((string-equal style "links") "links-inverse")
        ((string-equal style "hybrid-inverse") "hybrid-inverse")
        ((string-equal style "targets-inverse") "targets-inverse")
        ((string-equal style "links-inverse") "links-inverse")))

(defun to-hybrid-style (style)
    (cond
        ((string-equal style "hybrid") "hybrid")
        ((string-equal style "targets") "hybrid")
        ((string-equal style "links") "hybrid")
        ((string-equal style "hybrid-inverse") "hybrid-inverse")
        ((string-equal style "targets-inverse") "hybrid-inverse")
        ((string-equal style "links-inverse") "hybrid-inverse")))

(defun to-links-style (style)
    (cond
        ((string-equal style "hybrid") "links")
        ((string-equal style "targets") "links")
        ((string-equal style "links") "links")
        ((string-equal style "hybrid-inverse") "links-inverse")
        ((string-equal style "targets-inverse") "links-inverse")
        ((string-equal style "links-inverse") "links-inverse")))

(defun to-targets-style (style)
    (cond
        ((string-equal style "hybrid") "targets")
        ((string-equal style "targets") "targets")
        ((string-equal style "links") "targets")
        ((string-equal style "hybrid-inverse") "targets-inverse")
        ((string-equal style "targets-inverse") "targets-inverse")
        ((string-equal style "links-inverse") "targets-inverse")))

(defun request-view (root depth style minv maxv minw maxw)
    (setq view-current-line 1)
    (http-get (request-view-url root depth style minv maxv minw maxw) 'receive-view))

(defun request-view-url  (root depth style minv maxv minw maxw)
	(concat (base-url) "view"
            "?root=" (w3m-url-encode-string root)
            "&depth=" (number-to-string depth)
            "&minSharability=" (number-to-string minv)
            "&maxSharability=" (number-to-string maxv)
            "&minWeight=" (number-to-string minw)
            "&maxWeight=" (number-to-string maxw)
            "&style=" style))

(defun request-search-results (query style minv maxv minw maxw)
    (setq view-current-line 1)
    (http-get
        (concat (base-url) "search"
            "?query=" (w3m-url-encode-string query)
            "&depth=2"
            "&style=" style
            "&minSharability=" (number-to-string minv)
            "&maxSharability=" (number-to-string maxv)
            "&minWeight=" (number-to-string minw)
            "&maxWeight=" (number-to-string maxw)) 'receive-view))

(defun visit-target ()
    (interactive)
    (let ((key (current-target-key)))
        (if key
            (request-view key view-depth view-style view-min-sharability view-max-sharability view-min-weight view-max-weight)
            (no-target))))

(defun visit-link ()
    (interactive)
    (let ((key (current-link-key)))
        (if key
            (request-view key view-depth view-style view-min-sharability view-max-sharability view-min-weight view-max-weight)
            (no-link))))

(defun search ()
    (interactive)
    (let ((query (read-from-minibuffer "query: ")))
        (if (> (length query) 0)
            (request-search-results
                ;;(concat "*" query "*")
                query
                view-style
                view-min-sharability view-max-sharability view-min-weight view-max-weight))))

(defun not-in-view ()
	(error-message "this command must be executed from within a view"))

(defun no-link ()
    (error-message "there is no link associated with this line"))
    
(defun no-target ()
    (error-message "there is no target associated with this line"))
    
(defun refresh-view ()
    (interactive)
    (if view-root
        (request-view view-root view-depth view-style view-min-sharability view-max-sharability view-min-weight view-max-weight)
        (not-in-view)))

(defun refresh-view-new (url)
    (url-retrieve url 'receive-view))

(defun decrease-depth ()
    (interactive)
    (if view-root
        (request-view view-root (- view-depth 1) view-style view-min-sharability view-max-sharability view-min-weight view-max-weight)
        (not-in-view)))

(defun increase-depth ()
    (interactive)
    (if view-root
        (request-view view-root (+ view-depth 1) view-style view-min-sharability view-max-sharability view-min-weight view-max-weight)
        (not-in-view)))

(defun refresh-to-forward-view ()
    (interactive)
    (if view-root
        (request-view view-root view-depth (to-forward-style view-style) view-min-sharability view-max-sharability view-min-weight view-max-weight)
        (not-in-view)))

(defun refresh-to-backward-view ()
    (interactive)
    (if view-root
        (request-view view-root view-depth (to-backward-style view-style) view-min-sharability view-max-sharability view-min-weight view-max-weight)
        (not-in-view)))

(defun refresh-to-hybrid-view ()
    (interactive)
    (if view-root
        (request-view view-root view-depth (to-hybrid-style view-style) view-min-sharability view-max-sharability view-min-weight view-max-weight)
        (not-in-view)))

(defun refresh-to-links-view ()
    (interactive)
    (if view-root
        (request-view view-root view-depth (to-links-style view-style) view-min-sharability view-max-sharability view-min-weight view-max-weight)
        (not-in-view)))

(defun refresh-to-targets-view ()
    (interactive)
    (if view-root
        (request-view view-root view-depth (to-targets-style view-style) view-min-sharability view-max-sharability view-min-weight view-max-weight)
        (not-in-view)))

(defun decrease-min-weight ()
    (interactive)
    (if view-root
        (request-view view-root view-depth view-style view-min-sharability view-max-sharability (- view-min-weight 0.25) view-max-weight)
        (not-in-view)))

(defun increase-min-weight ()
    (interactive)
    (if view-root
        (request-view view-root view-depth view-style view-min-sharability view-max-sharability (+ view-min-weight 0.25) view-max-weight)
        (not-in-view)))

(defun decrease-max-weight ()
    (interactive)
    (if view-root
        (request-view view-root view-depth view-style view-min-sharability view-max-sharability view-min-weight (- view-max-weight 0.25))
        (not-in-view)))

(defun increase-max-weight ()
    (interactive)
    (if view-root
        (request-view view-root view-depth view-style view-min-sharability view-max-sharability view-min-weight (+ view-max-weight 0.25))
        (not-in-view)))

(defun decrease-min-sharability ()
    (interactive)
    (if view-root
        (request-view view-root view-depth view-style (- view-min-sharability 0.25) view-max-sharability view-min-weight view-max-weight)
        (not-in-view)))

(defun increase-min-sharability ()
    (interactive)
    (if view-root
        (request-view view-root view-depth view-style (+ view-min-sharability 0.25) view-max-sharability view-min-weight view-max-weight)
        (not-in-view)))

(defun decrease-max-sharability ()
    (interactive)
    (if view-root
        (request-view view-root view-depth view-style view-min-sharability (- view-max-sharability 0.25) view-min-weight view-max-weight)
        (not-in-view)))

(defun increase-max-sharability ()
    (interactive)
    (if view-root
        (request-view view-root view-depth view-style view-min-sharability (+ view-max-sharability 0.25) view-min-weight view-max-weight)
        (not-in-view)))


;; UPDATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun push-view ()
    (interactive)
    (let (
        (entity (buffer-string)))
        ;; The received view may very well differ from the pushed view in terms of line numbering,
        ;; but we'll try to stay on the same line anyway.
        (setq view-current-line (line-number-at-pos))
        (http-post
            (concat (base-url) "update")
            (list
                (list "root" view-root)
                (list "view" entity)
                (list "style" view-style)
                (list "minSharability" (number-to-string view-min-sharability))
                (list "maxSharability" (number-to-string view-max-sharability))
                (list "minWeight" (number-to-string view-min-weight))
                (list "maxWeight" (number-to-string view-max-weight))
                (list "depth" (number-to-string view-depth)))
            'receive-view)))

(defun set-properties (key weight sharability)
    (interactive)
    (if view-root
        (let ((url (request-view-url view-root view-depth view-style view-min-sharability view-max-sharability view-min-weight view-max-weight)))
(setq hack-url url)
            (setq view-current-line (line-number-at-pos))
            (http-get
                (concat (base-url) "set"
                    "?key=" (w3m-url-encode-string key)
                    "&weight=" (number-to-string weight)
                    "&sharability=" (number-to-string sharability))
	(lambda (status)
        (let ((json (json-read-from-string (strip-http-headers (buffer-string)))))
            (if status
                (let ((msg (cdr (assoc 'message json)))
				    (error (cdr (assoc 'error json))))
                        (if error
                            (error-message error)
                            (error-message msg)))
		         (refresh-view-new hack-url))))
		                     ))
        (not-in-view)))

(defun decrease-link-weight ()
    (interactive)
    (let ((link (current-link)))
        (if link
            (let (
                (key (get-key link))
                (weight (get-weight link))
                (sharability (get-sharability link)))
	                (set-properties key (- weight 0.25) sharability))
	        (no-link))))

(defun increase-link-weight ()
    (interactive)
    (let ((link (current-link)))
        (if link
            (let (
                (key (get-key link))
                (weight (get-weight link))
                (sharability (get-sharability link)))
	                (set-properties key (+ weight 0.25) sharability))
	        (no-link))))

(defun decrease-target-weight ()
    (interactive)
    (let ((target (current-target)))
        (if target
            (let (
                (key (get-key target))
                (weight (get-weight target))
                (sharability (get-sharability target)))
	                (set-properties key (- weight 0.25) sharability))
	        (no-target))))

(defun increase-target-weight ()
    (interactive)
    (let ((target (current-target)))
        (if target
            (let (
                (key (get-key target))
                (weight (get-weight target))
                (sharability (get-sharability target)))
	                (set-properties key (+ weight 0.25) sharability))
	        (no-target))))

(defun decrease-link-sharability ()
    (interactive)
    (let ((link (current-link)))
        (if link
            (let (
                (key (get-key link))
                (weight (get-weight link))
                (sharability (get-sharability link)))
		            (set-properties key weight (- sharability 0.25)))
		    (no-link))))

(defun increase-link-sharability ()
    (interactive)
    (let ((link (current-link)))
        (if link
            (let (
                (key (get-key link))
                (weight (get-weight link))
                (sharability (get-sharability link)))
		            (set-properties key weight (+ sharability 0.25)))
		    (no-link))))

(defun decrease-target-sharability ()
    (interactive)
    (let ((target (current-target)))
        (if target
            (let (
                (key (get-key target))
                (weight (get-weight target))
                (sharability (get-sharability target)))
		            (set-properties key weight (- sharability 0.25)))
		    (no-target))))

(defun increase-target-sharability ()
    (interactive)
    (let ((target (current-target)))
        (if target
            (let (
                (key (get-key target))
                (weight (get-weight target))
                (sharability (get-sharability target)))
		            (set-properties key weight (+ sharability 0.25)))
		    (no-target))))


;; INTERFACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun info-message (msg)
    (message (concat "Info: " msg)))

(defun error-message (msg)
    (message (concat "Error: " msg)))

(defun my-debug ()
    (interactive)
    (message (number-to-string (length (defined-colors)))))

(global-set-key (kbd "C-c d") 'my-debug)
(global-set-key (kbd "C-c l") 'visit-link)
(global-set-key (kbd "C-c p") 'push-view)
(global-set-key (kbd "C-c q") 'search)
(global-set-key (kbd "C-c r") 'refresh-view)
(global-set-key (kbd "C-c t") 'visit-target)
(global-set-key (kbd "C-c C-d ,") 'decrease-depth)
(global-set-key (kbd "C-c C-d .") 'increase-depth)
(global-set-key (kbd "C-c C-l i") 'link-info)
(global-set-key (kbd "C-c C-l C-s ,") 'decrease-link-sharability)
(global-set-key (kbd "C-c C-l C-s .") 'increase-link-sharability)
(global-set-key (kbd "C-c C-l C-w ,") 'decrease-link-weight)
(global-set-key (kbd "C-c C-l C-w .") 'increase-link-weight)
(global-set-key (kbd "C-c C-s C-[ ,") 'decrease-min-sharability)
(global-set-key (kbd "C-c C-s C-[ .") 'increase-min-sharability)
(global-set-key (kbd "C-c C-s C-] ,") 'decrease-max-sharability)
(global-set-key (kbd "C-c C-s C-] .") 'increase-max-sharability)
(global-set-key (kbd "C-c C-t i") 'target-info)
(global-set-key (kbd "C-c C-t C-s ,") 'decrease-target-sharability)
(global-set-key (kbd "C-c C-t C-s .") 'increase-target-sharability)
(global-set-key (kbd "C-c C-t C-w ,") 'decrease-target-weight)
(global-set-key (kbd "C-c C-t C-w .") 'increase-target-weight)
(global-set-key (kbd "C-c C-v b") 'refresh-to-backward-view)
(global-set-key (kbd "C-c C-v f") 'refresh-to-forward-view)
(global-set-key (kbd "C-c C-v h") 'refresh-to-hybrid-view)
(global-set-key (kbd "C-c C-v l") 'refresh-to-links-view)
(global-set-key (kbd "C-c C-v t") 'refresh-to-targets-view)
(global-set-key (kbd "C-c C-w C-[ ,") 'decrease-min-weight)
(global-set-key (kbd "C-c C-w C-[ .") 'increase-min-weight)
(global-set-key (kbd "C-c C-w C-] ,") 'decrease-max-weight)
(global-set-key (kbd "C-c C-w C-] .") 'increase-max-weight)

;; Note: these should perhaps be local settings
(global-set-key (kbd "C-c C-v ;") 'toggle-truncate-lines)
(setq-default truncate-lines t)
(if full-colors-supported
    (let ()
        (global-hl-line-mode 1)
        (set-face-background 'hl-line "ivory")))
(defvar current-date-format "%Y-%m-%d")
(defun insert-current-date ()
  "insert the current date into the current buffer."
       (interactive)
       (insert (format-time-string current-date-format (current-time))))
(global-set-key (kbd "C-c C-a d") 'insert-current-date)
;; These may or may not be necessary
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;(setq syntax-keywords
;; '(
;;   ("^\([0-9A-Za-z+/]*:[0-9A-Za-z+/]*\)" . font-lock-doc-face)
;;  ))
;;
;;(define-derived-mode tinkernotes-mode fundamental-mode
;;  (setq font-lock-defaults '(syntax-keywords))
;;  (setq mode-name "tinkernotes")
;;)


;; Uncomment only when debugging
(add-hook 'after-init-hook '(lambda () (setq debug-on-error t)))


(provide 'tinkernotes)
