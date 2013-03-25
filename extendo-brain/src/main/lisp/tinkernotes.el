;; TinkerNotes Emacs client
;;
;; Required global variables:
;;
;;     tinkernotes-rexster-host: IP of the rexster server
;;     tinkernotes-rexster-port: port of the Rexster server
;;     tinkernotes-rexster-graph: name of the Rexster graph
;;
;; For example:
;;
;;     (defun tinkernotes ()
;;         (defvar tinkernotes-rexster-host "localhost")
;;         (defvar tinkernotes-rexster-port "8182")
;;         (defvar tinkernotes-rexster-graph "tinkernotes"))

;; Uncomment only when debugging
(add-hook 'after-init-hook '(lambda () (setq debug-on-error t)))

(eval-when-compile (require 'cl))

;; for JSON-formatted messages to and from Rexster
(require 'json)

;; for line number annotations in buffers
(require 'linum)

;; for visiting URLs in a browser
(require 'goto-addr)

;;(require 'ring)


;; for encryption of sensitive values
(require 'aes)

;; LOCAL IMPORTS ;;;;;;;;;;;;;;;;;;;;;;;
;; not required by this library ;;;;;;;;

(require 'latex-math-preview)


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

(setq tn-readonly-mode "readonly")
(setq tn-edit-mode "readwrite")
(setq tn-search-mode "search")
(setq tn-history-mode "history")

(setq tn-forward-view-style "forward")
(setq tn-backward-view-style "backward")

;; Buffer-local variables. Given them initial, global bindings so they're defined before there are actual view buffers.
(setq tn-depth 3)
(setq tn-root nil)
(setq tn-title nil)
(setq tn-style tn-forward-view-style)
;; "private" atoms are hidden to begin with
(setq tn-min-sharability 0.25)
(setq tn-max-sharability 1)
;; default to "average" sharability to begin with
(setq tn-default-sharability 0.5)
(setq tn-future-sharability tn-default-sharability)
;; atoms of all weights are visible to begin with
(setq tn-min-weight 0.0)
(setq tn-max-weight 1.0)
;; default to "average" weight to begin with
(setq tn-default-weight 0.5)
(setq tn-atoms nil)
(setq tn-current-line 1)
(setq tn-mode nil)  ;; Note: 'view-mode' is used by Emacs.
(setq tn-value-truncation-length 100)

;; NAVIGATION ;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tn-enable-linum t)
(linum-mode tn-enable-linum)

(defun current-line ()
    (interactive)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(setq line-addr-keypairs (list
    '(?0 ?0) '(?1 ?1) '(?2 ?2) '(?3 ?3) '(?4 ?4) '(?5 ?5) '(?6 ?6) '(?7 ?7) '(?8 ?8) '(?9 ?9)
    '(?; ?0) '(?a ?1) '(?s ?2) '(?d ?3) '(?f ?4) '(?g ?5) '(?h ?6) '(?j ?7) '(?k ?8) '(?l ?9)
             '(?u ?1) '(?i ?2) '(?o ?3) '(?p ?4)))

(setq line-addr-keymap (make-hash-table))
(dolist (pair line-addr-keypairs)
    (puthash (car pair) (car (cdr pair)) line-addr-keymap))

(defun mapkey (c)
    (gethash c line-addr-keymap))

(defun address-to-lineno (address)
    (if (string-match "[0-9asdfghjkl;]+" address)
        (string-to-number (coerce (mapcar 'mapkey (coerce address 'list)) 'string))
        nil))

(defun handle-changewindow (address)
    (setq c (car (coerce address 'list)))
    (if (string-match "[uiop]" (string c))
       (let ((n (string-to-number (string (gethash c line-addr-keymap)))))
           (other-window n)
           (coerce (cdr (coerce address 'list)) 'string))
       address))

(defun tn-goto-line ()
    (interactive)
    (let ((address (read-from-minibuffer "line: ")))
        (let ((line
                (address-to-lineno (handle-changewindow address))))
            (if line
                (goto-line line)
                (error-message "invalid line address")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-id ()
    (let ((line (current-line)))
        (if (string-match "^[0-9A-Za-z@&]*: " line)
            (let (
                (i3 (string-match ": " line)))
                (let (
                    (s2 (substring line 0 i3)))
                    (let (
                        (assoc-id nil)
                        (atom-id (if (< 0 (length s2)) s2 nil)))
                        (list assoc-id atom-id))))
            (list nil (get-text-property (line-beginning-position) 'target-id))
            )))

(defun get-id (atom)
    (cdr (assoc 'id atom)))

(defun get-created (atom)
    (cdr (assoc 'created atom)))

(defun get-value (atom)
    (cdr (assoc 'value atom)))

(defun get-priority (atom)
    (cdr (assoc 'priority atom)))

(defun get-sharability (atom)
    (cdr (assoc 'sharability atom)))

(defun get-weight (atom)
    (cdr (assoc 'weight atom)))

(defun get-alias (atom)
    (let ((x (assoc 'alias atom)))
        (if x (cdr x) nil)))

(defun view-name (root-id json)
    (let ((title (cdr (assoc 'title json))))
        (if root-id
            (let ((name
                (if (> (length title) 20)
                    (concat (substring title 0 20) "...")
                    title)))
                (concat name " [" root-id "]"))
            title)))

(defun current-target-id ()
    (car (last (find-id))))

(defun current-target ()
    (get-atom (current-target-id)))

(defun current-target-value ()
    (let ((g (current-target)))
        (if g
            (get-value g))))

(defun current-target-alias ()
    (let ((g (current-target)))
        (if g
            (get-alias g))))

(defun current-target-sharability ()
    (let ((g (current-target)))
        (if g
            (get-sharability g))))

;; change the default sharability in the new view after a user visits a link or target
;; The default will never be greater than 0.75 unless explicitly set by the user.
(defun future-sharability (s)
    (if s
        (if (<= s 0.75) s 0.75)
        0.5))

(defun get-atom (key)
    (if key
        (if tn-atoms
            (gethash key tn-atoms)
            nil)
        nil))

(defun show-info (atom)
    (let (
        (created (get-created atom))
        (value (get-value atom))
        (weight (get-weight atom))
        (sharability (get-sharability atom))
        (priority (get-priority atom))
        (alias (get-alias atom)))
            (message (concat
                 "weight: " (number-to-string weight)
                 " sharability: " (number-to-string sharability)
                 (if priority (concat " priority: " (number-to-string priority)) "")
                 " created: " (format-time-string "%Y-%m-%dT%H:%M:%S%z" (seconds-to-time (/ created 1000.0)))
                 " value: " value
                 (if alias (concat " alias: " alias) "")))))

(defun tn-target-info()
    (interactive)
    (let ((target (current-target)))
        (if target
            (show-info target)
            (no-target))))


;; COMMUNICATION ;;;;;;;;;;;;;;;;;;;;;;;

(defun base-url ()
    (concat "http://" tinkernotes-rexster-host ":" tinkernotes-rexster-port "/graphs/" tinkernotes-rexster-graph "/tinkernotes/"))

(defun receive-view (mode)
    (lexical-let ((m mode))
        (lambda (status) (receive-view-internal status m))))

(defun receive-view-internal (status mode)
    (let ((json (json-read-from-string (strip-http-headers (buffer-string))))
          (editable (equal mode tn-edit-mode)))
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
                (default-weight (string-to-number (cdr (assoc 'defaultWeight json))))
                (style (cdr (assoc 'style json)))
                (title (cdr (assoc 'title json))))
                    (switch-to-buffer (view-name root json))
                    (make-local-variable 'tn-root)
                    (make-local-variable 'tn-depth)
                    (make-local-variable 'tn-style)
                    (make-local-variable 'tn-title)
                    (make-local-variable 'tn-min-sharability)
                    (make-local-variable 'tn-max-sharability)
                    (make-local-variable 'tn-default-sharability)
                    (make-local-variable 'tn-min-weight)
                    (make-local-variable 'tn-max-weight)
                    (make-local-variable 'tn-default-weight)
                    (make-local-variable 'tn-atoms)
                    (make-local-variable 'tn-current-line)
                    (make-local-variable 'tn-mode)
                    (make-local-variable 'tn-value-truncation-length)
                    (setq tn-root root)
                    (if (equal mode tn-search-mode)
                        ;; Always leave a search view with depth 1, rather than that of the last view.
                        ;; The user experience is a little unpredictable otherwise.
                        (setq tn-depth 1)
                        (if depth (setq tn-depth (string-to-number depth))))
                    (setq tn-min-sharability min-sharability)
                    (setq tn-max-sharability max-sharability)
                    (setq tn-default-sharability tn-future-sharability)
                    (setq tn-min-weight min-weight)
                    (setq tn-max-weight max-weight)
                    (setq tn-default-weight default-weight)
                    (setq tn-style (if style style tn-style))
                    (setq tn-title title)
                    (setq tn-atoms (make-hash-table :test 'equal))
                    (setq tn-mode mode)
                    (setq buffer-read-only nil)
                    (erase-buffer)
                    (write-view editable (cdr (assoc 'children view)) 0)
                    (beginning-of-buffer)
                    (setq visible-cursor t)
                    ;; Try to move to the corresponding line in the previous view.
                    ;; This is not always possible and not always helpful, but it is often both.
                    (beginning-of-line tn-current-line)
                    (setq buffer-read-only (not editable))
                    (linum-mode tn-enable-linum)
                    (info-message (concat "updated to view " (view-info)))))))

(defun receive-export-results (status)
    (let ((json (json-read-from-string (strip-http-headers (buffer-string)))))
        (if status
            (let ((msg (cdr (assoc 'message json)))
                (error (cdr (assoc 'error json))))
                    (if error
                        (error-message error)
                        (error-message msg)))
            (info-message "exported successfully"))))

;; unused colors: black/gray, purple, cyan, orange
(setq base-colors  '("#660000" "#604000" "#005000" "#000066"))
(setq bright-colors  '("#D00000" "#D0B000" "#00B000" "#0000D0"))
(setq reduced-colors '("red" "red" "blue" "blue"))

(defun color-part-red (color)
    (string-to-number (substring color 1 3) 16))
(defun color-part-green (color)
    (string-to-number (substring color 3 5) 16))
(defun color-part-blue (color)
    (string-to-number (substring color 5 7) 16))

(defun color-string (red green blue)
    (concat "#" (format "%02X" red) (format "%02X" green) (format "%02X" blue)))

(defun weighted-average (a b weight)
    (+ (* a (- 1 weight)) (* b weight)))

(defun fade-color (color weight)
    (let ((low (weighted-average color 255 0.9375))
          (high color))
        (weighted-average low high weight)))

(defun find-color (weight sharability bright)
    (let ((s
        (if bright
            (elt bright-colors (- (ceiling (* sharability 4)) 1))
            (elt base-colors (- (ceiling (* sharability 4)) 1)))))
        (color-string
            (fade-color (color-part-red s) weight)
            (fade-color (color-part-green s) weight)
            (fade-color (color-part-blue s) weight))))

(setq full-colors-supported (> (length (defined-colors)) 8))

(defun colorize (text weight sharability underline bold bright background)
    (let ((color (if full-colors-supported
            (find-color weight sharability bright)
            (elt reduced-colors (- (ceiling (* sharability 4)) 1)))))
        (setq l (list :foreground color :background background))
        (if bold (setq l (cons 'bold l)))
        (if underline (setq l (cons 'underline l)))
        (propertize text 'face l)))

(defun black (text)
    (propertize text 'face (list :foreground "black")))

(defun light-gray (text background)
    (propertize text
	    'face (if full-colors-supported
		    (list :foreground "grey80" :background background)
			(list :foreground "black"))))

(defun dark-gray (text background)
    (propertize text
	    'face (if full-colors-supported
		    (list :foreground "grey50" :background background)
			(list :foreground "black"))))

(defun create-id-suffix (id)
    (concat " " (light-gray (concat ":" (propertize id 'invisible t) ":") "white") (black " ")))

(defun write-view (editable children tree-indent)
    (loop for json across children do
    (let (
        (link (cdr (assoc 'link json)))
        (children (cdr (assoc 'children json))))
            (let (
                (target-id (get-id json))
                (target-value (let ((v (get-value json))) (if v v "")))
		        (target-weight (get-weight json))
		        (target-sharability (get-sharability json))
                (target-has-children (not (equal json-false (cdr (assoc 'hasChildren json)))))
		        (target-alias (get-alias json)))
		            (if target-id (puthash target-id json tn-atoms))
		            (if (not target-id) (error "missing target id"))
		            ;;(if (not target-value) (error (concat "missing value for target with id " target-id)))
		            (if (not target-weight) (error (concat "missing weight for target with id " target-id)))
		            (if (not target-sharability) (error (concat "missing sharability for target with id " target-id)))
		            ;; black space at the end of the line makes the next line black when you enter a newline and continue typing
		            (let ((line "") (id-suffix (create-id-suffix target-id)))
		                (if (not editable)
                            (setq id-suffix (propertize id-suffix 'invisible t)))
                        (let ((space ""))
                            (loop for i from 1 to tree-indent do (setq space (concat space " ")))
                            (setq line (concat line space)))
                        (let ((bullet (if target-has-children "+" "\u00b7")))   ;; previously: "-" or "\u25ba"
                            (setq line (concat line
                                (colorize (concat bullet " " target-value) target-weight target-sharability nil nil target-alias "white")
                                id-suffix
                                 "\n")))
                        (insert (propertize line 'target-id target-id)))
                    (write-view editable children (+ tree-indent 4))))))


;; VIEWS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun view-info ()
    (concat
        "(root: " tn-root
         " :depth " (number-to-string tn-depth)
         " :style " tn-style
         " :sharability [" (number-to-string tn-min-sharability) ", " (number-to-string tn-default-sharability) ", " (number-to-string tn-max-sharability) "]"
         " :weight [" (number-to-string tn-min-weight) ", " (number-to-string tn-default-weight) ", " (number-to-string tn-max-weight) "]"
         " :value \"" tn-title "\")"))  ;; TODO: actuallly escape the title string

(defun request-view (preserve-line mode root depth style mins maxs defaults minw maxw)
    (setq tn-current-line (if preserve-line (line-number-at-pos) 1))
    (setq tn-future-sharability defaults)
    (http-get (request-view-url root depth style mins maxs minw maxw) (receive-view mode)))

(defun request-view-url  (root depth style mins maxs minw maxw)
	(concat (base-url) "view"
            "?root=" (w3m-url-encode-string root)
            "&depth=" (number-to-string depth)
            "&minSharability=" (number-to-string mins)
            "&maxSharability=" (number-to-string maxs)
            "&minWeight=" (number-to-string minw)
            "&maxWeight=" (number-to-string maxw)
            "&style=" style))

(defun request-history (mins maxs minw maxw)
    (setq tn-current-line 1)
    (setq tn-future-sharability tn-default-sharability)
    (http-get
        (concat (base-url) "history"
            "?minSharability=" (number-to-string mins)
            "&maxSharability=" (number-to-string maxs)
            "&minWeight=" (number-to-string minw)
            "&maxWeight=" (number-to-string maxw)) (receive-view tn-search-mode)))

(defun request-duplicates (mins maxs minw maxw)
    (setq tn-current-line 1)
    (setq tn-future-sharability tn-default-sharability)
    (http-get
        (concat (base-url) "duplicates"
            "?minSharability=" (number-to-string mins)
            "&maxSharability=" (number-to-string maxs)
            "&minWeight=" (number-to-string minw)
            "&maxWeight=" (number-to-string maxw)) (receive-view tn-search-mode)))

(defun request-search-results (query style mins maxs minw maxw)
    (setq tn-current-line 1)
    (setq tn-future-sharability tn-default-sharability)
    (http-get
        (concat (base-url) "search"
            "?query=" (w3m-url-encode-string query)
            "&valueCutoff=" (number-to-string tn-value-truncation-length)
            "&depth=1"
            "&style=" style
            "&minSharability=" (number-to-string mins)
            "&maxSharability=" (number-to-string maxs)
            "&minWeight=" (number-to-string minw)
            "&maxWeight=" (number-to-string maxw)) (receive-view tn-search-mode)))

(defun request-priorities-results (mins maxs minw maxw)
    (setq tn-current-line 1)
    (setq tn-future-sharability tn-default-sharability)
    (http-get
        (concat (base-url) "priorities"
            "?maxResults=100"
            "&minSharability=" (number-to-string mins)
            "&maxSharability=" (number-to-string maxs)
            "&minWeight=" (number-to-string minw)
            "&maxWeight=" (number-to-string maxw)) (receive-view tn-search-mode)))

(defun request-find-roots-results (style mins maxs minw maxw)
    (setq tn-current-line 1)
    (setq tn-future-sharability tn-default-sharability)
    (http-get
        (concat (base-url) "find-roots"
            "?depth=1"
            "&style=" style
            "&minSharability=" (number-to-string mins)
            "&maxSharability=" (number-to-string maxs)
            "&minWeight=" (number-to-string minw)
            "&maxWeight=" (number-to-string maxw)) (receive-view tn-search-mode)))

(defun request-ripple-results (query style mins maxs minw maxw)
    (setq tn-current-line 1)
    (setq tn-future-sharability tn-default-sharability)
    (http-get
        (concat (base-url) "ripple"
            "?query=" (w3m-url-encode-string query)
            "&depth=1"
            "&style=" style
            "&minSharability=" (number-to-string mins)
            "&maxSharability=" (number-to-string maxs)
            "&minWeight=" (number-to-string minw)
            "&maxWeight=" (number-to-string maxw)) (receive-view tn-search-mode)))

(defun do-export ()
    (http-get
        (concat (base-url) "export") 'receive-export-results))

(defun mode-for-visit ()
    (if (or (equal tn-mode tn-edit-mode) (equal tn-mode tn-readonly-mode))
        tn-mode
        tn-readonly-mode))

(defun tn-visit-target ()
    (interactive)
    (let ((key (current-target-id)))
        (if key
            (request-view nil (mode-for-visit) key tn-depth tn-style tn-min-sharability tn-max-sharability (future-sharability (current-target-sharability)) tn-min-weight tn-max-weight)
            (no-target))))

(defun tn-history ()
    (interactive)
    (request-history
        tn-min-sharability tn-max-sharability tn-min-weight tn-max-weight))


(defun tn-duplicates ()
    (interactive)
    (request-duplicates
        tn-min-sharability tn-max-sharability tn-min-weight tn-max-weight))

(defun tn-search ()
    (interactive)
    (let ((query (read-from-minibuffer "query: ")))
        (if (> (length query) 0)
            (request-search-results
                query
                tn-style
                tn-min-sharability tn-max-sharability tn-min-weight tn-max-weight))))

(defun tn-priorities ()
    (interactive)
    (request-priorities-results
        tn-min-sharability tn-max-sharability tn-min-weight tn-max-weight))

(defun tn-find-roots ()
    (interactive)
        (request-find-roots-results
            tn-style
            tn-min-sharability tn-max-sharability tn-min-weight tn-max-weight))

(defun tn-ripple-query ()
    (interactive)
    (let ((query (read-from-minibuffer "query: ")))
        (if (> (length query) 0)
            (request-ripple-results
                query
                tn-style
                tn-min-sharability tn-max-sharability tn-min-weight tn-max-weight))))

(defun tn-export ()
    (interactive)
    (message "exporting")
    (do-export))

(defun current-view-mode-is-atom-view ()
    (or
        (equal tn-mode tn-readonly-mode)
        (equal tn-mode tn-edit-mode)))

(defun in-view ()
    (if (or
;;            (equal tn-mode tn-search-mode)
            (equal tn-mode tn-readonly-mode)
            (equal tn-mode tn-edit-mode))
        t
	    (and (error-message "this command can only be executed from within an atom view") nil)))

(defun in-edit-view ()
    (if (equal tn-mode tn-edit-mode)
        t
	    (and (error-message "this command can only be executed from within an edit view") nil)))

(defun no-link ()
    (error-message "there is no link associated with this line"))
    
(defun no-target ()
    (error-message "there is no target associated with this line"))
    
(defun tn-refresh-view ()
    (interactive)
    (if (in-view)
        (request-view t tn-mode tn-root tn-depth tn-style tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight)))

(defun tn-enter-edit-view ()
    (interactive)
    (if (and (in-view) (equal tn-mode tn-readonly-mode))
        (request-view t tn-edit-mode tn-root tn-depth tn-style tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight)))

(defun tn-enter-readonly-view ()
    (interactive)
    (if (and (in-view) (equal tn-mode tn-edit-mode))
        (request-view t tn-readonly-mode tn-root tn-depth tn-style tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight)))

(defun tn-decrease-depth ()
    (interactive)
    (if (in-view)
        (request-view nil tn-mode tn-root (- tn-depth 1) tn-style tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight)))

(defun tn-increase-depth ()
    (interactive)
    (if (in-view)
        (request-view nil tn-mode tn-root (+ tn-depth 1) tn-style tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight)))

(defun tn-refresh-to-forward-view ()
    (interactive)
    (if (in-view)
        (request-view nil tn-mode tn-root tn-depth tn-forward-view-style tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight)))

(defun tn-refresh-to-backward-view ()
    (interactive)
    (if (in-view)
        (request-view nil tn-mode tn-root tn-depth tn-backward-view-style tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight)))


;; set weight ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-default-weight (s)
    (if (and (in-view) (> s 0) (<= s 1))
        (setq tn-default-weight s)
        (error-message
            (concat "weight " (number-to-string s) " is outside of range (0, 1]"))))

(defun tn-set-default-weight-1 ()
    (interactive)
    (set-default-weight 0.25))

(defun tn-set-default-weight-2 ()
    (interactive)
    (set-default-weight 0.5))

(defun tn-set-default-weight-3 ()
    (interactive)
    (set-default-weight 0.75))

(defun tn-set-default-weight-4 ()
    (interactive)
    (set-default-weight 1.0))

(defun set-min-weight (s)
    (if (and (in-view) (>= s 0) (<= s 1))
        (request-view t tn-mode tn-root tn-depth tn-style tn-min-sharability tn-max-sharability tn-default-sharability s tn-max-weight)
        (error-message
            (concat "min weight " (number-to-string s) " is outside of range [0, 1]"))))

(defun tn-set-min-weight-0 ()
    (interactive)
    (set-min-weight 0.0))

(defun tn-set-min-weight-1 ()
    (interactive)
    (set-min-weight 0.25))

(defun tn-set-min-weight-2 ()
    (interactive)
    (set-min-weight 0.5))

(defun tn-set-min-weight-3 ()
    (interactive)
    (set-min-weight 0.75))

(defun tn-set-min-weight-4 ()
    (interactive)
    (set-min-weight 1.0))

(defun set-max-weight (s)
    (if (and (in-view) (>= s 0) (<= s 1))
        (request-view t tn-mode tn-root tn-depth tn-style tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight s)
        (error-message
            (concat "max weight " (number-to-string s) " is outside of range [0, 1]"))))

(defun tn-set-max-weight-0 ()
    (interactive)
    (set-max-weight 0.0))

(defun tn-set-max-weight-1 ()
    (interactive)
    (set-max-weight 0.25))

(defun tn-set-max-weight-2 ()
    (interactive)
    (set-max-weight 0.5))

(defun tn-set-max-weight-3 ()
    (interactive)
    (set-max-weight 0.75))

(defun tn-set-max-weight-4 ()
    (interactive)
    (set-max-weight 1.0))

;; set sharability ;;;;;;;;;;;;;;;;;;;;;

(defun set-default-sharability (s)
    (if (and (in-view) (> s 0) (<= s 1))
        (setq tn-default-sharability s)
        (error-message
            (concat "sharability " (number-to-string s) " is outside of range (0, 1]"))))

(defun tn-set-default-sharability-1 ()
    (interactive)
    (set-default-sharability 0.25))

(defun tn-set-default-sharability-2 ()
    (interactive)
    (set-default-sharability 0.5))

(defun tn-set-default-sharability-3 ()
    (interactive)
    (set-default-sharability 0.75))

(defun tn-set-default-sharability-4 ()
    (interactive)
    (set-default-sharability 1.0))

(defun set-min-sharability (s)
    (if (and (in-view) (>= s 0) (<= s 1))
        (request-view t tn-mode tn-root tn-depth tn-style s tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight)
        (error-message
            (concat "min sharability " (number-to-string s) " is outside of range [0, 1]"))))

(defun tn-set-min-sharability-0 ()
    (interactive)
    (set-min-sharability 0.0))

(defun tn-set-min-sharability-1 ()
    (interactive)
    (set-min-sharability 0.25))

(defun tn-set-min-sharability-2 ()
    (interactive)
    (set-min-sharability 0.5))

(defun tn-set-min-sharability-3 ()
    (interactive)
    (set-min-sharability 0.75))

(defun tn-set-min-sharability-4 ()
    (interactive)
    (set-min-sharability 1.0))

(defun set-max-sharability (s)
    (if (and (in-view) (>= s 0) (<= s 1))
        (request-view t tn-mode tn-root tn-depth tn-style tn-min-sharability s tn-default-sharability tn-min-weight tn-max-weight)
        (error-message
            (concat "max sharability " (number-to-string s) " is outside of range [0, 1]"))))

(defun tn-set-max-sharability-0 ()
    (interactive)
    (set-max-sharability 0.0))

(defun tn-set-max-sharability-1 ()
    (interactive)
    (set-max-sharability 0.25))

(defun tn-set-max-sharability-2 ()
    (interactive)
    (set-max-sharability 0.5))

(defun tn-set-max-sharability-3 ()
    (interactive)
    (set-max-sharability 0.75))

(defun tn-set-max-sharability-4 ()
    (interactive)
    (set-max-sharability 1.0))


;; UPDATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tn-push-view ()
    (interactive)
    (if (in-edit-view)
    (let (
        (entity (buffer-string)))
        ;; The received view may very well differ from the pushed view in terms of line numbering,
        ;; but we'll try to stay on the same line anyway.
        (setq tn-current-line (line-number-at-pos))
        (setq tn-future-sharability tn-default-sharability)
        (http-post
            (concat (base-url) "update")
            (list
                (list "root" tn-root)
                (list "view" entity)
                (list "style" tn-style)
                (list "minSharability" (number-to-string tn-min-sharability))
                (list "maxSharability" (number-to-string tn-max-sharability))
                (list "defaultSharability" (number-to-string tn-default-sharability))
                (list "minWeight" (number-to-string tn-min-weight))
                (list "maxWeight" (number-to-string tn-max-weight))
                (list "defaultWeight" (number-to-string tn-default-weight))
                (list "depth" (number-to-string tn-depth)))
            (receive-view tn-edit-mode)))))

(defun set-property (id name value)
    (interactive)
    (if (in-view)
        (lexical-let (
                (mode tn-mode)
                (url (request-view-url tn-root tn-depth tn-style tn-min-sharability tn-max-sharability tn-min-weight tn-max-weight)))
            (setq tn-current-line (line-number-at-pos))
            (setq tn-future-sharability tn-default-sharability)
            (http-get
                (concat (base-url) "set"
                    "?id=" (w3m-url-encode-string id)
                    "&name=" name
                    "&value=" (number-to-string value))
	(lambda (status)
        (let ((json (json-read-from-string (strip-http-headers (buffer-string)))))
            (if status
                (let ((msg (cdr (assoc 'message json)))
				    (error (cdr (assoc 'error json))))
                        (if error
                            (error-message error)
                            (error-message msg)))
                 (url-retrieve url (receive-view mode)))))))))

(defun set-target-priority (v)
    (if (and (>= v 0) (<= v 1))
        (let ((target (current-target)))
            (if target
                (let (
                    (id (get-id target)))
	                    (set-property id "priority" v))
	            (no-target)))
        (error-message
            (concat "priority " (number-to-string v) " is outside of range [0, 1]"))))

(defun tn-set-target-priority-0 ()
    (interactive)
    (set-target-priority 0))

(defun tn-set-target-priority-1 ()
    (interactive)
    (set-target-priority 0.25))

(defun tn-set-target-priority-2 ()
    (interactive)
    (set-target-priority 0.5))

(defun tn-set-target-priority-3 ()
    (interactive)
    (set-target-priority 0.75))

(defun tn-set-target-priority-4 ()
    (interactive)
    (set-target-priority 1.0))

(defun set-target-sharability (v)
    (if (and (> v 0) (<= v 1))
        (let ((target (current-target)))
            (if target
                (let (
                    (id (get-id target)))
	                    (set-property id "sharability" v))
	            (no-target)))
        (error-message
            (concat "sharability " (number-to-string v) " is outside of range (0, 1]"))))

(defun tn-set-target-sharability-1 ()
    (interactive)
    (set-target-sharability 0.25))

(defun tn-set-target-sharability-2 ()
    (interactive)
    (set-target-sharability 0.5))

(defun tn-set-target-sharability-3 ()
    (interactive)
    (set-target-sharability 0.75))

(defun tn-set-target-sharability-4 ()
    (interactive)
    (set-target-sharability 1.0))

(defun set-target-weight (v)
    (if (and (> v 0) (<= v 1))
        (let ((target (current-target)))
            (if target
                (let (
                    (id (get-id target)))
	                    (set-property id "weight" v))
	            (no-target)))
        (error-message
            (concat "weight " (number-to-string v) " is outside of range (0, 1]"))))

(defun tn-set-target-weight-1 ()
    (interactive)
    (set-target-weight 0.25))

(defun tn-set-target-weight-2 ()
    (interactive)
    (set-target-weight 0.5))

(defun tn-set-target-weight-3 ()
    (interactive)
    (set-target-weight 0.75))

(defun tn-set-target-weight-4 ()
    (interactive)
    (set-target-weight 1.0))


;; INTERFACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun info-message (msg)
    (message (concat "Info: " msg)))

(defun error-message (msg)
    (message (concat "Error: " msg)))

(defun tn-visit-url-at-point ()
    (interactive)
    (goto-address-at-point))  ;; defined in Emacs goto-addr.el

(defun browse-target-value (value-to-url)
    (let ((value (current-target-value)))
        (if value
            (browse-url (funcall value-to-url value))
            (no-target))))

(defun tn-browse-target-value-as-url ()
    (interactive)
    (browse-target-value (lambda (value)
        value)))

(defun tn-browse-target-alias ()
    (interactive)
    (let ((alias (current-target-alias)))
        (if alias
            (browse-url alias)
            (no-target))))

(defun tn-browse-target-value-in-amazon ()
    (interactive)
    (browse-target-value (lambda (value)
        (concat "http://www.amazon.com/s?ie=UTF8&index=blended&link_code=qs&field-keywords=" (w3m-url-encode-string value)))))

(defun tn-browse-target-value-in-ebay ()
    (interactive)
    (browse-target-value (lambda (value)
        (concat "http://www.ebay.com/sch/i.html?_nkw=" (w3m-url-encode-string value)))))

(defun tn-browse-target-value-in-delicious ()
    (interactive)
    (browse-target-value (lambda (value)
        (concat "http://www.delicious.com/search?p=" (w3m-url-encode-string value)))))

(defun tn-browse-target-value-in-google ()
    (interactive)
    (browse-target-value (lambda (value)
        (concat "http://www.google.com/search?ie=UTF-8&q=" (w3m-url-encode-string value)))))

(defun tn-browse-target-value-in-google-scholar ()
    (interactive)
    (browse-target-value (lambda (value)
        (concat "http://scholar.google.com/scholar?q=" (w3m-url-encode-string value)))))

(defun tn-browse-target-value-in-google-maps ()
    (interactive)
    (browse-target-value (lambda (value)
        (concat "http://maps.google.com/maps?q=" (w3m-url-encode-string value)))))

(defun tn-browse-target-value-in-twitter ()
    (interactive)
    (browse-target-value (lambda (value)
        (concat "http://twitter.com/#!/search/" (w3m-url-encode-string value)))))

(defun tn-browse-target-value-in-wikipedia ()
    (interactive)
    (browse-target-value (lambda (value)
        (concat "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&search=" (w3m-url-encode-string value)))))

(defun tn-browse-target-value-in-youtube ()
    (interactive)
    (browse-target-value (lambda (value)
        (concat "http://www.youtube.com/results?search_query=" (w3m-url-encode-string value)))))

(defvar tn-date-format "%Y-%m-%d")
(defvar tn-time-format "%H:%M")
(defvar tn-time-with-seconds-format "%H:%M:%S")

(defun insert-current-date ()
  "insert the current date into the current buffer."
       (interactive)
       (insert (format-time-string tn-date-format (current-time))))
(defun insert-current-time ()
  "insert the current time into the current buffer."
       (interactive)
       (insert (format-time-string tn-time-format (current-time))))
(defun insert-current-time-with-seconds ()
  "insert the current time into the current buffer."
       (interactive)
       (insert (format-time-string tn-time-with-seconds-format (current-time))))

(defun insert-attr-sharability-1 ()
    (interactive)
    (insert "\n                @sharability 0.25\n"))
(defun insert-attr-sharability-2 ()
    (interactive)
    (insert "\n                @sharability 0.5\n"))
(defun insert-attr-sharability-3 ()
    (interactive)
    (insert "\n                @sharability 0.75\n"))
(defun insert-attr-sharability-4 ()
    (interactive)
    (insert "\n                @sharability 1.0\n"))

(defun insert-attr-weight-1 ()
    (interactive)
    (insert "\n                @weight 0.25\n"))
(defun insert-attr-weight-2 ()
    (interactive)
    (insert "\n                @weight 0.5\n"))
(defun insert-attr-weight-3 ()
    (interactive)
    (insert "\n                @weight 0.75\n"))
(defun insert-attr-weight-4 ()
    (interactive)
    (insert "\n                @weight 1.0\n"))

(defun copy-to-clipboard (g)
    (let ((buffer (get-buffer-create "*temp*")))
        (with-current-buffer buffer
            (unwind-protect
                 (insert g)
                 (let ((beg 1) (end (+ (length g) 1)))
                    (clipboard-kill-ring-save beg end))
                (kill-buffer buffer)))))

;; note: for some reason, the id doesn't stay invisible when you paste it, although it stays light gray
(defun tn-copy-target-reference-to-clipboard ()
    (interactive)
    (let ((id (current-target-id)))
        (if id
            (copy-to-clipboard (concat "*" (create-id-suffix id)))
            (no-target))))

(defun tn-copy-target-value-to-clipboard ()
    (interactive)
    (let ((value (current-target-value)))
        (if value
            (copy-to-clipboard value)
            (no-target))))

(defun tn-preview-target-latex-math ()
    (interactive)
    (end-of-line)
    (backward-word)
    (latex-math-preview-expression))

(defun tn-new-note ()
    (interactive)
    (tn-enter-edit-view)
    (beginning-of-buffer)
    (kill-new "         * \n")(yank)
    (beginning-of-buffer)(end-of-line))


(global-set-key (kbd "C-c a")           'tn-visit-url-at-point)
(global-set-key (kbd "C-c d")           'tn-duplicates)
(global-set-key (kbd "C-c e")           'tn-export)
(global-set-key (kbd "C-c f")           'tn-find-roots)
(global-set-key (kbd "C-c h")           'tn-history)
(global-set-key (kbd "C-c n")           'tn-new-note)
(global-set-key (kbd "C-c p")           'tn-push-view)
(global-set-key (kbd "C-c P")           'tn-priorities)
(global-set-key (kbd "C-c r")           'tn-ripple-query)
(global-set-key (kbd "C-c s")           'tn-search)
(global-set-key (kbd "C-c t")           'tn-visit-target)
(global-set-key (kbd "C-c u")           'tn-refresh-view)
(global-set-key (kbd "C-c C-a C-s 1")   'insert-attr-sharability-1)
(global-set-key (kbd "C-c C-a C-s 2")   'insert-attr-sharability-2)
(global-set-key (kbd "C-c C-a C-s 3")   'insert-attr-sharability-3)
(global-set-key (kbd "C-c C-a C-s 4")   'insert-attr-sharability-4)
(global-set-key (kbd "C-c C-a C-s a")   'insert-attr-sharability-1)
(global-set-key (kbd "C-c C-a C-s s")   'insert-attr-sharability-2)
(global-set-key (kbd "C-c C-a C-s d")   'insert-attr-sharability-3)
(global-set-key (kbd "C-c C-a C-s f")   'insert-attr-sharability-4)
(global-set-key (kbd "C-c C-a C-w 1")   'insert-attr-weight-1)
(global-set-key (kbd "C-c C-a C-w 2")   'insert-attr-weight-2)
(global-set-key (kbd "C-c C-a C-w 3")   'insert-attr-weight-3)
(global-set-key (kbd "C-c C-a C-w 4")   'insert-attr-weight-4)
(global-set-key (kbd "C-c C-a C-w a")   'insert-attr-weight-1)
(global-set-key (kbd "C-c C-a C-w s")   'insert-attr-weight-2)
(global-set-key (kbd "C-c C-a C-w d")   'insert-attr-weight-3)
(global-set-key (kbd "C-c C-a C-w f")   'insert-attr-weight-4)
(global-set-key (kbd "C-c C-a d")       'insert-current-date)
(global-set-key (kbd "C-c C-a s")       'insert-current-time-with-seconds)
(global-set-key (kbd "C-c C-a t")       'insert-current-time)
(global-set-key (kbd "C-c C-d ,")       'tn-decrease-depth)
(global-set-key (kbd "C-c C-d .")       'tn-increase-depth)
(global-set-key (kbd "C-c C-f")         'tn-push-point)
(global-set-key (kbd "C-c C-l")         'tn-goto-line)

;; copied verbatim from the "C-c C-t" section
;; TODO: deduplicate this code in some way
;;(global-set-key (kbd "C-c C-r a")       'tn-browse-root-value-as-url)
;;(global-set-key (kbd "C-c C-r c")       'tn-copy-root-value-to-clipboard)
;;(global-set-key (kbd "C-c C-r C-a b")   'tn-browse-root-alias)
;;(global-set-key (kbd "C-c C-r C-b a")   'tn-browse-root-value-in-amazon)
;;(global-set-key (kbd "C-c C-r C-b e")   'tn-browse-root-value-in-ebay)
;;(global-set-key (kbd "C-c C-r C-b d")   'tn-browse-root-value-in-delicious)
;;(global-set-key (kbd "C-c C-r C-b g")   'tn-browse-root-value-in-google)
;;(global-set-key (kbd "C-c C-r C-b m")   'tn-browse-root-value-in-google-maps)
;;(global-set-key (kbd "C-c C-r C-b s")   'tn-browse-root-value-in-google-scholar)
;;(global-set-key (kbd "C-c C-r C-b t")   'tn-browse-root-value-in-twitter)
;;(global-set-key (kbd "C-c C-r C-b w")   'tn-browse-root-value-in-wikipedia)
;;(global-set-key (kbd "C-c C-r C-b y")   'tn-browse-root-value-in-youtube)
;;(global-set-key (kbd "C-c C-r i")       'tn-root-info)
;;(global-set-key (kbd "C-c C-r l")       'tn-preview-root-latex-math)

(global-set-key (kbd "C-c C-s 1")       'tn-set-default-sharability-1)
(global-set-key (kbd "C-c C-s 2")       'tn-set-default-sharability-2)
(global-set-key (kbd "C-c C-s 3")       'tn-set-default-sharability-3)
(global-set-key (kbd "C-c C-s 4")       'tn-set-default-sharability-4)
(global-set-key (kbd "C-c C-s a")       'tn-set-default-sharability-1)
(global-set-key (kbd "C-c C-s s")       'tn-set-default-sharability-2)
(global-set-key (kbd "C-c C-s d")       'tn-set-default-sharability-3)
(global-set-key (kbd "C-c C-s f")       'tn-set-default-sharability-4)
(global-set-key (kbd "C-c C-s C-[ 0")   'tn-set-min-sharability-0)
(global-set-key (kbd "C-c C-s C-[ 1")   'tn-set-min-sharability-1)
(global-set-key (kbd "C-c C-s C-[ 2")   'tn-set-min-sharability-2)
(global-set-key (kbd "C-c C-s C-[ 3")   'tn-set-min-sharability-3)
(global-set-key (kbd "C-c C-s C-[ 4")   'tn-set-min-sharability-4)
(global-set-key (kbd "C-c C-s C-[ z")   'tn-set-min-sharability-0)
(global-set-key (kbd "C-c C-s C-[ a")   'tn-set-min-sharability-1)
(global-set-key (kbd "C-c C-s C-[ s")   'tn-set-min-sharability-2)
(global-set-key (kbd "C-c C-s C-[ d")   'tn-set-min-sharability-3)
(global-set-key (kbd "C-c C-s C-[ f")   'tn-set-min-sharability-4)
(global-set-key (kbd "C-c C-s C-] 0")   'tn-set-max-sharability-0)
(global-set-key (kbd "C-c C-s C-] 1")   'tn-set-max-sharability-1)
(global-set-key (kbd "C-c C-s C-] 2")   'tn-set-max-sharability-2)
(global-set-key (kbd "C-c C-s C-] 3")   'tn-set-max-sharability-3)
(global-set-key (kbd "C-c C-s C-] 4")   'tn-set-max-sharability-4)
(global-set-key (kbd "C-c C-s C-] z")   'tn-set-max-sharability-0)
(global-set-key (kbd "C-c C-s C-] a")   'tn-set-max-sharability-1)
(global-set-key (kbd "C-c C-s C-] s")   'tn-set-max-sharability-2)
(global-set-key (kbd "C-c C-s C-] d")   'tn-set-max-sharability-3)
(global-set-key (kbd "C-c C-s C-] f")   'tn-set-max-sharability-4)
(global-set-key (kbd "C-c C-t a")       'tn-browse-target-value-as-url)
(global-set-key (kbd "C-c C-t r")       'tn-copy-target-reference-to-clipboard)
(global-set-key (kbd "C-c C-t c")       'tn-copy-target-value-to-clipboard)
(global-set-key (kbd "C-c C-t C-a b")   'tn-browse-target-alias)
(global-set-key (kbd "C-c C-t C-b a")   'tn-browse-target-value-in-amazon)
(global-set-key (kbd "C-c C-t C-b e")   'tn-browse-target-value-in-ebay)
(global-set-key (kbd "C-c C-t C-b d")   'tn-browse-target-value-in-delicious)
(global-set-key (kbd "C-c C-t C-b g")   'tn-browse-target-value-in-google)
(global-set-key (kbd "C-c C-t C-b m")   'tn-browse-target-value-in-google-maps)
(global-set-key (kbd "C-c C-t C-b s")   'tn-browse-target-value-in-google-scholar)
(global-set-key (kbd "C-c C-t C-b t")   'tn-browse-target-value-in-twitter)
(global-set-key (kbd "C-c C-t C-b w")   'tn-browse-target-value-in-wikipedia)
(global-set-key (kbd "C-c C-t C-b y")   'tn-browse-target-value-in-youtube)
(global-set-key (kbd "C-c C-t i")       'tn-target-info)
(global-set-key (kbd "C-c C-t l")       'tn-preview-target-latex-math)
(global-set-key (kbd "C-c C-t C-p 0")   'tn-set-target-priority-0)
(global-set-key (kbd "C-c C-t C-p 1")   'tn-set-target-priority-1)
(global-set-key (kbd "C-c C-t C-p 2")   'tn-set-target-priority-2)
(global-set-key (kbd "C-c C-t C-p 3")   'tn-set-target-priority-3)
(global-set-key (kbd "C-c C-t C-p 4")   'tn-set-target-priority-4)
(global-set-key (kbd "C-c C-t C-p z")   'tn-set-target-priority-0)
(global-set-key (kbd "C-c C-t C-p a")   'tn-set-target-priority-1)
(global-set-key (kbd "C-c C-t C-p s")   'tn-set-target-priority-2)
(global-set-key (kbd "C-c C-t C-p d")   'tn-set-target-priority-3)
(global-set-key (kbd "C-c C-t C-p f")   'tn-set-target-priority-4)
(global-set-key (kbd "C-c C-t C-s 1")   'tn-set-target-sharability-1)
(global-set-key (kbd "C-c C-t C-s 2")   'tn-set-target-sharability-2)
(global-set-key (kbd "C-c C-t C-s 3")   'tn-set-target-sharability-3)
(global-set-key (kbd "C-c C-t C-s 4")   'tn-set-target-sharability-4)
(global-set-key (kbd "C-c C-t C-s a")   'tn-set-target-sharability-1)
(global-set-key (kbd "C-c C-t C-s s")   'tn-set-target-sharability-2)
(global-set-key (kbd "C-c C-t C-s d")   'tn-set-target-sharability-3)
(global-set-key (kbd "C-c C-t C-s f")   'tn-set-target-sharability-4)
(global-set-key (kbd "C-c C-t C-w 1")   'tn-set-target-weight-1)
(global-set-key (kbd "C-c C-t C-w 2")   'tn-set-target-weight-2)
(global-set-key (kbd "C-c C-t C-w 3")   'tn-set-target-weight-3)
(global-set-key (kbd "C-c C-t C-w 4")   'tn-set-target-weight-4)
(global-set-key (kbd "C-c C-t C-w a")   'tn-set-target-weight-1)
(global-set-key (kbd "C-c C-t C-w s")   'tn-set-target-weight-2)
(global-set-key (kbd "C-c C-t C-w d")   'tn-set-target-weight-3)
(global-set-key (kbd "C-c C-t C-w f")   'tn-set-target-weight-4)
(global-set-key (kbd "C-c C-v b")       'tn-refresh-to-backward-view)
(global-set-key (kbd "C-c C-v e")       'tn-enter-edit-view)
(global-set-key (kbd "C-c C-v f")       'tn-refresh-to-forward-view)
(global-set-key (kbd "C-c C-v r")       'tn-enter-readonly-view)
;; new bindings (not yet recorded in mob-data)
(global-set-key (kbd "C-c C-v s")       'tn-toggle-emacspeak)
(global-set-key (kbd "C-c C-v t")       'tn-set-value-truncation-length)
(global-set-key (kbd "C-c C-w 1")       'tn-set-default-weight-1)
(global-set-key (kbd "C-c C-w 2")       'tn-set-default-weight-2)
(global-set-key (kbd "C-c C-w 3")       'tn-set-default-weight-3)
(global-set-key (kbd "C-c C-w 4")       'tn-set-default-weight-4)
(global-set-key (kbd "C-c C-w a")       'tn-set-default-weight-1)
(global-set-key (kbd "C-c C-w s")       'tn-set-default-weight-2)
(global-set-key (kbd "C-c C-w d")       'tn-set-default-weight-3)
(global-set-key (kbd "C-c C-w f")       'tn-set-default-weight-4)
(global-set-key (kbd "C-c C-w C-[ 0")   'tn-set-min-weight-0)
(global-set-key (kbd "C-c C-w C-[ 1")   'tn-set-min-weight-1)
(global-set-key (kbd "C-c C-w C-[ 2")   'tn-set-min-weight-2)
(global-set-key (kbd "C-c C-w C-[ 3")   'tn-set-min-weight-3)
(global-set-key (kbd "C-c C-w C-[ 4")   'tn-set-min-weight-4)
(global-set-key (kbd "C-c C-w C-[ z")   'tn-set-min-weight-0)
(global-set-key (kbd "C-c C-w C-[ a")   'tn-set-min-weight-1)
(global-set-key (kbd "C-c C-w C-[ s")   'tn-set-min-weight-2)
(global-set-key (kbd "C-c C-w C-[ d")   'tn-set-min-weight-3)
(global-set-key (kbd "C-c C-w C-[ f")   'tn-set-min-weight-4)
(global-set-key (kbd "C-c C-w C-] 0")   'tn-set-max-weight-0)
(global-set-key (kbd "C-c C-w C-] 1")   'tn-set-max-weight-1)
(global-set-key (kbd "C-c C-w C-] 2")   'tn-set-max-weight-2)
(global-set-key (kbd "C-c C-w C-] 3")   'tn-set-max-weight-3)
(global-set-key (kbd "C-c C-w C-] 4")   'tn-set-max-weight-4)
(global-set-key (kbd "C-c C-w C-] z")   'tn-set-max-weight-0)
(global-set-key (kbd "C-c C-w C-] a")   'tn-set-max-weight-1)
(global-set-key (kbd "C-c C-w C-] s")   'tn-set-max-weight-2)
(global-set-key (kbd "C-c C-w C-] d")   'tn-set-max-weight-3)
(global-set-key (kbd "C-c C-w C-] f")   'tn-set-max-weight-4)

(defun tn-toggle-emacspeak ()
    (interactive)
    (dtk-toggle-quiet))

(defun toggle-linum-mode ()
    (interactive)
    (setq tn-enable-linum (not tn-enable-linum))
    (linum-mode tn-enable-linum))

(defun tn-set-value-truncation-length ()
    (interactive)
    (let ((n (string-to-number (read-from-minibuffer "value truncation length: "))))
        (setq tn-value-truncation-length n)))

;; Note: these should perhaps be local settings
(global-set-key (kbd "C-c C-v ;") 'toggle-truncate-lines)
(global-set-key (kbd "C-c C-g l") 'toggle-linum-mode)
(setq-default truncate-lines t)
(if full-colors-supported
    (let ()
        (global-hl-line-mode 1)
        (set-face-background 'hl-line "ivory")))

;; These may or may not be necessary
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


(provide 'tinkernotes)
