;; Brain-mode: the Extend-o-Brain Emacs library
;;
;; Required global variables:
;;
;;     exo-rexster-url: IP, port, and local path to the rexster server
;;     exo-rexster-graph: name of Extend-o-Brain graph served by Rexster
;;
;; For example:
;;
;;     (defun brain-mode ()
;;         (defvar exo-rexster-url "http://localhost:8182")
;;         (defvar exo-rexster-graph "joshkb"))


;; DEPENDENCIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for JSON-formatted messages to and from Extendo services (see json-read-from-string, json-encode)
(require 'json)

;; for line number annotations in buffers (see linum-mode)
(require 'linum)

;; for visiting URLs in a browser (see goto-address-at-point)
(require 'goto-addr)

;; for encryption of sensitive values
(require 'aes)

;; for LaTeX views (nice-to-have, but not essential)
(require 'latex-math-preview)

;;(eval-when-compile (require 'cl))


;; VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq exo-readonly-mode "readonly")
(setq exo-edit-mode "readwrite")
(setq exo-search-mode "search")

(setq exo-sharability-viewstyle "sharability")
(setq exo-inference-viewstyle "inference")

(setq exo-forward-style "forward")
(setq exo-backward-style "backward")

;; Buffer-local variables. Given them initial, global bindings so they're defined before there are actual view buffers.
(setq exo-depth 3)
(setq exo-root-id nil)
(setq exo-title nil)
(setq exo-style exo-forward-style)
;; "private" atoms are hidden to begin with
(setq exo-min-sharability 0.25)
(setq exo-max-sharability 1)
;; default to "average" sharability to begin with
(setq exo-default-sharability 0.5)
(setq exo-future-sharability exo-default-sharability)
;; atoms of all weights are visible to begin with
(setq exo-min-weight 0.0)
(setq exo-max-weight 1.0)
;; default to "average" weight to begin with
(setq exo-default-weight 0.5)
(setq exo-atoms nil)
(setq exo-current-line 1)
(setq exo-mode nil)  ;; Note: 'view-mode' is used by Emacs.
(setq exo-viewstyle exo-sharability-viewstyle)
(setq exo-value-truncation-length 100)


;; DATA MODEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar exo-date-format "%Y-%m-%d")
(defvar exo-time-format "%H:%M")
(defvar exo-time-with-seconds-format "%H:%M:%S")

(defun atom-id-at-point ()
    (let ((line (current-line)))
        (if (string-match "^[0-9A-Za-z@&]*: " line)
            (let ((i3 (string-match ": " line)))
                (let ((s2 (substring line 0 i3)))
                    (if (< 0 (length s2)) s2 nil)))
            (get-text-property (line-beginning-position) 'target-id))))

(defun get-id (atom)
    (cdr (assoc 'id atom)))

(defun get-created (atom)
    (cdr (assoc 'created atom)))

(defun get-value (atom)
    (cdr (assoc 'value atom)))

(defun get-priority (atom)
    (let ((v (assoc 'priority atom)))
        (if v (cdr v) nil)))

(defun get-sharability (atom)
    (let ((v (assoc 'sharability atom)))
        (if v (cdr v) exo-default-sharability)))

(defun get-weight (atom)
    (let ((v (assoc 'weight atom)))
        (if v (cdr v) exo-default-weight)))

(defun get-alias (atom)
    (let ((x (assoc 'alias atom)))
        (if x (cdr x) nil)))

(defun get-type (atom)
    (let ((x (assoc 'type atom)))
        (if x (cdr x) nil)))

(defun current-root-id ()
    exo-root-id)

(defun current-root-value ()
    exo-title)

(defun current-root ()
    (get-atom (current-root-id)))

(defun current-target-value ()
    (let ((g (current-root)))
        (if g (get-value g))))

(defun current-target ()
    (get-atom (atom-id-at-point)))

(defun current-target-value ()
    (let ((g (current-target)))
        (if g (get-value g))))

(defun current-target-alias ()
    (let ((g (current-target)))
        (if g (get-alias g))))

(defun current-target-sharability ()
    (let ((g (current-target)))
        (if g (get-sharability g))))

;; change the default sharability in the new view after a user visits a link or target
;; The default will never be greater than 0.75 unless explicitly set by the user.
(defun future-sharability (s)
    (if s
        (if (<= s 0.75) s 0.75)
        0.5))

(defun get-atom (id)
    (if id
        (if exo-atoms
            (gethash id exo-atoms)
            nil)
        nil))

(defun show-info (atom)
    (let (
        (created (get-created atom))
        (value (get-value atom))
        (weight (get-weight atom))
        (sharability (get-sharability atom))
        (priority (get-priority atom))
        (alias (get-alias atom))
        (type (get-type atom)))
            (message (concat
                 (if type (concat "type: " type ", "))
                 "weight: " (number-to-string weight)
                 ", sharability: " (number-to-string sharability)
                 (if priority (concat ", priority: " (number-to-string priority)) "")
                 ", created: " (format-time-string "%Y-%m-%dT%H:%M:%S%z" (seconds-to-time (/ created 1000.0)))
                 ", value: " value
                 (if alias (concat ", alias: " alias) "")))))

(defun numeric-value (json prop default)
    (let ((v (assoc prop json)))
        (if v (string-to-number (cdr v)) default)))


;; INTERFACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun info-message (msg)
    (message (concat "Info: " msg)))

(defun error-message (msg)
    (message (concat "Error: " msg)))

(setq fast-numbers '(
    (?0 0) (?1 1) (?2 2) (?3 3) (?4 4) (?5 5) (?6 6) (?7 7) (?8 8) (?9 9)
    (?z 0) (?a 1) (?s 2) (?d 3) (?f 4) (?g 5) (?h 6) (?j 7) (?k 8) (?l 9) (?; 10)))

(defun number-shorthand-to-number (c)
    (interactive)
    (let ((l (assoc c fast-numbers)))
        (if l (car (cdr l)) (error-message (concat "no number associated with character " (char-to-string c))))))

;; note: working in Aquamacs, but apparently not in the terminal Emacs 24 on Mac OS X
(defun copy-to-clipboard (g)
    (let ((buffer (get-buffer-create "*temp*")))
        (with-current-buffer buffer
            (unwind-protect
                 (insert g)
                 (let ((beg 1) (end (+ (length g) 1)))
                    (clipboard-kill-ring-save beg end))
                (kill-buffer buffer)))))


;; NAVIGATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-line ()
    (interactive)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun no-link ()
    (error-message "there is no link associated with this line"))

(defun no-target ()
    (error-message "there is no target associated with this line"))

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

(defun visit-target-value (value-selector value-to-url)
    (lexical-let ((vs value-selector) (vu value-to-url))
        (lambda () (interactive)
            (let ((value (funcall vs)))
                (if value
                    (browse-url (funcall vu value))
                    (no-target))))))


;; COMMUNICATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun base-url ()
    (concat exo-rexster-url "/graphs/" exo-rexster-graph "/extendo/"))

(defun receive-view (mode)
    (lexical-let ((m mode))
        (lambda (status) (receive-view-internal status m))))

(defun show-http-response-status (status)
    (let ((msg (cdr (assoc 'message json)))
        (error (cdr (assoc 'error json))))
            (if error
                (error-message error)
                (error-message msg))))

(defun receive-view-internal (status mode)
    (let ((json (json-read-from-string (strip-http-headers (buffer-string))))
          (editable (equal mode exo-edit-mode)))
        (if status
            (show-http-response-status status json)
            (let (
                (root (cdr (assoc 'root json)))
                (view (cdr (assoc 'view json)))
                (depth (numeric-value json 'depth nil))

                ;; if the service doesn't specify these values, they will carry over from the previous buffer state
                (min-sharability (numeric-value json 'minSharability exo-min-sharability))
                (max-sharability (numeric-value json 'maxSharability exo-max-sharability))
                (default-sharability (numeric-value json 'defaultSharability exo-default-sharability))
                (min-weight (numeric-value json 'minWeight exo-min-weight))
                (max-weight (numeric-value json 'maxWeight exo-max-weight))
                (default-weight (numeric-value json 'defaultWeight exo-default-weight))

                (style (cdr (assoc 'style json)))
                (title (cdr (assoc 'title json))))
                    (switch-to-buffer (view-name root json))
                    (make-local-variable 'exo-root-id)
                    (make-local-variable 'exo-depth)
                    (make-local-variable 'exo-style)
                    (make-local-variable 'exo-title)
                    (make-local-variable 'exo-min-sharability)
                    (make-local-variable 'exo-max-sharability)
                    (make-local-variable 'exo-default-sharability)
                    (make-local-variable 'exo-min-weight)
                    (make-local-variable 'exo-max-weight)
                    (make-local-variable 'exo-atoms)
                    (make-local-variable 'exo-current-line)
                    (make-local-variable 'exo-mode)
                    (make-local-variable 'exo-value-truncation-length)
                    (setq exo-root-id root)
                    (if (equal mode exo-search-mode)
                        ;; Always leave a search view with depth 1, rather than that of the last view.
                        ;; The user experience is a little unpredictable otherwise.
                        (setq exo-depth 1)
                        (if depth (setq exo-depth depth)))
                    (setq exo-min-sharability min-sharability)
                    (setq exo-max-sharability max-sharability)
                    (setq exo-default-sharability exo-future-sharability)
                    (setq exo-min-weight min-weight)
                    (setq exo-max-weight max-weight)
                    (setq exo-default-weight default-weight)
                    (setq exo-style (if style style exo-style))
                    (setq exo-title title)
                    (setq exo-atoms (make-hash-table :test 'equal))
                    (setq exo-mode mode)
                    (setq buffer-read-only nil)
                    (erase-buffer)
                    (write-view editable (cdr (assoc 'children view)) 0)
                    (beginning-of-buffer)
                    (setq visible-cursor t)
                    ;; Try to move to the corresponding line in the previous view.
                    ;; This is not always possible and not always helpful, but it is often both.
                    (beginning-of-line exo-current-line)
                    (setq buffer-read-only (not editable))
                    ;; always include line numbers in views
                    (linum-mode t)
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

(defun receive-inference-results (status)
    (let ((json (json-read-from-string (strip-http-headers (buffer-string)))))
        (if status
            (let ((msg (cdr (assoc 'message json)))
                (error (cdr (assoc 'error json))))
                    (if error
                        (error-message error)
                        (error-message msg)))
            (info-message "type inference completed successfully"))))

(defun request-view (preserve-line mode root depth style mins maxs defaults minw maxw defaultw)
    (setq exo-current-line (if preserve-line (line-number-at-pos) 1))
    (setq exo-future-sharability defaults)
    (http-get (request-view-url root depth style mins maxs defaults minw maxw defaultw) (receive-view mode)))

(defun filter-json (mins maxs defaults minw maxw defaultw)
    (list :minSharability mins :maxSharability maxs :defaultSharability defaults :minWeight minw :maxWeight maxw :defaultWeight defaultw))

(defun request-view-url (root depth style mins maxs defaults minw maxw defaultw)
    (concat (base-url) "view?request=" (w3m-url-encode-string (json-encode
        (list :root root :depth depth :style style :includeTypes (if (using-inference) "true" "false") :filter (filter-json mins maxs defaults minw maxw defaultw))))))

(defun request-history (mins maxs minw maxw)
    (setq exo-current-line 1)
    (setq exo-future-sharability exo-default-sharability)
    (http-get
        (concat (base-url) "history?request=" (w3m-url-encode-string (json-encode
            (list :filter (filter-json mins maxs exo-default-sharability minw maxw exo-default-weight)))))
        (receive-view exo-search-mode)))

(defun request-events (depth)
    (setq exo-current-line 1)
    (setq exo-future-sharability exo-default-sharability)
    (http-get
        (concat (base-url) "get-events?request=" (w3m-url-encode-string (json-encode
            (list :depth depth))))
        (receive-view exo-search-mode)))

(defun request-duplicates (mins maxs minw maxw)
    (setq exo-current-line 1)
    (setq exo-future-sharability exo-default-sharability)
    (http-get
        (concat (base-url) "duplicates?request=" (w3m-url-encode-string (json-encode
            (list :filter (filter-json mins maxs exo-default-sharability minw maxw exo-default-weight)))))
        (receive-view exo-search-mode)))

(defun request-search-results (query style mins maxs minw maxw)
    (setq exo-current-line 1)
    (setq exo-future-sharability exo-default-sharability)
    (http-get
        (concat (base-url) "search?request=" (w3m-url-encode-string (json-encode
            (list :query query :valueCutoff exo-value-truncation-length :depth 1 :style style
                :filter (filter-json mins maxs exo-default-sharability minw maxw exo-default-weight)))))
        (receive-view exo-search-mode)))

(defun request-priorities-results (mins maxs minw maxw)
    (setq exo-current-line 1)
    (setq exo-future-sharability exo-default-sharability)
    (http-get
        (concat (base-url) "priorities?request=" (w3m-url-encode-string (json-encode
            (list :maxResults 100
                :filter (filter-json mins maxs exo-default-sharability minw maxw exo-default-weight)))))
        (receive-view exo-search-mode)))

(defun request-find-isolated-atoms-results (mins maxs minw maxw)
    (setq exo-current-line 1)
    (setq exo-future-sharability exo-default-sharability)
    (http-get
        (concat (base-url) "find-isolated-atoms?request=" (w3m-url-encode-string (json-encode
            (list
                :filter (filter-json mins maxs exo-default-sharability minw maxw exo-default-weight)))))
        (receive-view exo-search-mode)))

(defun request-find-roots-results (style mins maxs minw maxw)
    (setq exo-current-line 1)
    (setq exo-future-sharability exo-default-sharability)
    (http-get
        (concat (base-url) "find-roots?request=" (w3m-url-encode-string (json-encode
            (list :style style :depth 1
                :filter (filter-json mins maxs exo-default-sharability minw maxw exo-default-weight)))))
        (receive-view exo-search-mode)))

(defun request-remove-isolated-atoms (mins maxs minw maxw)
    (setq exo-current-line 1)
    (setq exo-future-sharability exo-default-sharability)
    (http-get
        (concat (base-url) "remove-isolated-atoms?request=" (w3m-url-encode-string (json-encode
            (list
                :filter (filter-json mins maxs exo-default-sharability minw maxw exo-default-weight)))))
        (lambda (status)
            (interactive)
            (if status
                (let ((json (json-read-from-string (strip-http-headers (buffer-string))))
                    (show-http-response-status status json)))
                (message "removed isolated atoms")))))

(defun request-ripple-results (query style mins maxs minw maxw)
    (setq exo-current-line 1)
    (setq exo-future-sharability exo-default-sharability)
    (http-get
        (concat (base-url) "ripple?request=" (w3m-url-encode-string (json-encode
            (list :query query :depth 1 :style style
                :filter (filter-json mins maxs exo-default-sharability minw maxw exo-default-weight)))))
             (receive-view exo-search-mode)))

(defun do-export ()
    (http-get
        (concat (base-url) "export") 'receive-export-results))

(defun do-infer-types ()
    (http-get
        (concat (base-url) "infer-types") 'receive-inference-results))


;; VIEWS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun view-name (root-id json)
    (let ((title (cdr (assoc 'title json))))
        (if root-id
            (let ((name
                (if (> (length title) 20)
                    (concat (substring title 0 20) "...")
                    title)))
                (concat name " [" root-id "]"))
            title)))

(defun using-inference ()
    (equal exo-viewstyle exo-inference-viewstyle))

;; unused colors: black/gray, orange
(setq sharability-base-colors  '("#660000" "#604000" "#005000" "#000066"))
(setq sharability-bright-colors  '("#D00000" "#D0B000" "#00B000" "#0000D0"))
(setq sharability-reduced-colors '("red" "red" "blue" "blue"))
(setq inference-base-colors '("#660066" "#006666"))
(setq inference-bright-colors '("#FF00FF" "#00FFFF"))

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

(defun find-color (weight sharability bright type)
    (let ((s
        (if (using-inference)
            (elt (if bright inference-bright-colors inference-base-colors) (if type 0 1))
            (elt (if bright sharability-bright-colors sharability-base-colors) (- (ceiling (* sharability 4)) 1)))))
        (color-string
            (fade-color (color-part-red s) weight)
            (fade-color (color-part-green s) weight)
            (fade-color (color-part-blue s) weight))))

(setq full-colors-supported (> (length (defined-colors)) 8))

(defun colorize (text weight sharability underline bold bright type background)
    (let ((color (if full-colors-supported
            (find-color weight sharability bright type)
            (elt sharability-reduced-colors (- (ceiling (* sharability 4)) 1)))))
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

(defun create-id-infix (id)
    (light-gray (concat (propertize (concat " :" id) 'invisible t) ":") "white"))

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
		        (target-alias (get-alias json))
		        (target-type (get-type json)))
		            (if target-id (puthash target-id json exo-atoms))
		            (if (not target-id) (error "missing target id"))
		            ;; black space at the end of the line makes the next line black when you enter a newline and continue typing
		            (let ((line "") (id-infix (create-id-infix target-id)))
		                (if (not editable)
                            (setq id-infix (propertize id-infix 'invisible t)))
                        (let ((space ""))
                            (loop for i from 1 to tree-indent do (setq space (concat space " ")))
                            (setq line (concat line space)))
                        (let ((bullet (if target-has-children "+" "\u00b7")))   ;; previously: "-" or "\u25ba"
                            (setq line (concat line
                                (colorize bullet target-weight target-sharability nil nil target-alias target-type "white")
                                id-infix
                                " "
                                (colorize target-value target-weight target-sharability nil nil target-alias target-type "white")
                                 "\n")))
                        (insert (propertize line 'target-id target-id)))
                    (write-view editable children (+ tree-indent 4))))))

(defun num-or-nil-to-string (n)
    (if n (number-to-string n) "nil"))

(defun view-info ()
    (concat
        "(root: " exo-root-id
         " :depth " (num-or-nil-to-string exo-depth)
         " :style " exo-style
         " :sharability [" (num-or-nil-to-string exo-min-sharability) ", " (num-or-nil-to-string exo-default-sharability) ", " (num-or-nil-to-string exo-max-sharability) "]"
         " :weight [" (num-or-nil-to-string exo-min-weight) ", " (num-or-nil-to-string exo-default-weight) ", " (num-or-nil-to-string exo-max-weight) "]"
         " :value \"" exo-title "\")"))  ;; TODO: actually escape the title string

(defun mode-for-visit ()
    (if (or (equal exo-mode exo-edit-mode) (equal exo-mode exo-readonly-mode))
        exo-mode
        exo-readonly-mode))

(defun current-view-mode-is-atom-view ()
    (or
        (equal exo-mode exo-readonly-mode)
        (equal exo-mode exo-edit-mode)))

(defun in-view ()
    (if (or
;;            (equal exo-mode exo-search-mode)
            (equal exo-mode exo-readonly-mode)
            (equal exo-mode exo-edit-mode))
        t
	    (and (error-message "this command can only be executed from within an atom view") nil)))

(defun in-edit-view ()
    (if (equal exo-mode exo-edit-mode)
        t
	    (and (error-message "this command can only be executed from within an edit view") nil)))


;; UPDATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-default-weight (s)
    (if (and (in-view) (> s 0) (<= s 1))
        (setq exo-default-weight s)
        (error-message
            (concat "weight " (number-to-string s) " is outside of range (0, 1]"))))

(defun set-min-weight (s)
    (if (and (in-view) (>= s 0) (<= s 1))
        (request-view t exo-mode exo-root-id exo-depth exo-style exo-min-sharability exo-max-sharability exo-default-sharability s exo-max-weight exo-default-weight)
        (error-message
            (concat "min weight " (number-to-string s) " is outside of range [0, 1]"))))

(defun set-default-sharability (s)
    (if (and (in-view) (> s 0) (<= s 1))
        (setq exo-default-sharability s)
        (error-message
            (concat "sharability " (number-to-string s) " is outside of range (0, 1]"))))

(defun set-min-sharability (s)
    (if (and (in-view) (>= s 0) (<= s 1))
        (request-view t exo-mode exo-root-id exo-depth exo-style s exo-max-sharability exo-default-sharability exo-min-weight exo-max-weight exo-default-weight)
        (error-message
            (concat "min sharability " (number-to-string s) " is outside of range [0, 1]"))))

(defun set-property (id name value)
    (interactive)
    (if (in-view)
        (lexical-let (
                (mode exo-mode)
                (url (request-view-url exo-root-id exo-depth exo-style exo-min-sharability exo-max-sharability exo-default-sharability exo-min-weight exo-max-weight exo-default-weight)))
            (setq exo-current-line (line-number-at-pos))
            (setq exo-future-sharability exo-default-sharability)
            (http-get
                (concat (base-url) "set?request=" (w3m-url-encode-string (json-encode
                    (list :id id :name name :value value))))
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


;; API FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exo-atom-info (selector)
    "display, in the minibuffer, information about an atom produced by SELECTOR"
    (lexical-let ((as selector))
        (lambda () (interactive)
            (let ((atom (funcall as)))
                (if atom
                    (show-info atom)
                    (no-target))))))

;; note: for some reason, the id doesn't stay invisible when you paste it, although it stays light gray
(defun exo-copy-target-reference-to-clipboard ()
    "copy a reference to the atom at point to the system clipboard"
    (interactive)
    (let ((id (atom-id-at-point)))
        (if id
            (copy-to-clipboard (concat "*" (create-id-infix id)))
            (no-target))))

(defun exo-copy-target-value-to-clipboard ()
    "copy the value of the atom at point to the system clipboard"
    (interactive)
    (let ((value (current-target-value)))
        (if value
            (copy-to-clipboard value)
            (no-target))))

(defun exo-duplicates ()
    "retrieve a list of atoms with duplicate values"
    (interactive)
    (request-duplicates
        exo-min-sharability exo-max-sharability exo-min-weight exo-max-weight))

(defun exo-emacsclient-eval (function)
    "evaluate FUNCTION from emacsclient as if a user had typed it into the current buffer"
    (set-buffer (window-buffer (selected-window)))
    (funcall function))

(defun exo-enter-edit-view ()
    "enter edit (read/write) mode in the current view"
    (interactive)
    (if (and (in-view) (equal exo-mode exo-readonly-mode))
        (request-view t exo-edit-mode exo-root-id exo-depth exo-style exo-min-sharability exo-max-sharability exo-default-sharability exo-min-weight exo-max-weight exo-default-weight)))

(defun exo-enter-readonly-view ()
    "enter read-only mode in the current view"
    (interactive)
    (if (and (in-view) (equal exo-mode exo-edit-mode))
        (request-view t exo-readonly-mode exo-root-id exo-depth exo-style exo-min-sharability exo-max-sharability exo-default-sharability exo-min-weight exo-max-weight exo-default-weight)))

(defun exo-events ()
    "retrieve the Extend-o-Brain event stack (e.g. notifications of gestural events), ordered by decreasing time stamp"
    (interactive)
    (request-events 2))

(defun exo-export ()
    "export the Extend-o-Brain graph to the file system"
    (interactive)
    (message "exporting")
    (do-export))

(defun exo-find-isolated-atoms ()
    "retrieve a list of isolated atoms (i.e. atoms with neither parents nor children) in the Extend-o-Brain graph"
    (interactive)
        (request-find-isolated-atoms-results
            exo-min-sharability exo-max-sharability exo-min-weight exo-max-weight))

(defun exo-remove-isolated-atoms ()
    "remove all isolated atoms (i.e. atoms with neither parents nor children) from the Extend-o-Brain graph"
    (interactive)
        (request-remove-isolated-atoms
            exo-min-sharability exo-max-sharability exo-min-weight exo-max-weight))

(defun exo-find-roots ()
    "retrieve a list of roots (i.e. atoms with no parents) in the Extend-o-Brain graph"
    (interactive)
        (request-find-roots-results
            exo-style
            exo-min-sharability exo-max-sharability exo-min-weight exo-max-weight))

(defun exo-goto-line (address)
    "move point to the line represented by ADDRESS"
    (interactive)
    (let ((line (address-to-lineno (handle-changewindow address))))
        (if line
            (goto-line line)
            (error-message "invalid line address"))))

(defun exo-history ()
    "retrieve a list of the most recently viewed or updated atoms, in decreasing order of recency"
    (interactive)
    (request-history
        exo-min-sharability exo-max-sharability exo-min-weight exo-max-weight))

(defun exo-infer-types ()
    "perform type inference on the Extend-o-Brain knowledge base, adding type annotations"
    (interactive)
    (message "performing type inference")
    (do-infer-types))

(defun exo-insert-attr-priority (expr)
    "insert a line to set the priority of an atom to the value given by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (insert (concat "\n                @priority " (number-to-string (/ n 4.0)) "\n")))))

(defun exo-insert-attr-sharability (expr)
    "insert a line to set the sharability of an atom to the value given by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (insert (concat "\n                @sharability " (number-to-string (/ n 4.0)) "\n")))))

(defun exo-insert-attr-weight (expr)
    "insert a line to set the weight of an atom to the value given by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (insert (concat "\n                @weight " (number-to-string (/ n 4.0)) "\n")))))

(defun exo-insert-current-date ()
    "insert the current date, in the format yyyy-mm-dd, into the current buffer"
    (interactive)
    (insert (format-time-string exo-date-format (current-time))))

(defun exo-insert-current-time ()
    "insert the current time, in the format hh:mm, into the current buffer"
    (interactive)
    (insert (format-time-string exo-time-format (current-time))))

(defun exo-insert-current-time-with-seconds ()
    "insert the current time with seconds, in the format hh:mm:ss, into the current buffer"
    (interactive)
    (insert (format-time-string exo-time-with-seconds-format (current-time))))

(defun exo-preview-target-latex-math ()
    "create a graphical preview of the value of the atom at point, which must be a LaTeX mathematical expression"
    (interactive)
    (end-of-line)
    (backward-word)
    (latex-math-preview-expression))

(defun exo-priorities ()
    "retrieve a list of atoms with nonzero priority values, ordered by decreasing priority"
    (interactive)
    (request-priorities-results
        exo-min-sharability exo-max-sharability exo-min-weight exo-max-weight))

(defun exo-push-view ()
    "push an up-to-date view into the Extend-o-Brain graph"
    (interactive)
    (if (in-edit-view)
    (let (
        (entity (buffer-string)))
        ;; The received view may very well differ from the pushed view in terms of line numbering,
        ;; but we'll try to stay on the same line anyway.
        (setq exo-current-line (line-number-at-pos))
        (setq exo-future-sharability exo-default-sharability)
        (http-post
            (concat (base-url) "update")
            (list
                (list "request" (json-encode (list
                    :root exo-root-id
                    :depth (number-to-string exo-depth)
                    :style exo-style
                    :view entity
                    :filter (filter-json exo-min-sharability exo-max-sharability exo-default-sharability exo-min-weight exo-max-weight exo-default-weight)))))
            (receive-view exo-edit-mode)))))

(defun exo-ripple-query (query)
    "evaluate Ripple expression QUERY"
    (interactive)
    (if (> (length query) 0)
        (request-ripple-results
            query
            exo-style
            exo-min-sharability exo-max-sharability exo-min-weight exo-max-weight)))

(defun exo-search (query)
    "evaluate full-text search for QUERY, yielding a ranked list of search results in a new buffer"
    (interactive)
    (if (> (length query) 0)
        (request-search-results
            query
            exo-style
            exo-min-sharability exo-max-sharability exo-min-weight exo-max-weight)))

(defun exo-set-default-sharability (expr)
    "set the default @sharability (for atoms created in update operations) to the number represented by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-default-sharability (/ n 4.0)))))

(defun exo-set-default-weight (expr)
    "set the default @weight (for atoms created in update operations) to the number represented by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-default-weight (/ n 4.0)))))

(defun exo-set-min-sharability (expr)
    "set the minimum @sharability (for atoms visible in the current view) to the number represented by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-min-sharability (/ n 4.0)))))

(defun exo-set-min-weight (expr)
    "set the minimum @weight (for atoms visible in the current view) to the number represented by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-min-weight (/ n 4.0)))))

(defun exo-set-target-priority (expr)
    "set the @priority of the atom at point to the number represented by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-target-priority (/ n 4.0)))))

(defun exo-set-target-sharability (expr)
    "set the @sharability of the atom at point to the number represented by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-target-sharability (/ n 4.0)))))

(defun exo-set-target-weight (expr)
    "set the @weight of the atom at point to the number represented by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-target-weight (/ n 4.0)))))

(defun exo-set-value-truncation-length (length-str)
    "set the value truncation length to the number represented by LENGTH-STR.
Longer values are truncated, for efficiency and readability, when they appear in views.
A value of -1 indicates that values should not be truncated."
    (interactive)
    (let ((n (string-to-number length-str)))
        (setq exo-value-truncation-length n)))

(defun exo-set-view-depth (expr)
    "set the depth of the current view to the number of levels represented by EXPR"
    (interactive)
    (let ((depth (number-shorthand-to-number expr)))
        (if (< depth 1) (error-message (concat "depth of " (number-to-string depth) " is too low (must be >= 1)"))
            (if (> depth 5) (error-message (concat "depth of " (number-to-string depth) " is too high (must be <= 5)"))
                (request-view nil exo-mode exo-root-id depth exo-style exo-min-sharability exo-max-sharability exo-default-sharability exo-min-weight exo-max-weight exo-default-weight)))))

(defun exo-toggle-emacspeak ()
    "turn Emacspeak on or off"
    (interactive)
    (dtk-toggle-quiet))

(defun exo-toggle-inference-viewstyle ()
    "toggle between the sharability view style and the type inference view style.
In the sharability view style, colors are assigned to atoms based on the sharability of each atom
(for example, private atoms are red, while public atoms are green).
However, in the type inference view style, an atom is either cyan or magenta depending on whether
a type has been assigned to it by the inference engine."
    (interactive)
    (setq exo-viewstyle (if (equal exo-viewstyle exo-sharability-viewstyle)
        exo-inference-viewstyle
        exo-sharability-viewstyle))
    (exo-update-view)
    (message (concat "switched to " exo-viewstyle " view style")))

(defun exo-toggle-truncate-lines ()
    "toggle line wrap mode"
    (interactive)
    (toggle-truncate-lines))

(defun exo-update-to-backward-view ()
    "switch to a 'backward' view, i.e. a view in which an atom's parents appear as list items beneath it"
    (interactive)
    (if (in-view)
        (request-view nil exo-mode exo-root-id exo-depth exo-backward-style exo-min-sharability exo-max-sharability exo-default-sharability exo-min-weight exo-max-weight exo-default-weight)))

(defun exo-update-to-forward-view ()
    "switch to a 'forward' view (the default), i.e. a view in which an atom's children appear as list items beneath it"
    (interactive)
    (if (in-view)
        (request-view nil exo-mode exo-root-id exo-depth exo-forward-style exo-min-sharability exo-max-sharability exo-default-sharability exo-min-weight exo-max-weight exo-default-weight)))

(defun exo-update-view ()
    "refresh the current view from the data store"
    (interactive)
    (if (in-view)
        (request-view t exo-mode exo-root-id exo-depth exo-style exo-min-sharability exo-max-sharability exo-default-sharability exo-min-weight exo-max-weight exo-default-weight)))

(defun exo-visit-in-amazon (value-selector)
    "search Amazon.com for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://www.amazon.com/s?ie=UTF8&index=blended&link_code=qs&field-keywords=" (w3m-url-encode-string value)))))

(defun exo-visit-in-delicious (value-selector)
    "search delicious.com for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://www.delicious.com/search?p=" (w3m-url-encode-string value)))))

(defun exo-visit-in-ebay (value-selector)
    "search ebay.com for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://www.ebay.com/sch/i.html?_nkw=" (w3m-url-encode-string value)))))

(defun exo-visit-in-google (value-selector)
    "search google.com for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://www.google.com/search?ie=UTF-8&q=" (w3m-url-encode-string value)))))

(defun exo-visit-in-google-maps (value-selector)
    "search Google Maps for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://maps.google.com/maps?q=" (w3m-url-encode-string value)))))

(defun exo-visit-in-google-scholar (value-selector)
    "search Google Scholar for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://scholar.google.com/scholar?q=" (w3m-url-encode-string value)))))

(defun exo-visit-in-twitter (value-selector)
    "search twitter.com for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://twitter.com/#!/search/" (w3m-url-encode-string value)))))

(defun exo-visit-in-wikipedia (value-selector)
    "search en.wikipedia.org for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&search=" (w3m-url-encode-string value)))))

(defun exo-visit-in-youtube (value-selector)
    "search youtube.com for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://www.youtube.com/results?search_query=" (w3m-url-encode-string value)))))

(defun exo-visit-target ()
    "navigate to the atom at point, opening a new view with that atom as root"
    (interactive)
    (let ((id (atom-id-at-point)))
        (if id
            (request-view nil (mode-for-visit) id exo-depth exo-style exo-min-sharability exo-max-sharability (future-sharability (current-target-sharability)) exo-min-weight exo-max-weight exo-default-weight)
            (no-target))))

(defun exo-visit-target-alias ()
    "visit the @alias of the atom at point (normally a URL) in a browser"
    (interactive)
    (let ((alias (current-target-alias)))
        (if alias
            (browse-url alias)
            (no-target))))

(defun exo-visit-as-url (value-selector)
    "visit the URL generated by VALUE-SELECTOR in a browser"
    (visit-target-value value-selector (lambda (value) value)))

(defun exo-visit-url-at-point ()
    "visit the URL at point in a browser"
    (interactive)
    (goto-address-at-point))


;; KEYBOARD MAPPINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun minibuffer-arg (function prompt)
    (lexical-let ((f function) (p prompt))
        (lambda ()
            (interactive)
            (let ((arg (read-from-minibuffer p)))
                (if arg (funcall f arg))))))

(defun char-arg (function prompt)
    (lexical-let ((f function) (p prompt))
        (lambda ()
            (interactive)
            (let ((c (read-char p)))
                (if c (funcall f c))))))

;; Note: when updating this list of mappings, also update the PKB
(global-set-key (kbd "C-c C-a C-p")     (char-arg 'exo-insert-attr-priority "priority = ?"))
(global-set-key (kbd "C-c C-a C-s")     (char-arg 'exo-insert-attr-sharability "sharability = ?"))
(global-set-key (kbd "C-c C-a C-w")     (char-arg 'exo-insert-attr-weight "weight = ?"))
(global-set-key (kbd "C-c C-a d")       'exo-insert-current-date)
(global-set-key (kbd "C-c C-a s")       'exo-insert-current-time-with-seconds)
(global-set-key (kbd "C-c C-a t")       'exo-insert-current-time)
(global-set-key (kbd "C-c C-d")         (char-arg 'exo-set-view-depth "depth = ?"))
(global-set-key (kbd "C-c C-i f")       'exo-find-isolated-atoms)
(global-set-key (kbd "C-c C-i r")       'exo-remove-isolated-atoms)
(global-set-key (kbd "C-c C-l")         (minibuffer-arg 'exo-goto-line "line: "))
(global-set-key (kbd "C-c C-r C-b a")   (exo-visit-in-amazon 'current-root-value))
(global-set-key (kbd "C-c C-r C-b e")   (exo-visit-in-ebay 'current-root-value))
(global-set-key (kbd "C-c C-r C-b d")   (exo-visit-in-delicious 'current-root-value))
(global-set-key (kbd "C-c C-r C-b g")   (exo-visit-in-google 'current-root-value))
(global-set-key (kbd "C-c C-r C-b m")   (exo-visit-in-google-maps 'current-root-value))
(global-set-key (kbd "C-c C-r C-b s")   (exo-visit-in-google-scholar 'current-root-value))
(global-set-key (kbd "C-c C-r C-b t")   (exo-visit-in-twitter 'current-root-value))
(global-set-key (kbd "C-c C-r C-b w")   (exo-visit-in-wikipedia 'current-root-value))
(global-set-key (kbd "C-c C-r C-b y")   (exo-visit-in-youtube 'current-root-value))
(global-set-key (kbd "C-c C-s C-d")     (char-arg 'exo-set-default-sharability "default sharability = ?"))
(global-set-key (kbd "C-c C-s C-m")     (char-arg 'exo-set-min-sharability "minimum sharability = ?"))
(global-set-key (kbd "C-c C-t C-a b")   'exo-visit-target-alias)
(global-set-key (kbd "C-c C-t C-b a")   (exo-visit-in-amazon 'current-target-value))
(global-set-key (kbd "C-c C-t C-b e")   (exo-visit-in-ebay 'current-target-value))
(global-set-key (kbd "C-c C-t C-b d")   (exo-visit-in-delicious 'current-target-value))
(global-set-key (kbd "C-c C-t C-b g")   (exo-visit-in-google 'current-target-value))
(global-set-key (kbd "C-c C-t C-b m")   (exo-visit-in-google-maps 'current-target-value))
(global-set-key (kbd "C-c C-t C-b s")   (exo-visit-in-google-scholar 'current-target-value))
(global-set-key (kbd "C-c C-t C-b t")   (exo-visit-in-twitter 'current-target-value))
(global-set-key (kbd "C-c C-t C-b w")   (exo-visit-in-wikipedia 'current-target-value))
(global-set-key (kbd "C-c C-t C-b y")   (exo-visit-in-youtube 'current-target-value))
(global-set-key (kbd "C-c C-t C-p")     (char-arg 'exo-set-target-priority "new priority = ?"))
(global-set-key (kbd "C-c C-t C-s")     (char-arg 'exo-set-target-sharability "new sharability = ?"))
(global-set-key (kbd "C-c C-t C-w")     (char-arg 'exo-set-target-weight "new weight = ?"))
;; TODO: finish generalizing these "C-c C-t x" functions to root vs. target
(global-set-key (kbd "C-c C-t a")       (exo-visit-as-url 'current-target-value))
(global-set-key (kbd "C-c C-t c")       'exo-copy-target-value-to-clipboard)
(global-set-key (kbd "C-c C-t i")       (exo-atom-info 'current-target))
(global-set-key (kbd "C-c C-t l")       'exo-preview-target-latex-math)
(global-set-key (kbd "C-c C-t r")       'exo-copy-target-reference-to-clipboard)
;; Note: this should perhaps be a local setting
(global-set-key (kbd "C-c C-v ;")       'exo-toggle-truncate-lines)
(global-set-key (kbd "C-c C-v b")       'exo-update-to-backward-view)
(global-set-key (kbd "C-c C-v e")       'exo-enter-edit-view)
(global-set-key (kbd "C-c C-v f")       'exo-update-to-forward-view)
(global-set-key (kbd "C-c C-v i")       'exo-toggle-inference-viewstyle)
(global-set-key (kbd "C-c C-v r")       'exo-enter-readonly-view)
(global-set-key (kbd "C-c C-v s")       'exo-toggle-emacspeak)
(global-set-key (kbd "C-c C-v t")       (minibuffer-arg 'exo-set-value-truncation-length "value truncation length: "))
(global-set-key (kbd "C-c C-w C-d")     (char-arg 'exo-set-default-weight "default weight = ?"))
(global-set-key (kbd "C-c C-w C-m")     (char-arg 'exo-set-min-weight "minimun weight = ?"))
(global-set-key (kbd "C-c a")           'exo-visit-url-at-point)
(global-set-key (kbd "C-c d")           'exo-duplicates)
(global-set-key (kbd "C-c e")           'exo-export)
(global-set-key (kbd "C-c f")           'exo-find-roots)
(global-set-key (kbd "C-c h")           'exo-history)
(global-set-key (kbd "C-c i")           'exo-infer-types)
(global-set-key (kbd "C-c P")           'exo-priorities)
(global-set-key (kbd "C-c p")           'exo-push-view)
(global-set-key (kbd "C-c r")           (minibuffer-arg 'exo-ripple-query "ripple query: "))
(global-set-key (kbd "C-c s")           (minibuffer-arg 'exo-search "search for: "))
(global-set-key (kbd "C-c t")           'exo-visit-target)
(global-set-key (kbd "C-c u")           'exo-update-view)
(global-set-key (kbd "C-c v")           'exo-events)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(provide 'brain-mode)
