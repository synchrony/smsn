;; Brain-mode: the Extend-o-Brain Emacs UI
;;
;; Required global variables:
;;
;;     extendo-rexster-host: IP of the rexster server
;;     extendo-rexster-port: port of the Rexster server
;;     extendo-rexster-graph: name of Extend-o-Brain graph served by Rexster
;;
;; For example:
;;
;;     (defun brain-mode ()
;;         (defvar extendo-rexster-host "localhost")
;;         (defvar extendo-rexster-port "8182")
;;         (defvar extendo-rexster-graph "joshkb"))

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

(setq fast-numbers '(
    (?0 0) (?1 1) (?2 2) (?3 3) (?4 4) (?5 5) (?6 6) (?7 7) (?8 8) (?9 9)
    (?z 0) (?a 1) (?s 2) (?d 3) (?f 4) (?g 5) (?h 6) (?j 7) (?k 8) (?l 9) (?; 10)))

(defun number-shorthand-to-number (c)
    (interactive)
    (let ((l (assoc c fast-numbers)))
        (if l (car (cdr l)) (error-message (concat "no number associated with character " (char-to-string c))))))

(defun read-character-as-number ()
    (interactive)
    (let ((c (read-char)))
        (number-shorthand-to-number c)))

(defun read-number-shorthand ()
    (interactive)
    (read-char))


;; BUFFERS / VARIABLES ;;;;;;;;;;;;;;;;;

(setq tn-readonly-mode "readonly")
(setq tn-edit-mode "readwrite")
(setq tn-search-mode "search")
;;(setq tn-history-mode "history")
;;(setq tn-event-mode "events")

(setq tn-sharability-viewstyle "sharability")
(setq tn-inference-viewstyle "inference")

(setq tn-forward-view-style "forward")
(setq tn-backward-view-style "backward")

;; Buffer-local variables. Given them initial, global bindings so they're defined before there are actual view buffers.
(setq tn-depth 3)
(setq tn-root-id nil)
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
(setq tn-viewstyle tn-sharability-viewstyle)
(setq tn-value-truncation-length 100)


;; NAVIGATION ;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun tn-goto-line (address)
    (interactive)
    (let ((line (address-to-lineno (handle-changewindow address))))
        (if line
            (goto-line line)
            (error-message "invalid line address"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun using-inference ()
    (equal tn-viewstyle tn-inference-viewstyle))

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
    (let ((v (assoc 'priority atom)))
        (if v (cdr v) nil)))

(defun get-sharability (atom)
    (let ((v (assoc 'sharability atom)))
        (if v (cdr v) tn-default-sharability)))

(defun get-weight (atom)
    (let ((v (assoc 'weight atom)))
        (if v (cdr v) tn-default-weight)))

(defun get-alias (atom)
    (let ((x (assoc 'alias atom)))
        (if x (cdr x) nil)))

(defun get-type (atom)
    (let ((x (assoc 'type atom)))
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

(defun current-root-id ()
    tn-root-id)

(defun current-root-value ()
    tn-title)

(defun current-target ()
    (get-atom (current-root-id)))

(defun current-target-id ()
    (car (last (find-id))))

(defun current-target-value ()
    (let ((g (current-root)))
        (if g
            (get-value g))))

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

(defun tn-atom-info (atom-selector)
    (lexical-let ((as atom-selector))
        (lambda () (interactive)
            (let ((atom (funcall as)))
                (if atom
                    (show-info atom)
                    (no-target))))))


;; COMMUNICATION ;;;;;;;;;;;;;;;;;;;;;;;

(defun base-url ()
    (concat "http://" extendo-rexster-host ":" extendo-rexster-port "/graphs/" extendo-rexster-graph "/extendo/"))

(defun receive-view (mode)
    (lexical-let ((m mode))
        (lambda (status) (receive-view-internal status m))))

(defun numeric-value (json prop default)
    (let ((v (assoc prop json)))
        (if v (string-to-number (cdr v)) default)))

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
                (depth (numeric-value json 'depth nil))

                ;; if the service doesn't specify these values, they will carry over from the previous buffer state
                (min-sharability (numeric-value json 'minSharability tn-min-sharability))
                (max-sharability (numeric-value json 'maxSharability tn-max-sharability))
                (default-sharability (numeric-value json 'defaultSharability tn-default-sharability))
                (min-weight (numeric-value json 'minWeight tn-min-weight))
                (max-weight (numeric-value json 'maxWeight tn-max-weight))
                (default-weight (numeric-value json 'defaultWeight tn-default-weight))

                (style (cdr (assoc 'style json)))
                (title (cdr (assoc 'title json))))
                    (switch-to-buffer (view-name root json))
                    (make-local-variable 'tn-root-id)
                    (make-local-variable 'tn-depth)
                    (make-local-variable 'tn-style)
                    (make-local-variable 'tn-title)
                    (make-local-variable 'tn-min-sharability)
                    (make-local-variable 'tn-max-sharability)
                    (make-local-variable 'tn-default-sharability)
                    (make-local-variable 'tn-min-weight)
                    (make-local-variable 'tn-max-weight)
                    (make-local-variable 'tn-atoms)
                    (make-local-variable 'tn-current-line)
                    (make-local-variable 'tn-mode)
                    (make-local-variable 'tn-value-truncation-length)
                    (setq tn-root-id root)
                    (if (equal mode tn-search-mode)
                        ;; Always leave a search view with depth 1, rather than that of the last view.
                        ;; The user experience is a little unpredictable otherwise.
                        (setq tn-depth 1)
                        (if depth (setq tn-depth depth)))
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
		            (if target-id (puthash target-id json tn-atoms))
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


;; VIEWS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun num-or-nil-to-string (n)
    (if n (number-to-string n) "nil"))

(defun view-info ()
    (concat
        "(root: " tn-root-id
         " :depth " (num-or-nil-to-string tn-depth)
         " :style " tn-style
         " :sharability [" (num-or-nil-to-string tn-min-sharability) ", " (num-or-nil-to-string tn-default-sharability) ", " (num-or-nil-to-string tn-max-sharability) "]"
         " :weight [" (num-or-nil-to-string tn-min-weight) ", " (num-or-nil-to-string tn-default-weight) ", " (num-or-nil-to-string tn-max-weight) "]"
         " :value \"" tn-title "\")"))  ;; TODO: actually escape the title string

(defun request-view (preserve-line mode root depth style mins maxs defaults minw maxw defaultw)
    (setq tn-current-line (if preserve-line (line-number-at-pos) 1))
    (setq tn-future-sharability defaults)
    (http-get (request-view-url root depth style mins maxs defaults minw maxw defaultw) (receive-view mode)))

(defun prop (key value)
    (cons key (cons value nil)))

(defun filter-json (mins maxs defaults minw maxw defaultw)
    (list :minSharability mins :maxSharability maxs :defaultSharability defaults :minWeight minw :maxWeight maxw :defaultWeight defaultw))

(defun request-view-url (root depth style mins maxs defaults minw maxw defaultw)
    (concat (base-url) "view?request=" (w3m-url-encode-string (json-encode
        (list :root root :depth depth :style style :includeTypes (if (using-inference) "true" "false") :filter (filter-json mins maxs defaults minw maxw defaultw))))))

(defun request-history (mins maxs minw maxw)
    (setq tn-current-line 1)
    (setq tn-future-sharability tn-default-sharability)
    (http-get
        (concat (base-url) "history?request=" (w3m-url-encode-string (json-encode
            (list :filter (filter-json mins maxs tn-default-sharability minw maxw tn-default-weight)))))
        (receive-view tn-search-mode)))

(defun request-events (depth)
    (setq tn-current-line 1)
    (setq tn-future-sharability tn-default-sharability)
    (http-get
        (concat (base-url) "get-events?request=" (w3m-url-encode-string (json-encode
            (list :depth depth))))
        (receive-view tn-search-mode)))

(defun request-duplicates (mins maxs minw maxw)
    (setq tn-current-line 1)
    (setq tn-future-sharability tn-default-sharability)
    (http-get
        (concat (base-url) "duplicates?request=" (w3m-url-encode-string (json-encode
            (list :filter (filter-json mins maxs tn-default-sharability minw maxw tn-default-weight)))))
        (receive-view tn-search-mode)))

(defun request-search-results (query style mins maxs minw maxw)
    (setq tn-current-line 1)
    (setq tn-future-sharability tn-default-sharability)
    (http-get
        (concat (base-url) "search?request=" (w3m-url-encode-string (json-encode
            (list :query query :valueCutoff tn-value-truncation-length :depth 1 :style style
                :filter (filter-json mins maxs tn-default-sharability minw maxw tn-default-weight)))))
        (receive-view tn-search-mode)))

(defun request-priorities-results (mins maxs minw maxw)
    (setq tn-current-line 1)
    (setq tn-future-sharability tn-default-sharability)
    (http-get
        (concat (base-url) "priorities?request=" (w3m-url-encode-string (json-encode
            (list :maxResults 100
                :filter (filter-json mins maxs tn-default-sharability minw maxw tn-default-weight)))))
        (receive-view tn-search-mode)))

(defun request-find-roots-results (style mins maxs minw maxw)
    (setq tn-current-line 1)
    (setq tn-future-sharability tn-default-sharability)
    (http-get
        (concat (base-url) "find-roots?request=" (w3m-url-encode-string (json-encode
            (list :style style :depth 1
                :filter (filter-json mins maxs tn-default-sharability minw maxw tn-default-weight)))))
        (receive-view tn-search-mode)))

(defun request-ripple-results (query style mins maxs minw maxw)
    (setq tn-current-line 1)
    (setq tn-future-sharability tn-default-sharability)
    (http-get
        (concat (base-url) "ripple?request=" (w3m-url-encode-string (json-encode
            (list :query query :depth 1 :style style
                :filter (filter-json mins maxs tn-default-sharability minw maxw tn-default-weight)))))
             (receive-view tn-search-mode)))

(defun do-export ()
    (http-get
        (concat (base-url) "export") 'receive-export-results))

(defun do-infer-types ()
    (http-get
        (concat (base-url) "infer-types") 'receive-inference-results))

(defun mode-for-visit ()
    (if (or (equal tn-mode tn-edit-mode) (equal tn-mode tn-readonly-mode))
        tn-mode
        tn-readonly-mode))

(defun tn-visit-target ()
    (interactive)
    (let ((key (current-target-id)))
        (if key
            (request-view nil (mode-for-visit) key tn-depth tn-style tn-min-sharability tn-max-sharability (future-sharability (current-target-sharability)) tn-min-weight tn-max-weight tn-default-weight)
            (no-target))))

(defun tn-history ()
    (interactive)
    (request-history
        tn-min-sharability tn-max-sharability tn-min-weight tn-max-weight))

(defun tn-events ()
    (interactive)
    (request-events 2))

(defun tn-duplicates ()
    (interactive)
    (request-duplicates
        tn-min-sharability tn-max-sharability tn-min-weight tn-max-weight))

(defun tn-search (query)
    (interactive)
    (if (> (length query) 0)
        (request-search-results
            query
            tn-style
            tn-min-sharability tn-max-sharability tn-min-weight tn-max-weight)))

(defun tn-priorities ()
    (interactive)
    (request-priorities-results
        tn-min-sharability tn-max-sharability tn-min-weight tn-max-weight))

(defun tn-find-roots ()
    (interactive)
        (request-find-roots-results
            tn-style
            tn-min-sharability tn-max-sharability tn-min-weight tn-max-weight))

(defun tn-ripple-query (query)
    (interactive)
    (if (> (length query) 0)
        (request-ripple-results
            query
            tn-style
            tn-min-sharability tn-max-sharability tn-min-weight tn-max-weight)))

(defun tn-export ()
    (interactive)
    (message "exporting")
    (do-export))

(defun tn-infer-types ()
    (interactive)
    (message "performing type inference")
    (do-infer-types))

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
    
(defun tn-update-view ()
    (interactive)
    (if (in-view)
        (request-view t tn-mode tn-root-id tn-depth tn-style tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight tn-default-weight)))

(defun tn-enter-edit-view ()
    (interactive)
    (if (and (in-view) (equal tn-mode tn-readonly-mode))
        (request-view t tn-edit-mode tn-root-id tn-depth tn-style tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight tn-default-weight)))

(defun tn-enter-readonly-view ()
    (interactive)
    (if (and (in-view) (equal tn-mode tn-edit-mode))
        (request-view t tn-readonly-mode tn-root-id tn-depth tn-style tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight tn-default-weight)))

(defun tn-choose-depth (expr)
    (interactive)
    (let ((depth (number-shorthand-to-number expr)))
        (if (< depth 1) (error-message (concat "depth of " (number-to-string depth) " is too low (must be >= 1)"))
            (if (> depth 5) (error-message (concat "depth of " (number-to-string depth) " is too high (must be <= 5)"))
                (request-view nil tn-mode tn-root-id depth tn-style tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight tn-default-weight)))))

(defun tn-update-to-forward-view ()
    (interactive)
    (if (in-view)
        (request-view nil tn-mode tn-root-id tn-depth tn-forward-view-style tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight tn-default-weight)))

(defun tn-update-to-backward-view ()
    (interactive)
    (if (in-view)
        (request-view nil tn-mode tn-root-id tn-depth tn-backward-view-style tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight tn-default-weight)))


;; set weight ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-default-weight (s)
    (if (and (in-view) (> s 0) (<= s 1))
        (setq tn-default-weight s)
        (error-message
            (concat "weight " (number-to-string s) " is outside of range (0, 1]"))))

(defun tn-set-default-weight (expr)
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-default-weight (/ n 4.0)))))

(defun set-min-weight (s)
    (if (and (in-view) (>= s 0) (<= s 1))
        (request-view t tn-mode tn-root-id tn-depth tn-style tn-min-sharability tn-max-sharability tn-default-sharability s tn-max-weight tn-default-weight)
        (error-message
            (concat "min weight " (number-to-string s) " is outside of range [0, 1]"))))

(defun tn-set-min-weight (expr)
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-min-weight (/ n 4.0)))))


;; set sharability ;;;;;;;;;;;;;;;;;;;;;

(defun set-default-sharability (s)
    (if (and (in-view) (> s 0) (<= s 1))
        (setq tn-default-sharability s)
        (error-message
            (concat "sharability " (number-to-string s) " is outside of range (0, 1]"))))

(defun tn-set-default-sharability (expr)
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-default-sharability (/ n 4.0)))))

(defun set-min-sharability (s)
    (if (and (in-view) (>= s 0) (<= s 1))
        (request-view t tn-mode tn-root-id tn-depth tn-style s tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight tn-default-weight)
        (error-message
            (concat "min sharability " (number-to-string s) " is outside of range [0, 1]"))))

(defun tn-set-min-sharability (expr)
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-min-sharability (/ n 4.0)))))


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
                (list "request" (json-encode (list
                    :root tn-root-id
                    :depth (number-to-string tn-depth)
                    :style tn-style
                    :view entity
                    :filter (filter-json tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight tn-default-weight)))))
            (receive-view tn-edit-mode)))))

(defun set-property (id name value)
    (interactive)
    (if (in-view)
        (lexical-let (
                (mode tn-mode)
                (url (request-view-url tn-root-id tn-depth tn-style tn-min-sharability tn-max-sharability tn-default-sharability tn-min-weight tn-max-weight tn-default-weight)))
            (setq tn-current-line (line-number-at-pos))
            (setq tn-future-sharability tn-default-sharability)
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

(defun tn-set-target-priority (expr)
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-target-priority (/ n 4.0)))))

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

(defun tn-set-target-sharability (expr)
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-target-sharability (/ n 4.0)))))

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

(defun tn-set-target-weight (expr)
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-target-weight (/ n 4.0)))))


;; INTERFACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun info-message (msg)
    (message (concat "Info: " msg)))

(defun error-message (msg)
    (message (concat "Error: " msg)))

(defun tn-visit-url-at-point ()
    (interactive)
    (goto-address-at-point))  ;; defined in Emacs goto-addr.el

(defun browse-target-value (value-selector value-to-url)
    (lexical-let ((vs value-selector) (vu value-to-url))
        (lambda () (interactive)
            (let ((value (funcall vs)))
                (if value
                    (browse-url (funcall vu value))
                    (no-target))))))

(defun tn-visit-as-url (value-selector)
    (browse-target-value value-selector (lambda (value) value)))

(defun tn-browse-target-alias ()
    (interactive)
    (let ((alias (current-target-alias)))
        (if alias
            (browse-url alias)
            (no-target))))

(defun tn-visit-in-amazon (value-selector)
    (browse-target-value value-selector (lambda (value)
        (concat "http://www.amazon.com/s?ie=UTF8&index=blended&link_code=qs&field-keywords=" (w3m-url-encode-string value)))))

(defun tn-visit-in-ebay (value-selector)
    (browse-target-value value-selector (lambda (value)
        (concat "http://www.ebay.com/sch/i.html?_nkw=" (w3m-url-encode-string value)))))

(defun tn-visit-in-delicious (value-selector)
    (browse-target-value value-selector (lambda (value)
        (concat "http://www.delicious.com/search?p=" (w3m-url-encode-string value)))))

(defun tn-visit-in-google (value-selector)
    (browse-target-value value-selector (lambda (value)
        (concat "http://www.google.com/search?ie=UTF-8&q=" (w3m-url-encode-string value)))))

(defun tn-visit-in-google-scholar (value-selector)
    (browse-target-value value-selector (lambda (value)
        (concat "http://scholar.google.com/scholar?q=" (w3m-url-encode-string value)))))

(defun tn-visit-in-google-maps (value-selector)
    (browse-target-value value-selector (lambda (value)
        (concat "http://maps.google.com/maps?q=" (w3m-url-encode-string value)))))

(defun tn-visit-in-twitter (value-selector)
    (browse-target-value value-selector (lambda (value)
        (concat "http://twitter.com/#!/search/" (w3m-url-encode-string value)))))

(defun tn-visit-in-wikipedia (value-selector)
    (browse-target-value value-selector (lambda (value)
        (concat "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&search=" (w3m-url-encode-string value)))))

(defun tn-visit-in-youtube (value-selector)
    (browse-target-value value-selector (lambda (value)
        (concat "http://www.youtube.com/results?search_query=" (w3m-url-encode-string value)))))

(defvar tn-date-format "%Y-%m-%d")
(defvar tn-time-format "%H:%M")
(defvar tn-time-with-seconds-format "%H:%M:%S")

(defun tn-insert-current-date ()
  "insert the current date into the current buffer."
       (interactive)
       (insert (format-time-string tn-date-format (current-time))))
(defun tn-insert-current-time ()
  "insert the current time into the current buffer."
       (interactive)
       (insert (format-time-string tn-time-format (current-time))))
(defun tn-insert-current-time-with-seconds ()
  "insert the current time into the current buffer."
       (interactive)
       (insert (format-time-string tn-time-with-seconds-format (current-time))))

(defun tn-insert-attr-priority (expr)
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (insert (concat "\n                @priority " (number-to-string (/ n 4.0)) "\n")))))

(defun tn-insert-attr-sharability (expr)
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (insert (concat "\n                @sharability " (number-to-string (/ n 4.0)) "\n")))))

(defun tn-insert-attr-weight (expr)
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (insert (concat "\n                @weight " (number-to-string (/ n 4.0)) "\n")))))

;; note: working in Aquamacs, apparently not in the terminal Emacs 24 on Mac OS X
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
            (copy-to-clipboard (concat "*" (create-id-infix id)))
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

(defun tn-toggle-truncate-lines ()
    (interactive)
    (toggle-truncate-lines))

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


;; KEYBOARD MAPPINGS ;;;;;;;;;;;;;;;;;;;

;; Note: when updating this list of mappings, also update the PKB
(global-set-key (kbd "C-c C-a C-p")     (char-arg 'tn-insert-attr-priority "priority = ?"))
(global-set-key (kbd "C-c C-a C-s")     (char-arg 'tn-insert-attr-sharability "sharability = ?"))
(global-set-key (kbd "C-c C-a C-w")     (char-arg 'tn-insert-attr-weight "weight = ?"))
(global-set-key (kbd "C-c C-a d")       'tn-insert-current-date)
(global-set-key (kbd "C-c C-a s")       'tn-insert-current-time-with-seconds)
(global-set-key (kbd "C-c C-a t")       'tn-insert-current-time)
(global-set-key (kbd "C-c C-d")         (char-arg 'tn-choose-depth "depth = ?"))
(global-set-key (kbd "C-c C-l")         (minibuffer-arg 'tn-goto-line "line: "))
(global-set-key (kbd "C-c C-r C-b a")   (tn-visit-in-amazon 'current-root-value))
(global-set-key (kbd "C-c C-r C-b e")   (tn-visit-in-ebay 'current-root-value))
(global-set-key (kbd "C-c C-r C-b d")   (tn-visit-in-delicious 'current-root-value))
(global-set-key (kbd "C-c C-r C-b g")   (tn-visit-in-google 'current-root-value))
(global-set-key (kbd "C-c C-r C-b m")   (tn-visit-in-google-maps 'current-root-value))
(global-set-key (kbd "C-c C-r C-b s")   (tn-visit-in-google-scholar 'current-root-value))
(global-set-key (kbd "C-c C-r C-b t")   (tn-visit-in-twitter 'current-root-value))
(global-set-key (kbd "C-c C-r C-b w")   (tn-visit-in-wikipedia 'current-root-value))
(global-set-key (kbd "C-c C-r C-b y")   (tn-visit-in-youtube 'current-root-value))
(global-set-key (kbd "C-c C-s C-d")     (char-arg 'tn-set-default-sharability "default sharability = ?"))
(global-set-key (kbd "C-c C-s C-m")     (char-arg 'tn-set-min-sharability "minimum sharability = ?"))
(global-set-key (kbd "C-c C-t C-a b")   'tn-browse-target-alias)
(global-set-key (kbd "C-c C-t C-b a")   (tn-visit-in-amazon 'current-target-value))
(global-set-key (kbd "C-c C-t C-b e")   (tn-visit-in-ebay 'current-target-value))
(global-set-key (kbd "C-c C-t C-b d")   (tn-visit-in-delicious 'current-target-value))
(global-set-key (kbd "C-c C-t C-b g")   (tn-visit-in-google 'current-target-value))
(global-set-key (kbd "C-c C-t C-b m")   (tn-visit-in-google-maps 'current-target-value))
(global-set-key (kbd "C-c C-t C-b s")   (tn-visit-in-google-scholar 'current-target-value))
(global-set-key (kbd "C-c C-t C-b t")   (tn-visit-in-twitter 'current-target-value))
(global-set-key (kbd "C-c C-t C-b w")   (tn-visit-in-wikipedia 'current-target-value))
(global-set-key (kbd "C-c C-t C-b y")   (tn-visit-in-youtube 'current-target-value))
(global-set-key (kbd "C-c C-t C-p")     (char-arg 'tn-set-target-priority "new priority = ?"))
(global-set-key (kbd "C-c C-t C-s")     (char-arg 'tn-set-target-sharability "new sharability = ?"))
(global-set-key (kbd "C-c C-t C-w")     (char-arg 'tn-set-target-weight "new weight = ?"))
;; TODO: finish generalizing these "C-c C-t x" functions to root vs. target
(global-set-key (kbd "C-c C-t a")       (tn-visit-as-url 'current-target-value))
(global-set-key (kbd "C-c C-t c")       'tn-copy-target-value-to-clipboard)
(global-set-key (kbd "C-c C-t i")       (tn-atom-info 'current-target))
(global-set-key (kbd "C-c C-t l")       'tn-preview-target-latex-math)
(global-set-key (kbd "C-c C-t r")       'tn-copy-target-reference-to-clipboard)
;; Note: this should perhaps be a local setting
(global-set-key (kbd "C-c C-v ;")       'tn-toggle-truncate-lines)
(global-set-key (kbd "C-c C-v b")       'tn-update-to-backward-view)
(global-set-key (kbd "C-c C-v e")       'tn-enter-edit-view)
(global-set-key (kbd "C-c C-v f")       'tn-update-to-forward-view)
(global-set-key (kbd "C-c C-v i")       'tn-toggle-inference-viewstyle)
(global-set-key (kbd "C-c C-v r")       'tn-enter-readonly-view)
(global-set-key (kbd "C-c C-v s")       'tn-toggle-emacspeak)
(global-set-key (kbd "C-c C-v t")       (minibuffer-arg 'tn-set-value-truncation-length "value truncation length: "))
(global-set-key (kbd "C-c C-w C-d")     (char-arg 'tn-set-default-weight "default weight = ?"))
(global-set-key (kbd "C-c C-w C-m")     (char-arg 'tn-set-min-weight "minimun weight = ?"))
(global-set-key (kbd "C-c a")           'tn-visit-url-at-point)
(global-set-key (kbd "C-c d")           'tn-duplicates)
(global-set-key (kbd "C-c e")           'tn-export)
(global-set-key (kbd "C-c f")           'tn-find-roots)
(global-set-key (kbd "C-c h")           'tn-history)
(global-set-key (kbd "C-c i")           'tn-infer-types)
(global-set-key (kbd "C-c P")           'tn-priorities)
(global-set-key (kbd "C-c p")           'tn-push-view)
(global-set-key (kbd "C-c r")           (minibuffer-arg 'tn-ripple-query "ripple query: "))
(global-set-key (kbd "C-c s")           (minibuffer-arg 'tn-search "search for: "))
(global-set-key (kbd "C-c t")           'tn-visit-target)
(global-set-key (kbd "C-c u")           'tn-update-view)
(global-set-key (kbd "C-c v")           'tn-events)


;; WRAPPERS FOR EMACSCLIENT ;;;;;;;;;;;;

(defun do-in-current-buffer (f)
    (set-buffer (window-buffer (selected-window)))
    (funcall f))

(defun tn-arrow-up ()
    (do-in-current-buffer 'previous-line))

(defun tn-arrow-down ()
    (do-in-current-buffer 'next-line))

(defun tn-arrow-left ()
    (do-in-current-buffer 'left-char))

(defun tn-arrow-right ()
    (do-in-current-buffer 'right-char))

(defun tn-insert (s)
    (do-in-current-buffer (lambda () (insert s))))


;; OTHER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tn-toggle-emacspeak ()
    (interactive)
    (dtk-toggle-quiet))

(defun tn-toggle-inference-viewstyle ()
    (interactive)
    (setq tn-viewstyle (if (equal tn-viewstyle tn-sharability-viewstyle)
        tn-inference-viewstyle
        tn-sharability-viewstyle))
    (tn-update-view)
    (message (concat "switched to " tn-viewstyle " view style")))

(defun tn-set-value-truncation-length (length-str)
    (interactive)
    (let ((n (string-to-number length-str)))
        (setq tn-value-truncation-length n)))


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
