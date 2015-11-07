;; Brain-mode: the Extend-o-Brain Emacs library
;;
;; Required global variables:
;;
;;     smsn-rexster-url: IP, port, and local path to the rexster server
;;     smsn-rexster-graph: name of Extend-o-Brain graph served by Rexster
;;
;; Optional global variables:
;;
;;     smsn-default-graphml-file: file to which GraphML dumps will be exported by default
;;     smsn-default-vertices-file: file to which tab-separated vertex dumps will be exported by default
;;     smsn-default-edge-file: file to which tab-separated edge dumps will be exported by default
;;     smsn-default-pagerank-file: file to which PageRank results will be exported by default
;;
;; For example:
;;
;;     (defvar smsn-rexster-url "http://localhost:8182")
;;     (defvar smsn-rexster-graph "joshkb")
;;     (defvar smsn-default-graphml-file "/tmp/joshkb-graphml.xml")
;;     (defvar smsn-default-vertices-file "/tmp/joshkb-vertices.tsv")
;;     (defvar smsn-default-edges-file "/tmp/joshkb-edges.tsv")
;;     (defvar smsn-default-pagerank-file "/tmp/joshkb-pagerank.tsv")


;; DEPENDENCIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for JSON-formatted messages to and from Semantic Synchrony services (see json-read-from-string, json-encode)

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


;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq smsn-max-height 7)


;; VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq smsn-readonly-mode "readonly")
(setq smsn-edit-mode "readwrite")
(setq smsn-search-mode "search")
(setq smsn-sharability-viewstyle "sharability")
(setq smsn-inference-viewstyle "inference")

(setq smsn-forward-style "forward")
(setq smsn-backward-style "backward")

;; Buffer-local variables. Given them initial, global bindings so they're defined before there are actual view buffers.
(setq smsn-height 3)
(setq smsn-root-id nil)
(setq smsn-title nil)
(setq smsn-style smsn-forward-style)
;; "private" atoms are hidden to begin with
(setq smsn-min-sharability 0.25)
(setq smsn-max-sharability 1)
;; default to "average" sharability to begin with
(setq smsn-default-sharability 0.5)
(setq smsn-future-sharability smsn-default-sharability)
;; atoms of all weights are visible to begin with
(setq smsn-min-weight 0.0)
(setq smsn-max-weight 1.0)
;; default to "average" weight to begin with
(setq smsn-default-weight 0.5)
(setq smsn-atoms nil)
(setq smsn-current-line 1)
(setq smsn-mode nil)  ;; Note: 'view-mode' is used by Emacs.
(setq smsn-viewstyle smsn-sharability-viewstyle)
(setq smsn-view-properties nil)
(setq smsn-value-truncation-length 100)
(setq smsn-minimize-verbatim-blocks nil)


;; DATA MODEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar smsn-date-format "%Y-%m-%d")
(defvar smsn-time-format "%H:%M")
(defvar smsn-time-with-seconds-format "%H:%M:%S")

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
        (if v (cdr v) smsn-default-sharability)))

(defun get-weight (atom)
    (let ((v (assoc 'weight atom)))
        (if v (cdr v) smsn-default-weight)))

(defun get-alias (atom)
    (let ((x (assoc 'alias atom)))
        (if x (cdr x) nil)))

(defun get-shortcut (atom)
    (let ((x (assoc 'shortcut atom)))
        (if x (cdr x) nil)))

(defun get-type (atom)
    (let ((x (assoc 'type atom)))
        (if x (cdr x) nil)))

(defun get-meta (atom)
    (let ((x (assoc 'meta atom)))
        (if x (cdr x) nil)))

(defun current-root-id ()
    smsn-root-id)

(defun current-root ()
    (get-atom (current-root-id)))

;;(defun current-root-value ()
;;    smsn-title)

(defun current-root-value ()
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
        (if smsn-atoms
            (gethash id smsn-atoms)
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
        (meta (get-meta atom)))
        ;;(type (get-type atom)))
            (message (concat
                 ;;(if type (concat "type: " type ", "))
                 (if meta (concat "[meta], "))
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
    (?z 0) (?a 1) (?s 2) (?d 3) (?f 4) (?g 5) (?h 6) (?j 7) (?k 8) (?l 9) (?\; 10)))

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
    '(?\; ?0) '(?a ?1) '(?s ?2) '(?d ?3) '(?f ?4) '(?g ?5) '(?h ?6) '(?j ?7) '(?k ?8) '(?l ?9)
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
    (concat smsn-rexster-url "/graphs/" smsn-rexster-graph "/smsn/"))

(defun receive-view (mode)
    (lexical-let ((m mode))
        (lambda (status) (receive-view-internal status m))))

(defun show-http-response-status (status json)
    (let ((msg (cdr (assoc 'message json)))
        (error (cdr (assoc 'error json))))
            (if error
                (error-message error)
                (error-message msg))))

(defun acknowledge-http-response (status success-message)
    (let ((json (json-read-from-string (strip-http-headers (buffer-string)))))
        (if status
            (show-http-response-status status json)
            (info-message success-message))))

(defun receive-view-internal (status mode)
    (let ((json (json-read-from-string (strip-http-headers (buffer-string))))
          (editable (equal mode smsn-edit-mode)))
        (if status
            (show-http-response-status status json)
            (let (
                (root (cdr (assoc 'root json)))
                (view (cdr (assoc 'view json)))
                (height (numeric-value json 'height nil))

                ;; if the service doesn't specify these values, they will carry over from the previous buffer state
                (min-sharability (numeric-value json 'minSharability smsn-min-sharability))
                (max-sharability (numeric-value json 'maxSharability smsn-max-sharability))
                (default-sharability (numeric-value json 'defaultSharability smsn-default-sharability))
                (min-weight (numeric-value json 'minWeight smsn-min-weight))
                (max-weight (numeric-value json 'maxWeight smsn-max-weight))
                (default-weight (numeric-value json 'defaultWeight smsn-default-weight))

                (style (cdr (assoc 'style json)))
                (title (cdr (assoc 'title json))))
                    (switch-to-buffer (view-name root json))
                    (make-local-variable 'smsn-root-id)
                    (make-local-variable 'smsn-height)
                    (make-local-variable 'smsn-style)
                    (make-local-variable 'smsn-title)
                    (make-local-variable 'smsn-min-sharability)
                    (make-local-variable 'smsn-max-sharability)
                    (make-local-variable 'smsn-default-sharability)
                    (make-local-variable 'smsn-min-weight)
                    (make-local-variable 'smsn-max-weight)
                    (make-local-variable 'smsn-atoms)
                    (make-local-variable 'smsn-current-line)
                    (make-local-variable 'smsn-mode)
                    (make-local-variable 'smsn-value-truncation-length)
                    (make-local-variable 'smsn-minimize-verbatim-blocks)
                    (setq smsn-root-id root)
                    (if (equal mode smsn-search-mode)
                        ;; Always leave a search view with height 1, rather than that of the last view.
                        ;; The user experience is a little unpredictable otherwise.
                        (setq smsn-height 1)
                        (if height (setq smsn-height height)))
                    (setq smsn-min-sharability min-sharability)
                    (setq smsn-max-sharability max-sharability)
                    (setq smsn-default-sharability smsn-future-sharability)
                    (setq smsn-min-weight min-weight)
                    (setq smsn-max-weight max-weight)
                    (setq smsn-default-weight default-weight)
                    (setq smsn-style (if style style smsn-style))
                    (setq smsn-title title)
                    (setq smsn-atoms (make-hash-table :test 'equal))
                    (setq smsn-mode mode)
                    (setq buffer-read-only nil)
                    (erase-buffer)
                    (write-view editable (cdr (assoc 'children view)) 0)
                    (beginning-of-buffer)
                    (setq visible-cursor t)
                    ;; Try to move to the corresponding line in the previous view.
                    ;; This is not always possible and not always helpful, but it is often both.
                    (beginning-of-line smsn-current-line)
                    (setq buffer-read-only (not editable))
                    ;; always include line numbers in views
                    (linum-mode t)
                    (info-message (concat "updated to view " (view-info)))))))

(defun receive-export-results (status)
    (acknowledge-http-response status "exported successfully"))

(defun receive-import-results (status)
    (acknowledge-http-response status "imported successfully"))

(defun receive-inference-results (status)
    (acknowledge-http-response status "type inference completed successfully"))

(defun request-view (preserve-line mode root height style mins maxs defaults minw maxw defaultw)
    (setq smsn-current-line (if preserve-line (line-number-at-pos) 1))
    (setq smsn-future-sharability defaults)
    (http-get (request-view-url root height style mins maxs defaults minw maxw defaultw) (receive-view mode)))

(defun filter-json (mins maxs defaults minw maxw defaultw)
    (list :minSharability mins :maxSharability maxs :defaultSharability defaults
          :minWeight minw :maxWeight maxw :defaultWeight defaultw))

(defun request-view-url (root height style mins maxs defaults minw maxw defaultw)
    (concat (base-url) "view?request=" (w3m-url-encode-string (json-encode
        (list :root root :height height :style style :includeTypes (if (using-inference) "true" "false")
              :filter (filter-json mins maxs defaults minw maxw defaultw))))))

(defun request-history (mins maxs minw maxw)
    (setq smsn-current-line 1)
    (setq smsn-future-sharability smsn-default-sharability)
    (http-get
        (concat (base-url) "history?request=" (w3m-url-encode-string (json-encode
            (list :filter (filter-json mins maxs smsn-default-sharability minw maxw smsn-default-weight)))))
        (receive-view smsn-search-mode)))

(defun request-events (height)
    (setq smsn-current-line 1)
    (setq smsn-future-sharability smsn-default-sharability)
    (http-get
        (concat (base-url) "get-events?request=" (w3m-url-encode-string (json-encode
            (list :height height))))
        (receive-view smsn-search-mode)))

(defun request-duplicates (mins maxs minw maxw)
    (setq smsn-current-line 1)
    (setq smsn-future-sharability smsn-default-sharability)
    (http-get
        (concat (base-url) "duplicates?request=" (w3m-url-encode-string (json-encode
            (list :filter (filter-json mins maxs smsn-default-sharability minw maxw smsn-default-weight)))))
        (receive-view smsn-search-mode)))

(defun request-query-results (query query-type style mins maxs minw maxw)
    (setq smsn-current-line 1)
    (setq smsn-future-sharability smsn-default-sharability)
    (http-get
        (concat (base-url) "search?request=" (w3m-url-encode-string (json-encode
            (list :queryType query-type :query query :valueCutoff smsn-value-truncation-length :height 1 :style style
                :filter (filter-json mins maxs smsn-default-sharability minw maxw smsn-default-weight)))))
        (receive-view smsn-search-mode)))

(defun request-priorities-results (mins maxs minw maxw)
    (setq smsn-current-line 1)
    (setq smsn-future-sharability smsn-default-sharability)
    (http-get
        (concat (base-url) "priorities?request=" (w3m-url-encode-string (json-encode
            (list :maxResults 100
                :filter (filter-json mins maxs smsn-default-sharability minw maxw smsn-default-weight)))))
        (receive-view smsn-search-mode)))

(defun request-find-isolated-atoms-results (mins maxs minw maxw)
    (setq smsn-current-line 1)
    (setq smsn-future-sharability smsn-default-sharability)
    (http-get
        (concat (base-url) "find-isolated-atoms?request=" (w3m-url-encode-string (json-encode
            (list
                :filter (filter-json mins maxs smsn-default-sharability minw maxw smsn-default-weight)))))
        (receive-view smsn-search-mode)))

(defun request-find-roots-results (style mins maxs minw maxw)
    (setq smsn-current-line 1)
    (setq smsn-future-sharability smsn-default-sharability)
    (http-get
        (concat (base-url) "find-roots?request=" (w3m-url-encode-string (json-encode
            (list :style style :height 1
                :filter (filter-json mins maxs smsn-default-sharability minw maxw smsn-default-weight)))))
        (receive-view smsn-search-mode)))

(defun request-remove-isolated-atoms (mins maxs minw maxw)
    (setq smsn-current-line 1)
    (setq smsn-future-sharability smsn-default-sharability)
    (http-get
        (concat (base-url) "remove-isolated-atoms?request=" (w3m-url-encode-string (json-encode
            (list
                :filter (filter-json mins maxs smsn-default-sharability minw maxw smsn-default-weight)))))
        (lambda (status)
            (interactive)
            (if status
                (acknowledge-http-response status "removed isolated atoms")))))

(defun request-ripple-results (query style mins maxs minw maxw)
    (setq smsn-current-line 1)
    (setq smsn-future-sharability smsn-default-sharability)
    (http-get
        (concat (base-url) "ripple?request=" (w3m-url-encode-string (json-encode
            (list :query query :height 1 :style style
                :filter (filter-json mins maxs smsn-default-sharability minw maxw smsn-default-weight)))))
             (receive-view smsn-search-mode)))

(defun do-export (format file mins maxs minw maxw)
    (http-get
        (concat (base-url) "export?request=" (w3m-url-encode-string (json-encode
            (list :root smsn-root-id :height smsn-height :format format :file file
                :filter (filter-json mins maxs smsn-default-sharability minw maxw smsn-default-weight)))))
         'receive-export-results))

(defun do-import (format file)
    (http-get
        (concat (base-url) "import?request=" (w3m-url-encode-string (json-encode
            (list :format format :file file))))
         'receive-import-results))

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
    (equal smsn-viewstyle smsn-inference-viewstyle))

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

(defun find-color (weight sharability bright has-meta)
    (let ((s
        (if (using-inference)
            (elt (if bright inference-bright-colors inference-base-colors) (if has-meta 0 1))
            (elt (if bright sharability-bright-colors sharability-base-colors) (- (ceiling (* sharability 4)) 1)))))
        (color-string
            (fade-color (color-part-red s) weight)
            (fade-color (color-part-green s) weight)
            (fade-color (color-part-blue s) weight))))

(setq full-colors-supported (> (length (defined-colors)) 8))

(defun colorize (text weight sharability priority-bg priority-fg bright has-meta)
    (let ((color (if full-colors-supported
            (find-color weight sharability bright has-meta)
            (elt sharability-reduced-colors (- (ceiling (* sharability 4)) 1)))))
        (setq l (list
            :foreground color
            ;;:weight 'bold
            :underline (if (and priority-fg (> priority-fg 0))
                (list :color (find-color priority-fg sharability bright has-meta)) nil)
            :box (if priority-bg (list
                :color (find-color priority-bg sharability bright has-meta)) nil)))
        (propertize text 'face l)))

(defun black (text)
    (propertize text 'face (list :foreground "black")))

(defun light-gray (text)
    (propertize text
	    'face (if full-colors-supported
		    (list :foreground "grey80" :background "white")
			(list :foreground "black"))))

(defun dark-gray (text background)
    (propertize text
	    'face (if full-colors-supported
		    (list :foreground "grey50" :background background)
			(list :foreground "black"))))

(defun create-id-infix (id)
    (propertize (concat " :" id ":") 'invisible t))

(defun delimit-value (value)
    (let ((s (string-match "\n" value)))
        (if s (let ((content (concat "\n" value "\n")))
            (concat "{{{"
                (if smsn-minimize-verbatim-blocks (propertize content 'invisible t) content)
            "}}}")) value)))

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
		        (target-priority (get-priority json))
                (target-has-children (not (equal json-false (cdr (assoc 'hasChildren json)))))
		        (target-alias (get-alias json))
		        (target-shortcut (get-shortcut json))
		        (target-meta (get-meta json)))
		            (if target-id (puthash target-id json smsn-atoms))
		            (if (not target-id) (error "missing target id"))
		            (setq space "")
		            (loop for i from 1 to tree-indent do (setq space (concat space " ")))
		            (let ((line "") (id-infix (create-id-infix target-id)))
		                (if (not editable)
                            (setq id-infix (propertize id-infix 'invisible t)))
                        (setq line (concat line space))
                        (let ((bullet (if target-has-children "+" "\u00b7")))   ;; previously: "-" or "\u25ba"
                            (setq line (concat line
                                (colorize bullet target-weight target-sharability target-priority nil target-alias target-meta)
                                id-infix
                                " "
                                (colorize (delimit-value target-value)
                                          target-weight target-sharability nil target-priority target-alias target-meta)
                                 "\n")))
                        (insert (propertize line 'target-id target-id)))
                    (if (using-inference)
                        (loop for a across target-meta do (insert (light-gray (concat space "    @{" a "}\n")))))
                    (if smsn-view-properties (let ()
                        (insert (light-gray (concat space "    @sharability " (number-to-string target-sharability) "\n")))
                        (insert (light-gray (concat space "    @weight      " (number-to-string target-weight) "\n")))
                        (if target-shortcut
                            (insert (light-gray (concat space "    @shortcut    " target-shortcut "\n"))))
                        (if target-alias
                            (insert (light-gray (concat space "    @alias       " target-alias "\n"))))))
                    (write-view editable children (+ tree-indent 4))))))

(defun num-or-nil-to-string (n)
    (if n (number-to-string n) "nil"))

(defun view-info ()
    (concat
        "(root: " smsn-root-id
         " :height " (num-or-nil-to-string smsn-height)
         " :style " smsn-style
         " :sharability [" (num-or-nil-to-string smsn-min-sharability) ", " (num-or-nil-to-string smsn-default-sharability) ", " (num-or-nil-to-string smsn-max-sharability) "]"
         " :weight [" (num-or-nil-to-string smsn-min-weight) ", " (num-or-nil-to-string smsn-default-weight) ", " (num-or-nil-to-string smsn-max-weight) "]"
         " :value \"" smsn-title "\")"))  ;; TODO: actually escape the title string

(defun mode-for-visit ()
    (if (or (equal smsn-mode smsn-edit-mode) (equal smsn-mode smsn-readonly-mode))
        smsn-mode
        smsn-readonly-mode))

(defun current-view-mode-is-atom-view ()
    (or
        (equal smsn-mode smsn-readonly-mode)
        (equal smsn-mode smsn-edit-mode)))

(defun in-view ()
    (if (or
;;            (equal smsn-mode smsn-search-mode)
            (equal smsn-mode smsn-readonly-mode)
            (equal smsn-mode smsn-edit-mode))
        t
	    (and (error-message "this command can only be executed from within an atom view") nil)))

(defun in-edit-view ()
    (if (equal smsn-mode smsn-edit-mode)
        t
	    (and (error-message "this command can only be executed from within an edit view") nil)))


;; UPDATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-default-weight (s)
    (if (and (in-view) (> s 0) (<= s 1))
        (setq smsn-default-weight s)
        (error-message
            (concat "weight " (number-to-string s) " is outside of range (0, 1]"))))

(defun set-min-weight (s)
    (if (and (in-view) (>= s 0) (<= s 1))
        (request-view t smsn-mode smsn-root-id smsn-height smsn-style smsn-min-sharability smsn-max-sharability smsn-default-sharability s smsn-max-weight smsn-default-weight)
        (error-message
            (concat "min weight " (number-to-string s) " is outside of range [0, 1]"))))

(defun set-default-sharability (s)
    (if (and (in-view) (> s 0) (<= s 1))
        (setq smsn-default-sharability s)
        (error-message
            (concat "sharability " (number-to-string s) " is outside of range (0, 1]"))))

(defun set-min-sharability (s)
    (if (and (in-view) (>= s 0) (<= s 1))
        (request-view t smsn-mode smsn-root-id smsn-height smsn-style s smsn-max-sharability smsn-default-sharability smsn-min-weight smsn-max-weight smsn-default-weight)
        (error-message
            (concat "min sharability " (number-to-string s) " is outside of range [0, 1]"))))

(defun set-property (id name value)
    (interactive)
    (if (in-view)
        (lexical-let (
                (mode smsn-mode)
                (url (request-view-url smsn-root-id smsn-height smsn-style smsn-min-sharability smsn-max-sharability smsn-default-sharability smsn-min-weight smsn-max-weight smsn-default-weight)))
            (setq smsn-current-line (line-number-at-pos))
            (setq smsn-future-sharability smsn-default-sharability)
            (http-get
                (concat (base-url) "set?request=" (w3m-url-encode-string (json-encode
                    (list :id id :name name :value value))))
	(lambda (status)
        (let ((json (json-read-from-string (strip-http-headers (buffer-string)))))
            (if status
                (show-http-response-status status json)
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


;; USER API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smsn-atom-info (selector)
    "display, in the minibuffer, information about an atom produced by SELECTOR"
    (lexical-let ((as selector))
        (lambda () (interactive)
            (let ((atom (funcall as)))
                (if atom
                    (show-info atom)
                    (no-target))))))

;; note: for some reason, the id doesn't stay invisible when you paste it, although it stays light gray
(defun smsn-copy-target-reference-to-clipboard ()
    "copy a reference to the atom at point to the system clipboard"
    (interactive)
    (let ((id (atom-id-at-point)))
        (if id
            (copy-to-clipboard (concat "*" (create-id-infix id)))
            (no-target))))

(defun smsn-copy-target-value-to-clipboard ()
    "copy the value of the atom at point to the system clipboard"
    (interactive)
    (let ((value (current-target-value)))
        (if value
            (copy-to-clipboard value)
            (no-target))))

(defun smsn-duplicates ()
    "retrieve a list of atoms with duplicate values"
    (interactive)
    (request-duplicates
        smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight))

(defun smsn-enter-edit-view ()
    "enter edit (read/write) mode in the current view"
    (interactive)
    (if (and (in-view) (equal smsn-mode smsn-readonly-mode))
        (request-view t smsn-edit-mode smsn-root-id smsn-height smsn-style
            smsn-min-sharability smsn-max-sharability smsn-default-sharability
            smsn-min-weight smsn-max-weight smsn-default-weight)))

(defun smsn-enter-readonly-view ()
    "enter read-only mode in the current view"
    (interactive)
    (if (and (in-view) (equal smsn-mode smsn-edit-mode))
        (request-view t smsn-readonly-mode smsn-root-id smsn-height smsn-style
            smsn-min-sharability smsn-max-sharability smsn-default-sharability
            smsn-min-weight smsn-max-weight smsn-default-weight)))

(defun smsn-events ()
    "retrieve the Extend-o-Brain event stack (e.g. notifications of gestural events), ordered by decreasing time stamp"
    (interactive)
    (request-events 2))

(defun smsn-export-edges (file)
    "export tab-separated dump of Extend-o-Brain parent-child edges to the file system"
    (interactive)
    (message (concat "exporting edges to " file))
    (do-export "Edges" file
        smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight))

(defun smsn-export-graphml (file)
    "export a GraphML dump of the knowledge base to the file system"
    (interactive)
    (message (concat "exporting GraphML to " file))
    (do-export "GraphML" file
        smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight))

(defun smsn-export-latex (file)
    "export a LaTeX-formatted view of a subtree of the knowledge base to the file system"
    (interactive)
    (message (concat "exporting LaTeX to " file))
    (do-export "LaTeX" file
        smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight))

(defun smsn-export-pagerank (file)
    "export a tab-separated PageRank ranking of Extend-o-Brain atoms to the file system"
    (interactive)
    (message (concat "computing and exporting PageRank to " file))
    (do-export "PageRank" file
        smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight))

(defun smsn-export-rdf (file)
    "export a complete RDF dump of the knowledge base (including personal and private data) to the file system"
    (interactive)
    (message (concat "exporting private RDF dump to " file))
    (do-export "RDF" file
        smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight))

(defun smsn-export-webrdf (file)
    "export a Web-friendly dump of the public portion of the knowledge base to the file system"
    (interactive)
    (message (concat "exporting public Web RDF dump to " file))
    (do-export "Web" file
        smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight))

(defun smsn-export-vertices (file)
    "export tab-separated dump of Extend-o-Brain vertices (atoms) to the file system"
    (interactive)
    (message (concat "exporting vertices to " file))
    (do-export "Vertices" file
        smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight))

(defun smsn-import-graphml (file)
    "import a GraphML dump from the file system into the knowledge base"
    (interactive)
    (message (concat "importing GraphML from " file))
    (do-import "GraphML" file))

(defun smsn-find-isolated-atoms ()
    "retrieve a list of isolated atoms (i.e. atoms with neither parents nor children) in the knowledge base"
    (interactive)
        (request-find-isolated-atoms-results
            smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight))

(defun smsn-remove-isolated-atoms ()
    "remove all isolated atoms (i.e. atoms with neither parents nor children) from the knowledge base"
    (interactive)
        (request-remove-isolated-atoms
            smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight))

(defun smsn-find-roots ()
    "retrieve a list of roots (i.e. atoms with no parents) in the knowledge base"
    (interactive)
        (request-find-roots-results
            smsn-style
            smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight))

(defun smsn-goto-line (address)
    "move point to the line represented by ADDRESS"
    (interactive)
    (let ((line (address-to-lineno (handle-changewindow address))))
        (if line
            (goto-line line)
            (error-message "invalid line address"))))

(defun smsn-history ()
    "retrieve a list of the most recently viewed or updated atoms, in decreasing order of recency"
    (interactive)
    (request-history
        smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight))

(defun smsn-infer-types ()
    "perform type inference on the Extend-o-Brain knowledge base, adding type annotations"
    (interactive)
    (message "performing type inference")
    (do-infer-types))

(defun smsn-insert-attr-priority (expr)
    "insert a line to set the priority of an atom to the value given by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (insert (concat "\n                @priority " (number-to-string (/ n 4.0)) "\n")))))

(defun smsn-insert-attr-sharability (expr)
    "insert a line to set the sharability of an atom to the value given by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (insert (concat "\n                @sharability " (number-to-string (/ n 4.0)) "\n")))))

(defun smsn-insert-attr-weight (expr)
    "insert a line to set the weight of an atom to the value given by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (insert (concat "\n                @weight " (number-to-string (/ n 4.0)) "\n")))))

(defun smsn-insert-current-date ()
    "insert the current date, in the format yyyy-mm-dd, into the current buffer"
    (interactive)
    (insert (format-time-string smsn-date-format (current-time))))

(defun smsn-insert-current-time ()
    "insert the current time, in the format hh:mm, into the current buffer"
    (interactive)
    (insert (format-time-string smsn-time-format (current-time))))

(defun smsn-insert-current-time-with-seconds ()
    "insert the current time with seconds, in the format hh:mm:ss, into the current buffer"
    (interactive)
    (insert (format-time-string smsn-time-with-seconds-format (current-time))))

(defun smsn-preview-target-latex-math ()
    "create a graphical preview of the value of the atom at point, which must be a LaTeX mathematical expression"
    (interactive)
    (end-of-line)
    (backward-word)
    (latex-math-preview-expression))

(defun smsn-priorities ()
    "retrieve a list of atoms with nonzero priority values, ordered by decreasing priority"
    (interactive)
    (request-priorities-results
        smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight))

(defun smsn-push-view ()
    "push an up-to-date view into the knowledge base"
    (interactive)
    (if (in-edit-view)
    (let (
        (entity (buffer-string)))
        ;; The received view may very well differ from the pushed view in terms of line numbering,
        ;; but we'll try to stay on the same line anyway.
        (setq smsn-current-line (line-number-at-pos))
        (setq smsn-future-sharability smsn-default-sharability)
        (http-post
            (concat (base-url) "update")
            (list
                (list "request" (json-encode (list
                    :root smsn-root-id
                    :height (number-to-string smsn-height)
                    :style smsn-style
                    :view entity
                    :filter (filter-json smsn-min-sharability smsn-max-sharability smsn-default-sharability smsn-min-weight smsn-max-weight smsn-default-weight)))))
            (receive-view smsn-edit-mode))))
    (sit-for 0 500)(smsn-update-view)(sit-for 0 500)(smsn-update-view)) ;; TODO: this is a hack to get around the 405 issue on the server

(defun smsn-ripple-query (query)
    "evaluate Ripple expression QUERY"
    (interactive)
    (if (> (length query) 0)
        (request-ripple-results
            query
            smsn-style
            smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight)))

(defun smsn-fulltext-query (query)
    "evaluate full-text query for QUERY, yielding a ranked list of query results in a new buffer"
    (interactive)
    (if (> (length query) 0)
        (request-query-results
            query
            "FullText"
            smsn-style
            smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight)))

(defun smsn-acronym-query (query)
    "evaluate acronym (abbreviated fulltext) query for QUERY, yielding a ranked list of query results in a new buffer"
    (interactive)
    (if (> (length query) 0)
        (request-query-results
            query
            "Acronym"
            smsn-style
            smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight)))

(defun smsn-shortcut-query (query)
    "evaluate shortcut query for QUERY, yielding query results (normally zero or one) in a new buffer"
    (interactive)
    (if (> (length query) 0)
        (request-query-results
            query
            "Shortcut"
            smsn-style
            smsn-min-sharability smsn-max-sharability smsn-min-weight smsn-max-weight)))

(defun smsn-set-default-sharability (expr)
    "set the default @sharability (for atoms created in update operations) to the number represented by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-default-sharability (/ n 4.0)))))

(defun smsn-set-default-weight (expr)
    "set the default @weight (for atoms created in update operations) to the number represented by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-default-weight (/ n 4.0)))))

(defun smsn-set-min-sharability (expr)
    "set the minimum @sharability (for atoms visible in the current view) to the number represented by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-min-sharability (/ n 4.0)))))

(defun smsn-set-min-weight (expr)
    "set the minimum @weight (for atoms visible in the current view) to the number represented by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-min-weight (/ n 4.0)))))

(defun smsn-set-target-priority (expr)
    "set the @priority of the atom at point to the number represented by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-target-priority (/ n 4.0)))))

(defun smsn-set-target-sharability (expr)
    "set the @sharability of the atom at point to the number represented by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-target-sharability (/ n 4.0)))))

(defun smsn-set-target-weight (expr)
    "set the @weight of the atom at point to the number represented by EXPR"
    (interactive)
    (let ((n (number-shorthand-to-number expr)))
        (if n (set-target-weight (/ n 4.0)))))

(defun smsn-set-value-truncation-length (length-str)
    "set the value truncation length to the number represented by LENGTH-STR.
Longer values are truncated, for efficiency and readability, when they appear in views.
A value of -1 indicates that values should not be truncated."
    (interactive)
    (let ((n (string-to-number length-str)))
        (setq smsn-value-truncation-length n)))

(defun smsn-set-view-height (expr)
    "set the height of the current view to the number of levels represented by EXPR"
    (interactive)
    (let ((height (number-shorthand-to-number expr)))
        (if (< height 1) (error-message (concat "height of " (number-to-string height) " is too low (must be >= 1)"))
            (if (> height smsn-max-height)
                (error-message (concat "height of " (number-to-string height) " is too high (must be <= "
                    (number-to-string smsn-max-height) ")"))
                (request-view nil smsn-mode smsn-root-id height smsn-style
                    smsn-min-sharability smsn-max-sharability smsn-default-sharability
                    smsn-min-weight smsn-max-weight smsn-default-weight)))))

(defun smsn-toggle-emacspeak ()
    "turn Emacspeak on or off"
    (interactive)
    (dtk-toggle-quiet))

(defun smsn-toggle-inference-viewstyle ()
    "toggle between the sharability view style and the type inference view style.
In the sharability view style, colors are assigned to atoms based on the sharability of each atom
(for example, private atoms are red, while public atoms are green).
However, in the type inference view style, an atom is either cyan or magenta depending on whether
a type has been assigned to it by the inference engine."
    (interactive)
    (setq smsn-viewstyle (if (equal smsn-viewstyle smsn-sharability-viewstyle)
        smsn-inference-viewstyle
        smsn-sharability-viewstyle))
    (smsn-update-view)
    (message (concat "switched to " smsn-viewstyle " view style")))

(defun smsn-toggle-minimize-verbatim-blocks ()
    "enable or disable the hiding of the contents of {{{verbatim blocks}}}, which may span multiple lines"
    (interactive)
    (setq smsn-minimize-verbatim-blocks (not smsn-minimize-verbatim-blocks))
    (smsn-update-view)
    (message (concat (if smsn-minimize-verbatim-blocks "minimized" "expanded") " verbatim blocks")))

(defun smsn-toggle-properties-view ()
    "enable or disable the explicit display of atom properties as extra lines within views"
    (interactive)
    (setq smsn-view-properties (not smsn-view-properties))
    (smsn-update-view)
    (message (concat (if smsn-view-properties "enabled" "disabled") " property view")))

(defun smsn-toggle-truncate-lines ()
    "toggle line wrap mode"
    (interactive)
    (toggle-truncate-lines))

(defun smsn-update-to-backward-view ()
    "switch to a 'backward' view, i.e. a view in which an atom's parents appear as list items beneath it"
    (interactive)
    (if (in-view)
        (request-view nil smsn-mode smsn-root-id smsn-height smsn-backward-style
            smsn-min-sharability smsn-max-sharability smsn-default-sharability
            smsn-min-weight smsn-max-weight smsn-default-weight)))

(defun smsn-update-to-forward-view ()
    "switch to a 'forward' view (the default), i.e. a view in which an atom's children appear as list items beneath it"
    (interactive)
    (if (in-view)
        (request-view nil smsn-mode smsn-root-id smsn-height smsn-forward-style
            smsn-min-sharability smsn-max-sharability smsn-default-sharability
            smsn-min-weight smsn-max-weight smsn-default-weight)))

(defun smsn-update-view ()
    "refresh the current view from the data store"
    (interactive)
    (if (in-view)
        (request-view t smsn-mode smsn-root-id smsn-height smsn-style
            smsn-min-sharability smsn-max-sharability smsn-default-sharability
            smsn-min-weight smsn-max-weight smsn-default-weight)))

(defun smsn-visit-in-amazon (value-selector)
    "search Amazon.com for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://www.amazon.com/s?ie=UTF8&index=blended&link_code=qs&field-keywords=" (w3m-url-encode-string value)))))

(defun smsn-visit-in-delicious (value-selector)
    "search delicious.com for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://www.delicious.com/search?p=" (w3m-url-encode-string value)))))

(defun smsn-visit-in-ebay (value-selector)
    "search ebay.com for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://www.ebay.com/sch/i.html?_nkw=" (w3m-url-encode-string value)))))

(defun smsn-visit-in-google (value-selector)
    "search google.com for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://www.google.com/search?ie=UTF-8&q=" (w3m-url-encode-string value)))))

(defun smsn-visit-in-google-maps (value-selector)
    "search Google Maps for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://maps.google.com/maps?q=" (w3m-url-encode-string value)))))

(defun smsn-visit-in-google-scholar (value-selector)
    "search Google Scholar for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://scholar.google.com/scholar?q=" (w3m-url-encode-string value)))))

(defun smsn-visit-in-twitter (value-selector)
    "search twitter.com for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://twitter.com/#!/search/" (w3m-url-encode-string value)))))

(defun smsn-visit-in-wikipedia (value-selector)
    "search en.wikipedia.org for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&search=" (w3m-url-encode-string value)))))

(defun smsn-visit-in-youtube (value-selector)
    "search youtube.com for the value generated by VALUE-SELECTOR and view the results in a browser"
    (visit-target-value value-selector (lambda (value)
        (concat "http://www.youtube.com/results?search_query=" (w3m-url-encode-string value)))))

(defun smsn-visit-target ()
    "navigate to the atom at point, opening a new view with that atom as root"
    (interactive)
    (let ((id (atom-id-at-point)))
        (if id
            (request-view nil (mode-for-visit) id smsn-height smsn-style smsn-min-sharability smsn-max-sharability (future-sharability (current-target-sharability)) smsn-min-weight smsn-max-weight smsn-default-weight)
            (no-target))))

(defun smsn-visit-target-alias ()
    "visit the @alias of the atom at point (normally a URL) in a browser"
    (interactive)
    (let ((alias (current-target-alias)))
        (if alias
            (browse-url alias)
            (no-target))))

(defun smsn-visit-as-url (value-selector)
    "visit the URL generated by VALUE-SELECTOR in a browser"
    (visit-target-value value-selector (lambda (value) value)))

(defun smsn-visit-url-at-point ()
    "visit the URL at point in a browser"
    (interactive)
    (goto-address-at-point))


;; KEYBOARD MAPPINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun minibuffer-arg (function prompt &optional initial)
    (lexical-let ((f function) (p prompt) (i initial))
        (lambda ()
            (interactive)
            ;; note: use of the INITIAL argument is discouraged, but here it makes sense
            (let ((arg (read-from-minibuffer p i)))
                (if arg (funcall f arg))))))

(defun char-arg (function prompt)
    (lexical-let ((f function) (p prompt))
        (lambda ()
            (interactive)
            (let ((c (read-char p)))
                (if c (funcall f c))))))

;; Note: when updating this list of mappings, also update the PKB
(global-set-key (kbd "C-c C-I f")       'smsn-find-isolated-atoms)
(global-set-key (kbd "C-c C-I r")       'smsn-remove-isolated-atoms)
(global-set-key (kbd "C-c C-a C-p")     (char-arg 'smsn-insert-attr-priority "priority = ?"))
(global-set-key (kbd "C-c C-a C-s")     (char-arg 'smsn-insert-attr-sharability "sharability = ?"))
(global-set-key (kbd "C-c C-a C-w")     (char-arg 'smsn-insert-attr-weight "weight = ?"))
(global-set-key (kbd "C-c C-a d")       'smsn-insert-current-date)
(global-set-key (kbd "C-c C-a s")       'smsn-insert-current-time-with-seconds)
(global-set-key (kbd "C-c C-a t")       'smsn-insert-current-time)
(global-set-key (kbd "C-c C-d")         (char-arg 'smsn-set-view-height "height = ?"))
(global-set-key (kbd "C-c C-e e")       (minibuffer-arg 'smsn-export-edges "export edges to file: " smsn-default-edges-file))
(global-set-key (kbd "C-c C-e g")       (minibuffer-arg 'smsn-export-graphml "export GraphML to file: " smsn-default-graphml-file))
(global-set-key (kbd "C-c C-e l")       (minibuffer-arg 'smsn-export-latex "export LaTeX to file: " smsn-default-latex-file))
(global-set-key (kbd "C-c C-e p")       (minibuffer-arg 'smsn-export-pagerank "export PageRank results to file: " smsn-default-pagerank-file))
(global-set-key (kbd "C-c C-e r")       (minibuffer-arg 'smsn-export-rdf "export private RDF dump to file: " smsn-default-rdf-file))
(global-set-key (kbd "C-c C-e v")       (minibuffer-arg 'smsn-export-vertices "export vertices to file: " smsn-default-vertices-file))
(global-set-key (kbd "C-c C-e w")       (minibuffer-arg 'smsn-export-webrdf "export public Web RDF dump to file: " smsn-default-webrdf-file))
(global-set-key (kbd "C-c C-i g")       (minibuffer-arg 'smsn-import-graphml "import GraphML from file: " smsn-default-graphml-file))
(global-set-key (kbd "C-c C-l")         (minibuffer-arg 'smsn-goto-line "line: "))
(global-set-key (kbd "C-c C-r C-b a")   (smsn-visit-in-amazon 'current-root-value))
(global-set-key (kbd "C-c C-r C-b e")   (smsn-visit-in-ebay 'current-root-value))
(global-set-key (kbd "C-c C-r C-b d")   (smsn-visit-in-delicious 'current-root-value))
(global-set-key (kbd "C-c C-r C-b g")   (smsn-visit-in-google 'current-root-value))
(global-set-key (kbd "C-c C-r C-b m")   (smsn-visit-in-google-maps 'current-root-value))
(global-set-key (kbd "C-c C-r C-b s")   (smsn-visit-in-google-scholar 'current-root-value))
(global-set-key (kbd "C-c C-r C-b t")   (smsn-visit-in-twitter 'current-root-value))
(global-set-key (kbd "C-c C-r C-b w")   (smsn-visit-in-wikipedia 'current-root-value))
(global-set-key (kbd "C-c C-r C-b y")   (smsn-visit-in-youtube 'current-root-value))
(global-set-key (kbd "C-c C-s C-d")     (char-arg 'smsn-set-default-sharability "default sharability = ?"))
(global-set-key (kbd "C-c C-s C-m")     (char-arg 'smsn-set-min-sharability "minimum sharability = ?"))
(global-set-key (kbd "C-c C-t C-a b")   'smsn-visit-target-alias)
(global-set-key (kbd "C-c C-t C-b a")   (smsn-visit-in-amazon 'current-target-value))
(global-set-key (kbd "C-c C-t C-b e")   (smsn-visit-in-ebay 'current-target-value))
(global-set-key (kbd "C-c C-t C-b d")   (smsn-visit-in-delicious 'current-target-value))
(global-set-key (kbd "C-c C-t C-b g")   (smsn-visit-in-google 'current-target-value))
(global-set-key (kbd "C-c C-t C-b m")   (smsn-visit-in-google-maps 'current-target-value))
(global-set-key (kbd "C-c C-t C-b s")   (smsn-visit-in-google-scholar 'current-target-value))
(global-set-key (kbd "C-c C-t C-b t")   (smsn-visit-in-twitter 'current-target-value))
(global-set-key (kbd "C-c C-t C-b w")   (smsn-visit-in-wikipedia 'current-target-value))
(global-set-key (kbd "C-c C-t C-b y")   (smsn-visit-in-youtube 'current-target-value))
(global-set-key (kbd "C-c C-t C-p")     (char-arg 'smsn-set-target-priority "new priority = ?"))
(global-set-key (kbd "C-c C-t C-s")     (char-arg 'smsn-set-target-sharability "new sharability = ?"))
(global-set-key (kbd "C-c C-t C-w")     (char-arg 'smsn-set-target-weight "new weight = ?"))
;; TODO: finish generalizing these "C-c C-t x" functions to root vs. target
(global-set-key (kbd "C-c C-t a")       (smsn-visit-as-url 'current-target-value))
(global-set-key (kbd "C-c C-t c")       'smsn-copy-target-value-to-clipboard)
(global-set-key (kbd "C-c C-t i")       (smsn-atom-info 'current-target))
(global-set-key (kbd "C-c C-t l")       'smsn-preview-target-latex-math)
(global-set-key (kbd "C-c C-t r")       'smsn-copy-target-reference-to-clipboard)
;; Note: this should perhaps be a local setting
(global-set-key (kbd "C-c C-v ;")       'smsn-toggle-truncate-lines)
(global-set-key (kbd "C-c C-v b")       'smsn-update-to-backward-view)
(global-set-key (kbd "C-c C-v e")       'smsn-enter-edit-view)
(global-set-key (kbd "C-c C-v f")       'smsn-update-to-forward-view)
(global-set-key (kbd "C-c C-v i")       'smsn-toggle-inference-viewstyle)
(global-set-key (kbd "C-c C-v p")       'smsn-toggle-properties-view)
(global-set-key (kbd "C-c C-v r")       'smsn-enter-readonly-view)
(global-set-key (kbd "C-c C-v s")       'smsn-toggle-emacspeak)
(global-set-key (kbd "C-c C-v t")       (minibuffer-arg 'smsn-set-value-truncation-length "value truncation length: "))
(global-set-key (kbd "C-c C-v v")       'smsn-toggle-minimize-verbatim-blocks)
(global-set-key (kbd "C-c C-w C-d")     (char-arg 'smsn-set-default-weight "default weight = ?"))
(global-set-key (kbd "C-c C-w C-m")     (char-arg 'smsn-set-min-weight "minimun weight = ?"))
(global-set-key (kbd "C-c a")           (minibuffer-arg 'smsn-acronym-query "acronym search for: "))
(global-set-key (kbd "C-c b")           'smsn-visit-url-at-point)
(global-set-key (kbd "C-c d")           'smsn-duplicates)
(global-set-key (kbd "C-c f")           'smsn-find-roots)
(global-set-key (kbd "C-c h")           'smsn-history)
(global-set-key (kbd "C-c i")           'smsn-infer-types)
(global-set-key (kbd "C-c o")           (minibuffer-arg 'smsn-shortcut-query "shortcut search for: "))
(global-set-key (kbd "C-c P")           'smsn-priorities)
(global-set-key (kbd "C-c p")           'smsn-push-view)
(global-set-key (kbd "C-c r")           (minibuffer-arg 'smsn-ripple-query "ripple query: "))
(global-set-key (kbd "C-c s")           (minibuffer-arg 'smsn-fulltext-query "full-text search for: "))
(global-set-key (kbd "C-c t")           'smsn-visit-target)
(global-set-key (kbd "C-c u")           'smsn-update-view)
(global-set-key (kbd "C-c v")           'smsn-events)

;; special mappings reserved for use through emacsclient
;; C-c c  --  atom-id-at-point


;; WRAPPER API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smsn-emacsclient-eval (function)
    "evaluate FUNCTION from emacsclient as if a user had typed it into the current buffer"
    (set-buffer (window-buffer (selected-window)))
    (funcall function))

(defun smsn-previous-line ()
    (interactive)
    (previous-line)
    (emacspeak-speak-line))

(defun smsn-next-line ()
    (interactive)
    (next-line)
    (emacspeak-speak-line))

(defun smsn-backward-char ()
    (interactive)
    (backward-char)
    (emacspeak-speak-display-char t)) ;; PREFIX arg disables phonetic pronunciation

(defun smsn-forward-char ()
    (interactive)
    (forward-char)
    (emacspeak-speak-display-char t)) ;; PREFIX arg disables phonetic pronunciation


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'brain-mode)
