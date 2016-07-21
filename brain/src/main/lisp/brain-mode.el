;; Brain-mode: the MyOtherBrain Emacs library
;;
;; This major mode allows you to view, edit, search, and process a MyOtherBrain personal knowledge base.
;;
;; Dependencies:
;;
;;     json, linum, goto-addr, aes, and latex-math-preview
;;
;; Required global variables:
;;
;;     brain-rexster-url: IP, port, and local path to the rexster server
;;     brain-rexster-graph: name of MyOtherBrain graph served by Rexster
;;
;; Optional global variables:
;;
;;     brain-default-graphml-file: file to which GraphML dumps will be exported by default
;;     brain-default-vertices-file: file to which tab-separated vertex dumps will be exported by default
;;     brain-default-edge-file: file to which tab-separated edge dumps will be exported by default
;;     brain-default-pagerank-file: file to which PageRank results will be exported by default
;;
;; For example:
;;
;;     (defvar brain-rexster-url "http://localhost:8182")
;;     (defvar brain-rexster-graph "joshkb")
;;     (defvar brain-default-graphml-file "/tmp/joshkb-graphml.xml")
;;     (defvar brain-default-vertices-file "/tmp/joshkb-vertices.tsv")
;;     (defvar brain-default-edges-file "/tmp/joshkb-edges.tsv")
;;     (defvar brain-default-pagerank-file "/tmp/joshkb-pagerank.tsv")


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


;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst brain-const-max-height 7)

(defconst brain-const-readonly-mode "readonly")
(defconst brain-const-edit-mode "readwrite")
(defconst brain-const-search-mode "search")

(defconst brain-const-color-by-sharability "sharability")
(defconst brain-const-color-by-class-inference "inference")

(defconst brain-const-forward-style "forward")
(defconst brain-const-backward-style "backward")

(defconst brain-const-sharability-private 0.25)
(defconst brain-const-sharability-personal 0.5)
(defconst brain-const-sharability-universal 1.0)
(defconst brain-const-weight-none 0.0)
(defconst brain-const-weight-default 0.5)
(defconst brain-const-weight-all 1.0)

(defconst brain-const-date-format "%Y-%m-%d")
(defconst brain-const-time-format "%H:%M")
(defconst brain-const-time-with-seconds-format "%H:%M:%S")


;; BUFFER-LOCAL CONTEXT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-context (&optional context)
  (if context context brain-bufferlocal-context))

(defun set-context (context)
  (setq brain-bufferlocal-context context)
  (make-local-variable 'brain-bufferlocal-context))

(defun refresh-context (&optional context)
  (set-line (line-number-at-pos) context))

(defun clone-context (&optional context)
  (refresh-context context)
  (let ((context (copy-alist (get-context context))))
    (set-future-sharability context)
    context))

(defun get-value (key json)
  (cdr (assoc key json)))

(defun get-bufferlocal (context key)
  (let ((value (get-value key (get-context context))))
    (if (equal NIL value) nil value)))

(defun set-bufferlocal (context key value)
  (setcdr (assoc key (get-context context)) value))

(defun get-atoms-by-id (&optional context) (get-bufferlocal context 'atoms-by-id))
(defun get-default-sharability (&optional context) (get-bufferlocal context 'default-sharability))
(defun get-default-weight (&optional context) (get-bufferlocal context 'default-weight))
(defun get-file (&optional context) (get-bufferlocal context 'file))
(defun get-format (&optional context) (get-bufferlocal context 'format))
(defun get-height (&optional context) (get-bufferlocal context 'height))
(defun get-line (&optional context) (get-bufferlocal context 'line))
(defun get-max-sharability (&optional context) (get-bufferlocal context 'max-sharability))
(defun get-max-weight (&optional context) (get-bufferlocal context 'max-weight))
(defun get-min-sharability (&optional context) (get-bufferlocal context 'min-sharability))
(defun get-min-weight (&optional context) (get-bufferlocal context 'min-weight))
(defun get-minimize-verbatim-blocks (&optional context) (get-bufferlocal context 'minimize-verbatim-blocks))
(defun get-mode (&optional context) (get-bufferlocal context 'mode))
(defun get-query (&optional context) (get-bufferlocal context 'query))
(defun get-query-type (&optional context) (get-bufferlocal context 'query-type))
(defun get-root-id (&optional context) (get-bufferlocal context 'root-id))
(defun get-style (&optional context) (get-bufferlocal context 'style))
(defun get-title (&optional context) (get-bufferlocal context 'title))
(defun get-value-length-cutoff (&optional context) (get-bufferlocal context 'value-length-cutoff))
(defun get-view (&optional context) (get-bufferlocal context 'view))
(defun get-view-properties (&optional context) (get-bufferlocal context 'view-properties))
(defun get-view-style (&optional context) (get-bufferlocal context 'view-style))

(defun set-atoms-by-id (atoms-by-id &optional context) (set-bufferlocal context 'atoms-by-id atoms-by-id))
(defun set-file (file &optional context) (set-bufferlocal context 'file file))
(defun set-format (format &optional context) (set-bufferlocal context 'format format))
(defun set-default-sharability (sharability &optional context) (set-bufferlocal context 'default-sharability sharability))
(defun set-default-weight (weight &optional context) (set-bufferlocal context 'default-weight weight))
(defun set-height (height &optional context) (set-bufferlocal context 'height height))
(defun set-line (line &optional context) (set-bufferlocal context 'line line))
(defun set-max-sharability (sharability &optional context) (set-bufferlocal context 'max-sharability sharability))
(defun set-max-weight (weight &optional context) (set-bufferlocal context 'max-weight weight))
(defun set-min-sharability (sharability &optional context) (set-bufferlocal context 'min-sharability sharability))
(defun set-min-weight (weight &optional context) (set-bufferlocal context 'min-weight weight))
(defun set-minimize-verbatim-blocks (flag &optional context) (set-bufferlocal context 'minimize-verbatim-blocks flag))
(defun set-mode (mode &optional context) (set-bufferlocal context 'mode mode))
(defun set-query (query &optional context) (set-bufferlocal context 'query query))
(defun set-query-type (type &optional context) (set-bufferlocal context 'query-type type))
(defun set-root-id (root-id &optional context) (set-bufferlocal context 'root-id root-id))
(defun set-style (style &optional context) (set-bufferlocal context 'style style))
(defun set-title (title &optional context) (set-bufferlocal context 'title title))
(defun set-value-length-cutoff (cutoff &optional context) (set-bufferlocal context 'value-length-cutoff cutoff))
(defun set-view (view &optional context) (set-bufferlocal context 'view view))
(defun set-view-properties (view-properties &optional context) (set-bufferlocal context 'view-properties view-properties))
(defun set-view-style (view-style &optional context) (set-bufferlocal context 'view-style view-style))

(defun set-search-mode (context)
    (set-mode brain-const-search-mode context)
    (set-line 1 context))

(defconst NIL "none")

(defun default-context () (list
  (cons 'atoms-by-id NIL)
  (cons 'default-sharability brain-const-sharability-personal)
  (cons 'default-weight brain-const-weight-default)
  (cons 'file NIL)
  (cons 'format NIL)
  (cons 'height 2)
  (cons 'line 1)
  (cons 'max-sharability brain-const-sharability-universal)
  (cons 'max-weight brain-const-weight-all)
  (cons 'min-sharability brain-const-sharability-private)
  (cons 'min-weight brain-const-weight-none)
  (cons 'minimize-verbatim-blocks NIL)
  (cons 'mode brain-const-readonly-mode)
  (cons 'query NIL)
  (cons 'query-type NIL)
  (cons 'root-id NIL)
  (cons 'style brain-const-forward-style)
  (cons 'title NIL)
  (cons 'value-length-cutoff 100)
  (cons 'view NIL)
  (cons 'view-properties NIL)
  (cons 'view-style brain-const-color-by-sharability)))

(defun brain-define-buffer-local-variables ()
  (defvar brain-bufferlocal-context (default-context)))


;; MISC. HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun debug-message (msg)
  (message "%s" (concat "Debug: " msg)))

(defun info-message (msg)
  (message "%s" (concat "Info: " msg)))

(defun error-message (msg)
  (message "%s" (concat "Error: " msg)))


;; DATA MODEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun atom-id-at-point ()
  (let ((line (current-line)))
    (if (string-match "^[0-9A-Za-z@&]*: " line)
        (let ((i3 (string-match ": " line)))
          (let ((s2 (substring line 0 i3)))
            (if (< 0 (length s2)) s2 nil)))
      (get-text-property (line-beginning-position) 'target-id))))

(defun get-atom-id (atom)
  (get-value 'id atom))

(defun get-atom-created (atom)
  (get-value 'created atom))

(defun get-atom-value (atom)
  (get-value 'value atom))

(defun get-atom-priority (atom)
  (let ((v (assoc 'priority atom)))
    (if v (cdr v) nil)))

(defun get-atom-sharability (atom)
  (let ((v (assoc 'sharability atom)))
    (if v (cdr v) (get-default-sharability))))

(defun get-atom-weight (atom)
  (let ((v (assoc 'weight atom)))
    (if v (cdr v) (get-default-weight))))

(defun get-atom-alias (atom)
  (let ((x (assoc 'alias atom)))
    (if x (cdr x) nil)))

(defun get-atom-shortcut (atom)
  (let ((x (assoc 'shortcut atom)))
    (if x (cdr x) nil)))

(defun get-atom-type (atom)
  (let ((x (assoc 'type atom)))
    (if x (cdr x) nil)))

(defun get-atom-meta (atom)
  (let ((x (assoc 'meta atom)))
    (if x (cdr x) nil)))

(defun current-root ()
  (get-atom (get-root-id)))

(defun current-root-value ()
  (let ((g (current-root)))
    (if g (get-atom-value g))))

(defun current-target ()
  (get-atom (atom-id-at-point)))

(defun current-target-value ()
  (let ((g (current-target)))
    (if g (get-atom-value g))))

(defun current-target-alias ()
  (let ((g (current-target)))
    (if g (get-atom-alias g))))

(defun current-target-sharability ()
  (let ((g (current-target)))
    (if g (get-atom-sharability g))))

(defun set-future-sharability (context)
  (let ((sharability (current-target-sharability)))
    (set-default-sharability (adjust-default-sharability sharability) context)))

;; change the default sharability in the new view after a user visits a link or target
;; The default will never be greater than 0.75 unless explicitly set by the user.
(defun adjust-default-sharability (sharability)
  (if sharability
      (if (<= sharability 0.75) sharability 0.75)
    0.5))

(defun get-atom (id)
  (if id
      (let ((atoms (get-atoms-by-id)))
        (if atoms (gethash id atoms) nil))
    nil))

(defun show-info (atom)
  (let (
        (created (get-atom-created atom))
        (value (get-atom-value atom))
        (weight (get-atom-weight atom))
        (sharability (get-atom-sharability atom))
        (priority (get-atom-priority atom))
        (alias (get-atom-alias atom))
        (meta (get-atom-meta atom)))
    ;;(type (get-atom-type atom)))
    (info-message (concat
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

(defconst fast-numbers
  '(
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

(defconst line-addr-keypairs
 (list
  '(?0 ?0) '(?1 ?1) '(?2 ?2) '(?3 ?3) '(?4 ?4) '(?5 ?5) '(?6 ?6) '(?7 ?7) '(?8 ?8) '(?9 ?9)
  '(?\; ?0) '(?a ?1) '(?s ?2) '(?d ?3) '(?f ?4) '(?g ?5) '(?h ?6) '(?j ?7) '(?k ?8) '(?l ?9)
  '(?u ?1) '(?i ?2) '(?o ?3) '(?p ?4)))

(defvar line-addr-keymap (make-hash-table))

(defun current-line ()
  (interactive)
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun no-link ()
  (error-message "there is no link associated with this line"))

(defun no-target ()
  (error-message "there is no target associated with this line"))

(dolist (pair line-addr-keypairs)
  (puthash (car pair) (car (cdr pair)) line-addr-keymap))

(defun mapkey (c)
  (gethash c line-addr-keymap))

(defun address-to-lineno (address)
  (if (string-match "[0-9asdfghjkl;]+" address)
      (string-to-number (coerce (mapcar 'mapkey (coerce address 'list)) 'string))
    nil))

(defun handle-changewindow (address)
  (let ((c (car (coerce address 'list))))
    (if (string-match "[uiop]" (string c))
        (let ((n (string-to-number (string (gethash c line-addr-keymap)))))
          (other-window n)
          (coerce (cdr (coerce address 'list)) 'string))
      address)))

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
         (mapcar (lambda (ch)
                   (cond
                    ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
                     (char-to-string ch)) ; printable
                    (t
                     (format "%%%02X" ch)))) ; escape
                 ;; Coerce a string to a list of chars.
                 (append (encode-coding-string str (or coding 'utf-8))
                         nil))))

(defun format-request-data (params)
  (debug-message (concat "request to encode: " (json-encode params)))
  (mapconcat
    (lambda (arg)
      (concat
        (w3m-url-encode-string (car arg))
        "="
        (w3m-url-encode-string (car (last arg)))))
    params
    "&"))

(defun http-post (url params callback)
  "Send PARAMS to URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded;charset=UTF-8")))
        (url-request-data
          (format-request-data params)))
    (url-retrieve url callback)))

(defun http-get (url callback)
  (url-retrieve url callback))

(defun strip-http-headers (entity)
  (let ((i (string-match "\n\n" entity)))
    (decode-coding-string (substring entity (+ i 2)) 'utf-8)))

(defun base-url ()
  (concat brain-rexster-url "/graphs/" brain-rexster-graph "/smsn/"))

(defun show-http-response-status (status json)
  (let ((msg (get-value 'message json))
        (error (get-value 'error json)))
    (if error
        (error-message error)
      (error-message msg))))

(defun acknowledge-http-response (status success-message)
  (let ((json (json-read-from-string (strip-http-headers (buffer-string)))))
    (if status
        (show-http-response-status status json)
      (info-message success-message))))

(defun brain-switch-to-buffer (name context)
  "activate Brain-mode in all new view buffers created by Brain-mode"
  (switch-to-buffer name)
  (brain-mode)
  (set-context context))

(defun receive-view (&optional context)
  (lexical-let ((context (get-context context)))
    (lambda (status) (receive-view-internal status context))))

(defun receive-view-internal (status context)
  (let ((json (json-read-from-string (strip-http-headers (buffer-string))))
      (editable (is-readwrite-context context)))
  (debug-message (concat "mode provided: " (get-mode context)))
  (debug-message (concat "editable: " (if editable "T" "F")))
  (debug-message (concat "context before: " (json-encode context)))
  (debug-message (concat "global context before: " (json-encode brain-bufferlocal-context)))
    (if status
      (show-http-response-status status json)
  (debug-message (concat "mode before: " (get-mode)))
      (let (
        (view (get-value 'view json))
        (root-id (get-value 'root json))
        (height (numeric-value json 'height nil)))
          (brain-switch-to-buffer (name-for-view-buffer root-id json) context)
          (receive-context json context)
  (debug-message (concat "context after: " (json-encode context)))
  (debug-message (concat "global context after: " (json-encode brain-bufferlocal-context)))
  (debug-message (concat "root: " root-id))
  (debug-message (concat "json: " (json-encode json)))
          (if (equal (get-mode) brain-const-search-mode)
              ;; Always leave a search view with height 1, rather than that of the last view.
              ;; The user experience is a little unpredictable otherwise.
              (setq brain-current-height 1)
              (if height (setq brain-current-height height)))
          (erase-buffer)
  (debug-message "got the view...")
   (debug-message (concat "mode after: " (get-mode)))
         (write-view editable (get-value 'children view) 0)
          (beginning-of-buffer)
          (setq visible-cursor t)
          ;; Try to move to the corresponding line in the previous view.
          ;; This is not always possible and not always helpful, but it is often both.
          (beginning-of-line (get-line))
          (setq buffer-read-only (not editable))
          ;; always include line numbers in views
          (linum-mode t)
          (info-message (concat "updated to view " (view-info)))))))

(defun receive-context (json context)
  (set-min-sharability (numeric-value json 'minSharability (get-min-sharability context)))
  (set-max-sharability (numeric-value json 'maxSharability (get-max-sharability context)))
  (set-default-sharability (numeric-value json 'defaultSharability (get-default-sharability context)))
  (set-min-weight (numeric-value json 'minWeight (get-min-weight context)))
  (set-max-weight (numeric-value json 'maxWeight (get-max-weight)))
  (set-default-weight (numeric-value json 'defaultWeight (get-default-weight)))
  (set-root-id (get-value 'root json))
  (set-height
    ;; Always leave a search view with height 1, rather than that of the last view.
    ;; The user experience is a little unpredictable otherwise.
    (if (equal (get-mode) brain-const-search-mode)
      1
      (numeric-value json 'height (get-height context))))
  (let ((style (get-value 'style json)))
    (if style (set-style style)))
  (set-title (get-value 'title json))
  (set-atoms-by-id (make-hash-table :test 'equal)))

(defun receive-export-results (status)
  (acknowledge-http-response status "exported successfully"))

(defun receive-import-results (status)
  (acknowledge-http-response status "imported successfully"))

(defun receive-inference-results (status)
  (acknowledge-http-response status "type inference completed successfully"))

(defun to-query-list (&optional context)
  (list
    :root (get-root-id context)
    :height (get-height context)
    :style (get-style context)
    :includeTypes (if (using-inference) "true" "false")
    :file (get-file context)
    :format (get-format context)
    :query (get-query context)
    :queryType (get-query-type context)
    :valueCutoff (get-value-length-cutoff)
    ;;:maxResults 100
    :view (get-view)
    :filter (list
      :minSharability (get-min-sharability context)
      :maxSharability (get-max-sharability context)
      :defaultSharability (get-default-sharability context)
      :minWeight (get-min-weight context)
      :maxWeight (get-max-weight context)
      :defaultWeight (get-default-weight context))))

(defun entity-for-request (params)
  (w3m-url-encode-string (json-encode params)))

(defun url-for-request (path &optional params)
  (concat
    (base-url)
    path
    (if params
      (entity-for-request params)
      nil)))

(defun url-for-view-request (&optional context)
  (url-for-request "view?request=" (to-query-list context)))

(defun http-get-and-receive (url &optional handler context)
  (http-get url
    (if handler handler (receive-view context))))

(defun http-post-and-receive (url params &optional handler context)
  (http-post url params
    (if handler handler (receive-view context))))

(defun to-params (context params)
  (if params params
    (to-query-list (if context context (get-context)))))
    
(defun fetch-path (path context params &optional handler)
  (http-get-and-receive (url-for-request path (to-params context params)) handler context))

(defun fetch-path-post (path context params &optional handler)
  (http-post-and-receive (url-for-request path)
    (to-params context params) handler context))
    ;;(entity-for-request (to-params context params)) handler context))

(defun fetch-view (&optional context)
  (http-get-and-receive (url-for-view-request context) nil context))

(defun fetch-history ()
  (let ((context (clone-context)))
    (set-search-mode context)
    (fetch-path "history?request=" context nil)))

(defun fetch-events (height)
  (let ((context (clone-context)))
    (set-search-mode context)
    (fetch-path "get-events?request=" context nil)))

(defun fetch-duplicates ()
  (let ((context (clone-context)))
    (set-search-mode context)
    (fetch-path "duplicates?request=" context nil)))

(defun fetch-query (query query-type)
  (let ((context (clone-context)))
    (set-search-mode context)
    (set-query query context)
    (set-query-type query-type context)
    (fetch-path "search?request=" context nil)))

(defun fetch-priorities ()
  (let ((context (clone-context)))
    (set-search-mode context)
    (fetch-path "priorities?request=" context nil)))

(defun fetch-find-isolated-atoms ()
  (let ((context (clone-context)))
    (set-search-mode context)
    (fetch-path "find-isolated-atoms?request=" context nil)))

(defun fetch-find-roots ()
  (let ((context (clone-context)))
    (set-search-mode context)
    (fetch-path "find-roots?request=" context nil)))

(defun fetch-remove-isolated-atoms ()
  (fetch-path "remove-isolated-atoms?request=" nil nil
    (lambda (status)
      (interactive)
      (if status
         (acknowledge-http-response status "removed isolated atoms")))))

(defun fetch-ripple-results (query)
  (let ((context (clone-context)))
    (set-search-mode context)
    (set-height 1 context)
    (set-query query context)
    (fetch-path "ripple?request=" context nil)))

(defun do-export (format file)
  (let ((context (clone-context)))
    (set-format format context)
    (set-file file context)
    ;;(set-context context)))
    (fetch-path "export?request=" context nil 'receive-export-results)))

(defun do-import (format file)
  (let ((context (clone-context)))
    (set-format format context)
    (set-file file context)
    (fetch-path "import?request=" context nil 'receive-import-results)))

(defun set-property (id name value)
  (if (in-setproperties-mode)
    (let ((params (list :id id :name name :value value)))
       (fetch-path "set?request=" context params))))

(defun push-view ()
  (let ((context (clone-context)) (entity (buffer-string)))
    (set-view entity context)
    (set-mode brain-const-edit-mode context)
    (fetch-path-post "update" context nil)))
         
(defun do-infer-types ()
  (http-get
   (concat (base-url) "infer-types") 'receive-inference-results))


;; VIEWS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; unused colors: black/gray, orange
(defconst sharability-base-colors  '("#660000" "#604000" "#005000" "#000066"))
(defconst sharability-bright-colors  '("#D00000" "#D0B000" "#00B000" "#0000D0"))
(defconst sharability-reduced-colors '("red" "red" "blue" "blue"))
(defconst inference-base-colors '("#660066" "#006666"))
(defconst inference-bright-colors '("#FF00FF" "#00FFFF"))

(defvar full-colors-supported (> (length (defined-colors)) 8))

(defun shorten-title (str maxlen)
  (if (> (length str) maxlen)
    (concat (substring str 0 maxlen) "...")
    str))

(defun name-for-view-buffer (root-id json)
  (let ((title (get-value 'title json)))
    (if root-id
      (concat (shorten-title title 20) " [" root-id "]")
      title)))

(defun using-inference ()
  (equal (get-view-style) brain-const-color-by-class-inference))

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

(defun atom-color (weight sharability bright has-meta)
  (let ((s
         (if (using-inference)
             (elt (if bright inference-bright-colors inference-base-colors) (if has-meta 0 1))
           (elt (if bright sharability-bright-colors sharability-base-colors) (- (ceiling (* sharability 4)) 1)))))
    (color-string
     (fade-color (color-part-red s) weight)
     (fade-color (color-part-green s) weight)
     (fade-color (color-part-blue s) weight))))

(defun atom-color-at-visibility-threshold ()
  (atom-color 0.75 (+ 0.25 (get-min-sharability)) nil nil))

(defun colorize (text weight sharability priority-bg priority-fg bright has-meta)
  (let ((color (if full-colors-supported
                   (atom-color weight sharability bright has-meta)
                 (elt sharability-reduced-colors (- (ceiling (* sharability 4)) 1)))))
    (setq l (list
             :foreground color
             ;;:weight 'bold
             :underline (if (and priority-fg (> priority-fg 0))
                            (list :color (atom-color priority-fg sharability bright has-meta)) nil)
             :box (if priority-bg (list
                                   :color (atom-color priority-bg sharability bright has-meta)) nil)))
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
                    (if (get-minimize-verbatim-blocks) (propertize content 'invisible t) content)
                    "}}}")) value)))

(defun write-view (editable children tree-indent)
  (loop for json across children do
        (let (
              (link (get-value 'link json))
              (children (get-value 'children json)))
          (let ((target-id (get-atom-id json))
                (target-value (let ((v (get-atom-value json))) (if v v "")))
		        (target-weight (get-atom-weight json))
		        (target-sharability (get-atom-sharability json))
		        (target-priority (get-atom-priority json))
                (target-has-children (not (equal json-false (get-value 'hasChildren json))))
		        (target-alias (get-atom-alias json))
		        (target-shortcut (get-atom-shortcut json))
		        (target-meta (get-atom-meta json)))
            (if target-id
              (puthash target-id json (get-atoms-by-id))
              (error "missing target id"))
            (setq space "")
            (loop for i from 1 to tree-indent do (setq space (concat space " ")))
            (let ((line "") (id-infix (create-id-infix target-id)))
              (if (not editable)
                  (setq id-infix (propertize id-infix 'invisible t)))
              (setq line (concat line space))
              (let ((bullet (if target-has-children "+" "\u00b7"))) ;; previously: "-" or "\u25ba"
                (setq line (concat line
                                   (colorize bullet
                                             target-weight target-sharability target-priority nil target-alias target-meta)
                                   id-infix
                                   " "
                                   (colorize (delimit-value target-value)
                                             target-weight target-sharability nil target-priority target-alias target-meta)
                                   "\n")))
              (insert (propertize line 'target-id target-id)))
            (if (using-inference)
                (loop for a across target-meta do (insert (light-gray (concat space "    @{" a "}\n")))))
            (if (get-view-properties) (let ()
                                                (insert (light-gray
                                                         (concat space "    @sharability " (number-to-string target-sharability) "\n")))
                                                (insert (light-gray
                                                         (concat space "    @weight      " (number-to-string target-weight) "\n")))
                                                (if target-shortcut
                                                    (insert (light-gray (concat space "    @shortcut    " target-shortcut "\n"))))
                                                (if target-alias
                                                    (insert (light-gray (concat space "    @alias       " target-alias "\n"))))))
            (write-view editable children (+ tree-indent 4))))))

(defun num-or-nil-to-string (n)
  (if n (number-to-string n) "nil"))

(defun view-info ()
  (concat
   "(root: " (get-root-id)
   " :height " (num-or-nil-to-string (get-height))
   " :style " (get-style)
   " :sharability
             [" (num-or-nil-to-string (get-min-sharability))
   ", " (num-or-nil-to-string (get-default-sharability))
   ", " (num-or-nil-to-string (get-max-sharability)) "]"
   " :weight
             [" (num-or-nil-to-string (get-min-weight))
   ", " (num-or-nil-to-string (get-default-weight))
   ", " (num-or-nil-to-string (get-max-weight)) "]"
   " :value \"" (get-title) "\")")) ;; TODO: actually escape the title string

(defun mode-for-visit ()
  (if (or (equal (get-mode) brain-const-edit-mode) (equal (get-mode) brain-const-readonly-mode))
      (get-mode)
    brain-const-readonly-mode))

(defun in-view-mode ()
  (let ((mode (get-mode)))
    (if (or
        (equal mode brain-const-readonly-mode)
        (equal mode brain-const-edit-mode))
      t
    (and (error-message (concat "cannot create tree view in mode '" mode "'")) nil))))

(defun in-setproperties-mode ()
  "determines whether atom properties can be set in the current buffer"
  (if (or
       (equal (get-mode) brain-const-search-mode)
       (equal (get-mode) brain-const-readonly-mode)
       (equal (get-mode) brain-const-edit-mode))
      t
    (and (error-message "cannot set properties in current mode") nil)))

(defun is-readwrite-context (&optional context)
  (let ((mode (get-mode context)))
    (and mode
      (equal mode brain-const-edit-mode))))

(defun assert-readwrite-context ()
  (if (is-readwrite-context)
    t
    (and (error-message (concat "cannot update view in current mode: " (get-mode))) nil)))


;; UPDATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-min-weight-withchecks (s)
  (if (and (in-setproperties-mode) (>= s 0) (<= s 1))
    (progn
      (set-min-weight s)
      (fetch-view t (get-mode) (get-root-id) (get-height) (get-style)
                    (get-min-sharability) (get-max-sharability) (get-default-sharability)
                    s (get-max-weight) (get-default-weight)))
    (error-message
     (concat "min weight " (number-to-string s) " is outside of range [0, 1]"))))

(defun set-min-sharability-withchecks (s)
  (if (and (in-setproperties-mode) (>= s 0) (<= s 1))
    (progn
      (set-min-sharability s)
      (fetch-view t (get-mode) (get-root-id) (get-height) (get-style)
                    s (get-max-sharability) (get-default-sharability)
                    (get-min-weight) (get-max-weight) (get-default-weight)))
    (error-message
     (concat "min sharability " (number-to-string s) " is outside of range [0, 1]"))))

(defun set-target-priority (v)
  (if (and (>= v 0) (<= v 1))
      (let ((target (current-target)))
        (if target
            (let (
                  (id (get-atom-id target)))
              (set-property id "priority" v))
          (no-target)))
    (error-message
     (concat "priority " (number-to-string v) " is outside of range [0, 1]"))))

(defun set-target-sharability (v)
  (if (and (> v 0) (<= v 1))
      (let ((target (current-target)))
        (if target
            (let ((id (get-atom-id target)))
              (set-property id "sharability" v))
          (no-target)))
    (error-message
     (concat "sharability " (number-to-string v) " is outside of range (0, 1]"))))

(defun set-target-weight (v)
  (if (and (> v 0) (<= v 1))
      (let ((target (current-target)))
        (if target
            (let (
                  (id (get-atom-id target)))
              (set-property id "weight" v))
          (no-target)))
    (error-message
     (concat "weight " (number-to-string v) " is outside of range (0, 1]"))))


;; USER API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun brain-atom-info (selector)
  "display, in the minibuffer, information about an atom produced by SELECTOR"
  (lexical-let ((as selector))
    (lambda () (interactive)
      (let ((atom (funcall as)))
        (if atom
            (show-info atom)
          (no-target))))))

;; note: for some reason, the id doesn't stay invisible when you paste it, although it stays light gray
(defun brain-copy-target-reference-to-clipboard ()
  "copy a reference to the atom at point to the system clipboard"
  (interactive)
  (let ((id (atom-id-at-point)))
    (if id
        (copy-to-clipboard (concat "*" (create-id-infix id)))
      (no-target))))

(defun brain-copy-target-value-to-clipboard ()
  "copy the value of the atom at point to the system clipboard"
  (interactive)
  (let ((value (current-target-value)))
    (if value
        (copy-to-clipboard value)
      (no-target))))

(defun brain-duplicates ()
  "retrieve a list of atoms with duplicate values"
  (interactive)
  (fetch-duplicates))

(defun brain-enter-edit-view ()
  "enter edit (read/write) mode in the current view"
  (interactive)
(debug-message (concat "current mode: " (get-mode)))
  (if (equal (get-mode) brain-const-readonly-mode)
    (let ((context (clone-context)))
      (set-mode brain-const-edit-mode context)
      (fetch-view context))))

(defun brain-enter-readonly-view ()
  "enter read-only mode in the current view"
  (interactive)
  (if (equal (get-mode) brain-const-edit-mode)
    (let ((context (clone-context)))
      (set-mode brain-const-readonly-mode context)
      (fetch-view context))))

(defun brain-events ()
  "retrieve the MyOtherBrain event stack (e.g. notifications of gestural events), ordered by decreasing time stamp"
  (interactive)
  (fetch-events 2))

(defun brain-export-edges (file)
  "export tab-separated dump of MyOtherBrain parent-child edges to the file system"
  (interactive)
  (info-message (concat "exporting edges to " file))
  (do-export "Edges" file))

(defun brain-export-graphml (file)
  "export a GraphML dump of the knowledge base to the file system"
  (interactive)
  (info-message (concat "exporting GraphML to " file))
  (do-export "GraphML" file))

(defun brain-export-latex (file)
  "export a LaTeX-formatted view of a subtree of the knowledge base to the file system"
  (interactive)
  (info-message (concat "exporting LaTeX to " file))
  (do-export "LaTeX" file))

(defun brain-export-pagerank (file)
  "export a tab-separated PageRank ranking of MyOtherBrain atoms to the file system"
  (interactive)
  (info-message (concat "computing and exporting PageRank to " file))
  (do-export "PageRank" file))

(defun brain-export-rdf (file)
  "export a complete RDF dump of the knowledge base (including personal and private data) to the file system"
  (interactive)
  (info-message (concat "exporting private RDF dump to " file))
  (do-export "RDF" file))

(defun brain-export-webrdf (file)
  "export a Web-friendly dump of the public portion of the knowledge base to the file system"
  (interactive)
  (info-message (concat "exporting public Web RDF dump to " file))
  (do-export "Web" file))

(defun brain-export-vertices (file)
  "export tab-separated dump of MyOtherBrain vertices (atoms) to the file system"
  (interactive)
  (info-message (concat "exporting vertices to " file))
  (do-export "Vertices" file))

(defun brain-import-graphml (file)
  "import a GraphML dump from the file system into the knowledge base"
  (interactive)
  (info-message (concat "importing GraphML from " file))
  (do-import "GraphML" file))

(defun brain-find-isolated-atoms ()
  "retrieve a list of isolated atoms (i.e. atoms with neither parents nor children) in the knowledge base"
  (interactive)
  (fetch-find-isolated-atoms))

(defun brain-remove-isolated-atoms ()
  "remove all isolated atoms (i.e. atoms with neither parents nor children) from the knowledge base"
  (interactive)
  (fetch-remove-isolated-atoms))

(defun brain-find-roots ()
  "retrieve a list of roots (i.e. atoms with no parents) in the knowledge base"
  (interactive)
  (fetch-find-roots))

(defun brain-goto-line (address)
  "move point to the line represented by ADDRESS"
  (interactive)
  (let ((line (address-to-lineno (handle-changewindow address))))
    (if line
        (goto-line line)
      (error-message "invalid line address"))))

(defun brain-history ()
  "retrieve a list of the most recently viewed or updated atoms, in decreasing order of recency"
  (interactive)
  (fetch-history))

(defun brain-infer-types ()
  "perform type inference on the MyOtherBrain knowledge base, adding type annotations"
  (interactive)
  (info-message "performing type inference")
  (do-infer-types))

(defun brain-insert-attr-priority (expr)
  "insert a line to set the priority of an atom to the value given by EXPR"
  (interactive)
  (let ((n (number-shorthand-to-number expr)))
    (if n (insert (concat "\n                @priority " (number-to-string (/ n 4.0)) "\n")))))

(defun brain-insert-attr-sharability (expr)
  "insert a line to set the sharability of an atom to the value given by EXPR"
  (interactive)
  (let ((n (number-shorthand-to-number expr)))
    (if n (insert (concat "\n                @sharability " (number-to-string (/ n 4.0)) "\n")))))

(defun brain-insert-attr-weight (expr)
  "insert a line to set the weight of an atom to the value given by EXPR"
  (interactive)
  (let ((n (number-shorthand-to-number expr)))
    (if n (insert (concat "\n                @weight " (number-to-string (/ n 4.0)) "\n")))))

(defun brain-insert-current-date ()
  "insert the current date, in the format yyyy-mm-dd, into the current buffer"
  (interactive)
  (insert (format-time-string brain-const-date-format (current-time))))

(defun brain-insert-current-time ()
  "insert the current time, in the format hh:mm, into the current buffer"
  (interactive)
  (insert (format-time-string brain-const-time-format (current-time))))

(defun brain-insert-current-time-with-seconds ()
  "insert the current time with seconds, in the format hh:mm:ss, into the current buffer"
  (interactive)
  (insert (format-time-string brain-const-time-with-seconds-format (current-time))))

(defun brain-preview-target-latex-math ()
  "create a graphical preview of the value of the atom at point, which must be a LaTeX mathematical expression"
  (interactive)
  (end-of-line)
  (backward-word)
  (latex-math-preview-expression))

(defun brain-priorities ()
  "retrieve a list of atoms with nonzero priority values, ordered by decreasing priority"
  (interactive)
  (fetch-priorities))

(defun brain-push-view ()
  "push an up-to-date view into the knowledge base"
  (interactive)
  (if (assert-readwrite-context)
    (push-view)))

(defun brain-ripple-query (query)
  "evaluate Ripple expression QUERY"
  (interactive)
  (if (> (length query) 0)
      (fetch-ripple-results query)))

(defun brain-fulltext-query (query)
  "evaluate full-text query for QUERY, yielding a ranked list of query results in a new buffer"
  (interactive)
  (if (> (length query) 0)
    (fetch-query query "FullText")))

(defun brain-acronym-query (query)
  "evaluate acronym (abbreviated fulltext) query for QUERY, yielding a ranked list of query results in a new buffer"
  (interactive)
  (if (> (length query) 0)
    (fetch-query query "Acronym")))

(defun brain-shortcut-query (query)
  "evaluate shortcut query for QUERY, yielding query results (normally zero or one) in a new buffer"
  (interactive)
  (if (> (length query) 0)
    (fetch-query query "Shortcut")))

(defun brain-set-min-sharability (expr)
  "set the minimum @sharability (for atoms visible in the current view) to the number represented by EXPR"
  (interactive)
  (let ((n (number-shorthand-to-number expr)))
    (if n (set-min-sharability-withchecks (/ n 4.0)))))

(defun brain-set-min-weight (expr)
  "set the minimum @weight (for atoms visible in the current view) to the number represented by EXPR"
  (interactive)
  (let ((n (number-shorthand-to-number expr)))
    (if n (set-min-weight-withchecks (/ n 4.0)))))

(defun brain-set-target-priority (expr)
  "set the @priority of the atom at point to the number represented by EXPR"
  (interactive)
  (let ((n (number-shorthand-to-number expr)))
    (if n (set-target-priority (/ n 4.0)))))

(defun brain-set-target-sharability (expr)
  "set the @sharability of the atom at point to the number represented by EXPR"
  (interactive)
  (let ((n (number-shorthand-to-number expr)))
    (if n (set-target-sharability (/ n 4.0)))))

(defun brain-set-target-weight (expr)
  "set the @weight of the atom at point to the number represented by EXPR"
  (interactive)
  (let ((n (number-shorthand-to-number expr)))
    (if n (set-target-weight (/ n 4.0)))))

(defun brain-set-value-truncation-length (length-str)
  "set the value truncation length to the number represented by LENGTH-STR.
Longer values are truncated, for efficiency and readability, when they appear in views.
A value of -1 indicates that values should not be truncated."
  (interactive)
  (let ((n (string-to-number length-str)))
    (set-value-length-cutoff n)))

(defun brain-set-view-height (expr)
  "set the height of the current view to the number of levels represented by EXPR"
  (interactive)
  (let ((height (number-shorthand-to-number expr)))
    (if (< height 1) (error-message (concat "height of " (number-to-string height) " is too low (must be >= 1)"))
      (if (> height brain-const-max-height)
          (error-message (concat "height of " (number-to-string height) " is too high (must be <= "
                                 (number-to-string brain-const-max-height) ")"))
          (let ((context (clone-context)))
            (set-height height context)
            (fetch-view context))))))

(defun brain-toggle-emacspeak ()
  "turn Emacspeak on or off"
  (interactive)
  (dtk-toggle-quiet))

(defun brain-toggle-inference-viewstyle ()
  "toggle between the sharability view style and the type inference view style.
In the sharability view style, colors are assigned to atoms based on the sharability of each atom
(for example, private atoms are red, while public atoms are green).
However, in the type inference view style, an atom is either cyan or magenta depending on whether
a type has been assigned to it by the inference engine."
  (interactive)
  (setq (get-view-style) (if (equal (get-view-style) brain-const-color-by-sharability)
                                     brain-const-color-by-class-inference
                                   brain-const-color-by-sharability))
  (brain-update-view)
  (info-message (concat "switched to " (get-view-style) " view style")))

(defun brain-toggle-minimize-verbatim-blocks ()
  "enable or disable the hiding of the contents of {{{verbatim blocks}}}, which may span multiple lines"
  (interactive)
  (set-minimize-verbatim-blocks (not (get-minimize-verbatim-blocks)))
  (brain-update-view)
  (info-message (concat (if (get-minimize-verbatim-blocks) "minimized" "expanded") " verbatim blocks")))

(defun brain-toggle-properties-view ()
  "enable or disable the explicit display of atom properties as extra lines within views"
  (interactive)
  (set-view-properties (not (get-view-properties)))
  (brain-update-view)
  (info-message (concat (if (get-view-properties) "enabled" "disabled") " property view")))

(defun brain-toggle-truncate-lines ()
  "toggle line wrap mode"
  (interactive)
  (toggle-truncate-lines))

(defun brain-update-to-backward-view ()
  "switch to a 'backward' view, i.e. a view in which an atom's parents appear as list items beneath it"
  (interactive)
  (if (in-view-mode)
    (let ((context (clone-context)))
      (set-style brain-const-backward-style)
      (fetch-view context))))

(defun brain-update-to-forward-view ()
  "switch to a 'forward' view (the default), i.e. a view in which an atom's children appear as list items beneath it"
  (interactive)
  (if (in-view-mode)
    (let ((context (clone-context)))
      (set-style brain-const-forward-style)
      (fetch-view context))))

(defun brain-update-view ()
  "refresh the current view from the data store"
  (interactive)
  (if (in-view-mode) (fetch-view)))

(defun brain-visit-in-amazon (value-selector)
  "search Amazon.com for the value generated by VALUE-SELECTOR and view the results in a browser"
  (visit-target-value value-selector (lambda (value)
                                       (concat
                                        "http://www.amazon.com/s?ie=UTF8&index=blended&link_code=qs&field-keywords="
                                        (w3m-url-encode-string value)))))

(defun brain-visit-in-delicious (value-selector)
  "search delicious.com for the value generated by VALUE-SELECTOR and view the results in a browser"
  (visit-target-value value-selector (lambda (value)
                                       (concat "http://www.delicious.com/search?p=" (w3m-url-encode-string value)))))

(defun brain-visit-in-ebay (value-selector)
  "search ebay.com for the value generated by VALUE-SELECTOR and view the results in a browser"
  (visit-target-value value-selector (lambda (value)
                                       (concat "http://www.ebay.com/sch/i.html?_nkw=" (w3m-url-encode-string value)))))

(defun brain-visit-in-google (value-selector)
  "search google.com for the value generated by VALUE-SELECTOR and view the results in a browser"
  (visit-target-value value-selector (lambda (value)
                                       (concat "http://www.google.com/search?ie=UTF-8&q=" (w3m-url-encode-string value)))))

(defun brain-visit-in-google-maps (value-selector)
  "search Google Maps for the value generated by VALUE-SELECTOR and view the results in a browser"
  (visit-target-value value-selector (lambda (value)
                                       (concat "http://maps.google.com/maps?q=" (w3m-url-encode-string value)))))

(defun brain-visit-in-google-scholar (value-selector)
  "search Google Scholar for the value generated by VALUE-SELECTOR and view the results in a browser"
  (visit-target-value value-selector (lambda (value)
                                       (concat "http://scholar.google.com/scholar?q=" (w3m-url-encode-string value)))))

(defun brain-visit-in-twitter (value-selector)
  "search twitter.com for the value generated by VALUE-SELECTOR and view the results in a browser"
  (visit-target-value value-selector (lambda (value)
                                       (concat "http://twitter.com/#!/search/" (w3m-url-encode-string value)))))

(defun brain-visit-in-wikipedia (value-selector)
  "search en.wikipedia.org for the value generated by VALUE-SELECTOR and view the results in a browser"
  (visit-target-value value-selector (lambda (value)
                                       (concat "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&search=" (w3m-url-encode-string value)))))

(defun brain-visit-in-youtube (value-selector)
  "search youtube.com for the value generated by VALUE-SELECTOR and view the results in a browser"
  (visit-target-value value-selector (lambda (value)
                                       (concat "http://www.youtube.com/results?search_query=" (w3m-url-encode-string value)))))

(defun brain-navigate-to-target-atom ()
  "navigate to the atom at point, opening a new view with that atom as root"
  (interactive)
  (let ((id (atom-id-at-point)))
    (if id
        (let ((context (clone-context)))
          (set-root-id id context)
          (set-mode brain-const-readonly-mode context)
          (fetch-view context))
      (no-target))))

(defun brain-navigate-to-target-atom-alias ()
  "visit the @alias of the atom at point (normally a URL) in a browser"
  (interactive)
  (let ((alias (current-target-alias)))
    (if alias
        (browse-url alias)
      (no-target))))

(defun brain-visit-as-url (value-selector)
  "visit the URL generated by VALUE-SELECTOR in a browser"
  (visit-target-value value-selector (lambda (value) value)))

(defun brain-visit-url-at-point ()
  "visit the URL at point in a browser"
  (interactive)
  (goto-address-at-point))


;; KEYBOARD MAPPINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: when updating this list of mappings, also update the PKB

(defun prompt-for-string (function prompt &optional initial)
  ;; note: use of the INITIAL argument is discouraged, but here it makes sense
  (let ((arg (read-from-minibuffer prompt initial)))
    (if arg (funcall function arg))))

(defun prompt-for-char (function prompt)
  (let ((c (read-char prompt)))
    (if c (funcall function c))))

(defun brain-insert-attr-priority-prompt ()
  (interactive)
  (prompt-for-char 'brain-insert-attr-priority "priority = ?"))

(defun brain-insert-attr-sharability-prompt ()
  (interactive)
  (prompt-for-char 'brain-insert-attr-sharability "sharability = ?"))

(defun brain-insert-attr-weight-prompt ()
  (interactive)
  (prompt-for-char 'brain-insert-attr-weight "weight = ?"))

(defun brain-set-view-height-prompt ()
  (interactive)
  (prompt-for-char 'brain-set-view-height "height = ?"))

(defun brain-export-edges-prompt ()
  (interactive)
  (prompt-for-string 'brain-export-edges "export edges to file: " brain-default-edges-file))

(defun brain-export-graphml-prompt ()
  (interactive)
  (prompt-for-string 'brain-export-graphml "export GraphML to file: " brain-default-graphml-file))

(defun brain-export-latex-prompt ()
  (interactive)
  (prompt-for-string 'brain-export-latex "export LaTeX to file: " brain-default-latex-file))

(defun brain-export-pagerank-prompt ()
  (interactive)
  (prompt-for-string 'brain-export-pagerank "export PageRank results to file: " brain-default-pagerank-file))

(defun brain-export-rdf-prompt ()
  (interactive)
  (prompt-for-string 'brain-export-rdf "export private RDF dump to file: " brain-default-rdf-file))

(defun brain-export-vertices-prompt ()
  (interactive)
  (prompt-for-string 'brain-export-vertices "export vertices to file: " brain-default-vertices-file))

(defun brain-export-webrdf-prompt ()
  (interactive)
  (prompt-for-string 'brain-export-webrdf "export public Web RDF dump to file: " brain-default-webrdf-file))

(defun brain-import-graphml-prompt ()
  (interactive)
  (prompt-for-string 'brain-import-graphml "import GraphML from file: " brain-default-graphml-file))

(defun brain-goto-line-prompt ()
  (interactive)
  (prompt-for-string 'brain-goto-line "line: "))

(defun brain-set-min-sharability-prompt ()
  (interactive)
  (prompt-for-char 'brain-set-min-sharability "minimum sharability = ?"))

(defun brain-set-target-priority-prompt ()
  (interactive)
  (prompt-for-char 'brain-set-target-priority "new priority = ?"))

(defun brain-set-target-sharability-prompt ()
  (interactive)
  (prompt-for-char 'brain-set-target-sharability "new sharability = ?"))

(defun brain-set-target-weight-prompt ()
  (interactive)
  (prompt-for-char 'brain-set-target-weight "new weight = ?"))

(defun brain-set-value-truncation-length-prompt ()
  (interactive)
  (prompt-for-string 'brain-set-value-truncation-length "value truncation length: "))

(defun brain-set-min-weight-prompt ()
  (interactive)
  (prompt-for-char 'brain-set-min-weight "minimun weight = ?"))

(defun brain-acronym-query-prompt ()
  (interactive)
  (prompt-for-string 'brain-acronym-query "acronym search for: "))

(defun brain-shortcut-query-prompt ()
  (interactive)
  (prompt-for-string 'brain-shortcut-query "shortcut search for: "))

(defun brain-ripple-query-prompt ()
  (interactive)
  (prompt-for-string 'brain-ripple-query "ripple query: "))

(defun brain-fulltext-query-prompt ()
  (interactive)
  (let (
        (newcol (atom-color-at-visibility-threshold))
        (oldcol (face-foreground 'minibuffer-prompt)))
    (set-face-foreground 'minibuffer-prompt newcol)
    (prompt-for-string 'brain-fulltext-query "full-text search for: ")
    (set-face-foreground 'minibuffer-prompt oldcol)))

(defvar brain-mode-map nil)
(if brain-mode-map ()
  (progn
    (setq brain-mode-map (make-sparse-keymap))
    (define-key brain-mode-map (kbd "C-c C-i f")       'brain-find-isolated-atoms)
    (define-key brain-mode-map (kbd "C-c C-i r")       'brain-remove-isolated-atoms)
    (define-key brain-mode-map (kbd "C-c C-a C-p")     'brain-insert-attr-priority-prompt)
    (define-key brain-mode-map (kbd "C-c C-a C-s")     'brain-insert-attr-sharability-prompt)
    (define-key brain-mode-map (kbd "C-c C-a C-w")     'brain-insert-attr-weight-prompt)
    (define-key brain-mode-map (kbd "C-c C-a d")       'brain-insert-current-date)
    (define-key brain-mode-map (kbd "C-c C-a s")       'brain-insert-current-time-with-seconds)
    (define-key brain-mode-map (kbd "C-c C-a t")       'brain-insert-current-time)
    (define-key brain-mode-map (kbd "C-c C-d")         'brain-set-view-height-prompt)
    (define-key brain-mode-map (kbd "C-c C-e e")       'brain-export-edges-prompt)
    (define-key brain-mode-map (kbd "C-c C-e g")       'brain-export-graphml-prompt)
    (define-key brain-mode-map (kbd "C-c C-e l")       'brain-export-latex-prompt)
    (define-key brain-mode-map (kbd "C-c C-e p")       'brain-export-pagerank-prompt)
    (define-key brain-mode-map (kbd "C-c C-e r")       'brain-export-rdf-prompt)
    (define-key brain-mode-map (kbd "C-c C-e v")       'brain-export-vertices-prompt)
    (define-key brain-mode-map (kbd "C-c C-e w")       'brain-export-webrdf-prompt)
    (define-key brain-mode-map (kbd "C-c C-i g")       'brain-import-graphml-prompt)
    (define-key brain-mode-map (kbd "C-c l")           'brain-goto-line-prompt)
    (define-key brain-mode-map (kbd "C-c C-r C-b a")   (brain-visit-in-amazon 'current-root-value))
    (define-key brain-mode-map (kbd "C-c C-r C-b e")   (brain-visit-in-ebay 'current-root-value))
    (define-key brain-mode-map (kbd "C-c C-r C-b d")   (brain-visit-in-delicious 'current-root-value))
    (define-key brain-mode-map (kbd "C-c C-r C-b g")   (brain-visit-in-google 'current-root-value))
    (define-key brain-mode-map (kbd "C-c C-r C-b m")   (brain-visit-in-google-maps 'current-root-value))
    (define-key brain-mode-map (kbd "C-c C-r C-b s")   (brain-visit-in-google-scholar 'current-root-value))
    (define-key brain-mode-map (kbd "C-c C-r C-b t")   (brain-visit-in-twitter 'current-root-value))
    (define-key brain-mode-map (kbd "C-c C-r C-b w")   (brain-visit-in-wikipedia 'current-root-value))
    (define-key brain-mode-map (kbd "C-c C-r C-b y")   (brain-visit-in-youtube 'current-root-value))
    (define-key brain-mode-map (kbd "C-c C-s C-m")     'brain-set-min-sharability-prompt)
    (define-key brain-mode-map (kbd "C-c C-t C-a b")   'brain-navigate-to-target-atom-alias)
    (define-key brain-mode-map (kbd "C-c C-t C-b a")   (brain-visit-in-amazon 'current-target-value))
    (define-key brain-mode-map (kbd "C-c C-t C-b e")   (brain-visit-in-ebay 'current-target-value))
    (define-key brain-mode-map (kbd "C-c C-t C-b d")   (brain-visit-in-delicious 'current-target-value))
    (define-key brain-mode-map (kbd "C-c C-t C-b g")   (brain-visit-in-google 'current-target-value))
    (define-key brain-mode-map (kbd "C-c C-t C-b m")   (brain-visit-in-google-maps 'current-target-value))
    (define-key brain-mode-map (kbd "C-c C-t C-b s")   (brain-visit-in-google-scholar 'current-target-value))
    (define-key brain-mode-map (kbd "C-c C-t C-b t")   (brain-visit-in-twitter 'current-target-value))
    (define-key brain-mode-map (kbd "C-c C-t C-b w")   (brain-visit-in-wikipedia 'current-target-value))
    (define-key brain-mode-map (kbd "C-c C-t C-b y")   (brain-visit-in-youtube 'current-target-value))
    (define-key brain-mode-map (kbd "C-c C-t C-p")     'brain-set-target-priority-prompt)
    (define-key brain-mode-map (kbd "C-c C-t C-s")     'brain-set-target-sharability-prompt)
    (define-key brain-mode-map (kbd "C-c C-t C-w")     'brain-set-target-weight-prompt)
    (define-key brain-mode-map (kbd "C-c C-t a")       (brain-visit-as-url 'current-target-value))
    (define-key brain-mode-map (kbd "C-c v")           'brain-copy-target-value-to-clipboard)
    (define-key brain-mode-map (kbd "C-c C-t i")       (brain-atom-info 'current-target))
    (define-key brain-mode-map (kbd "C-c C-t l")       'brain-preview-target-latex-math)
    (define-key brain-mode-map (kbd "C-c r")           'brain-copy-target-reference-to-clipboard)
    (define-key brain-mode-map (kbd "C-c C-v ;")       'brain-toggle-truncate-lines)
    (define-key brain-mode-map (kbd "C-c b")           'brain-update-to-backward-view)
    (define-key brain-mode-map (kbd "C-c C-v e")       'brain-enter-edit-view)
    (define-key brain-mode-map (kbd "C-c f")           'brain-update-to-forward-view)
    (define-key brain-mode-map (kbd "C-c C-v i")       'brain-toggle-inference-viewstyle)
    (define-key brain-mode-map (kbd "C-c C-v p")       'brain-toggle-properties-view)
    (define-key brain-mode-map (kbd "C-c C-v r")       'brain-enter-readonly-view)
    (define-key brain-mode-map (kbd "C-c C-v s")       'brain-toggle-emacspeak)
    (define-key brain-mode-map (kbd "C-c C-v t")       'brain-set-value-truncation-length-prompt)
    (define-key brain-mode-map (kbd "C-c C-v v")       'brain-toggle-minimize-verbatim-blocks)
    (define-key brain-mode-map (kbd "C-c C-w C-m")     'brain-set-min-weight-prompt)
    (define-key brain-mode-map (kbd "C-c a")           'brain-acronym-query-prompt)
    (define-key brain-mode-map (kbd "C-c C-b")         'brain-visit-url-at-point)
    (define-key brain-mode-map (kbd "C-c d")           'brain-duplicates)
    (define-key brain-mode-map (kbd "C-c C-f")         'brain-find-roots)
    (define-key brain-mode-map (kbd "C-c h")           'brain-history)
    (define-key brain-mode-map (kbd "C-c i")           'brain-infer-types)
    (define-key brain-mode-map (kbd "C-c o")           'brain-shortcut-query-prompt)
    (define-key brain-mode-map (kbd "C-c P")           'brain-priorities)
    (define-key brain-mode-map (kbd "C-c p")           'brain-push-view)
    (define-key brain-mode-map (kbd "C-c C-w r")       'brain-ripple-query-prompt)
      ;; likely not the greatest shortcut -- w just stands for weird
    (define-key brain-mode-map (kbd "C-c s")           'brain-fulltext-query-prompt)
    (define-key brain-mode-map (kbd "C-c t")           'brain-navigate-to-target-atom)
    (define-key brain-mode-map (kbd "C-c u")           'brain-update-view)
    (define-key brain-mode-map (kbd "C-c C-w v")       'brain-events)))
      ;; likely not the greatest shortcut -- w just stands for weird

;; special mappings reserved for use through emacsclient
;; C-c c  --  atom-id-at-point


;; WRAPPER API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun brain-emacsclient-eval (function)
  "evaluate FUNCTION from emacsclient as if a user had typed it into the current buffer"
  (set-buffer (window-buffer (selected-window)))
  (funcall function))

(defun brain-previous-line ()
  (interactive)
  (previous-line)
  (emacspeak-speak-line))

(defun brain-next-line ()
  (interactive)
  (next-line)
  (emacspeak-speak-line))

(defun brain-backward-char ()
  (interactive)
  (backward-char)
  (emacspeak-speak-display-char t)) ;; PREFIX arg disables phonetic pronunciation

(defun brain-forward-char ()
  (interactive)
  (forward-char)
  (emacspeak-speak-display-char t)) ;; PREFIX arg disables phonetic pronunciation


;; MAJOR MODE DEFINITION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar brain-mode-syntax-table nil
  "Syntax table used while in Brain-mode.")
(if brain-mode-syntax-table ()
  (setq brain-mode-syntax-table (make-syntax-table)))

(defvar brain-mode-abbrev-table nil
  "Abbrev table used while in Brain-mode.")
(define-abbrev-table 'brain-mode-abbrev-table ())

(defun brain-mode ()
  "Major mode for interacting with a MyOtherBrain personal knowledge base"
  (interactive)
  (kill-all-local-variables)
  (use-local-map brain-mode-map)
  (brain-define-buffer-local-variables)
  (setq local-abbrev-table brain-mode-abbrev-table)
  (set-syntax-table brain-mode-syntax-table)
  ;; note: not customizing indent style with indent-line-function
  (setq mode-name "Brain-mode")
  (setq major-mode 'brain-mode)
  (run-hooks 'brain-hook))

(provide 'brain-mode)
