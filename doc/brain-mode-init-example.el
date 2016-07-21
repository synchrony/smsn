;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs environment (essential)

;; add the directory containing Brain-mode and other libs
(let ((default-directory "~/.emacs.d/elisp/"))
      (normal-top-level-add-subdirs-to-load-path))

;; load the library
(require 'brain-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rexster URL and MyOtherBrain graph (essential)

(defvar smsn-rexster-url "http://localhost:8182")
(defvar smsn-rexster-graph "joshkb")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data importing and exporting

;; export the graph to here, or populate an empty graph from here
(defvar smsn-default-graphml-file "/Volumes/encrypted/personal-git-repo/joshkb.xml")

;; RDF inference over entire graph is written here
(defvar smsn-default-rdf-file "/tmp/joshkb.nt")

;; RDF inference over the public portion of the graph is written here
;; using Dropbox and Apache+.htaccess allows it to be automatically published as Linked Data
(defvar smsn-default-webrdf-file "/Users/josh/Dropbox/shared/domains/www.fortytwo.net/people/josh/myotherbrain.rdf")

;; default location for dumps of tab-separated vertex and edge files
;; vertex files contain the properties of each atom
;; edge files are parent/child adjacency lists
(defvar smsn-default-vertices-file "/tmp/joshkb-vertices.tsv")
(defvar smsn-default-edges-file "/tmp/joshkb-edges.tsv")

;; default location for PageRank results
(defvar smsn-default-pagerank-file "/tmp/joshkb-pagerank.tsv")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other settings useful with Brain-mode

;; tabs as 4-spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq-default truncate-lines t)
(if full-colors-supported
    (global-hl-line-mode 1))

