;;; org-book-capture.el --- Capture book metadata into Org files via Google Books API -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Igor Zhirkov
;;
;; Author: Igor Zhirkov <igorjirkov@gmail.com>
;; Maintainer: Igor Zhirkov <igorjirkov@gmail.com>
;; Created: December 07, 2025
;; Modified: December 07, 2025
;; Version: 0.0.1
;; Keywords: convenience org bib
;; Homepage: https://github.com/sayon/org-book-capture
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; Capture org-book information into an org file with fuzzy search through Google API.
;;
;;  Description
;;  Allows capturing and conveniently storing information about physical or digital org-books that you own.
;;
;;   Example output:
;;   #+begin_src elisp
;;  * Discourse on Method and Meditations on First Philosophy :physical:philosophy:
;; :PROPERTIES:
;; :ID:       656E30CC-8261-4A1A-98EC-2146E79E4EE6
;; :AUTHOR:   Rene Descartes
;; :ADDED:    [2025-12-07]
;; :AMAZON:   https://www.amazon.com/dp/9781603844482
;; :ISBN:     9781603844482
;; :PHYSICAL: true
;; :END:
;;  #+end_src

;;
;;; Code:


(require 'org)
(require 'org-capture)
(require 'url)
(require 'json)
(require 'cl-lib)

(defgroup org-book-capture nil
  "Capture org-book information into an org library file with fuzzy search through Google API"
  :group 'convenience)

(defcustom org-book-capture-library-file "~/org/library.org"
  "Path to your library file."
  :type 'file
  :group 'org-book-capture)

(defcustom org-book-capture-default-physical t
  "Whether books that you are adding are physical or digital by default."
  :type 'boolean
  :group 'org-book-capture)

(defvar org-book-capture--last-results nil
  "Cache of last search results.")

(defun org-book-capture--url-encode (string)
  "URL encode STRING."
  (url-hexify-string string))

(defun org-book-capture--fetch-json (url)
  "Fetch JSON from URL synchronously."
  (with-current-buffer (url-retrieve-synchronously url t nil 10)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point-min) (point))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol))
      (json-read))))

(defun org-book-capture--search-google-books (query)
  "Search Google org-books API with QUERY."
  (let* ((encoded-query (org-book-capture--url-encode query))
         (url (format "https://www.googleapis.com/books/v1/volumes?q=%s&maxResults=10"
                      encoded-query)))
    (condition-case err
        (when-let ((response (org-book-capture--fetch-json url)))
          (when (> (alist-get 'totalItems response 0) 0)
            (alist-get 'items response)))
      (error
       (message "Error fetching from Google org-books: %s" err)
       nil))))

(defun org-book-capture--parse-book-result (item)
  "Parse a single org-book result ITEM from Google org-books API."
  (when-let* ((volume-info (alist-get 'volumeInfo item))
              (title (alist-get 'title volume-info)))
    (let* ((authors (alist-get 'authors volume-info))
           (author-string (if authors (mapconcat #'identity authors ", ") ""))
           (categories (alist-get 'categories volume-info))
           (language (alist-get 'language volume-info "en"))
           (identifiers (alist-get 'industryIdentifiers volume-info))
           (isbn (or (org-book-capture--extract-isbn identifiers "ISBN_13")
                     (org-book-capture--extract-isbn identifiers "ISBN_10")
                     "")))
      (list :title title
            :author author-string
            :isbn isbn
            :categories categories
            :language language))))

(defun org-book-capture--extract-isbn (identifiers type)
  "Extract ISBN of TYPE from IDENTIFIERS list."
  (when identifiers
    (cl-loop for id in identifiers
             when (equal (alist-get 'type id) type)
             return (alist-get 'identifier id))))

(defun org-book-capture--format-result-for-selection (parsed-book index)
  "Format PARSED-BOOK at INDEX for selection."
  (let ((title (plist-get parsed-book :title))
        (author (plist-get parsed-book :author))
        (isbn (plist-get parsed-book :isbn)))
    (format "%d. %s - %s%s"
            index
            title
            author
            (if (and isbn (not (string-empty-p isbn)))
                (format " [ISBN: %s]" isbn)
              ""))))

(defun org-book-capture--detect-language-name (lang-code)
  "Convert LANG-CODE to full language name."
  (pcase lang-code
    ("ru" "Russian")
    ("fr" "French")
    ("de" "German")
    ("pl" "Polish")
    ("el" "Greek")
    ("la" "Latin")
    (_ nil)))

(defun org-book-capture--generate-isbn-link (isbn)
  "Generate ISBN link from ISBN string.
Returns URL string or empty string if ISBN is invalid."
  (when (and isbn (not (string-empty-p isbn)))
    (format "https://isbnsearch.org/isbn/%s" isbn)))

(defun org-book-capture--categories-to-tags (categories)
  "Convert CATEGORIES list to org tags."
  (when categories
    (mapcar (lambda (cat)
              (downcase (replace-regexp-in-string " " "_" cat)))
            categories)))

(defun org-book-capture--generate-uuid ()
  "Generate a UUID for org ID property."
  (upcase (org-id-uuid)))

(defun org-book-capture--check-duplicate (title)
  "Check if TITLE already exists in library file."
  (when (file-exists-p org-book-capture-library-file)
    (with-temp-buffer
      (insert-file-contents org-book-capture-library-file)
      (goto-char (point-min))
      (let ((title-lower (downcase title)))
        (catch 'found
          (while (re-search-forward "^\\*+\\s-+\\(.+?\\)\\(?:\\s-+:[[:word:]_:]+:\\)?\\s-*$" nil t)
            (let ((existing-title (match-string 1)))
              (when (equal (downcase existing-title) title-lower)
                (throw 'found t))))
          nil)))))

(defun org-book-capture--insert-book-entry (org-book-data)
  "Insert ORG-BOOK-DATA into library file."
  (let* ((title (plist-get org-book-data :title))
         (author (plist-get org-book-data :author))
         (isbn (plist-get org-book-data :isbn))
         (categories (plist-get org-book-data :categories))
         (language (plist-get org-book-data :language))
         (is-physical (plist-get org-book-data :physical))
         (file-path (plist-get org-book-data :file-path))
         (format-type (plist-get org-book-data :format))
         (language-name (org-book-capture--detect-language-name language))
         (tags (org-book-capture--categories-to-tags categories))
         (isbn-link (org-book-capture--generate-isbn-link isbn))
         (uuid (org-book-capture--generate-uuid)))

    ;; Add physical tag if applicable
    (when is-physical
      (push "physical" tags))

    (with-current-buffer (find-file-noselect org-book-capture-library-file)
      (goto-char (point-max))
      ;; Insert the entry
      (unless (bolp) (insert "\n"))
      (insert (format "* %s%s\n"
                      title
                      (if tags
                          (format " :%s:" (mapconcat #'identity tags ":"))
                        ""))
              ":PROPERTIES:\n"
              (format ":ID:       %s\n" uuid)
              (if (and author (not (string-empty-p author)))
                  (format ":AUTHOR:   %s\n" author)
                "")
              (format ":ADDED:    [%s]\n" (format-time-string "%Y-%m-%d"))
              (if (and isbn-link (not (string-empty-p isbn)))
                  (format ":ISBN-LINK:   %s\n:ISBN:        %s\n" isbn-link isbn)
                "")
              (if is-physical
                  ":PHYSICAL: true\n"
                "")
              (if (and format-type (not (string-empty-p format-type)))
                  (format ":FORMAT:   %s\n" format-type)
                "")
              (if (and language-name (not (equal language "en")))
                  (format ":LANGUAGE: %s\n" language-name)
                "")
              (if (and file-path (not (string-empty-p file-path)))
                  (format ":link:     [[file:%s][ebook]]\n" file-path)
                "")
              ":END:\n\n")

      (save-buffer)
      (message "Added: %s" title))))

;;;###autoload
(defun org-book-capture-add-book (query)
  "Add a org-book to library with metadata lookup.
QUERY can be title, author, or both (e.g., 'Dostoevsky Crime')."
  (interactive "sBook title/author (fuzzy): ")

  (when (string-empty-p (string-trim query))
    (user-error "Query cannot be empty"))

  (message "Searching for: %s..." query)
  (let ((results (org-book-capture--search-google-books query)))
    (if (not results)
        (message "No results found for: %s" query)
      (setq org-book-capture--last-results
            (mapcar #'org-book-capture--parse-book-result results))

      ;; Display results for selection
      (let* ((choices (cl-loop for org-book in org-book-capture--last-results
                               for i from 1
                               collect (org-book-capture--format-result-for-selection org-book i)))
             (selection (completing-read "Select org-book: " choices nil t)))

        (when selection
          (let* ((index (1- (string-to-number (car (split-string selection "\\.")))))
                 (selected-book (nth index org-book-capture--last-results))
                 (title (plist-get selected-book :title)))

            ;; Check for duplicates
            (if (org-book-capture--check-duplicate title)
                (message "Book already exists: %s" title)

              ;; Ask if physical or digital
              (let ((is-physical (y-or-n-p "Is this a physical org-book? ")))
                (plist-put selected-book :physical is-physical)

                ;; If digital, ask for file path
                (unless is-physical
                  (let ((file-path (read-file-name "Path to ebook file (optional): " nil "" t)))
                    (when (and file-path (not (string-empty-p file-path)))
                      (plist-put selected-book :file-path file-path)
                      ;; Detect format from file extension
                      (let ((ext (upcase (file-name-extension file-path))))
                        (plist-put selected-book :format ext)))))

                ;; Insert the entry
                (org-book-capture--insert-book-entry selected-book)))))))))

;;;###autoload
(defun org-book-capture-add-book-quick (title author)
  "Quick add org-book with TITLE and AUTHOR without file path prompt."
  (interactive "sTitle: \nsAuthor: ")

  (let ((query (format "%s %s" title author)))
    (message "Searching for: %s..." query)
    (let ((results (org-book-capture--search-google-books query)))
      (if (not results)
          (message "No results found. Adding manually...")
        (setq org-book-capture--last-results
              (mapcar #'org-book-capture--parse-book-result results))

        ;; Display results
        (let* ((choices (cl-loop for org-book in org-book-capture--last-results
                                 for i from 1
                                 collect (org-book-capture--format-result-for-selection org-book i)))
               (selection (completing-read "Select org-book: " choices nil t)))

          (when selection
            (let* ((index (1- (string-to-number (car (split-string selection "\\.")))))
                   (selected-book (nth index org-book-capture--last-results))
                   (book-title (plist-get selected-book :title)))

              (if (org-book-capture--check-duplicate book-title)
                  (message "Book already exists: %s" book-title)

                ;; Assume physical by default
                (plist-put selected-book :physical org-book-capture-default-physical)
                (org-book-capture--insert-book-entry selected-book)))))))))

;;;###autoload
(defun org-book-capture-setup-org-capture ()
  "Setup org-capture template for org-book entry."
  (interactive)
  (add-to-list 'org-capture-templates
               '("b" "Book" plain
                 (file org-book-capture-library-file)
                 "* %(org-book-capture--prompt-and-return)"
                 :empty-lines 1
                 :immediate-finish t)))

(defun org-book-capture--prompt-and-return ()
  "Prompt for org-book and return formatted entry."
  (let ((query (read-string "Book title/author: ")))
    (org-book-capture-add-book query)
    ""))

(provide 'org-book-capture)
;;; org-book-capture.el ends here
