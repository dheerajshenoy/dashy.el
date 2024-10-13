;; -*- lexical-binding: t -*-

;; This is the source code for the program ‘dashy.el’ which is displays a minimal dashboard.
;; Created by: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; Github Repository: https://www.github.com/dheerajshenoy/dashy.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the dashy Group ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bookmark)

(defgroup dashy nil
  "Minimal Dashboard"
  :group 'extensions
  :group 'convenience
  :version "31"
  :link '(emacs-library-link :tag "Lisp File" "dashy.el"))

;;;;;;;;;;;;;;;;;;;;;;
;; Custom Variables ;;
;;;;;;;;;;;;;;;;;;;;;;

(defcustom dashy-center-vertically t
  "Center dashboard contents vertically"
  :type 'boolean
  :group 'dashy)

(defcustom dashy-center-horizontally  t
  "Center dashboard contents horizontally"
  :type 'boolean
  :group 'dashy)

;; (defcustom dashy-center-image t
;;   "Center align the image"
;;   :type 'boolean
;;   :group 'dashy)

;; (defcustom dashy-image nil
;;   "Path to an image that will be displayed in the dashboard"
;;   :type 'string
;;   :group 'dashy)

(defcustom dashy-show-title t
  "Show title in the dashy dashboard"
  :type 'boolean
  :group 'dashy)

;; (defcustom dashy-image-scale 8.0
;;   "Scale of the dashy image displayed"
;;   :type 'float
;;   :group 'dashy)

(defcustom dashy-title "Dashy"
  "Title text displayed in the dashboard"
  :type 'boolean
  :group 'dashy)

(defcustom dashy-if-no-title-show-blank-lines t
  "If ‘dashy-title’ is nil, then insert blank lines in it’s place"
  :type 'boolean
  :group 'dashy)

(defcustom dashy-show-recent-files t
  "Show recent files if t, otherwise do not display"
  :type 'boolean
  :group 'dashy)

(defcustom dashy-show-bookmarks t
  "Show bookmarks if t, otherwise do not display"
  :type 'boolean
  :group 'dashy)

(defcustom dashy-linkify-recent-files t
  "Add links to recent files if t"
  :type 'boolean
  :group 'dashy)

(defcustom dashy-num-recent-files 5
  "Number of recent files to display. If -1 display all recent files"
  :type 'int
  :group 'dashy)

(defcustom dashy-num-bookmarks 5
  "Number of bookmarks to display. If -1 display all bookmarks"
  :type 'int
  :group 'dashy)

;; (defcustom dashy-show-image t
;;   "Show image if t"
;;   :type 'boolean
;;   :group 'dashy)

;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;

(defvar-local dashy--buffer-name "*Dashy*"
  "Dashy dashboard buffer name")

;;;;;;;;;;;
;; Faces ;;
;;;;;;;;;;;

(defface dashy-title-face
  '((t :weight bold :height 2.0 :foreground "pink"))
  "Face for the dashy title"
  :group 'dashy)

(defface dashy-header-face
  '((t :weight bold :italic t :foreground "pink"))
  "Face for the dashy category headers"
  :group 'dashy)

(defface dashy-recent-files-link-face
  '((t :weight bold :italic t :foreground "light blue" :underline t))
  "Face for the recent files link"
  :group 'dashy)

(defface dashy-recent-files-mouse-hover-face
  '((t :weight bold :italic t :foreground "blue" :underline t))
  "Face for the recent files link mouse hover"
  :group 'dashy)

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

(defun dashy--get-title-text ()
  "Return the dashy title"
  (format "%s\n\n" dashy-title))

(defun dashy--get-dashy-buffer-name ()
  "Return the dashy buffer name"
  (format "%s" dashy--buffer-name))

(defun dashy--create-link (str link)
  "Return a link STR which points to the link LINK"
  (when dashy-linkify-recent-files
    (propertize link
                'face 'dashy-recent-files-link-face
                'mouse-face 'dashy-recent-files-mouse-hover-face
                'help-echo (format "Click to open %s" link)
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map [mouse-1] (lambda () (interactive)
                                                      (find-file link)))
                          (define-key map [return] (lambda () (interactive)
                                                     (find-file link)))
                          map))))

(defun dashy--insert-header (text)
  "Returns a formatted text for header that takes in the text TEXT"
  (let* ((header (propertize (format "%s\n\n" text) 'face 'dashy-header-face)))
    (if dashy-center-horizontally
        (dashy--insert-center header)
      (insert header))))

(defun dashy--insert-title ()
  "Insert the dashy title into the dashy dashboard"
  (if dashy-show-title
      (let* ((title (propertize (dashy--get-title-text) 'face 'dashy-title-face)))
        (if dashy-center-horizontally
            (dashy--insert-center title)
          (insert title)))
    (if dashy-if-no-title-show-blank-lines (insert "\n\n"))))

(defun dashy--insert-recent-files ()
  "Insert recent files into the dashy dashboard"
  (if dashy-show-recent-files
      (progn
        (dashy--insert-header "Recent Files")
        (if recentf-list
            (let* ((files (if (equal dashy-num-recent-files -1)
                              recentf-list
                            (seq-take recentf-list dashy-num-recent-files))))
              (if dashy-center-horizontally
                  (dolist (file files)
                    (dashy--insert-center (dashy--create-link file file))
                    (insert "\n"))
                (dolist (file files)
                  (insert (dashy--create-link file file))
                  (insert "\n"))))
          (insert "No recent files\n"))
        (insert "\n"))))

(defun dashy--insert-bookmarks ()
  "Insert bookmarks into the dashy dashboard"
  (if dashy-show-bookmarks
      (progn
        (dashy--insert-header "Bookmarks")
        (if dashy-center-horizontally
            (dolist (bookmark bookmark-alist)
              (let ((filename (cdr (assoc 'filename (cdr bookmark))))
                    (bookmark-name (car bookmark)))
                (dashy--insert-center (dashy--create-link bookmark-name filename))
                (insert "\n")))
          (dolist (bookmark bookmark-alist)
            (let ((filename (cdr (assoc 'filename (cdr bookmark))))
                  (bookmark-name (car bookmark)))
              (insert (dashy--create-link bookmark-name filename))
              (insert "\n")))))
        (insert "No bookmarks found.")))

(defun dashy--str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

(defun dashy--find-max-width (start end)
  "Return the max width within the region START and END."
  (save-excursion
    (goto-char start)
    (let ((width 0))
      (while (< (point) end)
        (let* ((line-str (buffer-substring (line-beginning-position) (line-end-position)))
               (line-length (dashy--str-len line-str)))
          (setq width (max width line-length)))
        (forward-line 1))
      width)))

(defun dashy--center-text (start end)
  "Center the text between START and END."
  (let* ((width (dashy--find-max-width start end))
         (prefix (propertize " " 'display `(space . (:align-to (- center ,(/ (float width) 2)))))))
    (add-text-properties start end `(line-prefix ,prefix indent-prefix ,prefix))))

(defun dashy--insert-center (&rest strings)
  "Insert STRINGS in the center of the buffer."
  (let ((start (point)))
    (apply #'insert strings)
    (dashy--center-text start (point))))

;; (defun dashy--insert-image ()
;;   "Insert image into dashboard"
;;   (if dashy-show-image
;;       (if (and (> (length dashy-image) 0) (file-exists-p dashy-image))
;;           (let* ((img (create-image dashy-image nil nil :scale dashy-image-scale))
;;                  (line-prefix
;;                   `(space . (:align-to (- center ,(/ 100 2))))))
;;             (insert-image img)
;;             (insert "\n\n")))))

(defun dashy-contents ()
  "Function that defines the content of the dashboard"
  ;; Image

  ;; (dashy--insert-image)

  ;; Title
  (dashy--insert-title)

  ;; Recent Files
  (dashy--insert-recent-files)

  ;; Bookmarks
  (dashy--insert-bookmarks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dashy-show ()
  "Create a simple dashboard with useful information."
  (interactive)
  (let ((buf (get-buffer-create (dashy--get-dashy-buffer-name))))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (dashy-contents)
      (read-only-mode 1)
      (goto-char (point-min)))
    (switch-to-buffer buf)))

(provide 'dashy)