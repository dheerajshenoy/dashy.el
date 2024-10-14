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

(define-derived-mode dashy-mode fundamental-mode "Dashy"
  "Toggle dashy-mode.
Interactively with no argument, this command toggles the dashboard mode.

When Dashy mode is enabled, the 'C-n' key goes to the next link item and 'C-p' goes to the
previous item. 'C-{' goes to the previous header and 'C-}' goes to the next header.
See the commands \\[dashy-goto-next-item] \\[dashy-goto-prev-item]
\\[dashy-goto-prev-header] and \\[dashy-goto-next-header]."
  ;; The initial value
  nil
  ;; The indicator for the modeline
  " Dashy")


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

(defcustom dashy-bookmark-show-file-path t
  "Show filepath of the associated bookmark file"
  :type 'boolean
  :group 'dashy)

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
  "Add links to files if t"
  :type 'boolean
  :group 'dashy)

(defcustom dashy-linkify-bookmarks t
  "Add links to bookmarks if t"
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

(defcustom dashy-components '(title recentfiles bookmarks)
  "Components that are displayed in the dashboard"
  :type '(repeat symbol)
  :group 'dashy)

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
  '((t :weight bold :italic t :foreground "light blue" :underline nil))
  "Face for the recent files link"
  :group 'dashy)

(defface dashy-recent-files-mouse-hover-face
  '((t :weight bold :italic t :foreground "blue" :underline t))
  "Face for the recent files link mouse hover"
  :group 'dashy)

(defface dashy-bookmark-link-face
  '((t :weight bold :italic t :foreground "light blue" :underline nil))
  "Face for the bookmark link"
  :group 'dashy)

(defface dashy-bookmark-mouse-hover-face
  '((t :weight bold :italic t :foreground "blue" :underline t))
  "Face for the bookmark link mouse hover"
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

(defun dashy--create-recent-file-link (str link &optional func)
  "Return a link STR which points to the link LINK"
  (when dashy-linkify-recent-files
    (propertize str
                'face 'dashy-recent-files-link-face
                'mouse-face 'dashy-recent-files-mouse-hover-face
                'help-echo (format "Click to open %s" link)
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map [mouse-1] (lambda () (interactive)
                                                      (if func
                                                          (funcall func link)
                                                        (find-file link))))
                          (define-key map [return] (lambda () (interactive)
                                                     (if func
                                                         (funcall func link)
                                                       (find-file link))))
                          map))))

(defun dashy--create-bookmark-link (str link &optional func)
  "Return a link STR which points to the link LINK"
  (when dashy-linkify-bookmarks
    (propertize str
                'face 'dashy-bookmark-link-face
                'mouse-face 'dashy-bookmark-mouse-hover-face
                'help-echo (format "Click to open %s" link)
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map [mouse-1] (lambda () (interactive)
                                                      (if func
                                                          (funcall func link)
                                                        (find-file link))))
                          (define-key map [return] (lambda () (interactive)
                                                     (if func
                                                         (funcall func link)
                                                       (find-file link))))
                          map))))

(defun dashy--insert-header (text)
  "Returns a formatted text for header that takes in the text TEXT"
  (let* ((header (propertize text 'face 'dashy-header-face))
         (start (point)))
    (if dashy-center-horizontally
        (dashy--insert-center header)
      (insert header))
    (put-text-property start (point) 'header "t" )
    (insert "\n\n")
    ))

(defun dashy--insert-title ()
  "Insert the dashy title into the dashy dashboard"
  (if dashy-show-title
      (let* ((title (propertize (dashy--get-title-text) 'face 'dashy-title-face)))
        (if dashy-center-horizontally
            (dashy--insert-center title)
          (insert title)))
    (if dashy-if-no-title-show-blank-lines (insert "\n\n"))))

(defun dashy--add-item-property (object start end)
  "Add item text-property for the OBJECT from START to END"
  (put-text-property start end 'item "t"))

(defun dashy--insert-recent-files ()
  "Insert recent files into the dashy dashboard"
  (if dashy-show-recent-files
      (progn
        (dashy--insert-header "Recent Files")
        (if recentf-list
            (let* ((files (if (equal dashy-num-recent-files -1)
                              recentf-list
                            (seq-take recentf-list dashy-num-recent-files)))
                   start)
              (if dashy-center-horizontally
                  (dolist (file files)
                    (setq-local start (point))
                    (dashy--add-item-property (dashy--insert-center (dashy--create-recent-file-link file file)) start (point))
                    (insert "\n"))
                (dolist (file files)
                  (insert (dashy--create-recent-file-link file file))
                  (insert "\n"))))
          (insert "No recent files\n"))
        (insert "\n"))))

(defun dashy--get-bookmarks-with-locations ()
  "Return a list of bookmarks with their locations."
  (bookmark-maybe-load-default-file) ;; Ensure bookmarks are loaded
  (mapcar (lambda (bookmark)
            (let* ((bm (bookmark-get-bookmark bookmark))
                   (name (bookmark-name-from-full-record bm))
                   (location (bookmark-get-filename bm)))
              (cons name location)))
          bookmark-alist))

(defun dashy--insert-bookmarks ()
  "Insert bookmarks into the dashy dashboard"
  (if dashy-show-bookmarks
      (progn
        (dashy--insert-header "Bookmarks")
        (let ((bookmarks (dashy--get-bookmarks-with-locations)))
          (if bookmarks
              (if dashy-center-horizontally
                  (dolist (bookmark bookmarks)
                    (dashy--insert-center (dashy--create-bookmark-link
                                           (if dashy-bookmark-show-file-path
                                               (format "%s (%s)" (car bookmark) (cdr bookmark))
                                             (car bookmark))
                                           (car bookmark) #'bookmark-jump))
                    (insert "\n"))
                (dolist (bookmark bookmarks)
                  (insert (dashy--create-bookmark-link
                           (if dashy-bookmark-show-file-path
                               (format "%s (%s)" (car bookmark) (cdr bookmark))
                             (car bookmark))
                           (car bookmark) #'bookmark-jump))
                  (insert "\n")))
            (insert "No bookmarks found."))))))


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
  (dolist (component dashy-components)
    (cond ((equal component 'title) (dashy--insert-title))
          ((equal component 'recentfiles) (dashy--insert-recent-files))
          ((equal component 'bookmarks) (dashy--insert-bookmarks)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dashy-show ()
  "Create a simple dashboard with useful information."
  (interactive)
  (dashy-mode)
  (let ((buf (get-buffer-create (dashy--get-dashy-buffer-name))))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (dashy-contents)
      (read-only-mode 1)
      (goto-char (point-min)))
    (switch-to-buffer buf)))

(defun dashy-goto-next-item ()
  "Goto the next item in the dashboard"
  (interactive)
  (let ((pos (next-single-property-change (point) 'item)))
    (if pos
        (goto-char pos))))

(defun dashy-goto-prev-item ()
  "Goto the previous item in the dashboard"
  (interactive)
  ())

(defun dashy-goto-next-header ()
  "Goto the next header in the dashboard"
  (interactive)
  (let ((pos (next-single-property-change (point) 'header)))
    (if pos
        (goto-char pos))))

(defun dashy-goto-prev-header ()
  "Goto the prev header in the dashboard"
  (interactive)
  (let ((pos (next-single-property-change (point) 'header)))
    (if pos
        (goto-char pos))))

(define-key dashy-mode-map (kbd "C-n") 'dashy-goto-next-item)
(define-key dashy-mode-map (kbd "C-p") 'dashy-goto-prev-item)

;; Evil mode support
(if (bound-and-true-p evil-mode)
    (progn
      (define-key dashy-mode-map (kbd "j") 'dashy-goto-next-item)
      (define-key dashy-mode-map (kbd "k") 'dashy-goto-prev-item)))

(provide 'dashy)