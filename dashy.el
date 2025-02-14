;;; dashy.el --- Minimal & Customizable Dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Dheeraj Vittal Shenoy

;; Author: Dheeraj Vittal Shenoy <dheerajshenoy22@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29"))
;; Keywords: startup, screen, tools, dashboard
;; URL: https://github.com/dheerajshenoy/dashy.el

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Minimal & Customizable startup screen.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the dashy Group ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bookmark)

(defgroup dashy nil
  "Minimal & Customizable Dashboard"
  :group 'applications
  :prefix "dashy-")

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major Mode Definition ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode dashy-mode
  special-mode "Dashy"
  (when (fboundp #'cursor-face-highlight-mode)
    (setq cursor-type nil)
    (cursor-face-highlight-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;
;; Custom Variables ;;
;;;;;;;;;;;;;;;;;;;;;;

(defcustom dashy-no-evil-bindings t
  "If set to true, make emacs state the default state on init"
  :type 'boolean
  :group 'dashy)

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
  :type 'string
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

;;;;;;;;;;;
;; Hooks ;;
;;;;;;;;;;;

(defcustom dashy-after-show-hook nil
  "Hook run after showing dashy dashboard"
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;; ;;
;; ;; Variables ;; ;;
;; ;;;;;;;;;;;;;;; ;;
;;;;;;;;;;;;;;;;;;;;;

(defvar dashy--buffer-name "*Dashy*"
  "Dashy dashboard buffer name")

;;;;;;;;;;;
;; Faces ;;
;;;;;;;;;;;

(defface dashy-title-face
  '((t :weight bold :height 2.0 :foreground "pink"))
  "Face for the dashy title"
  :group 'dashy)

(defface dashy-item-face
  '((t :weight bold :height 1.0 :underline t))
  "Face for the dashboard items")

(defface dashy-header-face
  '((t :weight bold :italic t :foreground "pink"))
  "Face for the dashy category headers"
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

(defun dashy--insert-item-button (text func)
  "Inserts a text button with text TEXT and callback function FUNC"
  (insert-button text
                 'action func
                 'follow-link t
                 'face 'dashy-item-face))

(defun dashy--insert-header (text)
  "Returns a formatted text for header that takes in the text TEXT"
  (let* ((header (propertize text 'face 'dashy-header-face))
         (start (point)))
    (insert header)
    (put-text-property start (point) 'header "t" )
    (insert "\n\n")
    ))

(defun dashy--insert-title ()
  "Insert the dashy title into the dashy dashboard"
  (if dashy-show-title
      (let* ((title (propertize (dashy--get-title-text) 'face 'dashy-title-face)))
        (insert title))
    (if dashy-if-no-title-show-blank-lines (insert "\n\n"))))

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
              (dolist (file files)
                (dashy--insert-item-button file #'(lambda (x) (find-file file)))
                (insert "\n")
                ))
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
              (dolist (bookmark bookmarks)
                (dashy--insert-item-button
                 (if dashy-bookmark-show-file-path
                     (format "%s (%s)" (car bookmark) (car bookmark))
                   (car bookmark))
                 #'(lambda (x) (bookmark-jump (car bookmark))))
                (insert "\n"))
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

;;;###autoload
(defun dashy-show ()
  "Create a simple dashboard with useful information."
  (interactive)
  (let ((buf (get-buffer-create (dashy--get-dashy-buffer-name))))
    (with-current-buffer buf
      (unless (derived-mode-p 'dashy-mode)
        (dashy-mode))
      (read-only-mode -1)
      (erase-buffer)
      (dashy-contents)
      (read-only-mode 1))
    (switch-to-buffer buf)
    (run-hooks 'dashy-after-show-hook)
    (dashy-goto-next-item)))

(defun dashy-goto-next-item ()
  "Goto the next item in the dashboard"
  (interactive)
  (unless (forward-button 1 nil nil t)
    (goto-char (point-min))
    (dashy-goto-next-item)))

(defun dashy-goto-prev-item ()
  "Goto the previous item in the dashboard"
  (interactive)
  (unless (backward-button 1 nil nil t)
    (goto-char (point-max))
    (dashy-goto-prev-item)))

(defun dashy-goto-next-header ()
  "Go to the next header in the dashboard, skipping the current header if present."
  (interactive)
  (when (text-property-search-forward
         'header (get-text-property (point) 'header)
         (lambda (val prop) (and prop (not (eq val prop)))))))

(defun dashy-goto-prev-header ()
  "Goto the prev header in the dashboard"
  (interactive)
  (when (text-property-search-backward
         'header (get-text-property (point) 'header)
         (lambda (val prop) (and prop (not (eq val prop)))))))

;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;

(defvar-keymap dashy-mode-map
  "n" #'dashy-goto-next-item
  "p" #'dashy-goto-prev-item
  "N" #'dashy-goto-next-header
  "P" #'dashy-goto-prev-header)

(provide 'dashy)
