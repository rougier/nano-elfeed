;;; nano-elfeed.el --- NANO elfeed -*- lexical-binding: t -*-

;; Copyright (C) 2024 Nicolas P. Rougier
;;
;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/nano-elfeed
;; Keywords: news
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (elfeed) (nano-theme) (relative-date))

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
(require 's)
(require 'stripes)
(require 'hl-line)
(require 'relative-date)
(require 'elfeed)
(require 'elfeed-org)
(require 'nano-theme)

(defconst nano-elfeed--rss-icon-data
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<svg xmlns=\"http://www.w3.org/2000/svg\" id=\"RSSicon\" viewBox=\"0 0 8 8\" width=\"256\" height=\"256\">
  <title>RSS feed icon</title>
  <style type=\"text/css\">
    .button {stroke: none; fill: %s;}
    .symbol {stroke: none; fill: white;}
  </style>
  <rect class=\"button\" width=\"8\" height=\"8\" rx=\"1.5\"/>
  <circle class=\"symbol\" cx=\"2\" cy=\"6\" r=\"1\"/>
  <path class=\"symbol\" d=\"m 1,4 a 3,3 0 0 1 3,3 h 1 a 4,4 0 0 0 -4,-4 z\"/>
  <path class=\"symbol\" d=\"m 1,2 a 5,5 0 0 1 5,5 h 1 a 6,6 0 0 0 -6,-6 z\"/>
</svg>")

(defun nano-elfeed--make-icon (image)
  (let* ((img-width (car (image-size image t)))
         (img-height (cdr (image-size image t)))
         (ch (frame-char-height))
         (cw (frame-char-width))
         (icon-height (+ 0 (* 2 ch)))
         (char-width  (+ 1 (truncate (/ (* 2 ch) cw))))
         (icon-width  (* char-width cw))
         (scale (/ (float icon-height) (float img-height)))
         (scaled-width (truncate (* scale img-width)))
         (scaled-height (truncate (* scale img-height)))
         (icon-true-width (truncate (* img-width scale)))
         (margin (max 0 (- icon-width icon-true-width)))
         (icon-width (+ icon-width (% margin 2)))
         (margin (- margin (% margin 2)))
         (thumbnail (cons (car image) (cl-copy-list (cdr image)))))
    (plist-put (cdr thumbnail) :height scaled-height)
    (plist-put (cdr thumbnail) :width  scaled-width)
    (plist-put (cdr thumbnail) :margin (cons (/ margin 2) 0))
    (plist-put (cdr thumbnail) :ascent 80)
    (cons (propertize (make-string char-width ?-)
                                  'display (list (list 'slice 0  0 icon-width ch) thumbnail)
                                  'line-height t)
          (propertize (make-string char-width ?-)
                                  'display (list (list 'slice 0  ch icon-width ch) thumbnail)
                                  'line-height t))))

(defvar nano-elfeed-icon-path "~/Documents/GitHub/nano-elfeed/icons")

(defun nano-elfeed-make-icon (name)
  (let* ((image-unread (create-image (format "%s/%s-unread.svg" nano-elfeed-icon-path name)))
         (image-unread (nano-elfeed--make-icon image-unread))
         (image-read (create-image (format "%s/%s-read.svg" nano-elfeed-icon-path name)))
         (image-read (nano-elfeed--make-icon image-read)))
    (cons image-unread image-read)))

(defvar nano-elfeed-icons
  `(("RSS"             . ,(nano-elfeed-make-icon "rss"))
    ("Aeon"            . ,(nano-elfeed-make-icon "aeon"))
    ("Emacs"           . ,(nano-elfeed-make-icon "reddit"))
    ("Emacs org-mode"  . ,(nano-elfeed-make-icon "reddit"))
    ("Slashdot"        . ,(nano-elfeed-make-icon "slashdot"))
    ("Ars Technica"    . ,(nano-elfeed-make-icon "ars-technica"))
    ("Boing Boing"     . ,(nano-elfeed-make-icon "boing-boing"))
    ("eLife"           . ,(nano-elfeed-make-icon "elife"))
    ("Plos Comp.Bio"   . ,(nano-elfeed-make-icon "plos"))
    ("Quanta"          . ,(nano-elfeed-make-icon "quanta"))
    ("BioRxiv Neuro"   . ,(nano-elfeed-make-icon "biorxiv"))))

(defun nano-elfeed-get-icon (name unread)
  (let ((icon (alist-get (s-trim name) nano-elfeed-icons nil nil #'equal)))
    (when icon (if unread (car icon) (cdr icon)))))
  
(defvar nano-elfeed-rss-icon-active
      (nano-elfeed--make-icon
       (create-image (format nano-elfeed--rss-icon-data "orange") 'svg t)))

(defvar nano-elfeed-rss-icon-inactive
      (nano-elfeed--make-icon
       (create-image (format nano-elfeed--rss-icon-data "#90A4AE") 'svg t)))

(defun nano-elfeed-entry (title subtitle date unread &optional no-newline)
  (let* ((icon (or (nano-elfeed-get-icon title unread)
                   (if unread
                       nano-elfeed-rss-icon-active
                     nano-elfeed-rss-icon-inactive)))
         (date (relative-date date))
         (subtitle (s-truncate (truncate (* 1 (window-width))) subtitle "…"))
         (foreground-color (if unread
                               (face-foreground 'default)
                             (face-foreground 'font-lock-comment-face nil t)))
         (background-color (face-background 'highlight))
         (border-color     (face-background 'default))
         (face-upper    `(:foreground ,foreground-color
                          :background ,background-color
                          :overline ,border-color))
         (face-title    `(:foreground ,foreground-color
                          :background ,background-color
                          :weight ,(face-attribute 'bold :weight)
                          :overline ,border-color))
         (face-subtitle `(:foreground ,foreground-color
                          :background ,background-color
                          :family "Roboto Condensed"
                          :height 160
                          :underline nil
;;                                     (:color ,border-color
;;                                      :style line
;;                                      :position t)
                          ))
         (face-lower    `(:foreground ,foreground-color
                          :background ,background-color
                          :underline nil
;;                                     (:color ,border-color
;;                                      :style line
;;                                      :position t)
                          )))
    (insert (concat
     ;; Upper part
;;     (propertize " " 'face `(:background ,border-color)
;;                     'display '((raise 0.5) (space :width (1))))
     (propertize " " 'face face-upper 'display '(raise 0.5))
     (propertize (car icon) 'face face-upper)
     (propertize " " 'face face-upper)
     (propertize title 'face face-title 'elfeed-entry t)
     (propertize " " 'face face-upper
                     'display `(space :align-to (- right ,(length date) 2)))
     (propertize date 'face face-upper)
     (propertize " " 'face face-upper
                     'display '(space :align-to (- right (0))))
;;     (propertize " " 'face `(:background ,border-color)
;;                     'display '(space :width (1)))
     (propertize " " 'display "\n")

     ;; Lower part
;;     (propertize " " 'face `(:background ,border-color)
;;                     'display '((raise -0.5) (space :width (1))))
     (propertize " " 'face face-lower 'display '(raise -0.5))
     (propertize (cdr icon) 'face face-lower)
     (propertize " " 'face face-lower)
     (propertize subtitle 'face face-subtitle)
     (propertize " " 'face face-lower
                     'display '(space :align-to (- right (0))))
;;     (propertize " " 'face `(:background ,border-color)
;;                 'display '(space :width (1)))
     (unless no-newline
       (propertize "\n"))))))


(defun nano-elfeed-search-print-entry (entry)
  "Alternative printing of elfeed entries using SVG tags."
  
  (let* ((date (elfeed-entry-date entry))
         (title (or (elfeed-meta entry :title)
                    (elfeed-entry-title entry) ""))
         (unread (member 'unread (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title (when feed
                       (or (elfeed-meta feed :title)
                           (elfeed-feed-title feed)))))

    (nano-elfeed-entry feed-title title date unread t)))

(defun nano-elfeed-search-mode ()
  (setq left-fringe-width 1
        right-fringe-width 1
        left-margin-width 0
        right-margin-width 0
        stripes-unit 1)
  (set-window-buffer nil (current-buffer))

  (setq stripes-overlay-priority 50)
  (stripes-mode 1)
  (setq hl-line-overlay-priority 100)
  (hl-line-mode -1)
  (setq cursor-type nil)
  (face-remap-add-relative 'hl-line :inherit 'nano-faded-i)
  (hl-line-mode t)
  )

(defun nano-elfeed-show-mode ()
  (visual-line-mode)
;;  (setq truncate-lines t)
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (setq-local truncate-lines nil)
    (setq-local shr-width 79)
    ;; (setq header-line-format nil)
    ;; (face-remap-set-base 'default '(:height 140))
    (set-buffer-modified-p nil)))

(defun nano-elfeed-next-entry ()
  (interactive)
  (text-property-search-forward 'elfeed-entry t))

(defun nano-elfeed-prev-entry ()
  (interactive)
  (text-property-search-backward 'elfeed-entry t))

(defun nano-elfeed-show-next ()
  "Show the next item in the elfeed-search buffer."
  (interactive)
  (funcall elfeed-show-entry-delete)
  (with-current-buffer (elfeed-search-buffer)
    (when elfeed-search-remain-on-entry
      (nano-elfeed-next-entry))
    (call-interactively #'elfeed-search-show-entry)))

(defun nano-elfeed-show-prev ()
  "Show the previous item in the elfeed-search buffer."
  (interactive)
  (funcall elfeed-show-entry-delete)
  (with-current-buffer (elfeed-search-buffer)
    (when elfeed-search-remain-on-entry (forward-line 1))
    (nano-elfeed-prev-entry)
    (call-interactively #'elfeed-search-show-entry)))

(setq elfeed-search-filter "@1-weeks-ago +unread"          
      elfeed-search-print-entry-function
           #'nano-elfeed-search-print-entry)

(bind-key "<down>" #'nano-elfeed-next-entry 'elfeed-search-mode-map)
(bind-key "n" #'nano-elfeed-next-entry 'elfeed-search-mode-map)

(bind-key "<up>" #'nano-elfeed-prev-entry 'elfeed-search-mode-map)
(bind-key "p" #'nano-elfeed-prev-entry 'elfeed-search-mode-map)

(bind-key "p" #'nano-elfeed-prev-next 'elfeed-show-mode-map)
(bind-key "n" #'nano-elfeed-show-next 'elfeed-show-mode-map)

(add-hook 'elfeed-search-mode-hook #'nano-elfeed-search-mode)
(add-hook 'elfeed-show-mode-hook #'nano-elfeed-show-mode)

(setq rmh-elfeed-org-files
      (list (expand-file-name "elfeed.org" user-emacs-directory)))
(elfeed-org)

(provide 'nano-elfeed)
