;;; treemacs-elfeed.el --- Elfeed integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2018 Daniel Kraus

;; Author: Daniel Kraus <daniel@kraus.my>
;; Package-Requires: ((treemacs "2.1") (elfeed "3.1"))
;; Package-Version: 0
;; Homepage: https://github.com/Alexander-Miller/treemacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Show a list of your elfeed RSS feeds.

;;; Code:

(require 'elfeed)
(require 'treemacs)
(require 'dash)


(defun treemacs-elfeed--gather-summary ()
  (let ((table (make-hash-table :test 'eq)))
    ;; Stuff all the feeds we care about in a table
    ;; Each entry is (total-count unread-count most-recent-entry)
    (dolist (url (elfeed-feed-list))
      (setf (gethash (elfeed-db-get-feed url) table)
            (list 0 0 nil nil)))
    ;; Visit every database entry efficiently ordered by time, descending
    (with-elfeed-db-visit (entry feed)
      (let ((info (gethash feed table)))
        (when info
          (when (= 1 (cl-incf (car info)))
            (setf (caddr info) entry))
          (when (elfeed-entry-tags entry)
            (setf (nth 3 info) (elfeed-entry-tags entry)))
          (when (memq 'unread (elfeed-entry-tags entry))
            (cl-incf (cadr info))))))
    ;; Create a table of the results
    (cl-loop for feed being the hash-keys of table
             using (hash-values info)
             for (total unread most-recent tags) = info
             for title = (or (elfeed-feed-title feed) "<no-title>")
             for date = (format-time-string
                         "%Y-%m-%d" (elfeed-entry-date most-recent))
             collect (list :title title
                           :url (elfeed-feed-url feed)
                           :total total
                           :unread unread
                           :date date
                           :tags tags)
             into items
             finally return (cl-sort items #'string<
                                     :key (lambda (x) (plist-get :title x))))))

(defun treemacs-elfeed--get-elfeed-tags ()
  "Get the list of buffers, grouped by their major mode."
  (->> (treemacs-elfeed--gather-summary)
       ;;(--reject (eq ?\ (aref (buffer-name it) 0)))
       (--group-by (format "%-10s (%s/%s)"
                           ;; (plist-get it :date)
                           (plist-get it :title)
                           (plist-get it :unread)
                           (plist-get it :total)))))

(defun treemacs-elfeed--get-elfeed-tagsXXX ()
  "Get the list of buffers, grouped by their major mode."
  (->> (treemacs-elfeed--gather-summary)
       ;;(--reject (eq ?\ (aref (buffer-name it) 0)))
       (--group-by (car (plist-get it :tags)))))


(defun treemacs-elfeed-visit-tag (&rest _)
  "Set the elfeed filter"
  (let* ((node (treemacs-current-button))
         (feed (car (treemacs-button-get node :buffer)))
         (url (plist-get feed :url))
         (unread (plist-get feed :unread))
         (_tag (car (plist-get feed :tags)))
         ;; (tag (symbol-name (car (plist-get (car tags) :tags))))
         )
    ;; (elfeed-search-set-filter (concat "+unread +" tag))
    (elfeed-search-set-filter
     (concat (if (> unread 0) "+unread " "") "=" url))
    ;; (elfeed-search-set-filter "+dev +unread")
    ;; (select-window (next-window))
    ))


(treemacs-define-leaf-node elfeed-leaf
  (treemacs-as-icon "• " 'face 'font-lock-builtin-face)
  :ret-action #'treemacs-elfeed-visit-tag
  :tab-action #'treemacs-elfeed-visit-tag)



(treemacs-define-expandable-node elfeed-root-top
  :icon-open (treemacs-as-icon "- " 'face 'font-lock-string-face)
  :icon-closed (treemacs-as-icon "+ " 'face 'font-lock-string-face)
  :query-function (treemacs-elfeed--get-elfeed-tags)
  :render-action
  (treemacs-render-node
   :icon treemacs-elfeed-leaf-icon ;; treemacs-icon-buffer-group-closed
   ;; :label-form (symbol-name (car item))
   :label-form (car item)
   :state treemacs-elfeed-leaf-state
   :face 'font-lock-keyword-face
   :key-form (car item)
   :more-properties (:buffer (cdr item)))
  :top-level-marker t
  :root-label "Elfeed"
  :root-face 'font-lock-type-face
  :root-key-form 'Elfeed)


;;;###autoload
(defun treemacs-elfeed-display-feeds-list ()
  "Show a list of RSS feeds."
  (interactive)
  (let* ((buffer (get-buffer-create "*Treemacs Elfeed List*"))
         (window (display-buffer-in-side-window buffer '((side . left)))))
    (select-window window)
    (treemacs-initialize)
    (treemacs-ELFEED-ROOT-TOP-extension)))

(provide 'treemacs-elfeed)
;;; treemacs-elfeed.el ends here
