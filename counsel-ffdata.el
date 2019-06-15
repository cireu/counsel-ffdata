;;; counsel-ffdata.el  --- Use ivy to access firefox data  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Zhu Zihao

;; Author: Zhu Zihao all_but_last@163.com
;; URL: https://github.com/cireu/counsel-ffdata
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2") (ivy "") (counsel "") (emacsql ""))
;; Keywords: firefox, browser

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'ivy)
(require 'counsel)
(require 'emacsql)
(require 'org-faces)                    ;For face `org-date'

(eval-when-compile
  (require 'pcase))                     ;`pcase-let' and `pcase-lambda'

(defvar counsel-ffdata-database-path
  (car (file-expand-wildcards "~/.mozilla/firefox/*.default/places.sqlite")))

(defvar counsel-ffdata--temp-db-path
  (expand-file-name (make-temp-name "ffdb") temporary-file-directory))

(defvar counsel-ffdata--cache (make-hash-table :test #'equal))

(defun counsel-ffdata--ensure-db! (&optional force-update?)
  (cl-flet ((update-db!
             ()
             (copy-file counsel-ffdata-database-path
                        counsel-ffdata--temp-db-path)
             (clrhash counsel-ffdata--cache)))
    (let* ((path counsel-ffdata--temp-db-path))
      (condition-case e
          (if (file-exists-p path)
              (when force-update?
                (delete-file path)
                (update-db!))
            (update-db!))
        (error "Failed to ensure firefox database: %s" e))
      nil)))

(defun counsel-ffdata--parse-sql-result ()
  (goto-char (point-min))
  (let (result)
    (while (re-search-forward (rx (group (+? any)) (eval (kbd "C-^"))) nil t)
      (push (match-string 1) result))
    (nreverse (mapcar (lambda (it) (split-string it (kbd "C-_")))
                      result))))

(defsubst counsel-ffdata--prepare-sql-stmt (sql &rest args)
  (concat (apply #'emacsql-format (emacsql-prepare sql) args) ";"))


(cl-defun counsel-ffdata--prepare-candidates! (&key
                                              (caller this-command)
                                              query-stmt
                                              force-update?
                                              transformer)
  (counsel-require-program "sqlite3")
  (counsel-ffdata--ensure-db! force-update?)
  (or
   (if force-update? nil (gethash caller counsel-ffdata--cache nil))
   (let* ((db-path counsel-ffdata--temp-db-path)
          (query-cmd (counsel-ffdata--prepare-sql-stmt query-stmt)))
     (with-temp-buffer
       (let ((errno (call-process "sqlite3" nil (current-buffer) nil
                                  "--ascii" db-path query-cmd))
             result)
         (if (= errno 0)
             (setq result (counsel-ffdata--parse-sql-result))
           (error "SQLite exited with error code %d" errno))
         (when (functionp transformer)
           (cl-callf2 mapcar transformer result))
         (setf (gethash caller counsel-ffdata--cache) result))))))

(defun counsel-ffdata-firefox-bookmarks (&optional force-update?)
  (interactive "P")
  (ivy-read "Firefox Bookmarks: "
            (counsel-ffdata--prepare-candidates!
             :query-stmt [:select [bm:title p:url]
                          :from (as moz_bookmarks bm)
                          :inner-join (as moz_places p)
                          :where (= bm:fk p:id)]
             :force-update? force-update?
             :caller 'counsel-ffdata-firefox-bookmarks
             :transformer (pcase-lambda ((and whole (let `(,title ,url) whole)))
                            (cons (format "%s %s"
                                          title (propertize url 'face 'link))
                                  whole)))
            :history 'counsel-ffdata-firefox-bookmarks
            :action (lambda (it) (browse-url (cl-third it)))
            :caller 'counsel-ffdata-firefox-bookmarks
            :require-match t))

;;;###autoload
(defun counsel-ffdata--history-cands-transformer (cands)
  (pcase-let* (((and whole (let `(,title ,url ,date-in-ms) whole))
                cands)
               (date (/ (string-to-number date-in-ms) 1000000))
               (readable-date (format-time-string "%Y-%m-%d %H:%M %a" date)))
    (cons (format "%s %s %s"
                  title
                  (propertize url 'face 'link)
                  (propertize readable-date 'face 'org-date))
          whole)))

;;;###autoload
(defun counsel-ffdata-firefox-history (&optional force-update?)
  (interactive "P")
  (ivy-read "Firefox History: "
            (counsel-ffdata--prepare-candidates!
             :query-stmt [:select [p:title p:url h:visit_date]
                          :from (as moz_historyvisits h)
                          :inner-join (as moz_places p)
                          :where (= h:place_id p:id)]
             :force-update? force-update?
             :caller 'counsel-ffdata-firefox-history
             :transformer #'counsel-ffdata--history-cands-transformer)
            :history 'counsel-ffdata-firefox-history
            :action (lambda (cand) (browse-url (cl-third cand)))
            :caller 'counsel-ffdata-firefox-history
            :require-match t))

(provide 'counsel-ffdata)
;;; counsel-ffdata.el ends here
