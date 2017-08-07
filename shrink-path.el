;;; shrink-path.el --- fish-style path -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Benjamin Andresen

;; Author: Benjamin Andresen
;; Version: 0.2.1
;; Keywords: path
;; URL: http://github.com/shrink-path.el/shrink-path.el
;; Package-Requires: ((s "1.6.1") (dash "1.8.0") (f "0.10.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;; No commentary

;;; Code:
(require 'dash)
(require 's)
(require 'f)
(require 'rx)

(defun shrink-path--truncate (str)
  "Return STR's first character or first two characters if hidden."
  (substring str 0 (if (s-starts-with? "." str) 2 1)))

(defun shrink-path--internal (full-path &optional truncate-all)
  "Return fish-style truncated string based on FULL-PATH.
Optional parameter TRUNCATE-ALL will cause the function to truncate the last
directory too."
  (let* ((home (getenv "HOME"))
         (path (replace-regexp-in-string
                (s-concat "^" home) "~" full-path))
         (split (s-split "/" path 'omit-nulls))
         (split-len (length split))
         shrunk)
    (->> split
         (--map-indexed (if (= it-index (1- split-len))
                            (if truncate-all (shrink-path--truncate it) it)
                          (shrink-path--truncate it)))
         (s-join "/")
         (setq shrunk))
    (s-concat (unless (s-matches? (rx bos (or "~" "/")) shrunk) "/")
              shrunk
              (unless (s-ends-with? "/" shrunk) "/"))))


;;;###autoload
(defun shrink-path (&optional path truncate-all)
  "Given PATH return fish-styled shrunken down path.
Optional parameter TRUNCATE-ALL will cause the function to truncate the last
directory too."
  (let* ((path (or path default-directory))
         (path (f-full path)))
    (cond
     ((s-equals? (f-short path) "/") "/")
     ((s-matches? (rx bos (or "~" "/") eos) "~/"))
     (t (shrink-path--internal path truncate-all)))))

;;;###autoload
(defun shrink-path-reverse (str &optional absolute-p)
  "Return expanded path from STR if found or list of matches on multiple.
The path referred to by STR has to exist for this to work.
If ABSOLUTE-P is t the returned path will be absolute."
  (let* ((str-split (s-split "/" str 'omit-nulls))
         (head (car str-split)))
    (if (= (length str-split) 1)
        (s-concat "/" str-split)
      (-as-> (-drop 1 str-split) it
             (s-join "*/" it)
             (s-concat (if (s-equals? head "~") "~/" head) it)
             (f-glob it)
             (if absolute-p (-map #'f-full it) (-map #'f-abbrev it))
             (if (= (length it) 1) (car it) it)))))

;;;###autoload
(defun shrink-path-prompt (&optional pwd)
  "Return cons of BASE and DIR for PWD."
  (let* ((pwd (or pwd default-directory))
         (shrunk (shrink-path pwd))
         (split (-as-> shrunk it (s-split "/" it 'omit-nulls)))
         base dir)
    (setq dir (or (-last-item split) "/"))
    (setq base (if (s-equals? dir "/") ""
                 (s-chop-suffix (s-concat dir "/") shrunk)))
    (cons base dir)))

;;;###autoload
(defun shrink-path-file (file &optional truncate-all-dirs)
  "Return FILE's shrunk down path and filename.
TRUNCATE-ALL-DIRS controls if the tail directory should also be shrunk."
  (let ((filename (f-filename file))
        (dirname (f-dirname file)))
    (s-concat (shrink-path dirname truncate-all-dirs) filename)))

;;;###autoload
(defun shrink-path-file-reverse (str &optional exist-p absolute-p)
  "Return STR's expanded filename.
The path referred to by STR has to exist for this to work.
If EXIST-P is t the filename also has to exist.
If ABSOLUTE-P is t the returned path will be absolute."
  ;; TODO
  )

(provide 'shrink-path)
;;; shrink-path.el ends here
