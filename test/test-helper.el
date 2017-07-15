;;; test-helper.el --- description -*- lexical-binding: t; -*-

(defvar shrink-path-test/test-path
  (directory-file-name (file-name-directory load-file-name))
  "Path to tests directory.")

(defvar shrink-path-test/root-path
  (directory-file-name (file-name-directory shrink-path-test/test-path))
"Path to root directory.")

(load (expand-file-name "shrink-path" shrink-path-test/root-path) 'noerror 'nomessage)
