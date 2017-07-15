;;; shrink-path-test.el --- tests for shrink-path.el -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Benjamin Andresen

;; Author: Benjamin Andresen
;; Maintainer: Benjamin Andresen
;; Version: 0.0.1
;; Keywords: path
;; URL: http://github.com/shrink-path.el/shrink-path.el

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
(require 'shrink-path)

(ert-deftest shrink-path/root ()
  (should (equal (shrink-path "/") "/")))

(ert-deftest shrink-path/home-tilde ()
  (should (with-home "/home/test"
                     (equal (shrink-path "~") "~/"))))

(ert-deftest shrink-path/home-absolute ()
  (should
   (with-home "/home/test"
              (equal (shrink-path "/home/test")
                     "~/"))))

(ert-deftest shrink-path/root-depth=1 ()
  (should (equal (shrink-path "/tmp") "/tmp/")))

(ert-deftest shrink-path/root-depth=1-hidden ()
  (should (equal (shrink-path "/.snapshotz") "/.snapshotz/")))

(ert-deftest shrink-path/root-depth>1 ()
  (should (equal
           (shrink-path "/etc/X11/xorg.conf.d")
           "/e/X/xorg.conf.d/")))

(ert-deftest shrink-path/home-tilde-depth=1 ()
  (should
   (with-home "/home/test"
              (equal
               (shrink-path "~/Projects/")
              "~/Projects/"))))

(ert-deftest shrink-path/home-tilde-depth=1-hidden ()
  (should (with-home "/home/test"
                     (equal
                      (shrink-path "~/.config/")
                      "~/.config/"))))

(ert-deftest shrink-path/home-tilde-depth>1-last-hidden ()
  (should (with-home "/home/test"
                     (equal
                      (shrink-path "~/Projects/dotfiles/emacs/.emacs.d")
                      "~/P/d/e/.emacs.d/"))))

(ert-deftest shrink-path/home-tilde-depth>1-last-regular ()
  (should (with-home "/home/test"
                     (equal
                      (shrink-path "~/Projects/dotfiles/emacs/.emacs.d/modules")
                      "~/P/d/e/.e/modules/"))))

(ert-deftest shrink-path/home-absolute-depth>1-last-hidden ()
  (should (with-home "/home/test"
                     (equal
                      (shrink-path "/home/test/Projects/dotfiles/emacs/.emacs.d")
                      "~/P/d/e/.emacs.d/"))))


(ert-deftest shrink-path/home-absolute-depth>1-last-regular ()
  (should (with-home "/home/test"
                     (equal
                      (shrink-path "/home/test/Projects/dotfiles/emacs/.emacs.d/modules")
                      "~/P/d/e/.e/modules/"))))

(ert-deftest shrink-path/home-tilde-depth>1-middle-hidden ()
  (should (with-home "/home/test"
                     (equal
                      (shrink-path  "~/Projects/.dotfiles/zsh")
                      "~/P/.d/zsh/"))))


(provide 'shrink-path-test)
;;; shrink-path-test.el ends here
