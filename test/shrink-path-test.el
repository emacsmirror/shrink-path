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

;;; shrink-path
;;
;; home-absolute
;;
(ert-deftest shrink-path/home-absolute ()
  (with-home "/home/test"
             (should
              (equal (shrink-path "/home/test")
                     "~/"))))

(ert-deftest shrink-path/home-absolute-depth=1 ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path "/home/test/Projects/")
                      "~/Projects/"))))

(ert-deftest shrink-path/home-absolute-depth=1-hidden ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path "/home/test/.config/")
                      "~/.config/"))))

(ert-deftest shrink-path/home-absolute-depth>1-last-hidden ()
  (with-home "/home/test"
             (should
              (equal
               (shrink-path "/home/test/Projects/dotfiles/emacs/.emacs.d")
               "~/P/d/e/.emacs.d/"))))

(ert-deftest shrink-path/home-absolute-depth>1-last-regular ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path "/home/test/Projects/dotfiles/emacs/.emacs.d/modules")
                      "~/P/d/e/.e/modules/"))))

(ert-deftest shrink-path/home-absolute-depth>1-middle-hidden ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path  "/home/test/Projects/.dotfiles/zsh")
                      "~/P/.d/zsh/"))))

;;
;; home-tilde
;;
(ert-deftest shrink-path/home-tilde ()
  (with-home "/home/test"
             (should (equal (shrink-path "~") "~/"))))

(ert-deftest shrink-path/home-tilde-depth=1 ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path "~/Projects/")
                      "~/Projects/"))))

(ert-deftest shrink-path/home-tilde-depth=1-hidden ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path "~/.config/")
                      "~/.config/"))))

(ert-deftest shrink-path/home-tilde-depth>1-last-hidden ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path "~/Projects/dotfiles/emacs/.emacs.d")
                      "~/P/d/e/.emacs.d/"))))

(ert-deftest shrink-path/home-tilde-depth>1-last-regular ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path "~/Projects/dotfiles/emacs/.emacs.d/modules")
                      "~/P/d/e/.e/modules/"))))

(ert-deftest shrink-path/home-tilde-depth>1-middle-hidden ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path  "~/Projects/.dotfiles/zsh")
                      "~/P/.d/zsh/"))))

;;
;; root
;;
(ert-deftest shrink-path/root ()
  (should (equal (shrink-path "/") "/")))

(ert-deftest shrink-path/root-depth=1 ()
  (should (equal (shrink-path "/tmp") "/tmp/")))

(ert-deftest shrink-path/root-depth=1-hidden ()
  (should (equal (shrink-path "/.snapshotz") "/.snapshotz/")))

(ert-deftest shrink-path/root-depth>1-last-hidden ()
  (should (equal
           (shrink-path "/etc/openvpn/.certificates")
           "/e/o/.certificates/")))
(ert-deftest shrink-path/root-depth>1-last-regular ()
  (should (equal
           (shrink-path "/etc/X11/xorg.conf.d")
           "/e/X/xorg.conf.d/")))

(ert-deftest shrink-path/root-depth>1-middle-hidden ()
  (should (equal
           (shrink-path "/etc/openvpn/.certificates/london")
           "/e/o/.c/london/")))

;;; shrink-path-prompt
;;
;; home-absolute
;;
(ert-deftest shrink-path/home-absolute ()
  (with-home "/home/test"
             (should
              (equal (shrink-path-prompt "/home/test")
                     (cons "" "~")))))

(ert-deftest shrink-path-prompt/home-absolute-depth=1 ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path-prompt "/home/test/Projects/")
                      (cons "~/" "Projects")))))

(ert-deftest shrink-path-prompt/home-absolute-depth=1-hidden ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path-prompt "/home/test/.config/")
                      (cons "~/" ".config")))))

(ert-deftest shrink-path-prompt/home-absolute-depth>1-last-hidden ()
  (with-home "/home/test"
             (should
              (equal
               (shrink-path-prompt "/home/test/Projects/dotfiles/emacs/.emacs.d")
               (cons "~/P/d/e/" ".emacs.d")))))

(ert-deftest shrink-path-prompt/home-absolute-depth>1-last-regular ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path-prompt "/home/test/Projects/dotfiles/emacs/.emacs.d/modules")
                      (cons "~/P/d/e/.e/" "modules")))))

(ert-deftest shrink-path-prompt/home-absolute-depth>1-middle-hidden ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path-prompt  "/home/test/Projects/.dotfiles/zsh")
                      (cons "~/P/.d/" "zsh")))))

;;
;; home-tilde
;;
(ert-deftest shrink-path-prompt/home-tilde ()
  (with-home "/home/test"
             (should (equal (shrink-path-prompt "~") (cons "" "~")))))

(ert-deftest shrink-path-prompt/home-tilde-depth=1 ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path-prompt "~/Projects/")
                      (cons "~/" "Projects")))))

(ert-deftest shrink-path-prompt/home-tilde-depth=1-hidden ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path-prompt "~/.config/")
                      (cons "~/" ".config")))))

(ert-deftest shrink-path-prompt/home-tilde-depth>1-last-hidden ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path-prompt "~/Projects/dotfiles/emacs/.emacs.d")
                      (cons "~/P/d/e/" ".emacs.d")))))

(ert-deftest shrink-path-prompt/home-tilde-depth>1-last-regular ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path-prompt "~/Projects/dotfiles/emacs/.emacs.d/modules")
                      (cons "~/P/d/e/.e/" "modules")))))

(ert-deftest shrink-path-prompt/home-tilde-depth>1-middle-hidden ()
  (with-home "/home/test"
             (should (equal
                      (shrink-path-prompt  "~/Projects/.dotfiles/zsh")
                      (cons "~/P/.d/" "zsh")))))

;;
;; root
;;
(ert-deftest shrink-path-prompt/root ()
  (should (equal (shrink-path-prompt "/") (cons "" "/"))))

(ert-deftest shrink-path-prompt/root-depth=1 ()
  (should (equal (shrink-path-prompt "/tmp") (cons "/" "tmp"))))

(ert-deftest shrink-path-prompt/root-depth=1-hidden ()
  (should (equal (shrink-path-prompt "/.snapshotz") (cons "/" ".snapshotz"))))

(ert-deftest shrink-path-prompt/root-depth>1-last-hidden ()
  (should (equal
           (shrink-path-prompt "/etc/openvpn/.certificates")
           (cons "/e/o/" ".certificates"))))

(ert-deftest shrink-path-prompt/root-depth>1-last-regular ()
  (should (equal
           (shrink-path-prompt "/etc/X11/xorg.conf.d")
           (cons "/e/X/" "xorg.conf.d"))))

(ert-deftest shrink-path-prompt/root-depth>1-middle-hidden ()
  (should (equal
           (shrink-path-prompt "/etc/openvpn/.certificates/london")
           (cons "/e/o/.c/" "london"))))

(provide 'shrink-path-test)
;;; shrink-path-test.el ends here
