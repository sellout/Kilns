;;; kilns-minor-mode.el --- A minor mode for the kilns programming language 
(defconst kilns-minor-mode-version "0.1")
;; Copyright (c)2008 Jonathan Arkell. (by)(nc)(sa)  Some rights reserved.
;; Author: Jonathan Arkell <jonnay@jonnay.net>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:
(defgroup kilns-minor-mode '()
  "This is a minor mode for the kilns programming lanugage.

For more information, see http://github.com/sellout/Kilns")
  
;;; Installation:
;; Put kilns-minor-mode.el somewhere in your load-path.
;; (Use M-x show-variable RET load-path to see what your load path is.)
;; Add this to your emacs init file.
;(require 'kilns-minor-mode)


;;; Commands:
;;
;; Below are complete command list:
;;
;;  `run-kilns'
;;    Switch to the kilns window if runing, otherwise Launch the Kilns interpreter in a comint window.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `inferior-kilns-binary-path'
;;    The path to the kilns executable with trailing slash.
;;    default = "~/projects/kilns-binary/"
;;  `inferior-kilns-binary'
;;    The name of the kilns executable.
;;    default = "kilns"

;;; TODO:
;; 

;;; CHANGELOG:
;; v 0.1 - Initial release

;;; Code:

(require 'comint)

(defcustom inferior-kilns-binary-path "~/projects/kilns-binary/"
  "The path to the kilns executable with trailing slash.

This should not include the name of the kilns binary."
  :type 'path
  :group 'kilns-minor-mode)

(defcustom inferior-kilns-binary "kilns"
  "The name of the kilns executable."
  
  :type 'string
  :group 'kilns-minor-mode)

(defvar kilns-minor-mode-map ()
  (let ((map (make-sparse-keymap)))
	(define-key map "\C-c\C-s" 'run-kilns)
	map))

(defvar inferior-kilns-buffer nil
  "Buffer that the kilns process runs in")

(defvar inverior-kilns-process nil
  "The comint process for the inferior kilns ")

(define-derived-mode inferior-kilns-mode comint-mode "Inf-Kilns"
  "Major mode for interacting with kilns"
  (setq comint-input-senter 'inferior-kilns-input-sender))

(defun inferior-kilns-info-grabber (proc string)
  "Just a stub function for now.  We'll do awesome stuff later.")

(defun inferior-kilns-input-sender (proc string)
  "I am not sure if this is strictly necessary yet.
This might get wacked."
  (comint-send-string proc (concat string "\n")))

(defun run-kilns ()
  "Switch to the kilns window if runing, otherwise Launch the Kilns interpreter in a comint window."
  (interactive)
  (pop-to-buffer (process-buffer (inferior-kilns-process)))
  (goto-char (process-mark (inferior-kilns-process))))

(defun inferior-kilns-process ()
  "Returns the current kilns process, or starts up a new one"
  (if (buffer-live-p inferior-kilns-buffer)
	  (get-buffer-process inferior-kilns-buffer)
	  (progn
	   (inferior-kilns-start-process)
	   (inferior-kilns-process))))

(defun inferior-kilns-start-process ()
  "Actually start the kilns interpreter.

This sets the current directory to the path of the kilns binary before launching kilns.
I am not 100% sure this is the proper way to do it, but it does seem the most convenient."
  (let ((old-dir default-directory))
	(cd inferior-kilns-binary-path)
	(setq inferior-kilns-buffer
		  (make-comint "Kilns" (concat inferior-kilns-binary-path inferior-kilns-binary)))
	(with-current-buffer inferior-kilns-buffer
						 (inferior-kilns-mode)
						 (run-hooks 'inferior-kilns-hook))
	(cd old-dir)))

(provide 'kilns-minor-mode)

;;; kilns-minor-mode ends here
