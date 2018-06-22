;;; exwm-wb.el --- Control web browsers running under EXWM. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Peter Jones <pjones@devalot.com>

;; Author: Peter Jones <pjones@devalot.com>
;; Homepage: https://github.com/pjones/exwm-wb
;; Package-Requires: ((emacs "25.1") (async "1.9") (exwm "0.18"))
;; Version: 0.1.0
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; FIXME:

;;; License:
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:
(require 'async)
(require 'browse-url)
(require 'seq)
(require 'exwm)


;;; Classes
;;
;;
(defclass exwm-wb-browser ()
  ((name
    :initarg :name
    :initform nil
    :custom string
    :documentation
    "The name of the browser shown in prompts and menus.")

   (class-name
    :initarg :class-name
    :initform nil
    :custom string
    :documentation
    "  The class name property of the browser window exposed by the
  `exwm-class-name' variable.  Used to detect if the current
  window is a browser and which type of browser it is.")

   (open-url
    :initarg :open-url
    :initform nil
    :custom function
    :documentation
    "  A function returning a list that can be passed to
  `asyn-start-process' to start a new instance/tab of the browser.  It
  should take a single argument, the URL to open.")

   (get-url
    :initarg :get-url
    :initform nil
    :custom function
    :documentation
    "  A function returning the URL being displayed in a browser
  window.  The EXWM buffer holding the browser will be current when
  this function is called.")

   (set-url
    :initarg :set-url
    :initform nil
    :custom function
    :documentation
    "  A function that sets the URL of the browser.  Its argument will
  be the new URL to set in the browser window.  The EXWM buffer
  holding the browser window will be current when this function is
  called.")

   (history-back
    :initarg :history-back
    :initform nil
    :custom function
    :documentation
    "  A function to move backwards in the location history.")

   (history-forward
    :initarg :history-forward
    :initform nil
    :custom function
    :documentation
    "  A function to move forwards in the location history.")))

(defmethod exwm-wb-browser--open-url ((browser exwm-wb-browser) url new-window)
  "Open URL in BROWSER.

If NEW-WINDOW is nil then use the set-url function slot to do
this.  Otherwise, when NEW-WINDOW is non-nil, use the open-url
function slot."
  (if new-window
      (let* ((func (oref browser open-url))
             (args (and func (apply func url nil))))
        (when args (apply #'async-start-process
                          (car args) (car args) nil (cdr args))))
    (let ((func (oref browser set-url)))
      (when func (apply func url nil)))))


;;; Predefined browsers:
(defconst exwm-wb-surf
  (exwm-wb-browser
     :name "Surf"
     :class-name "Surf"
     :open-url (lambda (url) (list "surf" url))
     :get-url  (lambda () (exwm-wb-get-prop "_SURF_URI"))
     :set-url  (lambda (url) (exwm-wb-set-prop "_SURF_GO" url))
     :history-back (lambda () (exwm-wb-send-keys exwm-wb-surf-back-key))
     :history-forward (lambda () (exwm-wb-send-keys exwm-wb-surf-forward-key)))
  "The Surf web browser.")


;;; Customize interface:
(defgroup exwm-wb nil
  "A minor mode and helpers for managing web browsers under EXWM."
  :version "0.1.0"
  :prefix "exwm-wb-"
  :group 'applications)

(defcustom exwm-wb-list
  (list exwm-wb-surf)
  "List of supported browsers.")

(defcustom exwm-wb-surf-back-key [?\C-b]
  "The key to send to Surf to make it move back in its history."
  :group 'exwm-wb
  :type 'key-sequence)

(defcustom exwm-wb-surf-forward-key [?\C-f]
  "The key to send to Surf to make it move forward in its history."
  :group 'exwm-wb
  :type 'key-sequence)

(defcustom exwm-wb-prompt-for-browser nil
  "Whether opening a URL should prompt for which browser to use.

Setting this to non-nil means always prompt.  The default setting
is to use the first configured browser instead of prompting.

See `exwm-wb-list' for the configured browser list."
  :group 'exwm-wb
  :type '(repeat FIXME:))

(defcustom exwm-wb-shortcuts
  '("https://duckduckgo.com/?q=%s"
    "https://www.google.com/search?q=%s"
    "https://en.wikipedia.org/w/index.php?title=Special:Search&search=%s&go=Go")
  "URLs of frequently visited sites and search engines.

This list is used by the `exwm-wb-open-shortcut' command to
build an interactive prompt for selecting a URL and jumping to it
in a new browser window/tab.

A URL can optionally include a %s format specifier.  If such a
URL is selected from the interactive prompt a second prompt will
be show to read a search string.  In this mode the %s in the URL
will be substituted for the input from the second prompt
before launching a browser."
  :group 'exwm-wb
  :type '(repeat (choice string)))


;;; Internal variables:
(defvar exwm-wb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") #'exwm-wb-history-forward)
    (define-key map (kbd "C-c C-b") #'exwm-wb-history-back)
    (define-key map (kbd "C-c C-l") #'exwm-wb-open-url)
    (define-key map (kbd "C-c C-j") #'exwm-wb-open-shortcut)
    map)
  "Key map for `exwm-wb-mode'.")


;;; Functions:
(defun exwm-wb-winid ()
  "Get the X11 window ID for the current EXWM buffer."
  (if (derived-mode-p 'exwm-mode)
      (exwm--buffer->id (current-buffer))
    (error "Not a EXWM window")))

(defun exwm-wb-get-prop (name)
  "Read window property NAME from the current EXWM buffer."
  ;; FIXME: This is a hack stolen from exwm-surf: Copyright (C) 2017
  ;; craven@gmx.net without permission.
  (let* ((winid (exwm-wb-winid))
         (text (shell-command-to-string
                (format "xprop -notype -id %s %s" winid name))))
    (string-match "\"\\(.*\\)\"" text)
    (match-string 1 text)))

(defun exwm-wb-set-prop (name value)
  "Set the X11 window property NAME to VALUE for the current EXWM buffer."
  (let ((winid (exwm-wb-winid)))
    (start-process-shell-command
     "xprop" nil (format "xprop -id %s -f %s 8s -set %s \"%s\""
                         winid name name value))))

(defun exwm-wb-send-keys (keys)
  "Send the key sequence KEYS to the current EXWM window."
  (when (derived-mode-p 'exwm-mode)
    (seq-doseq (key keys)
      (exwm-input--fake-key key))))

(defun exwm-wb-choose-browser ()
  "Prompt the user to select a browser."
  ;; FIXME:
  exwm-wb-surf)

(defun exwm-wb-current-browser ()
  "Return the browser instance for the current window or nil."
  (when (derived-mode-p 'exwm-mode)
    (object-assoc exwm-class-name 'class-name exwm-wb-list)))

(defun exwm-wb-funcall (slot arguments)
  "Call a function at SLOT with ARGUMENTS for the current browser buffer."
  (let* ((browser (exwm-wb-current-browser))
         (func (and browser (slot-value browser slot))))
    (when func (apply func arguments))))

(defun exwm-wb-url-prompt ()
  "Prompt for a URL.

If the current window is a browser, extract a URL from it and use
it as the default answer in the prompt.  Otherwise act like
`browse-url-interactive-arg' and try to find a URL around point
to use as the default answer.

Return a list just like `browse-url-interactive-arg' does where
the CAR is the URL read from the user and the CADR is the
new-window flag."
  (let ((prompt "URL: ")
        (url (exwm-wb-funcall 'get-url nil)))
    (if url (list (read-string prompt url)
                  (not (eq (null browse-url-new-window-flag)
                           (null current-prefix-arg))))
      (browse-url-interactive-arg prompt))))


;;; Commands:
(defun exwm-wb-open-url (url &optional new-window)
  "Open URL in a web browser.

If the current buffer is already a browser window and NEW-WINDOW
is nil, redirect the browser to URL replacing the current
location.  Otherwise, when NEW-WINDOW is non-nil, always open a
new browser window/tab."
  (interactive (exwm-wb-url-prompt))
  (let ((browser (or (and (not new-window) (exwm-wb-current-browser))
                     (exwm-wb-choose-browser)
                     exwm-wb-surf)))
    (exwm-browser--open-url browser url
                            (or new-window
                                (not (exwm-wb-current-browser))))))

(defun exwm-wb-open-shortcut (shortcut &optional new-window)
  "Open a shortcut URL in a browser window.

When called interactively prompt for SHORTCUT.  If SHORTCUT
contains a %s format placeholder also prompt for a search query.

If NEW-WINDOW is non-nil then display the URL in a new window."
  (interactive
   (list (completing-read "Shortcut: " exwm-wb-shortcuts)
         current-prefix-arg))
  (let ((url (if (string-match-p "%s" shortcut)
                 (format shortcut (read-string "Search Query: "))
               shortcut)))
    (exwm-wb-open-url url new-window)))

(defun exwm-wb-history-back ()
  "Tell the current browser to go to the previous URL in the history."
  (interactive)
  (exwm-wb-funcall 'history-back nil))

(defun exwm-wb-history-forward ()
  "Tell the current browser to go to the next URL in the history."
  (interactive)
  (exwm-wb-funcall 'history-forward nil))

(define-minor-mode exwm-wb-mode
  "Minor mode to interact with a web browser displayed by EXWM."
  :group  'exwm-wb
  :keymap 'exwm-wb-mode-map)

(defun exwm-wb-mode-maybe-enable ()
  "Activate `exwm-wb-mode' if the current buffer is a browser."
  (if (exwm-wb-current-browser) (exwm-wb-mode 1)))

;; Try to automatically enable `exwm-wb-mode' when needed:
(add-hook 'exwm-manage-finish-hook #'exwm-wb-mode-maybe-enable)

(provide 'exwm-wb)
;;; exwm-wb.el ends here
