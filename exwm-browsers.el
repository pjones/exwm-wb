;;; exwm-browsers.el --- Control web browsers running under EXWM. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Peter Jones <pjones@devalot.com>

;; Author: Peter Jones <pjones@devalot.com>
;; Homepage: https://github.com/pjones/exwm-browsers
;; Package-Requires: ((emacs "24.4") (async "1.9") (exwm "0.18"))
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
(defclass exwm-browser ()
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

(defmethod exwm-browser--open-url ((browser exwm-browser) url new-window)
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
(defconst exwm-browsers-surf
  (exwm-browser :name "Surf"
                :class-name "Surf"
                :open-url (lambda (url) (list "surf" url))
                :get-url  (lambda () (exwm-browsers-get-prop "_SURF_URI"))
                :set-url  (lambda (url) (exwm-browsers-set-prop "_SURF_GO" url))
                :history-back (lambda () (exwm-browsers-send-keys exwm-browsers-surf-back-key))
                :history-forward (lambda () (exwm-browsers-send-keys exwm-browsers-surf-forward-key)))
  "The Surf web browser.")


;;; Customize interface:
(defgroup exwm-browsers nil
  "A minor mode and helpers for managing web browsers under EXWM."
  :version "0.1.0"
  :prefix "exwm-browsers-"
  :group 'applications)

(defcustom exwm-browsers-list
  (list exwm-browsers-surf)
  "List of supported browsers.")

(defcustom exwm-browsers-surf-back-key [?\C-b]
  "The key to send to Surf to make it move back in its history."
  :group 'exwm-browsers
  :type 'key-sequence)

(defcustom exwm-browsers-surf-forward-key [?\C-f]
  "The key to send to Surf to make it move forward in its history."
  :group 'exwm-browsers
  :type 'key-sequence)

(defcustom exwm-browsers-prompt-for-browser nil
  "Whether opening a URL should prompt for which browser to use.

Setting this to non-nil means always prompt.  The default setting
is to use the first configured browser instead of prompting.

See `exwm-browsers-list' for the configured browser list."
  :group 'exwm-browsers
  :type '(repeat FIXME:))

(defcustom exwm-browsers-shortcuts
  '("https://duckduckgo.com/?q=%s"
    "https://www.google.com/search?q=%s"
    "https://en.wikipedia.org/w/index.php?title=Special:Search&search=%s&go=Go")
  "URLs of frequently visited sites and search engines.

This list is used by the `exwm-browsers-open-shortcut' command to
build an interactive prompt for selecting a URL and jumping to it
in a new browser window/tab.

A URL can optionally include a %s format specifier.  If such a
URL is selected from the interactive prompt a second prompt will
be show to read a search string.  In this mode the %s in the URL
will be substituted for the input from the second prompt
before launching a browser."
  :group 'exwm-browsers
  :type '(repeat (choice string)))


;;; Internal variables:
(defvar exwm-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") #'exwm-browsers-history-forward)
    (define-key map (kbd "C-c C-b") #'exwm-browsers-history-back)
    (define-key map (kbd "C-c C-l") #'exwm-browsers-open-url)
    (define-key map (kbd "C-c C-j") #'exwm-browsers-open-shortcut)
    map)
  "Key map for `exwm-browser-mode'.")


;;; Functions:
(defun exwm-browsers-winid ()
  "Get the X11 window ID for the current EXWM buffer."
  (if (derived-mode-p 'exwm-mode)
      (exwm--buffer->id (current-buffer))
    (error "Not a EXWM window")))

(defun exwm-browsers-get-prop (name)
  "Read window property NAME from the current EXWM buffer."
  ;; FIXME: This is a hack stolen from exwm-surf: Copyright (C) 2017
  ;; craven@gmx.net without permission.
  (let* ((winid (exwm-browsers-winid))
         (text (shell-command-to-string
                (format "xprop -notype -id %s %s" winid name))))
    (string-match "\"\\(.*\\)\"" text)
    (match-string 1 text)))

(defun exwm-browsers-set-prop (name value)
  "Set the X11 window property NAME to VALUE for the current EXWM buffer."
  (let ((winid (exwm-browsers-winid)))
    (start-process-shell-command
     "xprop" nil (format "xprop -id %s -f %s 8s -set %s \"%s\""
                         winid name name value))))

(defun exwm-browsers-send-keys (keys)
  "Send the key sequence KEYS to the current EXWM window."
  (when (derived-mode-p 'exwm-mode)
    (seq-doseq (key keys)
      (exwm-input--fake-key key))))

(defun exwm-browsers-choose-browser ()
  "Prompt the user to select a browser."
  ;; FIXME:
  exwm-browsers-surf)

(defun exwm-browsers-current-browser ()
  "Return the browser instance for the current window or nil."
  (when (derived-mode-p 'exwm-mode)
    (object-assoc exwm-class-name 'class-name exwm-browsers-list)))

(defun exwm-browsers-funcall (slot arguments)
  "Call a function at SLOT with ARGUMENTS for the current browser buffer."
  (let* ((browser (exwm-browsers-current-browser))
         (func (and browser (slot-value browser slot))))
    (when func (apply func arguments))))

(defun exwm-browsers-url-prompt ()
  "Prompt for a URL.

If the current window is a browser, extract a URL from it and use
it as the default answer in the prompt.  Otherwise act like
`browse-url-interactive-arg' and try to find a URL around point
to use as the default answer.

Return a list just like `browse-url-interactive-arg' does where
the CAR is the URL read from the user and the CADR is the
new-window flag."
  (let ((prompt "URL: ")
        (url (exwm-browsers-funcall 'get-url nil)))
    (if url (list (read-string prompt url)
                  (not (eq (null browse-url-new-window-flag)
                           (null current-prefix-arg))))
      (browse-url-interactive-arg prompt))))


;;; Commands:
(defun exwm-browsers-open-url (url &optional new-window)
  "Open URL in a web browser.

If the current buffer is already a browser window and NEW-WINDOW
is nil, redirect the browser to URL replacing the current
location.  Otherwise, when NEW-WINDOW is non-nil, always open a
new browser window/tab."
  (interactive (exwm-browsers-url-prompt))
  (let ((browser (or (and (not new-window) (exwm-browsers-current-browser))
                     (exwm-browsers-choose-browser)
                     exwm-browsers-surf)))
    (exwm-browser--open-url browser url
                            (or new-window
                                (not (exwm-browsers-current-browser))))))

(defun exwm-browsers-open-shortcut (shortcut &optional new-window)
  "Open a shortcut URL in a browser window.

When called interactively prompt for SHORTCUT.  If SHORTCUT
contains a %s format placeholder also prompt for a search query.

If NEW-WINDOW is non-nil then display the URL in a new window."
  (interactive
   (list (completing-read "Shortcut: " exwm-browsers-shortcuts)
         current-prefix-arg))
  (let ((url (if (string-match-p "%s" shortcut)
                 (format shortcut (read-string "Search Query: "))
               shortcut)))
    (exwm-browsers-open-url url new-window)))

(defun exwm-browsers-history-back ()
  "Tell the current browser to go to the previous URL in the history."
  (interactive)
  (exwm-browsers-funcall 'history-back nil))

(defun exwm-browsers-history-forward ()
  "Tell the current browser to go to the next URL in the history."
  (interactive)
  (exwm-browsers-funcall 'history-forward nil))

(define-minor-mode exwm-browser-mode
  "Minor mode to interact with a web browser displayed by EXWM."
  :group  'exwm-browsers
  :keymap 'exwm-browser-mode-map)

(defun exwm-browser-mode-maybe-enable ()
  "Activate `exwm-browser-mode' if the current buffer is a browser."
  (if (exwm-browsers-current-browser) (exwm-browser-mode 1)))

;; Try to automatically enable `exwm-browser-mode' when needed:
(add-hook 'exwm-manage-finish-hook #'exwm-browser-mode-maybe-enable)

(provide 'exwm-browsers)
;;; exwm-browsers.el ends here
