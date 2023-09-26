;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/renderer/electron
    (:documentation "Electron renderer."))
(in-package :nyxt/renderer/electron)

;; Demo:
;; (asdf:load-system :nyxt/electron)
;; (nyxt:start :urls '("https://en.wikipedia.org/wiki/Tomato"))
;; (buffer-load "https://en.wikipedia.org/wiki/Potato")

;; message buffer
;; Test via (ffi-print-message (current-window) "hello")

;; status buffer
;; Ensure that it is rendered properly when resizing window
;; Add support to nyxt://
;; Test via (print-status (window-make *browser*))

;; prompt buffer
;; (let ((prompt-buffer (make-instance 'prompt-buffer :window (current-window))))
;;   (push prompt-buffer (active-prompt-buffers (window prompt-buffer)))
;;   (prompt-render prompt-buffer)
;;   (setf (height prompt-buffer) 10))

;; (wait-on-prompt-buffer (make-instance 'prompt-buffer
;;                                       :window (current-window)
;;                                       :sources (make-instance 'prompter:source
;;                                                               :name "test"
;;                                                               :constructor '("one" "two"))))


(push :nyxt-electron *features*)

(define-class electron-renderer (renderer)
  ((name "Electron"))
  (:export-class-name-p t)
  (:export-accessor-names-p t))

(setf nyxt::*renderer* (make-instance 'electron-renderer))

(define-class electron-browser (electron:interface)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class))

(defmethod install ((renderer electron-renderer))
  (setf electron::*interface*
        (make-instance
         'electron:interface
         :electron-socket-path (uiop:xdg-runtime-dir "electron.socket")
         :lisp-socket-path (uiop:xdg-runtime-dir "lisp.socket")))
  (electron:launch)
  ;; https://github.com/atlas-engineer/cl-electron/issues/15
  (sleep 1)
  (flet ((set-superclasses (renderer-class-sym+superclasses)
           (closer-mop:ensure-finalized
            (closer-mop:ensure-class (first renderer-class-sym+superclasses)
                                     :direct-superclasses (rest renderer-class-sym+superclasses)
                                     :metaclass 'interface-class))))
    (mapc #'set-superclasses '((renderer-browser electron-browser)
                               (renderer-window electron-window)
                               (renderer-buffer electron-buffer)))))

(defmethod uninstall ((renderer electron-renderer))
  (flet ((remove-superclasses (renderer-class-sym)
           (closer-mop:ensure-finalized
            (closer-mop:ensure-class renderer-class-sym
                                     :direct-superclasses '()
                                     :metaclass 'interface-class))))
    (mapc #'remove-superclasses '(renderer-browser
                                  renderer-window
                                  renderer-buffer))))

(define-class electron-buffer (electron:browser-view)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class))

(define-class electron-window (electron:browser-window)
  ((prompt-buffer-view (make-instance 'electron-buffer)))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class))

(defmethod customize-instance :after ((window electron-window) &key)
  (let ((message-buffer (message-buffer window))
        (status-buffer (status-buffer window)))
    (electron::add-browser-view window message-buffer)
    ;; Bug: test how resizing the window affects the geometry of the buffer.
    (electron::set-auto-resize message-buffer t nil nil nil)
    (electron::set-bounds message-buffer
                          0
                          (- (electron::get-bounds window 'height)
                             (height message-buffer))
                          (electron::get-bounds window 'width)
                          (height message-buffer))
    (electron::load-url (electron::web-contents message-buffer) "about:blank")

    (electron::add-browser-view window status-buffer)
    ;; Bug: test how resizing the window affects the geometry of the buffer.
    (electron::set-auto-resize status-buffer t nil nil nil)
    (electron::set-bounds status-buffer
                          0
                          (- (electron::get-bounds window 'height)
                             (+ (height status-buffer)
                                (height message-buffer)))
                          (electron::get-bounds window 'width)
                          (height status-buffer))
    (electron::load-url (electron::web-contents status-buffer) "about:blank")))

;; To subclass or to not subclass... here's the question!
;; initialize-instance :after from electron:browser-view runs first
;; (defmethod initialize-instance :after ((buffer electron-buffer) &key)
;;   "TODO"
;;   (typecase buffer
;;     (status-buffer
;;      (break)
;;      (let ((window (window buffer)))
;;        ;; at this point window is still NIL... why?

;;        ;; because when the window is made it calls (make-instance
;;        ;; 'status-buffer), which creates a status buffer with window set to NIL.
;;        ;; only at initialize-instance :after is the window slot of status buffer
;;        ;; set.
;;        (electron::add-browser-view window buffer)
;;        (electron::set-auto-resize buffer t nil nil t)
;;        (electron::set-bounds buffer
;;                              0
;;                              (- (electron::get-bounds window 'height)
;;                                 (+ (height buffer)
;;                                    (message-buffer-height window)))
;;                              (electron::get-bounds window 'width)
;;                              (height buffer))))))

;; (defmethod ffi-within-renderer-thread ((browser electron-browser) thunk))

;; (defmethod ffi-initialize ((browser electron-browser) urls startup-timestamp)
;;   (declare (ignore urls startup-timestamp))
;;   (setf electron::*interface*
;;         (make-instance
;;          'electron:interface
;;          :electron-socket-path (uiop:xdg-runtime-dir "electron.socket")
;;          :lisp-socket-path (uiop:xdg-runtime-dir "lisp.socket")))
;;   (electron:launch)
;;   (call-next-method))

(defmethod ffi-buffer-evaluate-javascript ((buffer electron-buffer) javascript &optional world-name)
  (declare (ignore world-name))
  (electron::execute-javascript (electron:web-contents buffer) javascript))

(defmethod ffi-buffer-evaluate-javascript-async ((buffer electron-buffer) javascript &optional world-name)
  (declare (ignore world-name))
  (electron::execute-javascript (electron:web-contents buffer) javascript))

;; (defmethod ffi-buffer-sound-enabled-p ((buffer electron-buffer))
;;   (electron::set-audio-muted (electron:web-contents buffer)))

;; (defmethod ffi-buffer-download ((buffer electron-buffer) url))

;; (defmethod ffi-display-url ((browser electron-browser) text))

;; (defmethod ffi-buffer-cookie-policy ((buffer electron-buffer)))

;; (defmethod ffi-preferred-languages ((buffer electron-buffer)))

;; (defmethod ffi-buffer-copy ((buffer electron-buffer) &optional (text nil text-provided-p)))

;; (defmethod ffi-buffer-paste ((buffer electron-buffer) &optional (text nil text-provided-p)))

;; (defmethod ffi-buffer-cut ((buffer electron-buffer)))

;; (defmethod ffi-buffer-select-all ((buffer electron-buffer)))

;; (defmethod ffi-buffer-undo ((buffer electron-buffer)))

;; (defmethod ffi-buffer-redo ((buffer electron-buffer)))

(defmethod ffi-kill-browser ((browser electron-browser))
  (declare (ignore browser))
  (electron:terminate))

;; (defmethod on-signal-destroy ((window electron:browser-window)))

(defmethod ffi-window-delete ((window electron:browser-window))
  (electron::kill window))

(defmethod ffi-window-fullscreen ((window electron:browser-window))
  (electron::fullscreen window))

(defmethod ffi-window-unfullscreen ((window electron:browser-window))
  (electron::unfullscreen window))

(defmethod ffi-window-maximize ((window electron:browser-window))
  (electron::maximize window))

(defmethod ffi-window-unmaximize ((window electron:browser-window))
  (electron::unmaximize window))

;; (defmethod on-signal-key-press-event ((sender electron:browser-window) event))

;; (defmethod on-signal-key-release-event ((sender electron:browser-window) event))

;; (defmethod on-signal-button-press-event ((sender electron-buffer) event))

;; (defmethod on-signal-scroll-event ((sender electron-buffer) event))

(defmethod ffi-buffer-url ((buffer electron-buffer))
  (quri:uri (electron::get-url (electron:web-contents buffer))))

(defmethod ffi-buffer-title ((buffer electron-buffer))
  (electron::get-title (electron:web-contents buffer)))

;; (defmethod on-signal-load-failed-with-tls-errors ((buffer electron-buffer) certificate url))

;; (defmethod on-signal-decide-policy ((buffer electron-buffer) response-policy-decision policy-decision-type-response))

;; (defmethod on-signal-mouse-target-changed ((buffer electron-buffer) hit-test-result modifiers))

;; Not needed since the generic function creates a window that inherits from
;; renderer-window (which points to electron:browser-window, see the
;; install method above)

;; What is needed is to pass an option that disables the frame.  Options:
;; 1) Write a class that inherits from electron:browser-window and set the
;; options in customize-instance :after
;; 2) Create windows in such fashion by default in cl-electron.

;; (defmethod ffi-window-make ((browser electron-browser)))

(defmethod ffi-window-to-foreground ((window electron:browser-window))
  (unless nyxt::*headless-p*
    (electron::focus window))
  (call-next-method))

(defmethod ffi-window-title ((window electron:browser-window))
  (electron::get-title window))
(defmethod (setf ffi-window-title) (title (window electron:browser-window))
  (electron::set-title window title))

;; the solution can't rely on gtk
;; https://docs.gtk.org/gtk4/method.Application.get_active_window.html since
;; electron uses different GUI libraries depending on the OS.

;; https://stackoverflow.com/questions/60296423/how-to-focus-on-electron-window-when-it-opens-after-focussing-on-another-program
;; https://github.com/nullxx/electron-active-window
;; important for prompt-buffer
(defmethod ffi-window-active ((browser electron-browser))
  ;; TODO only handles a single window for now.
  (or (electron::last-active-window browser)
      (call-next-method)))

(defmethod ffi-window-set-buffer ((window electron:browser-window)
                                  (buffer electron-buffer)
                                  &key (focus t))
  ;; specialize on the buffer type, i.e. prompt buffer

  ;; in GTK, when a prompt buffer is open and a new main buffer is opened, the
  ;; prompt buffer remains open.  As per below, the prompt buffers are closed
  ;; when a main buffer is called
  (electron::add-browser-view window buffer)
  (electron::set-auto-resize buffer t t t t)
  ;; FIXME hardcoded. Who sets the bounds of buffers?  In the GTK, this is
  ;; embedded in the window (GtkBox) logic.
  (electron::set-bounds buffer
                        0
                        0
                        (electron::get-bounds window 'width)
                        (- (electron::get-bounds window 'height)
                           (+ (height (status-buffer window))
                              (height (message-buffer window)))))
  (electron::set-top-browser-view window buffer)
  (when focus (electron::focus (electron:web-contents buffer)))
  buffer)

;; (defmethod ffi-print-message ((window electron:browser-window) html-body)
;;   (let ((message-buffer (message-buffer window)))
;;     ;; the logic behind auto-resizing can only be moved to ffi-window-make or by
;;     ;; subclassing electron:browser-window.
;;     (electron::add-browser-view window message-buffer)
;;     ;; Bug: test how resizing the window affects the geometry of the buffer.
;;     (electron::set-auto-resize message-buffer t nil nil nil)
;;     (electron::set-bounds message-buffer
;;                           0
;;                           (- (electron::get-bounds window 'height)
;;                              (height message-buffer))
;;                           (electron::get-bounds window 'width)
;;                           (height message-buffer))
;;     (electron::load-url (electron::web-contents message-buffer) "about:blank")
;;      ;; (electron::execute-javascript (electron::web-contents message-buffer)
;;     ;;                               "document.body.style.background = 'blue'")
;;     ;; (electron::set-background-color message-buffer "red")
;;     (call-next-method)))

;; maybe wrong, but not important
;; (defmethod ffi-window-add-panel-buffer ((window electron:browser-window)
;;                                         (panel-buffer panel-buffer)
;;                                         side)
;;   (match side
;;     ;; panel-buffers-left is part of gtk-window not window...  then I need to
;;     ;; create electron-window that inherits from electron:browser-window
;;     (:left (push panel-buffer (panel-buffers-left window)))
;;     (:right (push panel-buffer (panel-buffers-right window)))))

;; maybe wrong, but not important
;; (defmethod ffi-window-delete-panel-buffer ((window electron:browser-window)
;;                                            (panel-buffer panel-buffer))
;;   (delete panel-buffer (panel-buffers window)))

(defmethod ffi-height ((window electron:browser-window))
  (electron::get-bounds window 'height))

(defmethod ffi-height ((buffer electron-buffer))
  (electron::get-bounds buffer 'height))

(defmethod ffi-height ((status-buffer status-buffer))
  (electron::get-bounds status-buffer 'height))
(defmethod (setf ffi-height) (height (status-buffer status-buffer))
  ;; Improve syntax of set-bounds?
  (electron::set-bounds status-buffer
                        (electron::get-bounds status-buffer 'x)
                        (electron::get-bounds status-buffer 'y)
                        (electron::get-bounds status-buffer 'width)
                        height))

(defmethod ffi-width ((window electron:browser-window))
  (electron::get-bounds window 'width))

(defmethod ffi-width ((buffer electron-buffer))
  (electron::get-bounds buffer 'width))

(defmethod ffi-width ((panel-buffer panel-buffer))
  (electron::get-bounds panel-buffer 'width))
(defmethod (setf ffi-width) (width (panel-buffer panel-buffer))
  (electron::set-bounds panel-buffer
                        (electron::get-bounds panel-buffer 'x)
                        (electron::get-bounds panel-buffer 'y)
                        width
                        (electron::get-bounds panel-buffer 'height)))

;; Test prompt-buffer:
;; Open:
;; (let ((prompt-buffer (make-instance 'prompt-buffer :window (current-window))))
;;   (push prompt-buffer (active-prompt-buffers (window prompt-buffer)))
;;   (prompt-render prompt-buffer)
;;   (setf (height prompt-buffer) 200))
;; Close:
;; (setf (height (first (active-prompt-buffers (current-window)))) 0)
(defmethod ffi-height ((prompt-buffer prompt-buffer))
  ;; same as for status-buffer
  (electron::get-bounds (prompt-buffer-view (window prompt-buffer)) 'height))
(defmethod (setf ffi-height) ((height integer) (prompt-buffer prompt-buffer))
  ;; abstract into a defun since this is the same code as for status-buffer.
  ;; in GTK, when set to 0, it hides the prompt buffer and focuses the
  ;; active-buffer.
  (if (eql height 0)
      ;; why not call ffi-window-set-buffer?
      (progn
        ;; ffi-buffer-delete?
        (electron::remove-browser-view (window prompt-buffer) prompt-buffer)
        (electron::kill (electron:web-contents prompt-buffer))
        (electron::focus (electron:web-contents (nyxt::active-buffer (window prompt-buffer)))))
      (let ((window (window prompt-buffer)))
        (electron::add-browser-view window prompt-buffer)
        ;; Bug: test how resizing the window affects the geometry of the buffer.
        (electron::set-auto-resize prompt-buffer t nil nil nil)
        (electron::set-bounds prompt-buffer
                              0
                              ;; 0
                              (- (electron::get-bounds window 'height)
                                 (+ height
                                    (height (status-buffer window))
                                    (height (message-buffer window))))
                              (electron::get-bounds window 'width)
                              height)
        (electron::load-url (electron::web-contents prompt-buffer) "about:blank")
        (electron::set-top-browser-view window prompt-buffer)
        ;; why not call ffi-focus-prompt-buffer?
        (electron::focus (electron:web-contents prompt-buffer)))))

(defmethod ffi-focus-prompt-buffer ((window electron:browser-window)
                                    (prompt-buffer prompt-buffer))
  ;; I'm confused by this... each window can have its own prompt-buffer?
  (electron::focus (electron:web-contents (prompt-buffer-view (window prompt-buffer))))
  ;; (electron::focus (electron::window-from-browser-view (interface window)
  ;;                                                      prompt-buffer))
  prompt-buffer)

;; Not needed right now, see corresponding method in foreign-interface.
;; (defmethod ffi-buffer-make ((buffer electron-buffer))
;;   (make-instance 'electron-buffer))

(defmethod ffi-buffer-delete ((buffer electron-buffer))
  (electron::kill (electron:web-contents buffer))
  ;; this one only hides it
  ;; (electron::remove-browser-view window view)
  )

(defmethod ffi-buffer-load ((buffer electron-buffer) url)
  (electron::load-url (electron:web-contents buffer) url))

;; doable with https://github.com/atlas-engineer/cl-electron/issues/6?
;; loadURL('data:text/html;charset=utf-8,<YOUR HTML/>');
;; this is not used anywhere so ignoring...
;; (defmethod ffi-buffer-load-html ((buffer electron-buffer) html-content url))
;; (defmethod ffi-buffer-load-alternate-html ((buffer electron-buffer) html-content content-url url))

;; (defmethod ffi-buffer-add-user-style ((buffer electron-buffer) (style electron-user-style)))

;; (defmethod ffi-buffer-remove-user-style ((buffer electron-buffer) (style electron-user-style)))

;; (defmethod ffi-buffer-add-user-script ((buffer electron-buffer) (script electron-user-script)))

;; (defmethod ffi-buffer-remove-user-script ((buffer electron-buffer) (script electron-user-script)))

(defmethod ffi-buffer-user-agent ((buffer electron-buffer)))

(defmethod (setf ffi-buffer-user-agent) (value (buffer electron-buffer)))

;; (defmethod ffi-buffer-proxy ((buffer electron-buffer)))

;; (defmethod (setf ffi-buffer-proxy) (proxy-specifier))

(defmethod ffi-buffer-zoom-level ((buffer electron-buffer))
  (call-next-method))

(defmethod (setf ffi-buffer-zoom-level) (value (buffer electron-buffer)))

;; next goal
;; see electron::register-before-input-event
(defmethod ffi-generate-input-event ((window electron:browser-window) event))

(defmethod ffi-generated-input-event-p ((window electron:browser-window) event))

(defmethod ffi-inspector-show ((buffer electron-buffer)))

(defmethod renderer-history ((buffer electron-buffer)))

(defmethod ffi-focused-p ((buffer electron-buffer)))

(defmethod ffi-muted-p ((buffer electron-buffer))
  (electron::is-audio-muted (electron:web-contents buffer)))

(defmethod ffi-tracking-prevention ((buffer electron-buffer)))

(defmethod (setf ffi-tracking-prevention) (value (buffer electron-buffer)))

;; (defmethod ffi-web-extension-send-message ((buffer electron-buffer)))
