;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/expedition-mode
    (:documentation "Traverse a list of links."))
(in-package :nyxt/expedition-mode)

(define-mode expedition-mode ()
  "Mode for traversing a set of URLs."
  ((urls (list))
   (index 0 :documentation "The index of the current element in URLs.")
   (keyscheme-map
    (define-keyscheme-map "expedition-mode" ()
      keyscheme:cua
      (list
       "C-[" 'expedition-previous
       "C-]" 'expedition-next)
      keyscheme:emacs
      (list
       "M-p" 'expedition-previous
       "M-n" 'expedition-next)))
   (rememberable-p nil)))

(define-command expedition-next (&key (expedition (find-submode 'expedition-mode)))
  "Go to the next URL in the expedition."
  (if (> (length (urls expedition)) (+ 1 (index expedition)))
      (progn
        (incf (index expedition))
        (buffer-load (nth (index expedition) (urls expedition))))
      (echo "End of expedition.")))

(define-command expedition-previous (&key (expedition (find-submode 'expedition-mode)))
  "Go to the previous URL in the expedition."
  (if (> (index expedition) 0)
      (progn
        (decf (index expedition))
        (buffer-load (nth (index expedition) (urls expedition))))
      (echo "Start of expedition.")))
