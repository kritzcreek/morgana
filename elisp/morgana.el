;;; morgana.el --- Magic for the PureScript programming language -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Christoph Hegemann

;; Author: Christoph Hegemann <christoph.hegemann1337@gmail.com>
;; Keywords: language
;; Version: 0.0.1
;; Package-Requires: ((s "1.11.0") (dash "2.12.1") (f "0.18.2"))

;; This file is not part of EMACS
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Magix

;;; Code:

(require 'dash)
(require 's)
(require 'f)

(defun pos-at-line-col (l c)
  (save-excursion
    (goto-char (point-min))
    (goto-line l)
    (move-to-column (- c 1))
    (point)))

(defun morgana-set ()
  "Starts a new selection at point"
  (interactive)
  (morgana-send-set (point)))

(defun morgana-widen ()
  "Widens a selection"
  (interactive)
  (morgana-send "w"))

(defun morgana-narrow ()
  "Narrows a selection"
  (interactive)
  (morgana-send "n"))

(defun morgana-occurrences ()
  "Finds all occurrences for the current selection"
  (interactive)
  (let* ((p (point))
         (cmd (format
               "o %s %d %d"
               (f-this-file)
               (line-number-at-pos p)
               (save-excursion
                 (goto-char p)
                 (+ 1 (current-column))))))
    (morgana-send cmd)))

(setq morgana-overlay
      (make-overlay 0 0 (current-buffer)))
(setq morgana-occurrences-overlays '())

(defun morgana-clear-occurrences ()
  (-each morgana-occurrences-overlays 'delete-overlay)
  (setq morgana-occurrences-overlays '()))

(defun morgana-add-occurrence (line1 column1 line2 column2)
  (let ((o (make-overlay (pos-at-line-col line1 column1)
                         (pos-at-line-col line2 column2)
                         (current-buffer))))
    (overlay-put o 'face `(:background "green"))
    (push o morgana-occurrences-overlays)))

(morgana-clear-occurrences)
(morgana-add-occurrence 1 1 2 2)

(defun morgana-send-set (p)
  (let ((cmd (format
              "s %s %d %d"
              (f-this-file)
              (line-number-at-pos p)
              (save-excursion
                (goto-char p)
                (+ 1 (current-column))))))
    (morgana-send cmd)))

(defun morgana-send (cmd)
  (let ((proc (get-process "morgana")))
    (unless proc
      (make-network-process
        :name "morgana"
        :filter 'morgana-process-filter
        :family 'ipv4
        :host "localhost"
        :service 5678)))
  (process-send-string (get-process "morgana") (s-prepend cmd "\n")))

(defun morgana-process-filter (proc output)
  (let* ((splitted (s-split " " output))
         (type (car splitted))
         (content (cdr splitted)))
    (cond
      ((string= type "m:") (message (s-chomp (s-join " " content))))
      ((string= type "s:") (morgana-select content))
      ((string= type "ss:") (morgana-hl-occurences content)))))

(defun morgana-hl-occurences (strings)
  "Highlights the passed source positions"
  (let* ((numbers (-map 'string-to-int strings))
         (batchedNumbers (-partition-all 4 numbers)))
    (morgana-clear-occurrences)
    (-each batchedNumbers (lambda (batch) (morgana-add-occurrence
                                           (nth 0 batch)
                                           (nth 1 batch)
                                           (nth 2 batch)
                                           (nth 3 batch))))))

(defun morgana-select (x)
  (message "hi")
  (overlay-put morgana-overlay 'face `(:background "red"))
  (let ((beg (pos-at-line-col (string-to-int (nth 0 x))
                              (string-to-int (nth 1 x))))
        (end (pos-at-line-col (string-to-int (nth 2 x))
                              (string-to-int (nth 3 x))))
        (matchType (nth 4 x)))
    (message matchType)
    (move-overlay morgana-overlay beg end (current-buffer))))

(global-set-key (quote [f1]) 'morgana-set)
(global-set-key (quote [f2]) 'morgana-widen)
(global-set-key (quote [f3]) 'morgana-narrow)
(global-set-key (quote [f4]) 'morgana-occurrences)

(provide 'morgana)
;;; morgana.el ends here
