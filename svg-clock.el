;;; svg-clock.el --- Analog clock using Scalable Vector Graphics

;; Copyright (C) 2011  Free Software Foundation, Inc.

;; Author:      Ulf Jasper <ulf.jasper@web.de>
;; Created:     22. Sep. 2011
;; Keywords:    demo

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; svg-clock provides a scalable analog clock.  Rendering is done by
;; means of svg (Scalable Vector Graphics).  Works only with Emacsen
;; which were built with svg support -- (image-type-available-p 'svg)
;; must return t.  Call `svg-clock' to start/stop the clock.
;; Set `svg-clock-size' to change its size.

;; Installation
;; ------------

;; Add the following lines to your Emacs startup file (`~/.emacs').
;; (add-to-list 'load-path "/path/to/svg-clock.el")
;; (autoload 'svg-clock "svg-clock" "Start/stop svg-clock" t)

;; ======================================================================

;;; History:

;; 0.1 (2011-09-22)
;;     - Initial release.

;;; Code:
(defconst svg-clock-version "0.1" "Version number of `svg-clock'.")

(require 'image-mode)

(defgroup svg-clock nil
  "svg-clock"
  :group 'applications)

(defcustom svg-clock-size 250
  "Size (width and height) of the clock, in pixels."
  :type 'integer
  :group 'svg-clock)

(defvar svg-clock-timer nil)

(defconst svg-clock-template
  "<?xml version=\"1.0\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
<svg xmlns=\"http://www.w3.org/2000/svg\"
     width=\"%SIZE%\" height=\"%SIZE%\" >
    <defs>
        <symbol id=\"tick\">
            <line x1=\"50\" y1=\"2\" x2=\"50\" y2=\"4\"
                  style=\"stroke:%FG%;stroke-width:1\"/>
        </symbol>
        <symbol id=\"ticklong\">
            <line x1=\"50\" y1=\"2\" x2=\"50\" y2=\"9\"
                  style=\"stroke:%FG%;stroke-width:2\"/>
        </symbol>
        <symbol id=\"hand-hour\">
            <line x1=\"50\" y1=\"22\" x2=\"50\" y2=\"54\"
                  style=\"stroke:%FG%;stroke-width:3\"/>
        </symbol>
        <symbol id=\"hand-minute\">
            <line x1=\"50\" y1=\"12\" x2=\"50\" y2=\"55\"
                  style=\"stroke:%FG%;stroke-width:3\"/>
        </symbol>
        <symbol id=\"hand-second\">
            <line x1=\"50\" y1=\"10\" x2=\"50\" y2=\"59\"
                  style=\"stroke:%FG%;stroke-width:0.5\"/>
        </symbol>
        <g id=\"minute-ticks-a\">
           <use xlink:href=\"#tick\"
                transform=\"rotate(6, 50, 50)\" />
           <use xlink:href=\"#tick\"
                transform=\"rotate(12, 50, 50)\" />
           <use xlink:href=\"#tick\"
                transform=\"rotate(18, 50, 50)\" />
           <use xlink:href=\"#tick\"
                transform=\"rotate(24, 50, 50)\" />
        </g>
        <g id=\"minute-ticks-b\">
            <use xlink:href=\"#minute-ticks-a\"
                 transform=\"rotate(0, 50, 50)\" />
            <use xlink:href=\"#minute-ticks-a\"
                 transform=\"rotate(30, 50, 50)\" />
            <use xlink:href=\"#minute-ticks-a\"
                 transform=\"rotate(60, 50, 50)\" />
            <use xlink:href=\"#minute-ticks-a\"
                 transform=\"rotate(90, 50, 50)\" />
            <use xlink:href=\"#minute-ticks-a\"
                 transform=\"rotate(120, 50, 50)\" />
            <use xlink:href=\"#minute-ticks-a\"
                 transform=\"rotate(150, 50, 50)\" />
        </g>

        <g id=\"5-minute-ticks\">
           <use xlink:href=\"#ticklong\" />
           <use xlink:href=\"#ticklong\" transform=\"rotate(30, 50, 50)\" />
           <use xlink:href=\"#ticklong\" transform=\"rotate(60, 50, 50)\" />
        </g>

        <g id=\"clock\">
          <use xlink:href=\"#5-minute-ticks\"
               transform=\"rotate(0, 50, 50)\" />
          <use xlink:href=\"#5-minute-ticks\"
               transform=\"rotate(90, 50, 50)\" />
          <use xlink:href=\"#5-minute-ticks\"
               transform=\"rotate(180, 50, 50)\" />
          <use xlink:href=\"#5-minute-ticks\"
               transform=\"rotate(270, 50, 50)\" />

          <use xlink:href=\"#minute-ticks-b\"
               transform=\"rotate(0, 50, 50)\" />
          <use xlink:href=\"#minute-ticks-b\"
               transform=\"rotate(180, 50, 50)\" />


          <use xlink:href=\"#hand-second\"
               transform=\"rotate(%SECOND%, 50, 50)\">
          </use>
          <use xlink:href=\"#hand-minute\"
               transform=\"rotate(%MINUTE%, 50, 50)\">
          </use>
          <use xlink:href=\"#hand-hour\"
               transform=\"rotate(%HOUR%, 50, 50)\">
          </use>


          <circle cx=\"50\" cy=\"50\" r=\"3\" fill=\"%FG%\"/>
        </g>
    </defs>
    <use xlink:href=\"#clock\"
         transform=\"scale(%SCALE%, %SCALE%)\"/>
</svg>"
  "The template for drawing the `svg-clock'.")

(defun svg-clock-color-to-hex (colour)
  "Return hex representation of COLOUR."
  (let ((values (color-values colour)))
    (format "#%04x%04x%04x" (nth 0 values) (nth 1 values) (nth 2 values))))

(defun svg-clock-replace (from to)
  "Replace all occurrences of FROM with TO."
  (goto-char (point-min))
  (while (re-search-forward from nil t)
    (replace-match to)))

(defun svg-clock-do-update (time)
  "Make the clock display TIME.
TIME must have the form (SECOND MINUTE HOUR ...), as returned by `decode-time'."
  (with-current-buffer (get-buffer-create "*clock*")
    (let* ((inhibit-read-only t)
           (seconds (nth 0 time))
           (minutes (nth 1 time))
           (hours (nth 2 time))
           (bg-colour (svg-clock-color-to-hex (face-background 'default)))
           (fg-colour (svg-clock-color-to-hex (face-foreground 'default))))
      (erase-buffer)
      (insert svg-clock-template)

      (svg-clock-replace "%BG%" bg-colour)
      (svg-clock-replace "%FG%" fg-colour)
      (svg-clock-replace "%HOUR%" (format "%f" (+ (* hours 30) (/ minutes 2.0))))
      (svg-clock-replace "%MINUTE%" (format "%f" (+ (* minutes 6)
                                                   (/ seconds 10.0))))
      (svg-clock-replace "%SECOND%" (format "%f" (* seconds 6)))
      (svg-clock-replace "%SIZE%" (format "%d" svg-clock-size))
      (svg-clock-replace "%SCALE%" (format "%f" (/ svg-clock-size 100.0)))
      (image-toggle-display-image))))

(defun svg-clock-update ()
  "Update the clock."
  (svg-clock-do-update (decode-time (current-time))))

;;;###autoload
(defun svg-clock ()
  "Start/stop the svg clock."
  (interactive)
  (if svg-clock-timer
      (progn
        (cancel-timer svg-clock-timer)
        (setq svg-clock-timer nil)
        (message "Clock stopped"))
    (switch-to-buffer (get-buffer-create "*clock*"))
    (setq svg-clock-timer
          (run-with-timer 0 1 'svg-clock-update))
    (message "Clock started")))

(provide 'svg-clock)

;;; svg-clock.el ends here
