;;; gptel-ui-render-benchmark.el --- Local rendering benchmark -*- lexical-binding: t; -*-

;; Run: emacs -Q --batch -L . -l test/gptel-ui-render-benchmark.el
;; Timings are informational; deterministic scaling assertions live in ERT.
;; Example on Emacs 31.1, interpreted source, median of three, seconds:
;;                initial       append token       unchanged refresh
;; 1k, before      0.135000        0.134635             0.136174
;; 1k, after       0.025394        0.002856             0.002551
;; 4k, before      1.863091        1.867549             1.864100
;; 4k, after       0.122456        0.013011             0.013176
;; Each link occupies one 30-byte line; the token starts the following line.
;; Structural rendering and ownership-list traversal remain whole-buffer work.

(require 'gptel-ui)
(require 'benchmark)

(dolist (count '(1000 4000))
  (let (initial append-token unchanged bytes)
    (dotimes (_ 3)
      (with-temp-buffer
        (text-mode)
        (dotimes (_ count) (insert "[link](https://example.com/x)\n"))
        (setq bytes (buffer-size))
        (let ((gc-cons-threshold most-positive-fixnum)
              (gptel-ui-refresh-delay 3600))
          (garbage-collect)
          (unwind-protect
              (progn
                (push (car (benchmark-run 1 (gptel-ui-mode 1))) initial)
                (goto-char (point-max))
                (insert "x")
                (push (car (benchmark-run 1 (gptel-ui-refresh))) append-token)
                (push (car (benchmark-run 1 (gptel-ui-refresh))) unchanged))
            (gptel-ui-mode -1)))))
    (princ (format "%d links / %d bytes: initial %.6fs, append %.6fs, unchanged %.6fs (median of 3)\n"
                   count bytes (nth 1 (sort initial #'<))
                   (nth 1 (sort append-token #'<))
                   (nth 1 (sort unchanged #'<))))))

;;; gptel-ui-render-benchmark.el ends here
