;;; munge-deva-libs.el --- Functions to fix spacing in IAST -> Devanāgarī conversions -* lexical-let: t *-

;; Copyright (C) 2019 Patrick McAllister

;; Author: Patrick McAllister <pma@rdorte.org>
;; Version: 0.1
;; Package-Requires: ((indian-ext) (xmltok) (subr-x))
;; Keywords: i18n, tools
;; URL: git://gitolite@rdorte.org:stei

;;; Commentary:

;; This package provides functions to correct the spacing between
;; words when transforming IAST encoded Sanskrit to Devanāgarī.

(require 'xmltok)
(require 'subr-x)
(or
 (require 'indian-ext nil 'noerr)
 (progn
   (push "./indian-ext" load-path)
   (require 'indian-ext)))


(defcustom munge-deva-stoppers '("p" "l" "lg" "pc")
  "Don’t munge across these elements."
  :group 'munge-deva
  :type 'list)


(defun hacks/deva-make-stand-alone-vowel-additional (string)
  "If STRING is a free-standing devanāgarī vowel, change it to an added one."
  (let ((map '(("ॡ" . "ḹ")
               ("ॠ" . "ṝ")
               ("औ" . "ौ")
               ("ओ" . "ो")
               ("ऐ" . "ै")
               ("ए" . "े")
               ("ऌ" . "ळ्")
               ("ऋ" . "ृ")
               ("ऊ" . "ू")
               ("उ" . "ु")
               ("ई" . "ी")
               ("इ" . "ि")
               ("आ" . "ा")
               ("अ" . ""))))
    (alist-get (string-trim string) map string nil #'string=)))

;; (string= (hacks/deva-make-stand-alone-vowel-additional "ई") "ी")

;; (hacks/merge-deva "शब्दो"  "ऽपि")

(defun hacks/merge-deva (s1 s2)
  "Merge strings S1 and S2 so that the devanāgarī looks good."
  (indian-ext-dev-iast-decode-string
   (indian-ext-dev-iast-encode-string (string-join `(,s1 ,s2) ""))))

;; (string= (apply #'hacks/merge-deva (split-string "तद् यथा")) "तद्यथा")
;; (string= (apply #'hacks/merge-deva (split-string "तद् ---")) "तद्यथा")

;; (setq hacks/deva-one-char-regex "\\ci")
(defvar hacks/deva-one-char-regex "\\ci")

(defun hacks/deva-classify-char (char)
  "Classify char according to the devanāgarī Unicode blocks.

See https://www.unicode.org/charts/PDF/U0900.pdf"
  (cond
   ((= char #x902)
    'anusvāra)
   ((or
     (<= #x900 char #x903)
     (<= #x93c char #x93d))
    'various-sign)
   ((or
     (<= #x904 char #x914)
     (<= #x960 char #x961)
     (= #x972 char)
     (<= #x973 char #x975)
     (<= #x976 char #x977))
    'independent-vowel)
   ((or
     (<= #x915 char #x939)
     (<= #x958 char #x95f)
     (<= #x978 char #x97a)
     (<= #x97b char #x97c)
     (= #x97d char)
     (<= #x97e char #x97f))
    'consonant)
   ((or (<= #x93a char #x93b)
        (<= #x93e char #x94c)
        (<= #x94e char #x94f)
        (= #x955 char)
        (<= #x956 char #x957)
        (<= #x962 char #x963))
    'dependent-vowel)
   ((= #x94d char)
    'virāma)
   ((= #x950 char)
    'oṃ)
   ((<= #x951 char #x952)
    'vedic-tone-mark)
   ((<= #x953 char #x954)
    'accent-mark)
   ((<= #x964 char #x965)
    'punct)
   ((<= #x966 char #x96f)
    'digit)
   ((<= #x970 char #x971)
    'abbreviation)
   ((<= #x972 char #x97)
    'abbreviation)
   (t nil)))

;; (equal?
;;  (hacks/deva-classify-char (string-to-char "ं"))
;;  'various-sign)


(defun hacks/forward-char-deva (&optional pos)
  "At POS or point, move forward one ‘character’ in devanāgarī
  encoding."
  (goto-char (or pos (point)))
  (let ((origin (point))
        string-start
        done?
        chars-to-skip)
    (save-match-data
      ;; skip space
      (if (looking-at (rx-to-string '(1+ space)))
          (goto-char (match-end 0)))
      (setq string-start (point))
      (while (and (not done?) (not (eobp)))
        (push (string-to-char (buffer-substring-no-properties (point) (1+ (point))))
              chars-to-skip)
        ;; Devanāgarī (and similar) unicode range:
        ;; (char-to-string #x900) up to (char-to-string #x97f)
        (let ((type (hacks/deva-classify-char (car chars-to-skip))))
          ;; Decide what to do and how much to advance
          (cond
           ;; A free standing vowel?
           ((eq type 'independent-vowel)
            (setq done? t)
            (goto-char (1+ (point))))
           ;; A dependent vowel: split if preceded by something
           ((eq type 'dependent-vowel)
            (when (and
                   (<= 2 (length chars-to-skip))
                   (eq 'consonant (hacks/deva-classify-char (cadr chars-to-skip))))
              (setq done? t))
            ;; and advance scan in any case
            (goto-char (1+ (point))))
           ;; A virāma
           ((eq type 'virāma)
            (goto-char (1+ (point))))
           ((eq type 'anusvāra)
            (setq done? t)
            (goto-char (1+ (point))))
           ;; A consonant?
           ((eq type 'consonant)
            ;; We can cut before a consonant if it follows another consonant.
            (if (and (cadr chars-to-skip)
                     (eq 'consonant (hacks/deva-classify-char (cadr chars-to-skip))))
                ;; We’re done, but don’t advance
                (setq done? t)
              ;; We still need something
              (goto-char (1+ (point)))))
           (t
            ;; Got lost, continue anyway
            (setq done? t)
            ;; (goto-char (1+ (point)))
            ))))
      (point))))


(defun hacks/backward-char-deva (&optional pom)
  "Return the start of the devanāgarī ‘character’  ending at point-or-marker POM."
  (with-current-buffer (if (markerp pom) (marker-buffer pom) (current-buffer))
    (let ((to-check
	   (nreverse
	    (string-to-list
	     (buffer-substring-no-properties (max (- pom 10) (point-min)) pom))))
	  ;; Whether to keep going: either ’dunno, ’yes, or nil
	  (one-more-p 'dunno))
      (- pom
	 (length
	  (seq-take-while
	   (lambda (x)
	     (cond
	      ;; Not in the Devanāgarī range
	      ((not (<= 2304 x 2431))
	       nil)
	      ;; A virāma: keep going for (at least) one more
	      ((eq x 2381)
	       (setq one-more-p 'yes) t)
	      ;; We’ve had enough!
	      ((or (not one-more-p)
		   ;; Or punctuation
		   (eq (get-char-code-property x 'general-category) 'Po))
	       nil)
	      ;; This could be a singleton character, so let’s say
	      ;; we’ve had enough.
	      ((eq (get-char-code-property x 'general-category) 'Lo)
	       (setq one-more-p nil)
	       t)
	      ((eq one-more-p t)
	       t)
	      (t t)))
	   to-check))))))


;; (string-to-list "बी") '(2348 2368)

(defun hacks/unsplit-deva (buffer s1-end-point s2-start-point scanstate)
  "Merge a devanāgarī string in BUFFER.

Merge the string ending at S1-END-POINT with the string starting
at S2-START-POINT.  S1-END-POINT should be where searching for a
virāma (char-to-string 2381) lands you, and S2-START-POINT should
be before the first char of the string that should be connected.

SCANSTATE is the list of xml tokens found between S1-END-POINT
and S2-START-POINT (e.g., ‘((type xmltok-start token-end))’)."
  (with-current-buffer buffer
    (message "Merging deva at %s and at %s: <munge>%s</munge>"
             s1-end-point s2-start-point
             (buffer-substring-no-properties (max 1 (- s1-end-point 10))
                                             (min (+ s2-start-point 10)
                                                  (point-max))))
    (save-match-data
      (xmltok-save
        (let* ((s1-end-point (if (markerp s1-end-point)
                                 s1-end-point
                               (set-marker (make-marker) s1-end-point)))
               (s1-start-point (set-marker (make-marker) (hacks/backward-char-deva s1-end-point)))
               (s2-end-point (save-excursion
                               (save-match-data
                                 (goto-char s2-start-point)
                                 (hacks/forward-char-deva)
                                 (set-marker (make-marker) (point)))))
               (s2-start-point (if (markerp s2-start-point)
                                   s2-start-point
                                 (set-marker (make-marker) s2-start-point))))
          (unless (or (string-empty-p
                       (string-trim
                        (buffer-substring-no-properties s1-start-point s1-end-point)))
                      (string-empty-p
                       (string-trim
                        (buffer-substring-no-properties s2-start-point s2-end-point))))
            ;; Okay, replace intervening spaces first
            (mapc
             (lambda (x)
               (let ((type (car x))
                     (start (cadr x))
                     (end (caddr x)))
                 (when (equal type 'space)
                   (delete-region start end))))
             scanstate)
            (let ((s1-extracted (string-trim
                                 (delete-and-extract-region s1-start-point s1-end-point)))
                  (s2-extracted (string-trim
                                 (delete-and-extract-region s2-start-point s2-end-point))))
	      ;; For a start-tag, move things into the tag. Everything
	      ;; else (only end-tag or empty, I suppose) move it
	      ;; before the tag (into the preceding element, or before
	      ;; the empty element). TODO: consider corner cases such
	      ;; as ‘(start-tag space end-tag).  The following works
	      ;; for simple cases and this case: '((data 31 36)
	      ;; (start-tag 8 31) (space 7 8))
	      (if (and (assoc 'start-tag scanstate)
		       (not (assoc 'end-tag (assoc 'start-tag scanstate))))
		  (goto-char s2-start-point)
		  (goto-char s1-end-point))
	      ;; (delete-horizontal-space)
              (insert
               (hacks/merge-deva s1-extracted s2-extracted))
              (message "Merge result for %s to %s: <munged>%s</munged>, \nScanstate was: %s"
                       s1-end-point s2-start-point
                       (buffer-substring-no-properties (max 1 (- s1-end-point 10))
                                                       (min (+ s2-start-point 10)
                                                            (point-max)))
		       scanstate)))
          (goto-char s2-start-point)
          (set-marker s1-start-point nil)
          (set-marker s1-end-point nil)
          (set-marker s2-start-point nil)
          (set-marker s2-end-point nil))))))


;;; A flexible member function
;; (cl-member
;;  'data
;;  (cdr
;;   '((data 34 . 36)
;;     (empty-element 9 . 34)
;;     (data 4 . 9)
;;     (start-tag 1 . 4)))
;;  :test (lambda (x y)
;;          (eq x (car y))))

(defun munge-deva-clean-up-for-avagraha (string)
  "Normalize spacing in STRING when it contains an avagraha."
  (let ((avagraha-rx
         (rx-to-string
          '(and (group-n 1 (not (any punct space))) (group-n 2 (1+ space)) (group-n 3 2365)))))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward avagraha-rx nil 'noerr)
         (replace-match "\\1\\3")))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun munge-deva-analyze-xml-for-avagraha (xml-buffer)
  "Analyze XML-BUFFER for fixing of avagraha spaces.

This function returns a token-list in the reverse order of
occurrence (parsed backwards, so to speak).  This makes
string-replacements by range easier.

The structure of each token in the returned list is:

‘((type . ,xmltok-type)
 (start . ,xmltok-start)
 (name . ,(or local-name 'ignore:anonym))
 (end . ,(point))
 (ended-in-punct-p . ,(or t nil)))’."
  (let ((punct-and-maybe-space-rx (rx-to-string '(and punct (0+ space) eos)))
	token-list)
    (save-excursion
      (save-match-data
	(xmltok-save
	 (while (xmltok-forward)
	   (push `((type . ,xmltok-type)
		   (start . ,xmltok-start)
		   (name . ,(cond
			     ((member xmltok-type '(empty-element start-tag))
			      (xmltok-start-tag-local-name))
			     ((member xmltok-type '(end-tag))
			      (xmltok-end-tag-local-name))
			     (t 'ignore:anonym)))
		   (end . ,(point))
		   (ended-in-punct-p
		    .
		    ,(if (and (eq xmltok-type 'data)
			      (string-match-p punct-and-maybe-space-rx
					      (buffer-substring-no-properties
					       xmltok-start (point))))
			 t
		       ;; or inherit value from last token
		       (cdr (assoc 'ended-in-punct-p (car token-list))))))
		 token-list)))))
    ;; returns token-list in reverse sequence of occurrence
    token-list))

(defun munge-deva-fix-deva-splits-and-spacing (xml-buffer)
  "Fix spacing for avagrahas in XML-BUFFER.

See ‘munge-tests.el’ for some examples."
  (save-excursion
    (save-match-data
      (xmltok-save
        (goto-char (point-min))
        (let ((token-list (munge-deva-analyze-xml-for-avagraha xml-buffer))
              (avagraha-at-start-rx (rx-to-string `(and bos (0+ space) 2365)))
	      to-clean)
	  ;; analyze xmldoc and save things to fix to to-clean
          (while token-list
	    (let ((current-token (pop token-list)))
              (cond
               ((eq (cdr (assoc 'type current-token)) 'data)
		;; does this *START* with an avagraha: then we have to
		;; go backward through the token-list
		(let ((current-data (buffer-substring-no-properties
                                     (cdr (assoc 'start current-token))
                                     (cdr (assoc 'end current-token)))))
                  (cond
                   ((string-match-p avagraha-at-start-rx current-data)
                    ;;  Fix depending on tokens before: since the
                    ;;  ended-in-punct-p property is inherited, it’s
                    ;;  okay to just look at the last value.
		    (unless (cdr (assoc 'ended-in-punct-p (car token-list)))
                      ;; Go through the token-list up to the last preceding data token,
		      (push (list 'delete-preceding-space current-token) to-clean)
		      (cl-loop for token in token-list
			       until (eq (cdr (assoc 'type token)) 'data)
			       ;; and for each token on the way: 
			       do (cond
				   ((eq (assoc 'type token) 'space)
				    (push (list 'kill-space token) to-clean))
;;; this part can never be reached, I think 
				   ;; ((eq (assoc 'type token) 'data)
				   ;;  (push (list 'delete-trailing-space token) to-clean))
				   (t t))
			       finally (push (list 'delete-trailing-space token) to-clean))))
		   ;; contains an avagraha --> fix internal spaces 
                   ((string-match-p (char-to-string 2365) current-data)
                    (push (list 'fix-internal current-token) to-clean))
                   (t t))))
               (t t))))
	  ;; do the actual cleanup
	  (atomic-change-group
	    (mapc
	     (lambda (item)
	       (let ((what-todo (car item))
		     (from (cdr (assoc 'start (cadr item))) )
		     (to (cdr (assoc 'end (cadr item)))))
		 (save-excursion
		   (goto-char from)
		   (cond
		    ((eq what-todo 'fix-internal)
		     (insert (munge-deva-clean-up-for-avagraha (delete-and-extract-region from to))))
		    ((eq what-todo 'kill-space)
		     (delete-and-extract-region from to))
		    ((eq what-todo 'delete-trailing-space)
		     (insert (string-trim-right (delete-and-extract-region from to))))
		    ((eq what-todo 'delete-preceding-space)
		     (insert (string-trim-left (delete-and-extract-region from to))))
		    (t t)))))
	     ;; Do backwards, from the end of the buffer:
	     (nreverse to-clean))))))))


;; (let ((token-list '(a b c d e f g))
;;       results)
;;   (cl-loop for token in (cdr token-list)
;; 	   until (eq (car results) 'e)
;; 	   do (push token results))
;;   (nreverse results))


(defun munge-deva-fix-deva-breaks (&optional xml-buffer stoppers interactive?)
  "Fix breaks in Devanāgarī text respecting the elements in STOPPERS.

Operates on XML-BUFFER (default: ‘current-buffer’).

STOPPERS (default: ‘munge-deva-stoppers’) is a list of the
non-namespaced element names (as strings) whose content should
not be changed in view of what precedes or follows these
elements.  Within the elements, the breaks are always fixed.  To
specify no stoppers, set STOPPERS to '(nil) or anything that’s
not a list.

Scans over the xml structure moving things around so that
devanāgarī doesn’t look weird.

Returns a new buffer containing the fixed text."
  (interactive
   (list (current-buffer)
	 munge-deva-stoppers
	 'interactive))
  (let ((mod-buff (with-current-buffer (get-buffer-create "*fix deva breaks*")
                    (erase-buffer)
                    (when interactive?
                      (pop-to-buffer (current-buffer)))
                    (insert-buffer-substring-no-properties (or xml-buffer (current-buffer)))
                    (current-buffer)))
        (end-to-complement-rx (rx-to-string `(and ,(char-to-string 2381)
                                                  word-boundary)))
        ;; don’t munge across these elements
        (stoppers (cond
		   (stoppers stoppers)
		   ((or (eq '(nil) stoppers)
			(not (listp stoppers)))
		    '())
		   (munge-deva-stoppers munge-deva-stoppers)
		   (t '("p" "l" "lg" "pc" "quote"))))
        (lonely-visarga-rx (rx-to-string `(and
                                           (group-n 1 "<" (1+ (not ">")) ">")
                                           ,(char-to-string 2307)))))
    (with-current-buffer mod-buff
      (when interactive?
	(pop-to-buffer (current-buffer)))
      (goto-char (point-min))
      (while (re-search-forward end-to-complement-rx nil 'noerror)
        (save-match-data
	  (xmltok-save
	    (let ((end-point (point))
		  (connected nil)
		  (scanstate '()))
	      (while (and (not (eobp)) (not connected) (xmltok-forward))
	        ;; Keep track of what’s going on
	        (push (list xmltok-type xmltok-start (point)) scanstate)
	        (cond
		 ((eq xmltok-type 'start-tag)
		  (when (member (xmltok-start-tag-local-name) stoppers)
		    (setq connected t)))
		 ((eq xmltok-type 'end-tag)
		  ;; If this closes where we started, don’t continue.
		  (cond
		   ;; Bad idea: tad</name></quote>iti won’t work then!
		   ;; ((null scanstate)
		   ;;  (setq connected t))
		   ((member (xmltok-end-tag-local-name) stoppers)
		    (setq connected t))
		   (t t)))
		 ((eq xmltok-type 'data)
		  ;; We were in the middle of a string: do your stuff,
		  ;; unless it’s followed by punctuation.
		  (goto-char end-point)
		  (unless (string-match-p
			   (rx-to-string `(and (0+ space) punct))
			   (buffer-substring-no-properties xmltok-start (1+ xmltok-start)))
		    (hacks/unsplit-deva (current-buffer) end-point xmltok-start scanstate))
		  ;; This place needs no further work!
		  (setq connected t))
		 (t t)))))))
      (goto-char (point-min))
      ;; Change <[/]tag>+visarga to  visarga<[/]tag>, assuming you can always do that.
      (while (re-search-forward lonely-visarga-rx nil 'noerr)
        (let ((tag-stuff (match-string 1)))
          (goto-char (match-beginning 0))
          (delete-region (point) (match-end 0))
          (insert (seq-concatenate 'string (char-to-string 2307) tag-stuff))))
      (munge-deva-fix-deva-splits-and-spacing mod-buff))
    ;; return buffer with fixed string
    mod-buff))

(defun munge-deva-fix-deva-breaks-in-string (str &optional stoppers interactive?)
  "Munge string STR.

This is just a wrapper around ‘munge-deva-fix-deva-breaks’, which
explains what STOPPERS and INTERACTIVE? do."
  (with-temp-buffer
    (insert str)
    (with-current-buffer (munge-deva-fix-deva-breaks (current-buffer) stoppers nil)
      (buffer-string))))



;; (butlast '(0 1 2 3 4) 0)

(provide 'munge-deva-libs)
