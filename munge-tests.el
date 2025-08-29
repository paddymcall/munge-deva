;; Typical invocation:

;; emacs -Q -batch -l ert -l munge-deva-libs.el -l munge-tests.el -f ert-run-tests-batch-and-exit

(unless noninteractive
  (require 'ert))

(ert-deftest test-backward-char-deva ()
  (let ((cases '(("अ" 2 "अ")
		 ("अत्र" nil "त्र")
		 ("द्यो" nil "द्यो")
		 ("क्षणभङ्गे" nil "ङ्गे")
		 ("क्षणभङ्गे" 4 "क्ष")
		 ("क्षणभङ्गे" 5 "ण")
		 ;; Not a real example:
		 ("अन्यत्त्तु" nil "त्त्तु")
		 ("अन्यं।" nil ""))))
    (mapc
     (lambda (c)
       (with-temp-buffer
	 (insert (car c))
	 (let ((result ))
	   
	   (should
	    (equal
	     (buffer-substring-no-properties
	      (hacks/backward-char-deva (or (cadr c) (point-max)))
	      (or (cadr c) (point-max)))
	     (caddr c))))))
     cases)))

;; (ert "test-backward-char-deva")

(ert-deftest test-split-and-combine-vowels ()
  (let ((cases '(("तद् इति" . "तदिति")
                 ("तद् अस्ति" . "तदस्ति"))))
    (mapc
     (lambda (c)
       (should
        (equal
         (apply #'hacks/merge-deva (split-string (car c)))
         (cdr c))))
     cases)))

;; (ert "test-split-and-combine-vowels")


(ert-deftest test-punctuation-cases ()
  (let ((cases '(("तत्. इति" . "तत्।इति")
                 ("स्यत्. यथा" . "स्यत्।यथा"))))
    (mapc
     (lambda (c)
       (should
        (equal
         (apply #'hacks/merge-deva (split-string (car c)))
         (cdr c))))
     cases)))

;; (ert "test-punctuation-cases")

(ert-deftest test-avagraha-cases ()
  (let ((cases '(("शब्दो ऽपि" . "शब्दोऽपि"))))
    (mapc
     (lambda (c)
       (should
        (equal
         (apply #'hacks/merge-deva (split-string (car c)))
         (cdr c))))
     cases)))

;; (ert "test-avagraha-cases")

(ert-deftest test-avagraha-cases-xml ()
  (let ((cases '(("<p>शब्दो ऽपि</p>" . "<p>शब्दोऽपि</p>")
                 ("<p>शब्दो\nऽपि</p>" . "<p>शब्दोऽपि</p>")
                 ;; Should this change? We don’t know what happened
                 ;; before! But, let’s clean it up anyway.
                 ("<p> ऽपि</p>" . "<p>ऽपि</p>")
                 ("<p>शब्दो. ऽपि</p>" . "<p>शब्दो. ऽपि</p>")
                 ("<p>शब्दो<anchor xml:id=\"abc123\"/> ऽपि</p>" .
                  "<p>शब्दो<anchor xml:id=\"abc123\"/>ऽपि</p>")
                 ("<p>शब्दो.<anchor xml:id=\"abc123\"/> ऽपि</p>" .
                  "<p>शब्दो.<anchor xml:id=\"abc123\"/> ऽपि</p>")
                 ("<p>शब्दो <quote>ऽपि</quote></p>" .
                  "<p>शब्दो<quote>ऽपि</quote></p>")
                 ("<p>शब्दो <quote>   ऽपि</quote></p>" .
                  "<p>शब्दो<quote>ऽपि</quote></p>"))))
    (mapc
     (lambda (c)
       (should
        (equal
         (with-temp-buffer
           (insert (car c))
           (goto-char (point-min))
           (let ((src-buff (current-buffer)))
             (with-current-buffer (munge-deva-fix-deva-breaks src-buff)
               (buffer-string))))
         (cdr c))))
     cases)))


;; (ert "test-avagraha-cases-xml")


(ert-deftest test-punctuation-cases-xml ()
  (let ((cases '(("तत्. इति" . "तत्. इति")
                 ("स्यत्. यथा" . "स्यत्. यथा")
                 ;; these are just tests for identity: nothing should be munged here
                 ("<div>स्यात्। न <tei:anchor xml:id=\"W-2958\" />च</div>"
                  . "<div>स्यात्। न <tei:anchor xml:id=\"W-2958\" />च</div>")
                 ("<div><tei:quote>स्यात्</tei:quote>। न <tei:anchor xml:id=\"W-2958\" />च</div>"
                  . "<div><tei:quote>स्यात्</tei:quote>। न <tei:anchor xml:id=\"W-2958\" />च</div>")
                 ("<div><tei:quote>स्याद्</tei:quote> इति। न <tei:anchor xml:id=\"W-2958\" />च</div>"
                  . "<div><tei:quote>स्याद्</tei:quote> इति। न <tei:anchor xml:id=\"W-2958\" />च</div>")
                 ;; Preserve space here between “ity” and “api”:
                 ("<tei:p><tei:anchor xml:id=\"W-60\" />अर्थापत्त्यादीनीत्य् <tei:anchor xml:id=\"W-61\" /><tei:quote type=\"basetext\">अपि</tei:quote></tei:p>"
                  .
                  "<tei:p><tei:anchor xml:id=\"W-60\" />अर्थापत्त्यादीनीत्य् <tei:anchor xml:id=\"W-61\" /><tei:quote type=\"basetext\">अपि</tei:quote></tei:p>")
                 ;; but not here:
                 ("<tei:p>अर्थापत्त्यादीनीत्य् <tei:hi>अपि</tei:hi>शब्दार्थः</tei:p>"
                  . "<tei:p>अर्थापत्त्यादीनी<tei:hi>त्यपि</tei:hi>शब्दार्थः</tei:p>"
		  ;; "<tei:p>अर्थापत्त्यादीनीत्य<tei:hi>पि</tei:hi>शब्दार्थः</tei:p>"
		  )
                 ;; across boundaries
                 ("<div><a>स्याद्</a> इति</div>"
                  . "<div><a>स्यादि</a>ति</div>")
                 ;; space checks
                 ("<p>प्रत्यायनार्थम्<anchor xml:id=\"W-126\"/><anchor xml:id=\"W-128\"/> उच्चार्यते</p>"
                  . "<p>प्रत्यायनार्थमु<anchor xml:id=\"W-126\"/><anchor xml:id=\"W-128\"/>च्चार्यते</p>")
                 ("<p>प्रत्यायनार्थम्<anchor xml:id=\"W-126\"/> <anchor xml:id=\"W-128\"/> उच्चार्यते</p>"
                  . "<p>प्रत्यायनार्थमु<anchor xml:id=\"W-126\"/><anchor xml:id=\"W-128\"/>च्चार्यते</p>")
                 ("<p>प्रत्यायनार्थम्<anchor xml:id=\"W-126\"/> <anchor xml:id=\"W-128\"/>उच्चार्यते</p>"
                  . "<p>प्रत्यायनार्थमु<anchor xml:id=\"W-126\"/><anchor xml:id=\"W-128\"/>च्चार्यते</p>")
                 ;; no change here
                 ("<p>प्रत्यायनार्थम्<anchor xml:id=\"W-126\"/> <anchor xml:id=\"W-128\"/> <quote>उच्चार्यते</quote></p>"
                  .
                  "<p>प्रत्यायनार्थम्<anchor xml:id=\"W-126\"/> <anchor xml:id=\"W-128\"/> <quote>उच्चार्यते</quote></p>")
                 ("<p>स्याद् <anchor xml:id=\"W-2576\"/>एतत् <anchor xml:id=\"W-2577\"/>– <anchor xml:id=\"W-2578\"/>सत्ताया</p>" .
                  "<p>स्यादे<anchor xml:id=\"W-2576\"/>तत् <anchor xml:id=\"W-2577\"/>– <anchor xml:id=\"W-2578\"/>सत्ताया</p>")))
	(pst5-stoppers '("p" "l" "lg" "pc" "quote")))
    (mapc
     (lambda (c)
       (should
        (equal
         (with-temp-buffer
           (insert (car c))
           (goto-char (point-min))
           (let ((src-buff (current-buffer)))
             (with-current-buffer (munge-deva-fix-deva-breaks src-buff pst5-stoppers)
               (buffer-string))))
         (cdr c))))
     cases)))


;; (ert "test-punctuation-cases-xml")

(ert-deftest test-spacing-cases-with-following-avagraha-xml ()
  (let ((cases
         '(("<p><anchor xml:id=\"W-1746\"/>अथ <anchor xml:id=\"W-1747\"/>कोऽ</p>"
                  .
                  "<p><anchor xml:id=\"W-1746\"/>अथ <anchor xml:id=\"W-1747\"/>कोऽ</p>")
                 ("<p><anchor xml:id=\"W-1746\"/>अथ <anchor xml:id=\"W-1747\"/><anchor n=\"D525\" xml:id=\"W-1748\"/>कोयम्</p>"
                  .
                  "<p><anchor xml:id=\"W-1746\"/>अथ <anchor xml:id=\"W-1747\"/><anchor n=\"D525\" xml:id=\"W-1748\"/>कोयम्</p>")
                 ("<p><anchor xml:id=\"W-1746\"/>अथ <anchor xml:id=\"W-1747\"/><anchor n=\"D525\" xml:id=\"W-1748\"/>कोऽयम्</p>"
                  .
                  "<p><anchor xml:id=\"W-1746\"/>अथ <anchor xml:id=\"W-1747\"/><anchor n=\"D525\" xml:id=\"W-1748\"/>कोऽयम्</p>")
                 ("<tei:p xml:id=\"psṭ-apoha__36r1XCXB3SK7E0HW6LDBNC51ZIN\" corresp=\"ps%E1%B9%AD-tib-acip.xml#ps%E1%B9%AD-tib-acip__36r1XDSCU8AUWV3360AXZYDLBP9\" ana=\"#checked-tibetan\"><tei:anchor xml:id=\"W-1746\" />अथ <tei:anchor xml:id=\"W-1747\" /><tei:anchor xml:id=\"W-1748\" n=\"D525\" />कोऽयम् <tei:anchor xml:id=\"W-1750\" />आक्षेपो <tei:anchor xml:id=\"W-1751\" />नाम। <tei:anchor xml:id=\"W-1752\" /><tei:lb n=\"4\" ed=\"#PSṬ-msB\" />अत्यागः<tei:anchor xml:id=\"W-1753\" n=\"D525\" />।
</tei:p>"
                  .
                  "<tei:p xml:id=\"psṭ-apoha__36r1XCXB3SK7E0HW6LDBNC51ZIN\" corresp=\"ps%E1%B9%AD-tib-acip.xml#ps%E1%B9%AD-tib-acip__36r1XDSCU8AUWV3360AXZYDLBP9\" ana=\"#checked-tibetan\"><tei:anchor xml:id=\"W-1746\" />अथ <tei:anchor xml:id=\"W-1747\" /><tei:anchor xml:id=\"W-1748\" n=\"D525\" />कोऽयमा<tei:anchor xml:id=\"W-1750\" />क्षेपो <tei:anchor xml:id=\"W-1751\" />नाम। <tei:anchor xml:id=\"W-1752\" /><tei:lb n=\"4\" ed=\"#PSṬ-msB\" />अत्यागः<tei:anchor xml:id=\"W-1753\" n=\"D525\" />।
</tei:p>"))))
    (mapc
     (lambda (c)
       (should
        (equal
         (with-temp-buffer
           (insert (car c))
           (goto-char (point-min))
           (let ((src-buff (current-buffer)))
             (with-current-buffer (munge-deva-fix-deva-breaks src-buff)
               (buffer-string))))
         (cdr c))))
     cases)))


;; (ert "test-spacing-cases-with-following-avagraha-xml")

(ert-deftest test-munge-deva-clean-up-for-avagraha ()
  (should
   (equal
    (munge-deva-clean-up-for-avagraha "दो ऽ")
    "दोऽ"))
  (should
   (equal
    (munge-deva-clean-up-for-avagraha "दो. ऽ")
    "दो. ऽ"))
  (should
   (equal
    (munge-deva-clean-up-for-avagraha "शब्दो. ऽपि. शब्दो ऽपि. शब्दो   ऽपि. शब्दो  	ऽपि.")
    "शब्दो. ऽपि. शब्दोऽपि. शब्दोऽपि. शब्दोऽपि.")))
;; (ert "test-munge-deva-clean-up-for-avagraha")


(ert-deftest test-various-practical-problems ()
  (let ((cases '(
		 ;; problem here was the space between anchor W-192 and W-193: caused the deletion of a “<”
		 ("<p><quote type=\"basetext\">कृ<lb ed=\"#PSṬ-msB\" n=\"7\"/>तकत्वादिवद् <anchor xml:id=\"W-192\"/> <anchor xml:id=\"W-193\"/><anchor xml:id=\"W-194\"/>भाषत</quote> <anchor xml:id=\"W-196\"/>इत्य्</p>"
		  . "<p><quote type=\"basetext\">कृ<lb ed=\"#PSṬ-msB\" n=\"7\"/>तकत्वादिवद्भा<anchor xml:id=\"W-192\"/><anchor xml:id=\"W-193\"/><anchor xml:id=\"W-194\"/>षत</quote> <anchor xml:id=\"W-196\"/>इत्य्</p>"))))
    (mapc
     (lambda (c)
       (should
        (equal
         (with-temp-buffer
           (insert (car c))
           (goto-char (point-min))
           (let ((src-buff (current-buffer)))
             (with-current-buffer (munge-deva-fix-deva-breaks src-buff)
               (buffer-string))))
         (cdr c))))
     cases)))

;; (ert "test-various-practical-problems")

;; This lost the space after “tu”
(ert-deftest test-vowel-before-element ()
  (let ((cases '(
                 ("<p><tei:pc type=\"half-daṇḍa\">,</tei:pc> शब्दस् तु <tei:anchor xml:id=\"W-240\" />विधिमुखेन</p>"
                  .
                  "<p><tei:pc type=\"half-daṇḍa\">,</tei:pc> शब्दस्तु <tei:anchor xml:id=\"W-240\" />विधिमुखेन</p>"))))
    (mapc
     (lambda (c)
       (should
        (equal
         (with-temp-buffer
           (insert (car c))
           (goto-char (point-min))
           (let ((src-buff (current-buffer)))
             (with-current-buffer (munge-deva-fix-deva-breaks src-buff)
               (buffer-string))))
         (cdr c))))
     cases)))

;; (ert "test-vowel-before-element")

;; Make sure the “,” doesn’t change things in a weird way
(ert-deftest test-vowel-before-string-or-punct-mixed ()
  (let ((cases
	 '((;; bad sandhi, but it’s just to make a point
	    "सारूप्यम् तन् नास्तीत्य् अर्थः<pc type=\"double-daṇḍa\">॥</pc>"
	    .
	    "सारूप्यम्तन्नास्तीत्यर्थः<pc type=\"double-daṇḍa\">॥</pc>")
	   ("सारूप्यम्, तन् नास्तीत्य् अर्थः<pc type=\"double-daṇḍa\">॥</pc>"
	    .
	    "सारूप्यम्, तन्नास्तीत्यर्थः<pc type=\"double-daṇḍa\">॥</pc>"))))
    (mapc
     (lambda (c)
       (should
	(equal
	 (with-temp-buffer
           (insert (car c))
           (goto-char (point-min))
           (let ((src-buff (current-buffer)))
             (with-current-buffer (munge-deva-fix-deva-breaks src-buff)
               (buffer-string))))
	 (cdr c))))
     cases)))

;; (ert "test-vowel-before-string-or-punct-mixed")

;; Test the stoppers a bit
(ert-deftest test-around-quote-element ()
  (let ((cases '(
		 ("<p>केवलेन्<tei:quote type=\"basetext\">आवगतेर्</tei:quote></p>"
		  ;; the stoppers to apply for the result
		  ("p" "l" "lg" "pc")
		  "<p>केवले<tei:quote type=\"basetext\">नावगतेर्</tei:quote></p>")
		 ("<p>केवलेन्<tei:quote type=\"basetext\">आवगतेर्</tei:quote></p>"
		  ;; the stoppers to apply for the result
		  ("p" "l" "lg" "pc" "quote")
		  "<p>केवलेन्<tei:quote type=\"basetext\">आवगतेर्</tei:quote></p>"))))
    (mapc
     (lambda (c)
       (should
        (equal
         (with-temp-buffer
           (insert (car c))
           (goto-char (point-min))
           (let ((src-buff (current-buffer)))
             (with-current-buffer (munge-deva-fix-deva-breaks src-buff (cadr c))
               (buffer-string))))
         (caddr c))))
     cases)))

;; (ert "test-around-quote-element")

(ert-deftest test-around-element-boundaries ()
  (let ((cases '(;; Around empty-elements
		 ("<p>केवलेन्<anchor xml:id=\"abc123\"/>आवगतेर्</p>"
		  ;; the stoppers to apply for the result
		  (nil)
		  "<p>केवलेना<anchor xml:id=\"abc123\"/>वगतेर्</p>")
		 ;; No change here:
		 ("<p>अवगतेर्<anchor xml:id=\"abc123\"/></p>"
		  ;; the stoppers to apply for the result
		  (nil)
		  "<p>अवगतेर्<anchor xml:id=\"abc123\"/></p>")
		 ;; Around start tags
		 ("<p>केवलेन्<quote xml:id=\"abc123\">आवगतेर्</quote></p>"
		  ;; the stoppers to apply for the result
		  (nil)
		  "<p>केवले<quote xml:id=\"abc123\">नावगतेर्</quote></p>")
		 ("<p>केवलेन्<quote xml:id=\"abc123\">आवगतेर्</quote></p>"
		  ;; the stoppers to apply for the result
		  ("quote")
		  "<p>केवलेन्<quote xml:id=\"abc123\">आवगतेर्</quote></p>")
		 ("<p>केवलेन्<quote xml:id=\"abc123\">आवगतेर्</quote></p>"
		  ;; the stoppers to apply for the result
		  ("quote")
		  "<p>केवलेन्<quote xml:id=\"abc123\">आवगतेर्</quote></p>")
		 ;; end-tag
		 ("<p><quote xml:id=\"abc123\">केवलेन्</quote> आवगतेर्</quote></p>"
		  ;; the stoppers to apply for the result
		  (nil)
		  "<p><quote xml:id=\"abc123\">केवलेना</quote>वगतेर्</quote></p>")
		 ("<p><quote xml:id=\"abc123\">केवलेन्</quote> आवगतेर्</quote></p>"
		  ;; the stoppers to apply for the result
		  ("quote")
		  "<p><quote xml:id=\"abc123\">केवलेन्</quote> आवगतेर्</quote></p>")
		 ("<p><quote type=\"basetext\">यत् तर्हीदम्</quote> इत्यादि<pc type=\"daṇḍa\">|</pc></p>"
		  (nil)
		  "<p><quote type=\"basetext\">यत्तर्हीदमि</quote>त्यादि<pc type=\"daṇḍa\">|</pc></p>")
		 ("<p><quote type=\"basetext\">यत् तर्हीदम्</quote> इत्यादि<pc type=\"daṇḍa\">|</pc></p>"
		  ("quote")
		  ;; Within the quote, we do apply the changes!
		  "<p><quote type=\"basetext\">यत्तर्हीदम्</quote> इत्यादि<pc type=\"daṇḍa\">|</pc></p>")
		 ;; Do nothing:
		 ("तद् <quote type=\"basetext\">उक्तं"
		  ("quote")
		  "तद् <quote type=\"basetext\">उक्तं")
		 ("तद् <quote type=\"basetext\">उक्तं"
		  (nil)
		  "त<quote type=\"basetext\">दुक्तं")
                 ;; Anusvāra case
                 ("<p>दर्शनात् <anchor xml:id=\"a1\"/>संशयः</p>"
                  (nil)
                  "<p>दर्शनात्सं<anchor xml:id=\"a1\"/>शयः</p>")
                 ;; Visarga case
                 ("<tei:quote>निश्चय</tei:quote>ः<tei:pc type=\"half-daṇḍa\">,</tei:pc>"
                  (nil)
                  "<tei:quote>निश्चयः</tei:quote><tei:pc type=\"half-daṇḍa\">,</tei:pc>")
                 )))
    (mapc
     (lambda (c)
       (should
        (equal
         (with-temp-buffer
           (insert (car c))
           (goto-char (point-min))
           (let ((src-buff (current-buffer)))
             (with-current-buffer (munge-deva-fix-deva-breaks src-buff (cadr c))
               (buffer-string))))
         (caddr c))))
     cases)))

;; (ert "test-around-element-boundaries")

(ert-deftest test-string-version ()
  (should
   (equal
    (munge-deva-fix-deva-breaks-in-string
     "<p>दर्शनात् <anchor xml:id=\"a1\"/>संशयः</p>" )
    "<p>दर्शनात्सं<anchor xml:id=\"a1\"/>शयः</p>")))
;; (ert "test-string-version")

(provide 'munge-tests)
;; munge-tests.el ends here
