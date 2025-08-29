test:
	emacs -Q -batch \
	-l ert \
	-l munge-deva-libs.el \
	-l munge-tests.el \
	-f ert-run-tests-batch-and-exit
