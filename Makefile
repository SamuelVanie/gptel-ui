EMACS ?= emacs
ELFILES := $(wildcard gptel-ui*.el gptel-runner-chat*.el)
TESTFILES := $(filter-out test/gptel-ui-integration-test.el,$(wildcard test/*-test.el))
INTEGRATION_LOAD_PATH ?=

.PHONY: test integration-test compile checkdoc clean

test: clean
	$(EMACS) -Q --batch -L . -L test $(foreach file,$(TESTFILES),-l $(file)) \
	  -f ert-run-tests-batch-and-exit

integration-test:
	$(EMACS) -Q --batch $(INTEGRATION_LOAD_PATH) -L . -L test \
	  --eval '(setq load-prefer-newer t)' \
	  -l test/gptel-ui-integration-test.el -f ert-run-tests-batch-and-exit

compile: clean
	$(EMACS) -Q --batch -L . --eval \
	  "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $(ELFILES)

checkdoc:
	$(EMACS) -Q --batch -L . --eval \
	  "(progn (require 'checkdoc) (dolist (f command-line-args-left) (checkdoc-file f)))" \
	  $(ELFILES)

clean:
	find . -name '*.elc' -delete
