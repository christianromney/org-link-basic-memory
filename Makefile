.POSIX:
.PHONY: test clean clean-test-projects

EMACS ?= emacs
TEST_FILES = test/org-link-basic-memory-test.el

test:
	@echo "Running org-link-basic-memory tests..."
	$(EMACS) -batch -L . -l test/org-link-basic-memory-test.el \
		-f ert-run-tests-batch-and-exit

clean:
	@echo "Cleaning compiled test files..."
	@rm -f test/*.elc

clean-test-projects:
	@echo "Removing basic-memory test projects..."
	@basic-memory project list 2>&1 | grep -i test | awk '{print $$1}' | xargs -I {} basic-memory project remove {} 2>&1 | grep -v Deprecation || true

help:
	@echo "Available targets:"
	@echo "  test                 - Run the test suite"
	@echo "  clean                - Remove compiled test files"
	@echo "  clean-test-projects  - Remove basic-memory projects with 'test' in name"
	@echo "  help                 - Show this help message"
