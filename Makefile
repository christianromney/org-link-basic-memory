.POSIX:
.PHONY: test clean

EMACS ?= emacs
TEST_FILES = test/org-link-basic-memory-test.el

test:
	@echo "Running org-link-basic-memory tests..."
	$(EMACS) -batch -L . -l test/org-link-basic-memory-test.el \
		-f ert-run-tests-batch-and-exit

clean:
	@echo "Cleaning compiled test files..."
	@rm -f test/*.elc

help:
	@echo "Available targets:"
	@echo "  test   - Run the test suite"
	@echo "  clean  - Remove compiled test files"
	@echo "  help   - Show this help message"
