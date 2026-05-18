CASK ?= $(shell command -v cask 2>/dev/null || printf '%s/.cask/bin/cask' "$(HOME)")

.PHONY: test test-legacy

test:
	$(CASK) exec buttercup -L . tests

test-legacy:
	$(CASK) exec buttercup -L . --load tests/legacy/netz-legacy.el --pattern Netz
