export IDRIS2 ?= idris2

.PHONY: page
page:
	pack build homelab.ipkg
	# mkdir -p js
	# cp -f build/exec/main.js js/main.js
