#!/usr/bin/env make -f

.PHONY: clean check help build FORCE

HERE := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
TOP  := $(abspath $(HERE)/..)
SCRIPTS = $(TOP)/scripts
BENCHMARKS = $(TOP)/benchmarks
RUNTIME = $(TOP)/runtime

# Not Makefile's responsibility
# REFDIR := $(abspath $(TOP)/../)/smlnj-reference
LLVMDIR := $(abspath $(TOP)/../)/smlnj-llvm21

PY  := $(SCRIPTS)/python-env/bin/python
PIP := $(SCRIPTS)/python-env/bin/pip

help:
	@echo "Building and benchmarking utilities"
	@echo ""
	@echo "TARGETS:"
	@echo "  help           - show this message"
	@echo "  check          - ensure all required programs are installed"
	@echo "  clean          - remove collected data"
	@echo "  llvm           - install all LLVM dependencies"
	@echo "  build          - pre-build all compilers for benchmarking"

check: | $(PY)
	$(CC) --version && echo
	$(CXX) --version && echo
	$(PY) --version && echo
	$(PY) -c 'import numpy; import pandas; import matplotlib; import rich' && echo
	autoconf --version && echo
	cmake --version && echo
	patch --version && echo
	cd $(LLVMDIR) && ./build-llvm.sh -config && rm -rf ./build && echo
	@echo Check OK

clean:
	rm -f $(BENCHMARKS)/*-data || true
	rm -f $(SCRIPTS)/benchmark-results.json $(SCRIPTS)/cloc.json

$(PY):
	cd $(HERE) && python3 -m venv python-env && $(PIP) install -r requirements.txt

build: $(TOP)/bin/sml | $(PY)
	@echo Build completed

cloc.json: $(wildcard $(BENCHMARKS)/*.sml)
	cd $(BENCHMARKS) && cloc --by-file --json --out=cloc.json .
	mv $(BENCHMARKS)/cloc.json .

.PHONY: llvm
llvm: $(TOP)/bin/.stamp $(TOP)/include/.stamp $(TOP)/lib/.stamp

%/bin/sml: %/bin/.stamp %/include/.stamp %/lib/.stamp
	# get the bootstrapping compiler (compiler0), build environment with
	# compiler0, and use compiler0 to compile the compiler (compiler1)
	cd $* && \
		(rm -f boot.*.tgz || true) && \
		find . -type d -name '.cm' -prune -print -exec rm -rf {} \; && \
		rm -rf bin lib && \
		./build.sh && \
		cd system && \
		./cmb-make ../bin/sml && \
		./iter-make 4 && \
		./makeml && \
		./installml -cleanall -boot
	# rebuild the environment with compiler3
	cd $* && ./build.sh

%/bin/.stamp: $(LLVMDIR)/bin/.stamp
	mkdir -p "$(@D)"
	cp -r $(LLVMDIR)/bin/* "$(@D)"

%/include/.stamp: $(LLVMDIR)/include/.stamp
	mkdir -p "$(@D)"
	cp -r $(LLVMDIR)/include/* "$(@D)"

%/lib/.stamp: $(LLVMDIR)/lib/.stamp
	mkdir -p "$(@D)"
	cp -r $(LLVMDIR)/lib/* "$(@D)"

$(LLVMDIR)/lib/.stamp $(LLVMDIR)/bin/.stamp $(LLVMDIR)/include/.stamp:
	# cd $(LLVMDIR) && ./build-llvm.sh
	touch $(LLVMDIR)/lib/.stamp $(LLVMDIR)/bin/.stamp $(LLVMDIR)/include/.stamp

