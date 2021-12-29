files := lib/*.asd lib/src/*.lisp src/%/*.asd src/%/src/*.lisp Makefile
names := $(notdir $(wildcard src/*))

.PHONY: all clean $(names)

all: $(names)

$(names): %: bin/% man/man1/%.1

bin/%: $(files)
	$(eval project := mfiano.cmd.freebsd.$(@F))
	@mkdir -p bin
	@echo "Compiling binary: $@"
	@sbcl --noinform \
	  --disable-debugger \
	  --eval "(handler-bind ((asdf:bad-system-name #'muffle-warning)) \
                (ql:quickload :$(project)))" \
	  --eval "(sb-ext:save-lisp-and-die \"bin/$(@F)\" \
                :executable t \
                :save-runtime-options t \
                :toplevel '$(project):app)" > /dev/null

man/man1/%.1: $(files)
	$(eval project := mfiano.cmd.freebsd.$(basename $(@F)))
	@mkdir -p man/man1
	@echo "Generating manual: $@"
	@sbcl --noinform \
	  --disable-debugger \
	  --eval "(handler-bind ((asdf:bad-system-name #'muffle-warning)) \
                (ql:quickload :$(project)))" \
	  --eval "(with-open-file (out \"man/man1/$(@F)\" :direction :output :if-exists :supersede) \
                (adopt:print-manual $(project):*ui* :stream out))" \
	  --quit > /dev/null

clean:
	rm -rf bin man
