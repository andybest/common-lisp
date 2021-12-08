files := include/*.asd include/*.lisp src/%/*.asd src/%/*.lisp Makefile
names := $(notdir $(wildcard src/*))

.PHONY: all clean $(names)

all: $(names)

$(names): %: bin/% man/man1/%.1

bin/%: $(files)
	$(eval project := mfiano.scripts.$(@F))
	@mkdir -p bin
	@echo "Compiling script: $@"
	@sbcl --noinform \
		--disable-debugger \
		--eval "(handler-bind ((asdf:bad-system-name #'muffle-warning)) \
		          (:load-bundle :$(project)))" \
		--eval "(sb-ext:save-lisp-and-die \"bin/$(@F)\" \
		  :executable t \
		  :save-runtime-options t \
		  :toplevel '$(project):toplevel)" > /dev/null

man/man1/%.1: $(files)
	$(eval project := mfiano.scripts.$(basename $(@F)))
	@mkdir -p man/man1
	@echo "Generating manpage: $@"
	@sbcl --noinform \
		--disable-debugger \
		--eval "(handler-bind ((asdf:bad-system-name #'muffle-warning)) \
		          (:load-bundle :$(project)))" \
		--eval "(with-open-file (out \"man/man1/$(@F)\" :direction :output :if-exists :supersede) \
		          (adopt:print-manual $(project):*ui* :stream out))" \
		--quit > /dev/null

clean:
	rm -rf bin man
