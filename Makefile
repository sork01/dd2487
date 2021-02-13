# https://stackoverflow.com/questions/2214575/passing-arguments-to-make-run
# If the first argument is "autodoc"...
ifeq (autodoc,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "autodoc"
  AUTODOC_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(AUTODOC_ARGS):;@:)
endif

JRE_PATH = /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/rt.jar
PROJECT_CLASSPATH = $(shell lein classpath):$(JRE_PATH)

.PHONY: all format run clean autodoc todo classpath

all:
	lein compile :all

format:
	./format.sh

run:
	lein run

clean:
	rm -rf target

autodoc:
	@CLASSPATH=$(PROJECT_CLASSPATH) clojure autodoc.clj $(AUTODOC_ARGS) > /tmp/autodoc_clj_output 2>&1;\
	EXIT_CODE=$$?;\
	if [ $$EXIT_CODE -eq 0 ]; then\
		cp /tmp/autodoc_clj_output /tmp/autodoc_clj_cache_$(word 1, $(AUTODOC_ARGS));\
		cat /tmp/autodoc_clj_output;\
	else\
		if [ -f "/tmp/autodoc_clj_cache_$(word 1, $(AUTODOC_ARGS))" ]; then\
			cat "/tmp/autodoc_clj_cache_$(word 1, $(AUTODOC_ARGS))";\
			(>&2 echo "autodoc.clj **** WARNING: the output is from a previously cached version ****");\
		else\
			(>&2 echo "autodoc.clj returned nonzero exit status (probably because the main source tree is currently not compiling) and there is no cache available :(");\
		fi;\
	fi

server:
	@CLASSPATH=$(PROJECT_CLASSPATH) clojure -e "(require 'firestone.definitions-loader 'firestone.server 'firestone.spec 'firestone.instrumentation 'firestone.server) (firestone.server/start!)"


todo:
	grep -nPR 'TODO' ./src || true
	grep -nPR 'TODO' ./test || true

classpath:
	@echo $(PROJECT_CLASSPATH)
