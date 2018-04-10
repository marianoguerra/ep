
.PHONY: build
build: compile script

.PHONY: compile
compile:
	rebar3 compile

.PHONY: script
script:
	rebar3 escriptize
	cp _build/default/bin/ep .

.PHONY: test
test:
	rebar3 ct

