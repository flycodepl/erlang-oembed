SHELL=/bin/sh

EFLAGS=-pa ebin -pa ../erlang-rfc4627/ebin -pa ../erlang-fmt/ebin

all: compile

compile: clean
	test -d ebin || mkdir ebin
	erl $(EFLAGS) -make

clean:
	rm -rf ebin erl_crash.dump

test: compile
	erl $(EFLAGS) -noshell -eval 'inets:start(), oembed_test:all(), c:q().'
