all:
	./rebar compile
clean:
	./rebar clean
	@rm -rf erl_crash.dump
