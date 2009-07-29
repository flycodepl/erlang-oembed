SOURCE_FILES := $(wildcard src/*.erl)


all: ebin

ebin: $(SOURCE_FILES:src/%.erl=ebin/%.beam)

ebin/%.beam: src/%.erl
	@test -d ebin || mkdir ebin
	@erl -make

clean:
	@rm -rf ebin erl_crash.dump
