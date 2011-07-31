
TEST_SUPPORT = \
	test/etap.beam \
	test/test_util.beam

%.beam: %.erl
	erlc -o test/ $<

all: deps
	./rebar compile

deps:
	./rebar get-deps

check: all $(TEST_SUPPORT)
	prove test/*.t

clean:
	./rebar clean
	rm -f test/*.beam
