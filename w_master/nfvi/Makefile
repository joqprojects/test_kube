## set the paths for a default setup
all:
	erl -pa ebin -s nfvi_lib boot -sname nfvi_master -setcookie glurk
test:
	erlc -o ebin src/*.erl;
	erlc -o ebin test_src/*.erl;
	erlc -o ebin ../common/src/*erl;
	cp src/*.app ebin;
	erl -pa ebin -s test_nfvi test -sname test_nfvi_1
