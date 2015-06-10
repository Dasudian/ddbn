all:
	erlc -o ebin src/*.erl

test: all
	erlc -o test test/*.erl

clean:
	rm ebin/*
