PROJECT = ddbn

dev: app
	erl -config config/dev -pa deps/*/ebin -pa ebin

include erlang.mk

