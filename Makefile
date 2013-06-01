all:
	erlc src/*.erl; mv *.beam ebin/

run:
	erl -pa ebin/
