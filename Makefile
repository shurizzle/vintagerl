all: compile

compile:
	erl -make

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump

run:
	erl -sname console -pa ebin -setcookie iterative

test:
	erl -pa ebin -noshell -run vintagerl start
