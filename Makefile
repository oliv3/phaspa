all: screen.beam

run:
	erl -noshell -s screen -s init stop

%.beam: %.erl
	erlc $< -o $@