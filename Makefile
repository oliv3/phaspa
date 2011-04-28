all: gui.beam win.beam screen.beam wirecube.beam

run:
	erl -noshell -s gui
# -s init stop

%.beam: %.erl
	erlc $< -o $@

clean:
	@rm -f *~ *.beam