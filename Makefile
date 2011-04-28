all: gui.beam win.beam screen.beam wirecube.beam

run:
	erl -noshell -s gui
# -s init stop

ERLC_FLAGS = +debug_info

%.beam: %.erl
	erlc $(ERLC_FLAGS) $< -o $@

clean:
	@rm -f *~ *.beam