all: gui.beam win.beam screen.beam wirecube.beam raw_file.beam

run:
	erl -noshell -s gui

ERLC_FLAGS = +debug_info +native

%.beam: %.erl
	erlc $(ERLC_FLAGS) $< -o $@

clean:
	@rm -f *~ *.beam
