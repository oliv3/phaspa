all: gui.beam win.beam screen.beam wirecube.beam raw_file.beam \
	rec

run:
	erl -noshell -s gui

# ---- Erlang
ERLC_FLAGS = +debug_info
# +native

%.beam: %.erl
	erlc $(ERLC_FLAGS) $< -o $@

# ---- C
ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.7.3
CFLAGS=-O3 -Wall -I$(ERL_LIB)/include
LDFLAGS=-L$(ERL_LIB)/lib `pkg-config libpulse-simple --libs` -lerl_interface -lei -lpthread
OBJS=rec.o

rec: $(OBJS)
	gcc $(OBJS) $(LDFLAGS) -o $@ -lerl_interface -lei -lpthread

%.o: %.c
	gcc $(DEBUG) -c $(CFLAGS) $< -o $@

clean:
	@rm -f *~ *.beam erl_crash.dump rec
