all: gui.beam win.beam screen.beam wirecube.beam raw_file.beam \
	rec rec.beam

run:
	erl -noshell -s gui

# ---- Erlang
ERLC_FLAGS = +debug_info +native

%.beam: %.erl
	erlc $(ERLC_FLAGS) $< -o $@

# ---- C
ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.7.3
CFLAGS=-O3 -Wall -I$(ERL_LIB)/include -Werror
LDFLAGS=-L$(ERL_LIB)/lib `pkg-config libpulse-simple --libs` -lerl_interface -lei -lpthread
REC_OBJS=rec.o marshal.o
SPL_OBJS=spline.o marshal.o

marshal.o: marshal.h marshal.c
rec.o: rec.c marshal.h

rec: $(REC_OBJS)
	gcc $(REC_OBJS) $(LDFLAGS) -o $@ -lerl_interface -lei -lpthread

%.o: %.c
	gcc $(DEBUG) -c $(CFLAGS) $< -o $@

clean:
	@rm -f *~ *.beam erl_crash.dump rec
