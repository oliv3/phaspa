all: gui.beam win.beam screen.beam wirecube.beam raw_file.beam \
	rec rec.beam spline spline.beam

run:
	erl -noshell -s gui

# ---- Erlang
ERLC_FLAGS = +debug_info +native

%.beam: %.erl
	erlc $(ERLC_FLAGS) $< -o $@

# ---- C
ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.7.3
CPPFLAGS=-I$(ERL_LIB)/include
CFLAGS=-O3 -Wall -Werror
LDFLAGS=-L$(ERL_LIB)/lib `pkg-config libpulse-simple --libs` -lerl_interface -lei -lpthread
REC_OBJS=rec.o marshal.o
SPL_OBJS=spline.o spline_main.o marshal.o
# DEBUG=-DDEBUG

marshal.o: marshal.h marshal.c
rec.o: rec.c marshal.h debug.h
spline.o: spline.h debug.h
spline_main.o: spline_main.c spline.h marshal.h debug.h

rec: $(REC_OBJS)
	gcc $(REC_OBJS) $(LDFLAGS) $(DEBUG) -o $@ -lerl_interface -lei -lpthread

spline: $(SPL_OBJS)
	gcc $(SPL_OBJS) $(LDFLAGS) $(DEBUG) -o $@ -lerl_interface -lei

%.o: %.c
	gcc $(CPPFLAGS) $(DEBUG) $(CFLAGS) -c $< -o $@

clean:
	@rm -f *~ *.beam erl_crash.dump rec spline *.o
