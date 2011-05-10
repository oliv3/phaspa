#include <pulse/simple.h>
#include <pulse/error.h>
#include <pthread.h>
#include "marshal.h"
#include "debug.h"

#define BUF_SIZE 65535

/* TODO: pass INSIZE it as an option to start */
#define INSIZE	   256
#define CHANNELS   2
#define NSAMPLES   (INSIZE*CHANNELS)
#define ABUFF_SIZE NSAMPLES * sizeof(float)

static float pa_buff[NSAMPLES];
static pa_simple *pa_s = NULL;
static pthread_t recorder;
static long frequency = -1;
static int recording = 0;

/*
 * PulseAudio-based recorder
 *
 * Protocol: {packet, 4}  %% on pourrait faire du {packet, 2} mais flemme
 *                        %% de modifier du code qui marche
 *
 * Commands:
 * 
 * {record, Frequency} -> ok | {error, already_started} // TODO: {record, Freq, Size}
 * stop                -> ok | {error, not_started}
 *
 */

static inline void
check(int val) {
  if (-1 == val) {
    /* we don't free anything, exiting anyway */
    exit(1);
  }
}


static void
ok() {
  ei_x_buff result;

  check(ei_x_new_with_version(&result));
  check(ei_x_encode_atom(&result, "ok"));

  write_cmd(&result);
  ei_x_free(&result);
}


static void
error() {
  ei_x_buff result;

  D("%s", "Error !");

  check(ei_x_new_with_version(&result));
  check(ei_x_encode_tuple_header(&result, 2));
  check(ei_x_encode_atom(&result, "error"));
  if (recording)
    check(ei_x_encode_atom(&result, "already_started"));
  else
    check(ei_x_encode_atom(&result, "not_started"));
  
  write_cmd(&result);
  ei_x_free(&result);
}


void *
record(void *args) {
  int error;
  pa_sample_spec ss;
  char ss_a[PA_SAMPLE_SPEC_SNPRINT_MAX];

  memset(pa_buff, 0, ABUFF_SIZE);

  ss.format = PA_SAMPLE_FLOAT32LE;
  ss.channels = CHANNELS;
  ss.rate = frequency;

  pa_s = pa_simple_new(NULL,               /* PulseAudio server. */
                       "Recorder",         /* Application's name. */
                       PA_STREAM_RECORD,   /* Stream direction. */
                       NULL,               /* Sink Device. */
                       "PulseAudio-read",  /* Stream description. */
                       &ss,                /* Sample format. */
                       NULL,               /* Channel map */
                       NULL,               /* Buffering attributes. */
                       &error              /* Error code. */
                       );

  if (NULL == pa_s) {
    fprintf(stderr, __FILE__": pa_simple_new() failed: %s\n",
           pa_strerror(error));
    exit(1);
  }

  pa_sample_spec_snprint(ss_a, sizeof(ss_a), &ss);
  D("Opening the recording stream with sample specification '%s'", ss_a);
  D("%s", "Start recording");

  while (recording) {
    int n;
    int error;

    n = pa_simple_read(pa_s, (void *)pa_buff, ABUFF_SIZE, &error);

    if (-1 != n) {
      int i;
      ei_x_buff result;

      /* Prepare the output buffer that will hold the result */
      check(ei_x_new_with_version(&result));

      /* List size */
      check(ei_x_encode_list_header(&result, INSIZE));

      /* List elements */
      for (i = 0; i < NSAMPLES; i+=2) {
	check(ei_x_encode_tuple_header(&result, 2));
	check(ei_x_encode_double(&result, pa_buff[i]));
	check(ei_x_encode_double(&result, pa_buff[i+1]));
      }
      check(ei_x_encode_empty_list(&result));

      D("%s", "Sending data");

      write_cmd(&result);
      ei_x_free(&result);
    }
  }

  pa_simple_free(pa_s);

  pthread_exit(NULL);
}


static void
start_recording() {
  recording = 1;
  ok();
  pthread_create(&recorder, NULL, record, NULL);
}


static void
stop_recording() {
  D("%s", "Stop recording");
  recording = 0;
  /* brutally kill thread, so it stops sending data */
  pthread_cancel(recorder);
  ok();
}


int
main(int argc, char **argv) {
  char     *buf = NULL;
  uint32_t size = BUF_SIZE;
  char     command[MAXATOMLEN];
  int      index, version;

  if ((buf = (char *)calloc(size, sizeof(char))) == NULL)
    return -1;

  while (read_cmd(buf, &size) > 0) {
    /* Reset the index, so that ei functions can decode terms from the 
     * beginning of the buffer */
    index = 0;

    // D("buf: %s", buf);
    
    /* Ensure that we are receiving the binary term by reading and 
     * stripping the version byte */
    check(ei_decode_version(buf, &index, &version));

    /* Ici donc le code du recorder:
     *
     * if decode_atom => stop:
     *   arreter le thread -> ok, {error, not_started} sinon
     * else
     * if decode tuple de taille 2:
     *   starter le thread -> ok, {error, already_started} sinon
     */
    if (!ei_decode_atom(buf, &index, command)) {
      D("Got atom: %s", command);
      if (!strcmp(command, "stop")) {
	if (recording)
	  stop_recording();
	else
	  error();
      } else
	check(-1);
    } else {
      int arity;
      // long _span;

      check(ei_decode_tuple_header(buf, &index, &arity));
      D("Arity: %d", arity);
      // if (arity != 3) check(-1);
      if (arity != 2) check(-1);

      check(ei_decode_atom(buf, &index, command));
      D("Got atom: %s", command);
      if (strcmp(command, "record")) check(-1);

      check(ei_decode_long(buf, &index, &frequency));
      D("Freq: %li", frequency);

      /* check(ei_decode_long(buf, &index, &_span)); */
      /* span = _span; */
      /* D("SPAN: %i", span); */

      if (!recording) {
	start_recording();
      } else
	error();
    }

    memset(buf, 0, size*sizeof(char));
  }

  free(buf);

  return 0;
}
