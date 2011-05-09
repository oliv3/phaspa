#include <stdio.h>
#include <stdlib.h>
#include <pulse/simple.h>
#include <pulse/error.h>
#include <ei.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <arpa/inet.h>

/* TODO: pass INSIZE it as an option to start */
#define INSIZE	   512
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

#define DEBUG
#define BUF_SIZE 65535

static uint32_t read_cmd(char *buf, uint32_t *size);
static uint32_t write_cmd(ei_x_buff* x);
static uint32_t read_exact(char *buf, uint32_t len);
static uint32_t write_exact(char *buf, uint32_t len);

#ifdef DEBUG
#define D(F, A) do { fprintf(stderr, F "\r\n", A); fflush(stderr); } while (0)
#else
#define D(F, A) {}
#endif


static void
rescale_buff()
{
  int i;

  for (i = 0; i < NSAMPLES; i++)
    pa_buff[i] /= (float)-SHRT_MIN;
}


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

  // fprintf(stderr, "IN ERROR\n");

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
#ifdef DEBUG
  char ss_a[PA_SAMPLE_SPEC_SNPRINT_MAX];
  size_t frame_size;
#endif

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

#ifdef DEBUG
  pa_sample_spec_snprint(ss_a, sizeof(ss_a), &ss);
  fprintf(stderr,
          "Opening the recording stream with sample specification '%s'.\r\n",
          ss_a);

  frame_size = pa_frame_size(&ss);
  fprintf(stderr, "Frame size: %d\r\n", frame_size);
#endif

  while (recording) {
    int n;
    int error;

    n = pa_simple_read(pa_s, (void *)pa_buff, ABUFF_SIZE, &error);
    // D("%s", "RECORD ON");

    if (-1 != n) {
      int i;
      ei_x_buff result;

      /* rescale data to [-1.0 .. +1.0] */
      // rescale_buff();

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

      // fprintf(stderr, "SENDING DATA\n");

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
      D("got atom: %s", command);
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
      D("ARITY: %d", arity);
      // if (arity != 3) check(-1);
      if (arity != 2) check(-1);

      check(ei_decode_atom(buf, &index, command));
      D("got atom 2: %s", command);
      if (strcmp(command, "record")) check(-1);

      check(ei_decode_long(buf, &index, &frequency));
      D("FREQ: %li", frequency);

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


/*-----------------------------------------------------------------
 * Data marshalling functions
 *----------------------------------------------------------------*/
static uint32_t
read_cmd(char *buf, uint32_t *size) {
  uint32_t plen, len;

  if (read_exact((char *)&plen, 4) != 4)
    return(-1);

  len = ntohl(plen);
  D("Reading %d bytes", len);

  return read_exact(buf, len);
}


static uint32_t
write_cmd(ei_x_buff *buff) {
  uint32_t len = buff->index;
  uint32_t plen;

  plen = htonl(len);
  write_exact((char *)&plen, 4);

  return write_exact(buff->buff, len);
}


static uint32_t
read_exact(char *buf, uint32_t len) {
  uint32_t i, got = 0;

  do {
    if ((i = read(0, buf+got*sizeof(char), (len-got)*sizeof(char))) <= 0)
      return i;
    got += i;
  } while (got <len);

  return len;
}


static uint32_t
write_exact(char *buf, uint32_t len) {
  uint32_t i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return i;
    wrote += i;
  } while (wrote<len);

  fflush(stdout);

  return len;
}
