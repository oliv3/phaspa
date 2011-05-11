#include "marshal.h"
// #undef DEBUG
#include "debug.h"
#include "spline.h"

#define BUF_SIZE 65535


static void write_spline(const Spline_t *);


static inline void
check(int val) {
  if (-1 == val) {
    /* we don't free anything, exiting anyway */
    exit(1);
  }
}


/*
static void
ok() {
  ei_x_buff result;

  check(ei_x_new_with_version(&result));
  check(ei_x_encode_atom(&result, "ok"));

  write_cmd(&result);
  ei_x_free(&result);
}
*/

static void
error() {
  ei_x_buff result;

  D("%s", "Error !");

  check(ei_x_new_with_version(&result));
  // check(ei_x_encode_tuple_header(&result, 2));
  check(ei_x_encode_atom(&result, "error"));
  /* if (recording) */
  /*   check(ei_x_encode_atom(&result, "already_started")); */
  /* else */
  /*   check(ei_x_encode_atom(&result, "not_started")); */
  
  write_cmd(&result);
  ei_x_free(&result);
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
    Spline_t *spline = NULL;

    /* Reset the index, so that ei functions can decode terms from the 
     * beginning of the buffer */
    index = 0;

    // D("buf: %s", buf);
    
    /* Ensure that we are receiving the binary term by reading and 
     * stripping the version byte */
    check(ei_decode_version(buf, &index, &version));

    /*    if (!ei_decode_atom(buf, &index, command)) {
	  D("Got atom: %s", command);
      if (!strcmp(command, "stop")) {
	if (recording)
	  stop_recording();
	else
	  error();
      } else
	check(-1);
	} else { */
    int arity;
    long span;
    int p, nb_cpoints;
    
    check(ei_decode_tuple_header(buf, &index, &arity));
    D("Arity: %d", arity);
    if (arity != 3) error(); // check(-1);
    
    check(ei_decode_atom(buf, &index, command));
    D("Got atom: %s", command);
    if (strcmp(command, "spline")) error(); // check(-1);
    
    check(ei_decode_long(buf, &index, &span));
    D("Span: %li", span);

    check(ei_decode_list_header(buf, &index, &nb_cpoints));
    D("List length: %d", nb_cpoints);

    spline = Spline_new(span, nb_cpoints);
    for (p = 0; p < nb_cpoints; p++) {
      double x, y, z;

      check(ei_decode_tuple_header(buf, &index, &arity));
      D("  Arity: %d", arity);
      if (arity != 3) check(-1);

      check(ei_decode_double(buf, &index, &x));
      check(ei_decode_double(buf, &index, &y));
      check(ei_decode_double(buf, &index, &z));

      spline->cpoints[p].pos.x = x;
      spline->cpoints[p].pos.y = y;
      spline->cpoints[p].pos.z = z;

      // fprintf(stderr, "  > Point: %f, %f, %f\r\n", x, y, z);
    }
    
    Spline_compute(spline);

    write_spline(spline);

    Spline_delete(spline);
    
    /*
    if (!recording) {
	start_recording();
      } else
	error();
    }
    */
    memset(buf, 0, size*sizeof(char));
  }

  free(buf);

  return 0;
}

void
write_spline(const Spline_t *spline)
{
  ei_x_buff result;
  u_long i;
  
  /* Prepare the output buffer that will hold the result */
  check(ei_x_new_with_version(&result));
  
  /* List size */
  check(ei_x_encode_list_header(&result, spline->nb_spoints));
  
  /* List elements */
  D("Sending %li elements", spline->nb_spoints);
  for (i = 0; i < spline->nb_spoints; i++) {
    check(ei_x_encode_tuple_header(&result, 3));
    check(ei_x_encode_double(&result, spline->spoints[i].pos.x));
    check(ei_x_encode_double(&result, spline->spoints[i].pos.y));
    check(ei_x_encode_double(&result, spline->spoints[i].pos.z));
    //check(ei_x_encode_double(&result, pa_buff[i+1]));
  }
  check(ei_x_encode_empty_list(&result));
  
  D("%s", "Sending spline");
  
  write_cmd(&result);
  ei_x_free(&result);
}
