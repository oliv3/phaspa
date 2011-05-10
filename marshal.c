#include "marshal.h"


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


uint32_t
read_cmd(char *buf, uint32_t *size) {
  uint32_t plen, len;

  if (read_exact((char *)&plen, 4) != 4)
    return(-1);

  len = ntohl(plen);

  return read_exact(buf, len);
}


uint32_t
write_cmd(ei_x_buff *buff) {
  uint32_t len = buff->index;
  uint32_t plen;

  plen = htonl(len);
  write_exact((char *)&plen, 4);

  return write_exact(buff->buff, len);
}
