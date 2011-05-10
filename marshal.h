#ifndef __MARSHAL_H
#define __MARSHAL_H

#include <stdint.h>
#include <ei.h>
#include <unistd.h>

uint32_t read_cmd(char *buf, uint32_t *size);
uint32_t write_cmd(ei_x_buff* x);

#endif /* __MARSHAL_H */
