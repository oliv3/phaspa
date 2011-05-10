/*
 *  Copyright 2011 Olivier Girondel
 */

#ifndef __SPLINE_H
#define __SPLINE_H

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>


typedef struct _Point3d_s {
  float x;
  float y;
  float z;
} _Point3d_t;

typedef union Point3d_u {
  _Point3d_t pos;
  float      coords[3];
} Point3d_t;


typedef struct Spline_s {
  u_char    span;
	
  float     *space[8], dt;
  Point3d_t *cpoints, *spoints;
  u_long     nb_cpoints;
  u_long     nb_spoints;
} Spline_t;


Spline_t *Spline_new(const u_char, const u_long);
void Spline_delete(Spline_t *);

void Spline_info(const Spline_t *);
void Spline_compute(const Spline_t *);

#endif /* __SPLINE_H */
