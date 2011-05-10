/*
 *  Copyright 1994-2011 Olivier Girondel
 *
 *  This file is part of lebiniou.
 *
 *  lebiniou is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  lebiniou is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with lebiniou. If not, see <http://www.gnu.org/licenses/>.
 */

#include "spline.h"


Spline_t *
Spline_new(const u_char span, const u_long nb_cpoints)
{
  u_char dd;
  Spline_t *s = calloc(1, sizeof(Spline_t));
	
  s->span = span;
  s->nb_cpoints = nb_cpoints;
	
  s->nb_spoints = (s->nb_cpoints-1) * s->span + 1;
  s->dt = 1.0 / (float)s->span;
	
  for (dd = 0; dd < 8; dd++)
    s->space[dd] = calloc(nb_cpoints, sizeof(float));
	
  s->cpoints = calloc(s->nb_cpoints, sizeof(Point3d_t));
  s->spoints = calloc(s->nb_spoints, sizeof(Point3d_t));

  return s;
}


void
Spline_delete(Spline_t *s)
{
  u_char dd;

  free(s->cpoints);
  free(s->spoints);

  for (dd = 0; dd < 8; dd++)
    free(s->space[dd]);

  free(s);
}


void
Spline_compute(const Spline_t *s)
{
  /* C'est parti */
	
  float *a, *b, *c, *d;
  float *h0, *h1, *h2, *h3, *hi_a;
  short i, i1, imax;
  float t;
  u_char k;  
  Point3d_t *v;
  const short nb_cpoints = s->nb_cpoints;

  if (nb_cpoints < 2)
    return;

  h0 = s->space[0]; 
  h1 = s->space[1]; 
  h2 = s->space[2];
  h3 = s->space[3];

  for (k = 0; k < 3; k++) {
    a = s->space[4];
    b = s->space[5];
    c = s->space[6];
    d = s->space[7];
    
    /* Trigonal system */
    
    for (i = 0; i < nb_cpoints; i++)
      d[i] = s->cpoints[i].coords[k];
    
    for (i = 0, imax = nb_cpoints - 2; i < imax; i++) {
      h3[i] = 3 * (d[i + 2] - 2 * d[i + 1] + d[i]);
      h2[i] = 1;
    }
    h2[nb_cpoints - 3] = 0;
    
    /* Dissolution of the system */
    
    a[0] = 4;
    h1[0] = h3[0] / a[0];

    for (i = 1, i1 = 0, imax = nb_cpoints - 2; i < imax; i++, i1++) {
      h0[i1] = h2[i1] / a[i1];
      a[i]   = 4 - h0[i1];
      h1[i]  = (h3[i] - h1[i1]) / a[i];
    }
    b[nb_cpoints - 3] = h1[nb_cpoints - 3];
    
    for (i = nb_cpoints - 4; i >= 0; i--)
      b[i] = h1[i] - h0[i] * b[i + 1];
    
    for (i = nb_cpoints - 2; i >= 1; i--)
      b[i] = b[i - 1];
    
    b[0] = b[nb_cpoints - 1] = 0;
    hi_a = a + nb_cpoints - 1;
    
    for ( ; a < hi_a; a++, b++, c++, d++) {
      *c = *(d + 1) - *d - ( 2 * *b + *(b + 1)) / 3;
      *a = (*(b + 1) - *b) / 3;
    }
    
    v = s->spoints;
    a = s->space[4];
    b = s->space[5];
    c = s->space[6];
    d = s->space[7];

#ifdef DEBUG
    {
      u_long lcount=0;
#endif
      for ( ; a < hi_a; a++, b++, c++, d++) 
	for (t = 0; t < 1 - 1e-7; t += s->dt) {
	  /* for (t = 0; t < 1.0 - s->dt; t += s->dt) { */
	  (*v++).coords[k] = ((*a * t + *b) * t + *c) * t + *d;
#ifdef DEBUG
	  lcount++;
#endif
	}
      (*v++).coords[k] = *d;
#ifdef DEBUG
      lcount++;
      if (1) { //lcount > s->nb_spoints) {
	fprintf(stderr, "spline fatal: %li points, wanted to set %li\n", s->nb_spoints, lcount);
	exit(1);
      }
    }
#endif
  }
}


void
Spline_info(const Spline_t *s)
{
  if (s != NULL) {
    printf("[s] Spline has span: %d\n", s->span);
    printf("[s] %li control points\n", s->nb_cpoints);
    printf("[s] %li spline points\n", s->nb_spoints);
  }
}
