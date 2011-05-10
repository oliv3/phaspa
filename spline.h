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

#ifndef __BINIOU_SPLINE_H
#define __BINIOU_SPLINE_H

#include "utils.h"
#include "point3d.h"


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

/* TODO Spline_draw (fully connected or not) dans un Buffer_8bits */

#endif /* __BINIOU_SPLINE_H */
