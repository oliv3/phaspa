#ifndef __DEBUG_H
#define __DEBUG_H

#ifdef DEBUG
#define D(F, A) do { fprintf(stderr, "%s:%d -- " F "\r\n", __FILE__, __LINE__, A); fflush(stderr); } while (0)
#else
#define D(F, A) {}
#endif

#endif /* __DEBUG_H */

