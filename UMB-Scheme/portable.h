/* portable.h -- UMB Scheme, general portability definitions 

UMB Scheme Interpreter  				$Revision: 3.2 $
Copyright (C) 1988, 1996 William R Campbell

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

UMB Scheme was written by Bill Campbell with help from Karl Berry,
Barbara Dixey, Ira Gerstein, Mary Glaser, Kathy Hargreaves, Bill McCabe,
Long Nguyen, Susan Quina, Jeyashree Sivasubram, Bela Sohoni, John Tam 
and Thang Quoc Tran.

Modified to run on Linux by Thomas Mullaly.

For additional information about UMB Scheme, contact the author:

	Bill Campbell
	Department of Mathematics and Computer Science
	University of Massachusetts at Boston
	Harbor Campus
	Boston, MA 02125

	Telephone: 617-287-6449		Internet: bill@cs.umb.edu

*/

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <math.h>

/* Machine dependencies */

/* ALIGNMENT is the byte-boundary on which double's must be aligned
   in memory.  This constant is necessary for insuring the proper
   alignment of Scheme objects in the Heap (architecture.c).
   For example, on the Vax structs can be aligned on any 1-byte
   boundary, so ALIGNMENT=1.  On the other hand, on the Sun Sparc
   (and most RISC machines) structs must be aligned on 8-byte
   boundaries, so ALIGNMENT=8.  If you don't know, then 8 is safest
   (but will cause a little more fragmentation); better safe than sorry.
*/

/* IF YOUR MACHINE (eg M68000's, Vaxes) ALLOWS DOUBLE PRECISION OBJECTS TO
   BE ALIGNED ON SMALLER THAN 8_BYTE BOUNDARIES THEN CHANGE THIS: */

#define ALIGNMENT 8

typedef unsigned long Integral_Pointer; 

/* The following declarations ought not to be necessary for C environments
   that support the ANSI C Standard. */

extern void * malloc( /* size_t size */ );
extern void   free( /* char *ptr */ );
extern char * getenv( /* const char *name */ );
extern int    system( /* const char command */ );
extern void   exit( /* int status */ );
extern int    abs( /* int x */ );

/* IF YOUR ENVIRONMENT HAS (any standard ANSI C library ought to have)
   a float.h defns file then replace the definition of DBL_MIN with
   an #include <float.h>                                                 */

#define DBL_MIN 10e-307

#ifdef sun
extern int    fclose( /* FILE *stream */ );
extern int    ungetc( /* int c, FILE *stream */ );
extern int    fprintf( /* FILE *stream, const char *format, ...  */ );
extern int    _filbuf(), _flsbuf(); /* Yes, I know; talk to Sun! */
#endif

#ifdef sun386
extern int    sprintf( /* char *s,      const char *format, ...  */ );
extern char * memcpy( /* void *dest, const void *src, size_t len */ );
#endif


/* The following are UMB Scheme src definitions.  It is unlikely that they
   will have to be modified.  
*/


#define	TRUE	1
#define	FALSE	0

#define MAX_TOKEN_SIZE 1000
#define RADIX          0x8000

#define	Private	 static
#define	Public
#define Import   extern
#define External extern

typedef unsigned char  Byte;
typedef	char	       Character ;
typedef	char *	       String ;
typedef	int	       Boolean ;
typedef	int	       Integer ;
typedef short	       Short ;
typedef double         Double ;
typedef unsigned short Unsigned_Short;

#define Integer_Format "%d"
#define Short_Format   "%d"
#define Double_Format  "%.16g"

typedef enum { LESS_THAN, GREATER_THAN, EQUAL_TO } Compare_Type;

#define Eq_Strs(s1,s2) (strcmp((s1),(s2)) == 0)

#define Is_Terminal(stream) (isatty(fileno(stream)))

