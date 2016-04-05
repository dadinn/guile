/* Copyright (C) 1995,1996,1998,1999,2000,2001, 2002, 2003, 2006, 2009, 2010, 2011, 2013 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <errno.h>

#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/chars.h"
#include "libguile/ports.h"
#include "libguile/ports-internal.h"
#include "libguile/fports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/vports.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif



/* {Ports - soft ports}
 * 
 */


static scm_t_bits scm_tc16_soft_port;

struct soft_port {
  SCM write_char;
  SCM write_string;
  SCM flush;
  SCM read_char;
  SCM close;
  SCM input_waiting;
};


static void
soft_port_flush (SCM port)
{
  struct soft_port *stream = (void *) SCM_STREAM (port);

  if (scm_is_true (stream->flush))
    scm_call_0 (stream->flush);
}

static void
soft_port_write (SCM port, const void *data, size_t size)
{
  struct soft_port *stream = (void *) SCM_STREAM (port);

  /* DATA is assumed to be a locale-encoded C string, which makes it
     hard to reliably pass binary data to a soft port.  It can be
     achieved by choosing a Latin-1 locale, though, but the recommended
     approach is to use an R6RS "custom binary output port" instead.  */
  scm_call_1 (stream->write_string,
	      scm_from_locale_stringn ((char *) data, size));
}

/* calling the flush proc (element 2) is in case old code needs it,
   but perhaps softports could the use port buffer in the same way as
   fports.  */

/* places a single char in the input buffer.  */
static int 
soft_port_fill_input (SCM port)
{
  struct soft_port *stream = (void *) SCM_STREAM (port);
  SCM ans;
  scm_t_wchar c;
  scm_t_port_internal *pti;

  ans = scm_call_0 (stream->read_char);
  if (scm_is_false (ans) || SCM_EOF_OBJECT_P (ans))
    return EOF;
  SCM_ASSERT (SCM_CHARP (ans), ans, SCM_ARG1, "soft_port_fill_input");
  pti = SCM_PORT_GET_INTERNAL (port);

  c = SCM_CHAR (ans);

  if (pti->encoding_mode == SCM_PORT_ENCODING_MODE_LATIN1
      || (pti->encoding_mode == SCM_PORT_ENCODING_MODE_UTF8 && c < 0xff))
    {
      scm_t_port *pt = SCM_PTAB_ENTRY (port);    
      
      *pt->read_buf = c;
      pt->read_pos = pt->read_buf;
      pt->read_end = pt->read_buf + 1;
    }
  else
    {
      long line = SCM_LINUM (port);
      int column = SCM_COL (port);

      scm_ungetc_unlocked (c, port);

      SCM_LINUM (port) = line;
      SCM_COL (port) = column;
    }

  return c;
}


static void
soft_port_close (SCM port)
{
  struct soft_port *stream = (void *) SCM_STREAM (port);
  if (scm_is_true (stream->close))
    scm_call_0 (stream->close);
}


static int 
soft_port_input_waiting (SCM port)
{
  struct soft_port *stream = (void *) SCM_STREAM (port);
  if (scm_is_true (stream->input_waiting))
    return scm_to_int (scm_call_0 (stream->input_waiting));
  /* Default is such that char-ready? for soft ports returns #t, as it
     did before this extension was implemented. */
  return 1;
}



SCM_DEFINE (scm_make_soft_port, "make-soft-port", 2, 0, 0,
           (SCM pv, SCM modes),
	    "Return a port capable of receiving or delivering characters as\n"
	    "specified by the @var{modes} string (@pxref{File Ports,\n"
	    "open-file}).  @var{pv} must be a vector of length 5 or 6.  Its\n"
	    "components are as follows:\n"
	    "\n"
	    "@enumerate 0\n"
	    "@item\n"
	    "procedure accepting one character for output\n"
	    "@item\n"
	    "procedure accepting a string for output\n"
	    "@item\n"
	    "thunk for flushing output\n"
	    "@item\n"
	    "thunk for getting one character\n"
	    "@item\n"
	    "thunk for closing port (not by garbage collection)\n"
	    "@item\n"
	    "(if present and not @code{#f}) thunk for computing the number of\n"
	    "characters that can be read from the port without blocking.\n"
	    "@end enumerate\n"
	    "\n"
	    "For an output-only port only elements 0, 1, 2, and 4 need be\n"
	    "procedures.  For an input-only port only elements 3 and 4 need\n"
	    "be procedures.  Thunks 2 and 4 can instead be @code{#f} if\n"
	    "there is no useful operation for them to perform.\n"
	    "\n"
	    "If thunk 3 returns @code{#f} or an @code{eof-object}\n"
	    "(@pxref{Input, eof-object?, ,r5rs, The Revised^5 Report on\n"
	    "Scheme}) it indicates that the port has reached end-of-file.\n"
	    "For example:\n"
	    "\n"
	    "@lisp\n"
	    "(define stdout (current-output-port))\n"
	    "(define p (make-soft-port\n"
	    "           (vector\n"
	    "            (lambda (c) (write c stdout))\n"
	    "            (lambda (s) (display s stdout))\n"
	    "            (lambda () (display \".\" stdout))\n"
	    "            (lambda () (char-upcase (read-char)))\n"
	    "            (lambda () (display \"@@\" stdout)))\n"
	    "           \"rw\"))\n"
	    "\n"
	    "(write p p) @result{} #<input-output: soft 8081e20>\n"
	    "@end lisp")
#define FUNC_NAME s_scm_make_soft_port
{
  int vlen;
  SCM z;
  struct soft_port *stream;

  SCM_VALIDATE_VECTOR (1, pv);
  vlen = SCM_SIMPLE_VECTOR_LENGTH (pv);
  SCM_ASSERT ((vlen == 5) || (vlen == 6), pv, 1, FUNC_NAME);
  SCM_VALIDATE_STRING (2, modes);

  stream = scm_gc_typed_calloc (struct soft_port);
  stream->write_char = SCM_SIMPLE_VECTOR_REF (pv, 0);
  stream->write_string = SCM_SIMPLE_VECTOR_REF (pv, 1);
  stream->flush = SCM_SIMPLE_VECTOR_REF (pv, 2);
  stream->read_char = SCM_SIMPLE_VECTOR_REF (pv, 3);
  stream->close = SCM_SIMPLE_VECTOR_REF (pv, 4);
  stream->input_waiting =
    vlen == 6 ? SCM_SIMPLE_VECTOR_REF (pv, 5) : SCM_BOOL_F;

  z = scm_c_make_port (scm_tc16_soft_port, scm_i_mode_bits (modes),
                       (scm_t_bits) stream);
  scm_port_non_buffer (SCM_PTAB_ENTRY (z));

  return z;
}
#undef FUNC_NAME


static scm_t_bits
scm_make_sfptob ()
{
  scm_t_bits tc = scm_make_port_type ("soft", soft_port_fill_input,
                                      soft_port_write);

  scm_set_port_flush (tc, soft_port_flush);
  scm_set_port_close (tc, soft_port_close);
  scm_set_port_needs_close_on_gc (tc, 1);
  scm_set_port_input_waiting (tc, soft_port_input_waiting);

  return tc;
}

void
scm_init_vports ()
{
  scm_tc16_soft_port = scm_make_sfptob ();

#include "libguile/vports.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
