/*	Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */

/* Headers.  */

#include <stdio.h>
#include "_scm.h"
#include "genio.h"
#include "chars.h"

#include "fports.h"
#include "strports.h"
#include "vports.h"
#include "keywords.h"

#include "ports.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif


/* The port kind table --- a dynamically resized array of port types.  */


/* scm_ptobs scm_numptob
 * implement a dynamicly resized array of ptob records.
 * Indexes into this table are used when generating type
 * tags for smobjects (if you know a tag you can get an index and conversely).
 */
scm_ptobfuns *scm_ptobs;
int scm_numptob;

/* GC marker for a port with stream of SCM type.  */
SCM 
scm_markstream (ptr)
     SCM ptr;
{
  int openp;
  openp = SCM_CAR (ptr) & SCM_OPN;
  if (openp)
    return SCM_STREAM  (ptr);
  else
    return SCM_BOOL_F;
}


long 
scm_newptob (ptob)
     scm_ptobfuns *ptob;
{
  char *tmp;
  if (255 <= scm_numptob)
    goto ptoberr;
  tmp = (char *) realloc ((char *) scm_ptobs, (1 + scm_numptob) * sizeof (scm_ptobfuns));
  if (tmp)
    {
      scm_ptobs = (scm_ptobfuns *) tmp;
      scm_ptobs[scm_numptob].mark = ptob->mark;
      scm_ptobs[scm_numptob].free = ptob->free;
      scm_ptobs[scm_numptob].print = ptob->print;
      scm_ptobs[scm_numptob].equalp = ptob->equalp;
      scm_ptobs[scm_numptob].fflush = ptob->fflush;
      scm_ptobs[scm_numptob].read_flush = ptob->read_flush;
      scm_ptobs[scm_numptob].fclose = ptob->fclose;
      scm_ptobs[scm_numptob].fill_buffer = ptob->fill_buffer;
      scm_ptobs[scm_numptob].seek = ptob->seek;
      scm_ptobs[scm_numptob].ftruncate = ptob->ftruncate;
      scm_ptobs[scm_numptob].input_waiting_p = ptob->input_waiting_p;
      scm_numptob++;
    }
  if (!tmp)
  ptoberr:scm_wta (SCM_MAKINUM ((long) scm_numptob), (char *) SCM_NALLOC, "newptob");
  return scm_tc7_port + (scm_numptob - 1) * 256;
}



SCM_PROC(s_char_ready_p, "char-ready?", 0, 1, 0, scm_char_ready_p);

SCM 
scm_char_ready_p (port)
     SCM port;
{
  scm_port *pt = SCM_PTAB_ENTRY (port);

  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG1,
		s_char_ready_p);

  /* if the current read buffer is filled, or the
     last pushed-back char has been read and the saved buffer is
     filled, result is true.  */
  if (pt->read_pos < pt->read_end 
      || (pt->read_buf == pt->putback_buf
	  && pt->saved_read_pos < pt->saved_read_end))
    return SCM_BOOL_T;
  else
    {
      scm_ptobfuns *ptob = &scm_ptobs[SCM_PTOBNUM (port)];
      
      if (ptob->input_waiting_p)
	return (ptob->input_waiting_p (port)) ? SCM_BOOL_T : SCM_BOOL_F;
      else
	return SCM_BOOL_T;
    }
}

/* Clear a port's read buffers, returning the contents.  */
SCM_PROC (s_drain_input, "drain-input", 1, 0, 0, scm_drain_input);
SCM
scm_drain_input (SCM port)
{
  SCM result;
  scm_port *pt = SCM_PTAB_ENTRY (port);
  int count;
  char *dst;

  SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG1,
	      s_drain_input);

  count = pt->read_end - pt->read_pos;
  if (pt->read_buf == pt->putback_buf)
    count += pt->saved_read_end - pt->saved_read_pos;

  result = scm_makstr (count, 0);
  dst = SCM_CHARS (result);

  while (pt->read_pos < pt->read_end)
    *dst++ = *(pt->read_pos++);
  
  if (pt->read_buf == pt->putback_buf)
    {
      while (pt->saved_read_pos < pt->saved_read_end)
	*dst++ = *(pt->saved_read_pos++);
    }

  return result;
}


/* Standard ports --- current input, output, error, and more(!).  */

SCM_PROC(s_current_input_port, "current-input-port", 0, 0, 0, scm_current_input_port);

SCM 
scm_current_input_port ()
{
  return scm_cur_inp;
}

SCM_PROC(s_current_output_port, "current-output-port", 0, 0, 0, scm_current_output_port);

SCM 
scm_current_output_port ()
{
  return scm_cur_outp;
}

SCM_PROC(s_current_error_port, "current-error-port", 0, 0, 0, scm_current_error_port);

SCM 
scm_current_error_port ()
{
  return scm_cur_errp;
}

SCM_PROC(s_current_load_port, "current-load-port", 0, 0, 0, scm_current_load_port);

SCM 
scm_current_load_port ()
{
  return scm_cur_loadp;
}

SCM_PROC(s_set_current_input_port, "set-current-input-port", 1, 0, 0, scm_set_current_input_port);

SCM 
scm_set_current_input_port (port)
     SCM port;
{
  SCM oinp = scm_cur_inp;
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG1, s_set_current_input_port);
  scm_cur_inp = port;
  return oinp;
}


SCM_PROC(s_set_current_output_port, "set-current-output-port", 1, 0, 0, scm_set_current_output_port);

SCM 
scm_set_current_output_port (port)
     SCM port;
{
  SCM ooutp = scm_cur_outp;
  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG1, s_set_current_output_port);
  scm_cur_outp = port;
  return ooutp;
}


SCM_PROC(s_set_current_error_port, "set-current-error-port", 1, 0, 0, scm_set_current_error_port);

SCM 
scm_set_current_error_port (port)
     SCM port;
{
  SCM oerrp = scm_cur_errp;
  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG1, s_set_current_error_port);
  scm_cur_errp = port;
  return oerrp;
}


/* The port table --- an array of pointers to ports.  */

scm_port **scm_port_table;

int scm_port_table_size = 0;	/* Number of ports in scm_port_table.  */
int scm_port_table_room = 20;	/* Size of the array.  */

/* Add a port to the table.  */

scm_port *
scm_add_to_port_table (port)
     SCM port;
{
  scm_port *entry;

  if (scm_port_table_size == scm_port_table_room)
    {
      void *newt = realloc ((char *) scm_port_table,
			    (scm_sizet) (sizeof (scm_port *)
					 * scm_port_table_room * 2));
      if (newt == NULL)
	scm_memory_error ("scm_add_to_port_table");
      scm_port_table = (scm_port **) newt;
      scm_port_table_room *= 2;
    }
  entry = (scm_port *) malloc (sizeof (scm_port));
  if (entry == NULL)
    scm_memory_error ("scm_add_to_port_table");

  entry->port = port;
  entry->entry = scm_port_table_size;
  entry->revealed = 0;
  entry->stream = 0;
  entry->file_name = SCM_BOOL_F;
  entry->line_number = 0;
  entry->column_number = 0;
  entry->putback_buf = 0;
  entry->putback_buf_size = 0;
  entry->rw_active = 0;

  scm_port_table[scm_port_table_size] = entry;
  scm_port_table_size++;

  return entry;
}

/* Remove a port from the table and destroy it.  */

void
scm_remove_from_port_table (port)
     SCM port;
{
  scm_port *p = SCM_PTAB_ENTRY (port);
  int i = p->entry;

  if (i >= scm_port_table_size)
    scm_wta (port, "Port not in table", "scm_remove_from_port_table");
  if (p->putback_buf)
    free (p->putback_buf);
  free (p);
  /* Since we have just freed slot i we can shrink the table by moving
     the last entry to that slot... */
  if (i < scm_port_table_size - 1)
    {
      scm_port_table[i] = scm_port_table[scm_port_table_size - 1];
      scm_port_table[i]->entry = i;
    }
  SCM_SETPTAB_ENTRY (port, 0);
  scm_port_table_size--;
}

#ifdef GUILE_DEBUG
/* Undocumented functions for debugging.  */
/* Return the number of ports in the table.  */

SCM_PROC(s_pt_size, "pt-size", 0, 0, 0, scm_pt_size);
SCM
scm_pt_size ()
{
  return SCM_MAKINUM (scm_port_table_size);
}

/* Return the ith member of the port table.  */
SCM_PROC(s_pt_member, "pt-member", 1, 0, 0, scm_pt_member);
SCM
scm_pt_member (member)
     SCM member;
{
  int i;
  SCM_ASSERT (SCM_INUMP (member), member, SCM_ARG1, s_pt_member);
  i = SCM_INUM (member);
  if (i < 0 || i >= scm_port_table_size)
    return SCM_BOOL_F;
  else
    return scm_port_table[i]->port;
}
#endif



/* Revealed counts --- an oddity inherited from SCSH.  */

/* Find a port in the table and return its revealed count.
   Also used by the garbage collector.
 */

int
scm_revealed_count (port)
     SCM port;
{
  return SCM_REVEALED(port);
}



/* Return the revealed count for a port.  */

SCM_PROC(s_port_revealed, "port-revealed", 1, 0, 0, scm_port_revealed);

SCM
scm_port_revealed (port)
     SCM port;
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port), port, SCM_ARG1, s_port_revealed);
  return SCM_MAKINUM (scm_revealed_count (port));
}

/* Set the revealed count for a port.  */
SCM_PROC(s_set_port_revealed_x, "set-port-revealed!", 2, 0, 0, scm_set_port_revealed_x);

SCM
scm_set_port_revealed_x (port, rcount)
     SCM port;
     SCM rcount;
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port), port, SCM_ARG1, s_set_port_revealed_x);
  SCM_ASSERT (SCM_INUMP (rcount), rcount, SCM_ARG2, s_set_port_revealed_x);
  SCM_REVEALED (port) = SCM_INUM (rcount);
  return SCM_UNSPECIFIED;
}



/* Retrieving a port's mode.  */

/* Return the flags that characterize a port based on the mode
 * string used to open a file for that port.
 *
 * See PORT FLAGS in scm.h
 */

long
scm_mode_bits (modes)
     char *modes;
{
  return (SCM_OPN
	  | (strchr (modes, 'r') || strchr (modes, '+') ? SCM_RDNG : 0)
	  | (   strchr (modes, 'w')
	     || strchr (modes, 'a')
	     || strchr (modes, '+') ? SCM_WRTNG : 0)
	  | (strchr (modes, '0') ? SCM_BUF0 : 0)
	  | (strchr (modes, 'l') ? SCM_BUFLINE : 0));
}


/* Return the mode flags from an open port.
 * Some modes such as "append" are only used when opening
 * a file and are not returned here.  */

SCM_PROC(s_port_mode, "port-mode", 1, 0, 0, scm_port_mode);

SCM
scm_port_mode (port)
     SCM port;
{
  char modes[3];
  modes[0] = '\0';

  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPPORTP (port), port, SCM_ARG1, s_port_mode);  
  if (SCM_CAR (port) & SCM_RDNG) {
    if (SCM_CAR (port) & SCM_WRTNG)
      strcpy (modes, "r+");
    else
      strcpy (modes, "r");
  }
  else if (SCM_CAR (port) & SCM_WRTNG)
    strcpy (modes, "w");
  if (SCM_CAR (port) & SCM_BUF0)
    strcat (modes, "0");
  return scm_makfromstr (modes, strlen (modes), 0);
}



/* Closing ports.  */

/* scm_close_port
 * Call the close operation on a port object. 
 * see also scm_close.
 */
SCM_PROC(s_close_port, "close-port", 1, 0, 0, scm_close_port);

SCM
scm_close_port (port)
     SCM port;
{
  scm_sizet i;
  int rv;

  port = SCM_COERCE_OUTPORT (port);

  SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port), port, SCM_ARG1,
	      s_close_port);
  if (SCM_CLOSEDP (port))
    return SCM_BOOL_F;
  i = SCM_PTOBNUM (port);
  if (scm_ptobs[i].fclose)
    rv = (scm_ptobs[i].fclose) (port);
  else
    rv = 0;
  scm_remove_from_port_table (port);
  SCM_SETAND_CAR (port, ~SCM_OPN);
  return (rv < 0) ? SCM_BOOL_F : SCM_BOOL_T;
}

SCM_PROC(s_close_all_ports_except, "close-all-ports-except", 0, 0, 1, scm_close_all_ports_except);

SCM
scm_close_all_ports_except (ports)
     SCM ports;
{
  int i = 0;
  SCM_ASSERT (SCM_NIMP (ports) && SCM_CONSP (ports), ports, SCM_ARG1, s_close_all_ports_except);
  while (i < scm_port_table_size)
    {
      SCM thisport = scm_port_table[i]->port;
      int found = 0;
      SCM ports_ptr = ports;

      while (SCM_NNULLP (ports_ptr))
	{
	  SCM port = SCM_COERCE_OUTPORT (SCM_CAR (ports_ptr));
	  if (i == 0)
	    SCM_ASSERT (SCM_NIMP (port) && SCM_OPPORTP (port), port, SCM_ARG1, s_close_all_ports_except);
	  if (port == thisport)
	    found = 1;
	  ports_ptr = SCM_CDR (ports_ptr);
	}
      if (found)
	i++;
      else
	/* i is not to be incremented here.  */
	scm_close_port (thisport);
    }
  return SCM_UNSPECIFIED;
}



/* Utter miscellany.  Gosh, we should clean this up some time.  */

SCM_PROC(s_input_port_p, "input-port?", 1, 0, 0, scm_input_port_p);

SCM 
scm_input_port_p (x)
     SCM x;
{
  if (SCM_IMP (x))
 return SCM_BOOL_F;
  return SCM_INPORTP (x) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_output_port_p, "output-port?", 1, 0, 0, scm_output_port_p);

SCM 
scm_output_port_p (x)
     SCM x;
{
  if (SCM_IMP (x))
 return SCM_BOOL_F;
  return SCM_OUTPORTP (x) ? SCM_BOOL_T : SCM_BOOL_F;
}


SCM_PROC(s_eof_object_p, "eof-object?", 1, 0, 0, scm_eof_object_p);

SCM 
scm_eof_object_p (x)
     SCM x;
{
  return SCM_EOF_OBJECT_P (x) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_force_output, "force-output", 0, 1, 0, scm_force_output);

SCM 
scm_force_output (port)
     SCM port;
{
  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    {
      port = SCM_COERCE_OUTPORT (port);
      SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG1, 
		  s_force_output);
    }
  scm_fflush (port);
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_flush_all_ports, "flush-all-ports", 0, 0, 0, scm_flush_all_ports);
SCM
scm_flush_all_ports (void)
{
  int i;

  for (i = 0; i < scm_port_table_size; i++)
    {
      if (SCM_OPOUTPORTP (scm_port_table[i]->port))
	scm_fflush (scm_port_table[i]->port);
    }
  return SCM_UNSPECIFIED;
}

SCM_PROC(s_read_char, "read-char", 0, 1, 0, scm_read_char);

SCM 
scm_read_char (port)
     SCM port;
{
  int c;
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG1, s_read_char);
  c = scm_getc (port);
  if (EOF == c)
    return SCM_EOF_VAL;
  return SCM_MAKICHR (c);
}

/* this should only be called when the read buffer is empty.  it
   tries to refill the buffer.  it returns the first char from
   the port, which is either EOF or *(pt->read_pos).  */
int
scm_fill_buffer (SCM port)
{
  scm_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->read_buf == pt->putback_buf)
    {
      /* finished reading put-back chars.  */
      pt->read_buf = pt->saved_read_buf;
      pt->read_pos = pt->saved_read_pos;
      pt->read_end = pt->saved_read_end;
      pt->read_buf_size = pt->saved_read_buf_size;
      if (pt->read_pos < pt->read_end)
	return *(pt->read_pos);
    }
  return scm_ptobs[SCM_PTOBNUM (port)].fill_buffer (port);
}

int 
scm_getc (port)
     SCM port;
{
  int c;
  scm_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->rw_active == SCM_PORT_WRITE)
    {
      /* may be marginally faster than calling scm_fflush.  */
      scm_ptobs[SCM_PTOBNUM (port)].fflush (port);
    }
  
  if (pt->rw_random)
    pt->rw_active = SCM_PORT_READ;

  if (pt->read_pos >= pt->read_end)
    {
      if (scm_fill_buffer (port) == EOF)
	return EOF;
    }

  c = *(pt->read_pos++);

  if (c == '\n')
    {
      SCM_INCLINE (port);
    }
  else if (c == '\t')
    {
      SCM_TABCOL (port);
    }
  else
    {
      SCM_INCCOL (port);
    }

  return c;
}

void 
scm_putc (c, port)
     int c;
     SCM port;
{
  scm_port *pt = SCM_PTAB_ENTRY (port);  
  scm_ptobfuns *ptob = &scm_ptobs[SCM_PTOBNUM (port)];

  if (pt->rw_active == SCM_PORT_READ)
    scm_read_flush (port);

  *(pt->write_pos++) = (char) c;

  if (pt->write_pos == pt->write_end)
    ptob->fflush (port);
  
  if (pt->rw_random)
    pt->rw_active = SCM_PORT_WRITE;
}

void 
scm_puts (s, port)
     char *s;
     SCM port;
{
  scm_port *pt = SCM_PTAB_ENTRY (port);
  scm_ptobfuns *ptob = &scm_ptobs[SCM_PTOBNUM (port)];

  if (pt->rw_active == SCM_PORT_READ)
    scm_read_flush (port);

  while (*s != 0)
    {
      *pt->write_pos++ = *s++;
      if (pt->write_pos == pt->write_end)
	ptob->fflush (port);
    }
  /* If the port is line-buffered, flush it.  */
  if ((SCM_CAR (port) & SCM_BUFLINE)
      && memchr (pt->write_buf, '\n', pt->write_pos - pt->write_buf))
    ptob->fflush (port);

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_WRITE;
}

void 
scm_lfwrite (ptr, size, port)
     char *ptr;
     scm_sizet size;
     SCM port;
{
  scm_port *pt = SCM_PTAB_ENTRY (port);
  scm_ptobfuns *ptob = &scm_ptobs[SCM_PTOBNUM (port)];

  if (pt->rw_active == SCM_PORT_READ)
    scm_read_flush (port);

  while (size > 0)
    {
      int space = pt->write_end - pt->write_pos;
      int write_len = (size > space) ? space : size;
      
      strncpy (pt->write_pos, ptr, write_len);
      pt->write_pos += write_len;
      size -= write_len;
      ptr += write_len;
      if (write_len == space)
	ptob->fflush (port);
    }
  /* If the port is line-buffered, flush it.  */
  if ((SCM_CAR (port) & SCM_BUFLINE)
      && memchr (pt->write_buf, '\n', pt->write_pos - pt->write_buf))
    (ptob->fflush) (port);

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_WRITE;
}


void 
scm_fflush (port)
     SCM port;
{
  scm_sizet i = SCM_PTOBNUM (port);
  (scm_ptobs[i].fflush) (port);
}

void
scm_read_flush (port)
     SCM port;
{
  int offset;
  scm_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->read_buf == pt->putback_buf)
    {
      offset = pt->read_end - pt->read_pos;
      pt->read_buf = pt->saved_read_buf;
      pt->read_pos = pt->saved_read_pos;
      pt->read_end = pt->saved_read_end;
      pt->read_buf_size = pt->saved_read_buf_size;
    }
  else
    offset = 0;

  scm_ptobs[SCM_PTOBNUM (port)].read_flush (port, offset);
}




void 
scm_ungetc (c, port)
     int c;
     SCM port;
{
  scm_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->read_buf == pt->putback_buf)
    /* already using the put-back buffer.  */
    {
      /* enlarge putback_buf if necessary.  */
      if (pt->read_end == pt->read_buf + pt->read_buf_size
	  && pt->read_buf == pt->read_pos)
	{
	  int new_size = pt->read_buf_size * 2;
	  unsigned char *tmp = 
	    (unsigned char *) realloc (pt->putback_buf, new_size);

	  if (tmp == NULL)
	    scm_memory_error ("scm_ungetc");
	  pt->read_pos = pt->read_buf = pt->putback_buf = tmp;
	  pt->read_end = pt->read_buf + pt->read_buf_size;
	  pt->read_buf_size = pt->putback_buf_size = new_size;
	}

      /* shift any existing bytes to buffer + 1.  */
      if (pt->read_pos == pt->read_end)
	pt->read_end = pt->read_buf + 1;
      else if (pt->read_pos != pt->read_buf + 1)
	{
	  int count = pt->read_end - pt->read_pos;

	  memmove (pt->read_buf + 1, pt->read_pos, count);
	  pt->read_end = pt->read_buf + 1 + count;
	}

      pt->read_pos = pt->read_buf;
    }
  else
    /* switch to the put-back buffer.  */
    {
      if (pt->putback_buf == NULL)
	{
	  pt->putback_buf = (char *) malloc (pt->putback_buf_size);
	  if (pt->putback_buf == NULL)
	    scm_memory_error ("scm_ungetc");
	  pt->putback_buf_size = SCM_INITIAL_PUTBACK_BUF_SIZE;
	}

      pt->saved_read_buf = pt->read_buf;
      pt->saved_read_pos = pt->read_pos;
      pt->saved_read_end = pt->read_end;
      pt->saved_read_buf_size = pt->read_buf_size;

      pt->read_pos = pt->read_buf = pt->putback_buf;
      pt->read_end = pt->read_buf + 1;
      pt->read_buf_size = pt->putback_buf_size;
    }

  *pt->read_buf = c;

  if (pt->rw_random)
    pt->rw_active = SCM_PORT_READ;

  if (c == '\n')
    {
      /* What should col be in this case?
       * We'll leave it at -1.
       */
      SCM_LINUM (port) -= 1;
    }
  else
    SCM_COL(port) -= 1;
}


void 
scm_ungets (s, n, port)
     char *s;
     int n;
     SCM port;
{
  /* This is simple minded and inefficient, but unreading strings is
   * probably not a common operation, and remember that line and
   * column numbers have to be handled...
   *
   * Please feel free to write an optimized version!
   */
  while (n--)
    scm_ungetc (s[n], port);
}


SCM_PROC(s_peek_char, "peek-char", 0, 1, 0, scm_peek_char);

SCM 
scm_peek_char (port)
     SCM port;
{
  int c;
  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG1, s_peek_char);
  c = scm_getc (port);
  if (EOF == c)
    return SCM_EOF_VAL;
  scm_ungetc (c, port);
  return SCM_MAKICHR (c);
}

SCM_PROC (s_unread_char, "unread-char", 2, 0, 0, scm_unread_char);

SCM 
scm_unread_char (cobj, port)
     SCM cobj;
     SCM port;
{
  int c;

  SCM_ASSERT (SCM_ICHRP (cobj), cobj, SCM_ARG1, s_unread_char);

  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port), port, SCM_ARG2, s_unread_char);


  c = SCM_ICHR (cobj);

  scm_ungetc (c, port);
  return cobj;
}

SCM_PROC (s_unread_string, "unread-string", 2, 0, 0, scm_unread_string);

SCM 
scm_unread_string (str, port)
     SCM str;
     SCM port;
{
  SCM_ASSERT (SCM_NIMP (str) && SCM_STRINGP (str),
	      str, SCM_ARG1, s_unread_string);

  if (SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (SCM_NIMP (port) && SCM_OPINPORTP (port),
		port, SCM_ARG2, s_unread_string);

  scm_ungets (SCM_ROUCHARS (str), SCM_LENGTH (str), port);
  
  return str;
}

SCM_PROC (s_lseek, "lseek", 3, 0, 0, scm_lseek);
SCM 
scm_lseek (SCM object, SCM offset, SCM whence)
{
  off_t off;
  off_t rv;
  int how;

  object = SCM_COERCE_OUTPORT (object);

  off = scm_num2long (offset, (char *)SCM_ARG2, s_lseek);
  SCM_ASSERT (SCM_INUMP (whence), whence, SCM_ARG3, s_lseek);
  how = SCM_INUM (whence);
  if (how != SEEK_SET && how != SEEK_CUR && how != SEEK_END)
    scm_out_of_range (s_lseek, whence);
  if (SCM_NIMP (object) && SCM_OPPORTP (object))
    {
      scm_port *pt = SCM_PTAB_ENTRY (object);
      scm_ptobfuns *ptob = scm_ptobs + SCM_PTOBNUM (object);

      if (!ptob->seek)
	scm_misc_error (s_lseek, "port is not seekable",
			scm_cons (object, SCM_EOL));
      else
	{
	  if (pt->rw_active == SCM_PORT_READ)
	    scm_read_flush (object);
	  else if (pt->rw_active == SCM_PORT_WRITE)
	    ptob->fflush (object);
	  
	  rv = ptob->seek (object, off, how);
	}
    }
  else /* file descriptor?.  */
    {
      SCM_ASSERT (SCM_INUMP (object), object, SCM_ARG1, s_lseek);
      rv = lseek (SCM_INUM (object), off, how);
      if (rv == -1)
	scm_syserror (s_lseek);
    }
  return scm_long2num (rv);
}

SCM_PROC (s_ftruncate, "ftruncate", 1, 1, 0, scm_ftruncate);

SCM
scm_ftruncate (SCM port, SCM length)
{
  scm_port *pt;
  scm_ptobfuns *ptob;

  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_OPOUTPORTP (port), port, SCM_ARG1,
	      s_ftruncate);
  pt = SCM_PTAB_ENTRY (port);
  ptob = scm_ptobs + SCM_PTOBNUM (port);
  if (!ptob->ftruncate)
    scm_misc_error (s_ftruncate, "port is not truncatable",
		    scm_cons (port, SCM_EOL));
  if (SCM_UNBNDP (length))
    {
      length = scm_lseek (port, SCM_INUM0, SCM_MAKINUM (SEEK_CUR));
    }
  if (pt->rw_active == SCM_PORT_READ)
    scm_read_flush (port);
  else if (pt->rw_active == SCM_PORT_WRITE)
    ptob->fflush (port);

  {
    off_t c_length = scm_num2long (length, (char *)SCM_ARG2, s_ftruncate);

    if (c_length < 0)
      scm_misc_error (s_ftruncate, "negative offset", 
		      scm_cons (length, SCM_EOL));
		  
    ptob->ftruncate (port, c_length);
  }
  return SCM_UNSPECIFIED;
}

SCM_PROC (s_port_line, "port-line", 1, 0, 0, scm_port_line);

SCM 
scm_port_line (port)
     SCM port;
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port) && SCM_OPENP (port),
	      port,
	      SCM_ARG1,
	      s_port_line);
  return SCM_MAKINUM (SCM_LINUM (port));
}

SCM_PROC (s_set_port_line_x, "set-port-line!", 2, 0, 0, scm_set_port_line_x);

SCM 
scm_set_port_line_x (port, line)
     SCM port;
     SCM line;
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port) && SCM_OPENP (port),
	      port,
	      SCM_ARG1,
	      s_set_port_line_x);
  SCM_ASSERT (SCM_INUMP (line), line, SCM_ARG2, s_set_port_line_x);
  return SCM_PTAB_ENTRY (port)->line_number = SCM_INUM (line);
}

SCM_PROC (s_port_column, "port-column", 1, 0, 0, scm_port_column);

SCM
scm_port_column  (port)
     SCM port;
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port) && SCM_OPENP (port),
	      port,
	      SCM_ARG1,
	      s_port_column);
  return SCM_MAKINUM (SCM_COL (port));
}

SCM_PROC (s_set_port_column_x, "set-port-column!", 2, 0, 0, scm_set_port_column_x);

SCM 
scm_set_port_column_x (port, column)
     SCM port;
     SCM column;
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port) && SCM_OPENP (port),
	      port,
	      SCM_ARG1,
	      s_set_port_column_x);
  SCM_ASSERT (SCM_INUMP (column), column, SCM_ARG2, s_set_port_column_x);
  return SCM_PTAB_ENTRY (port)->column_number = SCM_INUM (column);
}

SCM_PROC (s_port_filename, "port-filename", 1, 0, 0, scm_port_filename);

SCM 
scm_port_filename (port)
     SCM port;
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port) && SCM_OPENP (port),
	      port,
	      SCM_ARG1,
	      s_port_filename);
  return SCM_PTAB_ENTRY (port)->file_name;
}

SCM_PROC (s_set_port_filename_x, "set-port-filename!", 2, 0, 0, scm_set_port_filename_x);

SCM 
scm_set_port_filename_x (port, filename)
     SCM port;
     SCM filename;
{
  port = SCM_COERCE_OUTPORT (port);
  SCM_ASSERT (SCM_NIMP (port) && SCM_PORTP (port) && SCM_OPENP (port),
	      port,
	      SCM_ARG1,
	      s_set_port_filename_x);
  /* We allow the user to set the filename to whatever he likes.  */
  return SCM_PTAB_ENTRY (port)->file_name = filename;
}

#ifndef ttyname
extern char * ttyname();
#endif


void 
scm_prinport (exp, port, type)
     SCM exp;
     SCM port;
     char *type;
{
  scm_puts ("#<", port);
  if (SCM_CLOSEDP (exp))
    scm_puts ("closed: ", port);
  else
    {
      if (SCM_RDNG & SCM_CAR (exp))
	scm_puts ("input: ", port);
      if (SCM_WRTNG & SCM_CAR (exp))
	scm_puts ("output: ", port);
    }
  scm_puts (type, port);
  scm_putc (' ', port);
  if (SCM_OPFPORTP (exp))
    {
      int fdes = (SCM_FSTREAM (exp))->fdes;

      if (isatty (fdes))
	scm_puts (ttyname (fdes), port);
      else
	scm_intprint (fdes, 10, port);
    }
  else
    {
      scm_intprint (SCM_CDR (exp), 16, port);
    }
  scm_putc ('>', port);
}


void
scm_ports_prehistory ()
{
  scm_numptob = 0;
  scm_ptobs = (scm_ptobfuns *) malloc (sizeof (scm_ptobfuns));
  
  /* WARNING: These scm_newptob calls must be done in this order.
   * They must agree with the port declarations in tags.h.
   */
  /* scm_tc16_fport = */ scm_newptob (&scm_fptob);
  /* scm_tc16_pipe was here */ scm_newptob (&scm_fptob); /* dummy.  */
  /* scm_tc16_strport = */ scm_newptob (&scm_stptob);
  /* scm_tc16_sfport = */ scm_newptob (&scm_sfptob);
}



/* Void ports.   */

int scm_tc16_void_port = 0;

static int
print_void_port (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_prinport (exp, port, "void");
  return 1;
}

static void
flush_void_port (SCM port)
{
}

static void
read_flush_void_port (SCM port, int offset)
{
}

static int
close_void_port (SCM port)
{
  return 0;			/* this is ignored by scm_close_port. */
}



static int 
noop0 (SCM stream)
{
  return 0;
}


static struct scm_ptobfuns void_port_ptob =
{
  0, 
  noop0,
  print_void_port,
  0,				/* equal? */
  flush_void_port,
  read_flush_void_port,
  close_void_port,
  0,
  0,
  0,
  0,
};

SCM
scm_void_port (mode_str)
     char * mode_str;
{
  int mode_bits;
  SCM answer;
  scm_port * pt;

  SCM_NEWCELL (answer);
  SCM_DEFER_INTS;
  mode_bits = scm_mode_bits (mode_str);
  pt = scm_add_to_port_table (answer);
  SCM_SETPTAB_ENTRY (answer, pt);
  SCM_SETSTREAM (answer, 0);
  SCM_SETCAR (answer, scm_tc16_void_port | mode_bits);
  SCM_ALLOW_INTS;
  return answer;
}


SCM_PROC (s_sys_make_void_port, "%make-void-port", 1, 0, 0, scm_sys_make_void_port);

SCM
scm_sys_make_void_port (mode)
     SCM mode;
{
  SCM_ASSERT (SCM_NIMP (mode) && SCM_ROSTRINGP (mode), mode,
	      SCM_ARG1, s_sys_make_void_port);

  SCM_COERCE_SUBSTR (mode);
  return scm_void_port (SCM_ROCHARS (mode));
}


/* Initialization.  */

void
scm_init_ports ()
{
  /* lseek() symbols.  */
  scm_sysintern ("SEEK_SET", SCM_MAKINUM (SEEK_SET));
  scm_sysintern ("SEEK_CUR", SCM_MAKINUM (SEEK_CUR));
  scm_sysintern ("SEEK_END", SCM_MAKINUM (SEEK_END));

  scm_tc16_void_port = scm_newptob (&void_port_ptob);
#include "ports.x"
}
