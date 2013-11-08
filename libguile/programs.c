/* Copyright (C) 2001, 2009, 2010, 2011, 2012, 2013 Free Software Foundation, Inc.
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

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>
#include "_scm.h"
#include "instructions.h"
#include "modules.h"
#include "programs.h"
#include "procprop.h" /* scm_sym_name */
#include "srcprop.h"  /* scm_sym_filename */
#include "vm.h"


static SCM write_program = SCM_BOOL_F;

SCM_DEFINE (scm_make_program, "make-program", 1, 2, 0,
	    (SCM objcode, SCM objtable, SCM free_variables),
	    "")
#define FUNC_NAME s_scm_make_program
{
  SCM_VALIDATE_OBJCODE (1, objcode);
  if (SCM_UNLIKELY (SCM_UNBNDP (objtable)))
    objtable = SCM_BOOL_F;
  else if (scm_is_true (objtable))
    SCM_VALIDATE_VECTOR (2, objtable);

  if (SCM_UNBNDP (free_variables) || scm_is_false (free_variables))
    {
      SCM ret = scm_words (scm_tc7_program, 3);
      SCM_SET_CELL_OBJECT_1 (ret, objcode);
      SCM_SET_CELL_OBJECT_2 (ret, objtable);
      return ret;
    }
  else
    {
      size_t i, len;
      SCM ret;
      SCM_VALIDATE_VECTOR (3, free_variables);
      len = scm_c_vector_length (free_variables);
      if (SCM_UNLIKELY (len >> 16))
        SCM_OUT_OF_RANGE (3, free_variables);
      ret = scm_words (scm_tc7_program | (len<<16), 3 + len);
      SCM_SET_CELL_OBJECT_1 (ret, objcode);
      SCM_SET_CELL_OBJECT_2 (ret, objtable);
      for (i = 0; i < len; i++)
        SCM_SET_CELL_OBJECT (ret, 3+i,
                             SCM_SIMPLE_VECTOR_REF (free_variables, i));
      return ret;
    }
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_rtl_program, "make-rtl-program", 1, 2, 0,
	    (SCM bytevector, SCM byte_offset, SCM free_variables),
	    "")
#define FUNC_NAME s_scm_make_rtl_program
{
  scm_t_uint8 *code;
  scm_t_uint32 offset;

  if (!scm_is_bytevector (bytevector))
    scm_wrong_type_arg (FUNC_NAME, 1, bytevector);
  if (SCM_UNBNDP (byte_offset))
    offset = 0;
  else
    {
      offset = scm_to_uint32 (byte_offset);
      if (offset > SCM_BYTEVECTOR_LENGTH (bytevector))
        SCM_OUT_OF_RANGE (2, byte_offset);
    }

  code = (scm_t_uint8*) SCM_BYTEVECTOR_CONTENTS (bytevector) + offset;
  if (((scm_t_uintptr) code) % 4)
    SCM_OUT_OF_RANGE (2, byte_offset);

  if (SCM_UNBNDP (free_variables) || scm_is_false (free_variables))
    return scm_cell (scm_tc7_rtl_program, (scm_t_bits) code);
  else
    abort ();
}
#undef FUNC_NAME

SCM_DEFINE (scm_rtl_program_code, "rtl-program-code", 1, 0, 0,
            (SCM program),
            "")
#define FUNC_NAME s_scm_rtl_program_code
{
  SCM_VALIDATE_RTL_PROGRAM (1, program);

  return scm_from_uintptr_t ((scm_t_uintptr) SCM_RTL_PROGRAM_CODE (program));
}
#undef FUNC_NAME

SCM
scm_i_rtl_program_name (SCM program)
{
  static SCM rtl_program_name = SCM_BOOL_F;

  if (SCM_PRIMITIVE_P (program))
    return SCM_SUBR_NAME (program);

  if (scm_is_false (rtl_program_name) && scm_module_system_booted_p)
    rtl_program_name =
        scm_c_private_variable ("system vm program", "rtl-program-name");

  return scm_call_1 (scm_variable_ref (rtl_program_name), program);
}

SCM
scm_i_rtl_program_documentation (SCM program)
{
  static SCM rtl_program_documentation = SCM_BOOL_F;

  if (SCM_PRIMITIVE_P (program))
    return SCM_BOOL_F;

  if (scm_is_false (rtl_program_documentation) && scm_module_system_booted_p)
    rtl_program_documentation =
      scm_c_private_variable ("system vm program",
                              "rtl-program-documentation");

  return scm_call_1 (scm_variable_ref (rtl_program_documentation), program);
}

SCM
scm_i_rtl_program_properties (SCM program)
{
  static SCM rtl_program_properties = SCM_BOOL_F;

  if (SCM_PRIMITIVE_P (program))
    {
      SCM name = scm_i_rtl_program_name (program);
      if (scm_is_false (name))
        return SCM_EOL;
      return scm_acons (scm_sym_name, name, SCM_EOL);
    }

  if (scm_is_false (rtl_program_properties) && scm_module_system_booted_p)
    rtl_program_properties =
      scm_c_private_variable ("system vm program", "rtl-program-properties");

  return scm_call_1 (scm_variable_ref (rtl_program_properties), program);
}

void
scm_i_program_print (SCM program, SCM port, scm_print_state *pstate)
{
  static int print_error = 0;

  if (scm_is_false (write_program) && scm_module_system_booted_p)
    write_program = scm_c_private_variable ("system vm program",
                                            "write-program");
  
  if (SCM_PROGRAM_IS_CONTINUATION (program))
    {
      /* twingliness */
      scm_puts_unlocked ("#<continuation ", port);
      scm_uintprint (SCM_UNPACK (program), 16, port);
      scm_putc_unlocked ('>', port);
    }
  else if (SCM_PROGRAM_IS_PARTIAL_CONTINUATION (program))
    {
      /* twingliness */
      scm_puts_unlocked ("#<partial-continuation ", port);
      scm_uintprint (SCM_UNPACK (program), 16, port);
      scm_putc_unlocked ('>', port);
    }
  else if (scm_is_false (write_program) || print_error)
    {
      if (SCM_RTL_PROGRAM_P (program))
        {
          scm_puts_unlocked ("#<rtl-program ", port);
          scm_uintprint (SCM_UNPACK (program), 16, port);
          scm_putc_unlocked (' ', port);
          scm_uintprint ((scm_t_uintptr) SCM_RTL_PROGRAM_CODE (program), 16, port);
          scm_putc_unlocked ('>', port);
        }
      else
        {
          scm_puts_unlocked ("#<program ", port);
          scm_uintprint (SCM_UNPACK (program), 16, port);
          scm_putc_unlocked ('>', port);
        }
    }
  else
    {
      print_error = 1;
      scm_call_2 (SCM_VARIABLE_REF (write_program), program, port);
      print_error = 0;
    }
}


/*
 * Scheme interface
 */

SCM_DEFINE (scm_program_p, "program?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_program_p
{
  return scm_from_bool (SCM_PROGRAM_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_rtl_program_p, "rtl-program?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_rtl_program_p
{
  return scm_from_bool (SCM_RTL_PROGRAM_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_primitive_p, "primitive?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_primitive_p
{
  return scm_from_bool (SCM_PRIMITIVE_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_primitive_call_ip, "primitive-call-ip", 1, 0, 0,
	    (SCM prim),
	    "")
#define FUNC_NAME s_scm_primitive_p
{
  SCM_MAKE_VALIDATE (1, prim, PRIMITIVE_P);

  return scm_from_uintptr_t (scm_i_primitive_call_ip (prim));
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_base, "program-base", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_base
{
  const struct scm_objcode *c_objcode;

  SCM_VALIDATE_PROGRAM (1, program);

  c_objcode = SCM_PROGRAM_DATA (program);
  return scm_from_unsigned_integer ((scm_t_bits) SCM_C_OBJCODE_BASE (c_objcode));
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_objects, "program-objects", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_objects
{
  SCM_VALIDATE_PROGRAM (1, program);
  return SCM_PROGRAM_OBJTABLE (program);
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_module, "program-module", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_module
{
  SCM objs, mod;
  SCM_VALIDATE_PROGRAM (1, program);
  objs = SCM_PROGRAM_OBJTABLE (program);
  /* If a program is the result of compiling GLIL to assembly, then if
     it has an objtable, the first entry will be a module.  But some
     programs are hand-coded trampolines, like boot programs and
     primitives and the like.  So if a program happens to have a
     non-module in the first slot of the objtable, assume that it is
     such a trampoline, and just return #f for the module.  */
  mod = scm_is_true (objs) ? scm_c_vector_ref (objs, 0) : SCM_BOOL_F;
  return SCM_MODULEP (mod) ? mod : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_meta, "program-meta", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_meta
{
  SCM metaobj;
  
  SCM_VALIDATE_PROGRAM (1, program);

  metaobj = scm_objcode_meta (SCM_PROGRAM_OBJCODE (program));
  if (scm_is_true (metaobj))
    return scm_make_program (metaobj, SCM_PROGRAM_OBJTABLE (program),
                             SCM_BOOL_F);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_bindings, "program-bindings", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_bindings
{
  SCM meta;
  
  SCM_VALIDATE_PROGRAM (1, program);

  meta = scm_program_meta (program);
  if (scm_is_false (meta))
    return SCM_BOOL_F;
  
  return scm_car (scm_call_0 (meta));
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_sources, "%program-sources", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_sources
{
  SCM meta, sources, ret, filename;
  
  SCM_VALIDATE_PROGRAM (1, program);

  meta = scm_program_meta (program);
  if (scm_is_false (meta))
    return SCM_EOL;
  
  filename = SCM_BOOL_F;
  ret = SCM_EOL;
  for (sources = scm_cadr (scm_call_0 (meta)); !scm_is_null (sources);
       sources = scm_cdr (sources))
    {
      SCM x = scm_car (sources);
      if (scm_is_pair (x))
        {
          if (scm_is_number (scm_car (x)))
            {
              SCM addr = scm_car (x);
              ret = scm_acons (addr, scm_cons (filename, scm_cdr (x)),
                               ret);
            }
          else if (scm_is_eq (scm_car (x), scm_sym_filename))
            filename = scm_cdr (x);
        }
    }
  return scm_reverse_x (ret, SCM_UNDEFINED);
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_arities, "program-arities", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_arities
{
  SCM meta;
  
  SCM_VALIDATE_PROGRAM (1, program);

  meta = scm_program_meta (program);
  if (scm_is_false (meta))
    return SCM_BOOL_F;

  return scm_caddr (scm_call_0 (meta));
}
#undef FUNC_NAME

SCM
scm_i_program_properties (SCM program)
#define FUNC_NAME "%program-properties"
{
  SCM meta;
  
  SCM_VALIDATE_PROGRAM (1, program);

  meta = scm_program_meta (program);
  if (scm_is_false (meta))
    return SCM_EOL;
  
  return scm_cdddr (scm_call_0 (meta));
}
#undef FUNC_NAME

SCM
scm_find_source_for_addr (SCM ip)
{
  static SCM source_for_addr = SCM_BOOL_F;

  if (scm_is_false (source_for_addr)) {
    if (!scm_module_system_booted_p)
      return SCM_BOOL_F;

    source_for_addr =
      scm_c_private_variable ("system vm program", "source-for-addr");
  }

  return scm_call_1 (scm_variable_ref (source_for_addr), ip);
}

SCM
scm_program_source (SCM program, SCM ip, SCM sources)
{
  static SCM program_source = SCM_BOOL_F;

  if (scm_is_false (program_source)) {
    if (!scm_module_system_booted_p)
      return SCM_BOOL_F;

    program_source =
      scm_c_private_variable ("system vm program", "program-source");
  }

  if (SCM_UNBNDP (sources))
    return scm_call_2 (scm_variable_ref (program_source), program, ip);
  else
    return scm_call_3 (scm_variable_ref (program_source), program, ip, sources);
}
    
SCM_DEFINE (scm_program_num_free_variables, "program-num-free-variables", 1, 0, 0,
	    (SCM program),
	    "")
#define FUNC_NAME s_scm_program_num_free_variables
{
  if (SCM_RTL_PROGRAM_P (program)) {
    return scm_from_ulong (SCM_RTL_PROGRAM_NUM_FREE_VARIABLES (program));
  }

  SCM_VALIDATE_PROGRAM (1, program);
  return scm_from_ulong (SCM_PROGRAM_NUM_FREE_VARIABLES (program));
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_free_variable_ref, "program-free-variable-ref", 2, 0, 0,
	    (SCM program, SCM i),
	    "")
#define FUNC_NAME s_scm_program_free_variable_ref
{
  unsigned long idx;

  if (SCM_RTL_PROGRAM_P (program)) {
    SCM_VALIDATE_ULONG_COPY (2, i, idx);
    if (idx >= SCM_RTL_PROGRAM_NUM_FREE_VARIABLES (program))
      SCM_OUT_OF_RANGE (2, i);
    return SCM_RTL_PROGRAM_FREE_VARIABLE_REF (program, idx);
  }

  SCM_VALIDATE_PROGRAM (1, program);
  SCM_VALIDATE_ULONG_COPY (2, i, idx);
  if (idx >= SCM_PROGRAM_NUM_FREE_VARIABLES (program))
    SCM_OUT_OF_RANGE (2, i);
  return SCM_PROGRAM_FREE_VARIABLE_REF (program, idx);
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_free_variable_set_x, "program-free-variable-set!", 3, 0, 0,
	    (SCM program, SCM i, SCM x),
	    "")
#define FUNC_NAME s_scm_program_free_variable_set_x
{
  unsigned long idx;

  if (SCM_RTL_PROGRAM_P (program)) {
    SCM_VALIDATE_ULONG_COPY (2, i, idx);
    if (idx >= SCM_RTL_PROGRAM_NUM_FREE_VARIABLES (program))
      SCM_OUT_OF_RANGE (2, i);
    SCM_RTL_PROGRAM_FREE_VARIABLE_SET (program, idx, x);
    return SCM_UNSPECIFIED;
  }

  SCM_VALIDATE_PROGRAM (1, program);
  SCM_VALIDATE_ULONG_COPY (2, i, idx);
  if (idx >= SCM_PROGRAM_NUM_FREE_VARIABLES (program))
    SCM_OUT_OF_RANGE (2, i);
  SCM_PROGRAM_FREE_VARIABLE_SET (program, idx, x);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_program_objcode, "program-objcode", 1, 0, 0,
	    (SCM program),
	    "Return a @var{program}'s object code.")
#define FUNC_NAME s_scm_program_objcode
{
  SCM_VALIDATE_PROGRAM (1, program);

  return SCM_PROGRAM_OBJCODE (program);
}
#undef FUNC_NAME

/* procedure-minimum-arity support. */
static void
parse_arity (SCM arity, int *req, int *opt, int *rest)
{
  SCM x = scm_cddr (arity);
  
  if (scm_is_pair (x))
    {
      *req = scm_to_int (scm_car (x));
      x = scm_cdr (x);
      if (scm_is_pair (x))
        {
          *opt = scm_to_int (scm_car (x));
          x = scm_cdr (x);
          if (scm_is_pair (x))
            *rest = scm_is_true (scm_car (x));
          else
            *rest = 0;
        }
      else
        *opt = *rest = 0;
    }
  else
    *req = *opt = *rest = 0;
}
  
static int
scm_i_rtl_program_minimum_arity (SCM program, int *req, int *opt, int *rest)
{
  static SCM rtl_program_minimum_arity = SCM_BOOL_F;
  SCM l;

  if (SCM_PRIMITIVE_P (program))
    return scm_i_primitive_arity (program, req, opt, rest);

  if (SCM_PROGRAM_IS_FOREIGN (program))
    return scm_i_foreign_arity (program, req, opt, rest);

  if (SCM_PROGRAM_IS_CONTINUATION (program)
      || SCM_PROGRAM_IS_PARTIAL_CONTINUATION (program))
    {
      *req = *opt = 0;
      *rest = 1;
      return 1;
    }

  if (scm_is_false (rtl_program_minimum_arity) && scm_module_system_booted_p)
    rtl_program_minimum_arity =
        scm_c_private_variable ("system vm program",
                                "rtl-program-minimum-arity");

  l = scm_call_1 (scm_variable_ref (rtl_program_minimum_arity), program);
  if (scm_is_false (l))
    return 0;

  *req = scm_to_int (scm_car (l));
  *opt = scm_to_int (scm_cadr (l));
  *rest = scm_is_true (scm_caddr (l));

  return 1;
}

int
scm_i_program_arity (SCM program, int *req, int *opt, int *rest)
{
  SCM arities;
  
  if (SCM_RTL_PROGRAM_P (program))
    return scm_i_rtl_program_minimum_arity (program, req, opt, rest);

  arities = scm_program_arities (program);
  if (!scm_is_pair (arities))
    return 0;

  parse_arity (scm_car (arities), req, opt, rest);
  arities = scm_cdr (arities);
  
  for (; scm_is_pair (arities); arities = scm_cdr (arities))
    {
      int thisreq, thisopt, thisrest;

      parse_arity (scm_car (arities), &thisreq, &thisopt, &thisrest);

      if (thisreq < *req
          || (thisreq == *req
              && ((thisrest && (!*rest || thisopt > *opt))
                  || (!thisrest && !*rest && thisopt > *opt))))
        {
          *req = thisreq;
          *opt = thisopt;
          *rest = thisrest;
        }
    }

  return 1;
}



void
scm_bootstrap_programs (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_programs",
                            (scm_t_extension_init_func)scm_init_programs, NULL);
}

void
scm_init_programs (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/programs.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
