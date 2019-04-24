#include "test.h"

static float data[] = { -1.0, 0.0, 0.5 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  jit_load_args_1(j, jit_operand_gpr (JIT_OPERAND_ABI_INTMAX, JIT_R0));

  jit_ldxi_f(j, JIT_F0, JIT_R0, (uintptr_t)data);
  jit_retr_f(j, JIT_F0);

  float (*f)(uintmax_t) = jit_end(j, NULL);

  ASSERT(f(0) == data[0]);
  ASSERT(f(4) == data[1]);
  ASSERT(f(8) == data[2]);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}