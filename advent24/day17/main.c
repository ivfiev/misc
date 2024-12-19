#include "advent.h"

#define INSTRUCTION(opcode_num, code) \
  if (opcode == opcode_num) do { code } while (0)
#define COMBO_RVAL \
  (operand == 4 ? regs->A : operand == 5 ? regs->B : operand == 6 ? regs->C : operand)
#define LITERAL_RVAL \
  operand

typedef struct {
  uint64_t A, B, C;
} registers;

void parse_input(char **lines, registers *regs, char *code) {
  char buf[128];
  sscanf(lines[0], "Register A: %ld", &regs->A);
  sscanf(lines[1], "Register B: %ld", &regs->B);
  sscanf(lines[2], "Register C: %ld", &regs->C);
  sscanf(lines[4], "Program: %s", buf);
  PARSE_LINE(buf, ",", sizeof(buf), {
    code[token_ix] = atoi(token);
  });
}

int run(registers *regs, char *code, int i, char **output) {
  int opcode = code[i], operand = code[i + 1]; 
  if (!IN_RANGE(0, opcode, 7) || !IN_RANGE(0, operand, 6)) {
    return -1;
  }
  INSTRUCTION(0, { 
    regs->A = (uint64_t)((double)regs->A / pow(2, COMBO_RVAL)); 
  });
  INSTRUCTION(1, { 
    regs->B ^= LITERAL_RVAL; 
  });
  INSTRUCTION(2, { 
    regs->B = COMBO_RVAL & 0x7;
  });
  INSTRUCTION(3, { 
    if (regs->A != 0) {
      return LITERAL_RVAL;
    }
  });
  INSTRUCTION(4, { 
    regs->B = regs->B ^ regs->C;
  });
  INSTRUCTION(5, {
    **output = (char)(COMBO_RVAL & 7);
    (*output)++;
  });
  INSTRUCTION(6, {
    regs->B = (uint64_t)((double)regs->A / pow(2, COMBO_RVAL));  
  });
  INSTRUCTION(7, {
    regs->C = (uint64_t)((double)regs->A / pow(2, COMBO_RVAL));  
  });
  return i + 2;
}

uint64_t search(char *code, int i, uint64_t A) {
  char output[16];
  if (i == sizeof(output)) {
    return A;
  }
  A *= 8;
  for (int a = 0; a < 8; a++) {
    registers regs = {A + a, 0, 0};
    char *out = output;
    for (int ip = 0; ip < sizeof(output); ip = run(&regs, code, ip, &out)); 
    if (output[0] == code[sizeof(output) - i - 1]) {
      uint64_t attempt = search(code, i + 1, A + a);
      if (attempt != 0) {
        return attempt;
      }
    }
  }
  return 0;
}

int main(int argc, char **argv) {
  char *lines[5];
  read_lines(argv[1], lines, 5);
  registers regs;
  char code[16];
  char output[16];
  memset(output, -1, sizeof(output));
  parse_input(lines, &regs, code);
  char *out = output;
  for (int ip = 0; ip < sizeof(output); ip = run(&regs, code, ip, &out)); 
  printf("Part 1: [");
  for (int i = 0; i < sizeof(output) && output[i] >= 0; i++) {
    printf("%d,", output[i]);
  }
  puts("\b]");
  printf("Part 2: [%ld]\n", search(code, 0, 0));
  free_lines(lines, 5);
  return 0;
}