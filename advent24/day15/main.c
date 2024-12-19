#include "advent.h"

#define EMPTY 0
#define ROBOT 1
#define BOX 2
#define WALL 4

typedef struct {
  int row, col, width;
  char type;
} entity;

deque *parse(char **map, size_t height, size_t width) {
#define ADD(type) \
  entity *e = malloc(sizeof(entity)); \
  *e = (entity) {i, j, 1, type}; \
  deq_push_tail(entities, KV(.ptr = e))
  deque *entities = deq_new();
  for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++) {
      if (map[i][j] == '@') {
        ADD(ROBOT);
      } else if (map[i][j] == 'O') {
        ADD(BOX);
      } else if (map[i][j] == '#') {
        ADD(WALL);
      }
    }
  }
  return entities;
}

void scale2(deque *entities) {
  FOREACH_V(entities, {
    entity *e = val.ptr;
    e->width = 2;
    e->col *= 2;
  });
}

int deltas(char v, int *drow, int *dcol) {
  if (v == '^') {
    *drow = -1;
    *dcol = 0;
    return 1;
  }
  if (v == '>') {
    *drow = 0;
    *dcol = 1;
    return 1;
  }
  if (v == '<') {
    *drow = 0;
    *dcol = -1;
    return 1;
  }
  if (v == 'v') {
    *drow = 1;
    *dcol = 0;
    return 1;
  }
  return 0;
}

entity *find_pos(deque *entities, int row, int col, int flag) {
  FOREACH_V(entities, {
    entity *e = val.ptr;
    if (e->row == row && (e->col == col || e->col == col - 1 && e->width == 2) && (e->type & flag)) {
      return e;
    }
  });
  return NULL;
}

void dmove(entity *e, int drow, int dcol) {
  e->row += drow;
  e->col += dcol;
}

int robot_step(deque *entities, entity *robot, char v) {
  int drow, dcol, box;
  if (deltas(v, &drow, &dcol)) {
    int row = robot->row + drow;
    int col = robot->col + dcol;
    entity *e = find_pos(entities, row, col, WALL | BOX);
    if (e == NULL) {
      dmove(robot, drow, dcol);
      return 1;
    }
    if (e->type == WALL) {
      return 0;
    }
    if (e->type == BOX) {
      deque *tomove = deq_new();
      deq_push_tail(tomove, KV(.ptr = e));
      for (;;) {
        int none = 1;
        FOREACH_V(entities, {
          entity *e = val.ptr;
          FOREACH_V(tomove, {
            entity *m = val.ptr;
            if (drow != 0 && (e->row == m->row + drow && (IN_RANGE(e->col, m->col, e->col + e->width - 1) || IN_RANGE(e->col, m->col + m->width - 1, e->col + e->width - 1)))
                || dcol != 0 && (e->row == m->row && e->col == m->col + m->width * dcol)) {
              int dupe = 0;
              FOREACH_V(tomove, {
                if (e == val.ptr) {
                  dupe = 1;
                  break;
                }
              });
              if (!dupe) {
                if (e->type == WALL) { goto CONTINUE; }
                deq_push_tail(tomove, KV(.ptr = e));
                none = 0;
              }
            }
          });
        });
        if (none) goto MOVE;
      }
      MOVE:
      FOREACH_V(tomove, { dmove(val.ptr, drow, dcol); });
      dmove(robot, drow, dcol);
      CONTINUE:
      deq_free(tomove);
    }
  }
  return 0;
}

void debug(deque *entities, size_t height, size_t width, char cmd) {
  printf("%c\n", cmd);
  for (int row = 0; row < height; row++) {
    for (int col = 0; col < width; col++) {
      entity *e = find_pos(entities, row, col, ROBOT | WALL | BOX);
      if (e == NULL) {
        printf(".");
      } else if (e->type == ROBOT) {
        printf("@");
      } else if (e->type == WALL) {
        printf("#");
      } else if (e->type == BOX) {
        printf("O");
      }
    }
    puts("");
  }
  puts("");
}

int main(int argc, char **argv) {
  char *input[1024], *map[1024], *cmds[1024];
  size_t lines = read_lines(argv[1], input, SIZEARR(input));
  size_t height, width, count;
  uint64_t part1 = 0, part2 = 0;
  for (height = 0; height < lines && strcmp(input[height], "\n") != 0; height++) {
    map[height] = input[height];
  }
  width = strlen(*input);
  count = lines - height - 1;
  for (int i = 0; i < count; i++) {
    cmds[i] = input[height + 1 + i];
  }
  // part 1
  entity *robot;
  deque *entities = parse(map, height, width);
  FOREACH_V(entities, { entity *e = val.ptr; if (e->type == ROBOT) { robot = e; break; }});
  for (int i = 0; i < count; i++) {
    for (int c = 0; c < strlen(cmds[i]); c++) {
      robot_step(entities, robot, cmds[i][c]);
      //debug(entities, height, width, cmds[i][c]);
    }
  }
  for (int row = 0; row < height; row++) {
    for (int col = 0; col < width; col++) {
      entity *e = find_pos(entities, row, col, BOX);
      if (e != NULL) {
        part1 += 100 * row + col;
      }
    }
  }
  // part 2
  free(entities);
  entities = parse(map, height, width);
  FOREACH_V(entities, { entity *e = val.ptr; if (e->type == ROBOT) { robot = e; break; }});
  scale2(entities);
  robot->width = 1;
  for (int i = 0; i < count; i++) {
    for (int c = 0; c < strlen(cmds[i]); c++) {
      robot_step(entities, robot, cmds[i][c]);
      //debug(entities, height, width * 2, cmds[i][c]);
    }
  }
  for (int row = 0; row < height * 2; row++) {
    for (int col = 0; col < width * 2; col++) {
      entity *e = find_pos(entities, row, col, BOX);
      if (e != NULL && e->col == col) {
        part2 += 100 * row + col;
      }
    }
  } 

  printf("Part 1: [%ld]\n", part1);
  printf("Part 2: [%ld]\n", part2);
  deq_free(entities);
  free_lines(input, lines);
  return 0;
}