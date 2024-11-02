#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

class Entity {
public:
  Entity() {
    time = 0;
  }
  void tick() {
    time++;
    printf("Time: %d\n", time);
  }
private:
  uint time;
};

int main() {
  Entity e;
  for (;;) {
    e.tick();
    sleep(4);
  }
  return 0;
}