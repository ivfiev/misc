#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

class Entity {
public:
  Entity() {
    time = 0;
  }
  virtual void tick() {
    time++;
  }
protected:
  uint time;
};

class Player : public Entity {
public:
  void tick() override {
    Entity::tick();
    printf("Time: %d\n", time);
  }
};

Player p0;
Player *p1;

void tick(Entity *e) {
  e->tick();
}

int main() {
  p1 = new Player;
  Player *p2 = new Player;
  Player p3;
  for (;;) {
    tick(&p0);
    tick(p1);
    tick(p2);
    tick(&p3);
    sleep(3);
  }
  return 0;
}