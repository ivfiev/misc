#include <iostream>
#include <cstring>

class X {
public:
  X(int i) {

  }
  int test() {
    return 5;
  }
  ~X(){}
};

void by_ref(int &r) {
  r = 5;
}

union word {
  int i;
  float f;
};

struct s {
  int i;
};

int main() {
  int i = 7;
  double xs[i * i];
  by_ref(i);
  std::cout << i;
  for (auto x : xs) {
    std::cout << x << std::endl;
  }
  if (errno) {
    std::cout << strerror(errno);
  }
  auto x = X(5);
  x.test();
  word w;
  s s;
  return 0;
}
