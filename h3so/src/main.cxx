#include "h3so.hxx"

__attribute__((constructor))
void ctor() {
  log("constructor");
  attach();
  log("exit constructor");
}

__attribute__((destructor))
void dtor() {
  log("destructor");
  detach();
  log("exit destructor");
}