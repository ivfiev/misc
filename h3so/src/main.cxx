#include "h3so.hxx"

__attribute__((constructor))
void ctor() {
  log("constructor");
  inject();
  log("exit constructor");
}

__attribute__((destructor))
void dtor() {
  log("destructor");
  unject();
  log("exit destructor");
}