#include "csapp.h"

static void signal_handler(int signum, siginfo_t *info, void *data) {
  if (signum == SIGINT) {
    safe_printf("(%d) Screw you guys... I'm going home!\n", getpid());
    _exit(0);
  }
}

static void play(pid_t next, const sigset_t *set) {
  /*
  sigset_t intusr1;
  sigemptyset(&intusr1);
  sigaddset(&intusr1, SIGINT);
  sigaddset(&intusr1, SIGUSR1);
  */
  //sigset_t oldset;
  for (;;) {
    printf("(%d) Waiting for a ball!\n", getpid());
    //sigprocmask(SIG_BLOCK, &intusr1, NULL);
    sigsuspend(set);
    //sigprocmask(SIG_UNBLOCK, &intusr1, NULL);
    usleep((300 + random() % 400) * 1000);
    Kill(next, SIGUSR1);
    printf("(%d) Passing ball to (%d)!\n", getpid(), next);
  }
}

int main(int argc, char *argv[]) {
  if (argc != 2)
    app_error("Usage: %s [CHILDREN]", argv[0]);

  int children = atoi(argv[1]);

  if (children < 4 || children > 20)
    app_error("Give number of children in range from 4 to 20!");

  /* Register signal handler for SIGUSR1 */
  struct sigaction action = {.sa_sigaction = signal_handler};
  Sigaction(SIGINT, &action, NULL);
  Sigaction(SIGUSR1, &action, NULL);

  sigset_t init_mask;
  sigprocmask(SIG_BLOCK, NULL, &init_mask);

  sigset_t sigusr1;
  sigemptyset(&sigusr1);
  sigaddset(&sigusr1, SIGUSR1);
  sigprocmask(SIG_BLOCK, &sigusr1, NULL);

  pid_t prev = getpid();
  for (int i = 0; i < children; i++) {
    pid_t pid = Fork();
    if (pid == 0) {
      play(prev, &init_mask);
      break;
    } else {
      prev = pid;
    }
  }
  Kill(prev, SIGUSR1);
  play(prev, &init_mask);

  return EXIT_SUCCESS;
}

// vim: et sw=2 ts=2
