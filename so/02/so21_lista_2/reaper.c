#include "csapp.h"

static pid_t spawn(void (*fn)(void)) {
  pid_t pid = Fork();
  if (pid == 0) {
    fn();
    printf("(%d) I'm done!\n", getpid());
    exit(EXIT_SUCCESS);
  }
  return pid;
}

static void grandchild(void) {
  printf("(%d) Waiting for signal!\n", getpid());
  pause();
  printf("(%d) Got the signal!\n", getpid());
}

static void child(void) {
  pid_t pid;
  /* If pid is zero, then the process ID of the calling process is used.  If
   * pgid is zero, then the PGID of the process specified by pid is made the
   * same as its process ID.  */
  setpgid(0, 0);
  pid = spawn(grandchild);
  printf("(%d) Grandchild (%d) spawned!\n", getpid(), pid);
}

/* Runs command "ps -o pid,ppid,pgrp,stat,cmd" using execve(2). */
static void ps(void) {
  char * const argv[] = { "ps", "-o", "pid,ppid,pgrp,stat,cmd", NULL };
  execvp("ps", argv);
}

int main(void) {
  Prctl(PR_SET_CHILD_SUBREAPER, 1);
  printf("(%d) I'm a reaper now!\n", getpid());

  pid_t pid, pgrp;
  int status;

  pgrp = spawn(child);
  if (waitpid(pgrp, NULL, 0) == -1) {
    printf("waiting for child failed\n");
  }

  pid = spawn(ps);
  waitpid(pid, NULL, 0);

  /* If pid is less than -1, then sig is sent to every process in the process
   * group whose ID is -pid. */
  kill(-pgrp, SIGINT);
  /* wait for any child process whose process group ID is equal to the absolute
   * value of pid. */
  if (waitpid(-pgrp, &status, 0) == -1) {
    printf("waiting for grandchild failed\n");
  }
  printf("(%d) Grandchild returned (%d)\n", getpid(), status);
  return EXIT_SUCCESS;
}

// vim: et sw=2 ts=2
