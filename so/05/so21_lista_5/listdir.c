#include "csapp.h"

#define DIRBUFSZ 256

static void print_mode(mode_t m) {
  char t;

  if (S_ISDIR(m))
    t = 'd';
  else if (S_ISCHR(m))
    t = 'c';
  else if (S_ISBLK(m))
    t = 'b';
  else if (S_ISREG(m))
    t = '-';
  else if (S_ISFIFO(m))
    t = 'f';
  else if (S_ISLNK(m))
    t = 'l';
  else if (S_ISSOCK(m))
    t = 's';
  else
    t = '?';

  char ur = (m & S_IRUSR) ? 'r' : '-';
  char uw = (m & S_IWUSR) ? 'w' : '-';
  char ux = (m & S_IXUSR) ? 'x' : '-';
  char gr = (m & S_IRGRP) ? 'r' : '-';
  char gw = (m & S_IWGRP) ? 'w' : '-';
  char gx = (m & S_IXGRP) ? 'x' : '-';
  char or = (m & S_IROTH) ? 'r' : '-';
  char ow = (m & S_IWOTH) ? 'w' : '-';
  char ox = (m & S_IXOTH) ? 'x' : '-';

  /* Fix code to report set-uid/set-gid/sticky bit as 'ls' does. */
  if (m & S_ISUID) ux = (m & S_IXUSR) ? 's' : 'S';
  if (m & S_ISGID) gx = (m & S_IXGRP) ? 's' : 'S';
  if (m & S_ISVTX) ox = (m & S_IXOTH) ? 't' : 'T';

  printf("%c%c%c%c%c%c%c%c%c%c", t, ur, uw, ux, gr, gw, gx, or, ow, ox);
}

static void print_uid(uid_t uid) {
  struct passwd *pw = getpwuid(uid);
  if (pw)
    printf(" %10s", pw->pw_name);
  else
    printf(" %10d", uid);
}

static void print_gid(gid_t gid) {
  struct group *gr = getgrgid(gid);
  if (gr)
    printf(" %10s", gr->gr_name);
  else
    printf(" %10d", gid);
}

static void file_info(int dirfd, const char *name) {
  struct stat sb[1];

  /* Read file metadata. */
  fstatat(dirfd, name, sb, 0);

  print_mode(sb->st_mode);
  printf("%4ld", sb->st_nlink);
  print_uid(sb->st_uid);
  print_gid(sb->st_gid);

  /* For devices: print major/minor pair; for other files: size. */
  if (S_ISCHR(sb->st_mode) || S_ISBLK(sb->st_mode)) {
    printf(" %4d, %4d", major(sb->st_rdev), minor(sb->st_rdev));
  } else {
    printf(" %10ld", sb->st_size);
  }


  char *now = ctime(&sb->st_mtime);
  now[strlen(now) - 1] = '\0';
  printf("%26s", now);

  printf("  %s", name);

  if (S_ISLNK(sb->st_mode)) {
    /* Read where symlink points to and print '-> destination' string. */
    char path[257];
    if (readlinkat(dirfd, name, path, 256)) {
      path[257] = '\0';
      printf(" -> %s", path);
    }
  }

  putchar('\n');
}

int main(int argc, char *argv[]) {
  if (!argv[1])
    argv[1] = ".";

  int dirfd = Open(argv[1], O_RDONLY | O_DIRECTORY, 0);
  char buf[DIRBUFSZ];
  int n;

  while ((n = Getdents(dirfd, (void *)buf, DIRBUFSZ))) {
    struct linux_dirent *d;

    /* Iterate over directory entries and call file_info on them. */

    d = (struct linux_dirent *) buf;
    for (;;) {
      file_info(dirfd, d->d_name);
      if (((void *) d) + d->d_reclen < (void *) buf + n) {
        d = (void *) d + d->d_reclen;
      } else {
        break;
      }
    }
  }

  Close(dirfd);
  return EXIT_SUCCESS;
}

// vim: et sw=2 ts=2
