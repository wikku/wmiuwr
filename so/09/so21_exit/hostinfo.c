#include "csapp.h"

int main(int argc, char **argv) {
  struct addrinfo *p, *listp, hints;
  char buf[MAXLINE];
  char buf2[MAXLINE];
  int rc, flags;

  if (argc > 3)
    app_error("usage: %s <domain name> [service]\n", argv[0]);
  const char* service = argc > 2 ? argv[2] : NULL;

  /* Get a list of addrinfo records */
  memset(&hints, 0, sizeof(struct addrinfo));
  //hints.ai_family = AF_INET; /* IPv4 only */
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  /* Connections only */
  if ((rc = getaddrinfo(argv[1], service, &hints, &listp)) != 0)
    gai_error(rc, "getaddrinfo");

  /* Display address string instead of domain name */
  flags = NI_NUMERICHOST | NI_NUMERICSERV;
  /* Walk the list and display each IP address */
  for (p = listp; p; p = p->ai_next) {
    Getnameinfo(p->ai_addr, p->ai_addrlen, buf, MAXLINE, buf2, MAXLINE, flags);
    if (p->ai_family == AF_INET6) {
      printf("[%s]:%s\n", buf, buf2);
    } else {
      printf("%s:%s\n", buf, buf2);
    }
  }

  /* Clean up */
  freeaddrinfo(listp);

  return EXIT_SUCCESS;
}
// vim: et sw=2 ts=2
