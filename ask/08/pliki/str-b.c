char *somestr(void) {
  static char s[] = "Hello, world!";
  return s;
}
