#include "csapp.h"

/* First address of handled region. */
#define ADDR_START ((void *)0x10000000)
/* Last address of handled region (not inclusive). */
#define ADDR_END ((void *)0x10010000)

static size_t pagesize;

/* Maps anonymouse page with `prot` access permissions at `addr` address. */
static void mmap_page(void *addr, int prot) {
  Mmap(addr, pagesize, prot, MAP_ANONYMOUS | MAP_PRIVATE | MAP_FIXED, -1, 0);
}

/* Changes protection bits to `prot` for page at `addr` address. */
static void mprotect_page(void *addr, int prot) {
  Mprotect(addr, pagesize, prot);
}

static void sigsegv_handler(int signum, siginfo_t *info, void *data) {
  ucontext_t *uc = data;
  intptr_t rip;

  /* TODO: You need to get value of instruction pointer register from `uc`.
   * Print all useful data from `info` and quit in such a way that a shell
   * reports program has been terminated with SIGSEGV. */
  rip = uc->uc_mcontext.gregs[16];
  uintptr_t addr = (uintptr_t) info->si_addr;
  safe_printf("Fault at rip=%lx accessing %lx! ", rip, addr);
  if (addr < (uintptr_t) ADDR_START || (uintptr_t) ADDR_END <= addr) {
    safe_printf("Address not mapped - terminating!\n");
    exit(128 + SIGSEGV);
  }
  uintptr_t page_addr = (addr ^ (addr & (pagesize-1)));
  switch (info->si_code) {
    case SEGV_MAPERR:
      mmap_page((void*) page_addr, PROT_READ);
      safe_printf("Map missing page at %lx.\n", page_addr);
      break;
   case SEGV_ACCERR:
      mprotect_page((void*) page_addr, PROT_READ | PROT_WRITE);
      safe_printf("Make page at %lx writable.\n", page_addr);
      break;
    default: break;
  }
}

int main(int argc, char **argv) {
  pagesize = sysconf(_SC_PAGESIZE);

  /* Register signal handler for SIGSEGV */
  struct sigaction action = {.sa_sigaction = sigsegv_handler,
                             .sa_flags = SA_SIGINFO};
  sigaction(SIGSEGV, &action, NULL);

  /* Initially all pages in the range are either not mapped or readonly! */
  for (void *addr = ADDR_START; addr < ADDR_END; addr += pagesize)
    if (random() % 2)
      mmap_page(addr, PROT_READ);

  /* Generate lots of writes to the region. */
  volatile long *array = ADDR_START;
  long nelems = (ADDR_END - ADDR_START) / sizeof(long);

  for (long i = 0; i < nelems * 2; i++) {
    long index = random() % nelems;
    array[index] = (long)&array[index];
  }

  /* Perform off by one access - triggering a real fault! */
  array[nelems] = 0xDEADC0DE;

  return EXIT_SUCCESS;
}

// vim: et sw=2 ts=2
