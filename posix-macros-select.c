/* file: select-macros.c
   --------------------
   These subprograms provide access to POSIX functionality that is
   provided for C programs via macros.

   ....The treatment here if the macros are not defined is probably not
   very useful, but we need to do something to allow the code to compile
   on systems that are missing the macros.  The Ada-side code should
   probably also check whether we have this feature.

 */

/*
#define _REENTRANT
#include <sys/select.h>
#include <sys/types.h>
#include <stdio.h>
*/
#include "pconfig.h"
#include "config.h"

#undef DEBUG

#if HAVE_fd_set

void c_fd_set (int fd, fd_set *fdsetp) {
#ifdef FD_SET
   FD_SET (fd, fdsetp);
#else
   errno = ENOSYS;
   return -1;
#endif   
#ifdef DEBUG
   printf ("   ccc...set fd:%d =>%04x\n", fd, fdsetp->fds_bits[0]);
#endif
}
void c_fd_clr (int fd, fd_set *fdsetp) {
#ifdef FD_CLR
   FD_CLR (fd, fdsetp);
#else
   errno = ENOSYS;
   return -1;
#endif   
#ifdef DEBUG
   printf ("   ccc...cleared fd:%d =>%04x\n", fd, fdsetp->fds_bits[0]);
#endif
}
int c_fd_isset (int fd, fd_set *fdsetp) {
#ifdef DEBUG
   printf ("   ccc...testing fd:%d in:%04x =>%d\n",
     fd, fdsetp->fds_bits[0], FD_ISSET (fd, fdsetp));
#endif
#ifdef FD_ISSET
   return FD_ISSET (fd, fdsetp);
#else
   errno = ENOSYS;
   return -1;
#endif   
}
void c_fd_zero (fd_set *fdsetp) {
#ifdef DEBUG
   printf ("   ccc...zero fdset:%04x =>", fdsetp->fds_bits[0]);
#endif
#ifdef FD_ZERO
   FD_ZERO (fdsetp);
#else
   errno = ENOSYS;
   return -1;
#endif   
#ifdef DEBUG
   printf ("%04x\n", fdsetp->fds_bits[0]);
#endif
}

#endif
