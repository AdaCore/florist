#define _XOPEN_SOURCE_EXTENDED
#define _INCLUDE_HPUX_SOURCE
#define _REENTRANT
/* For HP-UX with DCE threads,
   we need pthread.h first, to get the desired
   effect, in the presence of #define sigaction cma_sigaction.
 */
#include <pthread.h>
#define pthread_sigmask sigprocmask
