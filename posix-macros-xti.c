/* file: posix-macros-xti.c
   ------------------------
   These subprograms provide access to POSIX functionality that is
   provided for C programs via macros.

 */

#include "pconfig.h"
#include "config.h"

#ifndef OPT_NEXTHDR

#ifndef HAVE_struct_t_opthdr
struct t_opthdr {
     unsigned long   len;    /* total length of option; i.e.         */
                             /* sizeof(struct t_opthdr) + length of  */
                             /* option value in bytes                */
     unsigned long   level;  /* protocol value in bytes              */
     unsigned long   name;   /* option name                          */
     unsigned long   status; /* status value                         */
     /* followed by the option value(s)                              */
};
#endif

#define T_ALIGN(p) (((unsigned long)(p) + (sizeof(unsigned long) - 1)) \
					& ~(sizeof(unsigned long) - 1)) 

#define OPT_NEXTHDR(pbuf, buflen, popt) \
  (((char *)(popt) + T_ALIGN((popt)->len) <  \
    (char *)(pbuf) + (buflen)) ? \
    (struct t_opthdr *)((char *)(popt) + T_ALIGN((popt)->len)) : \
    (struct t_opthdr *) 0)

#endif

struct t_opthdr *c_OPT_NEXTHDR ( char *pbuf, unsigned int buflen,
		                 struct t_opthdr *poption ){ 
   return OPT_NEXTHDR(pbuf, buflen, poption); 

}

int fetch_t_errno() {
#if HAVE_t_errno
   return t_errno;
#else
#if HAVE_t_nerr
   return t_nerr;
#else
   return ENOSYS;
#endif
#endif
}
