#include "pconfig.h"

char *c_inet_ntoa(struct in_addr *in)
{
   return inet_ntoa (*in);
}
