#include "pconfig.h"

char *c_inet_ntoa(struct in_addr *in)
{
   return inet_ntoa (*in);
}
#if 0
----------------------
-- REVISION HISTORY --
----------------------

----------------------------
revision 1.1  locked by: baker;
date: 1997/11/03 12:16:58;  author: baker;  state: Exp;
Integrated
---------------------------------
** New changes after this line and before endif. **
#endif
