#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

char *c_inet_ntoa(struct in_addr *in)
{
   return inet_ntoa (*in);
}
