#if ! defined (__addrinfo_h) && ! defined (AI_PASSIVE)
#define	__addrinfo_h

/*
 * Everything here really belongs in <netdb.h>.
 * These defines are separate for now, to avoid having to modify the
 * system's header.
 * The test for AI_PASSIVE in the first line will prevent compilation
 * of this file on any system where these defines ARE included in
 * <netdb.h>, e.g. Solaris 2.8
 */

struct addrinfo {
  int		ai_flags;			/* AI_PASSIVE, AI_CANONNAME */
  int		ai_family;			/* PF_xxx */
  int		ai_socktype;		/* SOCK_xxx */
  int		ai_protocol;		/* IPPROTO_xxx for IPv4 and IPv6 */
  size_t	ai_addrlen;			/* length of ai_addr */
  char		*ai_canonname;		/* canonical name for host */
  struct sockaddr	*ai_addr;	/* binary address */
  struct addrinfo	*ai_next;	/* next structure in linked list */
};

#define	AI_PASSIVE	 1	/* socket is intended for bind() + listen() */
#define	AI_CANONNAME	 2	/* return canonical name */

#define	EAI_ADDRFAMILY	 1	/* address family for host not supported */
#define	EAI_AGAIN	 2	/* temporary failure in name resolution */
#define	EAI_BADFLAGS	 3	/* invalid value for ai_flags */
#define	EAI_FAIL	 4	/* non-recoverable failure in name resolution */
#define	EAI_FAMILY	 5	/* ai_family not supported */
#define	EAI_MEMORY	 6	/* memory allocation failure */
#define	EAI_NODATA	 7	/* no address associated with host */
#define	EAI_NONAME	 8	/* host nor service provided, or not known */
#define	EAI_SERVICE	 9	/* service not supported for ai_socktype */
#define	EAI_SOCKTYPE	10	/* ai_socktype not supported */
#define	EAI_SYSTEM	11	/* system error returned in errno */

			/* function prototypes */
int getaddrinfo(const char *, const char *, const struct addrinfo *,
			 struct addrinfo **);
void freeaddrinfo(struct addrinfo *);
int getnameinfo(const struct sockaddr *, size_t, char *, size_t,
				     char *, size_t);
#endif	/* __addrinfo_h */
