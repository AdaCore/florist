/*----------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                                                                          --
--                             C - P O S I X . C                            --
--                                                                          --
--                                                                          --
--  Copyright (c) 1996-1999 Florida State University (FSU),                 --
--  All Rights Reserved.                                                    --
--                                                                          --
--  This file is a component of FLORIST, an  implementation of an  Ada API  --
--  for the POSIX OS services, for use with  the  GNAT  Ada  compiler  and  --
--  the FSU Gnu Ada Runtime Library (GNARL).   The  interface  is intended  --
--  to be close to that specified in  IEEE STD  1003.5: 1990  and IEEE STD  --
--  1003.5b: 1996.                                                          --
--                                                                          --
--  FLORIST is free software;  you can  redistribute  it and/or  modify it  --
--  under terms of the  GNU  General  Public  License as  published by the  --
--  Free Software Foundation;  either version  2, or (at  your option) any  --
--  later version.  FLORIST is distributed  in  the hope  that  it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without  even the implied  warranty  --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA                   --
--                                                                          --
--  As a special exception, if other files instantiate generics from  this  --
--  unit, or you link this unit with other files to produce an  executable, --
--  this  unit does not by itself cause the  resulting  executable  to  be  --
--  covered  by the  GNU  General  Public License. This exception does not  --
--  however invalidate any other  reasons why the executable file might be  --
--  covered by the GNU Public License.                                      --
--                                                                          --
----------------------------------------------------------------------------*/

/* file: c-posix.c
   ===============
   [$Revision$]

   This program generates the files:

     posix.ads, posix-limits.ads, posix-options.ads, posix-c.ads

 */

/* includes
   ========

   Files pconfig.h and config.h are generated
   by the "configure" script, which in turn is generated
   by running "autoconf" on "configure.in".

 */

#include "pconfig.h"
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include "config.h"
#include <stdarg.h>

#ifdef hpux
/* HP-UX headers define an obsolete (and wrong) OPEN_MAX when
  _XOPEN_SOURCE_EXTENDED is defined. Since we need this macro (_XOPEN.*)
  to get other POSIX definitions, we kludge here by undefining this unwanted
  symbol. */
#undef OPEN_MAX
#endif

int indent = 0;

int ifprintf (FILE *stream, const char * format, ...) {
   va_list arg;
   int done;
   int i;
   va_start(arg, format);
   for (i = 0; i < indent; i++) fprintf(stream, "   ");
   done = vfprintf(stream, format, arg);
   va_end(arg);
   return done;
}

/* Files pconfig.h and config.h are generated
   by the "configure" script, which in turn is generated
   by running "autoconf" on "configure.in".
 */

/* .... The following #define may belong in pconfig.h.
   Consider moving it there?
 */

/* TRY_MACRO_LINKNAMES activates a workaround for header files
   that define macros for certain POSIX function names, so that
   a substitute linkname is used.  See macro GFUNC for more info.
 */

#define TRY_MACRO_LINKNAMES

/* Macros
   ======
 */

#define  NON_SUPPORT_MESSAGE(NAME)\
  ifprintf(fp,"   --  *** MISSING: %s ***  --\n", NAME);\
  warn("missing",NAME);

/* universal constant declarations
   -------------------------------
   In the original design, we did not account for the possibility
   of long integers coming out as negative with the "%d" format.
   We then added GUCST, but have only used it for known problem
   cases.  By rights, we should go through all the POSIX symbols,
   find the ones that are used sa bit-masks, and make sure they
   all are put out using the unsigned format.
 */

#define GUCST(name, value) \
  ifprintf(fp,"   %s : constant := %u;\n", name, value);

#define GCST(name, value) \
  ifprintf(fp,"   %s : constant := %d;\n", name, value);

int  max_GCST2;
#define GCST2(name, name2, value) \
  ifprintf(fp,"   %s,\n", name);\
  ifprintf(fp,"   %s : constant := %d;\n", name2, value); \
  if (value > max_GCST2) max_GCST2 = value;

#define GDFLT(name, value) \
  NON_SUPPORT_MESSAGE(name)\
  ifprintf(fp,"   %s : constant := %d;\n", name, value);

#define GUFLT(name, value) \
  NON_SUPPORT_MESSAGE(name)\
  ifprintf(fp,"   %s : constant := %u;\n", name, value);

#define GDFLT2(name, name2) \
  NON_SUPPORT_MESSAGE(name)\
  GCST2(name, name2, -1);

/* We use -1 for value above because this may be a missing
   errno value, for which 0 may mean no error.
 */

/* struct type definitions
   -----------------------
   They are only to be used in the sequence
     GT1 (GT2 | GT2A | GT2V | GT2P)+ GT3.
   Do not interleave any other text or macros.
 */

/* GT1
   ---
   start of code to generate a struct/record declaration
   C type name is struct TYPENAME
   Ada type name is struct_TYPENAME
 */
#define GT1(TYPENAME,WE_HAVE_IT)\
void g_struct_##TYPENAME(){\
 int we_have_it = WE_HAVE_IT;\
 struct TYPENAME DUMMY;\
 char const typename [] = #TYPENAME;\
 int typekind = STRUCT_TYPE;\
 component_t comps [] = {\

/* GT1T
   ----
   start of code to generate a struct/record declaration
   C type name is TYPENAME
   Ada type name is struct_TYPENAME
 */
#define GT1T(TYPENAME,WE_HAVE_IT)\
void g_##TYPENAME(){\
 int we_have_it = WE_HAVE_IT;\
 TYPENAME DUMMY;\
 char const typename [] = #TYPENAME;\
 int typekind = TYPEDEF_STRUCT_TYPE;\
 component_t comps [] = {\

/* GT1R
   ---
   start of code to generate a struct/record declaration for structs
   that have a pointer to the struct type as a component.
   C type name is struct TYPENAME
   Ada type name is struct_TYPENAME
 */
#define GT1R(TYPENAME,WE_HAVE_IT)\
void g_struct_##TYPENAME(){\
 int we_have_it = WE_HAVE_IT;\
 struct TYPENAME DUMMY;\
 char const typename [] = #TYPENAME;\
 int typekind = RECURSIVE_STRUCT_TYPE;\
 component_t comps [] = {\

/* GT2
   ---
   component of struct/record type, non-aliased
   component name is COMPNAME
   component C type is CMPTYP
   function ada_type_name maps C type name to Ada
 */
#define GT2(COMPNAME,CMPTYP)\
 #COMPNAME, #CMPTYP, sizeof(CMPTYP),\
 ((char *)&DUMMY.COMPNAME - (char *)&DUMMY), 0,

/* GT2A
   ----
   variant of GT2, for component that is an array
   the component C type is CMPTYP
   which is an array with components of type CMPCMPTYP
   N is the number of components
 */
#define GT2A(COMPNAME,CMPTYP,CMPCMPTYP,N)\
 #COMPNAME, #CMPTYP, sizeof(CMPCMPTYP)*N,\
 ((char *)&DUMMY.COMPNAME - (char *)&DUMMY), 0,

/* GT2V
   ----
   variant of GT2, for component that is volatile
 */
#define GT2V(COMPNAME,CMPTYP)\
 #COMPNAME, #CMPTYP, sizeof(CMPTYP),\
 ((char *)&DUMMY.COMPNAME - (char *)&DUMMY), 1,

/* GT2P
   ----
   variant of GT2, for component that is a pointer
   to a function
   component name is COMPNAME
   component C type name is CMPTYP
   function ada_type_name maps C type name to Ada
 */

void (*DUMMYFPTR) ();

#define GT2P(COMPNAME,CMPTYP)\
 #COMPNAME, #CMPTYP, sizeof(DUMMYFPTR),\
 ((char *)&DUMMY.COMPNAME - (char *)&DUMMY), 0,

/* GT3
   ---
   end of struct/record component list
 */
#define GT3\
 0, 0, 0, 0, 0};\
 if (! we_have_it) {\
 ifprintf(fp,"   --  *** MISSING: %s ***  --\n", typename);\
 warn("missing struct type",typename);}\
 save_type(typename, sizeof(DUMMY), typekind, comps);\
 print_type_declaration(typename,fp);}

/* other macros
   ------------
 */

#ifdef ENOSYS
#else
#define ENOSYS -1
#endif

#define DEFAULTSIZE (sizeof(int)*bits_per_byte)
/* the number of bits in the dummy type that is used
   to stand in for an unsupported type
 */

#define XTI_ERROR_FIRST 10000
#define XTI_ERROR_LAST 19999
#define EAI_ERROR_FIRST 20000
#define EAI_ERROR_LAST 29999
/* offsets applied to error codes from t_errno
   (T_xxxx) and codes from addrinfo (EAI_xxxxx).
 */

/* type declarations
   =================
 */

/* We build a linked list structure to keep track of
   of information about POSIX types
   for which we might want to generate some output, either to the
   configuration files, or to an Ada package specification.
 */

typedef struct component {
    char * compname;     /* name of component */
    char * typename;     /* definition of component type */
    int    size;        /* sizeof (typename) */
    int    offset;      /* offset of component, in bytes */
    int is_volatile; /* 0 = nonvolatile, 1 = volatile */
  } component_t;
  /* describes a record component */

/* type kinds */
#define SIGNED_INTEGER_TYPE 1
#define UNSIGNED_INTEGER_TYPE 2
#define STRUCT_TYPE 3
#define TYPEDEF_STRUCT_TYPE 4
#define OPAQUE_TYPE 5
#define CHAR_ARRAY_TYPE 6
#define RECURSIVE_STRUCT_TYPE 7  /* BLH */
/* .... may need to also define a case for union types
   .... may not need to distinquish struct from typedef */

typedef struct type {
    char *typename;
    int  typesize;
    int  typekind;
    int  is_printed;
    component_t *comps;
    struct type *next;
  } type_t;
  /* describes a type */

type_t *all_type_list = NULL;
type_t **all_type_tail = &all_type_list;

/* subprograms
   ===========
 */

void warn(const char msg1[], const char msg2[]);
void quit(const char msg1[], const char msg2[]);
void save_type
  (char const name[],
   int typesize,
   int typekind,
   component_t const *comps);
void print_type_declaration(char const name[], FILE *fp);
void gieeeheader(const char pkgname[]);
void gspecheader(const char pkgname[]);
void print_ada_type (char const typename[]);
int wordsize(int n);
void gbrg(char name[], char lb[], char ub[]);
void gsitp(char name[], int size);
void gdflsitp(char name[]);
void guitp(char name[], int size);
void gdfluitp(char name[]);
void gptp(char name[], int size);
void gptrtp(char const ptrname[], char const desname[]);
void gdflptp(char name[]);
void gfunc(char const name[], char const xname[], int have_it);
void gfuncd(char const name[], char const xname[], int have_it);
void gmaxi(char const name[], int lower_bound);
void gmaxii(char const name[], int lower_bound);
void gmaxn(char const name[], int lower_bound);
void gmaxnn(char const name[], int lower_bound);
void gpmaxn(char const name[], int value);
void gpmaxr(char const name[], char const oname[]);
void grename(char const name[], char const oname[]);
void create_options();
void create_limits();
void create_posix();
void create_c();

/* variables
   =========
 */

int bits_per_byte = 0;
/* the number of bits in a byte,
   used for converting C "sizeof" return values
   to Ada 'size values
 */

char error_message[128];
/* a temporary buffer for building error messages
   It is global because we want to be able to return
   a pointer to it from inside a function.
 */

FILE *fp;      /* current output file */

int network_byte_order;
/* 1 means we have network order locally, and so don't
   need reordering
 */

/* declarations for C types
   ------------------------
   If you make any changes here, also make changes
   where Ada types are put out, below. (***)
 */

/* the following are required by the C language standard
 */
void g_size_t(){
 gsitp("size_t", sizeof(size_t));
 gptrtp("size_t", "size_t");
}
void g_time_t(){
 gsitp("time_t", sizeof(time_t));
 gptrtp("time_t", "time_t");
}
void g_clock_t(){
 guitp("clock_t", sizeof(clock_t));
 gptrtp("clock_t", "clock_t");
}

/* no sense configuring to do without the following types,
   since they are too basic
 */
void g_off_t(){gsitp("off_t", sizeof(off_t));}
void g_pid_t(){gsitp("pid_t", sizeof(pid_t));}
void g_gid_t(){guitp("gid_t", sizeof(gid_t));}
void g_uid_t(){guitp("uid_t", sizeof(uid_t));}
void g_mode_t(){guitp("mode_t", sizeof(mode_t));}
void g_ssize_t(){gsitp("ssize_t", sizeof(ssize_t));}

#ifdef HAVE_DIRENT_H
void g_DIR(){DIR * x; gptp("DIR", sizeof(x));}
#else
void g_DIR(){gdflptp("DIR");}
#endif

#ifdef HAVE_ino_t
void g_ino_t(){guitp("ino_t",sizeof(ino_t));}
#else
void g_ino_t(){gdfluitp("ino_t");}
#endif

#ifdef HAVE_dev_t
void g_dev_t(){guitp("dev_t", sizeof(dev_t));}
#else
void g_dev_t(){gdfluitp("dev_t");}
#endif

#ifdef HAVE_nlink_t
void g_nlink_t(){guitp("nlink_t", sizeof(nlink_t));}
#else
void g_nlink_t(){gdfluitp("nlink_t");}
#endif

#ifdef HAVE_cc_t
void g_cc_t(){guitp("cc_t", sizeof(cc_t));}
#else
void g_cc_t(){gdfluitp("cc_t");}
#endif

#ifdef HAVE_tcflag_t
void g_tcflag_t(){guitp("tcflag_t", sizeof(tcflag_t));}
#else
void g_tcflag_t(){gdfluitp("tcflag_t");}
#endif

#ifdef HAVE_clockid_t
void g_clockid_t(){gsitp("clockid_t", sizeof(clockid_t));}
#else
void g_clockid_t(){gdflsitp("clockid_t");}
#endif

#ifdef HAVE_mqd_t
void g_mqd_t(){gsitp("mqd_t", sizeof(mqd_t));}
#else
/* mqd_t must  be signed, since the value -1 is used for error return
 */
void g_mqd_t(){gdflsitp("mqd_t");}
#endif

#ifdef HAVE_fd_set
void g_fd_set(){gptp("fd_set", sizeof(fd_set));}
#else
void g_fd_set(){gdflptp("fd_set");}
#endif

#ifdef HAVE_pthread_attr_t
void g_pthread_attr_t(){gptp("pthread_attr_t", sizeof(pthread_attr_t));}
#else
void g_pthread_attr_t(){gdflptp("pthread_attr_t");}
#endif

#ifdef HAVE_pthread_cond_t
void g_pthread_cond_t(){gptp("pthread_cond_t", sizeof(pthread_cond_t));}
#else
void g_pthread_cond_t(){gdflptp("pthread_cond_t");}
#endif

#ifdef HAVE_pthread_condattr_t
void g_pthread_condattr_t()
  {gptp("pthread_condattr_t", sizeof(pthread_condattr_t));}
#else
void g_pthread_condattr_t(){gdflptp("pthread_condattr_t");}
#endif

#ifdef HAVE_pthread_key_t
void g_pthread_key_t(){gptp("pthread_key_t", sizeof(pthread_key_t));}
#else
void g_pthread_key_t(){gdflptp("pthread_key_t");}
#endif

#ifdef HAVE_pthread_mutex_t
void g_pthread_mutex_t(){gptp("pthread_mutex_t", sizeof(pthread_mutex_t));}
#else
void g_pthread_mutex_t(){gdflptp("pthread_mutex_t");}
#endif

#ifdef HAVE_pthread_mutexattr_t
void g_pthread_mutexattr_t()
  {gptp("pthread_mutexattr_t", sizeof(pthread_mutexattr_t));}
#else
void g_pthread_mutexattr_t(){gdflptp("pthread_mutexattr_t");}
#endif

#ifdef HAVE_pthread_once_t
void g_pthread_once_t(){gptp("pthread_once_t", sizeof(pthread_once_t));}
#else
void g_pthread_once_t(){gdflptp("pthread_once_t");}
#endif

#ifdef HAVE_pthread_t
void g_pthread_t(){gptp("pthread_t", sizeof(pthread_t));}
#else
void g_pthread_t(){gdflptp("pthread_t");}
#endif

#ifdef HAVE_sem_t
void g_sem_t(){gptp("sem_t", sizeof(sem_t));}
#else
void g_sem_t(){gdflptp("sem_t");}
#endif

#ifdef HAVE_sigset_t
void g_sigset_t(){gptp("sigset_t", sizeof(sigset_t));}
#else
void g_sigset_t(){gdflptp("sigset_t");}
#endif

#ifdef HAVE_speed_t
void g_speed_t(){guitp("speed_t", sizeof(speed_t));}
#else
void g_speed_t(){gdfluitp("speed_t");}
#endif

#ifdef HAVE_timer_t
void g_timer_t(){guitp("timer_t", sizeof(timer_t));}
#else
void g_timer_t(){gdfluitp("timer_t");}
#endif

/* sigval must precede siginfo_t and struct sigevent
 */
#ifdef HAVE_sigval
#else
union sigval {
   int sival_int;
   void *sival_ptr;
  };
#endif
void g_sigval(){
  ifprintf(fp,"   type sigval (Dummy : Boolean := True) is record\n");
  ifprintf(fp,"      case Dummy is\n");
  ifprintf(fp,"         when True  => sival_int : int;\n");
  ifprintf(fp,"         when False => sival_ptr : System.Address;\n");
  ifprintf(fp,"      end case;\n");
  ifprintf(fp,"   end record;\n");
  ifprintf(fp,"   pragma Unchecked_Union (sigval);\n");
}

/* siginfo_t must precede sigaction
 */
#ifdef HAVE_siginfo_t
    GT1T(siginfo_t, 1)
#else
typedef struct siginfo {
    int si_signo;
    int si_code;
    union sigval si_value;
  } siginfo_t;
  GT1T(siginfo_t, 0)
#endif
  GT2(si_signo, int)
  GT2(si_code, int)
  GT2(si_value, union sigval)
  GT3

/* sigevent must precede aiocb
 */
#ifdef HAVE_struct_sigevent
  GT1(sigevent, 1)
#else
struct sigevent {
    int sigev_notify;
    int sigev_signo;
    union sigval sigev_value;
/*    void (*)(union sigval) sigev_notify_function; */
    int sigev_notify_function;
/*    (pthread_attr_t *) sigev_notify_attributes; */
    int sigev_notify_attributes;
  };
  GT1(sigevent, 0)
#endif
  GT2(sigev_notify,int)
  GT2(sigev_signo,int)
  GT2(sigev_value,union sigval)
#ifdef HAVE_component_sigev_notify_function
  GT2(sigev_notify_function, void (*)(union sigval))
  GT2(sigev_notify_attributes,pthread_attr_t *)
#endif
  GT3

#ifdef HAVE_struct_aiocb
  GT1(aiocb, 1)
#else
struct aiocb {
    int aio_fildes;
    off_t aio_offset;
    volatile void * aio_buf;
    size_t aio_nbytes;
    int aio_reqprio;
    struct sigevent aio_sigevent;
    int aio_lio_opcode;
  };
  GT1(aiocb, 0)
#endif
  GT2(aio_fildes, int)
  GT2(aio_offset, off_t)
  GT2V(aio_buf, volatile void *)
  GT2(aio_nbytes, size_t)
  GT2(aio_reqprio, int)
  GT2(aio_sigevent, struct sigevent)
  GT2(aio_lio_opcode, int)
  GT3

#ifdef HAVE_struct_dirent
  GT1(dirent, 1)
#else
struct dirent {
    char d_name[1];
  };
  GT1(dirent, 0)
#endif
  GT2A(d_name, POSIX_String (1 .. 1), char, 1)
  GT3

#ifdef HAVE_struct_flock
  GT1(flock, 1)
#else
struct flock {
    short l_type;
    short l_whence;
    off_t l_start;
    off_t l_len;
    pid_t l_pid;
  };
  GT1(flock, 0)
#endif
  GT2(l_type, short)
  GT2(l_whence, short)
  GT2(l_start, off_t)
  GT2(l_len, off_t)
  GT2(l_pid, pid_t)
  GT3

#ifdef HAVE_struct_group
  GT1(group, 1)
#else
struct group {
    char * gr_name;
    gid_t gr_gid;
    char ** gr_mem;
  };
  GT1(group, 0)
#endif
  GT2(gr_name, char *)
  GT2(gr_gid, gid_t)
  GT2(gr_mem, char **)
  GT3

#ifdef HAVE_struct_mq_attr
  GT1(mq_attr, 1)
#else
struct mq_attr {
    long mq_flags;
    long mq_maxmsg;
    long mq_msgsize;
    long mq_curmsgs;
  };
  GT1(mq_attr, 0)
#endif
  GT2(mq_flags, long)
  GT2(mq_maxmsg, long)
  GT2(mq_msgsize, long)
  GT2(mq_curmsgs, long)
  GT3

#ifdef HAVE_struct_passwd
  GT1(passwd, 1)
#else
struct passwd {
    char * pw_name;
    uid_t  pw_uid;
    gid_t  pw_gid;
    char * pw_dir;
    char * pw_shell;
  };
  GT1(passwd, 0)
#endif
  GT2(pw_name, char *)
  GT2(pw_uid, uid_t)
  GT2(pw_gid, gid_t)
  GT2(pw_dir, char *)
  GT2(pw_shell, char *)
  GT3

#if defined(HAVE_struct_sigaction) || defined(HAVE_struct_cma_sigaction)
  GT1(sigaction, 1)
#else
struct sigaction {
    void (*sa_handler)();
    sigset_t sa_mask;
    int sa_flags;
    void (*sa_sigaction)(int,siginfo_t *, void *);
  };
  GT1(sigaction, 0)
#endif
  GT2P(sa_handler, System.Address)
  GT2(sa_mask, sigset_t)
  GT2(sa_flags, int)
#ifdef HAVE_component_sa_sigaction
  GT2P(sa_sigaction, System.Address)
#else
#endif
  GT3

/* ....how to put message if a component, like
   sa_sigaction, if it is missing?  consider putting a
   mark in the component descriptor.
 */

/* ....check P1003.1c to see what is the correct component name
   Provenzano uses "prio".  We had "sched_priority".
 */
#ifdef HAVE_struct_sched_param
  GT1(sched_param, 1)
#else
struct sched_param {
     int sched_priority;
  };
  GT1(sched_param, 0)
#endif
  GT2(sched_priority, int)
  GT3

#ifdef HAVE_struct_stat
  GT1(stat, 1)
#else
struct stat {
    mode_t st_mode;
    ino_t st_ino;
    dev_t st_dev;
    nlink_t st_nlink;
    uid_t st_uid;
    gid_t st_gid;
    off_t st_size;
    time_t st_atime;
    time_t st_mtime;
    time_t st_ctime;
  };
  GT1(stat, 0)
#endif
  GT2(st_mode, mode_t)
  GT2(st_ino, ino_t)
  GT2(st_dev, dev_t)
  GT2(st_nlink, nlink_t)
  GT2(st_uid, uid_t)
  GT2(st_gid, gid_t)
  GT2(st_size, off_t)
  GT2(st_atime, time_t)
  GT2(st_mtime, time_t)
  GT2(st_ctime, time_t)
  GT3

#ifdef HAVE_struct_termios
  GT1(termios, 1)
#else
struct termios {
    tcflag_t c_iflag;
    tcflag_t c_oflag;
    tcflag_t c_cflag;
    tcflag_t c_lflag;
    cc_t c_cc[1];
  };
  GT1(termios, 0)
#endif
  GT2(c_iflag, tcflag_t)
  GT2(c_oflag, tcflag_t)
  GT2(c_cflag, tcflag_t)
  GT2(c_lflag, tcflag_t)
  GT2A(c_cc, cc_t_array, cc_t, NCCS)
  GT3

#ifdef HAVE_suseconds_t
#else
  typedef int suseconds_t;
#endif

#ifdef HAVE_struct_timeval
  GT1(timeval, 1)
#else
struct timeval {
    time_t tv_sec;
    suseconds_t tv_usec;
  };
  GT1(timeval, 0)
#endif
  GT2(tv_sec, time_t)
  GT2(tv_usec, suseconds_t)
  GT3

struct timeval struct_timeval_dummy;
void g_suseconds_t()
 {gsitp("suseconds_t", sizeof(struct_timeval_dummy.tv_usec));}

#ifdef HAVE_struct_iovec
  GT1(iovec, 1)
#else
struct iovec {
    char * iov_base;
    size_t iov_len;
  };
  GT1(iovec, 0)
#endif
  GT2(iov_base, char *)
  GT2(iov_len, size_t)
  GT3

#ifdef HAVE_struct_timespec
  GT1(timespec, 1)
#else
struct timespec {
    time_t tv_sec;
    long   tv_nsec;
  };
  GT1(timespec, 0)
#endif
  GT2(tv_sec,time_t)
  GT2(tv_nsec,long)
  GT3

#ifdef HAVE_struct_itimerspec
  GT1(itimerspec, 1)
#else
struct itimerspec {
    struct timespec it_interval;
    struct timespec it_value;
  };
  GT1(itimerspec, 0)
#endif
  GT2(it_interval, struct timespec)
  GT2(it_value, struct timespec)
  GT3

#ifdef HAVE_struct_tm
  GT1(tm, 1)
#else
struct tm {
    int tm_sec;
    int tm_min;
    int tm_hour;
    int tm_mday;
    int tm_mon;
    int tm_year;
    int tm_wday;
    int tm_yday;
    int tm_isdst;
  };
  GT1(tm, 0)
#endif
  GT2(tm_sec,int)
  GT2(tm_min,int)
  GT2(tm_hour,int)
  GT2(tm_mday,int)
  GT2(tm_mon,int)
  GT2(tm_year,int)
  GT2(tm_wday,int)
  GT2(tm_yday,int)
  GT2(tm_isdst,int)
  GT3

#ifdef HAVE_struct_tms
  GT1(tms, 1)
#else
struct tms {
    clock_t tms_utime;
    clock_t tms_stime;
    clock_t tms_cstime;
    clock_t tms_cutime;
  };
  GT1(tms, 0)
#endif
  GT2(tms_utime, clock_t)
  GT2(tms_stime, clock_t)
  GT2(tms_cutime, clock_t)
  GT2(tms_cstime, clock_t)
  GT3

#ifdef HAVE_struct_utimbuf
  GT1(utimbuf, 1)
#else
struct utimbuf {
    time_t modtime;
    time_t actime;
  };
  GT1(utimbuf, 0)
#endif
  GT2(modtime, time_t)
  GT2(actime, time_t)
  GT3

#ifdef HAVE_struct_utsname
  GT1(utsname, 1)
#else
struct utsname {
    char sysname  [257];
    char nodename [257];
    char release  [257];
    char version  [257];
    char machine  [257];
  };
  GT1(utsname, 0)
#endif
  GT2A(sysname, utsname_sysname_string, char, sizeof(DUMMY.sysname))
  GT2A(nodename, utsname_nodename_string, char, sizeof(DUMMY.nodename))
  GT2A(release, utsname_release_string, char, sizeof(DUMMY.release))
  GT2A(version, utsname_version_string, char, sizeof(DUMMY.version))
  GT2A(machine, utsname_machine_string, char, sizeof(DUMMY.machine))
  GT3

#ifdef HAVE_sa_family_t
#else
  typedef unsigned short sa_family_t;
#endif
#ifdef HAVE_in_port_t
#else
  typedef unsigned short in_port_t;
#endif

/* in_addr_t should be 4 bytes long
   It is likely to be defined as int or long,
   depending on the machine architecture.
   We use char[4] here if the system does not
   define an appropriate type.
 */

#ifdef HAVE_struct_in_addr
#ifdef HAVE_in_addr_t
#else
  typedef char in_addr_t[4];
#endif
  GT1(in_addr, 1)
#else
#ifdef HAVE_in_addr_t
#else
  typedef char in_addr_t[4];
#endif
struct in_addr {
    in_addr_t s_addr;
  };
  GT1(in_addr, 0)
#endif
  GT2(s_addr, in_addr_t)
  GT3

#ifdef HAVE_struct_sockaddr
  GT1(sockaddr, 1)
#else
struct sockaddr {
    sa_family_t sa_family;
    char sa_data [14];
  };
  GT1(sockaddr, 0)
#endif
  GT2(sa_family, sa_family_t)
  GT2A(sa_data, POSIX_String (1 .. 14), char, sizeof (DUMMY.sa_data))
  GT3

#ifdef HAVE_struct_sockaddr_un
  GT1(sockaddr_un, 1)
#else
struct sockaddr_un {
    sa_family_t sun_family;
    char sun_path [100];
  };
  GT1(sockaddr_un, 0)
#endif
  GT2(sun_family, sa_family_t)
  GT2A(sun_path, sun_path_string, char, sizeof (DUMMY.sun_path))
  GT3

#ifdef HAVE_struct_sockaddr_in
  GT1(sockaddr_in, 1)
#else
struct sockaddr_in {
    sa_family_t sin_family;
    in_port_t sin_port;
    struct in_addr sin_addr;
    char sin_zero [8];
  };
  GT1(sockaddr_in, 0)
#endif
  GT2(sin_family, sa_family_t)
  GT2(sin_port, in_port_t)
  GT2(sin_addr, struct in_addr)
  GT2A(sin_zero, POSIX_String (1 .. 8), char, 8)
  GT3

#ifdef HAVE_struct_linger
  GT1(linger, 1)
#else
struct linger {
    int l_onoff;
    int l_linger;
  };
  GT1(linger, 0)
#endif
  GT2(l_onoff, int)
  GT2(l_linger, int)
  GT3

#ifdef HAVE_struct_msghdr
  GT1(msghdr, 1)
#else
struct msghdr {
    struct sockaddr * msg_name;
    size_t msg_namelen;
    struct iovec * msg_iov;
    size_t msg_iovlen;
#ifdef _BSD4_3_
    caddr_t msg_accrights;          /* access rights sent/received */
    int     msg_accrightslen;
#else
    char * msg_control;
    size_t msg_controllen;
    int msg_flags;
#endif
  };
  GT1(msghdr, 0)
#endif
  GT2(msg_name, struct sockaddr *)
  GT2(msg_namelen, size_t)
  GT2(msg_iov, struct iovec *)
  GT2(msg_iovlen, size_t)
#ifdef _BSD4_3_
  GT2(msg_accrights,caddr_t)
  GT2(msg_accrightslen,int)
#else
#ifdef HAVE_component_msg_control
  GT2(msg_control, char *)
#endif
#ifdef HAVE_component_msg_controllen
  GT2(msg_controllen, size_t)
#endif
#ifdef HAVE_component_msg_flags
  GT2(msg_flags, int)
#endif
#endif
  GT3

#ifdef HAVE_struct_cmsghdr
  GT1(cmsghdr, 1)
#else
struct cmsghdr {
    int cmsg_level;
    int cmsg_type;
    size_t cmsg_len;
  };
  GT1(cmsghdr, 0)
#endif
  GT2(cmsg_level, int)
  GT2(cmsg_type, int)
  GT2(cmsg_len, size_t)
  GT3

#ifdef HAVE_struct_ip_opts
   GT1(ip_opts, 1)
#else
struct ip_opts {
    struct in_addr ip_dst;
    char           ip_opts[40];
  };
  GT1(ip_opts, 0)
#endif
  GT2(ip_dst, struct in_addr)
  GT2A(ip_opts, POSIX.Octet_Array (1 .. 40), char, 40)
  GT3

#ifdef HAVE_struct_hostent
  GT1(hostent, 1)
#else
struct hostent {
    char * h_name;
    char ** h_aliases;
    int h_addrtype;
    int h_length;
    char ** h_addr_list;
  };
  GT1(hostent, 0)
#endif
  GT2(h_name, char *)
  GT2(h_aliases, char **)
  GT2(h_addrtype, int)
  GT2(h_length, int)
  GT2(h_addr_list, char **)
  GT3

#ifdef HAVE_struct_netent
  GT1(netent, 1)
#else
struct netent {
  char * n_name;
  char ** n_aliases;
  int n_addrtype;
  unsigned long n_net;
  };
  GT1(netent, 0)
#endif
  GT2(n_name, char *)
  GT2(n_aliases, char **)
  GT2(n_addrtype, int)
  GT2(n_net, in_addr_t)
  /* POSIX 1003.1g/D6.4 says n_net should be unsigned long,
     but it should be 4 bytes and unsigned long might be longer
     than that on some systems.
     Solaris 2.6 uses in_addr_t, which seems safer.
   */
  GT3

#ifdef HAVE_struct_protoent
  GT1(protoent, 1)
#else
struct protoent {
  char * p_name;
  char ** p_aliases;
  int p_proto;
  };
  GT1(protoent, 0)
#endif
  GT2(p_name, char *)
  GT2(p_aliases, char **)
  GT2(p_proto, int)
  GT3

#ifdef HAVE_struct_servent
  GT1(servent, 1)
#else
struct servent {
  char * s_name;
  char ** s_aliases;
  int s_port;
  char * s_proto;
  };
  GT1(servent, 0)
#endif
  GT2(s_name, char *)
  GT2(s_aliases, char **)
  GT2(s_port, int)
  GT2(s_proto, char *)
  GT3

/* BLH : Changed GT1 to GT1R to handle addrinfo pointer */
#ifdef HAVE_struct_addrinfo
  GT1R(addrinfo, 1)
#else
struct addrinfo {
  int ai_flags;
  int ai_family;
  int ai_socktype;
  int ai_protocol;
  size_t ai_addrlen;
  struct sockaddr * ai_addr;
  char * ai_canonname;
  struct addrinfo * ai_next;
  };
  GT1R(addrinfo, 0)
#endif
  GT2(ai_flags, int)
  GT2(ai_family, int)
  GT2(ai_socktype, int)
  GT2(ai_protocol, int)
  GT2(ai_addrlen, size_t)
  GT2(ai_addr, struct sockaddr *)
  GT2(ai_canonname, char *)
  GT2(ai_next, struct addrinfo *)
  GT3

/* XTI structs */

/* netbuf must precede all others
 */
#ifdef HAVE_struct_netbuf
  GT1(netbuf,1)
#else
struct netbuf {
    unsigned int    maxlen;
    unsigned int    len;
    char *          buf;
  };
  GT1(netbuf,0)
#endif
  GT2(maxlen, unsigned int)
  GT2(len, unsigned int)
  GT2(buf, char *)
  GT3

/* t_info structure */

#ifdef HAVE_struct_t_info
  GT1(t_info,1)
#else
struct t_info {
     long   addr;
     long   options;
     long   tsdu;
     long   etsdu;
     long   connect;
     long   discon;
     long   servtype;
     long   flags;
  };
  GT1(t_info,0)
#endif
  GT2(addr, long)
  GT2(options, long)
  GT2(tsdu, long)
  GT2(etsdu, long)
  GT2(connect, long)
  GT2(discon, long)
  GT2(servtype, long)
#ifndef _TLI_ /* not xti compliant but usable */
  GT2(flags, long)
#endif
  GT3

/* t_opthdr structure */
#ifdef HAVE_struct_t_opthdr
  GT1(t_opthdr,1)
#else
struct t_opthdr {
     unsigned long   len;
     unsigned long   level;
     unsigned long   name;
     unsigned long   status;
  };
  GT1(t_opthdr,0)
#endif
  GT2(len, unsigned long)
  GT2(level, unsigned long)
  GT2(name, unsigned long)
  GT2(status, unsigned long)
  GT3

/* t_bind structure */
#ifdef HAVE_struct_t_bind
  GT1(t_bind,1)
#else
struct t_bind {
     struct netbuf   addr;
     unsigned        qlen;
  };
  GT1(t_bind,0)
#endif
  GT2(addr, struct netbuf)
  GT2(qlen, unsigned)
  GT3

/* t_optmgmt structure */
#ifdef HAVE_struct_t_optmgmt
  GT1(t_optmgmt,1)
#else
struct t_optmgmt {
     struct netbuf   opt;
     long            flags;
  };
  GT1(t_optmgmt,0)
#endif
  GT2(opt, struct netbuf)
  GT2(flags, long)
  GT3

/* t_discon structure */
#ifdef HAVE_struct_t_discon
  GT1(t_discon,1)
#else
struct t_discon {
     struct netbuf   udata;
     int             reason;
     int             sequence;
  };
  GT1(t_discon,0)
#endif
  GT2(udata, struct netbuf)
  GT2(reason, int)
  GT2(sequence, int)
  GT3

/* t_call structure */
#ifdef HAVE_struct_t_call
  GT1(t_call,1)
#else
struct t_call {
     struct netbuf   addr;
     struct netbuf   opt;
     struct netbuf   udata;
     int             sequence;
  };
  GT1(t_call, 0)
#endif
  GT2(addr, struct netbuf)
  GT2(opt, struct netbuf)
  GT2(udata, struct netbuf)
  GT2(sequence, int)
  GT3

/* t_unitdata structure */
#ifdef HAVE_struct_t_unitdata
  GT1(t_unitdata,1)
#else
struct t_unitdata {
     struct netbuf   addr;
     struct netbuf   opt;
     struct netbuf   udata;
  };
  GT1(t_unitdata,0)
#endif
  GT2(addr, struct netbuf)
  GT2(opt, struct netbuf)
  GT2(udata, struct netbuf)
  GT3

/* t_uderr structure */
#ifdef HAVE_struct_t_uderr
  GT1(t_uderr,1)
#else
struct t_uderr {
     struct netbuf   addr;
     struct netbuf   opt;
     long            error;
  };
  GT1(t_uderr,0)
#endif
  GT2(addr, struct netbuf)
  GT2(opt, struct netbuf)
  GT2(error, long)
  GT3

/* Sturcture used with linger option */
#ifdef HAVE_struct_t_linger
  GT1(t_linger,1)
#else
struct t_linger {
     long            l_onoff;
     long            l_linger;
  };
  GT1(t_linger,0)
#endif
  GT2(l_onoff, long)
  GT2(l_linger, long)
  GT3

/* t_iovec structure */
#ifdef HAVE_struct_t_iovec
  GT1(t_iovec,1)
#else
struct t_iovec {
     char *          iov_base;
     unsigned int    iov_len;
  };
  GT1(t_iovec,0)
#endif
  GT2(iov_base, char *)
  GT2(iov_len, unsigned int)
  GT3

/* t_kpalive structure */
#ifdef HAVE_struct_t_kpalive
  GT1(t_kpalive,1)
#else
struct t_kpalive {
     long  kp_onoff;
     long  kp_timeout;
  };
  GT1(t_kpalive,0)
#endif
  GT2(kp_onoff, long)
  GT2(kp_timeout, long)
  GT3

/*
 Poll/Select
 */
/* pollfd structure */
#ifdef HAVE_struct_pollfd
  GT1(pollfd,1)
#else
struct pollfd {
    int   fd;
    short events;
    short revents;
  };
  GT1(pollfd,0)
#endif
  GT2(fd, int)
  GT2(events, short)
  GT2(revents, short)
  GT3

/* fd_set type
   -----------
   POSIX.1G does not require fd_set to be a (visible) struct
 */
#ifdef HAVE_fd_set
#else
typedef struct fd_set {
     unsigned int fds_bits[32];
  } fd_set;
#endif

/* warn
   ----
 */
void warn(const char msg1[], const char msg2[]) {
   fprintf(stderr,"%s: %s\n",msg1,msg2);
}

/* quit
   ----
 */
void quit(const char msg1[], const char msg2[]) {
   warn(msg1, msg2);
   exit(-1);
}

/* save_type
   ---------
   make a permanent copy of the information about a type with
   components, linked into a list on all_type_list,
   and return a pointer to the new node.
 */
void save_type
  (char const name[],
   int typesize,
   int typekind,
   component_t const *comps) {

  type_t *tmp;
  int count;
  component_t const *p;

  tmp = all_type_list;
  for (tmp=all_type_list;
       tmp && strcmp(tmp->typename,name);
       tmp=tmp->next);
  if (tmp) quit("DUPLICATE TYPE DEFINITION",name);

  tmp = malloc(sizeof(type_t));
  tmp->typekind = typekind;
  tmp->is_printed = 0;
  count = strlen(name);
  tmp->typename = malloc(count+1);
  memcpy(tmp->typename,name,count+1);
  tmp->typesize = typesize;
  count = 0;
  if (comps) {
    for (p=comps;p->compname;p++) count++;
    tmp->comps = malloc((count+1)*sizeof(component_t));
    memcpy(tmp->comps,comps,(count+1)*sizeof(component_t));
  }
  tmp->next = NULL;
  *all_type_tail = tmp;
  all_type_tail = &tmp->next;
}

/* print_type_declaration
   ----------------------
   print out Ada type declaration for C type
   with specified name
 */
void print_type_declaration(char const name[], FILE *fp) {

  type_t * type;
  component_t const * p;
  char extended_name[128];

  type = all_type_list;
  for (type=all_type_list;
       type && strcmp(type->typename,name);
       type=type->next);
  if (type == NULL) {
    NON_SUPPORT_MESSAGE(name);
    return;
  }

  if (type->is_printed) ("TYPE ALREADY DECLARED",name);

  if (type->typekind == STRUCT_TYPE || 
      type->typekind == RECURSIVE_STRUCT_TYPE) {
    if (strlen(type->typename)>=sizeof(extended_name)) {
       quit("type name too long",type->typename);
    }
    strcpy(extended_name,"struct_");
    strcat(extended_name,type->typename);
  } else {
    if (strlen(type->typename)>=sizeof(extended_name)) {
       quit("type name too long",type->typename);
    }
    strcpy(extended_name,type->typename);
  }
  switch (type->typekind) {
  case SIGNED_INTEGER_TYPE:
    ifprintf(fp,"   type %s is range -2**%d .. (2**%d)-1;\n",
      type->typename,
      type->typesize*bits_per_byte-1,
      type->typesize*bits_per_byte-1);
    ifprintf(fp,"   for %s'Size use %d;\n",
      extended_name, type->typesize*bits_per_byte);
    break;
  case UNSIGNED_INTEGER_TYPE:
    ifprintf(fp,"   type %s is mod 2**%d;\n",
      type->typename,
      type->typesize*bits_per_byte);
    ifprintf(fp,"   for %s'Size use %d;\n",
      extended_name, type->typesize*bits_per_byte);
    break;
  case STRUCT_TYPE:
  case TYPEDEF_STRUCT_TYPE:
  { component_t * p;
    int prev_offset = -1;
    ifprintf(fp,"   type %s is record\n", extended_name);
    for (p = type->comps; p && p->typename; p++) {
      ifprintf(fp,"      %s : ", p->compname);
      print_ada_type(p->typename);
      fprintf(fp,";\n");
      if (p->is_volatile) ifprintf(fp,"      pragma Volatile (%s);\n",
         p->compname);
    }
    ifprintf(fp,"   end record;\n");
    ifprintf(fp,"   for %s use record\n", extended_name);
    for (p = type->comps; p && p->compname; p++) {
      /* GNAT isn't able to handle overlapping components, so we add a simple
         minded test to prevent the most common cases */
      if (p->offset == prev_offset)
        ifprintf(fp,"      --  *** OVERLAPPING component ***\n"
                   "      --  %s at %d range 0 .. %d;\n", p->compname,
                   p->offset, p->size*bits_per_byte-1);
      else
        ifprintf(fp,"      %s at %d range 0 .. %d;\n", p->compname,
          p->offset, p->size*bits_per_byte-1);
      prev_offset = p->offset;
    }
    ifprintf(fp,"   end record;\n");
    ifprintf(fp,"   pragma Convention (C_Pass_By_Copy, %s);\n", extended_name);
    ifprintf(fp,"   for %s'Alignment use ALIGNMENT;\n", extended_name);
    ifprintf(fp,"   pragma Warnings (Off);\n");
    ifprintf(fp,"   --  There may be holes in the record, due to\n");
    ifprintf(fp,"   --  components not defined by POSIX standard.\n");
    ifprintf(fp,"   for %s'Size use %d;\n",
      extended_name, type->typesize*bits_per_byte);
    ifprintf(fp,"   pragma Warnings (On);\n");
    gptrtp(type->typename, extended_name);
    break;
  }
  case RECURSIVE_STRUCT_TYPE:  /* BLH */
  { component_t * p;
    int prev_offset = -1;
    ifprintf(fp,"   type %s;\n", extended_name);
    gptrtp(type->typename, extended_name);
    ifprintf(fp,"   type %s is record\n", extended_name);
    for (p = type->comps; p && p->typename; p++) {
      ifprintf(fp,"      %s : ", p->compname);
      print_ada_type(p->typename);
      fprintf(fp,";\n");
      if (p->is_volatile) ifprintf(fp,"      pragma Volatile (%s);\n",
         p->compname);
    }
    ifprintf(fp,"   end record;\n");
    ifprintf(fp,"   for %s use record\n", extended_name);
    for (p = type->comps; p && p->compname; p++) {
      /* GNAT isn't able to handle overlapping components, so we add a simple
         minded test to prevent the most common cases */
      if (p->offset == prev_offset)
        ifprintf(fp,"      --  *** OVERLAPPING component ***\n"
                   "      --  %s at %d range 0 .. %d;\n", p->compname,
                   p->offset, p->size*bits_per_byte-1);
      else
        ifprintf(fp,"      %s at %d range 0 .. %d;\n", p->compname,
          p->offset, p->size*bits_per_byte-1);
      prev_offset = p->offset;
    }
    ifprintf(fp,"   end record;\n");
    ifprintf(fp,"   pragma Convention (C_Pass_By_Copy, %s);\n", extended_name);
    ifprintf(fp,"   for %s'Alignment use ALIGNMENT;\n", extended_name);
    ifprintf(fp,"   pragma Warnings (Off);\n");
    ifprintf(fp,"   --  There may be holes in the record, due to\n");
    ifprintf(fp,"   --  components not defined by POSIX standard.\n");
    ifprintf(fp,"   for %s'Size use %d;\n",
      extended_name, type->typesize*bits_per_byte);
    ifprintf(fp,"   pragma Warnings (On);\n");
    break;
  }
  case CHAR_ARRAY_TYPE:
    ifprintf(fp,"   type %s is\n", extended_name);
    ifprintf(fp,"     array (1 .. %d) of char;\n", type->typesize);
    ifprintf(fp,"   for %s'Alignment use ALIGNMENT;\n", type->typename);
    gptrtp(type->typename,extended_name);
    break;
  case OPAQUE_TYPE:
    ifprintf(fp,"   type %s is\n", extended_name);
    ifprintf(fp,"     array (1 .. %d) of int;\n", wordsize(type->typesize));
    ifprintf(fp,"   for %s'Alignment use ALIGNMENT;\n", type->typename);
    ifprintf(fp,"   for %s'Size use %d;\n",
      extended_name, type->typesize*bits_per_byte);
    gptrtp(type->typename,extended_name);
    break;
  default:
    break;
  }
}

#define IEEE_Header 1
#define FSU_Header  2
#define LMCo_Header 3

/* gheader
   -------
   generate standard copyright header for automatically generated
   POSIX Ada binding package specification with name *pkgname
 */
void gheader(const char pkgname[], int header_kind) {
  int i;
  int namelen = strlen(pkgname);

  fprintf(fp,"--  DO NOT EDIT THIS FILE.\n");
  fprintf(fp,"--  It is generated automatically, by program c-posix.c");
  fprintf(fp,"\n");
  fprintf(fp,"----------------------------------------");
  fprintf(fp,"--------------------------------------\n");
  fprintf(fp,"--                                      ");
  fprintf(fp,"                                    --\n");
  if ((header_kind == FSU_Header || header_kind == LMCo_Header)) {
  fprintf(fp,"--            FLORIST (FSU Implementatio");
  fprintf(fp,"n of POSIX.5) COMPONENTS            --\n");
  } else {
  fprintf(fp,"--   POSIX Ada95 Bindings for Protocol I");
  fprintf(fp,"ndependent Interfaces (P1003.5c)    --\n");
  }
  fprintf(fp,"--                                      ");
  fprintf(fp,"                                    --\n");
  fprintf(fp,"--");
  for (i = 1; i<38-namelen; i++) fprintf(fp," ");
  for (i = 0; i<namelen; i++) fprintf(fp," %c", toupper (pkgname[i]));
  for (i = 1; i<38-namelen; i++) fprintf(fp," ");
  fprintf(fp,"--\n");
  fprintf(fp,"--                                      ");
  fprintf(fp,"                                    --\n");
  fprintf(fp,"--                                   S p");
  fprintf(fp," e c                                --\n");
  fprintf(fp,"--                                      ");
  fprintf(fp,"                                    --\n");
  fprintf(fp,"--                                      ");
  fprintf(fp,"                                    --\n");
  if (header_kind == FSU_Header) {
    fprintf(fp,"--  Copyright (c) 1996, 1997   Florida S");
    fprintf(fp,"tate University (FSU),              --\n");
    fprintf(fp,"--  All Rights Reserved.                ");
    fprintf(fp,"                                    --\n");
  } else if (header_kind == LMCo_Header) {
    fprintf(fp,"--  Copyright (c) 1997 Lockheed Martin\n");
    fprintf(fp," Corporation, All Rights Reserved.  --\n");
    fprintf(fp,"--  Modifications copyright(c) 1997 Flor");
    fprintf(fp,"ida State University (FSU),         --\n");
    fprintf(fp,"--  All Rights Reserved.                ");
    fprintf(fp,"                                    --\n");
  }
  fprintf(fp,"--                                      ");
  fprintf(fp,"                                    --\n");
  if ((header_kind == FSU_Header) ||
      (header_kind == IEEE_Header)) {
    fprintf(fp,"--  This  file  is a component of  FLORI");
    fprintf(fp,"ST,  an  Ada  application  program  --\n");
    fprintf(fp,"--  interface for operating system servi");
    fprintf(fp,"ces for use with the GNAT compiler  --\n");
    fprintf(fp,"--  and the  Gnu  Ada  Runtime  Library ");
    fprintf(fp,"(GNARL).   FLORIST  is intended to  --\n");
    fprintf(fp,"--  conform to the IEEE POSIX Ada standa");
    fprintf(fp,"rds, 1003.5-1992 and 1003.5b-1993.  --\n");
    fprintf(fp,"--  It also includes support for Draft 1");
    fprintf(fp," of IEEE Project 1003.5c.           --\n");
  } else {
    fprintf(fp,"--  This file is part of an implementati");
    fprintf(fp,"on of an Ada95 API for the sockets  --\n");
    fprintf(fp,"--  and network support services found i");
    fprintf(fp,"n P1003.1g -- Protocol Independent  --\n");
    fprintf(fp,"--  Interfaces.  It is integrated with t");
    fprintf(fp,"he  FSU Implementation of POSIX.5b  --\n");
    fprintf(fp,"--  (FLORIST), an Ada API for  POSIX OS ");
    fprintf(fp,"services for use with the GNAT Ada  --\n");
    fprintf(fp,"--  compiler and the FSU Gnu Ada Runtime");
    fprintf(fp," Library (GNARL). The interface is  --\n");
    fprintf(fp,"--  intended to be close to those specif");
    fprintf(fp,"ied in IEEE STD 1003.5: 1990, IEEE  --\n");
    fprintf(fp,"--  STD 1003.5b: 1996, and IEEE Draft ST");
    fprintf(fp,"D 1003.5c: 1997.                    --\n");
  }
  fprintf(fp,"--                                      ");
  fprintf(fp,"                                    --\n");
  if (header_kind == IEEE_Header) {
    fprintf(fp,"--  This package specification contains ");
    fprintf(fp,"some text extracted from  IEEE STD  --\n");
    fprintf(fp,"--  1003.5: 1990, Information Technology");
    fprintf(fp," -- POSIX Ada Language  Interfaces  --\n");
    fprintf(fp,"--  Part 1: Binding  for  System Applica");
    fprintf(fp,"tion Program Interface, as amended  --\n");
    fprintf(fp,"--  by IEEE STD 1003.5b: 1996, Amendment");
    fprintf(fp," 1: Realtime Extensions, copyright  --\n");
    fprintf(fp,"--  1996 by the Institute of Electrical ");
    fprintf(fp,"and Electronics Engineers, Inc.     --\n");
    fprintf(fp,"--                                      ");
    fprintf(fp,"                                    --\n");
    fprintf(fp,"--  The package specifications in the IE");
    fprintf(fp,"EE standards cited above represent  --\n");
    fprintf(fp,"--  only a  portion  of  the  documents ");
    fprintf(fp," and  are  not to be interpreteted  --\n");
    fprintf(fp,"--  outside the context  of  the documen");
    fprintf(fp,"ts.  The standards must be used in  --\n");
    fprintf(fp,"--  conjunction  with  the  package   sp");
    fprintf(fp,"ecifications  in  order  to  claim  --\n");
    fprintf(fp,"--  conformance.   The IEEE takes no res");
    fprintf(fp,"ponsibility for and will assume no  --\n");
    fprintf(fp,"--  liability for damages resulting from");
    fprintf(fp," the reader's misinterpretation of  --\n");
    fprintf(fp,"--  said  information resulting from its");
    fprintf(fp," out-of-context nature.   To order  --\n");
    fprintf(fp,"--  copies of the IEEE standards,  pleas");
    fprintf(fp,"e contact the  IEEE Service Center  --\n");
    fprintf(fp,"--  at 445 Hoes Lane, PO Box 1331, Pisca");
    fprintf(fp,"taway, NJ 08855-1331; via phone at  --\n");
    fprintf(fp,"--  1-800-678-IEEE, 908-981-1393; or via");
    fprintf(fp," fax at 908-981-9667.               --\n");
    fprintf(fp,"--                                      ");
    fprintf(fp,"                                    --\n");
    fprintf(fp,"--  These  package  specifications are  ");
    fprintf(fp,"distributed in  the hope that they  --\n");
    fprintf(fp,"--  will  be useful, but  WITHOUT  ANY  ");
    fprintf(fp,"WARRANTY; without even the implied  --\n");
    fprintf(fp,"--  warranty of MERCHANTABILITY or FITNE");
    fprintf(fp,"SS FOR A PARTICULAR PURPOSE.        --\n");
  } else {
    fprintf(fp,"--  This is free software;  you can redi");
    fprintf(fp,"stribute it and/or modify it under  --\n");
    fprintf(fp,"--  terms of the  GNU  General  Public  ");
    fprintf(fp,"License as  published by the  Free  --\n");
    fprintf(fp,"--  Software Foundation;  either version");
    fprintf(fp,"  2, or (at your option) any later  --\n");
    fprintf(fp,"--  version.  This software is distribut");
    fprintf(fp,"ed  in  the hope  that  it will be  --\n");
    fprintf(fp,"--  useful, but WITHOUT ANY WARRANTY;  w");
    fprintf(fp,"ithout  even the implied  warranty  --\n");
    fprintf(fp,"--  of MERCHANTABILITY or FITNESS FOR A ");
    fprintf(fp,"PARTICULAR  PURPOSE.  See  the GNU  --\n");
    fprintf(fp,"--  General Public License for more deta");
    fprintf(fp,"ils.  You  should have  received a  --\n");
    fprintf(fp,"--  copy of the GNU General Public Licen");
    fprintf(fp,"se  distributed  with  GNARL;  see  --\n");
    fprintf(fp,"--  file  COPYING.  If not,  write to  t");
    fprintf(fp,"he  Free  Software  Foundation, 59  --\n");
    fprintf(fp,"--  Temple Place - Suite 330, Boston, MA");
    fprintf(fp," 02111-1307, USA.                   --\n");
    fprintf(fp,"--                                      ");
    fprintf(fp,"                                    --\n");
    fprintf(fp,"--  As a special exception, if other fil");
    fprintf(fp,"es instantiate generics from  this  --\n");
    fprintf(fp,"--  unit, or you link this unit with oth");
    fprintf(fp,"er files to produce an  executable, --\n");
    fprintf(fp,"--  this  unit does not by itself cause ");
    fprintf(fp,"the  resulting  executable  to  be  --\n");
    fprintf(fp,"--  covered  by the  GNU  General  Publi");
    fprintf(fp,"c License. This exception does not  --\n");
    fprintf(fp,"--  however invalidate any other  reason");
    fprintf(fp,"s why the executable file might be  --\n");
    fprintf(fp,"--  covered by the GNU Public License.  ");
    fprintf(fp,"                                    --\n");
  }
  fprintf(fp,"--                                      ");
  fprintf(fp,"                                    --\n");
  fprintf(fp,"----------------------------------------");
  fprintf(fp,"--------------------------------------\n");
}

/* print_ada_type
   --------------
   print out short name of Ada type for given C type
 */
void print_ada_type (char const typename[]) {
  if (!strcmp (typename, "char *")) fprintf(fp,"char_ptr");
  else if (!strcmp (typename, "char **")) fprintf(fp,"char_ptr_ptr");
  else if (!strcmp (typename, "char")) fprintf(fp,"POSIX_Character");
  else if (!strcmp (typename, "int")) fprintf(fp,"int");
  else if (!strcmp (typename, "short")) fprintf(fp,"short");
  else if (!strcmp (typename, "long")) fprintf(fp,"long");
  else if (!strcmp (typename, "signed char")) fprintf(fp,"unsigned_char");
  else if (!strcmp (typename, "unsigned")) fprintf(fp,"unsigned");
  else if (!strcmp (typename, "unsigned short")) fprintf(fp,"unsigned_short");
  else if (!strcmp (typename, "unsigned int")) fprintf(fp,"unsigned_int");
  else if (!strcmp (typename, "unsigned long")) fprintf(fp,"unsigned_long");
  else if (!strcmp (typename, "unsigned char")) fprintf(fp,"unsigned_char");
  else if (!strcmp (typename, "size_t")) fprintf(fp,"size_t");
  else if (!strcmp (typename, "struct in_addr"))
    fprintf(fp,"struct_in_addr");
  else if (!strcmp (typename, "struct sigevent"))
    fprintf(fp,"struct_sigevent");
  else if (!strcmp (typename, "struct sockaddr *"))
    fprintf(fp,"sockaddr_ptr");
  else if (!strcmp (typename, "struct iovec *")) fprintf(fp,"iovec_ptr");
  else if (!strcmp (typename, "struct addrinfo *"))
    fprintf(fp,"addrinfo_ptr");
  else if (!strcmp (typename, "struct timespec"))
    fprintf(fp,"struct_timespec");
  else if (!strcmp (typename, "struct netbuf")) fprintf(fp,"struct_netbuf");
  else if (!strcmp (typename, "union sigval")) fprintf(fp,"sigval");
  else if (!strcmp (typename, "pthread_attr_t *"))
    fprintf(fp,"System.Address");
  else if (!strcmp (typename, "void (*)()")) fprintf(fp,"System.Address");
  else if (!strcmp (typename, "volatile void *")) fprintf(fp,"System.Address");
  else if (!strcmp (typename, "fd_mask")) {
    fprintf(fp,"fd_mask_array (1 .. %d)",
              sizeof(fd_set)/sizeof(unsigned int));
  } else if (!strncmp (typename, "void (*)(", 9) &&
           typename [strlen (typename) - 1] == ')')
   /* translate "void (*)(...)" into System.Address */
   fprintf(fp,"System.Address");
  else fprintf(fp,"%s", typename);
}

/* wordsize
   --------
   the number of locations of type int required to
   hold a value of a type of size n (in bytes)
 */
int wordsize (int n) {
  return (n % sizeof (unsigned int)) == 0 ?
    n / sizeof (int) :
    n / sizeof (int) + 1;
}

/* ghdrcmnt
   --------
   generate section header comment
 */
void ghdrcmnt(char name[]) {
  int len = strlen(name);
  int i;
  fprintf(fp,"\n");
  ifprintf(fp,"   ");
  for (i=0; i<len+8; i++) fprintf(fp,"%s","-");
  fprintf(fp,"\n");
  ifprintf(fp,"   --  %s  --\n", name);
  ifprintf(fp,"   ");
  for (i=0; i<len+8; i++) fprintf(fp,"%s","-");
  fprintf(fp,"\n\n");
}

/* gcmnt
   --------
   generate comment
 */
void gcmnt(char name[]) {
  fprintf(fp,"\n");
  ifprintf(fp,"   --  %s  --\n", name);
}

/* gbrg
   ----
   generate declaration of Boolean subtype with
   range lb .. ub
 */
void gbrg(char name[], char lb[], char ub[]) {
  ifprintf(fp,"   subtype %s is Boolean range\n",name);
  ifprintf(fp,"      %s .. %s;\n", lb, ub);
}

/* gptrtp
   -----
   generate variable and constant-pointer types
 */
void gptrtp(char const ptrname[], char const desname[]) {
  ifprintf(fp,"   type %s_ptr is access constant %s;\n", ptrname, desname);
  ifprintf(fp,"   pragma Convention (C, %s_ptr);\n", ptrname);
  ifprintf(fp,"   type %s_var_ptr is access all %s;\n", ptrname, desname);
  ifprintf(fp,"   pragma Convention (C, %s_var_ptr);\n", ptrname);
}

/* gsitp
   -----
   generate declaration of signed integer type
   with specified name and size (in bytes)
 */
void gsitp(char name[], int size) {
  save_type(name, size, SIGNED_INTEGER_TYPE, NULL);
  print_type_declaration(name,fp);
}

/* gdflsitp
   --------
   generate declaration of default integer type
   with specified name
   for C type not supported by the underlying OS
 */
void gdflsitp(char name[]) {
  NON_SUPPORT_MESSAGE(name)
  gsitp(name, DEFAULTSIZE/bits_per_byte);
}

/* guitp
   -----
   generate declaration of unsigned integer type
   with specified name and size (in bytes)
 */
void guitp(char name[], int size) {
  save_type(name, size, UNSIGNED_INTEGER_TYPE, NULL);
  print_type_declaration(name,fp);
}

/* gdfluitp
   --------
   generate declaration of default integer type
   with specified name
   for C type not supported by the underlying OS
 */
void gdfluitp(char name[]) {
  NON_SUPPORT_MESSAGE(name)
  guitp(name, DEFAULTSIZE/bits_per_byte);
}

/* gptp
   ----
   generate declaration of opaque (private type completion)
   for C type with specified name and size (in bytes)
 */
void gptp(char name[], int size) {
  if (size >= sizeof (int)) 
  {
    save_type(name, size, OPAQUE_TYPE, NULL);
  }
  else
  {
    save_type(name, size, CHAR_ARRAY_TYPE, NULL);
  }
  print_type_declaration(name,fp);
}

/* gdflptp
   -------
   generate default completion of private type declaration
   for C type not supported by the underlying OS
 */
void gdflptp(char name[])  {
  NON_SUPPORT_MESSAGE(name)
  gptp(name, DEFAULTSIZE/bits_per_byte);
}

/* GFUNC
   ------
   macro wrapper for function gfunc, for cases where
   we are concerned that the function may have been renamed
   via a macro.
   This came up with DEC UNIX, for the pthread_-names.
   It also came up with HP-UX, for the pthread_-names.
   The extra layer of macro is to force expansion of name2
   if it is a macro, even when we later quote it.
 */
#define GFUNC(name, have_it) GFUNCB(#name, name, have_it)
#define GFUNCB(qname, qxname, have_it) gfunc(qname, #qxname, have_it)

/* gfunc
   -----
   generate constant for C name of function
   that notfies of errors by returning -1 with an error code in errno
   This lets us raise an exception for functions
   that are not available in the libraries.
 */

void gfunc(char const name[], char const xname[], int have_it) {
  if (have_it) {
    ifprintf
      (fp,"   HAVE_%s : constant Boolean := True;\n", name);
    if (strlen(name) > 20) {
      ifprintf(fp,"   %s_LINKNAME : constant String :=\n", name);
      ifprintf(fp,"       \"%s\";\n", name);
    } else ifprintf
      (fp,"   %s_LINKNAME : constant String := \"%s\";\n", name, name);
   } else {
#ifdef TRY_MACRO_LINKNAMES
     if (strcmp(name, xname)) {
       /* We have a macro masquerading as a function name.
          If this code results in problems, #undef TRY_MACRO_LINKNAMES
          and recompile c-posix.c.
          These functions will then simply be treated as not available.
        */
      ifprintf(fp,"   --  We guessed %s is implemented as a macro that\n",
        name);
      ifprintf(fp,"   --  expands to the real function name."
       "  This is risky...\n");
      if (strlen(name) > 20) {
        ifprintf(fp,"   %s_LINKNAME : constant String :=\n", name);
        ifprintf(fp,"      \"%s\";\n", xname);
      } else ifprintf
        (fp,"   %s_LINKNAME : constant String := \"%s\";\n", name, xname);
    } else
#endif
    {
    ifprintf(fp,"   --  *** MISSING: function %s ***  --\n", name);
    warn("missing function ", name);
    ifprintf
      (fp,"   HAVE_%s : constant Boolean := False;\n", name);
    ifprintf
      (fp,"   %s_LINKNAME : constant String := \"nosys_neg_one\";\n", name);
    }
  }
}

/* gfuncsol
   --------
   variant of gfunc to work around feature of Solaris header files,
   which implement some POSIX functions by locally defined wrappers
   that call a real function whose name has the form __posix_XXX.
 */

void gfuncsol(char const name[], char const xname[]) {
  ifprintf(fp,"   --  We guessed %s is implemented as a wrapper\n", name);
  ifprintf(fp,"   --  that calls the real function.  This is risky...\n");
  ifprintf
    (fp,"   HAVE_%s : constant Boolean := True;\n", name);
  if (strlen(name) > 20) {
    ifprintf(fp,"   %s_LINKNAME : constant String :=\n", name);
    ifprintf(fp,"       \"%s\";\n", xname);
  } else ifprintf
    (fp,"   %s_LINKNAME : constant String := \"%s\";\n", name, xname);
}

/* GFUNCNS
   ------_
   like GFUNC, but generates ENOTSUP instead of ENOSYS if the
   function is  not supported.
 */
#define GFUNCNS(name, have_it) GFUNCNSB(#name, name, have_it)
#define GFUNCNSB(qname, qxname, have_it) gfuncns(qname, #qxname, have_it)

/* gfuncns
   -------
   like gfunc, except uses ENOTSUP instead of ENOSYS if not supported
 */

void gfuncns(char const name[], char const xname[], int have_it) {
  if (have_it) {
    ifprintf
      (fp,"   HAVE_%s : constant Boolean := True;\n", name);
    if (strlen(name) > 20) {
      ifprintf(fp,"   %s_LINKNAME : constant String :=\n", name);
      ifprintf(fp,"       \"%s\";\n", name);
    } else ifprintf
      (fp,"   %s_LINKNAME : constant String := \"%s\";\n", name, name);
   } else {
#ifdef TRY_MACRO_LINKNAMES
     if (strcmp(name, xname)) {
       /* We have a macro masquerading as a function name.
          If this code results in problems, #undef TRY_MACRO_LINKNAMES
          and recompile c-posix.c.
          These functions will then simply be treated as not available.
        */
      ifprintf(fp,"   --  We guessed %s is implemented as a macro that\n",
        name);
      ifprintf(fp,"   --  expands to the real function name."
       "  This is risky...\n");
      if (strlen(name) > 20) {
        ifprintf(fp,"   %s_LINKNAME : constant String :=\n", name);
        ifprintf(fp,"      \"%s\";\n", xname);
      } else ifprintf
        (fp,"   %s_LINKNAME : constant String := \"%s\";\n", name, xname);
    } else
#endif
    {
    ifprintf(fp,"   --  *** MISSING: function %s ***  --\n", name);
    warn("missing function ", name);
    ifprintf
      (fp,"   HAVE_%s : constant Boolean := False;\n", name);
    ifprintf
      (fp,"   %s_LINKNAME : constant String := \"notsup_neg_one\";\n", name);
    }
  }
}

/* GFUNCD
   ------
   macro wrapper for function gfuncd, for cases where
   we are concerned that the function may have been renamed
   via a macro.
   This came up with DEC UNIX, for the pthread_-names.
   The extra layer of macro is to force expansion of name2
   if it is a macro, even when we later quote it.
 */
#define GFUNCD(name, have_it) GFUNCDB(#name, name, have_it)
#define GFUNCDB(qname, qxname, have_it) gfuncd(qname, #qxname, have_it)

/* gfuncd
   ------
   same as gfunc, except for function that notifies of
   errors by returning an error code directly.

 */
void gfuncd(char const name[], char const xname[], int have_it) {
  if (have_it) {
    if (strlen(name) > 20) {
      ifprintf(fp,"   %s_LINKNAME : constant String :=\n", name);
      ifprintf(fp,"      \"%s\";\n", name);
    } else ifprintf
      (fp,"   %s_LINKNAME : constant String := \"%s\";\n", name, name);
  } else {
#ifdef TRY_MACRO_LINKNAMES
    if (strcmp(name, xname)) {
       /* We have a macro masquerading as a function name.
          If this results in problems, #undef TRY_MACRO_LINKNAMES
          and recompile c-posix.c.  These functions will then simply
          be treated as not available.
        */
      ifprintf(fp,"   --  Apparently this function is actually a macro.\n");
      ifprintf(fp,"   --  We are guessing that"
        " the macro expands to a linkable name.\n");
      ifprintf(fp,"   --  This is risky...\n");
      if (strlen(name) > 20) {
        ifprintf(fp,"   %s_LINKNAME : constant String :=\n", name);
        ifprintf(fp,"      \"%s\";\n", xname);
      } else ifprintf
        (fp,"   %s_LINKNAME : constant String := \"%s\";\n", name, xname);
    } else
#endif
   {
      ifprintf(fp,"   --  *** MISSING: function %s ***  --\n", name);
      warn("missing function ", name);
      if (strlen(name) > 20) {
        ifprintf(fp,"   %s_LINKNAME : constant String :=\n", name);
        ifprintf(fp,"      \"nosys_direct\";\n", name);
      } else ifprintf
        (fp,"   %s_LINKNAME : constant String := \"nosys_direct\";\n", name);
    }
  }
}

/* GFUNCDNS
   --------
   like GFUNCD, except that error code is ENOTSUP if not supported
 */
#define GFUNCDNS(name, have_it) GFUNCDNSB(#name, name, have_it)
#define GFUNCDNSB(qname, qxname, have_it) gfuncdns(qname, #qxname, have_it)

/* gfuncdns
   --------
   same as gfuncd, except that error code for nonsupport is ENOTSUP
 */
void gfuncdns(char const name[], char const xname[], int have_it) {
  if (have_it) {
    if (strlen(name) > 20) {
      ifprintf(fp,"   %s_LINKNAME : constant String :=\n", name);
      ifprintf(fp,"      \"%s\";\n", name);
    } else ifprintf
      (fp,"   %s_LINKNAME : constant String := \"%s\";\n", name, name);
  } else {
#ifdef TRY_MACRO_LINKNAMES
    if (strcmp(name, xname)) {
       /* We have a macro masquerading as a function name.
          If this results in problems, #undef TRY_MACRO_LINKNAMES
          and recompile c-posix.c.  These functions will then simply
          be treated as not available.
        */
      ifprintf(fp,"   --  Apparently this function is actually a macro.\n");
      ifprintf(fp,"   --  We are guessing that"
        " the macro expands to a linkable name.\n");
      ifprintf(fp,"   --  This is risky...\n");
      if (strlen(name) > 20) {
        ifprintf(fp,"   %s_LINKNAME : constant String :=\n", name);
        ifprintf(fp,"      \"%s\";\n", xname);
      } else ifprintf
        (fp,"   %s_LINKNAME : constant String := \"%s\";\n", name, xname);
    } else
#endif
   {
      ifprintf(fp,"   --  *** MISSING: function %s ***  --\n", name);
      warn("missing function ", name);
      if (strlen(name) > 20) {
        ifprintf(fp,"   %s_LINKNAME : constant String :=\n", name);
        ifprintf(fp,"      \"notsup_direct\";\n", name);
      } else ifprintf
        (fp,"   %s_LINKNAME : constant String := \"notsup_direct\";\n", name);
    }
  }
}

/* gmaxn
   -----
   generate _Maxima subtype of Natural
 */
void gmaxn(char const name[], int lower_bound) {
  ifprintf(fp,"   subtype %s_Maxima is Natural range\n",name);
  ifprintf(fp,"      %d .. Natural'Last;\n",lower_bound);
}

/* gmaxnn
   -----
   generate _Maxima subtype of Natural with tight bound
 */
void gmaxnn(char const name[], int bound) {
  ifprintf(fp,"   subtype %s_Maxima is Natural range\n",name);
  ifprintf(fp,"      %d .. %d;\n", bound, bound);
}

/* gmaxi
   -----
   generate _Maxima subtype of IO_Count
 */
void gmaxi(char const name[], int lower_bound) {
  ifprintf(fp,"   subtype %s_Maxima is IO_Count range\n",name);
  ifprintf(fp,"      %d .. IO_Count'Last;\n",lower_bound);
}

/* gmaxi
   -----
   generate _Maxima subtype of IO_Count with tight range
 */
void gmaxii(char const name[], int bound) {
  ifprintf(fp,"   subtype %s_Maxima is IO_Count range\n",name);
  ifprintf(fp,"      %d .. %d;\n", bound, bound);
}

/* gpmaxi
   ------
   generate portable maximum constant of type IO_Count
   with specified value
 */
void gpmaxi(char const name[], int value) {
  ifprintf(fp,"   Portable_%s_Maximum :\n",name);
  ifprintf(fp,"      constant IO_Count := %d;\n",value);
}

/* gpmaxn
   ------
   generate portable maximum constant of type Natural
   with specified value
 */
void gpmaxn(char const name[], int value) {
  ifprintf(fp,"   Portable_%s_Maximum :\n",name);
  ifprintf(fp,"      constant Natural := %d;\n",value);
}

/* gpmaxr
   ------
   generate portable maximum constant
   of type Natural
   as renaming of constant in package POSIX
 */
void gpmaxr(char const name[], char const oname[]) {
  ifprintf(fp,"   Portable_%s_Maximum : Natural\n",name);
  ifprintf(fp,"      renames POSIX.Portable_%s_Maximum;\n",oname);
}

/* gpmaxrioc
   ---------
   generate portable maximum constant
   of type IO_Count
   as renaming of constant in package POSIX
 */
void gpmaxrioc(char const name[], char const oname[]) {
  ifprintf(fp,"   Portable_%s_Maximum : POSIX.IO_Count\n",name);
  ifprintf(fp,"      renames POSIX.Portable_%s_Maximum;\n",oname);
}

/* grename
   -------
 */
void grename (char const name[], char const oname[]) {
  ifprintf(fp,"   subtype %s is\n",name);
  ifprintf(fp,"      POSIX.%s;\n",oname);
}

/* gmacrofunc
   ----------
 */
void gmacrofunc
  (char const funcname[],
   char const parmtype[],
   char const parmname[]) {
  ifprintf(fp,"   function %s (%s : %s) return int;\n",
     funcname, parmname, parmtype);
  ifprintf(fp,"   pragma Import (C, %s, \"%s\");\n", funcname, funcname);
}

/* create_options
   --------------
   create package POSIX.Options, in file posix-options.ads
 */
void create_options() {

  fprintf(stderr,"creating package POSIX_Options\n");
  if (! (fp = fopen ("posix-options.ads", "w"))) {
    perror ("posix-options.ads");
    quit("can't open file to write","");
  }
  gheader("POSIX.Options", IEEE_Header);
  ifprintf(fp,"package POSIX.Options is\n");

#ifdef _POSIX_ASYNCHRONOUS_IO
  gbrg("Asynchronous_IO_Support", "True", "True");
#else
#ifdef _POSIX_ASYNC_IO
#if (_POSIX_ASYNC_IO == -1)
  gbrg("Asynchronous_IO_Support", "False", "False");
#else
  gbrg("Asynchronous_IO_Support", "True", "True");
#endif
#else
  gbrg("Asynchronous_IO_Support", "False", "True");
#endif
#endif

  grename("Change_Owner_Restriction","Change_Owner_Restriction");
  grename("Filename_Truncation","Filename_Truncation");

#ifdef _POSIX_FSYNC
  gbrg("File_Synchronization_Support", "True", "True");
#else
  gbrg("File_Synchronization_Support", "False", "True");
#endif

  grename("Job_Control_Support","Job_Control_Support");

#ifdef _POSIX_MAPPED_FILES
  gbrg("Memory_Mapped_Files_Support", "True", "True");
#else
  gbrg("Memory_Mapped_Files_Support", "False", "True");
#endif

#ifdef _POSIX_MEMLOCK
  gbrg("Memory_Locking_Support", "True", "True");
#else
  gbrg("Memory_Locking_Support", "False", "True");
#endif

#ifdef _POSIX_MEMLOCK_RANGE
  gbrg("Memory_Range_Locking_Support", "True", "True");
#else
  gbrg("Memory_Range_Locking_Support", "False", "True");
#endif

#ifdef _POSIX_MEMORY_PROTECTION
  gbrg("Memory_Protection_Support", "True", "True");
#else
  gbrg("Memory_Protection_Support", "False", "True");
#endif

#ifdef _POSIX_MESSAGE_PASSING
  gbrg("Message_Queues_Support", "True", "True");
#else
  gbrg("Message_Queues_Support", "False", "True");
#endif

  grename("Saved_IDs_Support","Saved_IDs_Support");

  gbrg("Mutexes_Support", "True", "True");

#ifdef _POSIX_PRIORITIZED_IO
  gbrg("Prioritized_IO_Support", "True", "True");
#else
#ifdef _POSIX_PRIO_IO
#if (_POSIX_PRIO_IO == -1)
  gbrg("Prioritized_IO_Support", "False", "False");
#else
  gbrg("Prioritized_IO_Support", "True", "True");
#endif
#else
  gbrg("Prioritized_IO_Support", "False", "True");
#endif
#endif

#ifdef _POSIX_PRIORITY_SCHEDULING
  gbrg("Priority_Process_Scheduling_Support", "True", "True");
#ifdef _POSIX_THREADS
  gbrg("Priority_Task_Scheduling_Support", "True", "True");
#else
  gbrg("Priority_Task_Scheduling_Support", "False", "False");
#endif
#else
  gbrg("Priority_Process_Scheduling_Support", "False", "True");
  gbrg("Priority_Task_Scheduling_Support", "False", "False");
#endif

#ifdef _POSIX_REALTIME_SIGNALS
  gbrg("Realtime_Signals_Support", "True", "True");
#else
  gbrg("Realtime_Signals_Support", "False", "True");
#endif

#ifdef _POSIX_SEMAPHORES
  gbrg("Semaphores_Support", "True", "True");
#else
  gbrg("Semaphores_Support", "False", "True");
#endif

#ifdef _POSIX_SHARED_MEMORY_OBJECTS
  gbrg("Shared_Memory_Objects_Support", "True", "True");
#else
  gbrg("Shared_Memory_Objects_Support", "False", "True");
#endif

  gbrg("Signal_Entries_Support", "True", "True");

#ifdef _POSIX_SYNCHRONIZED_IO
  gbrg("Synchronized_IO_Support", "True", "True");
#else
#ifdef _POSIX_SYNC_IO
#if (_POSIX_SYNC_IO == -1)
  gbrg("Synchronized_IO_Support", "False", "False");
#else
  gbrg("Synchronized_IO_Support", "True", "True");
#endif
#else
  gbrg("Synchronized_IO_Support", "False", "True");
#endif
#endif

#ifdef _POSIX_THREAD_PRIO_PROTECT
  gbrg("Mutex_Priority_Ceiling_Support", "True", "True");
#else
  gbrg("Mutex_Priority_Ceiling_Support", "False", "True");
#endif

#ifdef _POSIX_THREAD_PRIO_INHERIT
  gbrg("Mutex_Priority_Inheritance_Support", "True", "True");
#else
  gbrg("Mutex_Priority_Inheritance_Support", "False", "True");
#endif

#ifdef _POSIX_THREAD_PROCESS_SHARED
  gbrg("Process_Shared_Support", "True", "True");
#else
  gbrg("Process_Shared_Support", "False", "True");
#endif

#ifdef _POSIX_TIMERS
  gbrg("Timers_Support", "True", "True");
#else
  gbrg("Timers_Support", "False", "True");
#endif

/* options from POSIX.5c [D2]
 */

/* What does _POSIX_PII map to in Ada?
#ifdef _POSIX_PII
  gbrg("????_Support", "True", "True");
#else
  gbrg("????_Support", "False", "True");
#endif
 */

#ifdef _POSIX_PII_XTI
  gbrg("XTI_DNI_Support", "True", "True");
#else
  gbrg("XTI_DNI_Support", "False", "True");
#endif

#ifdef _POSIX_PII_INTERNET_DGRAM
  gbrg("Internet_Datagram_Support", "True", "True");
#else
  gbrg("Internet_Datagram_Support", "False", "True");
#endif

#ifdef _POSIX_PII_INTERNET
  gbrg("Internet_Protocol_Support", "True", "True");
#else
  gbrg("Internet_Protocol_Support", "False", "True");
#endif

#ifdef _POSIX_PII_INTERNET_STREAM
  gbrg("Internet_Stream_Support", "True", "True");
#else
  gbrg("Internet_Stream_Support", "False", "True");
#endif

#ifdef _POSIX_PII_OSI
  gbrg("ISO_OSI_Protocol_Support", "True", "True");
#else
  gbrg("ISO_OSI_Protocol_Support", "False", "True");
#endif

#ifdef _POSIX_PII_OSI_M
  gbrg("OSI_Minimal_Support", "True", "True");
#else
  gbrg("OSI_Minimal_Support", "False", "True");
#endif

#ifdef _POSIX_PII_OSI_COTS
  gbrg("OSI_Connection_Support", "True", "True");
#else
  gbrg("OSI_Connection_Support", "False", "True");
#endif

#ifdef _POSIX_PII_OSI_CLTS
  gbrg("OSI_Connectionless_Support", "True", "True");
#else
  gbrg("OSI_Connectionless_Support", "False", "True");
#endif

#ifdef _POSIX_POLL
  gbrg("Poll_Support", "True", "True");
#else
  gbrg("Poll_Support", "False", "True");
#endif

#ifdef _POSIX_SELECT
  gbrg("Select_Support", "True", "True");
#else
  gbrg("Select_Support", "False", "True");
#endif

#ifdef _POSIX_PII_SOCKET
  gbrg("Sockets_DNI_Support", "True", "True");
#else
  gbrg("Sockets_DNI_Support", "False", "True");
#endif

#ifdef _POSIX_PII_NET_SUPPORT
  gbrg("Network_Management_Support", "True", "True");
#else
  gbrg("Network_Management_Support", "False", "True");
#endif

  ifprintf(fp,"end POSIX.Options;\n");
  fclose (fp);
  fprintf(stderr,"done generating posix-options.ads\n");
}

/* create_limits
   -------------
   create package POSIX.Limits, in file posix-limits.ads
 */
void create_limits() {

  fprintf(stderr,"creating package POSIX_Limits\n");
  if (! (fp = fopen ("posix-limits.ads", "w"))) {
    perror ("posix-limits.ads");
    quit("can't open file to write","");
  }
  gheader("POSIX.Limits", IEEE_Header);
  ifprintf(fp,"package POSIX.Limits is\n");
  ghdrcmnt("Portable System Limits");

  ifprintf(fp,"   --  .... Change P1003.5b?\n");
  ifprintf(fp,"   --  to allow these constants\n");
  ifprintf(fp,"   --  to be larger than the minimum values specified.\n\n");

  gpmaxr("Argument_List","Argument_List");

  ifprintf(fp,"   Portable_Asynchronous_IO_Maximum :\n");
  ifprintf(fp,"      constant Natural := 1;\n");

  gpmaxr("Child_Processes","Child_Processes");

  ifprintf(fp,"   Portable_Clock_Resolution_Minimum :\n");
  ifprintf(fp,"      constant := 20_000_000;\n");
  /* notice that this is a MINIMUM, so we don't use gpmax */

  gpmaxr("Filename","Filename_Limit");
  gpmaxr("Groups","Groups");
  gpmaxrioc("Input_Line","Input_Line_Limit");
  gpmaxrioc("Input_Queue","Input_Queue_Limit");
  gpmaxr("Links","Link_Limit");

#ifdef _POSIX_AIO_LISTIO_MAX
  gpmaxn("List_IO",_POSIX_AIO_LISTIO_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_AIO_LISTIO_MAX");
  gpmaxn("List_IO", 2);
#endif

#ifdef _POSIX_MQ_PRIO_MAX
  gpmaxn("Message_Priority",_POSIX_MQ_PRIO_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_MQ_PRIO_MAX");
  gpmaxn("Message_Priority", 32);
#endif

  gpmaxr("Open_Files","Open_Files");

#ifdef _POSIX_MQ_OPEN_MAX
  gpmaxn("Open_Message_Queues",_POSIX_MQ_OPEN_MAX);
#else
  gpmaxn("Open_Message_Queues",8);
#endif

  gpmaxr("Pathname","Pathname_Limit");
  gpmaxrioc("Pipe_Length","Pipe_Limit");

#ifdef _POSIX_SIGQUEUE_MAX
  gpmaxn("Queued_Signals",_POSIX_SIGQUEUE_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_SIGQUEUE_MAX");
  gpmaxn("Queued_Signals", 32);
#endif

#ifdef _POSIX_RTSIG_MAX
  gpmaxn("Realtime_Signals",_POSIX_RTSIG_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_RTSIG_MAX");
  gpmaxn("Realtime_Signals",8);
#endif

#ifdef _POSIX_SEM_NSEMS_MAX
  gpmaxn("Semaphores",_POSIX_SEM_NSEMS_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_SEM_NSEMS_MAX");
  gpmaxn("Semaphores", 256);
#endif

#ifdef _POSIX_SEM_VALUE_MAX
  gpmaxn("Semaphores_Value",_POSIX_SEM_VALUE_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_SEM_VALUE_MAX");
  gpmaxn("Semaphores_Value", 32767);
#endif

  gpmaxr("Streams","Stream");

#ifdef _POSIX_DELAYTIMER_MAX
  gpmaxn("Timer_Overruns",_POSIX_DELAYTIMER_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_DELAYTIMER_MAX");
  gpmaxn("Timer_Overruns", 32);
#endif

#ifdef _POSIX_TIMER_MAX
  gpmaxn("Timers",_POSIX_TIMER_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_NTIMER_MAX")
  gpmaxn("Timers", 32);
#endif

  gpmaxr("Time_Zone_String","Time_Zone_String");

  fprintf(fp,"\n");
  ifprintf(fp,"   --  limits from POSIX.5c [D2]\n\n");

#ifdef _POSIX_FD_SETSIZE
  gpmaxn("File_Descriptor_Set", _POSIX_FD_SETSIZE);
#else
  gpmaxr("File_Descriptor_Set", "Open_Files");
#endif

#ifdef _POSIX_HIWAT
  gpmaxi("Socket_Buffer", _POSIX_HIWAT);
#else
  gpmaxrioc("Socket_Buffer", "Pipe_Limit");
#endif

#ifdef _POSIX_UIO_MAXIOV
  gpmaxn("Socket_IO_Vector", _POSIX_UIO_MAXIOV);
#else
  gpmaxn("Socket_IO_Vector", 16);
#endif

#ifdef _POSIX_QLIMIT
  gpmaxn("Socket_Connection", _POSIX_QLIMIT);
#else
  gpmaxn("Socket_Connection", 1);
#endif

#ifdef _POSIX_UIO_MAXIOV
  gpmaxn("XTI_IO_Vector", _POSIX_UIO_MAXIOV);
#else
  gpmaxn("XTI_IO_Vector", 16);
#endif

  ghdrcmnt("Configurable Limits");

  grename("Argument_List_Maxima","Argument_List_Maxima");

#ifdef AIO_MAX
  gmaxnn("Asynchronous_IO",AIO_MAX);
#else
#ifdef _POSIX_AIO_MAX
  gmaxn("Asynchronous_IO",_POSIX_AIO_MAX);
#else
  gmaxn("Asynchronous_IO", 1);
#endif
#endif

#ifdef AIO_PRIO_DELTA_MAX
  gmaxnn("Asynchronous_IO_Priority_Delta",AIO_PRIO_DELTA_MAX);
#else
  gmaxn("Asynchronous_IO_Priority_Delta", 0);
#endif

  grename("Child_Processes_Maxima","Child_Processes_Maxima");

  grename("Filename_Maxima","Filename_Limit_Maxima");

  grename("Groups_Maxima","Groups_Maxima");

  grename("Input_Line_Maxima","Input_Line_Limit_Maxima");

  grename("Input_Queue_Maxima","Input_Queue_Limit_Maxima");

  grename("Links_Maxima","Link_Limit_Maxima");

#ifdef AIO_LISTIO_MAX
  gmaxnn("List_IO",AIO_LISTIO_MAX);
#else
#ifdef _POSIX_AIO_LISTIO_MAX
  gmaxn("List_IO",_POSIX_AIO_LISTIO_MAX);
#else
  gmaxn("List_IO", 2);
#endif
#endif

#ifdef MQ_PRIO_MAX
  gmaxnn("Message_Priority",MQ_PRIO_MAX);
#else
#ifdef _POSIX_MQ_PRIO_MAX
  gmaxn("Message_Priority",_POSIX_MQ_PRIO_MAX);
#else
  gmaxn("Message_Priority", 32);
#endif
#endif

#ifdef MQ_OPEN_MAX
  gmaxnn("Open_Message_Queues",MQ_OPEN_MAX);
#else
#ifdef _POSIX_MQ_OPEN_MAX
  gmaxn("Open_Message_Queues",_POSIX_MQ_OPEN_MAX);
#else
  gmaxn("Open_Message_Queues", 8);
#endif
#endif

  grename("Open_Files_Maxima","Open_Files_Maxima");

  ifprintf(fp,"   subtype Page_Size_Range ");
#ifdef PAGESIZE
  ifprintf(fp," is Natural range %d .. %d;\n", PAGESIZE, PAGESIZE);
#else
#ifdef _SC_PAGESIZE
  ifprintf(fp," is Natural range %d .. %d;\n",
    sysconf(_SC_PAGESIZE),  sysconf(_SC_PAGESIZE));
#else
  ifprintf(fp," is Natural range 0 .. -1;\n");
#endif
#endif

  grename("Pathname_Maxima","Pathname_Limit_Maxima");
  grename("Pipe_Length_Maxima","Pipe_Limit_Maxima");

#ifdef SIGQUEUE_MAX
  gmaxnn("Queued_Signals",SIGQUEUE_MAX);
#else
#ifdef _POSIX_SIGQUEUE_MAX
  gmaxn("Queued_Signals",_POSIX_SIGQUEUE_MAX);
#else
  gmaxn("Queued_Signals", 32);
#endif
#endif

#ifdef RTSIG_MAX
  gmaxnn("Realtime_Signals",RTSIG_MAX);
#else
#ifdef _POSIX_RTSIG_MAX
  gmaxn("Realtime_Signals",_POSIX_RTSIG_MAX);
#else
  gmaxn("Realtime_Signals",8);
#endif
#endif

#ifdef SEM_NSEMS_MAX
  gmaxnn("Semaphores",SEM_NSEMS_MAX);
#else
#ifdef _POSIX_SEM_NSEMS_MAX
  gmaxn("Semaphores",_POSIX_SEM_NSEMS_MAX);
#else
  gmaxn("Semaphores", 256);
#endif
#endif

#ifdef SEM_VALUE_MAX
  gmaxnn("Semaphores_Value",SEM_VALUE_MAX);
#else
#ifdef _POSIX_SEM_VALUE_MAX
  gmaxn("Semaphores_Value",_POSIX_SEM_VALUE_MAX);
#else
  gmaxn("Semaphores_Value", 32767);
#endif
#endif

  grename("Streams_Maxima","Stream_Maxima");

#ifdef DELAYTIMER_MAX
  gmaxnn("Timer_Overruns",DELAYTIMER_MAX);
#else
#ifdef _POSIX_DELAYTIMER_MAX
  gmaxn("Timer_Overruns",_POSIX_DELAYTIMER_MAX);
#else
  gmaxn("Timer_Overruns", 32);
#endif
#endif

#ifdef TIMER_MAX
  gmaxnn("Timers",TIMER_MAX);
#else
#ifdef _POSIX_TIMER_MAX
  gmaxn("Timers",_POSIX_TIMER_MAX);
#else
  gmaxn("Timers", 32);
#endif
#endif

  grename("Time_Zone_String_Maxima","Time_Zone_String_Maxima");

/* limits from POSIX.5c/D4
 */

  fprintf(fp,"\n");
  fprintf(fp,"   --  limits from POSIX.5c [D2]\n\n");

#ifdef FD_SETSIZE
  gmaxnn("File_Descriptor_Set", FD_SETSIZE);
#else
#ifdef _POSIX_FD_SETSIZE
  gmaxn("File_Descriptor_Set", _POSIX_FD_SETSIZE);
#else
#ifdef _POSIX_PIPE_BUF
  gmaxn("File_Descriptor_Set", _POSIX_PIPE_BUF);
#else
  gmaxn("File_Descriptor_Set", 8);
#endif
#endif
#endif

#ifdef SOCK_MAXBUF
  gmaxii("Socket_Buffer", SOCK_MAXBUF);
#else
#ifdef _POSIX_PIPE_BUF
  gmaxi("Socket_Buffer", _POSIX_PIPE_BUF);
#else
  gmaxi("Socket_Buffer", 8);
#endif
#endif

#ifdef UIO_MAXIOV
  gmaxnn ("Socket_IO_Vector", UIO_MAXIOV);
#else
#ifdef _POSIX_UIO_MAXIOV
  gmaxn ("Socket_IO_Vector", _POSIX_UIO_MAXIOV);
#else
  gmaxn ("Socket_IO_Vector", 16);
#endif
#endif

#ifdef _POSIX_QLIMIT
  gmaxn ("Socket_Connection", _POSIX_QLIMIT);
#else
  gmaxn ("Socket_Connection", 1);
#endif

#ifdef T_IOV_MAX
  gmaxnn ("XTI_IO_Vector", T_IOV_MAX);
#else
#ifdef _POSIX_UIO_MAXIOV
  gmaxn("XTI_IO_Vector", _POSIX_UIO_MAXIOV);
#else
  gmaxn("XTI_IO_Vector", 16);
#endif
#endif

  ifprintf(fp,"end POSIX.Limits;\n");
  fclose (fp);
  fprintf(stderr,"done generating posix-limits.ads\n");

}

/* create_posix
   ------------
   create package POSIX, in file posix.ads
 */
void create_posix() {

  int   max_posix_error;
  int   XTI_Error_First;
  int   XTI_Error_Last;
  int   EAI_Error_First;
  int   EAI_Error_Last;
  int   count;

  /* The Makefile is responsible for defining LIBS correctly */
#ifdef LIBS
  char  libs[] = LIBS, *s1, *s2;
#endif

  fprintf(stderr,"creating package POSIX\n");
  if (! (fp = fopen ("posix.ads", "w"))) {
    perror ("posix.ads");
    quit("can't open file to write","");
  }

  gheader("POSIX", IEEE_Header);
  ifprintf(fp,"with Ada_Streams;\n");
  ifprintf(fp,"with Interfaces;\n");
  ifprintf(fp,"package POSIX is\n\n");

#ifdef LIBS
  /* Generate one pragma Linker_Options per library */

  for (s1 = libs; *s1; ) {
    for (s2 = s1; *s2 && *s2 != ' '; s2++);
    if (*s2) {
      *s2 = '\0';
      ifprintf(fp,"   pragma Linker_Options (\"%s\");\n", s1);
      s1 = s2 + 1;
    } else
      s1 = s2;
  }
#endif

  fprintf(fp,"\n");
  ifprintf(fp,"   --  2.4.1 Constants and Static Subtypes\n\n");

  fprintf(fp,"   --   Version Identification\n\n");

#ifdef _POSIX_VERSION
  GCST("POSIX_Version", _POSIX_VERSION);
#else
  GDFLT("POSIX_Version", 0);
#endif
  ifprintf(fp,"   POSIX_Ada_Version : constant := 1995_00;\n\n");

  ifprintf(fp,"   --  Optional Facilities (obsolescent, 0)\n");
  ifprintf(fp,"   --  See package POSIX.Limits for preferred interfaces.\n\n");

#ifdef _POSIX_JOB_CONTROL
  gbrg("Job_Control_Support", "True", "True");
#else
  gbrg("Job_Control_Support", "False", "True");
#endif

#ifdef _POSIX_SAVED_IDS
  gbrg("Saved_IDs_Support", "True", "True");
#else
  gbrg("Saved_IDs_Support", "False", "False");
#endif

#ifdef _POSIX_CHOWN_RESTRICTED
#if (_POSIX_CHOWN_RESTRICTED == -1)
  gbrg("Change_Owner_Restriction", "False", "False");
#else
  gbrg("Change_Owner_Restriction", "True", "True");
#endif
#else
  gbrg("Change_Owner_Restriction", "False", "True");
#endif

#ifdef _POSIX_NO_TRUNC
#if (_POSIX_NO_TRUNC == -1)
  gbrg("Filename_Truncation", "False", "False");
#else
  gbrg("Filename_Truncation", "True", "True");
#endif
#else
  gbrg("Filename_Truncation", "False", "True");
#endif
  ifprintf(fp,"   --  Bytes and I/O Counts\n\n");

  ifprintf(fp,"   Byte_Size : constant :=  %d;\n\n",bits_per_byte);

  ifprintf(fp,"   type IO_Count is range -2**%d .. (2**%d)-1;\n\n",
    sizeof(ssize_t)*bits_per_byte-1,
    sizeof(ssize_t)*bits_per_byte-1);
  ifprintf(fp,"   for IO_Count'Size use %d;\n", sizeof(ssize_t)*bits_per_byte);

  gmaxi("IO_Count", 32767);

  ifprintf(fp,"   --  System Limits (obsolescent)\n");
  ifprintf(fp,"   --  See package POSIX.Limits for preferred interfaces.\n\n");

/* Run-Time Increasable Values
   These must be defined, but
   the actual limits, as reported by sysconf(), may be higher.
*/

#ifdef _POSIX_NGROUPS_MAX
  gpmaxn("Groups",_POSIX_NGROUPS_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_NGROUPS_MAX");
  gpmaxn("Groups", 0);
#endif
#ifdef NGROUPS_MAX
  /* run-time increasable value */
  gmaxn("Groups",NGROUPS_MAX);
#else
#ifdef _POSIX_NGROUPS_MAX
  gmaxn("Groups", _POSIX_NGROUPS_MAX);
#else
  gmaxn("Groups", 0);
#endif
#endif

/*
   Runtime Invariant Values
   These need not be defined.
   If defined, these are reliable static bounds,
   not to be exceeded by the sysconf() result.
 */

#ifdef _POSIX_ARG_MAX
  gpmaxn("Argument_List",_POSIX_ARG_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_ARG_MAX");
  gpmaxn("Argument_List",4096);
#endif
#ifdef ARG_MAX
  gmaxnn("Argument_List", ARG_MAX);
#else
#ifdef _POSIX_ARG_MAX
  gmaxn("Argument_List", _POSIX_ARG_MAX);
#else
  gmaxn("Argument_List",4096);
#endif
#endif

#ifdef _POSIX_CHILD_MAX
  gpmaxn("Child_Processes",_POSIX_CHILD_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_CHILD_MAX");
  gpmaxn("Child_Processes",6);
#endif
#ifdef CHILD_MAX
  gmaxnn("Child_Processes", CHILD_MAX);
#else
#ifdef _POSIX_CHILD_MAX
  gmaxn("Child_Processes", _POSIX_CHILD_MAX);
#else
  gmaxn("Child_Processes",6);
#endif
#endif

#ifdef _POSIX_OPEN_MAX
  gpmaxn("Open_Files",_POSIX_OPEN_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_OPEN_MAX");
  gpmaxn("Open_Files", 16);
#endif
#ifdef OPEN_MAX
  gmaxnn("Open_Files", OPEN_MAX);
#else
#ifdef _POSIX_OPEN_MAX
  gmaxn("Open_Files", _POSIX_OPEN_MAX);
#else
  gmaxn("Open_Files", 16);
#endif
#endif

#ifdef _POSIX_STREAM_MAX
  gpmaxn("Stream",_POSIX_STREAM_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_STREAM_MAX");
  gpmaxn("Stream",8);
#endif
#ifdef STREAM_MAX
  gmaxnn("Stream", STREAM_MAX);
#else
#ifdef _POSIX_STREAM_MAX
  gmaxn("Stream", _POSIX_STREAM_MAX);
#else
  gmaxn("Stream",8);
#endif
#endif

#ifdef _POSIX_TZNAME_MAX
  gpmaxn("Time_Zone_String",_POSIX_TZNAME_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_TZNAME_MAX");
  gpmaxn("Time_Zone_String", 3);
#endif
#ifdef TZNAME_MAX
  gmaxnn("Time_Zone_String", TZNAME_MAX);
#else
#ifdef _POSIX_TZNAME_MAX
  gmaxn("Time_Zone_String", _POSIX_TZNAME_MAX);
#else
  gmaxn("Time_Zone_String", 3);
#endif
#endif

/*
   Pathname Variable Values
   These need not be defined.
   If defined, these are reliable static bounds,
   not to be exceeded by pathconf() result.

 */

   ifprintf(fp,"   --  Pathname Variable Values (obsolescent)\n");
   ifprintf(fp,"   --  See package POSIX.Limits for preferred"
     " interfaces.\n\n");

#ifdef _POSIX_LINK_MAX
  gpmaxn("Link_Limit",_POSIX_LINK_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_LINK_MAX");
  gpmaxn("Link_Limit",8);
#endif
#ifdef LINK_MAX
  gmaxnn("Link_Limit", LINK_MAX);
#else
#ifdef _POSIX_LINK_MAX
  gmaxn("Link_Limit", _POSIX_LINK_MAX);
#else
  gmaxn("Link_Limit",8);
#endif
#endif

#ifdef _POSIX_MAX_INPUT
  gpmaxi("Input_Line_Limit",_POSIX_MAX_INPUT);
#else
  NON_SUPPORT_MESSAGE("_POSIX_MAX_INPUT");
  gpmaxi("Input_Line_Limit", 255);
#endif
#ifdef MAX_INPUT
  gmaxii("Input_Line_Limit",MAX_INPUT);
#else
#ifdef _POSIX_MAX_INPUT
  gmaxi("Input_Line_Limit", _POSIX_MAX_INPUT);
#else
  gmaxi("Input_Line_Limit", 255);
#endif
#endif

#ifdef _POSIX_MAX_CANON
  gpmaxi("Input_Queue_Limit",_POSIX_MAX_CANON);
#else
  NON_SUPPORT_MESSAGE("_POSIX_MAX_CANON");
  gpmaxi("Input_Queue_Limit", 255);
#endif
#ifdef MAX_CANON
  gmaxii("Input_Queue_Limit", MAX_CANON);
#else
#ifdef _POSIX_MAX_CANON
  gmaxi("Input_Queue_Limit", _POSIX_MAX_CANON);
#else
  gmaxi("Input_Queue_Limit", 255);
#endif
#endif

#ifdef _POSIX_NAME_MAX
  gpmaxn("Filename_Limit",_POSIX_NAME_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_NAME_MAX");
  gpmaxn("Filename_Limit", 14);
#endif
#ifdef NAME_MAX
  gmaxnn("Filename_Limit", NAME_MAX);
#else
#ifdef _POSIX_NAME_MAX
  gmaxn("Filename_Limit", _POSIX_NAME_MAX);
#else
  gmaxn("Filename_Limit", 14);
#endif
#endif

#ifdef _POSIX_PATH_MAX
  gpmaxn("Pathname_Limit",_POSIX_PATH_MAX);
#else
  NON_SUPPORT_MESSAGE("_POSIX_PATH_MAX");
  gpmaxn("Pathname_Limit", 255);
#endif
#ifdef PATH_MAX
  gmaxnn("Pathname_Limit", PATH_MAX);
#else
#ifdef _POSIX_PATH_MAX
  gmaxn("Pathname_Limit", _POSIX_PATH_MAX);
#else
  gmaxn("Pathname_Limit", 255);
#endif
#endif

#ifdef _POSIX_PIPE_BUF
  gpmaxi("Pipe_Limit",_POSIX_PIPE_BUF);
#else
  gpmaxi("Pipe_Limit", 512);
#endif
#ifdef PIPE_BUF
  gmaxii("Pipe_Limit", PIPE_BUF);
#else
#ifdef _POSIX_PIPE_BUF
  gmaxi("Pipe_Limit", _POSIX_PIPE_BUF);
#else
  gmaxi("Pipe_Limit", 512);
#endif
#endif

  ifprintf(fp,"   --  Blocking Behavior Values\n");

  ifprintf(fp,"   type Blocking_Behavior is (Tasks, Program, Special);\n");

  ifprintf(fp,"   subtype Text_IO_Blocking_Behavior is Blocking_Behavior\n");
  ifprintf(fp,"      range Tasks .. Tasks;\n");

  ifprintf(fp,"   IO_Blocking_Behavior               :");
  ifprintf(fp," constant Blocking_Behavior\n");
  ifprintf(fp,"                                      := Tasks;\n");
  ifprintf(fp,"   File_Lock_Blocking_Behavior        :");
  ifprintf(fp," constant Blocking_Behavior\n");
  ifprintf(fp,"                                      := Tasks;\n");
  ifprintf(fp,"   Wait_For_Child_Blocking_Behavior   :");
  ifprintf(fp," constant Blocking_Behavior\n");
  ifprintf(fp,"                                      := Tasks;\n");

  ifprintf(fp,"   subtype Realtime_Blocking_Behavior is Blocking_Behavior\n");
  ifprintf(fp,"      range Tasks .. Program;\n");

  ifprintf(fp,"   --  Signal Masking\n");

  ifprintf(fp,"   type Signal_Masking is ");
  ifprintf(fp,"(No_Signals, RTS_Signals, All_Signals);\n");

  ifprintf(fp,"   --  Characters and Strings\n");

  ifprintf(fp,"   type POSIX_Character is new Standard.Character;\n");

  ifprintf(fp,"   --  We rely here on the fact that the GNAT"
    " type Character\n");
  ifprintf(fp,"   --  is the same as the GCC type char in C,\n");
  ifprintf(fp,"   --  which in turn must be the same as POSIX_Character.\n\n");
  ifprintf(fp,"   NUL : constant POSIX_Character := POSIX_Character (ASCII.NUL);\n");
  ifprintf(fp,"   SOH : constant POSIX_Character := POSIX_Character (ASCII.SOH);\n");
  ifprintf(fp,"   STX : constant POSIX_Character := POSIX_Character (ASCII.STX);\n");
  ifprintf(fp,"   ETX : constant POSIX_Character := POSIX_Character (ASCII.ETX);\n");
  ifprintf(fp,"   EOT : constant POSIX_Character := POSIX_Character (ASCII.EOT);\n");
  ifprintf(fp,"   ENQ : constant POSIX_Character := POSIX_Character (ASCII.ENQ);\n");
  ifprintf(fp,"   ACK : constant POSIX_Character := POSIX_Character (ASCII.ACK);\n");
  ifprintf(fp,"   BEL : constant POSIX_Character := POSIX_Character (ASCII.BEL);\n");
  ifprintf(fp,"   BS  : constant POSIX_Character := POSIX_Character (ASCII.BS);\n");
  ifprintf(fp,"   HT  : constant POSIX_Character := POSIX_Character (ASCII.HT);\n");
  ifprintf(fp,"   LF  : constant POSIX_Character := POSIX_Character (ASCII.LF);\n");
  ifprintf(fp,"   VT  : constant POSIX_Character := POSIX_Character (ASCII.VT);\n");
  ifprintf(fp,"   FF  : constant POSIX_Character := POSIX_Character (ASCII.FF);\n");
  ifprintf(fp,"   CR  : constant POSIX_Character := POSIX_Character (ASCII.CR);\n");
  ifprintf(fp,"   SO  : constant POSIX_Character := POSIX_Character (ASCII.SO);\n");
  ifprintf(fp,"   SI  : constant POSIX_Character := POSIX_Character (ASCII.SI);\n");
  ifprintf(fp,"   DLE : constant POSIX_Character := POSIX_Character (ASCII.DLE);\n");
  ifprintf(fp,"   DC1 : constant POSIX_Character := POSIX_Character (ASCII.DC1);\n");
  ifprintf(fp,"   DC2 : constant POSIX_Character := POSIX_Character (ASCII.DC2);\n");
  ifprintf(fp,"   DC3 : constant POSIX_Character := POSIX_Character (ASCII.DC3);\n");
  ifprintf(fp,"   DC4 : constant POSIX_Character := POSIX_Character (ASCII.DC4);\n");
  ifprintf(fp,"   NAK : constant POSIX_Character := POSIX_Character (ASCII.NAK);\n");
  ifprintf(fp,"   SYN : constant POSIX_Character := POSIX_Character (ASCII.SYN);\n");
  ifprintf(fp,"   ETB : constant POSIX_Character := POSIX_Character (ASCII.ETB);\n");
  ifprintf(fp,"   CAN : constant POSIX_Character := POSIX_Character (ASCII.CAN);\n");
  ifprintf(fp,"   EM  : constant POSIX_Character := POSIX_Character (ASCII.EM);\n");
  ifprintf(fp,"   SUB : constant POSIX_Character := POSIX_Character (ASCII.SUB);\n");
  ifprintf(fp,"   ESC : constant POSIX_Character := POSIX_Character (ASCII.ESC);\n");
  ifprintf(fp,"   FS  : constant POSIX_Character := POSIX_Character (ASCII.FS);\n");
  ifprintf(fp,"   GS  : constant POSIX_Character := POSIX_Character (ASCII.GS);\n");
  ifprintf(fp,"   RS  : constant POSIX_Character := POSIX_Character (ASCII.RS);\n");
  ifprintf(fp,"   US  : constant POSIX_Character := POSIX_Character (ASCII.US);\n\n");

  ifprintf(fp,"   type POSIX_String is array (Positive range <>) ");
  ifprintf(fp,"of aliased POSIX_Character;\n");

  ifprintf(fp,"   function To_POSIX_String (Str : String) ");
  ifprintf(fp,"return POSIX_String;\n");

  ifprintf(fp,"   function To_POSIX_String (Str : Wide_String) ");
  ifprintf(fp,"return POSIX_String;\n");

  ifprintf(fp,"   function To_String (Str : POSIX_String) return String;\n");

  ifprintf(fp,"   function To_Wide_String (Str : POSIX_String) ");
  ifprintf(fp,"return Wide_String;\n");

  ifprintf(fp,"   function To_Stream_Element_Array (Buffer : POSIX_String)\n");
  ifprintf(fp,"      return Ada_Streams.Stream_Element_Array;\n");

  ifprintf(fp,"   function To_POSIX_String (Buffer : ");
  ifprintf(fp,"Ada_Streams.Stream_Element_Array)\n");
  ifprintf(fp,"      return POSIX_String;\n");

  ifprintf(fp,"   subtype Filename is POSIX_String;\n");
  ifprintf(fp,"   subtype Pathname is POSIX_String;\n");

  ifprintf(fp,"   function Is_Filename (Str : POSIX_String)"
    " return Boolean;\n");
  ifprintf(fp,"   function Is_Pathname (Str : POSIX_String)"
    " return Boolean;\n");

  ifprintf(fp,"   function Is_Portable_Filename (Str : POSIX_String)");
  ifprintf(fp," return Boolean;\n");
  ifprintf(fp,"   function Is_Portable_Pathname (Str : POSIX_String)");
  ifprintf(fp," return Boolean;\n");

  ifprintf(fp,"   --  String Lists\n");

  ifprintf(fp,"   type POSIX_String_List is limited private;\n");

  ifprintf(fp,"   Empty_String_List : constant POSIX_String_List;\n");

  ifprintf(fp,"   procedure Make_Empty (List : in out POSIX_String_List);\n");

  ifprintf(fp,"   procedure Append (List : in out POSIX_String_List;\n");
  ifprintf(fp,"                     Str  : in POSIX_String);\n");

  ifprintf(fp,"   generic\n");
  ifprintf(fp,"      with procedure Action\n");
  ifprintf(fp,"        (Item : in POSIX_String;\n");
  ifprintf(fp,"        Quit : in out Boolean);\n");
  ifprintf(fp,"   procedure For_Every_Item (List : in POSIX_String_List);\n");

  ifprintf(fp,"   function Length (List : POSIX_String_List) ");
  ifprintf(fp,"return Natural;\n");

  ifprintf(fp,"   function Value\n");
  ifprintf(fp,"     (List  : POSIX_String_List;\n");
  ifprintf(fp,"      Index : Positive) return POSIX_String;\n");

  ifprintf(fp,"   --  option sets\n");

  ifprintf(fp,"   type Option_Set is private;\n");
  ifprintf(fp,"   function Empty_Set return Option_Set;\n");
  ifprintf(fp,"   function \"+\" (L, R : Option_Set) return Option_Set;\n");
  ifprintf(fp,"   function \"-\" (L, R : Option_Set) return Option_Set;\n");
  ifprintf(fp,"   function \"<\" (Left, Right : Option_Set)"
    " return Boolean;\n");
  ifprintf(fp,"   function \"<=\"(Left, Right : Option_Set)"
    " return Boolean;\n");
  ifprintf(fp,"   function \">\" (Left, Right : Option_Set)"
    " return Boolean;\n");
  ifprintf(fp,"   function \">=\"(Left, Right : Option_Set)"
    " return Boolean;\n");

  { int i;
    for (i =1; i<32; i++) {
      ifprintf(fp,"   Option_%d :  constant Option_Set;\n", i);
    }
  }

  ifprintf(fp,"   --  Exceptions and error codes\n");

  ifprintf(fp,"   POSIX_Error : exception;\n");

  gsitp("Error_Code", sizeof(int));
  ifprintf(fp,"   function Get_Error_Code return Error_Code;\n");
  ifprintf(fp,"   procedure Set_Error_Code (Error : in Error_Code);\n");
  ifprintf(fp,"   function Is_POSIX_Error (Error : Error_Code) ");
  ifprintf(fp,"return Boolean;\n");
  ifprintf(fp,"   function Image (Error : Error_Code) return String;\n");
  ifprintf(fp,"   No_Error : constant Error_Code := 0;\n");
  ifprintf(fp,"   --  Error code constants with negative values ");
  ifprintf(fp,"correspond to\n");
  ifprintf(fp,"   --  error codes that are not supported by the ");
  ifprintf(fp,"current system.\n");
  ifprintf(fp,"   --  error codes\n");

  max_GCST2 = 0;

#ifdef E2BIG
  GCST2("E2BIG", "Argument_List_Too_Long", E2BIG);
#else
  GDFLT2("E2BIG", "Argument_List_Too_Long");
#endif
#ifdef EACCES
  GCST2("EACCES", "Permission_Denied", EACCES);
#else
  GDFLT2("EACCES", "Permission_Denied");
#endif
#ifdef EADDRINUSE
  GCST2("EADDRINUSE", "Address_In_Use", EADDRINUSE);
#else
  GDFLT2("EADDRINUSE", "Address_In_Use");
#endif
#ifdef EADDRNOTAVAIL
  GCST2("EADDRNOTAVAIL", "Address_Not_Available", EADDRNOTAVAIL);
#else
  GDFLT2("EADDRNOTAVAIL", "Address_Not_Available");
#endif
#ifdef EAFNOSUPPORT
  GCST2("EAFNOSUPPORT", "Inappropriate_Family", EAFNOSUPPORT);
#else
  GDFLT2("EAFNOSUPPORT", "Inappropriate_Family");
#endif
#ifdef EAGAIN
  GCST2("EAGAIN", "Resource_Temporarily_Unavailable", EAGAIN);
#else
  GDFLT2("EAGAIN", "Resource_Temporarily_Unavailable");
#endif
#ifdef EALREADY
  GCST2("EALREADY", "Already_Awaiting_Connection", EALREADY);
#else
  GDFLT2("EALREADY", "Already_Awaiting_Connection");
#endif
#ifdef EBADF
  GCST2("EBADF", "Bad_File_Descriptor", EBADF);
#else
  GDFLT2("EBADF", "Bad_File_Descriptor");
#endif
#ifdef EBADMSG
  GCST2("EBADMSG", "Bad_Message", EBADMSG);
#else
  GDFLT2("EBADMSG", "Bad_Message");
#endif
#ifdef EBUSY
  GCST2("EBUSY", "Resource_Busy", EBUSY);
#else
  GDFLT2("EBUSY", "Resource_Busy");
#endif
#ifdef ECANCELED
  GCST2("ECANCELED", "Operation_Canceled", ECANCELED);
#else
  GDFLT2("ECANCELED", "Operation_Canceled");
#endif
#ifdef ECHILD
  GCST2("ECHILD", "No_Child_Process", ECHILD);
#else
  GDFLT2("ECHILD", "No_Child_Process");
#endif
#ifdef ECONNABORTED
  GCST2("ECONNABORTED", "Connection_Aborted", ECONNABORTED);
#else
  GDFLT2("ECONNABORTED", "Connection_Aborted");
#endif
#ifdef ECONNREFUSED
  GCST2("ECONNREFUSED", "Connection_Refused", ECONNREFUSED);
#else
  GDFLT2("ECONNREFUSED", "Connection_Refused");
#endif
#ifdef ECONNRESET
  GCST2("ECONNRESET", "Connection_Reset", ECONNRESET);
#else
  GDFLT2("ECONNRESET", "Connection_Reset");
#endif
#ifdef EDEADLK
  GCST2("EDEADLK", "Resource_Deadlock_Avoided", EDEADLK);
#else
  GDFLT2("EDEADLK", "Resource_Deadlock_Avoided");
#endif
#ifdef EDOM
  GCST2("EDOM", "Domain_Error", EDOM);
#else
  GDFLT2("EDOM", "Domain_Error");
#endif
#ifdef EEXIST
  GCST2("EEXIST", "File_Exists", EEXIST);
#else
  GDFLT2("EEXIST", "File_Exists");
#endif
#ifdef EFAULT
  GCST2("EFAULT", "Bad_Address", EFAULT);
#else
  GDFLT2("EFAULT", "Bad_Address");
#endif
#ifdef EFBIG
  GCST2("EFBIG", "File_Too_Large", EFBIG);
#else
  GDFLT2("EFBIG", "File_Too_Large");
#endif
#ifdef EHOSTDOWN
  GCST2("EHOSTDOWN", "Host_Down", EHOSTDOWN);
#else
  GDFLT2("EHOSTDOWN", "Host_Down");
#endif
#ifdef EHOSTUNREACH
  GCST2("EHOSTUNREACH", "Host_Unreachable", EHOSTUNREACH);
#else
  GDFLT2("EHOSTUNREACH", "Host_Unreachable");
#endif
#ifdef EINPROGRESS
  GCST2("EINPROGRESS", "Operation_In_Progress", EINPROGRESS);
#else
  GDFLT2("EINPROGRESS", "Operation_In_Progress");
#endif
#ifdef EINTR
  GCST2("EINTR", "Interrupted_Operation", EINTR);
#else
  GDFLT2("EINTR", "Interrupted_Operation");
#endif
#ifdef EINVAL
  GCST2("EINVAL", "Invalid_Argument", EINVAL);
#else
  GDFLT2("EINVAL", "Invalid_Argument");
#endif
#ifdef EIO
  GCST2("EIO", "Input_Output_Error", EIO);
#else
  GDFLT2("EIO", "Input_Output_Error");
#endif
#ifdef EISCONN
  GCST2("EISCONN", "Is_Already_Connected", EISCONN);
#else
  GDFLT2("EISCONN", "Is_Already_Connected");
#endif
#ifdef EISDIR
  GCST2("EISDIR", "Is_A_Directory", EISDIR);
#else
  GDFLT2("EISDIR", "Is_A_Directory");
#endif
#ifdef EMFILE
  GCST2("EMFILE", "Too_Many_Open_Files", EMFILE);
#else
  GDFLT2("EMFILE", "Too_Many_Open_Files");
#endif
#ifdef EMLINK
  GCST2("EMLINK", "Too_Many_Links", EMLINK);
#else
  GDFLT2("EMLINK", "Too_Many_Links");
#endif
#ifdef EMSGSIZE
  GCST2("EMSGSIZE", "Message_Too_Long", EMSGSIZE);
#else
  GDFLT2("EMSGSIZE", "Message_Too_Long");
#endif
#ifdef ENAMETOOLONG
  GCST2("ENAMETOOLONG", "Filename_Too_Long", ENAMETOOLONG);
#else
  GDFLT2("ENAMETOOLONG", "Filename_Too_Long");
#endif
#ifdef ENETDOWN
  GCST2("ENETDOWN", "Network_Down", ENETDOWN);
#else
  GDFLT2("ENETDOWN", "Network_Down");
#endif
#ifdef ENETRESET
  GCST2("ENETRESET", "Network_Reset", ENETRESET);
#else
  GDFLT2("ENETRESET", "Network_Reset");
#endif
#ifdef ENETUNREACH
  GCST2("ENETUNREACH", "Network_Unreachable", ENETUNREACH);
#else
  GDFLT2("ENETUNREACH", "Network_Unreachable");
#endif
#ifdef ENFILE
  GCST2("ENFILE", "Too_Many_Open_Files_In_System", ENFILE);
#else
  GDFLT2("ENFILE", "Too_Many_Open_Files_In_System");
#endif
#ifdef ENOBUFS
  GCST2("ENOBUFS", "No_Buffer_Space", ENOBUFS);
#else
  GDFLT2("ENOBUFS", "No_Buffer_Space");
#endif
#ifdef ENODEV
  GCST2("ENODEV", "No_Such_Operation_On_Device", ENODEV);
#else
  GDFLT2("ENODEV", "No_Such_Operation_On_Device");
#endif
#ifdef ENOENT
  GCST2("ENOENT", "No_Such_File_Or_Directory", ENOENT);
#else
  GDFLT2("ENOENT", "No_Such_File_Or_Directory");
#endif
#ifdef ENOPROTOOPT
  GCST2("ENOPROTOOPT", "Unknown_Protocol_Option", ENOPROTOOPT);
#else
  GDFLT2("ENOPROTOOPT", "Unknown_Protocol_Option");
#endif
#ifdef ENOEXEC
  GCST2("ENOEXEC", "Exec_Format_Error", ENOEXEC);
#else
  GDFLT2("ENOEXEC", "Exec_Format_Error");
#endif
#ifdef ENOLCK
  GCST2("ENOLCK", "No_Locks_Available", ENOLCK);
#else
  GDFLT2("ENOLCK", "No_Locks_Available");
#endif
#ifdef ENOMEM
  GCST2("ENOMEM", "Not_Enough_Space", ENOMEM);
#else
  GDFLT2("ENOMEM", "Not_Enough_Space");
#endif
#ifdef ENOSPC
  GCST2("ENOSPC", "No_Space_Left_On_Device", ENOSPC);
#else
  GDFLT2("ENOSPC", "No_Space_Left_On_Device");
#endif
#ifdef ENOTCONN
  GCST2("ENOTCONN", "Not_Connected", ENOTCONN);
#else
  GDFLT2("ENOTCONN", "Not_Connected");
#endif
#ifdef ENOTSOCK
  GCST2("ENOTSOCK", "Not_A_Socket", ENOTSOCK);
#else
  GDFLT2("ENOTSOCK", "Not_A_Socket");
#endif
#ifdef ENOTSUP
  GCST2("ENOTSUP", "Operation_Not_Supported", ENOTSUP);
#else
  NON_SUPPORT_MESSAGE("ENOTSUP");
  GCST2("ENOTSUP", "Operation_Not_Supported", ENOSYS);
#endif
#ifdef ENOTDIR
  GCST2("ENOTDIR", "Not_A_Directory", ENOTDIR);
#else
  GDFLT2("ENOTDIR", "Not_A_Directory");
#endif
#ifdef ENOTEMPTY
  GCST2("ENOTEMPTY", "Directory_Not_Empty", ENOTEMPTY);
#else
  GDFLT2("ENOTEMPTY", "Directory_Not_Empty");
#endif
#ifdef ENOSYS
  GCST2("ENOSYS", "Operation_Not_Implemented", ENOSYS);
#else
  GDFLT2("ENOSYS", "Operation_Not_Supported");
#endif
#ifdef ENOTTY
  GCST2("ENOTTY", "Inappropriate_IO_Control_Operation", ENOTTY);
#else
  GDFLT2("ENOTTY", "Inappropriate_IO_Control_Operation");
#endif
#ifdef ENXIO
  GCST2("ENXIO", "No_Such_Device_Or_Address", ENXIO);
#else
  GDFLT2("ENXIO", "No_Such_Device_Or_Address");
#endif
#ifdef EOPNOTSUPP
  GCST2("EOPNOTSUPP", "Option_Not_Supported", EOPNOTSUPP);
#else
  GDFLT2("EOPNOTSUPP", "Option_Not_Supported");
#endif
#ifdef EPERM
  GCST2("EPERM", "Operation_Not_Permitted", EPERM);
#else
  GDFLT2("EPERM", "Operation_Not_Permitted");
#endif
#ifdef EPIPE
  GCST2("EPIPE", "Broken_Pipe", EPIPE);
#else
  GDFLT2("EPIPE", "Broken_Pipe");
#endif
#ifdef EPROTONOSUPPORT
  GCST2("EPROTONOSUPPORT", "Protocol_Not_Supported", EPROTONOSUPPORT);
#else
  GDFLT2("EPROTONOSUPPORT", "Protocol_Not_Supported");
#endif
#ifdef EPROTOTYPE
  GCST2("EPROTOTYPE", "Wrong_Protocol_Type", EPROTOTYPE);
#else
  GDFLT2("EPROTOTYPE", "Wrong_Protocol_Type");
#endif
/* .... what is ERANGE? .... */
#ifdef ERANGE
  GCST2("ERANGE", "TBD2", ERANGE);
#else
  GDFLT2("ERANGE", "TBD2");
#endif
#ifdef EROFS
  GCST2("EROFS", "Read_Only_File_System", EROFS);
#else
  GDFLT2("EROFS", "Read_Only_File_System");
#endif
#ifdef ESOCKTNOSUPPORT
  GCST2("ESOCKTNOSUPPORT", "Socket_Not_Supported", ESOCKTNOSUPPORT);
#else
  GDFLT2("ESOCKTNOSUPPORT", "Socket_Not_Supported");
#endif
#ifdef ESPIPE
  GCST2("ESPIPE", "Invalid_Seek", ESPIPE);
#else
  GDFLT2("ESPIPE", "Invalid_Seek");
#endif
#ifdef ESRCH
  GCST2("ESRCH", "No_Such_Process", ESRCH);
#else
  GDFLT2("ESRCH", "No_Such_Process");
#endif
#ifdef ETIMEDOUT
  GCST2("ETIMEDOUT", "Timed_Out", ETIMEDOUT);
#else
  GDFLT2("ETIMEDOUT", "Timed_Out");
#endif
#ifdef EWOULDBLOCK
  GCST2("EWOULDBLOCK", "Would_Block", EWOULDBLOCK);
#else
  GDFLT2("EWOULDBLOCK", "Would_Block");
#endif
#ifdef EXDEV
  GCST2("EXDEV", "Improper_Link", EXDEV);
#else
  GDFLT2("EXDEV", "Improper_Link");
#endif
#ifdef HOST_NOT_FOUND
  GCST("Host_Not_Found", HOST_NOT_FOUND);
#else
  GDFLT("Host_Not_Found", -1);
#endif
#ifdef NO_DATA
  GCST2("NO_DATA", "No_Address_Available", NO_DATA);
#else
  GDFLT2("NO_DATA", "No_Address_Available");
#endif
#ifdef NO_RECOVERY
  GCST2("NO_RECOVERY", "Unrecoverable_Error", NO_RECOVERY);
#else
  GDFLT2("NO_RECOVERY", "Unrecoverable_Error");
#endif

  max_posix_error = max_GCST2;

  EAI_Error_First = 10000;      /* Start with a bias of 10000 */
  while (1) {
     if (EAI_Error_First > max_posix_error) {
        break;
     } else {
        EAI_Error_First = EAI_Error_First * 10;
     } /* end if */
  } /* end while */

  max_GCST2 = EAI_Error_First;

#ifdef EAI_ADDRFAMILY
  GCST2("EAI_ADDRFAMILY", "Unknown_Address_Type",
         EAI_ADDRFAMILY+EAI_Error_First);
#else
  GDFLT2("EAI_ADDRFAMILY", "Unknown_Address_Type");
#endif
#ifdef EAI_AGAIN
  GCST2("EAI_AGAIN", "Try_Again",
         EAI_AGAIN+EAI_Error_First);
#else
  GDFLT2("EAI_AGAIN", "Try_Again");
#endif
#ifdef EAI_BADFLAGS
  GCST2("EAI_BADFLAGS", "Invalid_Flags",
         EAI_BADFLAGS+EAI_Error_First);
#else
  GDFLT2("EAI_BADFLAGS", "Invalid_Flags");
#endif
#ifdef EAI_FAIL
  GCST2("EAI_FAIL", "Name_Failed",
         EAI_FAIL+EAI_Error_First);
#else
  GDFLT2("EAI_FAIL", "Name_Failed");
#endif
#ifdef EAI_FAMILY
  GCST2("EAI_FAMILY", "Unknown_Protocol_Family",
         EAI_FAMILY+EAI_Error_First);
#else
  GDFLT2("EAI_FAMILY", "Unknown_Protocol_Family");
#endif
#ifdef EAI_MEMORY
  GCST2("EAI_MEMORY", "Memory_Allocation_Failed",
         EAI_MEMORY+EAI_Error_First);
#else
  GDFLT2("EAI_MEMORY", "Memory_Allocation_Failed");
#endif
#ifdef EAI_NODATA
  GCST2("EAI_NODATA", "No_Address_For_Name",
         EAI_NODATA+EAI_Error_First);
#else
  GDFLT2("EAI_NODATA", "No_Address_For_Name");
#endif
#ifdef EAI_NONAME
  GCST2("EAI_NONAME", "Name_Not_Known",
         EAI_NONAME+EAI_Error_First);
#else
  GDFLT2("EAI_NONAME", "Name_Not_Known");
#endif
#ifdef EAI_SERVICE
  GCST2("EAI_SERVICE", "Service_Not_Supported",
         EAI_SERVICE+EAI_Error_First);
#else
  GDFLT2("EAI_SERVICE", "Service_Not_Supported");
#endif
#ifdef EAI_SOCKTYPE
  GCST2("EAI_SOCKTYPE", "Unknown_Socket_Type",
         EAI_SOCKTYPE+EAI_Error_First);
#else
  GDFLT2("EAI_SOCKTYPE", "Unknown_Socket_Type");
#endif

  EAI_Error_Last = max_GCST2;
  XTI_Error_First = 100000;      /* Start with a bias of 100000 */
  while (1) {

     if (XTI_Error_First > max_posix_error) {
        break;
     } else {
        XTI_Error_First = XTI_Error_First * 10;
     } /* end if */
  } /* end while */

  ifprintf(fp,"   subtype Addrinfo_Error_Code is Error_Code\n");
  ifprintf(fp,"      range %d .. %d;\n", EAI_Error_First, EAI_Error_Last);

  max_GCST2 = XTI_Error_First;

#ifdef TACCES
  GCST2("TACCES", "Insufficient_Permission", TACCES+XTI_Error_First);
#else
  GDFLT2("TACCES", "Insufficient_Permission");
#endif
#ifdef TADDRBUSY
  GCST2("TADDRBUSY", "XTI_Address_In_Use", TADDRBUSY+XTI_Error_First);
#else
  GDFLT2("TADDRBUSY", "XTI_Address_In_Use");
#endif
#ifdef TBADADDR
  GCST2("TBADADDR", "Incorrect_Address_Format", TBADADDR+XTI_Error_First);
#else
  GDFLT2("TBADADDR", "Incorrect_Address_Format");
#endif
#ifdef TBADDATA
  GCST2("TBADDATA", "Illegal_Data_Range", TBADDATA+XTI_Error_First);
#else
  GDFLT2("TBADDATA", "Illegal_Data_Range");
#endif
#ifdef TBADF
  GCST2("TBADF", "Invalid_File_Descriptor", TBADF+XTI_Error_First);
#else
  GDFLT2("TBADF", "Invalid_File_Descriptor");
#endif
#ifdef TBADFLAG
  GCST2("TBADFLAG", "Invalid_Flag", TBADFLAG+XTI_Error_First);
#else
  GDFLT2("TBADFLAG", "Invalid_Flag");
#endif
#ifdef TBADNAME
  GCST2("TBADNAME", "Invalid_Communications_Provider",
         TBADNAME+XTI_Error_First);
#else
  GDFLT2("TBADNAME", "Invalid_Communications_Provider");
#endif
#ifdef TBADOPT
  GCST2("TBADOPT", "Incorrect_Or_Illegal_Option", TBADOPT+XTI_Error_First);
#else
  GDFLT2("TBADOPT", "Incorrect_Or_Illegal_Option");
#endif
#ifdef TBADQLEN
  GCST2("TBADQLEN", "Endpoint_Queue_Length_Is_Zero", TBADQLEN+XTI_Error_First);
#else
  GDFLT2("TBADQLEN", "Endpoint_Queue_Length_Is_Zero");
#endif
#ifdef TBADSEQ
  GCST2("TBADSEQ", "Invalid_Sequence_Number", TBADSEQ+XTI_Error_First);
#else
  GDFLT2("TBADSEQ", "Invalid_Sequence_Number");
#endif
#ifdef TBUFOVFLW
  GCST2("TBUFOVFLW", "Buffer_Not_Large_Enough", TBUFOVFLW+XTI_Error_First);
#else
  GDFLT2("TBUFOVFLW", "Buffer_Not_Large_Enough");
#endif
#ifdef TFLOW
  GCST2("TFLOW", "Flow_Control_Error", TFLOW+XTI_Error_First);
#else
  GDFLT2("TFLOW", "Flow_Control_Error");
#endif
#ifdef TINDOUT
  GCST2("TINDOUT", "Outstanding_Connection_Indications",
         TINDOUT+XTI_Error_First);
#else
  GDFLT2("TINDOUT", "Outstanding_Connection_Indications");
#endif
#ifdef TLOOK
  GCST2("TLOOK", "Event_Requires_Attention", TLOOK+XTI_Error_First);
#else
  GDFLT2("TLOOK", "Event_Requires_Attention");
#endif
#ifdef TNOADDR
  GCST2("TNOADDR", "Could_Not_Allocate_Address", TNOADDR+XTI_Error_First);
#else
  GDFLT2("TNOADDR", "Could_Not_Allocate_Address");
#endif
#ifdef TNODATA
  GCST2("TNODATA", "No_Data_Available", TNODATA+XTI_Error_First);
#else
  GDFLT2("TNODATA", "No_Data_Available");
#endif
#ifdef TNODIS
  GCST2("TNODIS", "No_Disconnect_Indication_On_Endpoint",
         TNODIS+XTI_Error_First);
#else
  GDFLT2("TNODIS", "No_Disconnect_Indication_On_Endpoint");
#endif
#ifdef TPROVMISMATCH
  GCST2("TPROVMISMATCH", "Communications_Provider_Mismatch",
         TPROVMISMATCH+XTI_Error_First);
#else
  GDFLT2("TPROVMISMATCH", "Communications_Provider_Mismatch");
#endif
#ifdef TNOREL
  GCST2("TNOREL", "No_Orderly_Release_Indication_On_Endpoint",
         TNOREL+XTI_Error_First);
#else
  GDFLT2("TNOREL", "No_Orderly_Release_Indication_On_Endpoint");
#endif
#ifdef TNOSTRUCTYPE
  GCST2("TNOSTRUCTYPE", "Unsupported_Object_Type_Requested",
         TNOSTRUCTYPE+XTI_Error_First);
#else
  GDFLT2("TNOSTRUCTYPE", "Unsupported_Object_Type_Requested");
#endif
#ifdef TNOTSUPPORT
  GCST2("TNOTSUPPORT", "Function_Not_Supported", TNOTSUPPORT+XTI_Error_First);
#else
  GDFLT2("TNOTSUPPORT", "Function_Not_Supported");
#endif
#ifdef TNOUDERR
  GCST2("TNOUDERR", "No_Unitdata_Error_On_Endpoint",
         TNOUDERR+XTI_Error_First);
#else
  GDFLT2("TNOUDERR", "No_Unitdata_Error_On_Endpoint");
#endif
#ifdef TOUTSTATE
  GCST2("TOUTSTATE", "Function_Not_Valid_For_State",
         TOUTSTATE+XTI_Error_First);
#else
  GDFLT2("TOUTSTATE", "Function_Not_Valid_For_State");
#endif
#ifdef TPROTO
  GCST2("TPROTO", "Protocol_Error", TPROTO+XTI_Error_First);
#else
  GDFLT2("TPROTO", "Protocol_Error");
#endif
#ifdef TQFULL
  GCST2("TQFULL", "Endpoint_Queue_Full", TQFULL+XTI_Error_First);
#else
  GDFLT2("TQFULL", "Endpoint_Queue_Full");
#endif
#ifdef TSTATECHNG
  GCST2("TSTATECHNG", "State_Change_In_Progress",
         TSTATECHNG+XTI_Error_First);
#else
  GDFLT2("TSTATECHNG", "State_Change_In_Progress");
#endif
#ifdef TRESADDR
  GCST2("TRESADDR", "Surrogate_File_Descriptor_Mismatch",
         TRESADDR+XTI_Error_First);
#else
  GDFLT2("TRESADDR", "Surrogate_File_Descriptor_Mismatch");
#endif
#ifdef TRESQLEN
  GCST2("TRESQLEN", "Incorrect_Surrogate_Queue_Length",
         TRESQLEN+XTI_Error_First);
#else
  GDFLT2("TRESQLEN", "Incorrect_Surrogate_Queue_Length");
#endif

  XTI_Error_Last = max_GCST2;

  ifprintf(fp,"   subtype XTI_Error_Code is Error_Code\n");
  ifprintf(fp,"      range %d .. %d;\n", XTI_Error_First, XTI_Error_Last);

  ifprintf(fp,"   --  System Identification\n");

  ifprintf(fp,"   function System_Name return POSIX_String;\n");
  ifprintf(fp,"   function Node_Name return POSIX_String;\n");
  ifprintf(fp,"   function Release return POSIX_String;\n");
  ifprintf(fp,"   function Version return POSIX_String;\n");
  ifprintf(fp,"   function Machine return POSIX_String;\n");

  ifprintf(fp,"   type Seconds is new Integer;\n");
  ifprintf(fp,"   type Minutes is new Integer;\n");
  ifprintf(fp,"   type Nanoseconds_Base is new Integer;\n");
  ifprintf(fp,"   subtype Nanoseconds   is ");
  ifprintf(fp,"Nanoseconds_Base range 0 .. (10**9) - 1;\n");
  ifprintf(fp,"   type Timespec         is private;\n");

  ifprintf(fp,"   function Get_Seconds (Time : Timespec) return Seconds;\n");
  ifprintf(fp,"   procedure Set_Seconds\n");
  ifprintf(fp,"      (Time : in out Timespec;\n");
  ifprintf(fp,"       S    : in Seconds);\n");
  ifprintf(fp,"   function Get_Nanoseconds (Time : Timespec) ");
  ifprintf(fp,"return Nanoseconds;\n");
  ifprintf(fp,"   procedure Set_Nanoseconds\n");
  ifprintf(fp,"      (Time : in out Timespec;\n");
  ifprintf(fp,"       NS   : in Nanoseconds);\n");
  ifprintf(fp,"   procedure Split\n");
  ifprintf(fp,"      (Time : in  Timespec;\n");
  ifprintf(fp,"       S    : out Seconds;\n");
  ifprintf(fp,"       NS   : out Nanoseconds);\n");
  ifprintf(fp,"   function To_Timespec\n");
  ifprintf(fp,"      (S  : Seconds;\n");
  ifprintf(fp,"       NS : Nanoseconds) return Timespec;\n");
  ifprintf(fp,"   function \"+\" (Left, Right : Timespec) return Timespec;\n");
  ifprintf(fp,"   function \"+\" (Left : Timespec; Right : Nanoseconds)\n");
  ifprintf(fp,"     return Timespec;\n");
  ifprintf(fp,"   function \"-\" (Right : Timespec) return Timespec;\n");
  ifprintf(fp,"   function \"-\" (Left, Right : Timespec) return Timespec;\n");
  ifprintf(fp,"   function \"-\" (Left : Timespec; Right : Nanoseconds)\n");
  ifprintf(fp,"      return Timespec;\n");
  ifprintf(fp,"   function \"*\" (Left : Timespec; Right : Integer)\n");
  ifprintf(fp,"      return Timespec;\n");
  ifprintf(fp,"   function \"*\" (Left : Integer; Right : Timespec)\n");
  ifprintf(fp,"      return Timespec;\n");
  ifprintf(fp,"   function \"/\" (Left : Timespec; Right : Integer)\n");
  ifprintf(fp,"      return Timespec;\n");
  ifprintf(fp,"   function \"/\" (Left, Right  : Timespec) return Integer;\n");
  ifprintf(fp,"   function \"<\" (Left, Right  : Timespec) return Boolean;\n");
  ifprintf(fp,"   function \"<=\" (Left, Right : Timespec) return Boolean;\n");
  ifprintf(fp,"   function \">\" (Left, Right  : Timespec) return Boolean;\n");
  ifprintf(fp,"   function \">=\" (Left, Right : Timespec) return Boolean;\n");
  ifprintf(fp,"   function To_Duration (Time : Timespec) return Duration;\n");
  ifprintf(fp,"   --  pragma Inline (To_Duration);\n");
  ifprintf(fp,"   function To_Timespec (D : Duration) return Timespec;\n");
  ifprintf(fp,"   --  pragma Inline (To_Timespec);\n");

  ghdrcmnt("Host-Network Byte Order Conversions");

  /* we need 32-bit and a 16-bit unsigned integer types
   */
  if (sizeof (unsigned int) == 4) {
    if (sizeof (unsigned short) == 2) {
      /* uint32_t -> unsigned int
         uint16_t -> unsigned int
       */
      union {
        unsigned int l;
        unsigned char c[4];
      } x;
      x.c[0] = 0; x.c[1] = 1; x.c[2] = 2; x.c[3] = 3;
      if (x.l == 0x00010203) {
         network_byte_order = 1;
         ifprintf(fp,"   Host_Byte_Order_Is_Net_Byte_Order"
           " : Boolean := True;\n\n");
      } else {
         network_byte_order = 0;
         ifprintf(fp,"   Host_Byte_Order_Is_Net_Byte_Order"
           " : Boolean := False;\n\n");
      }
    } else quit ("short is not 16-bit","");
  } else quit ("int is not 32-bit","");
  ifprintf(fp,"   function Host_To_Network_Byte_Order");
  ifprintf(fp," (Host_32 : Interfaces.Unsigned_32)\n");
  ifprintf(fp,"      return Interfaces.Unsigned_32;\n");

  ifprintf(fp,"   function Host_To_Network_Byte_Order");
  ifprintf(fp," (Host_16 : Interfaces.Unsigned_16)\n");
  ifprintf(fp,"      return Interfaces.Unsigned_16;\n");

  ifprintf(fp,"   function Network_To_Host_Byte_Order");
  ifprintf(fp," (Host_32 : Interfaces.Unsigned_32)\n");
  ifprintf(fp,"      return Interfaces.Unsigned_32;\n");

  ifprintf(fp,"   function Network_To_Host_Byte_Order");
  ifprintf(fp," (Host_16 : Interfaces.Unsigned_16)\n");
  ifprintf(fp,"      return Interfaces.Unsigned_16;\n");

  ifprintf(fp,"   XTI_Blocking_Behavior     : constant Blocking_Behavior\n");
  ifprintf(fp,"      := Tasks;\n");

  ifprintf(fp,"   Sockets_Blocking_Behavior     :"
    " constant Blocking_Behavior\n");
  ifprintf(fp,"      := Tasks;\n");

  ghdrcmnt("Octet declarations");

  ifprintf(fp,"   type Octet is mod 2 ** 8;\n");
  ifprintf(fp,"   type Octet_Array is\n");
  ifprintf(fp,"      array (Integer range <>) of aliased Octet;\n");
  ifprintf(fp,"   type Octet_Array_Pointer is access all Octet_Array;\n");
  ifprintf(fp,"private\n");

#ifdef VERSION
  ifprintf(fp,"   Florist_Version : constant String := \""VERSION"\";\n\n");
#endif

  ifprintf(fp,"   type String_List;\n");
  ifprintf(fp,"   --  See package body for comments on String_List.\n");
  ifprintf(fp,"   type POSIX_String_List is access all String_List;\n");
  ifprintf(fp,"   Empty_String_List : constant POSIX_String_List"
    " := null;\n\n");

  ifprintf(fp,"   type Timespec is record\n");
  ifprintf(fp,"      Val : Duration := 0.0;\n");
  ifprintf(fp,"   end record;\n");

  ifprintf(fp,"   --  The value is of type Duration because we can do more\n");
  ifprintf(fp,"   --  efficient arithmetic on that type ");
  ifprintf(fp,"than on a two-part C struct.\n");
  ifprintf(fp,"   --  We rely that GNAT implements type ");
  ifprintf(fp,"Duration with enough\n");
  ifprintf(fp,"   --  precision (64 bits) to hold a full C timespec value.\n");
  ifprintf(fp,"   --  The enclosing record is to permit ");
  ifprintf(fp,"implicit initialization.\n");

  guitp("Bits", sizeof(int));
  ifprintf(fp,"   --  Bits and the C int type are always the same size.\n");
  ifprintf(fp,"   --  We don't define int here,"
    " since we want to be able to\n");
  ifprintf(fp,"   --  use it in the visible parts of child packages.\n\n");
  ifprintf(fp,"   type Option_Set is\n");
  ifprintf(fp,"      record\n");
  ifprintf(fp,"         Option : Bits := 0;\n");
  ifprintf(fp,"      end record;\n");

  { int i;
    for (i=1; i<32; i++) {
      ifprintf(fp,"   Option_%d  : constant Option_Set"
        " := (Option => 2**%d);\n", i, i-1);
    }
  }

  ifprintf(fp,"end POSIX;\n");

  fclose (fp);
  fprintf(stderr,"done generating posix.ads\n");

}

/* create_c
   --------
   create package POSIX.C, in file posix-c.ads
 */
void create_c() {

  fprintf(stderr,"creating package POSIX.C\n");
  if (! (fp = fopen ("posix-c.ads", "w"))) {
    perror ("posix-c.ads");
    quit("can't open file to write","");
  }

  gheader("POSIX.C", FSU_Header);
  ifprintf(fp,"with System;\n");
  ifprintf(fp,"package POSIX.C is\n");
  ifprintf(fp,"   pragma Elaborate_Body;\n");

  ifprintf(fp,"   --  =========  --\n");
  ifprintf(fp,"   --   WARNING   --\n");
  ifprintf(fp,"   --  =========  --\n\n");
  ifprintf(fp,"   --  This package should NOT be used directly");
  ifprintf(fp," by an application.\n");
  ifprintf(fp,"   --  It is internal to the FLORIST implementation of the");
  ifprintf(fp," POSIX.5 API,\n");
  ifprintf(fp,"   --  and may be changed or replaced in future versions");
  ifprintf(fp," of FLORIST.\n\n");

  ifprintf(fp,"   ALIGNMENT : constant");
  ifprintf(fp," := Natural'Min (Standard'Maximum_Alignment, 8);\n");
  ifprintf(fp,"   --  worst-case alignment requirement\n");

  /* numeric types
     -------------
   */

  ghdrcmnt("basic C types");

  gsitp("short", sizeof(short));
  gsitp("int", sizeof(int));
  gptrtp("int","int");
  guitp("unsigned", sizeof(unsigned));
  gsitp("long", sizeof(long));
  guitp("unsigned_long", sizeof(unsigned long));
  guitp("unsigned_int", sizeof(unsigned int));
  guitp("unsigned_short", sizeof(unsigned short));
  guitp("caddr_t", sizeof(caddr_t));
  g_size_t();
  g_time_t();
  g_clock_t();
  gsitp("ptr_as_int", sizeof(char *));

  /* char * and char **
     ------------------
   */
  ifprintf(fp,"   subtype char is POSIX_Character;\n");
  gptrtp("char","char");
  gptrtp("char_ptr","char_ptr");
  ifprintf(fp,"   type char_ptr_array is\n");
  ifprintf(fp,"     array (Positive range <>) of aliased char_ptr;\n");

  ifprintf(fp,"   function malloc (size : in size_t) return char_ptr;\n");
  ifprintf(fp,"   function malloc (size : in size_t) return char_ptr_ptr;\n");
  ifprintf(fp,"      pragma Import (C, malloc, \"malloc\");\n");
  ifprintf(fp,"   procedure free (object : in char_ptr);\n");
  ifprintf(fp,"   procedure free (object : in char_ptr_ptr);\n");
  ifprintf(fp,"      pragma Import (C, free, \"free\");\n");
  ifprintf(fp,"   procedure Advance (Ptr : in out char_ptr);\n");
  ifprintf(fp,"   procedure Advance (Ptr : in out char_ptr_ptr);\n");
  ifprintf(fp,"   --  advance Ptr to next location\n");
  ifprintf(fp,"   --  pragma Inline (Advance);\n");
  ifprintf(fp,"   function Form_POSIX_String (Str : in char_ptr)\n");
  ifprintf(fp,"      return POSIX_String;\n");
  ifprintf(fp,"   --  makes new copy of string, without null terminator\n");

  /* constants
     ---------
   */

  ghdrcmnt("constants");

#ifdef AIO_ALLDONE
  GCST("AIO_ALLDONE", AIO_ALLDONE);
#else
  GDFLT("AIO_ALLDONE", 0);
#endif
#ifdef AIO_CANCELED
  GCST("AIO_CANCELED", AIO_CANCELED);
#else
  GDFLT("AIO_CANCELED", 0);
#endif
#ifdef AIO_NOTCANCELED
  GCST("AIO_NOTCANCELED", AIO_NOTCANCELED);
#else
  GDFLT("AIO_NOTCANCELED", 0);
#endif

#ifdef B0
  GCST("B0", B0);
#else
  GDFLT("B0", 0);
#endif
#ifdef B110
  GCST("B110", B110);
#else
  GDFLT("B110", 0);
#endif
#ifdef B1200
  GCST("B1200",B1200);
#else
  GDFLT("B1200", 0);
#endif
#ifdef B134
  GCST("B134", B134);
#else
  GDFLT("B134", 0);
#endif
#ifdef B150
  GCST("B150", B150);
#else
  GDFLT("B150", 0);
#endif
#ifdef B1800
  GCST("B1800", B1800);
#else
  GDFLT("B1800", 0);
#endif
#ifdef B19200
  GCST("B19200", B19200);
#else
  GDFLT("B19200", 0);
#endif
#ifdef B200
  GCST("B200", B200);
#else
  GDFLT("B200", 0);
#endif
#ifdef B2400
  GCST("B2400", B2400);
#else
  GDFLT("B2400", 0);
#endif
#ifdef B300
  GCST("B300", B300);
#else
  GDFLT("B300", 0);
#endif
#ifdef B38400
  GCST("B38400", B38400);
#else
  GDFLT("B38400", 0);
#endif
#ifdef B4800
  GCST("B4800", B4800);
#else
  GDFLT("B4800", 0);
#endif
#ifdef B50
  GCST("B50", B50);
#else
  GDFLT("B50", 0);
#endif
#ifdef B600
  GCST("B600", B600);
#else
  GDFLT("B600", 0);
#endif
#ifdef B75
  GCST("B75", B75);
#else
  GDFLT("B75", 0);
#endif
#ifdef B9600
  GCST("B9600", B9600);
#else
  GDFLT("B9600", 0);
#endif
#ifdef BRKINT
  GCST("BRKINT", BRKINT);
#else
  GDFLT("BRKINT", 0);
#endif

#ifdef CLK_TCK
  GCST("CLK_TCK", CLK_TCK);
#else
  GDFLT("CLK_TCK", 0);
#endif
#ifdef CLOCAL
  GCST("CLOCAL", CLOCAL);
#else
  GDFLT("CLOCAL", 0);
#endif
#ifdef CLOCK_REALTIME
  GCST("CLOCK_REALTIME", CLOCK_REALTIME);
#else
  GDFLT("CLOCK_REALTIME", 1);
#endif
#ifdef CREAD
  GCST("CREAD", CREAD);
#else
  GDFLT("CREAD", 0);
#endif
#ifdef CSIZE
  GCST("CSIZE", CSIZE);
#else
  GDFLT("CSIZE", 0);
#endif
#ifdef CSTOPB
  GCST("CSTOPB", CSTOPB);
#else
  GDFLT("CSTOPB", 0);
#endif
#ifdef CS5
  GCST("CS5", CS5);
#else
  GDFLT("CS5", 0);
#endif
#ifdef CS6
  GCST("CS6", CS6);
#else
  GDFLT("CS6", 0);
#endif
#ifdef CS7
  GCST("CS7", CS7);
#else
  GDFLT("CS7", 0);
#endif
#ifdef CS8
  GCST("CS8", CS8);
#else
  GDFLT("CS8", 0);
#endif

  ifprintf(fp,"   --   error code constants are in posix.ads\n");

#ifdef ECHO
  GCST("ECHO", ECHO);
#else
  GDFLT("ECHO", 0);
#endif
#ifdef ECHOE
  GCST("ECHOE", ECHOE);
#else
  GDFLT("ECHOE", 0);
#endif
#ifdef ECHOK
  GCST("ECHOK", ECHOK);
#else
  GDFLT("ECHOK", 0);
#endif
#ifdef ECHONL
  GCST("ECHONL", ECHONL);
#else
  GDFLT("ECHONL", 0);
#endif

#ifdef FD_CLOEXEC
  GCST("FD_CLOEXEC", FD_CLOEXEC);
#else
  GDFLT("FD_CLOEXEC", 0);
#endif
#ifdef F_DUPFD
  GCST("F_DUPFD", F_DUPFD);
#else
  GDFLT("F_DUPFD", 0);
#endif
#ifdef F_GETFD
  GCST("F_GETFD", F_GETFD);
#else
  GDFLT("F_GETFD", 0);
#endif
#ifdef F_GETFL
  GCST("F_GETFL", F_GETFL);
#else
  GDFLT("F_GETFL", 0);
#endif
#ifdef F_GETLK
  GCST("F_GETLK", F_GETLK);
#else
  GDFLT("F_GETLK", 0);
#endif
#ifdef F_OK
  GCST("F_OK", F_OK);
#else
  GDFLT("F_OK", 0);
#endif
#ifdef F_RDLCK
  GCST("F_RDLCK", F_RDLCK);
#else
  GDFLT("F_RDLCK", 0);
#endif
#ifdef F_SETFD
  GCST("F_SETFD", F_SETFD);
#else
  GDFLT("F_SETFD", 0);
#endif
#ifdef F_SETFL
  GCST("F_SETFL", F_SETFL);
#else
  GDFLT("F_SETFL", 0);
#endif
#ifdef F_SETLK
  GCST("F_SETLK", F_SETLK);
#else
  GDFLT("F_SETLK", 0);
#endif
#ifdef F_SETLKW
  GCST("F_SETLKW", F_SETLKW);
#else
  GDFLT("F_SETLKW", 0);
#endif
#ifdef F_UNLCK
  GCST("F_UNLCK", F_UNLCK);
#else
  GDFLT("F_UNLCK", 0);
#endif
#ifdef F_WRLCK
  GCST("F_WRLCK", F_WRLCK);
#else
  GDFLT("F_WRLCK", 0);
#endif

#ifdef HUPCL
  GCST("HUPCL", HUPCL);
#else
  GDFLT("HUPCL", 0);
#endif

#ifdef ICANON
  GCST("ICANON", ICANON);
#else
  GDFLT("ICANON", 0);
#endif
#ifdef ICRNL
  GCST("ICRNL", ICRNL);
#else
  GDFLT("ICRNL", 0);
#endif
#ifdef IEXTEN
  GUCST("IEXTEN", IEXTEN);
#else
  GDFLT("IEXTEN", 0);
#endif
#ifdef IGNBRK
  GCST("IGNBRK", IGNBRK);
#else
  GDFLT("IGNBRK", 0);
#endif
#ifdef IGNCR
  GCST("IGNCR", IGNCR);
#else
  GDFLT("IGNCR", 0);
#endif
#ifdef IGNPAR
  GCST("IGNPAR", IGNPAR);
#else
  GDFLT("IGNPAR", 0);
#endif
#ifdef INLCR
  GCST("INLCR", INLCR);
#else
  GDFLT("INLCR", 0);
#endif
#ifdef INPCK
  GCST("INPCK", INPCK);
#else
  GDFLT("INPCK", 0);
#endif
#ifdef ISIG
  GCST("ISIG", ISIG);
#else
  GDFLT("ISIG", 0);
#endif
#ifdef ISTRIP
  GCST("ISTRIP", ISTRIP);
#else
  GDFLT("ISTRIP", 0);
#endif
#ifdef IXOFF
  GCST("IXOFF", IXOFF);
#else
  GDFLT("IXOFF", 0);
#endif
#ifdef IXON
  GCST("IXON", IXON);
#else
  GDFLT("IXON", 0);
#endif

#ifdef L_ctermid
  GCST("L_ctermid", L_ctermid);
#else
  GDFLT("L_ctermid", 10);
#endif
#ifdef LIO_NOP
  GCST("LIO_NOP", LIO_NOP);
#else
  GDFLT("LIO_NOP", 0);
#endif
#ifdef LIO_NOWAIT
  GCST("LIO_NOWAIT", LIO_NOWAIT);
#else
  GDFLT("LIO_NOWAIT", 0);
#endif
#ifdef LIO_READ
  GCST("LIO_READ", LIO_READ);
#else
  GDFLT("LIO_READ", 0);
#endif
#ifdef LIO_WAIT
  GCST("LIO_WAIT", LIO_WAIT);
#else
  GDFLT("LIO_WAIT", 0);
#endif
#ifdef LIO_WRITE
  GCST("LIO_WRITE", LIO_WRITE);
#else
  GDFLT("LIO_WRITE", 0);
#endif

#ifdef MAP_FAILED
  GCST("MAP_FAILED", MAP_FAILED);
#else
  GDFLT("MAP_FAILED",-1);
#endif
  /* Linux wants MAP_FILE flag if we are memory-mapping
     a file.  We define it to be zero for other systems.
   */
#ifdef MAP_FILE
  GCST("MAP_FILE", MAP_FILE);
#else
  GDFLT("MAP_FILE", 0);
#endif
#ifdef MAP_FIXED
  GCST("MAP_FIXED", MAP_FIXED);
#else
  GDFLT("MAP_FIXED", 0);
#endif
#ifdef MAP_PRIVATE
  GCST("MAP_PRIVATE", MAP_PRIVATE);
#else
  GDFLT("MAP_PRIVATE", 0);
#endif
#ifdef MAP_SHARED
  GCST("MAP_SHARED", MAP_SHARED);
#else
  GDFLT("MAP_SHARED", 0);
#endif
#ifdef MAX_CANON
  GCST("MAX_CANON", MAX_CANON);
#else
  GDFLT("MAX_CANON", 0);
#endif
#ifdef MAX_INPUT
  GCST("MAX_INPUT", MAX_INPUT);
#else
  GDFLT("MAX_INPUT", 0);
#endif
#ifdef MCL_CURRENT
  GCST("MCL_CURRENT", MCL_CURRENT);
#else
  GDFLT("MCL_CURRENT", 0);
#endif
#ifdef MCL_FUTURE
  GCST("MCL_FUTURE", MCL_FUTURE);
#else
  GDFLT("MCL_FUTURE", 0);
#endif

#ifdef MS_ASYNC
  GCST("MS_ASYNC", MS_ASYNC);
#else
  GDFLT("MS_ASYNC", 0);
#endif
#ifdef MS_INVALIDATE
  GCST("MS_INVALIDATE", MS_INVALIDATE);
#else
  GDFLT("MS_INVALIDATE", 0);
#endif
#ifdef MS_SYNC
  GCST("MS_SYNC", MS_SYNC);
#else
  GDFLT("MS_SYNC", 0);
#endif

#ifdef NCCS
  GCST("NCCS", NCCS);
#else
  GDFLT("NCCS", 0);
#endif

#ifdef NOFLSH
  GUCST("NOFLSH", NOFLSH);
#else
  GDFLT("NOFLSH", 0);
#endif

#ifdef OPOST
  GCST("OPOST", OPOST);
#else
  GDFLT("OPOST", 0);
#endif
#ifdef O_ACCMODE
  GCST("O_ACCMODE", O_ACCMODE);
#else
  GDFLT("O_ACCMODE", 0);
#endif
#ifdef O_APPEND
  GCST("O_APPEND", O_APPEND);
#else
  GDFLT("O_APPEND", 0);
#endif
#ifdef O_CREAT
  GCST("O_CREAT", O_CREAT);
#else
  GDFLT("O_CREAT", 0);
#endif
#ifdef O_DSYNC
  GCST("O_DSYNC", O_DSYNC);
#else
  GDFLT("O_DSYNC", 0);
#endif
#ifdef O_EXCL
  GCST("O_EXCL", O_EXCL);
#else
  GDFLT("O_EXCL", 0);
#endif
#ifdef O_NOCTTY
  GCST("O_NOCTTY", O_NOCTTY);
#else
  GDFLT("O_NOCTTY", 0);
#endif
#ifdef O_NONBLOCK
  GCST("O_NONBLOCK", O_NONBLOCK);
#else
  GDFLT("O_NONBLOCK", 0);
#endif
#ifdef O_RDONLY
  GCST("O_RDONLY", O_RDONLY);
#else
  GDFLT("O_RDONLY", 0);
#endif
#ifdef O_RDWR
  GCST("O_RDWR", O_RDWR);
#else
  GDFLT("O_RDWR", 0);
#endif
#ifdef O_RSYNC
  GCST("O_RSYNC", O_RSYNC);
#else
  GDFLT("O_RSYNC", 0);
#endif
#ifdef O_SYNC
  GCST("O_SYNC", O_SYNC);
#else
  GDFLT("O_SYNC", 0);
#endif
#ifdef O_TRUNC
  GCST("O_TRUNC", O_TRUNC);
#else
  GDFLT("O_TRUNC", 0);
#endif
#ifdef O_WRONLY
  GCST("O_WRONLY", O_WRONLY);
#else
  GDFLT("O_WRONLY", 0);
#endif

#ifdef PAGESIZE
  GCST("PAGESIZE", PAGESIZE);
#else
#ifdef PAGE_SIZE
  GCST("PAGESIZE", PAGE_SIZE);
#else
  GDFLT("PAGESIZE", 0);
#endif
#endif
#ifdef PARENB
  GCST("PARENB", PARENB);
#else
  GDFLT("PARENB", 0);
#endif
#ifdef PARMRK
  GCST("PARMRK", PARMRK);
#else
  GDFLT("PARMRK", 0);
#endif
#ifdef PARODD
  GCST("PARODD", PARODD);
#else
  GDFLT("PARODD", 0);
#endif

#ifdef PROT_EXEC
  GCST("PROT_EXEC", PROT_EXEC);
#else
  GDFLT("PROT_EXEC", 0);
#endif
#ifdef PROT_NONE
  GCST("PROT_NONE", PROT_NONE);
#else
  GDFLT("PROT_NONE", 0);
#endif
#ifdef PROT_READ
  GCST("PROT_READ", PROT_READ);
#else
  GDFLT("PROT_READ", 0);
#endif
#ifdef PROT_WRITE
  GCST("PROT_WRITE", PROT_WRITE);
#else
  GDFLT("PROT_WRITE", 0);
#endif
#ifdef PTHREAD_DESTRUCTOR_ITERATIONS
  GCST("PTHREAD_DESTRUCTOR_ITERATIONS", PTHREAD_DESTRUCTOR_ITERATIONS);
#else
  GDFLT("PTHREAD_DESTRUCTOR_ITERATIONS", 0);
#endif
#ifdef PTHREAD_EXPLICIT_SCHED
  GCST("PTHREAD_EXPLICIT_SCHED", PTHREAD_EXPLICIT_SCHED);
#else
  GDFLT("PTHREAD_EXPLICIT_SCHED", 0);
#endif
#ifdef PTHREAD_INHERIT_SCHED
  GCST("PTHREAD_INHERIT_SCHED", PTHREAD_INHERIT_SCHED);
#else
  GDFLT("PTHREAD_INHERIT_SCHED", 0);
#endif

#ifdef PTHREAD_PRIO_INHERIT
  GCST("PTHREAD_PRIO_INHERIT", PTHREAD_PRIO_INHERIT);
#else
  GDFLT("PTHREAD_PRIO_INHERIT", 0);
#endif
#ifdef PTHREAD_PRIO_NONE
  GCST("PTHREAD_PRIO_NONE", PTHREAD_PRIO_NONE);
#else
  GDFLT("PTHREAD_PRIO_NONE", 0);
#endif
#ifdef PTHREAD_PRIO_PROTECT
  GCST("PTHREAD_PRIO_PROTECT", PTHREAD_PRIO_PROTECT);
#else
  GDFLT("PTHREAD_PRIO_PROTECT", 0);
#endif

#ifdef PTHREAD_PROCESS_SHARED
  GCST("PTHREAD_PROCESS_SHARED", PTHREAD_PROCESS_SHARED);
  GCST("PTHREAD_PROCESS_PRIVATE", PTHREAD_PROCESS_PRIVATE);
#else
  GDFLT("PTHREAD_PROCESS_SHARED", 1);
  GDFLT("PTHREAD_PROCESS_PRIVATE", 0);
#endif

#ifdef PTHREAD_SCOPE_PROCESS
  GCST("PTHREAD_SCOPE_PROCESS", PTHREAD_SCOPE_PROCESS);
#else
  GDFLT("PTHREAD_SCOPE_PROCESS", 0);
#endif
#ifdef PTHREAD_SCOPE_SYSTEM
  GCST("PTHREAD_SCOPE_SYSTEM", PTHREAD_SCOPE_SYSTEM);
#else
  GDFLT("PTHREAD_SCOPE_SYSTEM", 0);
#endif

#ifdef R_OK
  GCST("R_OK", R_OK);
#else
  GDFLT("R_OK", 0);
#endif

#ifdef SA_NOCLDSTOP
  GCST("SA_NOCLDSTOP", SA_NOCLDSTOP);
#else
  GDFLT("SA_NOCLDSTOP", 0);
#endif
#ifdef SA_SIGINFO
  GCST("SA_SIGINFO", SA_SIGINFO);
#else
  GDFLT("SA_SIGINFO", 0);
#endif
#ifdef SCHED_FIFO
  GCST("SCHED_FIFO", SCHED_FIFO);
#else
  GDFLT("SCHED_FIFO", 0);
#endif
#ifdef SCHED_OTHER
  GCST("SCHED_OTHER", SCHED_OTHER);
#else
  GDFLT("SCHED_OTHER", 0);
#endif
#ifdef SCHED_RR
  GCST("SCHED_RR", SCHED_RR);
#else
  GDFLT("SCHED_RR", 0);
#endif
#ifdef SEEK_CUR
  GCST("SEEK_CUR", SEEK_CUR);
#else
  GDFLT("SEEK_CUR", 0);
#endif
#ifdef SEEK_END
  GCST("SEEK_END", SEEK_END);
#else
  GDFLT("SEEK_END", 0);
#endif
#ifdef SEEK_SET
  GCST("SEEK_SET", SEEK_SET);
#else
  GDFLT("SEEK_SET", 0);
#endif

#ifdef SIGABRT
  GCST("SIGABRT", SIGABRT);
#else
  GDFLT("SIGABRT", 0);
#endif
#ifdef SIGALRM
  GCST("SIGALRM", SIGALRM);
#else
  GDFLT("SIGALRM", 0);
#endif
#ifdef SIGBUS
  GCST("SIGBUS", SIGBUS);
#else
  GDFLT("SIGBUS", 0);
#endif
#ifdef SIGCHLD
  GCST("SIGCHLD", SIGCHLD);
#else
  GDFLT("SIGCHLD", 0);
#endif
#ifdef SIGCONT
  GCST("SIGCONT", SIGCONT);
#else
  GDFLT("SIGCONT", 0);
#endif
#ifdef SIGEV_NONE
  GCST("SIGEV_NONE", SIGEV_NONE);
#else
  GDFLT("SIGEV_NONE", 100);
#endif
#ifdef SIGEV_SIGNAL
  GCST("SIGEV_SIGNAL", SIGEV_SIGNAL);
#else
  GDFLT("SIGEV_SIGNAL", 101);
#endif
#ifdef SIGEV_THREAD
  GCST("SIGEV_THREAD", SIGEV_THREAD);
#else
  GDFLT("SIGEV_THREAD", 102);
#endif
#ifdef SIGFPE
  GCST("SIGFPE", SIGFPE);
#else
  GDFLT("SIGFPE", 0);
#endif
#ifdef SIGHUP
  GCST("SIGHUP", SIGHUP);
#else
  GDFLT("SIGHUP", 0);
#endif
#ifdef SIGILL
  GCST("SIGILL", SIGILL);
#else
  GDFLT("SIGILL", 0);
#endif
#ifdef SIGINT
  GCST("SIGINT", SIGINT);
#else
  GDFLT("SIGINT", 0);
#endif
#ifdef SIGIO
  GCST("SIGIO", SIGIO);
#else
  GDFLT("SIGIO", 0);
#endif
#ifdef SIGKILL
  GCST("SIGKILL", SIGKILL);
#else
  GDFLT("SIGKILL", 0);
#endif
#ifdef SIGPIPE
  GCST("SIGPIPE", SIGPIPE);
#else
  GDFLT("SIGPIPE", 0);
#endif
#ifdef SIGQUIT
  GCST("SIGQUIT", SIGQUIT);
#else
  GDFLT("SIGQUIT", 0);
#endif
#ifdef SIGRTMAX
#ifdef SIGRTMIN
  if ((SIGRTMAX >= 0) && (SIGRTMIN >= 0)) {
  GCST("SIGRTMAX", SIGRTMAX);
  GCST("SIGRTMIN", SIGRTMIN);
  } else {
  GDFLT("SIGRTMAX", 0);
  GDFLT("SIGRTMIN", 1);
  }
#else
  GDFLT("SIGRTMAX", 0);
  GDFLT("SIGRTMIN", 1);
#endif
#else
  GDFLT("SIGRTMAX", 0);
  GDFLT("SIGRTMIN", 1);
#endif

/* Try to find out the range of valid signals.
   We have not yet discovered a portable C way of doing this.
   We assume the range starts at 0 and is continuous up to some
   limit.
   We need this becuase we want to represent sets of signals as
   Boolean arrays.
   We considered using sigset_t directly, and would have liked
   to do so, but had two problems:
   (1) sigset_t apparently allows the use of dynamically allocated memory
   (2) we could not figure out how to check for signal validity;
       in particular, we needed a way to check for whether a given signal
       is reserved by the Ada runtime system.
 */

{sigset_t set;
  int sig;
  int result;
  int last_good = -1;
  int first_bad = -1;
  sigfillset (&set);
  for (sig = 0; sig < 1024; sig++) {
    result = sigismember (&set, sig);
    if (result == 1) last_good = sig;
    else if ((result == -1) && (first_bad = -1)) first_bad = sig;
  }
  if (last_good == 1023)
    printf("c-posix: WARNING: signal range estimate probably too small\n");
  if (first_bad < last_good) {
    printf("c-posix: WARNING: signal range estimate may be invalid\n");
    last_good = first_bad - 1;
  }
  GCST("NSIGS", last_good);
}

#ifdef SIGSEGV
  GCST("SIGSEGV", SIGSEGV);
#else
  GDFLT("SIGSEGV", 0);
#endif
#ifdef SIGSTOP
  GCST("SIGSTOP", SIGSTOP);
#else
  GDFLT("SIGSTOP", 0);
#endif
#ifdef SIGTERM
  GCST("SIGTERM", SIGTERM);
#else
  GDFLT("SIGTERM", 0);
#endif
#ifdef SIGTSTP
  GCST("SIGTSTP", SIGTSTP);
#else
  GDFLT("SIGTSTP", 0);
#endif
#ifdef SIGTTIN
  GCST("SIGTTIN", SIGTTIN);
#else
  GDFLT("SIGTTIN", 0);
#endif
#ifdef SIGTTOU
  GCST("SIGTTOU", SIGTTOU);
#else
  GDFLT("SIGTTOU", 0);
#endif
#ifdef SIGURG
  GCST("SIGURG", SIGURG);
#else
  GDFLT("SIGURG", 0);
#endif
#ifdef SIGUSR1
  GCST("SIGUSR1", SIGUSR1);
#else
  GDFLT("SIGUSR1", 0);
#endif
#ifdef SIGUSR2
  GCST("SIGUSR2", SIGUSR2);
#else
  GDFLT("SIGUSR2", 0);
#endif
#ifdef SIG_BLOCK
  GCST("SIG_BLOCK", SIG_BLOCK);
#else
  GDFLT("SIG_BLOCK", 0);
#endif
  /* .... hope that nobody is really using a pointer
     or a nontrivial macro for SIG_IGN or SIG_DFL
   */
#ifdef SIG_DFL
  GCST("SIG_DFL", (int) SIG_DFL);
#else
  GDFLT("SIG_DFL", 0);
#endif
#ifdef SIG_IGN
  GCST("SIG_IGN", (int) SIG_IGN);
#else
  GDFLT("SIG_IGN", 0);
#endif
#ifdef SIG_SETMASK
  GCST("SIG_SETMASK", SIG_SETMASK);
#else
  GDFLT("SIG_SETMASK", 0);
#endif
#ifdef SIG_UNBLOCK
  GCST("SIG_UNBLOCK", SIG_UNBLOCK);
#else
  GDFLT("SIG_UNBLOCK", 0);
#endif
#ifdef SI_ASYNCIO
  GCST("SI_ASYNCIO", SI_ASYNCIO);
#else
  GDFLT("SI_ASYNCIO", 101);
#endif
#ifdef SI_MESGQ
  GCST("SI_MESGQ", SI_MESGQ);
#else
  GDFLT("SI_MESGQ", 102);
#endif
#ifdef SI_QUEUE
  GCST("SI_QUEUE", SI_QUEUE);
#else
  GDFLT("SI_QUEUE", 103);
#endif
#ifdef SI_TIMER
  GCST("SI_TIMER", SI_TIMER);
#else
  GDFLT("SI_TIMER", 104);
#endif
#ifdef SI_USER
  GCST("SI_USER", SI_USER);
#else
  GDFLT("SI_USER", 105);
#endif

#ifdef S_IFSOCK
  GCST("S_IFSOCK", S_IFSOCK);
#else
  GDFLT("S_IFSOCK", 0);
#endif
#ifdef S_IRGRP
  GCST("S_IRGRP", S_IRGRP);
#else
  GDFLT("S_IRGRP", 0);
#endif
#ifdef S_IROTH
  GCST("S_IROTH", S_IROTH);
#else
  GDFLT("S_IROTH", 0);
#endif
#ifdef S_IRUSR
  GCST("S_IRUSR", S_IRUSR);
#else
  GDFLT("S_IRUSR", 0);
#endif
#ifdef S_IRWXG
  GCST("S_IRWXG", S_IRWXG);
#else
  GDFLT("S_IRWXG", 0);
#endif
#ifdef S_IRWXO
  GCST("S_IRWXO", S_IRWXO);
#else
  GDFLT("S_IRWXO", 0);
#endif
#ifdef S_IRWXU
  GCST("S_IRWXU", S_IRWXU);
#else
  GDFLT("S_IRWXU", 0);
#endif
#ifdef S_ISGID
  GCST("S_ISGID", S_ISGID);
#else
  GDFLT("S_ISGID", 0);
#endif
#ifdef S_ISUID
  GCST("S_ISUID", S_ISUID);
#else
  GDFLT("S_ISUID", 0);
#endif
#ifdef S_IWGRP
  GCST("S_IWGRP", S_IWGRP);
#else
  GDFLT("S_IWGRP", 0);
#endif
#ifdef S_IWOTH
  GCST("S_IWOTH", S_IWOTH);
#else
  GDFLT("S_IWOTH", 0);
#endif
#ifdef S_IWUSR
  GCST("S_IWUSR", S_IWUSR);
#else
  GDFLT("S_IWUSR", 0);
#endif
#ifdef S_IXGRP
  GCST("S_IXGRP", S_IXGRP);
#else
  GDFLT("S_IXGRP", 0);
#endif
#ifdef S_IXOTH
  GCST("S_IXOTH", S_IXOTH);
#else
  GDFLT("S_IXOTH", 0);
#endif
#ifdef S_IXUSR
  GCST("S_IXUSR", S_IXUSR);
#else
  GDFLT("S_IXUSR", 0);
#endif

#ifdef TCIFLUSH
  GCST("TCIFLUSH", TCIFLUSH);
#else
  GDFLT("TCIFLUSH", 0);
#endif
#ifdef TCIOFF
  GCST("TCIOFF", TCIOFF);
#else
  GDFLT("TCIOFF", 0);
#endif
#ifdef TCIOFLUSH
  GCST("TCIOFLUSH", TCIOFLUSH);
#else
  GDFLT("TCIOFLUSH", 0);
#endif
#ifdef TCION
  GCST("TCION", TCION);
#else
  GDFLT("TCION", 0);
#endif
#ifdef TCOFLUSH
  GCST("TCOFLUSH", TCOFLUSH);
#else
  GDFLT("TCOFLUSH", 0);
#endif
#ifdef TCOOFF
  GCST("TCOOFF", TCOOFF);
#else
  GDFLT("TCOOFF", 0);
#endif
#ifdef TCOON
  GCST("TCOON", TCOON);
#else
  GDFLT("TCOON", 0);
#endif
#ifdef TCSADRAIN
  GCST("TCSADRAIN", TCSADRAIN);
#else
  GDFLT("TCSADRAIN", 0);
#endif
#ifdef TCSAFLUSH
  GCST("TCSAFLUSH", TCSAFLUSH);
#else
  GDFLT("TCSAFLUSH", 0);
#endif
#ifdef TCSANOW
  GCST("TCSANOW", TCSANOW);
#else
  GDFLT("TCSANOW", 0);
#endif
#ifdef TIMER_ABSTIME
  GCST("TIMER_ABSTIME", TIMER_ABSTIME);
#else
  GDFLT("TIMER_ABSTIME", 1);
#endif

/* TIMER_RELTIME is not defined by the POSIX.1b standard,
   but seems to be used by at least Solaris; letting the value
   default to 0 if undefined gives us the same effect as the
   POSIX.1c specification.
 */
#ifdef TIMER_RELTIME
  GCST("TIMER_RELTIME", TIMER_RELTIME);
#else
  GDFLT("TIMER_RELTIME", 0);
#endif

#ifdef TOSTOP
  GCST("TOSTOP", TOSTOP);
#else
  GDFLT("TOSTOP", 0);
#endif

#ifdef VEOF
  GCST("VEOF", VEOF);
#else
  GDFLT("VEOF", 0);
#endif
#ifdef VEOL
  GCST("VEOL", VEOL);
#else
  GDFLT("VEOL", 0);
#endif
#ifdef VERASE
  GCST("VERASE", VERASE);
#else
  GDFLT("VERASE", 0);
#endif
#ifdef VINTR
  GCST("VINTR", VINTR);
#else
  GDFLT("VINTR", 0);
#endif
#ifdef VKILL
  GCST("VKILL", VKILL);
#else
  GDFLT("VKILL", 0);
#endif
#ifdef VMIN
  GCST("VMIN", VMIN);
#else
  GDFLT("VMIN", 0);
#endif
#ifdef VQUIT
  GCST("VQUIT", VQUIT);
#else
  GDFLT("VQUIT", 0);
#endif
#ifdef VSTART
  GCST("VSTART", VSTART);
#else
  GDFLT("VSTART", 0);
#endif
#ifdef VSTOP
  GCST("VSTOP", VSTOP);
#else
  GDFLT("VSTOP", 0);
#endif
#ifdef VSUSP
  GCST("VSUSP", VSUSP);
#else
  GDFLT("VSUSP", 0);
#endif
#ifdef VTIME
  GCST("VTIME", VTIME);
#else
  GDFLT("VTIME", 0);
#endif

#ifdef WNOHANG
  GCST("WNOHANG", WNOHANG);
#else
  GDFLT("WNOHANG", 0);
#endif

#ifdef WUNTRACED
  GCST("WUNTRACED", WUNTRACED);
#else
  GDFLT("WUNTRACED", 0);
#endif
#ifdef W_OK
  GCST("W_OK", W_OK);
#else
  GDFLT("W_OK", 0);
#endif

#ifdef X_OK
  GCST("X_OK", X_OK);
#else
  GDFLT("X_OK", 0);
#endif

#ifdef _PC_ASYNC_IO
  GCST("PC_ASYNC_IO", _PC_ASYNC_IO);
#else
  GDFLT("PC_ASYNC_IO", 0);
#endif
#ifdef _PC_CHOWN_RESTRICTED
  GCST("PC_CHOWN_RESTRICTED", _PC_CHOWN_RESTRICTED);
#else
  GDFLT("PC_CHOWN_RESTRICTED", 0);
#endif
#ifdef _PC_LINK_MAX
  GCST("PC_LINK_MAX", _PC_LINK_MAX);
#else
  GDFLT("PC_LINK_MAX", 0);
#endif
#ifdef _PC_MAX_CANON
  GCST("PC_MAX_CANON", _PC_MAX_CANON);
#else
  GDFLT("PC_MAX_CANON", 0);
#endif
#ifdef _PC_MAX_INPUT
  GCST("PC_MAX_INPUT", _PC_MAX_INPUT);
#else
  GDFLT("PC_MAX_INPUT", 0);
#endif
#ifdef _PC_NAME_MAX
  GCST("PC_NAME_MAX", _PC_NAME_MAX);
#else
  GDFLT("PC_NAME_MAX", 0);
#endif
#ifdef _PC_NO_TRUNC
  GCST("PC_NO_TRUNC", _PC_NO_TRUNC);
#else
  GDFLT("PC_NO_TRUNC", 0);
#endif
#ifdef _PC_PATH_MAX
  GCST("PC_PATH_MAX", _PC_PATH_MAX);
#else
  GDFLT("PC_PATH_MAX", 0);
#endif
#ifdef _PC_PIPE_BUF
  GCST("PC_PIPE_BUF", _PC_PIPE_BUF);
#else
  GDFLT("PC_PIPE_BUF", 0);
#endif
#ifdef _PC_PRIO_IO
  GCST("PC_PRIO_IO", _PC_PRIO_IO);
#else
  GDFLT("PC_PRIO_IO", 0);
#endif
#ifdef _PC_SYNC_IO
  GCST("PC_SYNC_IO", _PC_SYNC_IO);
#else
  GDFLT("PC_SYNC_IO", 0);
#endif
#ifdef _PC_SOCK_MAXBUF
  GCST("PC_SOCK_MAXBUF", _PC_SOCK_MAXBUF);
#else
  GDFLT("PC_SOCK_MAXBUF", 0);
#endif

#ifdef _SC_AIO_LISTIO_MAX
  GCST("SC_AIO_LISTIO_MAX", _SC_AIO_LISTIO_MAX);
#else
  GDFLT("SC_AIO_LISTIO_MAX", 0);
#endif
#ifdef _SC_AIO_MAX
  GCST("SC_AIO_MAX", _SC_AIO_MAX);
#else
  GDFLT("SC_AIO_MAX", 0);
#endif
#ifdef _SC_AIO_PRIO_DELTA_MAX
  GCST("SC_AIO_PRIO_DELTA_MAX", _SC_AIO_PRIO_DELTA_MAX);
#else
  GDFLT("SC_AIO_PRIO_DELTA_MAX", 0);
#endif
#ifdef _SC_ARG_MAX
  GCST("SC_ARG_MAX", _SC_ARG_MAX);
#else
  GDFLT("SC_ARG_MAX", 0);
#endif
#ifdef _SC_ASYNCHRONOUS_IO
  GCST("SC_ASYNCHRONOUS_IO", _SC_ASYNCHRONOUS_IO);
#else
  GDFLT("SC_ASYNCHRONOUS_IO", 0);
#endif
#ifdef _SC_CHILD_MAX
  GCST("SC_CHILD_MAX", _SC_CHILD_MAX);
#else
  GDFLT("SC_CHILD_MAX", 0);
#endif
#ifdef _SC_CLK_TCK
  GCST("SC_CLK_TCK", _SC_CLK_TCK);
#else
  GDFLT("SC_CLK_TCK", 0);
#endif
#ifdef _SC_DELAYTIMER_MAX
  GCST("SC_DELAYTIMER_MAX", _SC_DELAYTIMER_MAX);
#else
  GDFLT("SC_DELAYTIMER_MAX", 0);
#endif
#ifdef _SC_FSYNC
  GCST("SC_FSYNC", _SC_FSYNC);
#else
  GDFLT("SC_FSYNC", 0);
#endif
#ifdef _SC_JOB_CONTROL
  GCST("SC_JOB_CONTROL", _SC_JOB_CONTROL);
#else
  GDFLT("SC_JOB_CONTROL", 0);
#endif
#ifdef _SC_MAPPED_FILES
  GCST("SC_MAPPED_FILES", _SC_MAPPED_FILES);
#else
  GDFLT("SC_MAPPED_FILES", 0);
#endif
#ifdef _SC_MEMLOCK
  GCST("SC_MEMLOCK", _SC_MEMLOCK);
#else
  GDFLT("SC_MEMLOCK", 0);
#endif
#ifdef _SC_MEMLOCK_RANGE
  GCST("SC_MEMLOCK_RANGE", _SC_MEMLOCK_RANGE);
#else
  GDFLT("SC_MEMLOCK_RANGE", 0);
#endif
#ifdef _SC_MEMORY_PROTECTION
  GCST("SC_MEMORY_PROTECTION", _SC_MEMORY_PROTECTION);
#else
  GDFLT("SC_MEMORY_PROTECTION", 0);
#endif
#ifdef _SC_MESSAGE_PASSING
  GCST("SC_MESSAGE_PASSING", _SC_MESSAGE_PASSING);
#else
  GDFLT("SC_MESSAGE_PASSING", 0);
#endif
#ifdef _SC_MQ_OPEN_MAX
  GCST("SC_MQ_OPEN_MAX", _SC_MQ_OPEN_MAX);
#else
  GDFLT("SC_MQ_OPEN_MAX", 0);
#endif
#ifdef _SC_MQ_PRIO_MAX
  GCST("SC_MQ_PRIO_MAX", _SC_MQ_PRIO_MAX);
#else
  GDFLT("SC_MQ_PRIO_MAX", 0);
#endif
#ifdef _SC_NGROUPS_MAX
  GCST("SC_NGROUPS_MAX", _SC_NGROUPS_MAX);
#else
  GDFLT("SC_NGROUPS_MAX", 0);
#endif
#ifdef _SC_OPEN_MAX
  GCST("SC_OPEN_MAX", _SC_OPEN_MAX);
#else
  GDFLT("SC_OPEN_MAX", 0);
#endif
#ifdef _SC_PAGESIZE
  GCST("SC_PAGESIZE", _SC_PAGESIZE);
#else
  GDFLT("SC_PAGESIZE", 0);
#endif
#ifdef _SC_PII
  GCST("SC_PII", _SC_PII);
#else
  GDFLT("SC_PII", 0);
#endif
#ifdef _SC_PII_XTI
  GCST("SC_PII_XTI", _SC_PII_XTI);
#else
  GDFLT("SC_PII_XTI", 0);
#endif
#ifdef _SC_PII_SOCKET
  GCST("SC_PII_SOCKET", _SC_PII_SOCKET);
#else
  GDFLT("SC_PII_SOCKET", 0);
#endif
#ifdef _SC_PII_INTERNET
  GCST("SC_PII_INTERNET", _SC_PII_INTERNET);
#else
  GDFLT("SC_PII_INTERNET", 0);
#endif
#ifdef _SC_PII_INTERNET_STREAM
  GCST("SC_PII_INTERNET_STREAM", _SC_PII_INTERNET_STREAM);
#else
  GDFLT("SC_PII_INTERNET_STREAM", 0);
#endif
#ifdef _SC_PII_INTERNET_DGRAM
  GCST("SC_PII_INTERNET_DGRAM", _SC_PII_INTERNET_DGRAM);
#else
  GDFLT("SC_PII_INTERNET_DGRAM", 0);
#endif
#ifdef _SC_PII_OSI
  GCST("SC_PII_OSI", _SC_PII_OSI);
#else
  GDFLT("SC_PII_OSI", 0);
#endif
#ifdef _SC_PII_OSI_M
  GCST("SC_PII_OSI_M", _SC_PII_OSI_M);
#else
  GDFLT("SC_PII_OSI_M", 0);
#endif
#ifdef _SC_PII_OSI_COTS
  GCST("SC_PII_OSI_COTS", _SC_PII_OSI_COTS);
#else
  GDFLT("SC_PII_OSI_COTS", 0);
#endif
#ifdef _SC_PII_OSI_CLTS
  GCST("SC_PII_OSI_CLTS", _SC_PII_OSI_CLTS);
#else
  GDFLT("SC_PII_OSI_CLTS", 0);
#endif
#ifdef _SC_PII_NET_SUPPORT
  GCST("SC_PII_NET_SUPPORT", _SC_PII_NET_SUPPORT);
#else
  GDFLT("SC_PII_NET_SUPPORT", 0);
#endif
#ifdef _SC_POLL
  GCST("SC_POLL", _SC_POLL);
#else
  GDFLT("SC_POLL", 0);
#endif
#ifdef _SC_POSIX_PII_NET_SUPPORT
  GCST("SC_POSIX_PII_NET_SUPPORT", _SC_POSIX_PII_NET_SUPPORT);
#else
  GDFLT("SC_POSIX_PII_NET_SUPPORT", 0);
#endif
#ifdef _SC_PRIORITIZED_IO
  GCST("SC_PRIORITIZED_IO", _SC_PRIORITIZED_IO);
#else
  GDFLT("SC_PRIORITIZED_IO", 0);
#endif
#ifdef _SC_PRIORITY_SCHEDULING
  GCST("SC_PRIORITY_SCHEDULING", _SC_PRIORITY_SCHEDULING);
#else
  GDFLT("SC_PRIORITY_SCHEDULING", 0);
#endif
#ifdef _SC_SELECT
  GCST("SC_SELECT", _SC_SELECT);
#else
  GDFLT("SC_SELECT", 0);
#endif
#ifdef _SC_THREAD_PROCESS_SHARED
  GCST("SC_THREAD_PROCESS_SHARED", _SC_THREAD_PROCESS_SHARED);
#else
  GDFLT("SC_THREAD_PROCESS_SHARED", 0);
#endif
#ifdef _SC_REALTIME_SIGNALS
  GCST("SC_REALTIME_SIGNALS", _SC_REALTIME_SIGNALS);
#else
  GDFLT("SC_REALTIME_SIGNALS", 0);
#endif
#ifdef _SC_RTSIG_MAX
  GCST("SC_RTSIG_MAX", _SC_RTSIG_MAX);
#else
  GDFLT("SC_RTSIG_MAX", 0);
#endif
#ifdef _SC_SAVED_IDS
  GCST("SC_SAVED_IDS", _SC_SAVED_IDS);
#else
  GDFLT("SC_SAVED_IDS", 0);
#endif
#ifdef _SC_SEMAPHORES
  GCST("SC_SEMAPHORES", _SC_SEMAPHORES);
#else
  GDFLT("SC_SEMAPHORES", 0);
#endif
#ifdef _SC_SEM_NSEMS_MAX
  GCST("SC_SEM_NSEMS_MAX", _SC_SEM_NSEMS_MAX);
#else
  GDFLT("SC_SEM_NSEMS_MAX", 0);
#endif
#ifdef _SC_SEM_VALUE_MAX
  GCST("SC_SEM_VALUE_MAX", _SC_SEM_VALUE_MAX);
#else
  GDFLT("SC_SEM_VALUE_MAX", 0);
#endif
#ifdef _SC_SHARED_MEMORY_OBJECTS
  GCST("SC_SHARED_MEMORY_OBJECTS", _SC_SHARED_MEMORY_OBJECTS);
#else
  GDFLT("SC_SHARED_MEMORY_OBJECTS", 0);
#endif
#ifdef _SC_SIGQUEUE_MAX
  GCST("SC_SIGQUEUE_MAX", _SC_SIGQUEUE_MAX);
#else
  GDFLT("SC_SIGQUEUE_MAX", 0);
#endif
#ifdef _SC_STREAM_MAX
  GCST("SC_STREAM_MAX", _SC_STREAM_MAX);
#else
  GDFLT("SC_STREAM_MAX", 0);
#endif
#ifdef _SC_SYNCHRONIZED_IO
  GCST("SC_SYNCHRONIZED_IO", _SC_SYNCHRONIZED_IO);
#else
  GDFLT("SC_SYNCHRONIZED_IO", 0);
#endif
#ifdef _SC_THREAD_PRIORITY_SCHEDULING
  GCST("SC_THREAD_PRIORITY_SCHEDULING", _SC_THREAD_PRIORITY_SCHEDULING);
#else
  GDFLT("SC_THREAD_PRIORITY_SCHEDULING", 0);
#endif
#ifdef _SC_THREAD_PRIO_INHERIT
  GCST("SC_THREAD_PRIO_INHERIT", _SC_THREAD_PRIO_INHERIT);
#else
  GDFLT("SC_THREAD_PRIO_INHERIT", 0);
#endif
#ifdef _SC_THREAD_PRIO_PROTECT
  GCST("SC_THREAD_PRIO_PROTECT", _SC_THREAD_PRIO_PROTECT);
#else
  GDFLT("SC_THREAD_PRIO_PROTECT", 0);
#endif
#ifdef _SC_TIMERS
  GCST("SC_TIMERS", _SC_TIMERS);
#else
  GDFLT("SC_TIMERS", 0);
#endif
#ifdef _SC_TIMER_MAX
  GCST("SC_TIMER_MAX", _SC_TIMER_MAX);
#else
  GDFLT("SC_TIMER_MAX", 0);
#endif
#ifdef _SC_TZNAME_MAX
  GCST("SC_TZNAME_MAX", _SC_TZNAME_MAX);
#else
  GDFLT("SC_TZNAME_MAX", 0);
#endif
#ifdef _SC_T_IOV_MAX
  GCST("SC_T_IOV_MAX", _SC_T_IOV_MAX);
#else
  GDFLT("SC_T_IOV_MAX", 0);
#endif
#ifdef _SC_UIO_MAXIOV
  GCST("SC_UIO_MAXIOV", _SC_UIO_MAXIOV);
#else
  GDFLT("SC_UIO_MAXIOV", 0);
#endif
#ifdef _SC_VERSION
  GCST("SC_VERSION", _SC_VERSION);
#else
  GDFLT("SC_VERSION", 0);
#endif

/* type declarations
   -----------------
   If you make any changes here, also make changes
   where procedures are declared, above. (***)
 */

  ghdrcmnt("type definitions");
  g_off_t();
  g_pid_t();
  g_gid_t();
  g_uid_t();
  g_mode_t();
  g_suseconds_t();
  g_ssize_t();
  g_DIR();
  g_ino_t();
  g_dev_t();
  g_cc_t();
  g_nlink_t();
  g_tcflag_t();
  g_clockid_t();
  g_mqd_t();
  g_pthread_attr_t();
  g_pthread_cond_t();
  g_pthread_condattr_t();
  g_pthread_key_t();
  g_pthread_mutex_t();
  g_pthread_mutexattr_t();
  g_pthread_once_t();
  g_pthread_t();
  g_sem_t();
  g_sigset_t();
  g_speed_t();
  g_timer_t();
  g_sigval(); /* must precede siginfo_t and struct_sigevent */
  g_siginfo_t(); /* is typedef of a struct type */

  ghdrcmnt("structure types");
  g_struct_sigevent(); /* must precede aiocb */
  g_struct_aiocb();
  g_struct_dirent();
  g_struct_flock();
  g_struct_group();
  g_struct_mq_attr();
  g_struct_passwd();
  g_struct_sigaction();
  g_struct_sched_param();
  ifprintf(fp,"   type cc_t_array is array (0 .. NCCS - 1) of cc_t;\n");
  g_struct_stat();
  g_struct_termios();
  gcmnt("timeval structure");
  g_struct_timeval();
  g_struct_timespec();
  g_struct_itimerspec();
  g_struct_tm();
  g_struct_tms();
  g_struct_utimbuf();
  { struct utsname DUMMY;
    ifprintf(fp,"   subtype utsname_sysname_string is\n"
"      POSIX_String (1 .. %d);\n", sizeof (DUMMY.sysname));
    ifprintf(fp,"   subtype utsname_nodename_string is\n"
"      POSIX_String (1 .. %d);\n", sizeof (DUMMY.nodename));
    ifprintf(fp,"   subtype utsname_release_string is\n"
"      POSIX_String (1 .. %d);\n", sizeof (DUMMY.release));
    ifprintf(fp,"   subtype utsname_version_string is\n"
"      POSIX_String (1 .. %d);\n", sizeof (DUMMY.version));
    ifprintf(fp,"   subtype utsname_machine_string is\n"
"      POSIX_String (1 .. %d);\n", sizeof (DUMMY.machine));
  }
  g_struct_utsname();

/* ......need to figure what to do with functions, next..... */

  ghdrcmnt("link names for C functions");

  GFUNC(access,HAVE_access);
  GFUNC(aio_cancel,HAVE_aio_cancel);
  GFUNC(aio_error,HAVE_aio_error);
  GFUNC(aio_fsync,HAVE_aio_fsync);
  GFUNC(aio_read,HAVE_aio_read);
  GFUNC(aio_return,HAVE_aio_return);
  GFUNC(aio_suspend,HAVE_aio_suspend);
  GFUNC(aio_write,HAVE_aio_write);
  /*  GFUNC(alarm,HAVE_alarm); */
  /*  GFUNC(asctime,HAVE_asctime); */
  GFUNC(cfgetispeed,HAVE_cfgetispeed);
  GFUNC(cfgetospeed,HAVE_cfgetospeed);
  GFUNC(cfsetispeed,HAVE_cfsetispeed);
  GFUNC(cfsetospeed,HAVE_cfsetospeed);
  GFUNC(chdir,HAVE_chdir);
  GFUNC(chmod,HAVE_chmod);
  GFUNC(chown,HAVE_chown);
  GFUNC(clock_getres,HAVE_clock_getres);
  GFUNC(clock_gettime,HAVE_clock_gettime);
  GFUNC(clock_settime,HAVE_clock_settime);
  GFUNC(close,HAVE_close);
  GFUNC(closedir,HAVE_closedir);
  /*  GFUNC(creat,HAVE_creat); */
  GFUNC(ctermid,HAVE_ctermid);
  GFUNC(ctime,HAVE_ctime);
#if defined(SOLARIS_HACK)
  if (HAVE___posix_ctime_r == 1) {
    gfuncsol("ctime_r","__posix_ctime_r");
  } else {
    GFUNC(ctime_r,HAVE_ctime_r);
  }
#else
  GFUNC(ctime_r,HAVE_ctime_r);
#endif
  GFUNC(dup,HAVE_dup);
  GFUNC(dup2,HAVE_dup2);

  /* .... still need to check all the following functions, to
     verify how they return errors, and arrange for the correct kind of
     stub link-name to be generated if the function is not supported
   */

  GFUNC(execl,HAVE_execl);
  GFUNC(execle,HAVE_execle);
  GFUNC(execlp,HAVE_execlp);
  GFUNC(execv,HAVE_execv);
  GFUNC(execve,HAVE_execve);
  GFUNC(execvp,HAVE_execvp);
  GFUNC(fchmod,HAVE_fchmod);
  GFUNC(fcntl,HAVE_fcntl);
  GFUNC(fdatasync,HAVE_fdatasync);
  /*  GFUNC(fdopen,HAVE_fdopen); */
  /*  GFUNC(fileno,HAVE_fileno); */
  /*  GFUNC(flockfile,HAVE_flockfile); */
  GFUNC(fork,HAVE_fork);
  GFUNC(fpathconf,HAVE_fpathconf);
  GFUNC(fstat,HAVE_fstat);
  GFUNC(fsync,HAVE_fsync);
  GFUNC(ftruncate,HAVE_ftruncate);
  /*  GFUNC(funlockfile,HAVE_funlockfile); */
  /*  GFUNC(getc_unlocked,HAVE_getc_unlocked); */
  /*  GFUNC(getchar_unlocked,HAVE_getchar_unlocked); */
  GFUNC(getcwd,HAVE_getcwd);
  GFUNC(getegid,HAVE_getegid);
  GFUNC(getenv,HAVE_getenv);
  GFUNC(geteuid,HAVE_geteuid);
  GFUNC(getgid,HAVE_getgid);
  GFUNC(getgrgid,HAVE_getgrgid);
  GFUNC(getgrnam,HAVE_getgrnam);
  GFUNC(getgroups,HAVE_getgroups);
  GFUNC(getlogin,HAVE_getlogin);
  GFUNC(getpgrp,HAVE_getpgrp);
  GFUNC(getpid,HAVE_getpid);
  GFUNC(getppid,HAVE_getppid);
  GFUNC(getpwnam,HAVE_getpwnam);
  GFUNC(getpwuid,HAVE_getpwuid);
  GFUNC(gettimeofday,HAVE_gettimeofday);
  GFUNC(getuid,HAVE_getuid);
  GFUNC(gmtime_r,HAVE_gmtime_r);
  GFUNC(isatty,HAVE_isatty);
  GFUNC(kill,HAVE_kill);
  GFUNC(link,HAVE_link);
  GFUNC(lio_listio,HAVE_lio_listio);
  /*  GFUNC(localtime_r,HAVE_localtime_r); */
  GFUNC(lseek,HAVE_lseek);
  GFUNC(mkdir,HAVE_mkdir);
  GFUNC(mkfifo,HAVE_mkfifo);
  GFUNC(mlock,HAVE_mlock);
  GFUNC(mlockall,HAVE_mlockall);
  GFUNC(mmap,HAVE_mmap);
  GFUNC(mprotect,HAVE_mprotect);
  GFUNC(mq_close,HAVE_mq_close);
  GFUNC(mq_getattr,HAVE_mq_getattr);
  GFUNC(mq_notify,HAVE_mq_notify);
  GFUNC(mq_open,HAVE_mq_open);
  GFUNC(mq_receive,HAVE_mq_receive);
  GFUNC(mq_send,HAVE_mq_send);
  GFUNC(mq_setattr,HAVE_mq_setattr);
  GFUNC(mq_unlink,HAVE_mq_unlink);
  GFUNC(msync,HAVE_msync);
  GFUNC(munlock,HAVE_munlock);
  GFUNC(munlockall,HAVE_munlockall);
  GFUNC(munmap,HAVE_munmap);
  /*  GFUNC(nanosleep,HAVE_nanosleep); */
  GFUNC(open,HAVE_open);
  GFUNC(opendir,HAVE_opendir);
  GFUNC(pathconf,HAVE_pathconf);
  /*  GFUNC(pause,HAVE_pause); */
  /*  GFUNC(perror,HAVE_perror); */
  GFUNC(pipe,HAVE_pipe);
  /*  GFUNCD(pthread_atfork,HAVE_pthread_atfork); */
  /*  GFUNCD(pthread_attr_destroy,HAVE_pthread_attr_destroy); */
  /*  GFUNCD(pthread_attr_getdetachstate,HAVE_pthread_attr_getdetachstate); */
  /*  GFUNCD(pthread_attr_getinheritsched,HAVE_pthread_attr_getinheritsched);
   */
  /*  GFUNCD(pthread_attr_getschedparam,HAVE_pthread_attr_getschedparam); */
  /*  GFUNCD(pthread_attr_getschedpolicy,HAVE_pthread_attr_getschedpolicy); */
  /*  GFUNCD(pthread_attr_getscope,HAVE_pthread_attr_getscope); */
  /*  GFUNCD(pthread_attr_getstackaddr,HAVE_pthread_attr_getstackaddr); */
  /*  GFUNCD(pthread_attr_getstacksize,HAVE_pthread_attr_getstacksize); */
  /*  GFUNCD(pthread_attr_init,HAVE_pthread_attr_init); */
  /*  GFUNCD(pthread_attr_setdetachstate,HAVE_pthread_attr_setdetachstate); */
  /*  GFUNCD(pthread_attr_setinheritsched,HAVE_pthread_attr_setinheritsched);
   */
  /*  GFUNCD(pthread_attr_setschedparam,HAVE_pthread_attr_setschedparam); */
  /*  GFUNCD(pthread_attr_setschedpolicy,HAVE_pthread_attr_setschedpolicy); */
  /*  GFUNCD(pthread_attr_setscope,HAVE_pthread_attr_setscope); */
  /*  GFUNCD(pthread_attr_setstackaddr,HAVE_pthread_attr_setstackaddr); */
  /*  GFUNCD(pthread_attr_setstacksize,HAVE_pthread_attr_setstacksize); */
  /*  GFUNCD(pthread_cancel,HAVE_pthread_cancel); */
  /*  GFUNCD(pthread_cleanup_pop,HAVE_pthread_cleanup_pop); */
  /*  GFUNCD(pthread_cleanup_push,HAVE_pthread_cleanup_push); */
  GFUNCD(pthread_cond_broadcast,HAVE_pthread_cond_broadcast);
  GFUNCD(pthread_cond_destroy,HAVE_pthread_cond_destroy);
  GFUNCD(pthread_cond_init,HAVE_pthread_cond_init);
  GFUNCD(pthread_cond_signal,HAVE_pthread_cond_signal);
  GFUNCD(pthread_cond_timedwait,HAVE_pthread_cond_timedwait);
  GFUNCD(pthread_cond_wait,HAVE_pthread_cond_wait);
  GFUNCD(pthread_condattr_destroy,HAVE_pthread_condattr_destroy);
  GFUNCD(pthread_condattr_getpshared,HAVE_pthread_condattr_getpshared);
  GFUNCD(pthread_condattr_init,HAVE_pthread_condattr_init);
  GFUNCD(pthread_condattr_setpshared,HAVE_pthread_condattr_setpshared);
  /*  GFUNCD(pthread_create,HAVE_pthread_create); */
  /*  GFUNCD(pthread_equal,HAVE_pthread_equal); */
  /*  GFUNCD(pthread_exit,HAVE_pthread_exit); */
  /*  GFUNCD(pthread_getschedparam,HAVE_pthread_getschedparam); */
  /*  GFUNCD(pthread_getspecific,HAVE_pthread_getspecific); */
  /*  GFUNCD(pthread_join,HAVE_pthread_join); */
  /*  GFUNCD(pthread_key_create,HAVE_pthread_key_create); */
  /*  GFUNCD(pthread_key_delete,HAVE_pthread_key_delete); */
  /*  GFUNCD(pthread_kill,HAVE_pthread_kill); */
  GFUNCD(pthread_mutex_destroy,HAVE_pthread_mutex_destroy);
  GFUNCD(pthread_mutex_getprioceiling,HAVE_pthread_mutex_getprioceiling);
  GFUNCD(pthread_mutex_init,HAVE_pthread_mutex_init);
  GFUNCD(pthread_mutex_lock,HAVE_pthread_mutex_lock);
  GFUNCD(pthread_mutex_setprioceiling,HAVE_pthread_mutex_setprioceiling);
  GFUNCD(pthread_mutex_trylock,HAVE_pthread_mutex_trylock);
  GFUNCD(pthread_mutex_unlock,HAVE_pthread_mutex_unlock);
  GFUNCD(pthread_mutexattr_destroy,HAVE_pthread_mutexattr_destroy);
  GFUNCD(pthread_mutexattr_getprioceiling,
    HAVE_pthread_mutexattr_getprioceiling);
  GFUNCD(pthread_mutexattr_getprotocol,HAVE_pthread_mutexattr_getprotocol);
  GFUNCD(pthread_mutexattr_getpshared,HAVE_pthread_mutexattr_getpshared);
  GFUNCD(pthread_mutexattr_init,HAVE_pthread_mutexattr_init);
  GFUNCD(pthread_mutexattr_setprioceiling,
    HAVE_pthread_mutexattr_setprioceiling);
  GFUNCD(pthread_mutexattr_setprotocol,HAVE_pthread_mutexattr_setprotocol);
  /* if not supported, the error code is Operation_Not_Implemented,
     i.e., ENOSYS */
  GFUNCD(pthread_mutexattr_setpshared,HAVE_pthread_mutexattr_setpshared);
  /*  GFUNCD(pthread_once,HAVE_pthread_once); */
  /*  GFUNCD(pthread_self,HAVE_pthread_self); */
  /*  GFUNCD(pthread_setcancelstate,HAVE_pthread_setcancelstate); */
  /*  GFUNCD(pthread_setcanceltype,HAVE_pthread_setcanceltype); */
  /*  GFUNCD(pthread_setschedparam,HAVE_pthread_setschedparam); */
  /*  GFUNCD(pthread_setspecific,HAVE_pthread_setspecific); */
  GFUNCD(pthread_sigmask,HAVE_pthread_sigmask);
  /*  GFUNCD(pthread_testcancel,HAVE_pthread_testcancel); */
  /*  GFUNC(putc_unlocked,HAVE_putc_unlocked); */
  /*  GFUNC(putchar_unlocked,HAVE_putchar_unlocked); */
  GFUNC(putenv,HAVE_putenv);
  /*  GFUNC(rand_r,HAVE_rand_r); */
  GFUNC(read,HAVE_read);
  GFUNC(readdir,HAVE_readdir);
#if defined(SOLARIS_HACK)
  if (HAVE___posix_readdir_r == 1) {
    gfuncsol("readdir_r","__posix_readdir_r");
  } else {
    GFUNC(readdir_r,HAVE_readdir_r);
  }
#else
  GFUNC(readdir_r,HAVE_readdir_r);
#endif
  GFUNC(rename,HAVE_rename);
  /* GFUNC(rewinddir,HAVE_rewinddir); */
  /* GFUNC(rewinddir_r,HAVE_rewinddir_r); */
  GFUNC(rmdir,HAVE_rmdir);
  GFUNC(sched_get_priority_max,HAVE_sched_get_priority_max);
  GFUNC(sched_get_priority_min,HAVE_sched_get_priority_min);
  GFUNC(sched_rr_get_interval,HAVE_sched_rr_get_interval);
  GFUNC(sched_getparam,HAVE_sched_getparam);
  GFUNC(sched_getscheduler,HAVE_sched_getscheduler);
  GFUNC(sched_setparam,HAVE_sched_setparam);
  GFUNC(sched_setscheduler,HAVE_sched_setscheduler);
  GFUNC(sched_yield,HAVE_sched_yield);
  GFUNC(sem_close,HAVE_sem_close);
  GFUNC(sem_destroy,HAVE_sem_destroy);
  GFUNC(sem_getvalue,HAVE_sem_getvalue);
  GFUNC(sem_init,HAVE_sem_init);
  GFUNC(sem_open,HAVE_sem_open);
  GFUNC(sem_post,HAVE_sem_post);
  GFUNC(sem_trywait,HAVE_sem_trywait);
  GFUNC(sem_unlink,HAVE_sem_unlink);
  GFUNC(sem_wait,HAVE_sem_wait);
  GFUNC(setenv,HAVE_setenv);
  GFUNC(setgid,HAVE_setgid);
  GFUNC(setpgid,HAVE_setpgid);
  GFUNC(setsid,HAVE_setsid);
  GFUNC(setuid,HAVE_setuid);
  GFUNC(shm_open,HAVE_shm_open);
  GFUNC(shm_unlink,HAVE_shm_unlink);
  GFUNC(sigaction,HAVE_sigaction);
  GFUNC(sigaddset,HAVE_sigaddset);
  GFUNC(sigdelset,HAVE_sigdelset);
  GFUNC(sigemptyset,HAVE_sigemptyset);
  GFUNC(sigfillset,HAVE_sigfillset);
  GFUNC(sigismember,HAVE_sigismember);
#ifdef siglongjmp
  /*  ifprintf(fp,"   --  *** WARNING: %s is a macro ***  --\n",
        "siglongjmp"); */
#endif
  /*  GFUNC(siglongjmp,HAVE_siglongjmp); */
  GFUNC(sigpending,HAVE_sigpending);
  GFUNC(sigprocmask,HAVE_sigprocmask);
  GFUNC(sigqueue,HAVE_sigqueue);
#ifdef sigsetjmp
  /*  ifprintf(fp,"   --  *** WARNING: %s is a macro ***  --\n",
        "sigsetjmp"); */
#endif
  /*  GFUNC(sigsetjmp,HAVE_sigsetjmp); */
  /*  GFUNC(sigsuspend,HAVE_sigsuspend); */
  GFUNC(sigtimedwait,HAVE_sigtimedwait);
#if defined(SOLARIS_HACK)
  if (HAVE___posix_sigwait == 1) {
    gfuncsol("sigwait","__posix_sigwait");
  } else {
    GFUNC(sigwait,HAVE_sigwait);
  }
#else
  GFUNC(sigwait,HAVE_sigwait);
#endif
  GFUNC(sigwaitinfo,HAVE_sigwaitinfo);
  /*  GFUNC(sleep,HAVE_sleep); */
  GFUNC(stat,HAVE_stat);
  /*  GFUNC(strtok_r,HAVE_strtok_r); */
  GFUNC(sysconf,HAVE_sysconf);
  GFUNC(tcdrain,HAVE_tcdrain);
  GFUNC(tcflow,HAVE_tcflow);
  GFUNC(tcflush,HAVE_tcflush);
  GFUNC(tcgetattr,HAVE_tcgetattr);
  GFUNC(tcgetpgrp,HAVE_tcgetpgrp);
  GFUNC(tcsendbreak,HAVE_tcsendbreak);
  GFUNC(tcsetattr,HAVE_tcsetattr);
  GFUNC(tcsetpgrp,HAVE_tcsetpgrp);
  GFUNC(time,HAVE_time);
  GFUNC(timer_create,HAVE_timer_create);
  /* POSIX.5b erroneously specifies OPERATION_NOT_SUPPORTED for
     Create/Delete_Timer.
     That is inconsistent with POSIX.1b.
     Therefore, we follow POSIX.1b instead.
   */
  GFUNC(timer_delete,HAVE_timer_delete);
  GFUNC(timer_getoverrun,HAVE_timer_getoverrun);
  GFUNC(timer_gettime,HAVE_timer_gettime);
  GFUNC(timer_settime,HAVE_timer_settime);
  GFUNC(times,HAVE_times);
  GFUNC(ttyname,HAVE_ttyname);
  /*  GFUNC(tzset,HAVE_tzset); */
  GFUNC(umask,HAVE_umask);
  GFUNC(uname,HAVE_uname);
  GFUNC(unlink,HAVE_unlink);
  GFUNC(unsetenv,HAVE_unsetenv);
  GFUNC(utime,HAVE_utime);
  /*  GFUNC(wait,HAVE_wait); */
  GFUNC(waitpid,HAVE_waitpid);
  GFUNC(write,HAVE_write);

  ghdrcmnt("C functions for macros");
  gmacrofunc("s_isdir","mode_t","mode");
  gmacrofunc("s_ischr","mode_t","mode");
  gmacrofunc("s_isblk","mode_t","mode");
  gmacrofunc("s_islnk","mode_t","mode");
  gmacrofunc("s_isreg","mode_t","mode");
  gmacrofunc("s_isfifo","mode_t","mode");
  gmacrofunc("s_ismsg","mode_t","mode");
  gmacrofunc("s_issem","mode_t","mode");
  gmacrofunc("s_isshm","mode_t","mode");
  gmacrofunc("s_issock","mode_t","mode");
  gmacrofunc("s_typeismq","stat_ptr","stat");
  gmacrofunc("s_typeissem","stat_ptr","stat");
  gmacrofunc("s_typeisshm","stat_ptr","stat");
  gmacrofunc("wifexited","int","stat_val");
  gmacrofunc("wifexitstatus","int","stat_val");
  gmacrofunc("wifsignaled","int","stat_val");
  gmacrofunc("wiftermsig","int","stat_val");
  gmacrofunc("wifstopped","int","stat_val");
  gmacrofunc("wifstopsig","int","stat_val");

/* c_sockets
   ----------------
 */

  indent++;
  ifprintf(fp,"package Sockets is\n");
  ghdrcmnt("socket.h");

#ifdef HAVE_sa_family_t
  guitp("sa_family_t", sizeof(sa_family_t));
#else
  NON_SUPPORT_MESSAGE("sa_family_t")
  guitp("sa_family_t", sizeof(short));
#endif

#ifdef HAVE_in_port_t
  guitp("in_port_t", sizeof(in_port_t));
#else
  NON_SUPPORT_MESSAGE("in_port_t")
  guitp("in_port_t", sizeof(in_port_t));
#endif

  ghdrcmnt("constants");

#ifdef HOST_NOT_FOUND
  GCST("HOST_NOT_FOUND", HOST_NOT_FOUND);
#else
  GDFLT("HOST_NOT_FOUND", 0);
#endif
#ifdef NO_DATA
  GCST("NO_DATA", NO_DATA);
#else
  GDFLT("NO_DATA", 0);
#endif
#ifdef NO_RECOVERY
  GCST("NO_RECOVERY", NO_RECOVERY);
#else
  GDFLT("NO_RECOVERY", 0);
#endif
#ifdef TRY_AGAIN
  GCST("TRY_AGAIN", TRY_AGAIN);
#else
  GDFLT("TRY_AGAIN", 0);
#endif

#ifdef MAX_SOCKADDR_EXT
  GCST("MAX_SOCKADDR_EXT", MAX_SOCKADDR_EXT);
#else
  GDFLT("MAX_SOCKADDR_EXT", 108);
#endif
  gcmnt("sockets protocol level");
#ifdef SOL_SOCKET
  GCST("SOL_SOCKET", SOL_SOCKET);
#else
  GDFLT("SOL_SOCKET", 0);
#endif
  gcmnt("socket types");
#ifdef SOCK_STREAM
  GCST("SOCK_STREAM", SOCK_STREAM);
#else
  GDFLT("SOCK_STREAM", 0);
#endif
#ifdef SOCK_DGRAM
  GCST("SOCK_DGRAM", SOCK_DGRAM);
#else
  GDFLT("SOCK_DGRAM", 0);
#endif
#ifdef SOCK_RAW
  GCST("SOCK_RAW", SOCK_RAW);
#else
  GDFLT("SOCK_RAW", 0);
#endif
#ifdef SOCK_SEQPACKET
  GCST("SOCK_SEQPACKET", SOCK_SEQPACKET);
#else
  GDFLT("SOCK_SEQPACKET", 0);
#endif
  gcmnt("address families");
#ifdef AF_MAX
  GCST("AF_MAX", AF_MAX);
#else
  GDFLT("AF_MAX", 0);
#endif
#ifdef AF_UNSPEC
  GCST("AF_UNSPEC", AF_UNSPEC);
#else
  GDFLT("AF_UNSPEC", 0);
#endif
#ifdef AF_UNIX
  GCST("AF_UNIX", AF_UNIX);
#else
  GDFLT("AF_UNIX", 0);
#endif
#ifdef AF_LOCAL
  GCST("AF_LOCAL", AF_LOCAL);
#else
#ifdef AF_UNIX
  GCST("AF_LOCAL", AF_UNIX);
#else
  GDFLT("AF_LOCAL", 0);
#endif
#endif
#ifdef AF_INET
  GCST("AF_INET", AF_INET);
#else
  GDFLT("AF_INET", 0);
#endif
#ifdef AF_OSI
  GCST("AF_OSI", AF_OSI);
#else
  GDFLT("AF_OSI", 0);
#endif
#ifdef AF_ISO
  GCST("AF_ISO", AF_ISO);
#else
  GDFLT("AF_ISO", 0);
#endif
  gcmnt("protocol families");
#ifdef PF_MAX
  GCST("PF_MAX", PF_MAX);
#else
  GDFLT("PF_MAX", 0);
#endif
#ifdef PF_UNSPEC
  GCST("PF_UNSPEC", PF_UNSPEC);
#else
  GDFLT("PF_UNSPEC", 0);
#endif
#ifdef PF_LOCAL
  GCST("PF_LOCAL", PF_LOCAL);
#else
#ifdef PF_UNIX
  GCST("PF_LOCAL", PF_UNIX);
#else
  GDFLT("PF_LOCAL", 0);
#endif
#endif
#ifdef PF_UNIX
  GCST("PF_UNIX", PF_UNIX);
#else
  GDFLT("PF_UNIX", 0);
#endif
#ifdef PF_INET
  GCST("PF_INET", PF_INET);
#else
  GDFLT("PF_INET", 0);
#endif
#ifdef PF_OSI
  GCST("PF_OSI", PF_OSI);
#else
  GDFLT("PF_OSI", 0);
#endif
#ifdef PF_ISO
  GCST("PF_ISO", PF_ISO);
#else
  GDFLT("PF_ISO", 0);
#endif
  gcmnt("socket options");
#ifdef SO_BROADCAST
  GUCST("SO_BROADCAST", SO_BROADCAST);
#else
  GDFLT("SO_BROADCAST", 0);
#endif
#ifdef SO_DEBUG
  GCST("SO_DEBUG", SO_DEBUG);
#else
  GDFLT("SO_DEBUG", 0);
#endif
#ifdef SO_DONTROUTE
  GCST("SO_DONTROUTE", SO_DONTROUTE);
#else
  GDFLT("SO_DONTROUTE", 0);
#endif
#ifdef SO_ERROR
  GCST("SO_ERROR", SO_ERROR);
#else
  GDFLT("SO_ERROR", 0);
#endif
#ifdef SO_KEEPALIVE
  GCST("SO_KEEPALIVE", SO_KEEPALIVE);
#else
  GDFLT("SO_KEEPALIVE", 0);
#endif
#ifdef SO_LINGER
  GCST("SO_LINGER", SO_LINGER);
#else
  GDFLT("SO_LINGER", 0);
#endif
#ifdef SO_OOBINLINE
  GCST("SO_OOBINLINE", SO_OOBINLINE);
#else
  GDFLT("SO_OOBINLINE", 0);
#endif
#ifdef SO_RCVBUF
  GCST("SO_RCVBUF", SO_RCVBUF);
#else
  GDFLT("SO_RCVBUF", 0);
#endif
#ifdef SO_RCVLOWAT
  GCST("SO_RCVLOWAT", SO_RCVLOWAT);
#else
  GDFLT("SO_RCVLOWAT", 0);
#endif
#ifdef SO_RCVTIMEO
  GCST("SO_RCVTIMEO", SO_RCVTIMEO);
#else
  GDFLT("SO_RCVTIMEO", 0);
#endif
#ifdef SO_REUSEADDR
  GCST("SO_REUSEADDR", SO_REUSEADDR);
#else
  GDFLT("SO_REUSEADDR", 0);
#endif
#ifdef SO_SNDBUF
  GCST("SO_SNDBUF", SO_SNDBUF);
#else
  GDFLT("SO_SNDBUF", 0);
#endif
#ifdef SO_SNDLOWAT
  GCST("SO_SNDLOWAT", SO_SNDLOWAT);
#else
  GDFLT("SO_SNDLOWAT", 0);
#endif
#ifdef SO_SNDTIMEO
  GCST("SO_SNDTIMEO", SO_SNDTIMEO);
#else
  GDFLT("SO_SNDTIMEO", 0);
#endif
#ifdef SO_TYPE
  GCST("SO_TYPE", SO_TYPE);
#else
  GDFLT("SO_TYPE", 0);
#endif
  gcmnt("max queued connections");
#ifdef SOMAXCONN
  GCST("SOMAXCONN", SOMAXCONN);
#else
  GDFLT("SOMAXCONN", 0);
#endif
  gcmnt("send & receive option flag bits");
#ifdef MSG_OOB
  GCST("MSG_OOB", MSG_OOB);
#else
  GDFLT("MSG_OOB", 0);
#endif
#ifdef MSG_PEEK
  GCST("MSG_PEEK", MSG_PEEK);
#else
  GDFLT("MSG_PEEK", 0);
#endif
#ifdef MSG_DONTROUTE
  GCST("MSG_DONTROUTE", MSG_DONTROUTE);
#else
  GDFLT("MSG_DONTROUTE", 0);
#endif
#ifdef MSG_EOR
  GCST("MSG_EOR", MSG_EOR);
#else
  GDFLT("MSG_EOR", 0);
#endif
#ifdef MSG_TRUNC
  GCST("MSG_TRUNC", MSG_TRUNC);
#else
  GDFLT("MSG_TRUNC", 0);
#endif
#ifdef MSG_CTRUNC
  GCST("MSG_CTRUNC", MSG_CTRUNC);
#else
  GDFLT("MSG_CTRUNC", 0);
#endif
#ifdef MSG_WAITALL
  GCST("MSG_WAITALL", MSG_WAITALL);
#else
  GDFLT("MSG_WAITALL", 0);
#endif
#ifdef MSG_MAXIOVLEN
  GCST("MSG_MAXIOVLEN", MSG_MAXIOVLEN);
#else
  GDFLT("MSG_MAXIOVLEN", 0);
#endif
  gcmnt("socket address information option flag bits");
#ifdef AI_PASSIVE
  GCST("AI_PASSIVE", AI_PASSIVE);
#else
  GDFLT("AI_PASSIVE", 0);
#endif
#ifdef AI_CANONNAME
  GCST("AI_CANONNAME", AI_CANONNAME);
#else
  GDFLT("AI_CANONNAME", 0);
#endif
  gcmnt("scoket shutdown mode flag bits");
#ifdef SHUT_RD
  GCST("SHUT_RD", SHUT_RD);
#else
  GDFLT("SHUT_RD", 0);
#endif
#ifdef SHUT_WR
  GCST("SHUT_WR", SHUT_WR);
#else
  GDFLT("SHUT_WR", 1);
#endif
#ifdef SHUT_RDWR
  GCST("SHUT_RDWR", SHUT_RDWR);
#else
  GDFLT("SHUT_RDWR", 2);
#endif

  ghdrcmnt("structures");
  /* can't follow alphabetic ordering; e.g.
    sockaddr needs to come early, to avoid forward references
   */
  gcmnt("generic socket address");
  g_struct_sockaddr();
  gcmnt("struct addrinfo...");
  g_struct_addrinfo();
  gcmnt("message option header");
  g_struct_cmsghdr();
  gcmnt("host database entry");
  g_struct_hostent();
  gcmnt("internet address");
#ifdef HAVE_in_addr_t
  guitp("in_addr_t", sizeof(in_addr_t));
#else
  NON_SUPPORT_MESSAGE("in_addr_t")
  guitp("in_addr_t", sizeof(long));
#endif
  g_struct_in_addr();
  gcmnt("linger option structure");
  g_struct_linger();
  gcmnt("I/O vector");
  g_struct_iovec();
  gcmnt("message header");
  g_struct_msghdr();
  gcmnt("local socket address");
  { struct sockaddr_un DUMMY;
    ifprintf(fp,"   subtype sun_path_string is
      POSIX_String (1 .. %d);\n", sizeof (DUMMY.sun_path));
  }
  g_struct_sockaddr_un();
  gcmnt("internet socket address");
  g_struct_sockaddr_in();
  gcmnt("IP Level ip_opts structure");
  g_struct_ip_opts();

  ghdrcmnt("link names for functions");
  GFUNC(accept, HAVE_accept);
  GFUNC(bind, HAVE_bind);
  GFUNC(connect, HAVE_connect);
  GFUNC(getsockname, HAVE_getsockname);
  GFUNC(getsockopt, HAVE_getsockopt);
  GFUNC(isfdtype, HAVE_isfdtype);
  GFUNC(listen, HAVE_listen);
  GFUNC(recv, HAVE_recv);
  GFUNC(recvfrom, HAVE_recvfrom);
  GFUNC(recvmsg, HAVE_recvmsg);
  GFUNC(send, HAVE_send);
  GFUNC(sendto, HAVE_sendto);
  GFUNC(sendmsg, HAVE_sendmsg);
  GFUNC(setsockopt, HAVE_setsockopt);
  GFUNC(shutdown, HAVE_shutdown);
  GFUNC(socket, HAVE_socket);
  GFUNC(sockatmark, HAVE_sockatmark);
  GFUNC(socketpair, HAVE_socketpair);

  fprintf(fp,"\n");
  ifprintf(fp,"end Sockets;\n\n");
  indent--;

/* c_xti
   ----------------
*/

  indent++;
  ifprintf(fp,"package XTI is\n");

  ghdrcmnt("XTI structures");
  /* can't follow alphabetic ordering; e.g.
   */

  gcmnt("netbuf structure");
  g_struct_netbuf();

  gcmnt("t_info structure");
  g_struct_t_info();

  gcmnt("t_opthdr structure");
  g_struct_t_opthdr();

  gcmnt("t_bind structure");
  g_struct_t_bind();

  gcmnt("t_optmgmt structure");
  g_struct_t_optmgmt();

  gcmnt("t_discon structure");
  g_struct_t_discon();

  gcmnt("t_call structure");
  g_struct_t_call();

  gcmnt("t_unitdata structure");
  g_struct_t_unitdata();

  gcmnt("t_uderr structure");
  g_struct_t_uderr();

  gcmnt("t_iovec structure");
  g_struct_t_iovec();

  gcmnt("t_kpalive structure");
  g_struct_t_kpalive();

/*
 * The following are the events returned from t_look()
 */
  gcmnt("The following are the events returned from t_look()");

#ifdef T_LISTEN
  GCST("T_LISTEN", T_LISTEN );
#else
  GDFLT("T_LISTEN", 0);
#endif
#ifdef T_CONNECT
  GCST("T_CONNECT", T_CONNECT );
#else
  GDFLT("T_CONNECT", 0);
#endif
#ifdef T_DATA
  GCST("T_DATA", T_DATA );
#else
  GDFLT("T_DATA", 0);
#endif
#ifdef T_EXDATA
  GCST("T_EXDATA", T_EXDATA );
#else
  GDFLT("T_EXDATA", 0);
#endif
#ifdef T_DISCONNECT
  GCST("T_DISCONNECT", T_DISCONNECT );
#else
  GDFLT("T_DISCONNECT", 0);
#endif
#ifdef T_UDERR
  GCST("T_UDERR", T_UDERR );
#else
  GDFLT("T_UDERR", 0);
#endif
#ifdef T_ORDREL
  GCST("T_ORDREL", T_ORDREL );
#else
  GDFLT("T_ORDREL", 0);
#endif
#ifdef T_GODATA
  GCST("T_GODATA", T_GODATA );
#else
  GDFLT("T_GODATA", 0);
#endif
#ifdef T_GOEXDATA
  GCST("T_GOEXDATA", T_GOEXDATA );
#else
  GDFLT("T_GOEXDATA", 0);
#endif
#ifdef T_EVENTS
  GCST("T_EVENTS", T_EVENTS );
#else
  GDFLT("T_EVENTS", 0);
#endif

/*
 * The following are the flag definitions needed by the
 * user level library routines.
 */

#ifdef T_MORE
  GCST("T_MORE", T_MORE );
#else
  GDFLT("T_MORE", 0);
#endif
#ifdef T_EXPEDITED
  GCST("T_EXPEDITED", T_EXPEDITED );
#else
  GDFLT("T_EXPEDITED", 0);
#endif
#ifdef T_PUSH
  GCST("T_PUSH", T_PUSH );
#else
  GDFLT("T_PUSH", 0);
#endif
#ifdef T_NEGOTIATE
  GCST("T_NEGOTIATE", T_NEGOTIATE );
#else
  GDFLT("T_NEGOTIATE", 0);
#endif
#ifdef T_CHECK
  GCST("T_CHECK", T_CHECK );
#else
  GDFLT("T_CHECK", 0);
#endif
#ifdef T_DEFAULT
  GCST("T_DEFAULT", T_DEFAULT );
#else
  GDFLT("T_DEFAULT", 0);
#endif
#ifdef T_SUCCESS
  GCST("T_SUCCESS", T_SUCCESS );
#else
  GDFLT("T_SUCCESS", 0);
#endif
#ifdef T_FAILURE
  GCST("T_FAILURE", T_FAILURE );
#else
  GDFLT("T_FAILURE", 0);
#endif
#ifdef T_CURRENT
  GCST("T_CURRENT", T_CURRENT );
#else
  GDFLT("T_CURRENT", 0);
#endif
#ifdef T_PARTSUCCESS
  GCST("T_PARTSUCCESS", T_PARTSUCCESS );
#else
  GDFLT("T_PARTSUCCESS", 0);
#endif
#ifdef T_READONLY
  GCST("T_READONLY", T_READONLY );
#else
  GDFLT("T_READONLY", 0);
#endif
#ifdef T_NOTSUPPORT
  GCST("T_NOTSUPPORT", T_NOTSUPPORT );
#else
  GDFLT("T_NOTSUPPORT", 0);
#endif
#ifdef T_RAW
  GCST("T_RAW", T_RAW );
#else
  GDFLT("T_RAW", 0);
#endif

/*
 * Service types defines
 */

  gcmnt("Service types defines");

#ifdef T_COTS
  GCST("T_COTS", T_COTS );
#else
  GDFLT("T_COTS", 0);
#endif
#ifdef T_COTS_ORD
  GCST("T_COTS_ORD", T_COTS_ORD );
#else
  GDFLT("T_COTS_ORD", 0);
#endif
#ifdef T_CLTS
  GCST("T_CLTS", T_CLTS );
#else
  GDFLT("T_CLTS", 0);
#endif

/*
 * Flags defines (other info about the transport provider).
 */

#ifdef T_SENDZERO
  GCST("T_SENDZERO", T_SENDZERO );
#else
  GDFLT("T_SENDZERO", 0);
#endif
#ifdef SENDZERO
  GCST("SENDZERO", SENDZERO );
#else
  GDFLT("SENDZERO", 0);
#endif
#ifdef T_XPG4_1
  GCST("T_XPG4_1", T_XPG4_1 );
#else
  GDFLT("T_XPG4_1", 0);
#endif
#ifdef XPG4_1
  GCST("XPG4_1", XPG4_1 );
#else
  GDFLT("XPG4_1", 0);
#endif

/*
 * The following are structure types used when dynamically
 * allocating the above structure via alloc().
 */

#ifdef T_BIND
  GCST("T_BIND", T_BIND );
#else
  GDFLT("T_BIND", 0);
#endif
#ifdef T_OPTMGMT
  GCST("T_OPTMGMT", T_OPTMGMT );
#else
  GDFLT("T_OPTMGMT", 0);
#endif
#ifdef T_CALL
  GCST("T_CALL", T_CALL );
#else
  GDFLT("T_CALL", 0);
#endif
#ifdef T_DIS
  GCST("T_DIS", T_DIS );
#else
  GDFLT("T_DIS", 0);
#endif
#ifdef T_UNITDATA
  GCST("T_UNITDATA", T_UNITDATA );
#else
  GDFLT("T_UNITDATA", 0);
#endif
#ifdef T_UDERROR
  GCST("T_UDERROR", T_UDERROR );
#else
  GDFLT("T_UDERROR", 0);
#endif
#ifdef T_INFO
  GCST("T_INFO", T_INFO );
#else
  GDFLT("T_INFO", 0);
#endif
#ifdef T_KUNITDATA
  GCST("T_KUNITDATA", T_KUNITDATA );
#else
  GDFLT("T_KUNITDATA", 0);
#endif

/*
 * The following bits specify which fields of the above
 * structures should be allocated by t_alloc().
 */

#ifdef T_ADDR
  GCST("T_ADDR", T_ADDR );
#else
  GDFLT("T_ADDR", 0);
#endif
#ifdef T_OPT
  GCST("T_OPT", T_OPT );
#else
  GDFLT("T_OPT", 0);
#endif
#ifdef T_UDATA
  GCST("T_UDATA", T_UDATA );
#else
  GDFLT("T_UDATA", 0);
#endif
#ifdef T_ALL
  GCST("T_ALL", T_ALL );
#else
  GDFLT("T_ALL", 0);
#endif

/*
 * The following are the states for the user.
 */

#ifdef T_UNINIT
  GCST("T_UNINIT", T_UNINIT );
#else
  GDFLT("T_UNINIT", 0);
#endif
#ifdef T_UNBND
  GCST("T_UNBND", T_UNBND );
#else
  GDFLT("T_UNBND", 0);
#endif
#ifdef T_IDLE
  GCST("T_IDLE", T_IDLE );
#else
  GDFLT("T_IDLE", 0);
#endif
#ifdef T_OUTCON
  GCST("T_OUTCON", T_OUTCON );
#else
  GDFLT("T_OUTCON", 0);
#endif
#ifdef T_INCON
  GCST("T_INCON", T_INCON );
#else
  GDFLT("T_INCON", 0);
#endif
#ifdef T_DATAXFER
  GCST("T_DATAXFER", T_DATAXFER );
#else
  GDFLT("T_DATAXFER", 0);
#endif
#ifdef T_OUTREL
  GCST("T_OUTREL", T_OUTREL );
#else
  GDFLT("T_OUTREL", 0);
#endif
#ifdef T_INREL
  GCST("T_INREL", T_INREL );
#else
  GDFLT("T_INREL", 0);
#endif

/* General purpose defines */

#ifdef T_YES
  GCST("T_YES", T_YES );
#else
#  if defined(_TLI_)
      GCST("T_YES", 1);
#  else
      GDFLT("T_YES", 0);
#  endif
#endif

#ifdef T_NO
  GCST("T_NO", T_NO );
#else
#  if defined(_TLI_)
      GCST("T_NO", 0);
#  else
      GDFLT("T_NO", 0);
#  endif
#endif

#ifdef T_UNUSED
  GCST("T_UNUSED", T_UNUSED );
#else
#  if defined(_TLI_)
      GCST("T_UNUSED", -1);
#  else
      GDFLT("T_UNUSED", 0);
#  endif
#endif

#ifdef T_NULL
  GCST("T_NULL", T_NULL );
#else
#  if defined(_TLI_)
      GCST("T_NULL", 0);
#  else
      GDFLT("T_NULL", 0);
#  endif
#endif

#ifdef T_ABSREQ
  GCST("T_ABSREQ", T_ABSREQ );
#else
#  if defined(_TLI_)
      GCST("T_ABSREQ", 0x8000);
#  else
      GDFLT("T_ABSREQ", 0);
#  endif
#endif

#ifdef T_INFINITE
  GCST("T_INFINITE", T_INFINITE );
#else
#  if defined(_TLI_)
      GCST("T_INFINITE", -1);
#  else
      GDFLT("T_INFINITE", 0);
#  endif
#endif

#ifdef T_INVALID
  GCST("T_INVALID", T_INVALID );
#else
#  if defined(_TLI_)
      GCST("T_INVALID", -2);
#  else
      GDFLT("T_INVALID", 0);
#  endif
#endif

  /* XTI-level Options */

  gcmnt ("XTI-level Options");

#ifdef XTI_GENERIC
  GCST("XTI_GENERIC", XTI_GENERIC );
#else
#  if defined(_TLI_)
      GCST("XTI_GENERIC", 0xffff);
#  else
      GDFLT("XTI_GENERIC", 0);
#  endif
#endif

#ifdef XTI_DEBUG
  GCST("XTI_DEBUG", XTI_DEBUG );
#else
#  if defined(_TLI_) && defined(SO_DEBUG)
      GCST("XTI_DEBUG", SO_DEBUG);
#  else
      GDFLT("XTI_DEBUG", 0);
#  endif
#endif

#ifdef XTI_LINGER
  GCST("XTI_LINGER", XTI_LINGER );
#else
#  if defined(_TLI_) && defined(SO_LINGER)
      GCST("XTI_LINGER", SO_LINGER);
#  else
      GDFLT("XTI_LINGER", 0);
#  endif
#endif

#ifdef XTI_RCVBUF
  GCST("XTI_RCVBUF", XTI_RCVBUF );
#else
#  if defined(_TLI_) && defined(SO_RCVBUF)
      GCST("XTI_RCVBUF", SO_RCVBUF);
#  else
      GDFLT("XTI_RCVBUF", 0);
#  endif
#endif

#ifdef XTI_RCVLOWAT
  GCST("XTI_RCVLOWAT", XTI_RCVLOWAT );
#else
#  if defined(_TLI_) && defined(SO_RCVLOWAT)
      GCST("XTI_RCVLOWAT", SO_RCVLOWAT);
#  else
      GDFLT("XTI_RCVLOWAT", 0);
#  endif
#endif

#ifdef XTI_SNDBUF
  GCST("XTI_SNDBUF", XTI_SNDBUF );
#else
#  if defined(_TLI_) && defined(SO_SNDBUF)
      GCST("XTI_SNDBUF", SO_SNDBUF);
#  else
      GDFLT("XTI_SNDBUF", 0);
#  endif
#endif

#ifdef XTI_SNDLOWAT
  GCST("XTI_SNDLOWAT", XTI_SNDLOWAT );
#else
#  if defined(_TLI_) && defined(SO_SNDLOWAT)
      GCST("XTI_SNDLOWAT", SO_SNDLOWAT);
#  else
      GDFLT("XTI_SNDLOWAT", 0);
#  endif
#endif

  gcmnt("t_linger structure");
  g_struct_t_linger();

  gcmnt("General definitions for option management");
#ifdef T_UNSPEC
  GCST("T_UNSPEC", T_UNSPEC );
#else
#  if defined(_TLI_)
      GCST("T_UNSPEC", (~0-2));
#  else
      GDFLT("T_UNSPEC", 0);
#  endif
#endif
#ifdef T_ALLOPT
  GCST("T_ALLOPT", T_ALLOPT );
#else
#  if defined(_TLI_)
      GCST("T_ALLOPT", 0);
#  else
      GDFLT("T_ALLOPT", 0);
#  endif
#endif

  gmacrofunc("c_T_ALIGN", "char_ptr", "p");

  /* TCP Level and options */
  gcmnt("TCP Level and Options");

#ifdef INET_TCP
  GCST("INET_TCP", INET_TCP);
#else
  GDFLT("INET_TCP",0);
#endif

#ifdef TCP_NODELAY
  GCST("TCP_NODELAY", TCP_NODELAY);
#else
  GDFLT("TCP_NODELAY",0);
#endif
#ifdef TCP_MAXSEG
  GCST("TCP_MAXSEG", TCP_MAXSEG);
#else
  GDFLT("TCP_MAXSEG",0);
#endif
#ifdef TCP_KEEPALIVE
  GCST("TCP_KEEPALIVE", TCP_KEEPALIVE);
#else
#  if defined(_TLI_) && defined (SO_KEEPALIVE)
      GCST("TCP_KEEPALIVE", SO_KEEPALIVE);
#  else
      GDFLT("TCP_KEEPALIVE",0);
#  endif
#endif

#ifdef T_GARBAGE
  GCST("T_GARBAGE", T_GARBAGE);
#else
#  if defined(_TLI_)
      GCST("T_GARBAGE", 2);
#  else
      GDFLT("T_GARBAGE",0);
#  endif
#endif

  /* UDP Level and options */

  gcmnt("UDP Level and Options");
#ifdef INET_UDP
  GCST("INET_UDP", INET_UDP);
#else
  GDFLT("INET_UDP",0);
#endif

#ifdef UDP_CHECKSUM
  GCST("UDP_CHECKSUM", UDP_CHECKSUM);
#else
  GDFLT("UDP_CHECKSUM",0);
#endif

  /* IP Level and Options */

  gcmnt("IP Level and Options");
#ifdef INET_IP
  GCST("INET_IP", INET_IP);
#else
  GDFLT("INET_IP",0);
#endif

#ifdef IP_OPTIONS
  GCST("IP_OPTIONS", IP_OPTIONS);
#else
  GDFLT("IP_OPTIONS",0);
#endif

#ifdef IP_TOS
  GCST("IP_TOS", IP_TOS);
#else
  GDFLT("IP_TOS",0);
#endif

#ifdef IP_TTL
  GCST("IP_TTL", IP_TTL);
#else
  GDFLT("IP_TTL",0);
#endif

#ifdef IP_REUSEADDR
  GCST("IP_REUSEADDR", IP_REUSEADDR);
#else
#  if defined(_TLI_)
      GCST("IP_REUSEADDR", SO_REUSEADDR);
#  else
      GDFLT("IP_REUSEADDR",0);
#  endif
#endif

#ifdef IP_DONTROUTE
  GCST("IP_DONTROUTE", IP_DONTROUTE);
#else
#  if defined(_TLI_)
      GCST("IP_DONTROUTE", SO_DONTROUTE);
#  else
      GDFLT("IP_DONTROUTE",0);
#  endif
#endif

#ifdef IP_BROADCAST
  GCST("IP_BROADCAST", IP_BROADCAST);
#else
#  if defined(_TLI_)
      GCST("IP_BROADCAST", SO_BROADCAST);
#  else
      GDFLT("IP_BROADCAST",0);
#  endif
#endif

   gcmnt("IP_TOS precedence levels");
#ifdef T_ROUTINE
  GCST("T_ROUTINE", T_ROUTINE);
#else
  GDFLT("T_ROUTINE",0);
#endif
#ifdef T_PRIORITY
  GCST("T_PRIORITY", T_PRIORITY);
#else
  GDFLT("T_PRIORITY",0);
#endif
#ifdef T_IMMEDIATE
  GCST("T_IMMEDIATE", T_IMMEDIATE);
#else
  GDFLT("T_IMMEDIATE",0);
#endif
#ifdef T_FLASH
  GCST("T_FLASH", T_FLASH);
#else
  GDFLT("T_FLASH",0);
#endif
#ifdef T_OVERRIDEFLASH
  GCST("T_OVERRIDEFLASH", T_OVERRIDEFLASH);
#else
  GDFLT("T_OVERRIDEFLASH",0);
#endif
#ifdef T_CRITIC_ECP
  GCST("T_CRITIC_ECP", T_CRITIC_ECP);
#else
  GDFLT("T_CRITIC_ECP",0);
#endif
#ifdef T_INETCONTROL
  GCST("T_INETCONTROL", T_INETCONTROL);
#else
  GDFLT("T_INETCONTROL",0);
#endif
#ifdef T_NETCONTROL
  GCST("T_NETCONTROL", T_NETCONTROL);
#else
  GDFLT("T_NETCONTROL",0);
#endif

   gcmnt("IP_TOS type of service");
#ifdef T_NOTOS
  GCST("T_NOTOS", T_NOTOS);
#else
  GDFLT("T_NOTOS",0);
#endif
#ifdef T_LDELAY
  GCST("T_LDELAY", T_LDELAY);
#else
  GDFLT("T_LDELAY",0);
#endif
#ifdef T_HITHRPT
  GCST("T_HITHRPT", T_HITHRPT);
#else
  GDFLT("T_HITHRPT",0);
#endif
#ifdef T_HIREL
  GCST("T_HIREL", T_HIREL);
#else
  GDFLT("T_HIREL",0);
#endif

  ghdrcmnt("link names for functions");

  GFUNC(t_accept, HAVE_t_accept);
  GFUNC(t_alloc, HAVE_t_accept);
  GFUNC(t_bind, HAVE_t_bind);
  GFUNC(t_blocking, HAVE_t_blocking);
  GFUNC(t_close, HAVE_t_close);
  GFUNC(t_connect, HAVE_t_connect);
  GFUNC(t_error, HAVE_t_error);
  GFUNC(t_free, HAVE_t_free);
  GFUNC(t_getinfo, HAVE_t_getinfo);
  GFUNC(t_getprotaddr, HAVE_t_getprotaddr);
  GFUNC(t_getstate, HAVE_t_getstate);
  GFUNC(t_listen, HAVE_t_listen);
  GFUNC(t_look, HAVE_t_look);
  GFUNC(t_nonblocking, HAVE_t_nonblocking);
  GFUNC(t_open, HAVE_t_open);
  GFUNC(t_optmgmt, HAVE_t_optmgmt);
  GFUNC(t_rcv, HAVE_t_rcv);
  GFUNC(t_rcvconnect, HAVE_t_rcvconnect);
  GFUNC(t_rcvdis, HAVE_t_rcvdis);
  GFUNC(t_rcvrel, HAVE_t_rcvrel);
  GFUNC(t_rcvreldata, HAVE_t_rcvreldata);
  GFUNC(t_rcvudata, HAVE_t_rcvudata);
  GFUNC(t_rcvuderr, HAVE_t_rcvuderr);
  GFUNC(t_rcvv, HAVE_t_rcvv);
  GFUNC(t_rcvvudata, HAVE_t_rcvvudata);
  GFUNC(t_snd, HAVE_t_snd);
  GFUNC(t_snddis, HAVE_t_snddis);
  GFUNC(t_sndudata, HAVE_t_sndudata);
  GFUNC(t_sndrel, HAVE_t_sndrel);
  GFUNC(t_sndreldata, HAVE_t_sndreldata);
  GFUNC(t_sndv, HAVE_t_sndv);
  GFUNC(t_sndvudata, HAVE_t_sndvudata);
  GFUNC(t_strerror, HAVE_t_strerror);
  GFUNC(t_sync, HAVE_t_sync);
  GFUNC(t_unbind, HAVE_t_unbind);

  fprintf(fp,"\n");
  ifprintf(fp,"end XTI;\n");
  indent--;

/* netinet/in.h
   ----------------
*/

  indent++;
  ifprintf(fp,"package Netinet is\n");

  ghdrcmnt("From netinet/in.h");

#ifdef IPPROTO_IP
  GUCST("IPPROTO_IP", IPPROTO_IP);
#else
  GDFLT("IPPROTO_IP", 0);
#endif
#ifdef IPPROTO_ICMP
  GUCST("IPPROTO_ICMP", IPPROTO_ICMP);
#else
  GDFLT("IPPROTO_ICMP", 0);
#endif
#ifdef IPPROTO_TCP
  GUCST("IPPROTO_TCP", IPPROTO_TCP);
#else
  GDFLT("IPPROTO_TCP", 0);
#endif
#ifdef IPPROTO_UDP
  GUCST("IPPROTO_UDP", IPPROTO_UDP);
#else
  GDFLT("IPPROTO_UDP", 0);
#endif
#ifdef IPPROTO_RAW
  GUCST("IPPROTO_RAW", IPPROTO_RAW);
#else
  GDFLT("IPPROTO_RAW", 0);
#endif
#ifdef IP_OPTIONS
  GUCST("IP_OPTIONS", IP_OPTIONS);
#else
  GDFLT("IP_OPTIONS", 0);
#endif
#ifdef IP_HDRINCL
  GUCST("IP_HDRINCL", IP_HDRINCL);
#else
  GDFLT("IP_HDRINCL", 0);
#endif
#ifdef IP_TOS
  GUCST("IP_TOS", IP_TOS);
#else
  GDFLT("IP_TOS", 0);
#endif
#ifdef IP_TTL
  GUCST("IP_TTL", IP_TTL);
#else
  GDFLT("IP_TTL", 0);
#endif
#ifdef IP_RECVDSTADDR
  GUCST("IP_RECVDSTADDR", IP_RECVDSTADDR);
#else
  GDFLT("IP_RECVDSTADDR", 0);
#endif

#ifdef INADDR_NONE
  GUCST("INADDR_NONE", INADDR_NONE);
#else
  GUFLT("INADDR_NONE", 0xffffffff);
#endif
#ifdef INADDR_ANY
  GUCST("INADDR_ANY",INADDR_ANY);
#else
  GDFLT("INADDR_ANY",0);
#endif
#ifdef INADDR_BROADCAST
  GUCST("INADDR_BROADCAST",INADDR_BROADCAST);
#else
  GDFLT("INADDR_BROADCAST",0);
#endif
#ifdef INADDR_LOOPBACK
  GUCST("INADDR_LOOPBACK",INADDR_LOOPBACK);
#else
  GDFLT("INADDR_LOOPBACK",0);
#endif
#ifdef INADDR_UNSPEC_GROUP
  GUCST("INADDR_UNSPEC_GROUP",INADDR_UNSPEC_GROUP);
#else
  GDFLT("INADDR_UNSPEC_GROUP",0);
#endif
#ifdef INADDR_ALLHOSTS_GROUP
  GUCST("INADDR_ALLHOSTS_GROUP",INADDR_ALLHOSTS_GROUP);
#else
  GDFLT("INADDR_ALLHOSTS_GROUP",0);
#endif
#ifdef INADDR_MAX_LOCAL_GROUP
  GUCST("INADDR_MAX_LOCAL_GROUP",INADDR_MAX_LOCAL_GROUP);
#else
  GDFLT("INADDR_MAX_LOCAL_GROUP",0);
#endif

  GFUNC(inet_addr, HAVE_inet_addr);
  GFUNC(inet_makeaddr, HAVE_inet_makeaddr);
  GFUNC(inet_network, HAVE_inet_network);
  GFUNC(inet_lnaof, HAVE_inet_lnaof);
  GFUNC(inet_netof, HAVE_inet_netof);
  GFUNC(inet_ntoa, HAVE_inet_ntoa);

   ghdrcmnt("From netinet/tcp.h");

#ifdef TCP_NODELAY
  GCST("TCP_NODELAY", TCP_NODELAY);
#else
  GDFLT("TCP_NODELAY",0);
#endif
#ifdef TCP_MAXSEG
  GCST("TCP_MAXSEG", TCP_MAXSEG);
#else
  GDFLT("TCP_MAXSEG",0);
#endif
#ifdef TCP_KEEPALIVE
  GCST("TCP_KEEPALIVE", TCP_KEEPALIVE);
#else
  GDFLT("TCP_KEEPALIVE",0);
#endif
#ifdef TCP_MAXRXT
  GCST("TCP_MAXRXT", TCP_MAXRXT);
#else
  GDFLT("TCP_MAXRXT",0);
#endif
#ifdef TCP_STDURG
  GCST("TCP_STDURG", TCP_STDURG);
#else
  GDFLT("TCP_STDURG",0);
#endif

   ghdrcmnt("From netinet/ip.h");

#ifdef IPTOS_LOWDELAY
  GCST("IPTOS_LOWDELAY", IPTOS_LOWDELAY);
#else
  GDFLT("IPTOS_LOWDELAY",0);
#endif
#ifdef IPTOS_THROUGHPUT
  GCST("IPTOS_THROUGHPUT", IPTOS_THROUGHPUT);
#else
  GDFLT("IPTOS_THROUGHPUT",0);
#endif
#ifdef IPTOS_RELIABILITY
  GCST("IPTOS_RELIABILITY", IPTOS_RELIABILITY);
#else
  GDFLT("IPTOS_RELIABILITY",0);
#endif

  fprintf(fp,"\n");
  ifprintf(fp,"end Netinet;\n");
  indent--;

/* netdb.h
   ----------------
*/

  indent++;
  ifprintf(fp,"package NetDB is\n");
  ifprintf(fp,"   use Sockets;\n");

  g_struct_netent();
  gcmnt("protocol database entry");
  g_struct_protoent();
  gcmnt("local socket address");

  GFUNC(endhostent, HAVE_endhostent);
  GFUNC(endnetent, HAVE_endnetent);
  GFUNC(endprotoent, HAVE_endprotoent);
  GFUNC(endservent, HAVE_endservent);
  /* Assume the following three are always there, since
     if getaddrinfo is not implemented we will provide our
     own freeware version.
   */
  GFUNC(getaddrinfo, 1);
  GFUNC(freeaddrinfo, 1);
  GFUNC(getnameinfo, 1);
  GFUNC(gethostbyaddr, HAVE_gethostbyaddr);
  GFUNC(gethostbyaddr_r, HAVE_gethostbyaddr_r);
  GFUNC(gethostbyname, HAVE_gethostbyname);
  GFUNC(gethostbyname_r, HAVE_gethostbyname_r);
  GFUNC(gethostname, HAVE_gethostname);
  GFUNC(getnetbyaddr, HAVE_getnetbyaddr);
  GFUNC(getnetbyaddr_r, HAVE_getnetbyaddr_r);
  GFUNC(getnetbyname, HAVE_getnetbyname);
  GFUNC(getnetbyname_r, HAVE_getnetbyname_r);
  GFUNC(getpeername, HAVE_getpeername);
  GFUNC(getprotobyname, HAVE_getprotobyname);
  GFUNC(getprotobyname_r, HAVE_getprotobyname_r);
  GFUNC(getprotobynumber, HAVE_getprotobynumber);
  GFUNC(getprotobynumber_r, HAVE_getprotobynumber_r);
  GFUNC(getservbyname, HAVE_getservbyname);
  GFUNC(getservbyname_r, HAVE_getservbyname_r);
  GFUNC(getservbyport, HAVE_getservbyport);
  GFUNC(getservbyport_r, HAVE_getservbyport_r);
  GFUNC(sethostent, HAVE_sethostent);
  GFUNC(setnetent, HAVE_setnetent);
  GFUNC(setprotoent, HAVE_setprotoent);
  GFUNC(setservent, HAVE_setservent);

  fprintf(fp,"\n");
  ifprintf(fp,"end NetDB;\n");
  indent--;

/*
 * Poll/Select
 */

  gcmnt("pollfd structure");
  g_struct_pollfd();

  ifprintf(fp,
     "   type fd_mask_array is array (Integer range <>) of unsigned_int;\n");
  gcmnt("fd_set structure");
  g_fd_set();

#ifdef FD_SETSIZE
  GCST("FD_SETSIZE", FD_SETSIZE);
#else
  GDFLT("FD_SETSIZE",0);
#endif
#ifdef INFTIM
  GCST("INFTIM", INFTIM);
#else
  GDFLT("INFTIM",0);
#endif

#ifdef POLLIN
  GCST("POLLIN", POLLIN);
#else
  GDFLT("POLLIN",0);
#endif
#ifdef POLLRDNORM
  GCST("POLLRDNORM", POLLRDNORM);
#else
  GDFLT("POLLRDNORM",0);
#endif
#ifdef POLLRDBAND
  GCST("POLLRDBAND", POLLRDBAND);
#else
  GDFLT("POLLRDBAND",0);
#endif
#ifdef POLLPRI
  GCST("POLLPRI", POLLPRI);
#else
  GDFLT("POLLPRI",0);
#endif
#ifdef POLLWRNORM
  GCST("POLLWRNORM", POLLWRNORM);
#else
  GDFLT("POLLWRNORM",0);
#endif
#ifdef POLLWRBAND
  GCST("POLLWRBAND", POLLWRBAND);
#else
  GDFLT("POLLWRBAND",0);
#endif
#ifdef POLLERR
  GCST("POLLERR", POLLERR);
#else
  GDFLT("POLLERR",0);
#endif
#ifdef POLLNVAL
  GCST("POLLNVAL", POLLNVAL);
#else
  GDFLT("POLLNVAL",0);
#endif

  GFUNC(poll, HAVE_poll);
  GFUNC(select, HAVE_select);

  fprintf(fp,"\n");
  ifprintf(fp,"end POSIX.C;\n");
  fclose(fp);
  fprintf(stderr,"done generating posix-c.ads\n");

} /* end create_c */

/*
   main
   ----
 */

int main() {

  /* Figure out the number of bits in a byte.
   */
  { unsigned char b;
    bits_per_byte = 0;
    b = 1;
    while (b) {
      bits_per_byte++;
      b = b << 1;
    }
  }
  if (bits_per_byte != 8) {
    quit("byte-size is not equal to 8\n","");
  }

  create_options();
  create_limits();
  create_posix();
  create_c();

  return 0;
}
