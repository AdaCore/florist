/*----------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POSIX.5) COMPONENTS            --
--                                                                          --
--                                                                          --
--                   C - P O S I X - S I G N A L S . C                      --
--                                                                          --
--                                                                          --
--  Copyright (c) 1998 Florida State University (FSU),           All Rights --
--  Reserved.                                                               --
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

/* file: c-posix-signals.c
   =======================
   [$Revision$]

   This program generates the file
   posix-implementation-ok_signals.ads,
   which is an Ada package specification that defines a
   list of signals that seem to work correctly with sigwait().

   The test is to mask a given signal, then use kill () to send the
   signal to the current process, then call sigwait () to receive the
   signal.

   The observed behavior for some signals is that the process hangs
   on sigwait, notably for signals whose default action is to ignore
   the signal or stop the process.

   In order to allow testing of more than one signal, the program
   tries to recover if sigwait() hangs.

   The first attempt was to use alarm() to arrange for another
   signal to be delivered, if sigwait() did not receive the tested
   signal within one second.  This seemed to have worked for SIGCHLD
   and SIGCONT, i.e. the signals whose default action is to ignore
   them.  It did not seem to work for SIGSTOP, SITTSTP, SIGTTIN,
   SIGTTOU, whose default action is to stop the process.

   In order to recover for these cases, we moved the test of
   sigwait() into a child process, and arranged for the parent
   process to send SIGCONT to the child if it stops.  This seems to
   solve the problem.

   When linking this test don't forget to use "-lpthread",
   or the appropriate thread library.

*/

#define _POSIX_C_SOURCE 199506L
#define _REENTRANT
#define _POSIX_C_SIGNALS_C
#include "pconfig.h"
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <pthread.h>
#include <string.h>

/* Uncomment the following only for debugging. */
/* #define DEBUG */

int sigs [] = {
   0,
#ifdef SIG32
   SIG32,
#endif
#ifdef SIGABRT
   SIGABRT,
#endif
#ifdef SIGALRM
   SIGALRM,
#endif
#ifdef SIGBUS
   SIGBUS,
#endif
#ifdef SIGCANCEL
   SIGCANCEL,
#endif
#ifdef SIGCHLD
   SIGCHLD,
#endif
#ifdef SIGCLD
   SIGCLD,
#endif
#ifdef SIGCONT
   SIGCONT,
#endif
#ifdef SIGEMT
   SIGEMT,
#endif
#ifdef SIGFPE
   SIGFPE,
#endif
#ifdef SIGFREEZE
   SIGFREEZE,
#endif
#ifdef SIGHUP
   SIGHUP,
#endif
#ifdef SIGILL
   SIGILL,
#endif
#ifdef SIGINT
   SIGINT,
#endif
#ifdef SIGIO
   SIGIO,
#endif
#ifdef SIGIOT
   SIGIOT,
#endif
#ifdef SIGKILL
   SIGKILL,
#endif
#ifdef SIGLOST
   SIGLOST,
#endif
#ifdef SIGLWP
   SIGLWP,
#endif
#ifdef SIGPIPE
   SIGPIPE,
#endif
#ifdef SIGPOLL
   SIGPOLL,
#endif
#ifdef SIGPROF
   SIGPROF,
#endif
#ifdef SIGQUIT
   SIGQUIT,
#endif
#ifdef SIGSEGV
   SIGSEGV,
#endif
#ifdef SIGSTKFLT
   SIGSTKFLT,
#endif
#ifdef SIGSYS
   SIGSYS,
#endif
#ifdef SIGTERM
   SIGTERM,
#endif
#ifdef SIGUSR1
   SIGUSR1,
#endif
#ifdef SIGUSR2
   SIGUSR2,
#endif
#ifdef SIGTRAP
   SIGTRAP,
#endif
#ifdef SIGSTOP
   SIGSTOP,
#endif
#ifdef SIGTHAW
   SIGTHAW,
#endif
#ifdef SIGTSTP
   SIGTSTP,
#endif
#ifdef SIGTTIN
   SIGTTIN,
#endif
#ifdef SIGTTOU
   SIGTTOU,
#endif
#ifdef SIGPWR
   SIGPWR,
#endif
#ifdef SIGURG
   SIGURG,
#endif
#ifdef SIGUNUSED
   SIGUNUSED,
#endif
#ifdef SIGVTALRM
   SIGVTALRM,
#endif
#ifdef SIGWAITING
   SIGWAITING,
#endif
#ifdef SIGWINCH
   SIGWINCH,
#endif
#ifdef SIGXCPU
   SIGXCPU,
#endif
#ifdef SIGXFSZ
   SIGXFSZ,
#endif
   0};

char *signames [] = {
   "SIGNONE",
#ifdef SIG32
   "SIG32",
#endif
#ifdef SIGABRT
   "SIGABRT",
#endif
#ifdef SIGALRM
   "SIGALRM",
#endif
#ifdef SIGBUS
   "SIGBUS",
#endif
#ifdef SIGCANCEL
   "SIGCANCEL",
#endif
#ifdef SIGCHLD
   "SIGCHLD",
#endif
#ifdef SIGCLD
   "SIGCLD",
#endif
#ifdef SIGCONT
   "SIGCONT",
#endif
#ifdef SIGEMT
   "SIGEMT",
#endif
#ifdef SIGFPE
   "SIGFPE",
#endif
#ifdef SIGFREEZE
   "SIGFREEZE",
#endif
#ifdef SIGHUP
   "SIGHUP",
#endif
#ifdef SIGILL
   "SIGILL",
#endif
#ifdef SIGINT
   "SIGINT",
#endif
#ifdef SIGIO
   "SIGIO",
#endif
#ifdef SIGIOT
   "SIGIOT",
#endif
#ifdef SIGKILL
   "SIGKILL",
#endif
#ifdef SIGLOST
   "SIGLOST",
#endif
#ifdef SIGLWP
   "SIGLWP",
#endif
#ifdef SIGPIPE
   "SIGPIPE",
#endif
#ifdef SIGPOLL
   "SIGPOLL",
#endif
#ifdef SIGPROF
   "SIGPROF",
#endif
#ifdef SIGQUIT
   "SIGQUIT",
#endif
#ifdef SIGSEGV
   "SIGSEGV",
#endif
#ifdef SIGSTKFLT
   "SIGSTKFLT",
#endif
#ifdef SIGSYS
   "SIGSYS",
#endif
#ifdef SIGTERM
   "SIGTERM",
#endif
#ifdef SIGUSR1
   "SIGUSR1",
#endif
#ifdef SIGUSR2
   "SIGUSR2",
#endif
#ifdef SIGTRAP
   "SIGTRAP",
#endif
#ifdef SIGSTOP
   "SIGSTOP",
#endif
#ifdef SIGTHAW
   "SIGTHAW",
#endif
#ifdef SIGTSTP
   "SIGTSTP",
#endif
#ifdef SIGTTIN
   "SIGTTIN",
#endif
#ifdef SIGTTOU
   "SIGTTOU",
#endif
#ifdef SIGPWR
   "SIGPWR",
#endif
#ifdef SIGURG
   "SIGURG",
#endif
#ifdef SIGUNUSED
   "SIGUNUSED",
#endif
#ifdef SIGVTALRM
   "SIGVTALRM",
#endif
#ifdef SIGWAITING
   "SIGWAITING",
#endif
#ifdef SIGWINCH
   "SIGWINCH",
#endif
#ifdef SIGXCPU
   "SIGXCPU",
#endif
#ifdef SIGXFSZ
   "SIGXFSZ",
#endif
   "SIGNONE"};

int namedsigs = sizeof (sigs) / sizeof (int);
int nsigs;
int *oksigs;
int *oksigs_nodefault;
int *oksigs_stop;

void comment (char const *msg) {
#ifdef DEBUG
   fprintf (stderr, "%s\n", msg);
   fflush (stderr);
#endif
}

void handler (int sig) {
   if (sig == SIGALRM) {
      comment ("==>   in handler for SIGALRM");
   } else {
      comment ("==>   in handler: unexpected signal");
      exit (-1);
   }
}

void handler_parent (int sig) {
   if (sig == SIGALRM) {
      comment ("==>   PARENT: in handler for SIGALRM");
   } else {
      comment ("==>   PARENT: in handler: unexpected signal");
      kill (0, SIGKILL);
      exit (-1);
   }
}

void print_package
  (int *oksigs, int *oksigs_nodefault, int *oksigs_stop) {
   int sig;
   FILE *fp;

   fprintf (stderr,"creating package POSIX.Implementation.OK_Signals\n");
   if (! (fp = fopen ("posix-implementation-ok_signals.ads", "w"))) {
      perror ("posix-implemenation-ok_signals.ads");
      exit (-1);
   }
   fprintf (fp, "package POSIX.Implementation.OK_Signals is\n");
   fprintf (fp, "\n");
   fprintf (fp, "   --  OK (Sig) = True iff we can use Sig"
     " with sigwait ().\n\n");
   fprintf (fp, "   OK : constant array (0 .. %d) of Boolean :=\n",
     nsigs - 1);
   fprintf (fp, "     (");
   for (sig = 0; sig < nsigs; sig++) {
      if (oksigs_nodefault[sig] == 1) fprintf (fp, " True");
      else  fprintf (fp, "False");
      if (sig == nsigs - 1) fprintf (fp, ");\n");
      else if (sig % 10 == 9) fprintf (fp, ",\n      ");
      else fprintf (fp, ", ");
   }
   fprintf (fp, "\n");
   fprintf (fp, "   --  Default_Is_Ignore (Sig) = True iff we need to"
     " override the default\n");
   fprintf (fp, "   --  treatment of Sig with a do-nothing handler"
     " before we try to\n");
   fprintf (fp, "   --  use sigwait() with it.\n\n");
   fprintf (fp, "   Default_Is_Ignore : constant array"
     " (0 .. %d) of Boolean :=\n",
     nsigs - 1);
   fprintf (fp, "     (");
   for (sig = 0; sig < nsigs; sig++) {
      if ((oksigs[sig] != 1) && (oksigs_nodefault[sig] == 1))
         fprintf (fp, " True");
      else  fprintf (fp, "False");
      if (sig == nsigs - 1) fprintf (fp, ");\n");
      else if (sig % 10 == 9) fprintf (fp, ",\n      ");
      else fprintf (fp, ", ");
   }
   fprintf (fp, "\n   --  Default_Is_Stop (Sig) = True iff the default"
     " action of Sig\n   --  is to stop the process.\n\n");
   fprintf (fp, "   Default_Is_Stop : constant array (0 .. %d)"
     " of Boolean :=\n",
     nsigs - 1);
   fprintf (fp, "     (");
   for (sig = 0; sig < nsigs; sig++) {
      if (oksigs_stop[sig] == 1)
         fprintf (fp, " True");
      else  fprintf (fp, "False");
      if (sig == nsigs - 1) fprintf (fp, ");\n");
      else if (sig % 10 == 9) fprintf (fp, ",\n      ");
      else fprintf (fp, ", ");
   }
   fprintf (fp, "\nend POSIX.Implementation.OK_Signals;\n");
   fclose (fp);
}

int guess_nsigs () {

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

   sigset_t set;
   int sig;
   int result;
   int last_good = -1;
   int first_bad = -1;

   sigfillset (&set);
   for (sig = 0; sig < 1024; sig++) {
      result = sigismember (&set, sig);
      if (result == 1) {
         last_good = sig;
      } else if ((result == -1) && (first_bad == -1)) {
         if (sig == 0) {
            fprintf (stderr, "WARNING: C library problem? "
             "sigfillset does not include zero\n");
         } else  first_bad = sig;
      }
   }
   if (last_good == 1023)
      fprintf (stderr, "WARNING: signal range estimate probably too small\n");
   if (first_bad < last_good) {
      fprintf (stderr, "WARNING: signal range estimate may be invalid\n");
      last_good = first_bad - 1;
   }
   return last_good + 1;
}

void parent_process(pid_t child, int *oksigs, int sig) {

/* Monitor the child process until it terminates.
   If the child process stops, wake it up.
   If the child process dies via signal, restart it and tell it to
   skip the last signal it was trying.
 */

   pid_t ret;
   int status;
   struct sigaction act;

   act.sa_flags = 0;
   act.sa_handler = handler_parent;
   if (sigaction (SIGALRM, &act, NULL)) perror ("sigaction (handler_parent)");

   while (1) {
      alarm (30);
      comment ("      PARENT: awaiting child status");
      fflush (stderr);
      ret = waitpid ( (pid_t) -1, &status, WUNTRACED);
      fflush (stderr);
      if (ret == -1) {
	 if (errno == EINTR) {
	    comment ("      PARENT: timed out, sending SIGCONT");
	    fflush (stderr);
	    kill (child, SIGCONT);
	    alarm (0);
	    sleep (1);
	    continue;
	 }
	 comment ("      PARENT: waitpid failed");
	 kill (0, SIGKILL);
	 exit (-1);
      }
      if (ret != child) {
	 comment ("      PARENT: unknown child");
	 kill (0, SIGKILL);
	 exit (-1);
      }
      if (WIFSTOPPED (status)) {
         oksigs_stop [sig] = 1;
	 comment ("      PARENT: sending SIGCONT to stopped child");
	 kill (child, SIGCONT);
	 alarm (0);
	 sleep (1);
	 continue;
      }
      if (WIFEXITED (status)) {
         if (WEXITSTATUS (status) != 1)
   	    comment ("      PARENT: child exited abnormally");
         else {
            oksigs [sig] = 1;
   	    comment ("      PARENT: child exited normally");
         }
         return;
      }
      if (WIFSIGNALED (status)) {
	 comment("      PARENT: child killed by signal");
	 fprintf (stderr, "child process killed by signal %d\n", WTERMSIG (status));
         return;
      }
      comment ("      PARENT: invalid child status %x");
      kill (0, SIGKILL);
      exit (-1);
   }
}

void test_signal (int nodefaults, int signal) {
   struct sigaction act;
   int sig;
   sigset_t  set, oset;
   int ret;

   act.sa_flags = 0;
   act.sa_handler = handler;

#ifdef LYNX_SIGTHREADKILL_HACK

   /* 
      Attempting to test SIGTHREADKILL on LynxOS will kill both 
      the child and the parent process: The call to sigaction fails
      and falls through to kill (0, SIGKILL).

      This code works around a problem specific to LynxOS, where
      signal 24 is reserved by the user space portion of the
      pthreads implementation.  It should be replaced by a general
      mechanism for skipping problematic signals.
   */

   if (signal == SIGTHREADKILL) {
     fprintf (stderr, "Reserved by C library.\n");
     fflush (stderr);
     exit (-1);
   }

#endif /* LYNX_SIGTHREADKILL_HACK */

   if (sigaction (signal, &act, NULL)) {
      if (errno == EINVAL) {
        fprintf (stderr, "cannot be caught\n");
        fflush (stderr);
        exit (-1);
      } else {
        fprintf (stderr, "  *** sigaction: %s\n", strerror (errno));
        fflush (stderr);
        kill (0, SIGKILL);
        exit (-1);
      }
   }
   if (! nodefaults) {
      act.sa_handler = SIG_IGN;
      if (sigaction (signal, &act, NULL)) {
         if (errno == EINVAL) {
            fprintf (stderr, "cannot be ignored\n");
            fflush (stderr);
	    exit (-1);
	 } else {
            fprintf (stderr, "  *** sigaction: %s\n", strerror (errno));
            fflush (stderr);
            kill (0, SIGKILL);
            exit (-1);
         }
      }
   }
   if (! nodefaults) {
     act.sa_handler = SIG_DFL;
     if (sigaction (signal, &act, NULL)) {
        fprintf (stderr, "  *** sigaction: %s\n", strerror (errno));
        fflush (stderr);
        kill (0, SIGKILL);
        exit (-1);
     }
   }
   if (sigemptyset (&set)) perror ("sigemptyset");
   if (sigaddset (&set, signal)) perror ("sigaddset");
   if (sigaddset (&set, SIGALRM)) perror ("sigaddset");
     comment ("  masking signal");
   if ((ret = pthread_sigmask (SIG_BLOCK, &set, NULL)))
     fprintf (stderr, "  *** pthread_sigmask (SIG_BLOCK): %s\n",
       strerror (ret));
   if ((ret = pthread_sigmask (SIG_BLOCK, &set, &oset)))
     fprintf (stderr, "  *** pthread_sigmask (SIG_BLOCK 2): %s\n",
       strerror (ret));
   if (sigismember (&oset, signal) == 0) {
     /* we are unable to mask this signal */
     fprintf (stderr, "cannot be masked.\n");
     exit (-1);
   }
   comment ("  sending signal to self");
   if (kill (getpid (), signal)) perror ("kill");
   comment ("  setting one second timeout");
   alarm (1);
   comment ("  doing sigwait");

#ifndef _CMA_OS_
   if (ret = sigwait (&set, &sig)) {
#else
   ret = sigwait (&set);
   if (ret == -1)
      ret = errno;
   else {
      sig = ret;
      ret = 0;
   }
   if (ret) {
#endif
     fprintf (stderr, "*** sigwait: %s\n", strerror (ret));
   } else if (sig == signal) {
     fprintf (stderr, "works OK.\n");
     exit (1);  /* signal is OK */
   } else if (sig == SIGALRM) {
     fprintf (stderr, "*** timed out\n");
   } else {
     fprintf (stderr, "*** wrong signal: %d\n", sig);
   }
   exit (-1);
}

void test_signals (int nodefaults, int *oksigs) {

/* Test each signal, to see if it works with sigwait().
   If nodefault != 0 then override the default treatment
   by installing a dummy handler.
 */

   int i;
   int signal;
   pid_t child;

   if (nodefaults) 
     fprintf (stderr, "testing signals without default treatments\n");
   else
     fprintf (stderr, "testing signals with default treatments\n");

   for (signal = 1; signal < nsigs; signal++) {
      for (i = 0; i < namedsigs; i++)
        if (sigs[i] == signal) break;
      if (i < namedsigs) 
        fprintf (stderr, "testing signal: %2d %10s ...", signal, signames[i]);
      else
        fprintf (stderr, "testing signal: %2d            ...", signal);
      fflush (stderr);
      if ((child = fork ()))
         parent_process (child, oksigs, signal);
      else {
         test_signal (nodefaults, signal);
         fprintf (stderr, "*** should never return ***");
         exit (-1);
      }
   }
}

int main (int argc, char *argv[]){

   int i;
   struct sigaction act;

   nsigs = guess_nsigs();
   fprintf (stderr, "nsigs: %d\n", nsigs);
   oksigs = (int *) malloc (nsigs * sizeof (int));
   oksigs_nodefault = (int *) malloc (nsigs * sizeof (int));
   oksigs_stop = (int *) malloc (nsigs * sizeof (int));
   for (i = 0; i < nsigs; i++)
     oksigs[i] = oksigs_nodefault[i] = oksigs_stop[i]= 0;

   act.sa_flags = 0;
   act.sa_handler = handler;
   if (sigaction (SIGALRM, &act, NULL)) perror ("sigaction (handler)");

#ifdef DEBUG
   comment ("sending 3 SIGALRM's, to test handler");
   fflush (stderr);
   kill (getpid (), SIGALRM);
   kill (getpid (), SIGALRM);
   kill (getpid (), SIGALRM);
#endif

   test_signals (0, oksigs);
   test_signals (1, oksigs_nodefault);
   print_package (oksigs, oksigs_nodefault, oksigs_stop);
   fprintf (stderr, "done.\n");
   return 0;
}
