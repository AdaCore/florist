dnl auto-configuration macros for Florist
dnl This version requires autoconf-2.10.

dnl AC_POSIX_HEADER(HEADER-FILE,
dnl   [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
dnl side-effect: extend file "pconfig.h",
dnl with includes for this header, if present.
AC_DEFUN(AC_POSIX_HEADER,
[dnl Do the transliteration at runtime so arg 1 can be a shell variable.
ac_old_cflags=$CFLAGS
# CFLAGS="$CFLAGS -Werror"
ac_safe=`echo "$1" | tr './\055' '___'`
AC_MSG_CHECKING([for $1])
AC_CACHE_VAL(ac_cv_header_$ac_safe,
[AC_TRY_CPP([#include <$1>], 
AC_TRY_COMPILE([#include "pconfig.h"
#include <$1>],,
eval "ac_cv_header_$ac_safe=yes",
eval "ac_cv_header_$ac_safe=no"),
eval "ac_cv_header_$ac_safe=no")])dnl
if eval "test \"`echo '$ac_cv_header_'$ac_safe`\" = yes"; then
  AC_MSG_RESULT(yes)
  echo "#include <$1>" >> pconfig.h
  ifelse([$2], , :, [$2])
else
  AC_MSG_RESULT(no)
ifelse([$3], , , [$3
])dnl
fi
CFLAGS=$ac_old_cflags
])dnl

dnl AC_POSIX_HEADERS(NAMES...)
dnl side-effect: create file "pconfig.h" containing includes
dnl for all the headers that are present.
AC_DEFUN(AC_POSIX_HEADERS,
[rm -f pconfig.h
cp pconfig.h.in pconfig.h
chmod 644 pconfig.h
if ( test ! -f pconfig.h ) then
   AC_MSG_ERROR(missing pconfig.h);
fi
for ac_hdr in $1
do
  AC_POSIX_HEADER($ac_hdr,
[changequote(, )dnl
  ac_tr_hdr=HAVE_`echo $ac_hdr |
   tr 'abcdefghijklmnopqrstuvwxyz./\055' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ___'`
changequote([, ])dnl
  AC_DEFINE_UNQUOTED($ac_tr_hdr)])dnl
done
])dnl

dnl AC_POSIX5C_HEADERS(NAMES...)
dnl side-effect: create file "pconfig.h" containing includes
dnl for all the headers that are present.
AC_DEFUN(AC_POSIX5C_HEADERS,
[for ac_hdr in $1
do
  AC_POSIX_HEADER($ac_hdr,
[changequote(, )dnl
  ac_tr_hdr=HAVE_`echo $ac_hdr |
    tr 'abcdefghijklmnopqrstuvwxyz./\055' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ___'`
changequote([, ])dnl
  AC_DEFINE_UNQUOTED($ac_tr_hdr)])dnl
done
AC_POSIX_HEADER(xti.h, AC_DEFINE_UNQUOTED(HAVE_XTI_H)
[    echo "-- don't want TLI because we have xti.h
TLI := False" >> gnatprep.config;],
AC_POSIX_HEADER(tli.h, AC_DEFINE_UNQUOTED(HAVE_TLI_H)
[    echo "-- using TLI because could not find xti.h
TLI := True" >> gnatprep.config;],
[    echo "-- could not find tli.h
TLI := False" >> gnatprep.config;],
))
AC_TRY_COMPILE([#include "pconfig.h"],
[  struct msghdr hdr;
   hdr.msg_controllen = 0;],
[echo "Socket interface Looks like BSD 4.4";

 # Put BSD flag in gnatprep.config
 if (grep BSD4_3 gnatprep.config >/dev/null 2>&1); then true;
 else
    echo "-- set BSD4_3 to False if using 4.4 style socket msghdr"
        >> gnatprep.config
    echo "BSD4_3 := False" >> gnatprep.config;
 fi;
 if (grep _BSD4_4_ pconfig.h >/dev/null 2>&1); then true;
 else
   echo "#define _BSD4_4_" >> pconfig.h;
 fi;],
[echo "Socket interface Looks like BSD 4.3";
 if (grep BSD4_3 gnatprep.config >/dev/null 2>&1); then true;
 else
    echo "-- set BSD4_3 to False if using 4.4 style socket msghdr
    BSD4_3 := True" >> gnatprep.config;
 fi;
 if (grep _BSD4_3_ pconfig.h >/dev/null 2>&1); then true;
 else
   echo "#define _BSD4_3_" >> pconfig.h;
 fi;])
[if (grep xti.h pconfig.h >/dev/null 2>&1); then
 if (grep _XTI_ pconfig.h >/dev/null 2>&1); then true;
 else
   echo "#define _XTI_" >> pconfig.h;
 fi ;
else
 if [ -f /usr/include/sys/tiuser.h ]; then
   echo "Have only TLI, will use that in place of XTI";
   if (grep _TLI_ pconfig.h >/dev/null 2>&1); then true; 
   else
     echo "#define _TLI_" >> pconfig.h;
     echo "#include <sys/tiuser.h>" >> pconfig.h;
   fi;
 fi;
fi]
AC_CHECK_FUNC(getaddrinfo,
[ADDRINFO_OBJECTS=""
 AC_SUBST(ADDRINFO_OBJECTS)
],
[AC_MSG_WARN(No getaddrinfo. Will try to use shareware.)
 ADDRINFO_OBJECTS="getaddrinfo.o inet_pton.o inet_ntop.o"
 AC_SUBST(ADDRINFO_OBJECTS)
 echo '#include "addrinfo.h"' >> pconfig.h;
])
])dnl

dnl AC_POSIX_LIBS(LIBRARY..., FUNCTION
dnl    [, ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
AC_DEFUN(AC_POSIX_LIBS,
[for ac_lib in $1
do
   AC_POSIX_LIB($ac_lib,$2,ac_lib_success="yes",ac_lib_success="no")
   if [[ "$ac_lib_success" = "yes" ]]
   then break;
   fi
done])

dnl AC_POSIX_LIB(LIBRARY, FUNCTION [, ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
AC_DEFUN(AC_POSIX_LIB,
[AC_MSG_CHECKING([for $2 in -l$1])
dnl Use a cache variable name containing both the library and function name,
dnl because the test really is for library $1 defining function $2, not
dnl just for library $1.  Separate tests with the same $1 and different $2s
dnl may have different results.
ac_lib_var=`echo $1[_]$2 | tr ':.-/+' '____p'`
ac_save_LIBS="$LIBS"
LIBS="-l$1 $LIBS"
AC_TRY_LINK([/* Override any gcc2 internal prototype to avoid an error.  */
]ifelse(AC_LANG, CPLUSPLUS, [#ifdef __cplusplus
extern "C"
#endif
])dnl
[char $2();
],
  [$2()],
  eval "ac_cv_lib_$ac_lib_var=yes",
  eval "ac_cv_lib_$ac_lib_var=no")
LIBS="$ac_save_LIBS"
if eval "test \"`echo '$ac_cv_lib_'$ac_lib_var`\" = yes"; then
  AC_MSG_RESULT(yes)
if echo ${LIBS} | grep $1; then true;
else
  LIBS="-l$1 ${LIBS}"
fi
ifelse([$3], , , [$3
])dnl
else
  AC_MSG_RESULT(no)
ifelse([$4], , , [$4
])dnl
fi
])

dnl AC_POSIX_TYPE(TYPE-NAME)
AC_DEFUN(AC_POSIX_TYPE,
[AC_REQUIRE([AC_POSIX_HEADERS])dnl
AC_MSG_CHECKING(for $1)
AC_CACHE_VAL(ac_cv_type_$1,
AC_EGREP_CPP($1[[[^0-9A-Za-z_]]],[#include "pconfig.h"],
eval "ac_cv_type_$1=yes", eval "ac_cv_type_$1=no"))
if eval "test \"`echo '$ac_cv_type_'$1`\" = yes"; then
  AC_DEFINE_UNQUOTED(HAVE_$1)
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
fi
])

dnl AC_POSIX_TYPES(TYPE-NAME...)
AC_DEFUN(AC_POSIX_TYPES,
[for ac_typ in $1
do
  AC_POSIX_TYPE($ac_typ)
done
])

dnl AC_POSIX_CONST(CONST)
AC_DEFUN(AC_POSIX_CONST,
[AC_REQUIRE([AC_POSIX_HEADERS])dnl
AC_MSG_CHECKING(for $1)
AC_CACHE_VAL(ac_cv_const_$1,
[AC_EGREP_CPP($1, [#include "pconfig.h"], eval "ac_cv_const_$1=yes",
AC_EGREP_CPP(yes, [#include "pconfig.h"
#ifdef $1
  yes
#endif], eval "ac_cv_const_$1=yes",
eval "ac_cv_const_$1=no"))])dnl
if eval "test \"`echo '$ac_cv_const_'$1`\" = yes"; then
  AC_DEFINE_UNQUOTED(HAVE_$1)
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
fi
])

dnl AC_POSIX_CONSTS(CONST-NAME...)
AC_DEFUN(AC_POSIX_CONSTS,
[for ac_const in $1
do
  AC_POSIX_CONST($ac_const)
done
])

dnl AC_POSIX_STRUCT(NAME,
dnl   [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
AC_DEFUN(AC_POSIX_STRUCT,
[AC_REQUIRE([AC_POSIX_HEADERS])dnl
AC_MSG_CHECKING(for struct $1)
AC_CACHE_VAL(ac_cv_struct_$1,
[AC_TRY_COMPILE([#include "pconfig.h"
struct $1 x;],,eval "ac_cv_struct_$1=yes",
 eval "ac_cv_struct_$1=no")])dnl
if eval "test \"`echo '$ac_cv_struct_'$1`\" = yes"; then
  AC_DEFINE_UNQUOTED(HAVE_struct_$1)
  AC_MSG_RESULT(yes)
  ifelse([$2], , :, [$2])
else
  AC_MSG_RESULT(no)
  ifelse([$3], , , [$3])dnl
fi
])

dnl AC_POSIX_STRUCTS(NAME...)
AC_DEFUN(AC_POSIX_STRUCTS,
[for ac_struct in $1
do
  AC_POSIX_STRUCT($ac_struct)
done
])

dnl AC_POSIX_FUNCS(FUNCTION... )
AC_DEFUN(AC_POSIX_FUNCS,
[for ac_func in $1
do
AC_CHECK_FUNC($ac_func,
[AC_DEFINE_UNQUOTED(HAVE_$ac_func,1)],
[AC_DEFINE_UNQUOTED(HAVE_$ac_func,0)])dnl
done
])

dnl AC_POSIX_VAR(NAME)
AC_DEFUN(AC_POSIX_VAR,
[AC_REQUIRE([AC_POSIX_HEADERS])dnl
AC_MSG_CHECKING(for global variable or macro $1)
AC_CACHE_VAL(ac_cv_comp_$1,
[AC_EGREP_CPP($1, [#include "pconfig.h"], eval "ac_cv_comp_$1=yes",
eval "ac_cv_comp_$1=no")])dnl
if eval "test \"`echo '$ac_cv_comp_'$1`\" = yes"; then
  AC_DEFINE_UNQUOTED(HAVE_$1)
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
fi
])

dnl AC_POSIX_COMP(STRUCTNAME, COMPNAME)
AC_DEFUN(AC_POSIX_COMP,
[AC_REQUIRE([AC_POSIX_HEADERS])dnl
AC_MSG_CHECKING(for struct $1 component $2)
AC_CACHE_VAL(ac_cv_comp_$2,
[AC_TRY_COMPILE([#include "pconfig.h"
struct $1 x;],
[x.$2 = x.$2;], eval "ac_cv_comp_$2=yes",
eval "ac_cv_comp_$2=no")])dnl
if eval "test \"`echo '$ac_cv_comp_'$2`\" = yes"; then
  AC_DEFINE_UNQUOTED(HAVE_component_$2)
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
fi
])

dnl AC_POSIX_COMP_OVERLAY(STRUCTNAME, COMPNAME1, COMPNAME2)
dnl check for COMPNAME1 but only if it does not overlay in memory
dnl layout with COMPNAME2; e.g. see overlaying of sigaction components
dnl sa_handler and sa_sigaction
AC_DEFUN(AC_POSIX_COMP_OVERLAY,
[AC_REQUIRE([AC_POSIX_HEADERS])dnl
AC_MSG_CHECKING(for struct $1 component $2 overlaying $3)
AC_CACHE_VAL(ac_cv_comp_$2,
AC_TRY_RUN([#include "pconfig.h"
main()
{
  struct $1 x;
  if (&x.$2 == &x.$3) {
    fprintf(stderr,"$2 overlays $3...");
    exit (1);
  } else {
    exit (0);
  }
}], eval "ac_cv_comp_$2=yes",
eval "ac_cv_comp_$2=no", eval "ac_cv_comp_$2=nu"))dnl
if eval "test \"`echo '$ac_cv_comp_'$2`\" = yes"; then
  AC_DEFINE_UNQUOTED(HAVE_component_$2)
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
fi
])
