# makefile for Gnatsocks tests
#
LIBS = -lsocket -lnsl -largs
FLORISTDIR = /part9/baker/florist-3.10p
GNATMAKEFLAGS1 = -g -A$(FLORISTDIR)/floristlib -aO$(FLORISTDIR)/floristlib
GNATMAKEFLAGS2 = -cargs -gnatg -largs $(LIBS) -lflorist
TESTS = test_sockets sockettest echoserver multiecho multidb test_unix_sockets
SOURCES = sockettest.adb sockets-internet.ads sockets-internet.adb\
   sockets.ads sockets.adb sockets-unix.ads sockets-unix.adb\
   echoserver.adb \
   test_sockets.adb multiecho.adb test_pkg.ads test_pkg.adb\
   table.adb table.ads multidb.adb\
   test_unix_sockets.adb\
   Makefile READ.ME
all : $(TESTS)
$(TESTS):  %: %.adb \
  $(FLORISTDIR)/floristlib/libflorist.a test_parameters.ads test_parameters.adb
	gnatmake  $(GNATMAKEFLAGS1) $@ $(GNATMAKEFLAGS2)
clean:
	-rm -f *~ *# *.o *.ali b_*.c $(TESTS)


