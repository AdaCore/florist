include ../../Config

GNATMAKEFLAGS = -c -gnatg -largs -L$(FLORISTLIBSDIR) $(LIBS)
GNATMAKEFLAGS2 = -largs -L$(FLORISTLIBSDIR) $(LIBS)

TEST_FILES=\
 test_tcp_listen.adb\
 test_tcp_connect.adb

TESTS=\
 test_tcp_listen\
 test_tcp_connect
   
all: $(TESTS)

tcp: test_tcp_listen test_tcp_connect

$(TESTS): $(TEST_FILES) $(POSIX_FILES)
	gnatmake $@ $(GNATMAKEFLAGS2)

neat:
	co -l RCS/*

clean:
	rm -f *.o *.ali test_tcp_connect test_tcp_listen
