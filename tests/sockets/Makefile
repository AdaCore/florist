include ../../Config

GNATMAKEFLAGS = -c -gnatg -largs -L$(FLORISTLIBSDIR) $(LIBS)
GNATMAKEFLAGS2 = -largs -L$(FLORISTLIBSDIR) $(LIBS)

TEST_FILES=\
 test_tcp_listen.adb\
 test_tcp_talk.adb\
 test_addrinfo.adb\
 test_database.adb\
 test_local_listen.adb\
 test_local_ltime.adb\
 test_local_talk.adb\
 test_local_ttime.adb\
 test_poll_listen.adb\
 test_select_listen.adb\
 test_tcp_ltime.adb\
 test_tcp_options.adb\
 test_tcp_ttime.adb\
 test_udp_listen.adb\
 test_udp_options.adb\
 test_udp_talk.adb
TESTS=\
 test_tcp_listen\
 test_tcp_talk\
 test_addrinfo\
 test_database\
 test_local_listen\
 test_local_ltime\
 test_local_talk\
 test_local_ttime\
 test_poll_listen\
 test_select_listen\
 test_tcp_ltime\
 test_tcp_options\
 test_tcp_ttime\
 test_udp_listen\
 test_udp_options\
 test_udp_talk

all: $(TESTS)

tcp: test_tcp_listen test_tcp_talk

$(TESTS): $(TEST_FILES) $(POSIX_FILES)
	gnatmake $@ $(GNATMAKEFLAGS2)

clean:
	rm -f *.o *.ali $(TESTS)

