include ../../Config

GNATMAKEFLAGS1 = -g -I../../floristlib -L../../floristlib
GNATMAKEFLAGS2= -cargs -gnatay -gnatwue -largs -lresolv -lflorist

TEST_FILES=\
 p180400.adb\
 p180401.adb\
 p180402.adb\
 p180402a.adb\
 p180402b.adb\
 p180402c.adb\
 p180402d.adb\
 pdd0100.adb\
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
 p180400\
 p180401\
 p180402\
 p180402a\
 p180402b\
 p180402c\
 p180402d\
 pdd0100\
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
posix_report.ads:
	ln -s ../posix_report.ads
posix_report.adb:
	ln -s ../posix_report.adb
test_parameters.ads:
#	ln -s ../test_parameters.ads
test_parameters.adb:
#	ln -s ../test_parameters.adb
p180400: p180400.adb
p180401: p180401.adb
p180402: p180402.adb
p180402a: p180402a.adb
p180402b: p180402b.adb
p180402c: p180402c.adb
p180402d: p180402d.adb
pdd0100: pdd0100.adb test_parameters.adb
test_tcp_listen: test_tcp_listen.adb
test_tcp_talk: test_tcp_talk.adb
test_addrinfo: test_addrinfo.adb
test_database: test_database.adb
test_local_listen: test_local_listen.adb
test_local_ltime: test_local_ltime.adb
test_local_talk: test_local_talk.adb
test_local_ttime: test_local_ttime.adb
test_poll_listen: test_poll_listen.adb
test_select_listen: test_select_listen.adb
test_tcp_ltime: test_tcp_ltime.adb
test_tcp_options: test_tcp_options.adb
test_tcp_ttime: test_tcp_ttime.adb
test_udp_listen: test_udp_listen.adb
test_udp_options: test_udp_options.adb
test_udp_tal: test_udp_tal.adb
tcp: test_tcp_listen test_tcp_talk

$(TESTS):  posix_report.ads posix_report.adb\
  ../../floristlib/libflorist.a test_parameters.ads test_parameters.adb
	gnatmake  $(GNATMAKEFLAGS1) $@ $(GNATMAKEFLAGS2)
clean:
	rm -f *.o *.ali $(TESTS)
