# file: Makefile for POSIX 1003.5b Test Suite
# [$Revision$]
#
# This builds the test suite for POSIX 1003.5 and 1003.5b.
# This version is for use with the GNAT Ada compiler
# and the Florist implementation of POSIX 1003.5b.
# This Makefile requires Gnu make.
#
# .POSIX:
#
include ../Config
MASTERDIR=/part1/florist/tests
PAVT_VERSION=pavt-1.3
GNATMAKEFLAGS1 = -g -A../../gnarl -A../floristlib -L../floristlib
GNATMAKEFLAGS2= -cargs -gnatg -gnata -largs $(LIBS) -lflorist
GNATMAKEFLAGS2a= -cargs -gnata -largs $(LIBS) -lflorist
GNATPREPFLAGS = -r
TOOLS =\
 renumber.adb\
 run_tests_1\
 run_tests_2
DOCS =\
 README\
 NOTES.LINUX\
 NOTES.SOLARIS\
 NOTES
TEST_SOURCES = \
 posix_report.ads\
 posix_report.adb\
 test_parameters.ads\
 test_parameters.adb\
 p020400.adb\
 p020400.ads\
 p021000.adb\
 p021000.ads\
 p030100.adb\
 p030100.ads\
 p030100b.adb\
 p030100b.ads\
 p030101.adb\
 p030101.ads\
 p030101b.adb\
 p030101b.ads\
 p030200.adb\
 p030200.ads\
 p030102.adb\
 p030102.ads\
 p030300.adb\
 p030300.ads\
 p030300a.adb\
 p030300a.ads\
 p030301.adb\
 p030301.ads\
 p030301b.adb\
 p030301b.ads\
 p030302.adb\
 p030302.ads\
 p030303.adb\
 p030303.ads\
 p030304.adb\
 p030304.ads\
 p030305.adb\
 p030305.ads\
 p030306.adb\
 p030306.ads\
 p030306a.adb\
 p030306a.ads\
 p040100.adb\
 p040100.ads\
 p040101.adb\
 p040101.ads\
 p040300.adb\
 p040300.ads\
 p040301.adb\
 p040301.ads\
 p050100.adb\
 p050100.ads\
 p050200.adb\
 p050200.ads\
 p050300.adb\
 p050300.ads\
 p060100.adb\
 p060100.ads\
 p060200.adb\
 p060200.ads\
 p060300.adb\
 p060300.ads\
 p070200.adb\
 p070200.ads\
 p090100.adb\
 p090100.ads\
 p090200.adb\
 p090200.ads\
 p110100.adb\
 p110100.ads\
 p110101.adb\
 p110101.ads\
 p110200.adb\
 p110200.ads\
 p110201.adb\
 p110201.ads\
 p110300.adb\
 p110300.ads\
 p120100.adb\
 p120100.ads\
 p120101.adb\
 p120101.ads\
 p120200.adb\
 p120200.ads\
 p120300.adb\
 p120300.ads\
 p120400.adb\
 p120400.ads\
 p120500.adb\
 p120500.ads\
 p120501.adb\
 p120501.ads\
 p120502.adb\
 p120502.ads\
 p120502a.adb\
 p120502a.ads\
 p140100.adb\
 p140100.ads\
 p140101.adb\
 p140101.ads\
 p150100.adb\
 p150100.ads\
 p150100b.adb\
 p150100b.ads\
 p150101.adb\
 p150101.ads\
 p9900x0.ads\
 p9900x0.adb\
 p990000.adb\
 p990000.ads\
 p99000x0.ads\
 p99000x0.adb\
 p990001a.adb\
 p990001a.ads\
 p990001b.adb\
 p990001b.ads\
 p990001c.adb\
 p990001c.ads\
 p990002a.adb\
 p990002a.ads\
 p990002b.adb\
 p990002b.ads\
 p990002c.adb\
 p990002c.ads\
 p990003a.adb\
 p990003a.ads\
 p990003b.adb\
 p990003b.ads\
 p990010.adb\
 p990010.ads\
 p990010a.ads\
 p990020.adb\
 p990020.ads\
 p990020a.ads\
 p990030.adb\
 p990030.ads\
 p990030a.ads\
 p990040.adb\
 p990040.ads\
 p990040a.ads\
 p990040b.adb\
 p990040b.ads\
 p9900doc.ads
TESTS =\
 p021000\
 p030100\
 p030100b\
 p030101\
 p030101b\
 p030200\
 p030301\
 p030301b\
 p030303\
 p030304\
 p030305\
 p030306\
 p030306a\
 p040100\
 p040101\
 p040300\
 p040301\
 p050100\
 p050200\
 p050300\
 p060100\
 p060200\
 p060300\
 p070200\
 p090100\
 p090200\
 p110101\
 p110200\
 p110201\
 p110300\
 p120100\
 p120101\
 p120200\
 p120300\
 p120400\
 p120500\
 p120501\
 p120502\
 p120502a\
 p150100b\
 p150101
SPECIAL_TESTS=\
 p020400\
 p030300\
 p140100\
 p150100\
 p990010\
 p990020\
 p990030
#
tests: $(TESTS) $(SPECIAL_TESTS)
#
# Tests in this group can be built by a single call to "gnatmake".
#
# If you are not using Gnu make, you may substitute the following
# simpler rule, but it will force unnecessary recompilations.
# $(TESTS):  $(TEST_SOURCES)
#
$(TESTS):  %: %.adb posix_report.ads posix_report.adb\
  ../floristlib/libflorist.a test_parameters.ads test_parameters.adb
	gnatmake  $(GNATMAKEFLAGS1) $@ $(GNATMAKEFLAGS2)
#
# Due to zealousness of the "-gnatg" style-checker,
# the following tests need to be compiled without "-gnatg".
#
$(SPECIAL_TESTS):  %: %.adb posix_report.ads posix_report.adb\
  ../floristlib/libflorist.a test_parameters.ads test_parameters.adb
	gnatmake  $(GNATMAKEFLAGS1) $@ $(GNATMAKEFLAGS2a)
#
# Make a separate directory in which to run the tests,
# so we have less chance of a wild test trashing our sources.
#
test.dir:
	mkdir test.dir
	ln -s .. test.dir/bin
#
# Compile all the tests
#
# Run all the tests
# Some tests require that the standard error file be a terminal.
run_tests: $(TESTS) test.dir
	-cd test.dir; ../run_tests_1 >> run_tests_1.log
#
# -----------------------------------
# Maintenance targets.
# -----------------------------------
#
# remove editor and compiler generated files
clean:
	rm -f *.o *.ali a.out *# *~ $(LITTER) $(TESTS) $(SPECIAL_TESTS) b_*.c *.log
	touch florist
# remove all generated files
realclean:
	rm -f *.o *.ali $(TESTS) $(SPECIAL_TESTS) \
             a.out *# *~ b_*.c $(GENERATED) $(LITTER)
#
# The following targets are only for use by the implementors.
#
# check all sources into RCS directory
# and remove everything except "Makefile"
distclean:
	rm -f *.o *.ali $(TESTS) $(SPECIAL_TESTS) \
          a.out *# *~ b_*.c $(GENERATED) $(LITTER)
	rm -rf test.dir
	-ci -m"make distclean" $(TEST_SOURCES) $(TOOLS) Makefile $(DOCS)
	-co Makefile
# check out all sources, ready for editing
neat:
	-co -l $(TEST_SOURCES) $(TOOLS) $(DOCS)
	-co -l Makefile
# check out all sources, read-only
checkallout:
	-co $(TEST_SOURCES) $(TOOLS) $(DOCS)
	-co Makefile
	rm -f $(GENERATED)
# make tarfile of just the POSIX.5b validation tests
distribution: make_test_links
	rm -f ${PAVT_VERSION}.tar.gz
	./make_test_links ./${PAVT_VERSION} $(MASTERDIR)
	tar cvhf ${PAVT_VERSION}.tar ${PAVT_VERSION}
	gzip ${PAVT_VERSION}.tar
rts: s-interr.adb s-intman.adb s-inmaop.adb
	gnatmake  -a $(GNATMAKEFLAGS1) dummy $(GNATMAKEFLAGS2a)
