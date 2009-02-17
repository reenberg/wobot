#
# ERSP Player driver
#

lib_LTLIBRARIES = libersp.la

libersp_la_SOURCES  = ersp.cc ersp.h
libersp_la_CPPFLAGS = $(AM_CPPFLAGS)
libersp_la_CXXFLAGS = $(AM_CXXFLAGS) -Wall -I$(top_srcdir)/include @PLAYERCORE_CFLAGS@ @ERSP_CFLAGS@
libersp_la_LIBADD   = @ERSP_LIBS@
