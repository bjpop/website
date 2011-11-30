---
title: Linux Performance Counters
---

# Linux Performance Counters

Modern CPUs can provide information about the runtime behaviour of software through so-called [hardware
performance counters](http://en.wikipedia.org/wiki/Hardware_performance_counter).

It is rather hard to find authorative information about performance counters on Linux. First of all,
what is the official name? It could be "Performance Counters for Linux" or perhaps "Linux Perf Events".
There is the kernel interface via syscalls, and then the user-space tool called "perf".

## The perf command

[A short tutorial on using the perf command](http://www.baptiste-wicht.com/2011/07/profile-applications-linux-perf-tools/)

## Installing on Ubuntu 11.10

    apt-get install linux-tools

## Perf file format

The 'perf record' command records information about performance events in a file called (by default) 'perf.data'.
It is hard to find official documentation on the format (other than reading the source code of the perf tool).
Some documentation is available
in [perf file format](https://openlab-mu-internal.web.cern.ch/openlab-mu-internal/03_Documents/3_Technical_Documents/Technical_Reports/2011/Urs_Fassler_report.pdf).


## Alternatives

   * [PAPI](http://icl.cs.utk.edu/papi/)
   * [Perfctr](http://www.ale.csce.kyushu-u.ac.jp/~satoshi/how_to_use_perfctr.htm)
   * [oprofile](http://oprofile.sourceforge.net/)
   * [Sysprof](http://sysprof.com/)

****
