---
title: Linux Performance Counters
---

# Linux Performance Counters

Modern CPUs can provide information about the runtime behaviour of software through so-called [hardware
performance counters](http://en.wikipedia.org/wiki/Hardware_performance_counter). Recent versions of
the Linux kernel (since 2.6.31) provide a generic interface to low-level events for running processes.
This includes access to hardware counters but also a wide array of *software events* such as page faults,
scheduling activity and system calls. A userspace tool called `perf` is built on top of the kernel interface,
which provides a convenient way to record and view events for running processes. 

Unfortunately it is rather hard to find authorative information about performance counters on Linux;
documentation is scarce. Here's a list of links that provide some information:

   * [Source perf code in the Linux Kernel](https://github.com/torvalds/linux/tree/master/tools/perf).
   * [Unofficial Linux Perf Events Web-Page](http://web.eecs.utk.edu/~vweaver1/projects/perf-events),
which is motivated by the lack of official documentation for the project.
   * [A short tutorial on using the perf command.](http://www.baptiste-wicht.com/2011/07/profile-applications-linux-perf-tools/)
   * [A detailed description of the perf file format](https://openlab-mu-internal.web.cern.ch/openlab-mu-internal/03_Documents/3_Technical_Documents/Technical_Reports/2011/Urs_Fassler_report.pdf).
   * [Mailing list for discussion of the perf tool.](http://dir.gmane.org/gmane.linux.kernel.perf.user)

## Installing on Ubuntu 11.10

    apt-get install linux-tools

### Alternatives

   * [PAPI](http://icl.cs.utk.edu/papi/)
   * [Perfctr](http://www.ale.csce.kyushu-u.ac.jp/~satoshi/how_to_use_perfctr.htm)
   * [oprofile](http://oprofile.sourceforge.net/)
   * [Sysprof](http://sysprof.com/)
   * [Dtrace](http://dtrace.org) on Solaris, OS X, FreeBSD, NetBSD, Linux.
   * [LTTng](http://lttng.org/) Linux Trace Toolkit - next generation.

## The perf command

The perf tool has many sub-commands which do a variety of things, but in general it has two main
purposes:

   1. Recording events.
   2. Displaying events.

Example record command:

        perf record -a -R -c 1 -e sched:sched_switch -e sched:sched_stat_wait -- foobar options

where `foobar options` is the command line that you want to run.

Explanation of the arguments in the example:

<pre>
   -a System-wide collection from all CPUs
   -c Event period to sample (in the above case record every single event, which is useful for getting schedule events).
   -R Collect raw sample records from all opened counters (default for tracepoint counters).
   -e Select the PMU event.
</pre>

## Perf file format

The `perf record` command records information about performance events in a file called (by default) `perf.data`.
It is a binary file format which is basically a memory dump of the data structures used to record event
information. The file has two main parts:

   1. A header which describes the layout of information in the file (section sizes etcetera) and common information
      about events in the second part of the file (an encoding of event types and their names).
   2. The payload of the file which is a sequence of event records.

Each event field has a header which says what general type of event it is plus information about the size of its body.

   There are nine types of event:

   1. `PERF_RECORD_MMAP`: 
   2. `PERF_RECORD_LOST`: an unknown event?
   3. `PERF_RECORD_COMM`: maps a command name string to a process and thread ID. Perhaps this corresponds to an exec?
   4. `PERF_RECORD_EXIT`: process exit.
   5. `PERF_RECORD_THROTTLE`
   6. `PERF_RECORD_UNTHROTTLE`
   7. `PERF_RECORD_FORK`: process creation.
   8. `PERF_RECORD_READ`:
   9. `PERF_RECORD_SAMPLE`: a sample of an actual hardware counter or a software event.

The `PERF_RECORD_SAMPLE` events (samples) are the most interesting ones in terms of program profiling. The other events
seem to be mostly useful for keeping track of process technicalities. Samples are timestamped with an unsigned 64 bit
word, which records elapsed nanoseconds since some point in time (I'm not exactly sure what, perhaps system running
time, since it seems to be based on the kernel scheduler clock). In fact the other events can be optionally timestamped
if a certain bit flag is set in the file header (`sample_id_all`). Samples have themselves a "type" which is defined
in the file header and linked to the sample by an integer identifier.

### Processing the perf file format from Haskell

There are three ways one might go about writing a tool to process the `perf.data` file from Haskell:

   1. Write a program to read the data directly from file.
   2. Call the perf code as a library.
   3. Parse the output of the `perf script` command.

Option 1 is what we have already done with the [haskell-linux-perf](https://github.com/bjpop/haskell-linux-perf)
library. The upside of this approach is
that it is independent of the existing perf source code. The downside is that the `perf.data` format is complicated
and largely undocumented. It does not seem to be designed for external tools to read. 
It might be hard to keep a custom parser
compatible with the format.

For option 2, the most likely interface to use would be the C function (from `<perf source>/util/session.c`):

    int perf_session__process_events(struct perf_session *self, struct perf_event_ops *ops)

The `struct perf session` is a representation of the `perf.data` file and the `struct perf_event_ops`
contains a collection of functions for processing each of the event types. One could envisage a Haskell FFI call
that would get `perf_session__process_events` to build a Haskell representation of the event list, but it seems
like it would have to build it all in memory at once, rather than stream the data lazily.

Option 3 is appealing because it avoids the need to deal with the `perf.data` file directly: we just need to parse 
the text output of `perf script`. One small downside is that it entails a double handling of the data,
which might be a little bit slower than reading `perf.data` directly. Another downside is that it means we
are at the whim of the format of the output of `perf script`, which could change at any point.

### Event timestamps

Certain events carry a timestamp in the form of a unsigned 64 bit integer. Time measurements come from the kernel function `perf_clock` in `linux/kernel/events/core.c`, which simply calls `local_clock`. On x86 systems with a 
stable [Time Stamp Counter](http://en.wikipedia.org/wiki/Time_Stamp_Counter) this ends up calling `native_sched_clock`
in `linux/arch/x86/kernel/tsc.c`. You might be able to read the value of this clock in userspace with a call to
`clock_gettime(CLOCK_MONOTONIC, ...)`, although I'm not sure it is guaranteed to line up with the counter that perf
uses.

## Extending ThreadScope to support perf events

How to incorporate perf event information into the GHC event format used by ThreadScope?

These are the design issues I can think of:

   1. How to synchronise the perf timestamps with the time format used by GHC events?
   2. How to encode the perf events in the GHC event log format?

### Synchronising timestamps

The perf timestamps are based on some kind of internal clock, perhaps the TSC hardware. 
A simple approach would be to just synchronise the timestamps at the start
of the profiled program, but this runs the risk of them drifting apart over the length of the computation.
It is not clear how CPU frequency scaling affects the perf clock, and the clock probably doesn't
run if the CPU core hibernates. For these reasons it is probably necessary to synchronise the two timestamps
on a regular basis throughout the execution of the traced program.

There are a number of ways we might try to synchronise the timestamps:

   * Look for a known sys-call in the perf event log which happens at a known time-of-day. For example, we could look
     for calls to `gettimeofday`. Alternatively, it might be possible to get the GHC RTS to emit some kind of
     innocuous sys-call at various intervals throughout the execution of the traced program. 
   * We could get the GHC RTS to sample the same clock that perf is using for its timestamps. Unfortunately there
     does not appear to be a portable way to do this, as the clock used is system specific. However, for many
     systems it is possible that the POSIX `clock_gettime(CLOCK_MONOTONIC, ...)` will work.

It is worth noting that any kind of sys-call sampling is likely to lead to "observer effects" whereby the 
re-scheduling due to the sys-call will affect the runtime behaviour of the traced Haskell program.

### Encoding perf events in the GHC event format

The GHC event format specifies in its header a fixed number of event types. Each event instance in the payload of
the file should have one of those types. As seen below there is a very large number of event types that
the perf tool can record. Obviously in the context of ThreadScope we are only interested in a limited subset of
all possible events. There appears to be a couple of choices regarding the encoding of perf events in the GHC
event stream:

   1. Pick a fixed subset of the perf events and add them to the fixed set of GHC event types.
   2. Allow for any number of different perf events to be encoded in the GHC event stream. 
   3. A combination of the above two approaches.

The advantages of option 1 are that is easy to implement and probably nicer for the visualisation part of ThreadScope
since it knows all the possible event types in advance. It may be possible and advantageous to pick a subset of events
that are likely to be available on other platforms (CPU, operating system, and tracing framework).

The advantage of option 2 is that it allows the user more flexibility with the set of events to include and will be
forwards and backwards compatible with different version of the perf tool.

Option 2 could be implemented using a two-level encoding. The first level of the encoding adds two new event types
to the GHC header:

   1. A perf event meta-type.
   2. A perf event record.

Instances of the meta-type are pseudo events which encode true perf event types. They contain a string name of
the actual perf event, and a unique integer token identifying them. Instances of the perf event record are actual
event values. They contain timestamp and other information such as thread-id, plus an integer token which links
the event to its corresponding perf type. The program which reads the perf event stream will have to be extended
to be aware of the two-level encoding.

## Types of events

You can get a listing of all the available events with the command:

    sudo perf list

The sudo is necessary (at least on my system) because some of the events (tracepoints?) require root privileges.

<pre>
List of pre-defined events (to be used in -e):
  cpu-cycles OR cycles                               [Hardware event]
  stalled-cycles-frontend OR idle-cycles-frontend    [Hardware event]
  stalled-cycles-backend OR idle-cycles-backend      [Hardware event]
  instructions                                       [Hardware event]
  cache-references                                   [Hardware event]
  cache-misses                                       [Hardware event]
  branch-instructions OR branches                    [Hardware event]
  branch-misses                                      [Hardware event]
  bus-cycles                                         [Hardware event]

  cpu-clock                                          [Software event]
  task-clock                                         [Software event]
  page-faults OR faults                              [Software event]
  minor-faults                                       [Software event]
  major-faults                                       [Software event]
  context-switches OR cs                             [Software event]
  cpu-migrations OR migrations                       [Software event]
  alignment-faults                                   [Software event]
  emulation-faults                                   [Software event]

  L1-dcache-loads                                    [Hardware cache event]
  L1-dcache-load-misses                              [Hardware cache event]
  L1-dcache-stores                                   [Hardware cache event]
  L1-dcache-store-misses                             [Hardware cache event]
  L1-dcache-prefetches                               [Hardware cache event]
  L1-dcache-prefetch-misses                          [Hardware cache event]
  L1-icache-loads                                    [Hardware cache event]
  L1-icache-load-misses                              [Hardware cache event]
  L1-icache-prefetches                               [Hardware cache event]
  L1-icache-prefetch-misses                          [Hardware cache event]
  LLC-loads                                          [Hardware cache event]
  LLC-load-misses                                    [Hardware cache event]
  LLC-stores                                         [Hardware cache event]
  LLC-store-misses                                   [Hardware cache event]
  LLC-prefetches                                     [Hardware cache event]
  LLC-prefetch-misses                                [Hardware cache event]
  dTLB-loads                                         [Hardware cache event]
  dTLB-load-misses                                   [Hardware cache event]
  dTLB-stores                                        [Hardware cache event]
  dTLB-store-misses                                  [Hardware cache event]
  dTLB-prefetches                                    [Hardware cache event]
  dTLB-prefetch-misses                               [Hardware cache event]
  iTLB-loads                                         [Hardware cache event]
  iTLB-load-misses                                   [Hardware cache event]
  branch-loads                                       [Hardware cache event]
  branch-load-misses                                 [Hardware cache event]

  rNNN (see 'perf list --help' on how to encode it)  [Raw hardware event descriptor]

  mem:<addr>[:access]                                [Hardware breakpoint]

  skb:kfree_skb                                      [Tracepoint event]
  skb:consume_skb                                    [Tracepoint event]
  skb:skb_copy_datagram_iovec                        [Tracepoint event]
  net:net_dev_xmit                                   [Tracepoint event]
  net:net_dev_queue                                  [Tracepoint event]
  net:netif_receive_skb                              [Tracepoint event]
  net:netif_rx                                       [Tracepoint event]
  napi:napi_poll                                     [Tracepoint event]
  scsi:scsi_dispatch_cmd_start                       [Tracepoint event]
  scsi:scsi_dispatch_cmd_error                       [Tracepoint event]
  scsi:scsi_dispatch_cmd_done                        [Tracepoint event]
  scsi:scsi_dispatch_cmd_timeout                     [Tracepoint event]
  scsi:scsi_eh_wakeup                                [Tracepoint event]
  regulator:regulator_enable                         [Tracepoint event]
  regulator:regulator_enable_delay                   [Tracepoint event]
  regulator:regulator_enable_complete                [Tracepoint event]
  regulator:regulator_disable                        [Tracepoint event]
  regulator:regulator_disable_complete               [Tracepoint event]
  regulator:regulator_set_voltage                    [Tracepoint event]
  regulator:regulator_set_voltage_complete           [Tracepoint event]
  gpio:gpio_direction                                [Tracepoint event]
  gpio:gpio_value                                    [Tracepoint event]
  block:block_rq_abort                               [Tracepoint event]
  block:block_rq_requeue                             [Tracepoint event]
  block:block_rq_complete                            [Tracepoint event]
  block:block_rq_insert                              [Tracepoint event]
  block:block_rq_issue                               [Tracepoint event]
  block:block_bio_bounce                             [Tracepoint event]
  block:block_bio_complete                           [Tracepoint event]
  block:block_bio_backmerge                          [Tracepoint event]
  block:block_bio_frontmerge                         [Tracepoint event]
  block:block_bio_queue                              [Tracepoint event]
  block:block_getrq                                  [Tracepoint event]
  block:block_sleeprq                                [Tracepoint event]
  block:block_plug                                   [Tracepoint event]
  block:block_unplug                                 [Tracepoint event]
  block:block_split                                  [Tracepoint event]
  block:block_bio_remap                              [Tracepoint event]
  block:block_rq_remap                               [Tracepoint event]
  jbd2:jbd2_checkpoint                               [Tracepoint event]
  jbd2:jbd2_start_commit                             [Tracepoint event]
  jbd2:jbd2_commit_locking                           [Tracepoint event]
  jbd2:jbd2_commit_flushing                          [Tracepoint event]
  jbd2:jbd2_commit_logging                           [Tracepoint event]
  jbd2:jbd2_end_commit                               [Tracepoint event]
  jbd2:jbd2_submit_inode_data                        [Tracepoint event]
  jbd2:jbd2_run_stats                                [Tracepoint event]
  jbd2:jbd2_checkpoint_stats                         [Tracepoint event]
  jbd2:jbd2_cleanup_journal_tail                     [Tracepoint event]
  ext4:ext4_free_inode                               [Tracepoint event]
  ext4:ext4_request_inode                            [Tracepoint event]
  ext4:ext4_allocate_inode                           [Tracepoint event]
  ext4:ext4_evict_inode                              [Tracepoint event]
  ext4:ext4_drop_inode                               [Tracepoint event]
  ext4:ext4_mark_inode_dirty                         [Tracepoint event]
  ext4:ext4_begin_ordered_truncate                   [Tracepoint event]
  ext4:ext4_write_begin                              [Tracepoint event]
  ext4:ext4_da_write_begin                           [Tracepoint event]
  ext4:ext4_ordered_write_end                        [Tracepoint event]
  ext4:ext4_writeback_write_end                      [Tracepoint event]
  ext4:ext4_journalled_write_end                     [Tracepoint event]
  ext4:ext4_da_write_end                             [Tracepoint event]
  ext4:ext4_da_writepages                            [Tracepoint event]
  ext4:ext4_da_write_pages                           [Tracepoint event]
  ext4:ext4_da_writepages_result                     [Tracepoint event]
  ext4:ext4_writepage                                [Tracepoint event]
  ext4:ext4_readpage                                 [Tracepoint event]
  ext4:ext4_releasepage                              [Tracepoint event]
  ext4:ext4_invalidatepage                           [Tracepoint event]
  ext4:ext4_discard_blocks                           [Tracepoint event]
  ext4:ext4_mb_new_inode_pa                          [Tracepoint event]
  ext4:ext4_mb_new_group_pa                          [Tracepoint event]
  ext4:ext4_mb_release_inode_pa                      [Tracepoint event]
  ext4:ext4_mb_release_group_pa                      [Tracepoint event]
  ext4:ext4_discard_preallocations                   [Tracepoint event]
  ext4:ext4_mb_discard_preallocations                [Tracepoint event]
  ext4:ext4_request_blocks                           [Tracepoint event]
  ext4:ext4_allocate_blocks                          [Tracepoint event]
  ext4:ext4_free_blocks                              [Tracepoint event]
  ext4:ext4_sync_file_enter                          [Tracepoint event]
  ext4:ext4_sync_file_exit                           [Tracepoint event]
  ext4:ext4_sync_fs                                  [Tracepoint event]
  ext4:ext4_alloc_da_blocks                          [Tracepoint event]
  ext4:ext4_mballoc_alloc                            [Tracepoint event]
  ext4:ext4_mballoc_prealloc                         [Tracepoint event]
  ext4:ext4_mballoc_discard                          [Tracepoint event]
  ext4:ext4_mballoc_free                             [Tracepoint event]
  ext4:ext4_forget                                   [Tracepoint event]
  ext4:ext4_da_update_reserve_space                  [Tracepoint event]
  ext4:ext4_da_reserve_space                         [Tracepoint event]
  ext4:ext4_da_release_space                         [Tracepoint event]
  ext4:ext4_mb_bitmap_load                           [Tracepoint event]
  ext4:ext4_mb_buddy_bitmap_load                     [Tracepoint event]
  ext4:ext4_read_block_bitmap_load                   [Tracepoint event]
  ext4:ext4_load_inode_bitmap                        [Tracepoint event]
  ext4:ext4_direct_IO_enter                          [Tracepoint event]
  ext4:ext4_direct_IO_exit                           [Tracepoint event]
  ext4:ext4_fallocate_enter                          [Tracepoint event]
  ext4:ext4_fallocate_exit                           [Tracepoint event]
  ext4:ext4_unlink_enter                             [Tracepoint event]
  ext4:ext4_unlink_exit                              [Tracepoint event]
  ext4:ext4_truncate_enter                           [Tracepoint event]
  ext4:ext4_truncate_exit                            [Tracepoint event]
  ext4:ext4_ext_map_blocks_enter                     [Tracepoint event]
  ext4:ext4_ind_map_blocks_enter                     [Tracepoint event]
  ext4:ext4_ext_map_blocks_exit                      [Tracepoint event]
  ext4:ext4_ind_map_blocks_exit                      [Tracepoint event]
  ext4:ext4_ext_load_extent                          [Tracepoint event]
  ext4:ext4_load_inode                               [Tracepoint event]
  writeback:writeback_nothread                       [Tracepoint event]
  writeback:writeback_queue                          [Tracepoint event]
  writeback:writeback_exec                           [Tracepoint event]
  writeback:writeback_pages_written                  [Tracepoint event]
  writeback:writeback_nowork                         [Tracepoint event]
  writeback:writeback_wake_background                [Tracepoint event]
  writeback:writeback_wake_thread                    [Tracepoint event]
  writeback:writeback_wake_forker_thread             [Tracepoint event]
  writeback:writeback_bdi_register                   [Tracepoint event]
  writeback:writeback_bdi_unregister                 [Tracepoint event]
  writeback:writeback_thread_start                   [Tracepoint event]
  writeback:writeback_thread_stop                    [Tracepoint event]
  writeback:wbc_writeback_start                      [Tracepoint event]
  writeback:wbc_writeback_written                    [Tracepoint event]
  writeback:wbc_writeback_wait                       [Tracepoint event]
  writeback:wbc_balance_dirty_start                  [Tracepoint event]
  writeback:wbc_balance_dirty_written                [Tracepoint event]
  writeback:wbc_balance_dirty_wait                   [Tracepoint event]
  writeback:wbc_writepage                            [Tracepoint event]
  writeback:writeback_congestion_wait                [Tracepoint event]
  writeback:writeback_wait_iff_congested             [Tracepoint event]
  vfs:dirty_inode                                    [Tracepoint event]
  fs:do_sys_open                                     [Tracepoint event]
  fs:open_exec                                       [Tracepoint event]
  compaction:mm_compaction_isolate_migratepages      [Tracepoint event]
  compaction:mm_compaction_isolate_freepages         [Tracepoint event]
  compaction:mm_compaction_migratepages              [Tracepoint event]
  kmem:kmalloc                                       [Tracepoint event]
  kmem:kmem_cache_alloc                              [Tracepoint event]
  kmem:kmalloc_node                                  [Tracepoint event]
  kmem:kmem_cache_alloc_node                         [Tracepoint event]
  kmem:kfree                                         [Tracepoint event]
  kmem:kmem_cache_free                               [Tracepoint event]
  kmem:mm_page_free_direct                           [Tracepoint event]
  kmem:mm_pagevec_free                               [Tracepoint event]
  kmem:mm_page_alloc                                 [Tracepoint event]
  kmem:mm_page_alloc_zone_locked                     [Tracepoint event]
  kmem:mm_page_pcpu_drain                            [Tracepoint event]
  kmem:mm_page_alloc_extfrag                         [Tracepoint event]
  vmscan:mm_vmscan_kswapd_sleep                      [Tracepoint event]
  vmscan:mm_vmscan_kswapd_wake                       [Tracepoint event]
  vmscan:mm_vmscan_wakeup_kswapd                     [Tracepoint event]
  vmscan:mm_vmscan_direct_reclaim_begin              [Tracepoint event]
  vmscan:mm_vmscan_memcg_reclaim_begin               [Tracepoint event]
  vmscan:mm_vmscan_memcg_softlimit_reclaim_begin     [Tracepoint event]
  vmscan:mm_vmscan_direct_reclaim_end                [Tracepoint event]
  vmscan:mm_vmscan_memcg_reclaim_end                 [Tracepoint event]
  vmscan:mm_vmscan_memcg_softlimit_reclaim_end       [Tracepoint event]
  vmscan:mm_vmscan_lru_isolate                       [Tracepoint event]
  vmscan:mm_vmscan_memcg_isolate                     [Tracepoint event]
  vmscan:mm_vmscan_writepage                         [Tracepoint event]
  vmscan:mm_vmscan_lru_shrink_inactive               [Tracepoint event]
  vmscan:replace_swap_token                          [Tracepoint event]
  vmscan:put_swap_token                              [Tracepoint event]
  vmscan:disable_swap_token                          [Tracepoint event]
  vmscan:update_swap_token_priority                  [Tracepoint event]
  power:cpu_idle                                     [Tracepoint event]
  power:cpu_frequency                                [Tracepoint event]
  power:machine_suspend                              [Tracepoint event]
  power:clock_enable                                 [Tracepoint event]
  power:clock_disable                                [Tracepoint event]
  power:clock_set_rate                               [Tracepoint event]
  power:power_domain_target                          [Tracepoint event]
  module:module_load                                 [Tracepoint event]
  module:module_free                                 [Tracepoint event]
  module:module_get                                  [Tracepoint event]
  module:module_put                                  [Tracepoint event]
  module:module_request                              [Tracepoint event]
  workqueue:workqueue_queue_work                     [Tracepoint event]
  workqueue:workqueue_activate_work                  [Tracepoint event]
  workqueue:workqueue_execute_start                  [Tracepoint event]
  workqueue:workqueue_execute_end                    [Tracepoint event]
  signal:signal_generate                             [Tracepoint event]
  signal:signal_deliver                              [Tracepoint event]
  signal:signal_overflow_fail                        [Tracepoint event]
  signal:signal_lose_info                            [Tracepoint event]
  timer:timer_init                                   [Tracepoint event]
  timer:timer_start                                  [Tracepoint event]
  timer:timer_expire_entry                           [Tracepoint event]
  timer:timer_expire_exit                            [Tracepoint event]
  timer:timer_cancel                                 [Tracepoint event]
  timer:hrtimer_init                                 [Tracepoint event]
  timer:hrtimer_start                                [Tracepoint event]
  timer:hrtimer_expire_entry                         [Tracepoint event]
  timer:hrtimer_expire_exit                          [Tracepoint event]
  timer:hrtimer_cancel                               [Tracepoint event]
  timer:itimer_state                                 [Tracepoint event]
  timer:itimer_expire                                [Tracepoint event]
  irq:irq_handler_entry                              [Tracepoint event]
  irq:irq_handler_exit                               [Tracepoint event]
  irq:softirq_entry                                  [Tracepoint event]
  irq:softirq_exit                                   [Tracepoint event]
  irq:softirq_raise                                  [Tracepoint event]
  sched:sched_kthread_stop                           [Tracepoint event]
  sched:sched_kthread_stop_ret                       [Tracepoint event]
  sched:sched_wakeup                                 [Tracepoint event]
  sched:sched_wakeup_new                             [Tracepoint event]
  sched:sched_switch                                 [Tracepoint event]
  sched:sched_migrate_task                           [Tracepoint event]
  sched:sched_process_free                           [Tracepoint event]
  sched:sched_process_exit                           [Tracepoint event]
  sched:sched_wait_task                              [Tracepoint event]
  sched:sched_process_wait                           [Tracepoint event]
  sched:sched_process_fork                           [Tracepoint event]
  sched:sched_stat_wait                              [Tracepoint event]
  sched:sched_stat_sleep                             [Tracepoint event]
  sched:sched_stat_iowait                            [Tracepoint event]
  sched:sched_stat_runtime                           [Tracepoint event]
  sched:sched_pi_setprio                             [Tracepoint event]
  mce:mce_record                                     [Tracepoint event]
  raw_syscalls:sys_enter                             [Tracepoint event]
  raw_syscalls:sys_exit                              [Tracepoint event]
  syscalls:sys_enter_socket                          [Tracepoint event]
  syscalls:sys_exit_socket                           [Tracepoint event]
  syscalls:sys_enter_socketpair                      [Tracepoint event]
  syscalls:sys_exit_socketpair                       [Tracepoint event]
  syscalls:sys_enter_bind                            [Tracepoint event]
  syscalls:sys_exit_bind                             [Tracepoint event]
  syscalls:sys_enter_listen                          [Tracepoint event]
  syscalls:sys_exit_listen                           [Tracepoint event]
  syscalls:sys_enter_accept4                         [Tracepoint event]
  syscalls:sys_exit_accept4                          [Tracepoint event]
  syscalls:sys_enter_accept                          [Tracepoint event]
  syscalls:sys_exit_accept                           [Tracepoint event]
  syscalls:sys_enter_connect                         [Tracepoint event]
  syscalls:sys_exit_connect                          [Tracepoint event]
  syscalls:sys_enter_getsockname                     [Tracepoint event]
  syscalls:sys_exit_getsockname                      [Tracepoint event]
  syscalls:sys_enter_getpeername                     [Tracepoint event]
  syscalls:sys_exit_getpeername                      [Tracepoint event]
  syscalls:sys_enter_sendto                          [Tracepoint event]
  syscalls:sys_exit_sendto                           [Tracepoint event]
  syscalls:sys_enter_recvfrom                        [Tracepoint event]
  syscalls:sys_exit_recvfrom                         [Tracepoint event]
  syscalls:sys_enter_setsockopt                      [Tracepoint event]
  syscalls:sys_exit_setsockopt                       [Tracepoint event]
  syscalls:sys_enter_getsockopt                      [Tracepoint event]
  syscalls:sys_exit_getsockopt                       [Tracepoint event]
  syscalls:sys_enter_shutdown                        [Tracepoint event]
  syscalls:sys_exit_shutdown                         [Tracepoint event]
  syscalls:sys_enter_sendmsg                         [Tracepoint event]
  syscalls:sys_exit_sendmsg                          [Tracepoint event]
  syscalls:sys_enter_sendmmsg                        [Tracepoint event]
  syscalls:sys_exit_sendmmsg                         [Tracepoint event]
  syscalls:sys_enter_recvmsg                         [Tracepoint event]
  syscalls:sys_exit_recvmsg                          [Tracepoint event]
  syscalls:sys_enter_recvmmsg                        [Tracepoint event]
  syscalls:sys_exit_recvmmsg                         [Tracepoint event]
  syscalls:sys_enter_add_key                         [Tracepoint event]
  syscalls:sys_exit_add_key                          [Tracepoint event]
  syscalls:sys_enter_request_key                     [Tracepoint event]
  syscalls:sys_exit_request_key                      [Tracepoint event]
  syscalls:sys_enter_keyctl                          [Tracepoint event]
  syscalls:sys_exit_keyctl                           [Tracepoint event]
  syscalls:sys_enter_mq_open                         [Tracepoint event]
  syscalls:sys_exit_mq_open                          [Tracepoint event]
  syscalls:sys_enter_mq_unlink                       [Tracepoint event]
  syscalls:sys_exit_mq_unlink                        [Tracepoint event]
  syscalls:sys_enter_mq_timedsend                    [Tracepoint event]
  syscalls:sys_exit_mq_timedsend                     [Tracepoint event]
  syscalls:sys_enter_mq_timedreceive                 [Tracepoint event]
  syscalls:sys_exit_mq_timedreceive                  [Tracepoint event]
  syscalls:sys_enter_mq_notify                       [Tracepoint event]
  syscalls:sys_exit_mq_notify                        [Tracepoint event]
  syscalls:sys_enter_mq_getsetattr                   [Tracepoint event]
  syscalls:sys_exit_mq_getsetattr                    [Tracepoint event]
  syscalls:sys_enter_shmget                          [Tracepoint event]
  syscalls:sys_exit_shmget                           [Tracepoint event]
  syscalls:sys_enter_shmctl                          [Tracepoint event]
  syscalls:sys_exit_shmctl                           [Tracepoint event]
  syscalls:sys_enter_shmat                           [Tracepoint event]
  syscalls:sys_exit_shmat                            [Tracepoint event]
  syscalls:sys_enter_shmdt                           [Tracepoint event]
  syscalls:sys_exit_shmdt                            [Tracepoint event]
  syscalls:sys_enter_semget                          [Tracepoint event]
  syscalls:sys_exit_semget                           [Tracepoint event]
  syscalls:sys_enter_semtimedop                      [Tracepoint event]
  syscalls:sys_exit_semtimedop                       [Tracepoint event]
  syscalls:sys_enter_semop                           [Tracepoint event]
  syscalls:sys_exit_semop                            [Tracepoint event]
  syscalls:sys_enter_msgget                          [Tracepoint event]
  syscalls:sys_exit_msgget                           [Tracepoint event]
  syscalls:sys_enter_msgctl                          [Tracepoint event]
  syscalls:sys_exit_msgctl                           [Tracepoint event]
  syscalls:sys_enter_msgsnd                          [Tracepoint event]
  syscalls:sys_exit_msgsnd                           [Tracepoint event]
  syscalls:sys_enter_msgrcv                          [Tracepoint event]
  syscalls:sys_exit_msgrcv                           [Tracepoint event]
  syscalls:sys_enter_quotactl                        [Tracepoint event]
  syscalls:sys_exit_quotactl                         [Tracepoint event]
  syscalls:sys_enter_name_to_handle_at               [Tracepoint event]
  syscalls:sys_exit_name_to_handle_at                [Tracepoint event]
  syscalls:sys_enter_open_by_handle_at               [Tracepoint event]
  syscalls:sys_exit_open_by_handle_at                [Tracepoint event]
  syscalls:sys_enter_nfsservctl                      [Tracepoint event]
  syscalls:sys_exit_nfsservctl                       [Tracepoint event]
  syscalls:sys_enter_flock                           [Tracepoint event]
  syscalls:sys_exit_flock                            [Tracepoint event]
  syscalls:sys_enter_io_setup                        [Tracepoint event]
  syscalls:sys_exit_io_setup                         [Tracepoint event]
  syscalls:sys_enter_io_destroy                      [Tracepoint event]
  syscalls:sys_exit_io_destroy                       [Tracepoint event]
  syscalls:sys_enter_io_submit                       [Tracepoint event]
  syscalls:sys_exit_io_submit                        [Tracepoint event]
  syscalls:sys_enter_io_cancel                       [Tracepoint event]
  syscalls:sys_exit_io_cancel                        [Tracepoint event]
  syscalls:sys_enter_io_getevents                    [Tracepoint event]
  syscalls:sys_exit_io_getevents                     [Tracepoint event]
  syscalls:sys_enter_eventfd2                        [Tracepoint event]
  syscalls:sys_exit_eventfd2                         [Tracepoint event]
  syscalls:sys_enter_eventfd                         [Tracepoint event]
  syscalls:sys_exit_eventfd                          [Tracepoint event]
  syscalls:sys_enter_timerfd_create                  [Tracepoint event]
  syscalls:sys_exit_timerfd_create                   [Tracepoint event]
  syscalls:sys_enter_timerfd_settime                 [Tracepoint event]
  syscalls:sys_exit_timerfd_settime                  [Tracepoint event]
  syscalls:sys_enter_timerfd_gettime                 [Tracepoint event]
  syscalls:sys_exit_timerfd_gettime                  [Tracepoint event]
  syscalls:sys_enter_signalfd4                       [Tracepoint event]
  syscalls:sys_exit_signalfd4                        [Tracepoint event]
  syscalls:sys_enter_signalfd                        [Tracepoint event]
  syscalls:sys_exit_signalfd                         [Tracepoint event]
  syscalls:sys_enter_epoll_create1                   [Tracepoint event]
  syscalls:sys_exit_epoll_create1                    [Tracepoint event]
  syscalls:sys_enter_epoll_create                    [Tracepoint event]
  syscalls:sys_exit_epoll_create                     [Tracepoint event]
  syscalls:sys_enter_epoll_ctl                       [Tracepoint event]
  syscalls:sys_exit_epoll_ctl                        [Tracepoint event]
  syscalls:sys_enter_epoll_wait                      [Tracepoint event]
  syscalls:sys_exit_epoll_wait                       [Tracepoint event]
  syscalls:sys_enter_epoll_pwait                     [Tracepoint event]
  syscalls:sys_exit_epoll_pwait                      [Tracepoint event]
  syscalls:sys_enter_fanotify_init                   [Tracepoint event]
  syscalls:sys_exit_fanotify_init                    [Tracepoint event]
  syscalls:sys_enter_inotify_init1                   [Tracepoint event]
  syscalls:sys_exit_inotify_init1                    [Tracepoint event]
  syscalls:sys_enter_inotify_init                    [Tracepoint event]
  syscalls:sys_exit_inotify_init                     [Tracepoint event]
  syscalls:sys_enter_inotify_add_watch               [Tracepoint event]
  syscalls:sys_exit_inotify_add_watch                [Tracepoint event]
  syscalls:sys_enter_inotify_rm_watch                [Tracepoint event]
  syscalls:sys_exit_inotify_rm_watch                 [Tracepoint event]
  syscalls:sys_enter_ioprio_set                      [Tracepoint event]
  syscalls:sys_exit_ioprio_set                       [Tracepoint event]
  syscalls:sys_enter_ioprio_get                      [Tracepoint event]
  syscalls:sys_exit_ioprio_get                       [Tracepoint event]
  syscalls:sys_enter_statfs                          [Tracepoint event]
  syscalls:sys_exit_statfs                           [Tracepoint event]
  syscalls:sys_enter_fstatfs                         [Tracepoint event]
  syscalls:sys_exit_fstatfs                          [Tracepoint event]
  syscalls:sys_enter_ustat                           [Tracepoint event]
  syscalls:sys_exit_ustat                            [Tracepoint event]
  syscalls:sys_enter_utime                           [Tracepoint event]
  syscalls:sys_exit_utime                            [Tracepoint event]
  syscalls:sys_enter_utimensat                       [Tracepoint event]
  syscalls:sys_exit_utimensat                        [Tracepoint event]
  syscalls:sys_enter_futimesat                       [Tracepoint event]
  syscalls:sys_exit_futimesat                        [Tracepoint event]
  syscalls:sys_enter_utimes                          [Tracepoint event]
  syscalls:sys_exit_utimes                           [Tracepoint event]
  syscalls:sys_enter_sync                            [Tracepoint event]
  syscalls:sys_exit_sync                             [Tracepoint event]
  syscalls:sys_enter_syncfs                          [Tracepoint event]
  syscalls:sys_exit_syncfs                           [Tracepoint event]
  syscalls:sys_enter_fsync                           [Tracepoint event]
  syscalls:sys_exit_fsync                            [Tracepoint event]
  syscalls:sys_enter_fdatasync                       [Tracepoint event]
  syscalls:sys_exit_fdatasync                        [Tracepoint event]
  syscalls:sys_enter_vmsplice                        [Tracepoint event]
  syscalls:sys_exit_vmsplice                         [Tracepoint event]
  syscalls:sys_enter_splice                          [Tracepoint event]
  syscalls:sys_exit_splice                           [Tracepoint event]
  syscalls:sys_enter_tee                             [Tracepoint event]
  syscalls:sys_exit_tee                              [Tracepoint event]
  syscalls:sys_enter_setxattr                        [Tracepoint event]
  syscalls:sys_exit_setxattr                         [Tracepoint event]
  syscalls:sys_enter_lsetxattr                       [Tracepoint event]
  syscalls:sys_exit_lsetxattr                        [Tracepoint event]
  syscalls:sys_enter_fsetxattr                       [Tracepoint event]
  syscalls:sys_exit_fsetxattr                        [Tracepoint event]
  syscalls:sys_enter_getxattr                        [Tracepoint event]
  syscalls:sys_exit_getxattr                         [Tracepoint event]
  syscalls:sys_enter_lgetxattr                       [Tracepoint event]
  syscalls:sys_exit_lgetxattr                        [Tracepoint event]
  syscalls:sys_enter_fgetxattr                       [Tracepoint event]
  syscalls:sys_exit_fgetxattr                        [Tracepoint event]
  syscalls:sys_enter_listxattr                       [Tracepoint event]
  syscalls:sys_exit_listxattr                        [Tracepoint event]
  syscalls:sys_enter_llistxattr                      [Tracepoint event]
  syscalls:sys_exit_llistxattr                       [Tracepoint event]
  syscalls:sys_enter_flistxattr                      [Tracepoint event]
  syscalls:sys_exit_flistxattr                       [Tracepoint event]
  syscalls:sys_enter_removexattr                     [Tracepoint event]
  syscalls:sys_exit_removexattr                      [Tracepoint event]
  syscalls:sys_enter_lremovexattr                    [Tracepoint event]
  syscalls:sys_exit_lremovexattr                     [Tracepoint event]
  syscalls:sys_enter_fremovexattr                    [Tracepoint event]
  syscalls:sys_exit_fremovexattr                     [Tracepoint event]
  syscalls:sys_enter_umount                          [Tracepoint event]
  syscalls:sys_exit_umount                           [Tracepoint event]
  syscalls:sys_enter_mount                           [Tracepoint event]
  syscalls:sys_exit_mount                            [Tracepoint event]
  syscalls:sys_enter_pivot_root                      [Tracepoint event]
  syscalls:sys_exit_pivot_root                       [Tracepoint event]
  syscalls:sys_enter_sysfs                           [Tracepoint event]
  syscalls:sys_exit_sysfs                            [Tracepoint event]
  syscalls:sys_enter_getcwd                          [Tracepoint event]
  syscalls:sys_exit_getcwd                           [Tracepoint event]
  syscalls:sys_enter_select                          [Tracepoint event]
  syscalls:sys_exit_select                           [Tracepoint event]
  syscalls:sys_enter_pselect6                        [Tracepoint event]
  syscalls:sys_exit_pselect6                         [Tracepoint event]
  syscalls:sys_enter_poll                            [Tracepoint event]
  syscalls:sys_exit_poll                             [Tracepoint event]
  syscalls:sys_enter_ppoll                           [Tracepoint event]
  syscalls:sys_exit_ppoll                            [Tracepoint event]
  syscalls:sys_enter_getdents                        [Tracepoint event]
  syscalls:sys_exit_getdents                         [Tracepoint event]
  syscalls:sys_enter_getdents64                      [Tracepoint event]
  syscalls:sys_exit_getdents64                       [Tracepoint event]
  syscalls:sys_enter_ioctl                           [Tracepoint event]
  syscalls:sys_exit_ioctl                            [Tracepoint event]
  syscalls:sys_enter_dup3                            [Tracepoint event]
  syscalls:sys_exit_dup3                             [Tracepoint event]
  syscalls:sys_enter_dup2                            [Tracepoint event]
  syscalls:sys_exit_dup2                             [Tracepoint event]
  syscalls:sys_enter_dup                             [Tracepoint event]
  syscalls:sys_exit_dup                              [Tracepoint event]
  syscalls:sys_enter_fcntl                           [Tracepoint event]
  syscalls:sys_exit_fcntl                            [Tracepoint event]
  syscalls:sys_enter_mknodat                         [Tracepoint event]
  syscalls:sys_exit_mknodat                          [Tracepoint event]
  syscalls:sys_enter_mknod                           [Tracepoint event]
  syscalls:sys_exit_mknod                            [Tracepoint event]
  syscalls:sys_enter_mkdirat                         [Tracepoint event]
  syscalls:sys_exit_mkdirat                          [Tracepoint event]
  syscalls:sys_enter_mkdir                           [Tracepoint event]
  syscalls:sys_exit_mkdir                            [Tracepoint event]
  syscalls:sys_enter_rmdir                           [Tracepoint event]
  syscalls:sys_exit_rmdir                            [Tracepoint event]
  syscalls:sys_enter_unlinkat                        [Tracepoint event]
  syscalls:sys_exit_unlinkat                         [Tracepoint event]
  syscalls:sys_enter_unlink                          [Tracepoint event]
  syscalls:sys_exit_unlink                           [Tracepoint event]
  syscalls:sys_enter_symlinkat                       [Tracepoint event]
  syscalls:sys_exit_symlinkat                        [Tracepoint event]
  syscalls:sys_enter_symlink                         [Tracepoint event]
  syscalls:sys_exit_symlink                          [Tracepoint event]
  syscalls:sys_enter_linkat                          [Tracepoint event]
  syscalls:sys_exit_linkat                           [Tracepoint event]
  syscalls:sys_enter_link                            [Tracepoint event]
  syscalls:sys_exit_link                             [Tracepoint event]
  syscalls:sys_enter_renameat                        [Tracepoint event]
  syscalls:sys_exit_renameat                         [Tracepoint event]
  syscalls:sys_enter_rename                          [Tracepoint event]
  syscalls:sys_exit_rename                           [Tracepoint event]
  syscalls:sys_enter_pipe2                           [Tracepoint event]
  syscalls:sys_exit_pipe2                            [Tracepoint event]
  syscalls:sys_enter_pipe                            [Tracepoint event]
  syscalls:sys_exit_pipe                             [Tracepoint event]
  syscalls:sys_enter_newstat                         [Tracepoint event]
  syscalls:sys_exit_newstat                          [Tracepoint event]
  syscalls:sys_enter_newlstat                        [Tracepoint event]
  syscalls:sys_exit_newlstat                         [Tracepoint event]
  syscalls:sys_enter_newfstatat                      [Tracepoint event]
  syscalls:sys_exit_newfstatat                       [Tracepoint event]
  syscalls:sys_enter_newfstat                        [Tracepoint event]
  syscalls:sys_exit_newfstat                         [Tracepoint event]
  syscalls:sys_enter_readlinkat                      [Tracepoint event]
  syscalls:sys_exit_readlinkat                       [Tracepoint event]
  syscalls:sys_enter_readlink                        [Tracepoint event]
  syscalls:sys_exit_readlink                         [Tracepoint event]
  syscalls:sys_enter_lseek                           [Tracepoint event]
  syscalls:sys_exit_lseek                            [Tracepoint event]
  syscalls:sys_enter_read                            [Tracepoint event]
  syscalls:sys_exit_read                             [Tracepoint event]
  syscalls:sys_enter_write                           [Tracepoint event]
  syscalls:sys_exit_write                            [Tracepoint event]
  syscalls:sys_enter_readv                           [Tracepoint event]
  syscalls:sys_exit_readv                            [Tracepoint event]
  syscalls:sys_enter_writev                          [Tracepoint event]
  syscalls:sys_exit_writev                           [Tracepoint event]
  syscalls:sys_enter_preadv                          [Tracepoint event]
  syscalls:sys_exit_preadv                           [Tracepoint event]
  syscalls:sys_enter_pwritev                         [Tracepoint event]
  syscalls:sys_exit_pwritev                          [Tracepoint event]
  syscalls:sys_enter_sendfile64                      [Tracepoint event]
  syscalls:sys_exit_sendfile64                       [Tracepoint event]
  syscalls:sys_enter_truncate                        [Tracepoint event]
  syscalls:sys_exit_truncate                         [Tracepoint event]
  syscalls:sys_enter_ftruncate                       [Tracepoint event]
  syscalls:sys_exit_ftruncate                        [Tracepoint event]
  syscalls:sys_enter_faccessat                       [Tracepoint event]
  syscalls:sys_exit_faccessat                        [Tracepoint event]
  syscalls:sys_enter_access                          [Tracepoint event]
  syscalls:sys_exit_access                           [Tracepoint event]
  syscalls:sys_enter_chdir                           [Tracepoint event]
  syscalls:sys_exit_chdir                            [Tracepoint event]
  syscalls:sys_enter_fchdir                          [Tracepoint event]
  syscalls:sys_exit_fchdir                           [Tracepoint event]
  syscalls:sys_enter_chroot                          [Tracepoint event]
  syscalls:sys_exit_chroot                           [Tracepoint event]
  syscalls:sys_enter_fchmod                          [Tracepoint event]
  syscalls:sys_exit_fchmod                           [Tracepoint event]
  syscalls:sys_enter_fchmodat                        [Tracepoint event]
  syscalls:sys_exit_fchmodat                         [Tracepoint event]
  syscalls:sys_enter_chmod                           [Tracepoint event]
  syscalls:sys_exit_chmod                            [Tracepoint event]
  syscalls:sys_enter_chown                           [Tracepoint event]
  syscalls:sys_exit_chown                            [Tracepoint event]
  syscalls:sys_enter_fchownat                        [Tracepoint event]
  syscalls:sys_exit_fchownat                         [Tracepoint event]
  syscalls:sys_enter_lchown                          [Tracepoint event]
  syscalls:sys_exit_lchown                           [Tracepoint event]
  syscalls:sys_enter_fchown                          [Tracepoint event]
  syscalls:sys_exit_fchown                           [Tracepoint event]
  syscalls:sys_enter_open                            [Tracepoint event]
  syscalls:sys_exit_open                             [Tracepoint event]
  syscalls:sys_enter_openat                          [Tracepoint event]
  syscalls:sys_exit_openat                           [Tracepoint event]
  syscalls:sys_enter_creat                           [Tracepoint event]
  syscalls:sys_exit_creat                            [Tracepoint event]
  syscalls:sys_enter_close                           [Tracepoint event]
  syscalls:sys_exit_close                            [Tracepoint event]
  syscalls:sys_enter_vhangup                         [Tracepoint event]
  syscalls:sys_exit_vhangup                          [Tracepoint event]
  syscalls:sys_enter_move_pages                      [Tracepoint event]
  syscalls:sys_exit_move_pages                       [Tracepoint event]
  syscalls:sys_enter_mbind                           [Tracepoint event]
  syscalls:sys_exit_mbind                            [Tracepoint event]
  syscalls:sys_enter_set_mempolicy                   [Tracepoint event]
  syscalls:sys_exit_set_mempolicy                    [Tracepoint event]
  syscalls:sys_enter_migrate_pages                   [Tracepoint event]
  syscalls:sys_exit_migrate_pages                    [Tracepoint event]
  syscalls:sys_enter_get_mempolicy                   [Tracepoint event]
  syscalls:sys_exit_get_mempolicy                    [Tracepoint event]
  syscalls:sys_enter_swapoff                         [Tracepoint event]
  syscalls:sys_exit_swapoff                          [Tracepoint event]
  syscalls:sys_enter_swapon                          [Tracepoint event]
  syscalls:sys_exit_swapon                           [Tracepoint event]
  syscalls:sys_enter_msync                           [Tracepoint event]
  syscalls:sys_exit_msync                            [Tracepoint event]
  syscalls:sys_enter_mremap                          [Tracepoint event]
  syscalls:sys_exit_mremap                           [Tracepoint event]
  syscalls:sys_enter_mprotect                        [Tracepoint event]
  syscalls:sys_exit_mprotect                         [Tracepoint event]
  syscalls:sys_enter_brk                             [Tracepoint event]
  syscalls:sys_exit_brk                              [Tracepoint event]
  syscalls:sys_enter_munmap                          [Tracepoint event]
  syscalls:sys_exit_munmap                           [Tracepoint event]
  syscalls:sys_enter_mlock                           [Tracepoint event]
  syscalls:sys_exit_mlock                            [Tracepoint event]
  syscalls:sys_enter_munlock                         [Tracepoint event]
  syscalls:sys_exit_munlock                          [Tracepoint event]
  syscalls:sys_enter_mlockall                        [Tracepoint event]
  syscalls:sys_exit_mlockall                         [Tracepoint event]
  syscalls:sys_enter_munlockall                      [Tracepoint event]
  syscalls:sys_exit_munlockall                       [Tracepoint event]
  syscalls:sys_enter_mincore                         [Tracepoint event]
  syscalls:sys_exit_mincore                          [Tracepoint event]
  syscalls:sys_enter_madvise                         [Tracepoint event]
  syscalls:sys_exit_madvise                          [Tracepoint event]
  syscalls:sys_enter_remap_file_pages                [Tracepoint event]
  syscalls:sys_exit_remap_file_pages                 [Tracepoint event]
  syscalls:sys_enter_perf_event_open                 [Tracepoint event]
  syscalls:sys_exit_perf_event_open                  [Tracepoint event]
  syscalls:sys_enter_kexec_load                      [Tracepoint event]
  syscalls:sys_exit_kexec_load                       [Tracepoint event]
  syscalls:sys_enter_acct                            [Tracepoint event]
  syscalls:sys_exit_acct                             [Tracepoint event]
  syscalls:sys_enter_delete_module                   [Tracepoint event]
  syscalls:sys_exit_delete_module                    [Tracepoint event]
  syscalls:sys_enter_init_module                     [Tracepoint event]
  syscalls:sys_exit_init_module                      [Tracepoint event]
  syscalls:sys_enter_set_robust_list                 [Tracepoint event]
  syscalls:sys_exit_set_robust_list                  [Tracepoint event]
  syscalls:sys_enter_get_robust_list                 [Tracepoint event]
  syscalls:sys_exit_get_robust_list                  [Tracepoint event]
  syscalls:sys_enter_futex                           [Tracepoint event]
  syscalls:sys_exit_futex                            [Tracepoint event]
  syscalls:sys_enter_getgroups                       [Tracepoint event]
  syscalls:sys_exit_getgroups                        [Tracepoint event]
  syscalls:sys_enter_setgroups                       [Tracepoint event]
  syscalls:sys_exit_setgroups                        [Tracepoint event]
  syscalls:sys_enter_setns                           [Tracepoint event]
  syscalls:sys_exit_setns                            [Tracepoint event]
  syscalls:sys_enter_nanosleep                       [Tracepoint event]
  syscalls:sys_exit_nanosleep                        [Tracepoint event]
  syscalls:sys_enter_timer_create                    [Tracepoint event]
  syscalls:sys_exit_timer_create                     [Tracepoint event]
  syscalls:sys_enter_timer_gettime                   [Tracepoint event]
  syscalls:sys_exit_timer_gettime                    [Tracepoint event]
  syscalls:sys_enter_timer_getoverrun                [Tracepoint event]
  syscalls:sys_exit_timer_getoverrun                 [Tracepoint event]
  syscalls:sys_enter_timer_settime                   [Tracepoint event]
  syscalls:sys_exit_timer_settime                    [Tracepoint event]
  syscalls:sys_enter_timer_delete                    [Tracepoint event]
  syscalls:sys_exit_timer_delete                     [Tracepoint event]
  syscalls:sys_enter_clock_settime                   [Tracepoint event]
  syscalls:sys_exit_clock_settime                    [Tracepoint event]
  syscalls:sys_enter_clock_gettime                   [Tracepoint event]
  syscalls:sys_exit_clock_gettime                    [Tracepoint event]
  syscalls:sys_enter_clock_adjtime                   [Tracepoint event]
  syscalls:sys_exit_clock_adjtime                    [Tracepoint event]
  syscalls:sys_enter_clock_getres                    [Tracepoint event]
  syscalls:sys_exit_clock_getres                     [Tracepoint event]
  syscalls:sys_enter_clock_nanosleep                 [Tracepoint event]
  syscalls:sys_exit_clock_nanosleep                  [Tracepoint event]
  syscalls:sys_enter_setpriority                     [Tracepoint event]
  syscalls:sys_exit_setpriority                      [Tracepoint event]
  syscalls:sys_enter_getpriority                     [Tracepoint event]
  syscalls:sys_exit_getpriority                      [Tracepoint event]
  syscalls:sys_enter_reboot                          [Tracepoint event]
  syscalls:sys_exit_reboot                           [Tracepoint event]
  syscalls:sys_enter_setregid                        [Tracepoint event]
  syscalls:sys_exit_setregid                         [Tracepoint event]
  syscalls:sys_enter_setgid                          [Tracepoint event]
  syscalls:sys_exit_setgid                           [Tracepoint event]
  syscalls:sys_enter_setreuid                        [Tracepoint event]
  syscalls:sys_exit_setreuid                         [Tracepoint event]
  syscalls:sys_enter_setuid                          [Tracepoint event]
  syscalls:sys_exit_setuid                           [Tracepoint event]
  syscalls:sys_enter_setresuid                       [Tracepoint event]
  syscalls:sys_exit_setresuid                        [Tracepoint event]
  syscalls:sys_enter_getresuid                       [Tracepoint event]
  syscalls:sys_exit_getresuid                        [Tracepoint event]
  syscalls:sys_enter_setresgid                       [Tracepoint event]
  syscalls:sys_exit_setresgid                        [Tracepoint event]
  syscalls:sys_enter_getresgid                       [Tracepoint event]
  syscalls:sys_exit_getresgid                        [Tracepoint event]
  syscalls:sys_enter_setfsuid                        [Tracepoint event]
  syscalls:sys_exit_setfsuid                         [Tracepoint event]
  syscalls:sys_enter_setfsgid                        [Tracepoint event]
  syscalls:sys_exit_setfsgid                         [Tracepoint event]
  syscalls:sys_enter_times                           [Tracepoint event]
  syscalls:sys_exit_times                            [Tracepoint event]
  syscalls:sys_enter_setpgid                         [Tracepoint event]
  syscalls:sys_exit_setpgid                          [Tracepoint event]
  syscalls:sys_enter_getpgid                         [Tracepoint event]
  syscalls:sys_exit_getpgid                          [Tracepoint event]
  syscalls:sys_enter_getpgrp                         [Tracepoint event]
  syscalls:sys_exit_getpgrp                          [Tracepoint event]
  syscalls:sys_enter_getsid                          [Tracepoint event]
  syscalls:sys_exit_getsid                           [Tracepoint event]
  syscalls:sys_enter_setsid                          [Tracepoint event]
  syscalls:sys_exit_setsid                           [Tracepoint event]
  syscalls:sys_enter_newuname                        [Tracepoint event]
  syscalls:sys_exit_newuname                         [Tracepoint event]
  syscalls:sys_enter_sethostname                     [Tracepoint event]
  syscalls:sys_exit_sethostname                      [Tracepoint event]
  syscalls:sys_enter_setdomainname                   [Tracepoint event]
  syscalls:sys_exit_setdomainname                    [Tracepoint event]
  syscalls:sys_enter_getrlimit                       [Tracepoint event]
  syscalls:sys_exit_getrlimit                        [Tracepoint event]
  syscalls:sys_enter_prlimit64                       [Tracepoint event]
  syscalls:sys_exit_prlimit64                        [Tracepoint event]
  syscalls:sys_enter_setrlimit                       [Tracepoint event]
  syscalls:sys_exit_setrlimit                        [Tracepoint event]
  syscalls:sys_enter_getrusage                       [Tracepoint event]
  syscalls:sys_exit_getrusage                        [Tracepoint event]
  syscalls:sys_enter_umask                           [Tracepoint event]
  syscalls:sys_exit_umask                            [Tracepoint event]
  syscalls:sys_enter_prctl                           [Tracepoint event]
  syscalls:sys_exit_prctl                            [Tracepoint event]
  syscalls:sys_enter_restart_syscall                 [Tracepoint event]
  syscalls:sys_exit_restart_syscall                  [Tracepoint event]
  syscalls:sys_enter_rt_sigprocmask                  [Tracepoint event]
  syscalls:sys_exit_rt_sigprocmask                   [Tracepoint event]
  syscalls:sys_enter_rt_sigpending                   [Tracepoint event]
  syscalls:sys_exit_rt_sigpending                    [Tracepoint event]
  syscalls:sys_enter_rt_sigtimedwait                 [Tracepoint event]
  syscalls:sys_exit_rt_sigtimedwait                  [Tracepoint event]
  syscalls:sys_enter_kill                            [Tracepoint event]
  syscalls:sys_exit_kill                             [Tracepoint event]
  syscalls:sys_enter_tgkill                          [Tracepoint event]
  syscalls:sys_exit_tgkill                           [Tracepoint event]
  syscalls:sys_enter_tkill                           [Tracepoint event]
  syscalls:sys_exit_tkill                            [Tracepoint event]
  syscalls:sys_enter_rt_sigqueueinfo                 [Tracepoint event]
  syscalls:sys_exit_rt_sigqueueinfo                  [Tracepoint event]
  syscalls:sys_enter_rt_tgsigqueueinfo               [Tracepoint event]
  syscalls:sys_exit_rt_tgsigqueueinfo                [Tracepoint event]
  syscalls:sys_enter_rt_sigaction                    [Tracepoint event]
  syscalls:sys_exit_rt_sigaction                     [Tracepoint event]
  syscalls:sys_enter_pause                           [Tracepoint event]
  syscalls:sys_exit_pause                            [Tracepoint event]
  syscalls:sys_enter_rt_sigsuspend                   [Tracepoint event]
  syscalls:sys_exit_rt_sigsuspend                    [Tracepoint event]
  syscalls:sys_enter_alarm                           [Tracepoint event]
  syscalls:sys_exit_alarm                            [Tracepoint event]
  syscalls:sys_enter_getpid                          [Tracepoint event]
  syscalls:sys_exit_getpid                           [Tracepoint event]
  syscalls:sys_enter_getppid                         [Tracepoint event]
  syscalls:sys_exit_getppid                          [Tracepoint event]
  syscalls:sys_enter_getuid                          [Tracepoint event]
  syscalls:sys_exit_getuid                           [Tracepoint event]
  syscalls:sys_enter_geteuid                         [Tracepoint event]
  syscalls:sys_exit_geteuid                          [Tracepoint event]
  syscalls:sys_enter_getgid                          [Tracepoint event]
  syscalls:sys_exit_getgid                           [Tracepoint event]
  syscalls:sys_enter_getegid                         [Tracepoint event]
  syscalls:sys_exit_getegid                          [Tracepoint event]
  syscalls:sys_enter_gettid                          [Tracepoint event]
  syscalls:sys_exit_gettid                           [Tracepoint event]
  syscalls:sys_enter_sysinfo                         [Tracepoint event]
  syscalls:sys_exit_sysinfo                          [Tracepoint event]
  syscalls:sys_enter_ptrace                          [Tracepoint event]
  syscalls:sys_exit_ptrace                           [Tracepoint event]
  syscalls:sys_enter_capget                          [Tracepoint event]
  syscalls:sys_exit_capget                           [Tracepoint event]
  syscalls:sys_enter_capset                          [Tracepoint event]
  syscalls:sys_exit_capset                           [Tracepoint event]
  syscalls:sys_enter_sysctl                          [Tracepoint event]
  syscalls:sys_exit_sysctl                           [Tracepoint event]
  syscalls:sys_enter_time                            [Tracepoint event]
  syscalls:sys_exit_time                             [Tracepoint event]
  syscalls:sys_enter_gettimeofday                    [Tracepoint event]
  syscalls:sys_exit_gettimeofday                     [Tracepoint event]
  syscalls:sys_enter_settimeofday                    [Tracepoint event]
  syscalls:sys_exit_settimeofday                     [Tracepoint event]
  syscalls:sys_enter_adjtimex                        [Tracepoint event]
  syscalls:sys_exit_adjtimex                         [Tracepoint event]
  syscalls:sys_enter_getitimer                       [Tracepoint event]
  syscalls:sys_exit_getitimer                        [Tracepoint event]
  syscalls:sys_enter_setitimer                       [Tracepoint event]
  syscalls:sys_exit_setitimer                        [Tracepoint event]
  syscalls:sys_enter_exit                            [Tracepoint event]
  syscalls:sys_exit_exit                             [Tracepoint event]
  syscalls:sys_enter_exit_group                      [Tracepoint event]
  syscalls:sys_exit_exit_group                       [Tracepoint event]
  syscalls:sys_enter_waitid                          [Tracepoint event]
  syscalls:sys_exit_waitid                           [Tracepoint event]
  syscalls:sys_enter_wait4                           [Tracepoint event]
  syscalls:sys_exit_wait4                            [Tracepoint event]
  syscalls:sys_enter_syslog                          [Tracepoint event]
  syscalls:sys_exit_syslog                           [Tracepoint event]
  syscalls:sys_enter_personality                     [Tracepoint event]
  syscalls:sys_exit_personality                      [Tracepoint event]
  syscalls:sys_enter_set_tid_address                 [Tracepoint event]
  syscalls:sys_exit_set_tid_address                  [Tracepoint event]
  syscalls:sys_enter_unshare                         [Tracepoint event]
  syscalls:sys_exit_unshare                          [Tracepoint event]
  syscalls:sys_enter_sched_setscheduler              [Tracepoint event]
  syscalls:sys_exit_sched_setscheduler               [Tracepoint event]
  syscalls:sys_enter_sched_setparam                  [Tracepoint event]
  syscalls:sys_exit_sched_setparam                   [Tracepoint event]
  syscalls:sys_enter_sched_getscheduler              [Tracepoint event]
  syscalls:sys_exit_sched_getscheduler               [Tracepoint event]
  syscalls:sys_enter_sched_getparam                  [Tracepoint event]
  syscalls:sys_exit_sched_getparam                   [Tracepoint event]
  syscalls:sys_enter_sched_setaffinity               [Tracepoint event]
  syscalls:sys_exit_sched_setaffinity                [Tracepoint event]
  syscalls:sys_enter_sched_getaffinity               [Tracepoint event]
  syscalls:sys_exit_sched_getaffinity                [Tracepoint event]
  syscalls:sys_enter_sched_yield                     [Tracepoint event]
  syscalls:sys_exit_sched_yield                      [Tracepoint event]
  syscalls:sys_enter_sched_get_priority_max          [Tracepoint event]
  syscalls:sys_exit_sched_get_priority_max           [Tracepoint event]
  syscalls:sys_enter_sched_get_priority_min          [Tracepoint event]
  syscalls:sys_exit_sched_get_priority_min           [Tracepoint event]
  syscalls:sys_enter_sched_rr_get_interval           [Tracepoint event]
  syscalls:sys_exit_sched_rr_get_interval            [Tracepoint event]
  syscalls:sys_enter_mmap                            [Tracepoint event]
  syscalls:sys_exit_mmap                             [Tracepoint event]
</pre>

****
