CBMC usually achieves coverage of 85 labels, but the number of
generated tests varies.  We therefore hide the detailed trace and
check afterwards with a hack.
  $ export SC_ENABLE_DETAILED_STATS=no

  $ seacoral --tools cbmc
  [A]{Sc} Starting to log into `_sc/codebase.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `p1'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `p1':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results

  $ tr -d '\0' < _sc/last/store/store | wc -c
  91
