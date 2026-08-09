  $ export SC_ENABLE_DETAILED_STATS=yes
  $ seacoral --tools libfuzzer --config switch.toml
  [A]{Sc} Starting to log into `_sc/switch.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `f':
          cov: 8 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 5 tests
  [A]{Sc} Covered labels: {1, 2, 3, 4, 5, 6, 7, 8}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
                 5: Covered
                 6: Covered
                 7: Covered
                 8: Covered
          Coverage: (8/8) 100.0%
