  $ export CBMC_MODE=assert
  $ seacoral --tools cbmc
  [A]{Sc} Starting to log into `_sc/tritype.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `tritype'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `tritype':
          cov: 20 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 10 tests
  [A]{Sc} Covered labels:
          {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
           20}
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
                 9: Covered
                10: Covered
                11: Covered
                12: Covered
                13: Covered
                14: Covered
                15: Covered
                16: Covered
                17: Covered
                18: Covered
                19: Covered
                20: Covered
          Coverage: (20/20) 100.0%

  $ seacoral --tools cbmc --inputs tritype-with-preconditions.c
  [A]{Sc} Starting to log into `_sc/tritype-with-preconditions.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `tritype'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `tritype':
          cov: 20 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 8 tests
  [A]{Sc} Covered labels:
          {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
           20}
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
                 9: Covered
                10: Covered
                11: Covered
                12: Covered
                13: Covered
                14: Covered
                15: Covered
                16: Covered
                17: Covered
                18: Covered
                19: Covered
                20: Covered
          Coverage: (20/20) 100.0%
