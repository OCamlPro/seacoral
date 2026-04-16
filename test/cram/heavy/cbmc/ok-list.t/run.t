  $ seacoral --tools cbmc --config length.toml
  [A]{Sc} Starting to log into `_sc/length.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `length'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `length':
          cov: 2 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 1 test
  [A]{Sc} Covered labels: {1, 2}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
          Coverage: (2/2) 100.0%
  $ seacoral --tools cbmc --config abs-sum.toml
  [A]{Sc} Starting to log into `_sc/abs-sum.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `abs_sum'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `abs_sum':
          cov: 4 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 2 tests
  [A]{Sc} Covered labels: {1, 2, 3, 4}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
          Coverage: (4/4) 100.0%
