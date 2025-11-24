  $ seacoral --tools cbmc --config cover.toml
  [A]{Sc} Starting to log into `_sc/late-segfault.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `f':
          cov: 1 (25.0%) uncov: 0 (0.0%) unkwn: 3 (75.0%) with 1 test
  [A]{Sc} Covered labels: {1}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Unknown
                 3: Unknown
                 4: Unknown
          Coverage: (1/4) 25.0%
  $ seacoral --tools cbmc --config assert.toml
  [A]{Sc} Starting to log into `_sc/late-segfault.c-DC-@2/logs/2.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Current coverage statistics for `f':
          cov: 1 (25.0%) uncov: 0 (0.0%) unkwn: 3 (75.0%) with 1 test
          rte: none
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `f':
          cov: 1 (25.0%) uncov: 0 (0.0%) unkwn: 3 (75.0%) with 1 test
  [A]{Sc} Covered labels: {1}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: 1 test
  [A]{Sc}        1: Covered
                 2: Unknown
                 3: Unknown
                 4: Unknown
          Coverage: (1/4) 25.0%
  $ seacoral --tools cbmc --config clabel.toml
  [A]{Sc} Starting to log into `_sc/late-segfault.c-DC-@3/logs/3.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Current coverage statistics for `f':
          cov: 1 (25.0%) uncov: 0 (0.0%) unkwn: 3 (75.0%) with 1 test
          rte: 1 test
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `f':
          cov: 1 (25.0%) uncov: 0 (0.0%) unkwn: 3 (75.0%) with 1 test
  [A]{Sc} Covered labels: {1}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: 1 test
  [A]{Sc}        1: Covered
                 2: Unknown
                 3: Unknown
                 4: Unknown
          Coverage: (1/4) 25.0%
