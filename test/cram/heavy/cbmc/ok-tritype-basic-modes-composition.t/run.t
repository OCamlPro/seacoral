We start cbmc in cover mode to quickly generate tests without focusing on
uncoverable labels.
  $ seacoral --tools cbmc --cbmc-mode cover
  [A]{Sc} Starting to log into `_sc/tritype.c-WM-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `tritype'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `tritype':
          cov: 92 (91.1%) uncov: 0 (0.0%) unkwn: 9 (8.9%) with 17 tests
  [A]{Sc} Covered labels:
          {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 19, 20,
           21, 23, 24, 25, 26, 27, 28, 30, 32, 33, 34, 35, 36, 37, 38, 39, 40,
           41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57,
           58, 59, 60, 61, 62, 63, 65, 66, 68, 69, 70, 71, 72, 73, 74, 75, 76,
           77, 78, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 92, 93, 94, 95,
           96, 97, 98, 99, 100}
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
                15: Unknown
                16: Covered
                17: Covered
                18: Covered
                19: Covered
                20: Covered
                21: Covered
                22: Unknown
                23: Covered
                24: Covered
                25: Covered
                26: Covered
                27: Covered
                28: Covered
                29: Unknown
                30: Covered
                31: Unknown
                32: Covered
                33: Covered
                34: Covered
                35: Covered
                36: Covered
                37: Covered
                38: Covered
                39: Covered
                40: Covered
                41: Covered
                42: Covered
                43: Covered
                44: Covered
                45: Covered
                46: Covered
                47: Covered
                48: Covered
                49: Covered
                50: Covered
                51: Covered
                52: Covered
                53: Covered
                54: Covered
                55: Covered
                56: Covered
                57: Covered
                58: Covered
                59: Covered
                60: Covered
                61: Covered
                62: Covered
                63: Covered
                64: Unknown
                65: Covered
                66: Covered
                67: Unknown
                68: Covered
                69: Covered
                70: Covered
                71: Covered
                72: Covered
                73: Covered
                74: Covered
                75: Covered
                76: Covered
                77: Covered
                78: Covered
                79: Unknown
                80: Covered
                81: Covered
                82: Covered
                83: Covered
                84: Covered
                85: Covered
                86: Covered
                87: Covered
                88: Covered
                89: Covered
                90: Covered
                91: Unknown
                92: Covered
                93: Covered
                94: Covered
                95: Covered
                96: Covered
                97: Covered
                98: Covered
                99: Covered
               100: Covered
               101: Unknown
          Coverage: (92/101) 91.1%

Then, we complete the analysis by running cbmc in assert mode to prove
the uncoverability of the remaining labels.
  $ seacoral --tools cbmc --cbmc-mode assert
  [A]{Sc} Starting to log into `_sc/tritype.c-WM-@2/logs/2.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Current coverage statistics for `tritype':
          cov: 92 (91.1%) uncov: 0 (0.0%) unkwn: 9 (8.9%) with 17 tests
          rte: none
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `tritype'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `tritype':
          cov: 92 (91.1%) uncov: 9 (8.9%) unkwn: 0 (0.0%) with 17 tests
  [A]{Sc} Covered labels:
          {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 19, 20,
           21, 23, 24, 25, 26, 27, 28, 30, 32, 33, 34, 35, 36, 37, 38, 39, 40,
           41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57,
           58, 59, 60, 61, 62, 63, 65, 66, 68, 69, 70, 71, 72, 73, 74, 75, 76,
           77, 78, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 92, 93, 94, 95,
           96, 97, 98, 99, 100}
  [A]{Sc} Uncoverable labels: {15, 22, 29, 31, 64, 67, 79, 91, 101}
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
                15: Unknown
                16: Covered
                17: Covered
                18: Covered
                19: Covered
                20: Covered
                21: Covered
                22: Unknown
                23: Covered
                24: Covered
                25: Covered
                26: Covered
                27: Covered
                28: Covered
                29: Unknown
                30: Covered
                31: Unknown
                32: Covered
                33: Covered
                34: Covered
                35: Covered
                36: Covered
                37: Covered
                38: Covered
                39: Covered
                40: Covered
                41: Covered
                42: Covered
                43: Covered
                44: Covered
                45: Covered
                46: Covered
                47: Covered
                48: Covered
                49: Covered
                50: Covered
                51: Covered
                52: Covered
                53: Covered
                54: Covered
                55: Covered
                56: Covered
                57: Covered
                58: Covered
                59: Covered
                60: Covered
                61: Covered
                62: Covered
                63: Covered
                64: Unknown
                65: Covered
                66: Covered
                67: Unknown
                68: Covered
                69: Covered
                70: Covered
                71: Covered
                72: Covered
                73: Covered
                74: Covered
                75: Covered
                76: Covered
                77: Covered
                78: Covered
                79: Unknown
                80: Covered
                81: Covered
                82: Covered
                83: Covered
                84: Covered
                85: Covered
                86: Covered
                87: Covered
                88: Covered
                89: Covered
                90: Covered
                91: Unknown
                92: Covered
                93: Covered
                94: Covered
                95: Covered
                96: Covered
                97: Covered
                98: Covered
                99: Covered
               100: Covered
               101: Unknown
          Coverage: (92/101) 91.1%
