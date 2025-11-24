  $ seacoral --tools cbmc
  [A]{Sc} Starting to log into `_sc/ignored-global.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `f':
          cov: 2 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 2 tests
  [A]{Sc} Covered labels: {1, 2}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
          Coverage: (2/2) 100.0%

Check that `__unused_global` is not symbolized:
  $ tail --lines +7 _sc/last/cbmc/harness.c
  int main () {
    int b;
    
    int __tmp_0 = nondet_int();
    b = __tmp_0;
    __CPROVER_input("b", __tmp_0);
    (void) f (b);
    sc_cover_statement(1);
    sc_cover_statement(2);
    
    return 0;
  }
       
