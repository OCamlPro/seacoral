
int f (int i, int j){
  switch(i){
  case 0: return j;
  case 1: j = 1; break;
  case 2: i = j;
  default: j = i; break;
  case 3: return i;
  }
  return 0;
}
