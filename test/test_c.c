#include <stdio.h>
#include <stdlib.h>
#include "libopensteuer.h"

int main(void)
{
  long r, lstlzz;
  int stkl;

  printf ("Sprotzel.. Hier ist das C-Programm...\n");
  printf ("     RE4       I      II     III      IV       V      VI\n");
  printf ("--------------------------------------------------------\n");
  for (r = 2; r <=24; ++r) {
    printf ("%8ld", r * 2500);
    for (stkl = 1; stkl <= 6; ++stkl) {
      reset_all ();
      set_lzz (1);
      set_re4 (r * 250000);
      set_stkl (stkl);
      calc_lst ();
      lstlzz = get_lstlzz () / 100;
      
      printf("%8ld", lstlzz);
    };
    printf ("\n");
  }
  return 0;
}
