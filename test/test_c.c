#include <stdio.h>
#include <stdlib.h>
#include <libopensteuer.h>

/*
 * Simple program to demonstrate how to use libopensteuer.
 * Author: Martin Klaiber (sorry for my bad C).
 */

int main(void)
{
  long r, lstlzz, error_code;
  int stkl, year;

  printf ("\nThis is the C-program...\n");
  printf ("\nLohnsteuer (all amounts in Euro):\n");
  /*
   * Error-checking by return-values is provided for every function.
   * I just show the principle on the basis of set_year (year).
   */
  for (year = get_first_year () - 1; year <= get_last_year (); ++year)
    {
      printf ("\nYear: %d\n", year);
      error_code = set_year (year);
      if (error_code == no_error_code)
        {
          printf ("     RE4       I      II     III      IV       V      VI\n");
          printf ("--------------------------------------------------------\n");
          for (r = 2; r <=24; ++r)
            {
              printf ("%8ld", r * 2500);
              for (stkl = 1; stkl <= 6; ++stkl)
                {
                  reset_all ();
                  set_year (year);
                  set_lzz (1);
                  /*
                   * The library expects all amounts in Euro-Cent.
                   * So we multiply by 100.
                   */
                  set_re4 (r * 250000);
                  set_stkl (stkl);
                  /*
                   * After all parameters are set we call the function
                   * which calculates the Lohnsteuer.
                   */
                  calc_lst ();
                  /*
                   * The same like above: the library returns Euro-Cents:
                   */
                  lstlzz = get_lstlzz () / 100;
                  printf ("%8ld", lstlzz);
                }
              printf ("\n");
            }
        }
      else if (error_code == constraint_error_code)
        printf ("Error: Value out of range.\n");
      else
        /* More elaboration could be done here */
        printf ("Unknown error.\n");
    }
  /*
   * Now an example for the Einkommensteuer.  The principle is the
   * same as above:
   */
  printf ("\nEinkommensteuer (all amounts in Euro):\n");
  for (year = get_first_year (); year <= get_last_year (); ++year)
    {
      printf ("\nYear: %d\n", year);
      printf ("     ZVE        Grundtabelle    Splittingtabelle\n");
      printf ("------------------------------------------------\n");
      for (r = 1; r <=10; ++r)
        {
          printf ("%8ld", r * 10000);
          reset_all ();
          set_year (year);
          set_lzz (1);
          set_zve (r * 1000000);
          calc_est ();
          printf ("%20ld", get_grundtab () / 100);
          printf ("%20ld\n", get_splittab () / 100);
        };
    }
  return 0;
}
