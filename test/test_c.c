#include <stdio.h>
#include <stdlib.h>
#include <libopensteuer.h>

int main(void)
{
  long r, lstlzz;
  int stkl, year;

  printf ("\nUnd hier das C-Programm...\n");
  printf ("\nLohnsteuerberechnung (alle Beträge in Euro):\n");
  for (year = get_first_year (); year <= get_last_year (); ++year)
    {
      printf ("\nJahr: %d\n", year);
      printf ("     RE4       I      II     III      IV       V      VI\n");
      printf ("--------------------------------------------------------\n");
      for (r = 2; r <=24; ++r)
        {
          printf ("%8ld", r * 2500);
          for (stkl = 1; stkl <= 6; ++stkl)
            {
              reset_all ();
              switch (set_year (year))
                {
                case 0 :
                  set_lzz (1);
                  set_re4 (r * 250000);
                  set_stkl (stkl);
                  calc_lst ();
                  lstlzz = get_lstlzz () / 100;
                  printf ("%8ld", lstlzz);
                  break;
                case -1 :
                  printf (" general");
                  break;
                case -2 :
                  printf (" program");
                  break;
                case -3 :
                  printf ("   range");
                  break;
                case -10 :
                  printf ("   undef");
                  break;
                default :
                  printf (" default");
                  break;
                };
            };
          printf ("\n");
        }
    }
  printf ("\nEinkommensteuerberechnung (alle Beträge in Euro):\n");
  for (year = get_first_year (); year <= get_last_year (); ++year)
    {
      printf ("\nJahr: %d\n", year);
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
