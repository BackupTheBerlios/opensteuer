#include <stdio.h>
#include <stdlib.h>
#include "libopensteuer.h"

int main(int argc, char **argv)
{
  Est_Eingabe *eingabe;
  Est_Ausgabe *ausgabe;
  Lst_Eingabe *lstein;
  Lst_Ausgabe *lstaus;
  int eks;
  int stkl;

  if (argc < 2) {
    printf("Usage: %s <Einkommensteuer-Euro>\n", argv[0]);
    return 1;
  }

  /* Init library */
  libopensteuerinit();
  eingabe = LibOpenSteuer_Est_Eingabe_New();
  ausgabe = LibOpenSteuer_Est_Ausgabe_New();
  lstein =  LibOpenSteuer_Lst_Eingabe_New();
  lstaus =  LibOpenSteuer_Lst_Ausgabe_New();

  eks = atoi(argv[1]);
  printf("Calculating Einkommensteuer for Einkommen of %d Euro.\n", eks);

  LibOpenSteuer_Est_Eingabe_Set_ZVE(eingabe, eks);

  /* Now calculate it */
  LibOpenSteuer_Einkommensteuer(eingabe, ausgabe);

  printf("Finished calculating. Result is:\n"
	 "Einkommenst Grund_Tab = %d Euro\n"
	 "Einkommenst Splitting_Tab = %d Euro\n",
	 LibOpenSteuer_Est_Ausgabe_Get_Grund_Tab(ausgabe),
	 LibOpenSteuer_Est_Ausgabe_Get_Splitting_Tab(ausgabe));

  printf("\nCalculating Lohnsteuer for Lohn of %d Euro. Result is:\n", eks);
  /* Watch out: the arguments are cent */
  LibOpenSteuer_Lst_Eingabe_Set_RE4(lstein, eks*100);

  for (stkl = 1; stkl <= 6; ++stkl)
    {
      LibOpenSteuer_Lst_Eingabe_Set_STKL(lstein, stkl);
      LibOpenSteuer_Lohnsteuer(lstein, lstaus);

      printf(" STKL %d: "
	     " LSTLZZ %6.2f EUR, SOLZLZZ %6.2f EUR, K8 %6.2f EUR, K9 %6.2f EUR\n",
	     stkl,
	     LibOpenSteuer_Lst_Ausgabe_Get_LSTLZZ(lstaus)/100.0,
	     LibOpenSteuer_Lst_Ausgabe_Get_SOLZLZZ(lstaus)/100.0,
	     LibOpenSteuer_Lst_Ausgabe_Get_K8(lstaus)/100.0,
	     LibOpenSteuer_Lst_Ausgabe_Get_K9(lstaus)/100.0);
    };

  /* Clean up library */
  libopensteuerfinal();

  return 0;
}
