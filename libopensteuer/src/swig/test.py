from libopensteuer import *

eingabe = LibOpenSteuer_Est_Eingabe_New()
ausgabe = LibOpenSteuer_Est_Ausgabe_New()
lstein =  LibOpenSteuer_Lst_Eingabe_New()
lstaus =  LibOpenSteuer_Lst_Ausgabe_New()

eks = 10000
print "Calculating Einkommensteuer for Einkommen of %d Euro.\n" % eks

LibOpenSteuer_Est_Eingabe_Set_ZVE(eingabe, eks)

# Now calculate it
LibOpenSteuer_Einkommensteuer(eingabe, ausgabe)

print "Finished calculating. Result is:"
print "Einkommenst Grund_Tab = %d Euro" % \
      LibOpenSteuer_Est_Ausgabe_Get_Grund_Tab(ausgabe)
print "Einkommenst Splitting_Tab = %d Euro\n" % \
      LibOpenSteuer_Est_Ausgabe_Get_Splitting_Tab(ausgabe)

print "Calculating Lohnsteuer for Lohn of %d Euro. Result is:\n" % eks

# Watch out: the arguments are cent 
LibOpenSteuer_Lst_Eingabe_Set_RE4(lstein, eks*100)

for stkl in range(1,7):
    LibOpenSteuer_Lst_Eingabe_Set_STKL(lstein, stkl)
    LibOpenSteuer_Lohnsteuer(lstein, lstaus)

    print " STKL %d: LSTLZZ %6.2f EUR, SOLZLZZ %6.2f EUR, K8 %6.2f EUR, K9 %6.2f EUR" % \
          (stkl,
           LibOpenSteuer_Lst_Ausgabe_Get_LSTLZZ(lstaus)/100.0,
           LibOpenSteuer_Lst_Ausgabe_Get_SOLZLZZ(lstaus)/100.0,
           LibOpenSteuer_Lst_Ausgabe_Get_K8(lstaus)/100.0,
           LibOpenSteuer_Lst_Ausgabe_Get_K9(lstaus)/100.0)

