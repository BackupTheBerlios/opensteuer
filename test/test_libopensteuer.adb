with Text_IO; use Text_IO;
with libopensteuer; use libopensteuer;

procedure test_libopensteuer is
   Eingabe : Lst_Eingabe;
   Ausgabe : Lst_Ausgabe;
begin
   Eingabe.RE4 := 10_000_00; -- entspricht 10.000,00 Euro
   Put_Line ("Alle Beträge in Cent! Lohnsteuer für:" & Cent'Image (Eingabe.RE4));
   for i in STKL_Type'Range loop
      Eingabe.STKL := i;
      Lohnsteuer (Eingabe, Ausgabe);
      Put_Line (
      " STKL" & STKL_Type'Image (i) & ":" &
      "   LSTLZZ:" & Cent'Image (Ausgabe.LSTLZZ) &
      "   SOLZLZZ:" & Cent'Image (Ausgabe.SOLZLZZ) &
      "   8%:" & Cent'Image (Ausgabe.K8) &
      "   9%:" & Cent'Image (Ausgabe.K9));
   end loop;
end test_libopensteuer;
