/*
  C wrappers for the Ada implementation of the Steuer-PAP.

  For connection Ada and C/C++, see e.g.

  http://www.adapower.com/articles/howto-gdllc.html -- looks easy
  enough to understand. Slightly different because it's about WIndows
  DLLs, though.

  http://www.ghs.com/download/whitepapers/ada_c++.pdf -- rather
  explains the other way round, but nevertheless a good read.

  http://www.adapower.com/rm95/arm95_264.html#SEC264 -- urg, the
  specification itself. Hard to read.



------------------------------------------------------------------------------
-- OpenSteuer -  An open-source taxprogram based on german tax laws         --
--                                                                          --
-- The latest version is available at:                                      --
--    http://www.opensteuer.de                                              --
------------------------------------------------------------------------------
-- COPYRIGHT (C) 2003:                                                      --
--    Hannes Birnbacher, Martin Klaiber, Sigrid W�rsd�rfer.                 --
--                                                                          --
-- AUTHOR:                                                                  --
--    Martin Klaiber.                                                       --
--                                                                          --
-- LICENCE:                                                                 --
--    This file is part of OpenSteuer.                                      --
--                                                                          --
--    OpenSteuer is free software; you can redistribute it and/or modify    --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation; either version 2 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    OpenSteuer is distributed in the hope that it will be useful, but     --
--    WITHOUT ANY WARRANTY; without even the implied warranty of            --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     --
--    General Public License for more details.                              --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with OpenSteuer;  see file COPYING.  If not, write to the       --
--       Free Software Foundation, Inc.                                     --
--       59 Temple Place, Suite 330                                         --
--       Boston, MA 02111-1307                                              --
--       USA                                                                --
------------------------------------------------------------------------------
*/

typedef struct OpenSteuer_PAP OpenSteuer_PAP;

/** Constructor. 

This function should create a new OpenSteuer_PAP object and return a
reference/pointer to it. Of course the C language doesn't know about
objects. But since only the pointer is passed to functions defined in
this header file, having the pointer is sufficient for C.

Note: I call this a "Constructor" and used the name _new() because
that's how this concept is called in C++, which I am familiar
with. But you can of course choose any other name -- e.g. call this an
"object creation function" and use the name _create().
*/
OpenSteuer_PAP *OpenSteuer_PAP_new();

/** Destructor. 

The same comments as with the Constructor apply here as well. In other
languages this function is called destroy(), free(), or unref(). 
*/
void OpenSteuer_PAP_delete(OpenSteuer_PAP *p);


/*   
     -- Die im PAP verwendeten Variablen (siehe Opensteuer-Homepage). Die
     -- Erl�uterungen sind ebenfalls aus dem PAP �bernommen. Eigene Anmerkungen
     -- sind als solche gekennzeichnet. Hier nicht definierte Typen sind in
     -- OpenSteuer_Globals definiert.
   
     ----------------------
     -- Eingangsparamter --
     ----------------------
*/   
   
/**   -- ALTER1
   
-- Wert = 1, wenn das 64. Lebensjahr vor Beginn des Kalenderjahres
-- vollendet wurde, in dem der Lohnzahlungszeitraum endet (� 24 a EStG),
-- sonst Wert = 0.
*/   
void OpenSteuer_PAP_set_ALTER1(OpenSteuer_PAP *p, int alter1);
   
/**   -- HINZUR
   
-- In der Lohnsteuerkarte des Arbeitnehmers eingetragener
-- Hinzurechnungsbetrag f�r den Lohnzahlungszeitraum in Cents.
*/   
void OpenSteuer_PAP_set_HINZUR(OpenSteuer_PAP *p, int hinzur);
   
/**   -- JFREIB
   
-- Jahresfreibetrag nach Massgabe der Eintragungen auf der
-- Lohnsteuerkarte in Cents (ggf. 0).
*/   
void OpenSteuer_PAP_set_JFREIB(OpenSteuer_PAP *p, int jfreib);
   
/**   -- JHINZU
   
-- Jahreshinzurechnungsbetrag in Cents (ggf. 0).

*/   
void OpenSteuer_PAP_set_JHINZU(OpenSteuer_PAP *p, int jhinzu);
   
/**   -- JRE4
   
-- Voraussichtlicher Jahresarbeitslohn ohne sonstige Bez�ge und ohne
-- Verg�tung f�r mehrj�hrige T�tigkeit in Cents (ggf. 0) Anmerkung: Die
-- Eingabe dieses Feldes ist erforderlich bei Eingabe "sonstiger Bez�ge"
-- �ber 150 EUR (Feld SONSTB) oder bei Eingabe der "Verg�tung f�r
-- mehrj�hrige T�tigkeit" (Feld VMT).
*/   
void OpenSteuer_PAP_set_JRE4(OpenSteuer_PAP *p, int jre4);
   
/**   -- JVBEZ
   
-- In JRE4 enthaltene Versorgungsbez�ge in Cents (ggf. 0).

*/   
void OpenSteuer_PAP_set_JVBEZ(OpenSteuer_PAP *p, int jvbez);
   
/**   -- KRV
   
-- 1 = der Arbeitnehmer ist im Lohnzahlungszeitraum in der gesetzlichen
-- Rentenversicherung versicherungsfrei und geh�rt zu den in � 10 c Abs.
-- 3 EStG genannten Personen. Bei anderen Arbeitnehmern ist "0"
-- einzusetzen.  F�r die Zuordnung sind allein die dem Arbeitgeber
-- ohnehin bekannten Tatsachen massgebend; zus�tzliche Ermittlungen
-- braucht der Arbeitgeber nicht anzustellen.
*/   
void OpenSteuer_PAP_set_KRV(OpenSteuer_PAP *p, int krv);
   
/**   -- LZZ
   
-- Lohnzahlungszeitraum: 1 = Jahr, 2 = Monat, 3 = Woche, 4 = Tag.
   
*/
void OpenSteuer_PAP_set_LZZ(OpenSteuer_PAP *p, int lzz);
   
/**   -- R
   
-- Religionsgemeinschaft des Arbeitnehmers lt. Lohnsteuerkarte (bei
-- keiner Religionszugeh�rigkeit = 0).
   
-- Anmerkung: Unklar, was das f�r ein Typ sein soll, vielleicht ein
-- String. Da wir die Kirchensteuer aber derzeit nicht ausrechnen,
-- lassen wir nur 0 und 1 als Wert zu:
*/   
void OpenSteuer_PAP_set_R(OpenSteuer_PAP *p, int r);
   
/**   -- RE4
   
-- Steuerpflichtiger Arbeitslohn vor Ber�cksichtigung des
-- Versorgungs-Freibetrags, des Altersentlastungsbetrags und des auf der
-- Lohnsteuerkarte f�r den Lohnzahlungszeitraum eingetragenen Freibetrags in Cents.
*/
void OpenSteuer_PAP_set_RE4(OpenSteuer_PAP *p, int re4);
   
/**   -- SONSTB
   
-- Sonstige Bez�ge (ohne Verg�tung aus mehrj�hriger T�tigkeit) in Cents
-- (ggf. 0).
*/   
void OpenSteuer_PAP_set_SONSTB(OpenSteuer_PAP *p, int);
   
/**   -- STKL
   
-- Steuerklasse: 1 = I, 2 = II, 3 = III, 4 = IV, 5 = V, 6 = VI.
   
*/
void OpenSteuer_PAP_set_STKL(OpenSteuer_PAP *p, int);

   
/**   -- VBEZ
   
-- In RE4 enthaltene Versorgungsbez�ge in Cents (ggf. 0).
   
*/
void OpenSteuer_PAP_set_VBEZ(OpenSteuer_PAP *p, int);
   
/**   -- VBS
   
-- In SONSTB enthaltene Versorgungsbez�ge in Cents (ggf. 0).
   
*/
void OpenSteuer_PAP_set_VBS(OpenSteuer_PAP *p, int);
   
/**   -- VMT
   
-- Verg�tung f�r mehrj�hrige T�tigkeit in Cents (ggf. 0).

*/   
void OpenSteuer_PAP_set_VMT(OpenSteuer_PAP *p, int);
   
/**   -- WFUNDF
   
-- In der Lohnsteuerkarte des Arbeitnehmers eingetragener Freibetrag f�r
-- den Lohnzahlungszeitraum in Cents.
*/   
void OpenSteuer_PAP_set_WFUNDF(OpenSteuer_PAP *p, int);
   
/** -- ZKF
   
-- Zahl der Kinderfreibetr�ge (eine Dezimalstelle, nur bei Steuerklassen
-- I, II, III und IV).
   
-- Anmerkung: 1,2 oder 2,7 Kinderfreibetr�ge sind nach dieser Definition
-- erlaubt, sind aber nicht realistisch. Vielleicht w�re ein eigener Typ,
-- der das verhindern kann sinnvoll.
*/   
void OpenSteuer_PAP_set_ZKF(OpenSteuer_PAP *p, int);
   
/*
  ----------------------------------------------------------------------------
   
  -----------------------
  -- Ausgangsparameter --
  -----------------------
   
*/
   
/**   -- BK
   
-- Bemessungsgrundlage f�r die Kirchenlohnsteuer in Cents.
*/   
int OpenSteuer_PAP_get_BK(const OpenSteuer_PAP *p);
   
/**   -- BKS
   
-- Bemessungsgrundlage der sonstigen Eink�nfte (ohne Verg�tung f�r
-- mehrj�hrige T�tigkeit) f�r die Kirchenlohnsteuer in Cents.
*/   
int OpenSteuer_PAP_get_BKS(const OpenSteuer_PAP *p);
   
/**   -- BKV
   
-- Bemessungsgrundlage der Verg�tung f�r mehrj�hrige T�tigkeit f�r die
-- Kirchenlohnsteuer in Cents.
*/   
int OpenSteuer_PAP_get_BKV(const OpenSteuer_PAP *p);
   
/**   -- LSTLZZ
   
-- F�r den Lohnzahlungszeitraum einzubehaltende Lohnsteuer in Cents.
   
*/
int OpenSteuer_PAP_get_LSTLZZ(const OpenSteuer_PAP *p);
   
/**   -- LZALOG
   
-- Obergrenze der Tabellenstufe in der Lohnsteuertabelle f�r den
-- Lohnzahlungszeitraum (nur, wenn Tabellen errechnet werden sollen) in Cents.
*/   
int OpenSteuer_PAP_get_LZALOG(const OpenSteuer_PAP *p);
   
/**   -- LZALUG
   
-- Untergrenze der Tabellenstufe in der Lohnsteuertabelle f�r den
-- Lohnzahlungszeitraum (nur, wenn Tabellen errechnet werden sollen) in Cents.
*/   
int OpenSteuer_PAP_get_LZALUG(const OpenSteuer_PAP *p);
   
/**   -- SOLZLZZ
   
-- F�r den Lohnzahlungszeitraum einzubehaltender Solidarit�tszuschlag in
-- Cents.
*/   
int OpenSteuer_PAP_get_SOLZLZZ(const OpenSteuer_PAP *p);
   
/**   -- SOLZS
   
-- Solidarit�tszuschlag f�r sonstige Bez�ge (ohne Verg�tung f�r mehrj�hrige
-- T�tigkeit) in Cents.
*/   
int OpenSteuer_PAP_get_SOLZS(const OpenSteuer_PAP *p);
   
/**   -- SOLZV
   
-- Solidarit�tszuschlag f�r die Verg�tung f�r mehrj�hrigeT�tigkeit in
-- Cents.
*/   
int OpenSteuer_PAP_get_SOLZV(const OpenSteuer_PAP *p);
   
/**   -- STS
   
-- Lohnsteuer f�r sonstige Eink�nfte (ohne Verg�tung f�r mehrj�hrige
-- T�tigkeit) in Cents.
*/   
int OpenSteuer_PAP_get_STS(const OpenSteuer_PAP *p);
   
/**   -- STV
   
-- Lohnsteuer f�r Verg�tung f�r mehrj�hrige T�tigkeit in Cents.
   
*/
int OpenSteuer_PAP_get_STV(const OpenSteuer_PAP *p);
   
/*
  ----------------------------------------------------------------------------
   
  ----------------------------
  -- Die Prozeduren im Body --
  ----------------------------
*/

void OpenSteuer_PAP_LST2002(OpenSteuer_PAP *p);
void OpenSteuer_PAP_UPTAB02(OpenSteuer_PAP *p);
void OpenSteuer_PAP_MLSTJAHR_2(OpenSteuer_PAP *p);
void OpenSteuer_PAP_Reset_PAP_Eingabe(OpenSteuer_PAP *p);
void OpenSteuer_PAP_Reset_PAP_Intern(OpenSteuer_PAP *p);
   

/*
  ----------------------------------------------------------------------------
   
  --------------------
  -- Interne Felder --
  --------------------
   
  -- Das Programm verwendet intern folgende Felder (wenn ggf. solche Felder
  -- im Umfeld des Programms verwendet werden sollen, k�nnen sie als
  -- Ausgangsparameter behandelt werden, soweit sie nicht w�hrend des
  -- Programmdurchlaufs noch ver�ndert wurden). Die internen Felder m�ssen
  -- vor Aufruf des Programms gel�scht werden:
   
  -- Anmerkung: extern sichtbare interne Felder sind nicht das Wahre. Das
  -- sollte noch ge�ndert werden.
*/

   
/**   -- KZTAB
   
-- Kennzahl f�r die Einkommensteuer-Tabellenart:
-- 1 = Grundtabelle, 2 = Splittingtabelle.
*/   
int OpenSteuer_PAP_get_KZTAB(const OpenSteuer_PAP *p);
   
/**   -- ST
   
-- Tarifliche Einkommensteuer in EUR.
   
*/
int OpenSteuer_PAP_get_ST(const OpenSteuer_PAP *p);
   
/**   -- ZVE
   
-- Zu versteuerndes Einkommen in EUR.
   
*/
int OpenSteuer_PAP_get_ZVE(const OpenSteuer_PAP *p);
   
