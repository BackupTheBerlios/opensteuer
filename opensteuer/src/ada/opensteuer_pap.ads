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

with OpenSteuer_Globals; use OpenSteuer_Globals;
with Interfaces.C;

package OpenSteuer_PAP is
   
   -- Die im PAP verwendeten Variablen (siehe Opensteuer-Homepage). Die
   -- Erl�uterungen sind ebenfalls aus dem PAP �bernommen. Eigene Anmerkungen
   -- sind als solche gekennzeichnet. Hier nicht definierte Typen sind in
   -- OpenSteuer_Globals definiert.
   
   ----------------------
   -- Eingangsparamter --
   ----------------------
   
   ----------------------------------------------------------------------------
   
   -- ALTER1
   
   -- Wert = 1, wenn das 64. Lebensjahr vor Beginn des Kalenderjahres
   -- vollendet wurde, in dem der Lohnzahlungszeitraum endet (� 24 a EStG),
   -- sonst Wert = 0.
   
   ALTER1 : ALTER1_Type;
   
   ----------------------------------------------------------------------------
   
   -- HINZUR
   
   -- In der Lohnsteuerkarte des Arbeitnehmers eingetragener
   -- Hinzurechnungsbetrag f�r den Lohnzahlungszeitraum in Cents.
   
   HINZUR : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- JFREIB
   
   -- Jahresfreibetrag nach Massgabe der Eintragungen auf der
   -- Lohnsteuerkarte in Cents (ggf. 0).
   
   JFREIB : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- JHINZU
   
   -- Jahreshinzurechnungsbetrag in Cents (ggf. 0).
   
   JHINZU : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- JRE4
   
   -- Voraussichtlicher Jahresarbeitslohn ohne sonstige Bez�ge und ohne
   -- Verg�tung f�r mehrj�hrige T�tigkeit in Cents (ggf. 0) Anmerkung: Die
   -- Eingabe dieses Feldes ist erforderlich bei Eingabe "sonstiger Bez�ge"
   -- �ber 150 EUR (Feld SONSTB) oder bei Eingabe der "Verg�tung f�r
   -- mehrj�hrige T�tigkeit" (Feld VMT).
   
   JRE4 : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- JVBEZ
   
   -- In JRE4 enthaltene Versorgungsbez�ge in Cents (ggf. 0).
   
   JVBEZ : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- KRV
   
   -- 1 = der Arbeitnehmer ist im Lohnzahlungszeitraum in der gesetzlichen
   -- Rentenversicherung versicherungsfrei und geh�rt zu den in � 10 c Abs.
   -- 3 EStG genannten Personen. Bei anderen Arbeitnehmern ist "0"
   -- einzusetzen.  F�r die Zuordnung sind allein die dem Arbeitgeber
   -- ohnehin bekannten Tatsachen massgebend; zus�tzliche Ermittlungen
   -- braucht der Arbeitgeber nicht anzustellen.
   
   KRV : KRV_Type;
   
   ----------------------------------------------------------------------------
   
   -- LZZ
   
   -- Lohnzahlungszeitraum: 1 = Jahr, 2 = Monat, 3 = Woche, 4 = Tag.
   
   LZZ : LZZ_Type;
   
   ----------------------------------------------------------------------------
   
   -- R
   
   -- Religionsgemeinschaft des Arbeitnehmers lt. Lohnsteuerkarte (bei
   -- keiner Religionszugeh�rigkeit = 0).
   
   -- Anmerkung: Unklar, was das f�r ein Typ sein soll, vielleicht ein
   -- String. Da wir die Kirchensteuer aber derzeit nicht ausrechnen,
   -- lassen wir nur 0 und 1 als Wert zu:
   
   R : R_Type;
   
   ----------------------------------------------------------------------------
   
   -- RE4
   
   -- Steuerpflichtiger Arbeitslohn vor Ber�cksichtigung des
   -- Versorgungs-Freibetrags, des Altersentlastungsbetrags und des auf der
   -- Lohnsteuerkarte f�r den Lohnzahlungszeitraum eingetragenen Freibetrags
   -- in Cents.
   
   RE4 : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- SONSTB
   
   -- Sonstige Bez�ge (ohne Verg�tung aus mehrj�hriger T�tigkeit) in Cents
   -- (ggf. 0).
   
   SONSTB : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- STKL
   
   -- Steuerklasse: 1 = I, 2 = II, 3 = III, 4 = IV, 5 = V, 6 = VI.
   
   STKL : STKL_Type;
   
   ----------------------------------------------------------------------------
   
   -- VBEZ
   
   -- In RE4 enthaltene Versorgungsbez�ge in Cents (ggf. 0).
   
   VBEZ : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- VBS
   
   -- In SONSTB enthaltene Versorgungsbez�ge in Cents (ggf. 0).
   
   VBS : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- VMT
   
   -- Verg�tung f�r mehrj�hrige T�tigkeit in Cents (ggf. 0).
   
   VMT : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- WFUNDF
   
   -- In der Lohnsteuerkarte des Arbeitnehmers eingetragener Freibetrag f�r
   -- den Lohnzahlungszeitraum in Cents.
   
   WFUNDF : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- ZKF
   
   -- Zahl der Kinderfreibetr�ge (eine Dezimalstelle, nur bei Steuerklassen
   -- I, II, III und IV).
   
   -- Anmerkung: 1,2 oder 2,7 Kinderfreibetr�ge sind nach dieser Definition
   -- erlaubt, sind aber nicht realistisch. Vielleicht w�re ein eigener Typ,
   -- der das verhindern kann sinnvoll.
   
   ZKF : RW_1;
   
   ----------------------------------------------------------------------------
   
   -----------------------
   -- Ausgangsparameter --
   -----------------------
   
   ----------------------------------------------------------------------------
   
   -- BK
   
   -- Bemessungsgrundlage f�r die Kirchenlohnsteuer in Cents.
   
   BK : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- BKS
   
   -- Bemessungsgrundlage der sonstigen Eink�nfte (ohne Verg�tung f�r
   -- mehrj�hrige T�tigkeit) f�r die Kirchenlohnsteuer in Cents.
   
   BKS : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- BKV
   
   -- Bemessungsgrundlage der Verg�tung f�r mehrj�hrige T�tigkeit f�r die
   -- Kirchenlohnsteuer in Cents.
   
   BKV : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- LSTLZZ
   
   -- F�r den Lohnzahlungszeitraum einzubehaltende Lohnsteuer in Cents.
   
   LSTLZZ : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- LZALOG
   
   -- Obergrenze der Tabellenstufe in der Lohnsteuertabelle f�r den
   -- Lohnzahlungszeitraum (nur, wenn Tabellen errechnet werden sollen) in
   -- Cents.
   
   LZALOG : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- LZALUG
   
   -- Untergrenze der Tabellenstufe in der Lohnsteuertabelle f�r den
   -- Lohnzahlungszeitraum (nur, wenn Tabellen errechnet werden sollen) in
   -- Cents.
   
   LZALUG : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- SOLZLZZ
   
   -- F�r den Lohnzahlungszeitraum einzubehaltender Solidarit�tszuschlag in
   -- Cents.
   
   SOLZLZZ : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- SOLZS
   
   -- Solidarit�tszuschlag f�r sonstige Bez�ge (ohne Verg�tung f�r mehrj�hrige
   -- T�tigkeit) in Cents.
   
   SOLZS : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- SOLZV
   
   -- Solidarit�tszuschlag f�r die Verg�tung f�r mehrj�hrigeT�tigkeit in
   -- Cents.
   
   SOLZV : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- STS
   
   -- Lohnsteuer f�r sonstige Eink�nfte (ohne Verg�tung f�r mehrj�hrige
   -- T�tigkeit) in Cents.
   
   STS : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- STV
   
   -- Lohnsteuer f�r Verg�tung f�r mehrj�hrige T�tigkeit in Cents.
   
   STV : Cent_0;
   
   ----------------------------------------------------------------------------
   
   ----------------------------
   -- Die Prozeduren im Body --
   ----------------------------
   
   procedure LST2002;
   procedure UPTAB02;
   procedure MLSTJAHR_2;
   procedure Reset_PAP_Eingabe;
   procedure Reset_PAP_Intern;
   
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
   
   ----------------------------------------------------------------------------
   
   -- KZTAB
   
   -- Kennzahl f�r die Einkommensteuer-Tabellenart:
   -- 1 = Grundtabelle, 2 = Splittingtabelle.
   
   KZTAB : KZTAB_Type;
   
   ----------------------------------------------------------------------------
   
   -- ST
   
   -- Tarifliche Einkommensteuer in EUR.
   
   ST : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- ZVE
   
   -- Zu versteuerndes Einkommen in EUR.
   
   ZVE : Euro_0;
   
   ----------------------------------------------------------------------------
   
private
   
   ----------------------------------------------------------------------------
   
   -- ALTE
   
   -- Altersentlastungsbetrag in Cents.
   
   ALTE : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- ANP
   
   -- Arbeitnehmer-Pauschbetrag in EUR.
   
   ANP : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- ANTEIL1
   
   -- Auf den Lohnzahlungszeitraum entfallender Anteil von Jahreswerten auf
   -- ganze Cents abgerundet.
   
   ANTEIL1 : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- ANTEIL2
   
   -- Auf den Lohnzahlungszeitraum entfallender Anteil von Jahreswerten auf
   -- ganze Cents aufgerundet.
   
   ANTEIL2 : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- BMG
   
   -- Bemessungsgrundlage f�r Altersentlastungsbetrag in Cents.
   
   BMG : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- DIFF
   
   -- Differenz zwischen ST1 und ST2 in EUR.
   
   DIFF : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- FVB
   
   -- Versorgungs-Freibetrag in Cents.
   
   FVB : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- HFB
   
   -- Haushalts-Freibetrag in EUR.
   
   HFB : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- JBMG
   
   -- Jahressteuer nach � 51a EStG, aus der Solidarit�tszuschlag und
   -- Bemessungsgrundlage f�r die Kirchenlohnsteuer ermittelt werden in EUR.
   
   JBMG : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- JW
   
   -- Jahreswert, dessen Anteil f�r einen Lohnzahlungszeitraum in UPANTEIL
   -- errechnet werden soll in Cents.
   
   JW : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- KFB
   
   -- Summe der Kinderfreibetr�ge (einschliesslich Betreuungs- und
   -- Erziehungsfreibetr�ge) in EUR.
   
   KFB : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- LSTJAHR
   
   -- Jahreslohnsteuer in EUR.
   
   LSTJAHR : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- LSTLZZS
   
   -- F�r den Lohnzahlungszeitraum einzubehaltende Lohnsteuer (f�r RE4 und
   -- SONSTB) in Cents.
   
   LSTLZZS : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- LST1, LST2, LST3
   
   -- Zwischenfelder der Jahreslohnsteuer in Cents
   
   LST1, LST2, LST3 : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- MIST
   
   -- Mindeststeuer f�r die Steuerklassen V und VI in EUR.
   
   MIST : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- RE4LZZ
   
   -- Arbeitslohn des Lohnzahlungszeitraums nach Abzug von
   -- Versorgungs-Freibetrag, Altersentlastungsbetrag und in der
   -- Lohnsteuerkarte eingetragenem Freibetrag und Hinzurechnung eines
   -- Hinzurechnungsbetrags in Cents. Entspricht dem Arbeitslohn, f�r den
   -- die Lohnsteuer im personellen Verfahren aus der zum
   -- Lohnzahlungszeitraum geh�renden Tabelle abgelesen w�rde.
   
   RE4LZZ : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- RE4LZZV
   
   -- Arbeitslohn des Lohnzahlungszeitraums nach Abzug von
   -- Versorgungs-Freibetrag und Altersentlastungsbetrag in Cents zur
   -- Berechnung der Vorsorgepauschale.
   
   RE4LZZV : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- RE4O
   
   -- Obergrenze der Tabellenstufe zur Berechnung der Vorsorgepauschale in
   -- EUR.
   
   RE4O : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- RE4U
   
   -- Untergrenze der Tabellenstufe in der Jahreslohnsteuertabelle in EUR.
   
   RE4U : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- RUND
   
   -- Feld f�r die Abrundung von Betr�gen in UPRUND36 auf einen ohne Rest
   -- durch 36 teilbaren Betrag in EUR.
   
   RUND : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- RW
   
   -- Rechenwert mit 3 Dezimalstellen.
   
   RW : RW_3;
   
   ----------------------------------------------------------------------------
   
   -- SAP
   
   -- Sonderausgaben-Pauschbetrag in EUR.
   
   SAP : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- SOLZFREI
   
   -- Freigrenze f�r den Solidarit�tszuschlag in EUR.
   
   SOLZFREI : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- SOLZJ
   
   -- Solidarit�tszuschlag auf die Jahreslohnsteuer in EUR, C (2 Dezimalstellen).
   
   SOLZJ : RW_2;
   
   ----------------------------------------------------------------------------
   
   -- SOLZMIN
   
   -- Zwischenwert f�r den Solidarit�tszuschlag auf die Jahreslohnsteuer in
   -- EUR, C (2 Dezimalstellen).
   
   SOLZMIN : RW_2;
   
   ----------------------------------------------------------------------------
   
   -- ST1
   
   -- Tarifliche Einkommensteuer auf das 1,25-fache ZX in EUR.
   
   ST1 : Euro_0; -- keine Kommastelle???
   
   ----------------------------------------------------------------------------
   
   -- ST2
   
   -- Tarifliche Einkommensteuer auf das 0,75-fache ZX in EUR.
   
   ST2 : Euro_0; -- keine Kommastelle???
   
   ----------------------------------------------------------------------------
   
   -- TW
   
   -- Tabellenwerte mit den Spr�ngen der Tabellenstufen f�r die Berechnung
   -- von LZALOG (indiziert durch LZZ), in Cents.
   
   TW : Cent_0;
   
   ----------------------------------------------------------------------------
   
   -- VSP
   
   -- Vorsorgepauschale in EUR, C (2 Dezimalstellen).
   
   VSP : RW_2;
   
   ----------------------------------------------------------------------------
   
   -- VSPKURZ
   
   -- H�chstbetrag der Vorsorgepauschale nach � 10c Abs. 3 EStG in EUR.
   
   VSPKURZ : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- VSPMAX1
   
   -- H�chstbetrag der Vorsorgepauschale nach � 10c Abs. 2 Nr. 2 EStG in
   -- EUR.
   
   VSPMAX1 : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- VSPMAX2
   
   -- H�chstbetrag der Vorsorgepauschale nach � 10c Abs. 2 Nr. 3 EStG in
   -- EUR.
   
   VSPMAX2 : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- VSPO
   
   -- Vorsorgepauschale nach � 10c Abs. 2 Satz 2 EStG vor der
   -- H�chstbetragsberechnung in EUR, C (2 Dezimalstellen).
   
   VSPO : RW_2;
   
   ----------------------------------------------------------------------------
   
   -- VSPREST
   
   -- F�r den Abzug nach � 10c Abs. 2 Nrn. 2 und 3 EStG verbleibender Rest
   -- von VSPO in EUR, C (2 Dezimalstellen).
   
   VSPREST : RW_2;
   
   ----------------------------------------------------------------------------
   
   -- VSPVOR
   
   -- H�chstbetrag der Vorsorgepauschale nach � 10c Abs. 2 Nr. 1 EStG in
   -- EUR, C (2 Dezimalstellen).
   
   VSPVOR : RW_2;
   
   ----------------------------------------------------------------------------
   
   -- X
   
   -- Zu versteuerndes Einkommen gem. � 32a Abs. 1 und 2 EStG (1 Dezimalstelle).
   
   X : RW_1;
   
   ----------------------------------------------------------------------------
   
   -- Y
   
   -- gem. � 32a Abs. 1 EStG (5 Dezimalstellen).
   
   Y : RW_5;
   
   ----------------------------------------------------------------------------
   
   -- ZRE4
   
   -- Auf einen Jahreslohn hochgerechnetes RE4LZZ in EUR, C (2 Dezimalstellen).
   
   ZRE4 : RW_2;
   
   ----------------------------------------------------------------------------
   
   -- ZRE4VP
   
   -- Auf einen Jahreslohn hochgerechnetes RE4LZZV zur Berechnung der
   -- Vorsorgepauschale in EUR, C (2 Dezimalstellen).
   
   ZRE4VP : RW_2;
   
   ----------------------------------------------------------------------------
   
   -- ZTABFB
   
   -- Feste Tabellenfreibetr�ge (ohne Vorsorgepauschale) in EUR.
   
   ZTABFB : Euro_0;
   
   ----------------------------------------------------------------------------
   
   -- ZX, ZZX, HOCH, VERGL
   
   -- Zwischenfelder zu X f�r die Berechnung der Steuer nach � 39b Abs. 2
   -- Satz 8 EStG in EUR.
   
   ZX, ZZX, HOCH, VERGL : Euro_0;
   
   ----------------------------------------------------------------------------

   ---------------------------
   --  Export Declarations  --
   ---------------------------

   pragma Export (C, LST2002, "OpenSteuer_PAP_LST2002");
   pragma Export (C, UPTAB02, "OpenSteuer_PAP_UPTAB02");
   pragma Export (C, MLSTJAHR_2, "OpenSteuer_PAP_MLSTJAHR_2");
   pragma Export (C, Reset_PAP_Eingabe, "OpenSteuer_PAP_Reset_PAP_Eingabe");
   pragma Export (C, Reset_PAP_Intern, "OpenSteuer_PAP_Reset_PAP_Intern");
   -- exporting procedures is easy. Concept copied from 
   -- http://www.adapower.com/articles/howto-gdllc.html
   -- 
   -- FIXME: Functions to get and set the values are still needed, 
   -- as well as  Constructor/Destructor functions.

   
end OpenSteuer_PAP;
