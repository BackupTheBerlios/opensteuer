--with LibOpenSteuer_Globals; use LibOpenSteuer_Globals;

package LibOpenSteuer.PAP is

   --------------------------------------
   -- Die Zahlentypen f�r Berechnungen --
   --------------------------------------
   
   -- Rechenwert mit 2 Nachkommastellen:
   type RW_2 is delta 0.01 digits OpenSteuer_Max_Digits;
   
   -- Rechenwert mit 3 Nachkommastellen:
   type RW_3 is delta 0.001 digits OpenSteuer_Max_Digits;
   
   -- Rechenwert mit 5 Nachkommastellen:
   type RW_5 is delta 0.00001 digits OpenSteuer_Max_Digits;
   
   -- Intern rechnen wir mit max. Genauigkeit:
   type RW_Intern is digits OpenSteuer_Max_Digits;
   
   -- der PAP verlangt, intern mit dreistelliger Genauigkeit
   -- zu rechnen. Scheint aber falsche Ergebnisse zu bringen:
   -- subtype RW_Intern is RW_3;
   

   -- Die im PAP verwendeten Variablen (siehe Opensteuer-Homepage). Die
   -- Erl�uterungen sind ebenfalls aus dem PAP �bernommen. Eigene Anmerkungen
   -- sind als solche gekennzeichnet. Hier nicht definierte Typen sind in
   -- OpenSteuer_Globals definiert.
   
   -----------------------
   -- Eingangsparameter --
   -----------------------
   
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
   
   HINZUR : Cent;
   
   ----------------------------------------------------------------------------
   
   -- JFREIB
   
   -- Jahresfreibetrag nach Massgabe der Eintragungen auf der
   -- Lohnsteuerkarte in Cents (ggf. 0).
   
   JFREIB : Cent;
   
   ----------------------------------------------------------------------------
   
   -- JHINZU
   
   -- Jahreshinzurechnungsbetrag in Cents (ggf. 0).
   
   JHINZU : Cent;
   
   ----------------------------------------------------------------------------
   
   -- JRE4
   
   -- Voraussichtlicher Jahresarbeitslohn ohne sonstige Bez�ge und ohne
   -- Verg�tung f�r mehrj�hrige T�tigkeit in Cents (ggf. 0) Anmerkung: Die
   -- Eingabe dieses Feldes ist erforderlich bei Eingabe "sonstiger Bez�ge"
   -- �ber 150 EUR (Feld SONSTB) oder bei Eingabe der "Verg�tung f�r
   -- mehrj�hrige T�tigkeit" (Feld VMT).
   
   JRE4 : Cent;
   
   ----------------------------------------------------------------------------
   
   -- JVBEZ
   
   -- In JRE4 enthaltene Versorgungsbez�ge in Cents (ggf. 0).
   
   JVBEZ : Cent;
   
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
   
   RE4 : Cent;
   
   ----------------------------------------------------------------------------
   
   -- SONSTB
   
   -- Sonstige Bez�ge (ohne Verg�tung aus mehrj�hriger T�tigkeit) in Cents
   -- (ggf. 0).
   
   SONSTB : Cent;
   
   ----------------------------------------------------------------------------
   
   -- STKL
   
   -- Steuerklasse: 1 = I, 2 = II, 3 = III, 4 = IV, 5 = V, 6 = VI.
   
   STKL : STKL_Type;
   
   ----------------------------------------------------------------------------
   
   -- VBEZ
   
   -- In RE4 enthaltene Versorgungsbez�ge in Cents (ggf. 0).
   
   VBEZ : Cent;
   
   ----------------------------------------------------------------------------
   
   -- VBS
   
   -- In SONSTB enthaltene Versorgungsbez�ge in Cents (ggf. 0).
   
   VBS : Cent;
   
   ----------------------------------------------------------------------------
   
   -- VMT
   
   -- Verg�tung f�r mehrj�hrige T�tigkeit in Cents (ggf. 0).
   
   VMT : Cent;
   
   ----------------------------------------------------------------------------
   
   -- WFUNDF
   
   -- In der Lohnsteuerkarte des Arbeitnehmers eingetragener Freibetrag f�r
   -- den Lohnzahlungszeitraum in Cents.
   
   WFUNDF : Cent;
   
   ----------------------------------------------------------------------------
   
   -- ZKF
   
   -- Zahl der Kinderfreibetr�ge (eine Dezimalstelle, nur bei Steuerklassen
   -- I, II, III und IV).
   
   -- Anmerkung: 1,2 oder 2,7 Kinderfreibetr�ge sind nach dieser Definition
   -- erlaubt, sind aber nicht realistisch. Vielleicht w�re ein eigener Typ,
   -- der das verhindern kann sinnvoll.
   
   ZKF : ZKF_Type;
   
   ----------------------------------------------------------------------------
   
   -----------------------
   -- Ausgangsparameter --
   -----------------------
   
   ----------------------------------------------------------------------------
   
   -- BK
   
   -- Bemessungsgrundlage f�r die Kirchenlohnsteuer in Cents.
   
   BK : Cent;
   
   ----------------------------------------------------------------------------
   
   -- BKS
   
   -- Bemessungsgrundlage der sonstigen Eink�nfte (ohne Verg�tung f�r
   -- mehrj�hrige T�tigkeit) f�r die Kirchenlohnsteuer in Cents.
   
   BKS : Cent;
   
   ----------------------------------------------------------------------------
   
   -- BKV
   
   -- Bemessungsgrundlage der Verg�tung f�r mehrj�hrige T�tigkeit f�r die
   -- Kirchenlohnsteuer in Cents.
   
   BKV : Cent;
   
   ----------------------------------------------------------------------------
   
   -- LSTLZZ
   
   -- F�r den Lohnzahlungszeitraum einzubehaltende Lohnsteuer in Cents.
   
   LSTLZZ : Cent;
   
   ----------------------------------------------------------------------------
   
   -- LZALOG
   
   -- Obergrenze der Tabellenstufe in der Lohnsteuertabelle f�r den
   -- Lohnzahlungszeitraum (nur, wenn Tabellen errechnet werden sollen) in
   -- Cents.
   
   LZALOG : Cent;
   
   ----------------------------------------------------------------------------
   
   -- LZALUG
   
   -- Untergrenze der Tabellenstufe in der Lohnsteuertabelle f�r den
   -- Lohnzahlungszeitraum (nur, wenn Tabellen errechnet werden sollen) in
   -- Cents.
   
   LZALUG : Cent;
   
   ----------------------------------------------------------------------------
   
   -- SOLZLZZ
   
   -- F�r den Lohnzahlungszeitraum einzubehaltender Solidarit�tszuschlag in
   -- Cents.
   
   SOLZLZZ : Cent;
   
   ----------------------------------------------------------------------------
   
   -- SOLZS
   
   -- Solidarit�tszuschlag f�r sonstige Bez�ge (ohne Verg�tung f�r mehrj�hrige
   -- T�tigkeit) in Cents.
   
   SOLZS : Cent;
   
   ----------------------------------------------------------------------------
   
   -- SOLZV
   
   -- Solidarit�tszuschlag f�r die Verg�tung f�r mehrj�hrigeT�tigkeit in
   -- Cents.
   
   SOLZV : Cent;
   
   ----------------------------------------------------------------------------
   
   -- STS
   
   -- Lohnsteuer f�r sonstige Eink�nfte (ohne Verg�tung f�r mehrj�hrige
   -- T�tigkeit) in Cents.
   
   STS : Cent;
   
   ----------------------------------------------------------------------------
   
   -- STV
   
   -- Lohnsteuer f�r Verg�tung f�r mehrj�hrige T�tigkeit in Cents.
   
   STV : Cent;
   
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
   
   ST : Euro;
   
   ----------------------------------------------------------------------------
   
   -- ZVE
   
   -- Zu versteuerndes Einkommen in EUR.
   
   ZVE : Euro;
   
   ----------------------------------------------------------------------------
   
private
   
   ----------------------------------------------------------------------------
   
   -- ALTE
   
   -- Altersentlastungsbetrag in Cents.
   
   ALTE : Cent;
   
   ----------------------------------------------------------------------------
   
   -- ANP
   
   -- Arbeitnehmer-Pauschbetrag in EUR.
   
   ANP : Euro;
   
   ----------------------------------------------------------------------------
   
   -- ANTEIL1
   
   -- Auf den Lohnzahlungszeitraum entfallender Anteil von Jahreswerten auf
   -- ganze Cents abgerundet.
   
   ANTEIL1 : Cent;
   
   ----------------------------------------------------------------------------
   
   -- ANTEIL2
   
   -- Auf den Lohnzahlungszeitraum entfallender Anteil von Jahreswerten auf
   -- ganze Cents aufgerundet.
   
   ANTEIL2 : Cent;
   
   ----------------------------------------------------------------------------
   
   -- BMG
   
   -- Bemessungsgrundlage f�r Altersentlastungsbetrag in Cents.
   
   BMG : Cent;
   
   ----------------------------------------------------------------------------
   
   -- DIFF
   
   -- Differenz zwischen ST1 und ST2 in EUR.
   
   DIFF : Euro;
   
   ----------------------------------------------------------------------------
   
   -- FVB
   
   -- Versorgungs-Freibetrag in Cents.
   
   FVB : Cent;
   
   ----------------------------------------------------------------------------
   
   -- HFB
   
   -- Haushalts-Freibetrag in EUR.
   
   HFB : Euro;
   
   ----------------------------------------------------------------------------
   
   -- JBMG
   
   -- Jahressteuer nach � 51a EStG, aus der Solidarit�tszuschlag und
   -- Bemessungsgrundlage f�r die Kirchenlohnsteuer ermittelt werden in EUR.
   
   JBMG : Euro;
   
   ----------------------------------------------------------------------------
   
   -- JW
   
   -- Jahreswert, dessen Anteil f�r einen Lohnzahlungszeitraum in UPANTEIL
   -- errechnet werden soll in Cents.
   
   JW : Cent;
   
   ----------------------------------------------------------------------------
   
   -- KFB
   
   -- Summe der Kinderfreibetr�ge (einschliesslich Betreuungs- und
   -- Erziehungsfreibetr�ge) in EUR.
   
   KFB : Euro;
   
   ----------------------------------------------------------------------------
   
   -- LSTJAHR
   
   -- Jahreslohnsteuer in EUR.
   
   LSTJAHR : Euro;
   
   ----------------------------------------------------------------------------
   
   -- LSTLZZS
   
   -- F�r den Lohnzahlungszeitraum einzubehaltende Lohnsteuer (f�r RE4 und
   -- SONSTB) in Cents.
   
   LSTLZZS : Cent;
   
   ----------------------------------------------------------------------------
   
   -- LST1, LST2, LST3
   
   -- Zwischenfelder der Jahreslohnsteuer in Cents
   
   LST1, LST2, LST3 : Cent;
   
   ----------------------------------------------------------------------------
   
   -- MIST
   
   -- Mindeststeuer f�r die Steuerklassen V und VI in EUR.
   
   MIST : Euro;
   
   ----------------------------------------------------------------------------
   
   -- RE4LZZ
   
   -- Arbeitslohn des Lohnzahlungszeitraums nach Abzug von
   -- Versorgungs-Freibetrag, Altersentlastungsbetrag und in der
   -- Lohnsteuerkarte eingetragenem Freibetrag und Hinzurechnung eines
   -- Hinzurechnungsbetrags in Cents. Entspricht dem Arbeitslohn, f�r den
   -- die Lohnsteuer im personellen Verfahren aus der zum
   -- Lohnzahlungszeitraum geh�renden Tabelle abgelesen w�rde.
   
   RE4LZZ : Cent;
   
   ----------------------------------------------------------------------------
   
   -- RE4LZZV
   
   -- Arbeitslohn des Lohnzahlungszeitraums nach Abzug von
   -- Versorgungs-Freibetrag und Altersentlastungsbetrag in Cents zur
   -- Berechnung der Vorsorgepauschale.
   
   RE4LZZV : Cent;
   
   ----------------------------------------------------------------------------
   
   -- RE4O
   
   -- Obergrenze der Tabellenstufe zur Berechnung der Vorsorgepauschale in
   -- EUR.
   
   RE4O : Euro;
   
   ----------------------------------------------------------------------------
   
   -- RE4U
   
   -- Untergrenze der Tabellenstufe in der Jahreslohnsteuertabelle in EUR.
   
   RE4U : Euro;
   
   ----------------------------------------------------------------------------
   
   -- RUND
   
   -- Feld f�r die Abrundung von Betr�gen in UPRUND36 auf einen ohne Rest
   -- durch 36 teilbaren Betrag in EUR.
   
   RUND : Euro;
   
   ----------------------------------------------------------------------------
   
   -- RW
   
   -- Rechenwert mit 3 Dezimalstellen.
   
   RW : RW_3;
   
   ----------------------------------------------------------------------------
   
   -- SAP
   
   -- Sonderausgaben-Pauschbetrag in EUR.
   
   SAP : Euro;
   
   ----------------------------------------------------------------------------
   
   -- SOLZFREI
   
   -- Freigrenze f�r den Solidarit�tszuschlag in EUR.
   
   SOLZFREI : Euro;
   
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
   
   ST1 : Euro; -- keine Kommastelle???
   
   ----------------------------------------------------------------------------
   
   -- ST2
   
   -- Tarifliche Einkommensteuer auf das 0,75-fache ZX in EUR.
   
   ST2 : Euro; -- keine Kommastelle???
   
   ----------------------------------------------------------------------------
   
   -- TW
   
   -- Tabellenwerte mit den Spr�ngen der Tabellenstufen f�r die Berechnung
   -- von LZALOG (indiziert durch LZZ), in Cents.
   
   TW : Cent;
   
   ----------------------------------------------------------------------------
   
   -- VSP
   
   -- Vorsorgepauschale in EUR, C (2 Dezimalstellen).
   
   VSP : RW_2;
   
   ----------------------------------------------------------------------------
   
   -- VSPKURZ
   
   -- H�chstbetrag der Vorsorgepauschale nach � 10c Abs. 3 EStG in EUR.
   
   VSPKURZ : Euro;
   
   ----------------------------------------------------------------------------
   
   -- VSPMAX1
   
   -- H�chstbetrag der Vorsorgepauschale nach � 10c Abs. 2 Nr. 2 EStG in
   -- EUR.
   
   VSPMAX1 : Euro;
   
   ----------------------------------------------------------------------------
   
   -- VSPMAX2
   
   -- H�chstbetrag der Vorsorgepauschale nach � 10c Abs. 2 Nr. 3 EStG in
   -- EUR.
   
   VSPMAX2 : Euro;
   
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
   
   ZTABFB : Euro;
   
   ----------------------------------------------------------------------------
   
   -- ZX, ZZX, HOCH, VERGL
   
   -- Zwischenfelder zu X f�r die Berechnung der Steuer nach � 39b Abs. 2
   -- Satz 8 EStG in EUR.
   
   ZX, ZZX, HOCH, VERGL : Euro;
   
   ----------------------------------------------------------------------------

end LibOpenSteuer.PAP;
