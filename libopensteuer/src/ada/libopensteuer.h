/*
  C wrappers for the Ada implementation of the Steuer-PAP.

  For connection Ada and C/C++, see e.g.

  http://www.adapower.com/articles/howto-gdllc.html -- looks easy
  enough to understand. Slightly different because it's about WIndows
  DLLs, though.

  http://www.ghs.com/download/whitepapers/ada_c++.pdf -- rather
  explains the other way round, but nevertheless a good read.

  http://www.adapower.com/rm95/arm95_264.html#SEC264 or
  http://www.adahome.com/rm95/rm9x-B-03.html -- urg, the specification
  itself. Hard to read.

  Additionally, the *info* files for gnat are important to read,
  especially the section "GNAT and libraries", subsection "Creating an
  Ada Library to be Used in a Non-Ada Context".
*/



/* The library elaboration procedure. The implementation for this is
   created by running 'gnatbind -Llibopensteuer libopensteuer' */
extern void libopensteuerinit (void);
          
/* The library finalization procedure. */
extern void libopensteuerfinal (void);

/* Type definitions. Only pointers to these types are used. */
typedef struct Est_Eingabe Est_Eingabe;
typedef struct Est_Ausgabe Est_Ausgabe;
typedef struct Lst_Eingabe Lst_Eingabe;
typedef struct Lst_Ausgabe Lst_Ausgabe;

extern void LibOpenSteuer_Einkommensteuer(const Est_Eingabe *eingabe, 
					  Est_Ausgabe *ausgabe);
extern void LibOpenSteuer_Lohnsteuer(const Lst_Eingabe *eingabe, 
				     Lst_Ausgabe *ausgabe);

extern Est_Eingabe *LibOpenSteuer_Est_Eingabe_New();
extern void LibOpenSteuer_Est_Eingabe_Set_ZVE(Est_Eingabe *e, int ZVE);

extern Est_Ausgabe *LibOpenSteuer_Est_Ausgabe_New();
extern int LibOpenSteuer_Est_Ausgabe_Get_Grund_Tab(const Est_Ausgabe *a);
extern int LibOpenSteuer_Est_Ausgabe_Get_Splitting_Tab(const Est_Ausgabe *a);

extern Lst_Eingabe *LibOpenSteuer_Lst_Eingabe_New();
extern void LibOpenSteuer_Lst_Eingabe_Set_ALTER1(Lst_Eingabe *E, int V);
extern void LibOpenSteuer_Lst_Eingabe_Set_ZKF(Lst_Eingabe *E, double V);
extern void LibOpenSteuer_Lst_Eingabe_Set_HINZUR(Lst_Eingabe *E, int V);
extern void LibOpenSteuer_Lst_Eingabe_Set_JFREIB(Lst_Eingabe *E, int V);
extern void LibOpenSteuer_Lst_Eingabe_Set_JHINZU(Lst_Eingabe *E, int V);
extern void LibOpenSteuer_Lst_Eingabe_Set_JRE4(Lst_Eingabe *E, int V);
extern void LibOpenSteuer_Lst_Eingabe_Set_JVBEZ(Lst_Eingabe *E, int V);
extern void LibOpenSteuer_Lst_Eingabe_Set_SONSTB(Lst_Eingabe *E, int V);
extern void LibOpenSteuer_Lst_Eingabe_Set_VBEZ(Lst_Eingabe *E, int V);
extern void LibOpenSteuer_Lst_Eingabe_Set_VBS(Lst_Eingabe *E, int V);
extern void LibOpenSteuer_Lst_Eingabe_Set_VMT(Lst_Eingabe *E, int V);
extern void LibOpenSteuer_Lst_Eingabe_Set_WFUNDF(Lst_Eingabe *E, int V);
extern void LibOpenSteuer_Lst_Eingabe_Set_RE4(Lst_Eingabe *E, int V);
extern void LibOpenSteuer_Lst_Eingabe_Set_KRV(Lst_Eingabe *E, int V);
extern void LibOpenSteuer_Lst_Eingabe_Set_LZZ(Lst_Eingabe *E, int V);
extern void LibOpenSteuer_Lst_Eingabe_Set_STKL(Lst_Eingabe *E, int V);

extern Lst_Ausgabe *LibOpenSteuer_Lst_Ausgabe_New();
extern int LibOpenSteuer_Lst_Ausgabe_Get_LSTLZZ(const Lst_Ausgabe *Arg);
extern int LibOpenSteuer_Lst_Ausgabe_Get_SOLZLZZ(const Lst_Ausgabe *Arg);
extern int LibOpenSteuer_Lst_Ausgabe_Get_K8(const Lst_Ausgabe *Arg);
extern int LibOpenSteuer_Lst_Ausgabe_Get_K9(const Lst_Ausgabe *Arg);
