/* error-codes */

extern const int no_error_code;
extern const int general_error_code;
extern const int program_error_code;
extern const int constraint_error_code;
extern const int not_defined_error_code;

/* misc */

extern void adainit (void);
extern void adafinal (void);

/* jahr */

extern int get_first_year (void);
extern int get_last_year (void);
extern int set_year (int year);

/* eingangsparameter */

extern int set_alter1 (int alter1);
extern int set_hinzur (long hinzur);
extern int set_jfreib (long jfreib);
extern int set_jhinzu (long jhinzu);
extern int set_jre4 (long jre4);
extern int set_jvbez (long jvbez);
extern int set_krv (int krv);
extern int set_lzz (int lzz);
extern int set_r (int r);
extern int set_re4 (long re4);
extern int set_sonstb (long sonstb);
extern int set_stkl (int stkl);
extern int set_vbez (long vbez);
extern int set_vbs (long vbs);
extern int set_vmt (long vmt);
extern int set_wfundf (long wfundf);
extern int set_zkf (int zkf);
extern int set_zve (long zve);

/* ausgangsparameter */

extern long get_bk (void);
extern long get_bks (void);
extern long get_bkv (void);
extern long get_lstlzz (void);
extern long get_lzalog (void);
extern long get_lzalug (void);
extern long get_solzlzz (void);
extern long get_solzs (void);
extern long get_solzv (void);
extern long get_sts (void);
extern long get_stv (void);

/* einkommensteuer */

extern long get_grundtab (void);
extern long get_splittab (void);
extern int calc_est (void);

/* lohnsteuer */

extern int calc_lst (void);

/* reset */

extern int reset_all (void);
