/* Exceptions and Error-Codes */

int const no_error_code = 0;
int const general_error_code = -1;
int const program_error_code = -2;
int const tasking_error_code = -3;
int const storage_error_code = -4;
int const constraint_error_code = -5;
int const parameter_not_defined_error_code = -20;
 
/* Set the year */
 
int get_first_year (void);
int get_last_year (void);
int set_year (int year);
 
/* Set PAP-parameters */

int set_alter1 (int alter1);
int set_hinzur (long hinzur);
int set_jfreib (long jfreib);
int set_jhinzu (long jhinzu);
int set_jre4 (long jre4);
int set_jvbez (long jvbez);
int set_krv (int krv);
int set_lzz (int lzz);
int set_r (int r);
int set_re4 (long re4);
int set_sonstb (long sonstb);
int set_stkl (int stkl);
int set_vbez (long vbez);
int set_vbs (long vbs);
int set_vmt (long vmt);
int set_wfundf (long wfundf);
int set_zkf (int zkf);
int set_zve (long zve);
 
/* Get PAP-parameters */
 
long get_bk (void);
long get_bks (void);
long get_bkv (void);
long get_lstlzz (void);
long get_lzalog (void);
long get_lzalug (void);
long get_solzlzz (void);
long get_solzs (void);
long get_solzv (void);
long get_sts (void);
long get_stv (void);
 
/* Calculate Einkommensteuer */
 
long get_grundtab (void);
long get_splittab (void);
int calc_est (void);
 
/* Calculate Lohnsteuer */
 
int calc_lst (void);
 
/* Reset-functions */
 
int reset_all (void);
 
/* Misc */
 
long get_min_amount (void);
long get_max_amount (void);
