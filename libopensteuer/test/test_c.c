#include <stdio.h>
#include <stdlib.h>
#include <libopensteuer.h>

/*****************************************************************
 * Simple program to demonstrate the use of libopensteuer with C *
 *****************************************************************/

int main(void)
{
    long r, lstlzz, error_code;
    int stkl, year;

    printf("\n/*************************");
    printf("\n * This is the C-program *");
    printf("\n *************************/\n");
    printf("\nLohnsteuer (german wage tax) (all amounts in Euro):\n");
    /*
     * Error-checking by return values is provided for every function.
     * Make sure, you always check them!  The library will not crash if
     * you set illegal values, you will get results but they will be wrong.
     * This is a feature not a bug.  Doing it this way, you can choose,
     * how to continue and what to do next.
     */
    for (year = get_first_year(); year <= get_last_year(); ++year) {
	printf("\nYear: %d\n", year);
	printf
	    ("     RE4       I      II     III      IV       V      VI\n");
	printf
	    ("--------------------------------------------------------\n");
	for (r = 2; r <= 24; ++r) {
	    printf("%8ld", r * 2500);
	    for (stkl = 1; stkl <= 6; ++stkl) {
		error_code = reset_all();
		if (error_code == no_error_code)
		    error_code = set_year(year);
		if (error_code == no_error_code)
		    error_code = set_lzz(1);
		/*
		 * The library expects all amounts in Euro-Cent.
		 * So we multiply by 100.
		 */
		if (error_code == no_error_code)
		    error_code = set_re4(r * 250000);
		if (error_code == no_error_code)
		    error_code = set_stkl(stkl);
		/*
		 * After all parameters are set we call the function
		 * which calculates the wage tax (Lohnsteuer)
		 */
		if (error_code == no_error_code)
		    error_code = calc_lst();
		/*
		   * As above: the library returns Euro-Cents:
		 */
		if (error_code == no_error_code)
		    lstlzz = get_lstlzz() / 100;
		if (error_code == no_error_code)
		    printf("%8ld", lstlzz);
		else
		    printf("   %s", "error");
	    }
	    printf("\n");
	};
	if (error_code != no_error_code) {
	    if (error_code == program_error_code)
		printf("Error: Program error.\n");
	    else if (error_code == tasking_error_code)
		printf("Error: Tasking error.\n");
	    else if (error_code == storage_error_code)
		printf("Error: Storage error.\n");
	    else if (error_code == constraint_error_code)
		printf
		    ("Error: Constraint Error (probably value out of range).\n");
	    else if (error_code == parameter_not_defined_error_code)
		printf("Error: Parameter not defined.\n");
	    else if (error_code == general_error_code)
		printf("Error: General error.\n");
	    else
		/*
		 * Should not be reached, all unknown errors should be caught
		 * by general_error.  If you get the following message, send
		 * me a bug-report.
		 */
		printf("Unknown error.\n");
	    error_code = no_error_code;
	}
    }
    /*
     * Now an example for the income tax.  Like above, you should always
     * check the return values.  I spare it here to keep the example short.
     */
    printf
	("\nEinkommensteuer (german income tax) (all amounts in Euro):\n");
    for (year = get_first_year(); year <= get_last_year(); ++year) {
	printf("\nYear: %d\n", year);
	printf("     ZVE        Grundtabelle    Splittingtabelle\n");
	printf("------------------------------------------------\n");
	for (r = 1; r <= 10; ++r) {
	    printf("%8ld", r * 10000);
	    reset_all();
	    set_year(year);
	    set_lzz(1);
	    set_zve(r * 1000000);
	    calc_est();
	    printf("%20ld", get_grundtab() / 100);
	    printf("%20ld\n", get_splittab() / 100);
	};
    }
    return 0;
}
