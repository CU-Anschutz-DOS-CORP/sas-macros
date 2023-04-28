/**
 @file
 @brief Changes specified value to missing
 
 @details 
 @par Purpose
 This macro will change the specified value to missing for 1 or 
 more columns.
 
 @author Rocio Lopez

 @date 02-21-2018
 
 @version SAS 9.2 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %setmissing(
     vlist= 
     ,missingcode=
 );
 @endcode

 @returns
 SAS data set.
 
 @param vList Names one or more variable names to be processed.

 @param missingCode The value that will be replaced by missing.

 @note 
 @parblock
 @li This macro will only run from within a data step.
 @li If you include a variable that does not exist in VLIST, the 
 macro will proceed without warning. If the reason is you 
 mispelled a name then that variable will not be set to missing. 
 Please be sure to run a check after the macro execution.
 @endparblock

 @par Example(s)
 @code{.sas}
 data test;
     input a b c;
     datalines;
     1 6 11
     2 7 12
     99 8 13
     4 9 99
     5 999 15
     ;
 run;
 data test2;
     set test;
     %setmissing(vlist=a c, missingcode=99);
     %setmissing(vlist=b, missingcode=999);
 run;
 proc print data = test2;
 run;
 @endcode
 
 @par Revision History
 n/a
**/

%macro setmissing(vlist=, missingcode= );
	
	array _m&MISSINGCODE &VLIST;
	do over _m&MISSINGCODE;
		if _m&MISSINGCODE=&MISSINGCODE then _m&MISSINGCODE =.;
	end;

%mend setmissing;