
/**
 @file
 @brief Label and format a single variable
 
 @details 
 @par Purpose
 This macro runs within a DATA step and will apply specified format and label
 to a variable.
 
 @author Rocio Lopez

 @date 11-21-2023
 
 @version SAS 9.4 or later
 
 @par Usage
 A call to this macro has all or some of these parameters:
 @code{.sas}
 %fmtLab(var, fmt, lab);
 @endcode
 
 @param var Variable to which apply format and label

 @param fmt Format to be applied

 @param label Label to be applied

 @note 
 @parblock
 @li This macro will only run from within a data step.

 @par Example(s)
 @code{.sas}

 data myData;
	trtDate = "21NOV2023"d;
	%fmtLab(trtDate, mmddyy10., "Date of treatment");
 run;

 proc print data=myData label;
 run; title;

 @endcode
 
 @par Revision History
 n/a
**/

%macro fmtLab(var, fmt, lab);

	format &var &fmt;
	label &var = &lab;

%mend fmtLab;