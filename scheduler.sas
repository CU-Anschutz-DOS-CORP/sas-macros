/**
 @file
 @brief Schedules a SAS job to run at a later time
 
 @details 
 @par Purpose
 This macro schedules the remainder of a SAS job to run at a later time. 
 It was developed to be used in SAS Studio in order to delay the running of a 
 background job submission.

 @author Rocio Lopez

 @date 03/26/2020
 
 @version SAS 9.4 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %scheduler(when = );
 @endcode

 @param when Datetime (ddmmmyyy:hr:mm) value specifying when you want the 
 job to run
 @n If no value provided then job will proceed immediately

 @par Example(s)
 @code{.sas}
 **Example 1: Schedule at a specific time/day;
  %scheduler(when=26mar2020:15:00);
    
 **Example 2: Use today/tomorrow for more flexibility;
 data _null_;
	 call symput('Today', left(put("&SYSDATE9.", date9.))); 
	 call symput('Tommorow', left(put(intnx('day', "&SYSDATE9."d, 1), date9.)));     
 run;
 
 %scheduler(when=&today:23:00);    **today at 11PM;
 %scheduler(when=&tommorow:05:00); **tomorrow at 5AM;
 @endcode
 
 @par Revision History
 none
**/

%macro scheduler(when=) ;
       
    data _null_ ;        
        %if %length(&when) gt 0 %then %do ;
            sleeptime = "&when"dt-datetime() ;
        %end ;
        %else %do ;
            sleeptime = 0 ;
        %end ;
        call sleep(sleeptime, 1) ;
    run ;
    
    data _null_ ;
        call symput('now', put(datetime(), datetime.)) ;
    run ; 

    %put ************************************************************;
    %put *** Submitted on &SYSDATE9 at &SYSTIME ;
    %put *** Running on &now ;
    %put ************************************************************;
    
%mend scheduler ;