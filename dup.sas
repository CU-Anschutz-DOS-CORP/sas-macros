/**
 @file
 @brief Checks for duplicates
 
 @details 
 @par Purpose
 Checks for the number of duplicate records in a given file.
 Adds in potentially two new variables for total count and which one the record is.
 
 @author Ed Mascha, Cleveland Clinic
 
 @par Maintenance
 Rocio Lopez

 @date Unknown
 
 @version SAS 9.2 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %dup(
      mainData = 
	  ,byVar = 
	  ,othervar = 
	  ,xtraSort = 
	  ,countMax = 1
	  ,sort = 1
	  ,mergeAll = 0
	  ,totlName = nPerID
	  ,ithName = nWithnID
	  ,print = 0
	  );
 @endcode

 @returns
 @parblock 
 The LISTING will show:
 @li Number of duplicates on file
 @li Min, max and a frequency table of observations per byVar

 The macro creates the following datasets:
 @li __DUPWRK - Copy of mainData excludeing all exact duplicate rows. N per Id 
 and N within columns are added.
 @li __DUPCNT - Has byVar and  N per Id
 @li __FIRST - Includes the first row for each byVar
 @endparblock
 
 @note 
 @parblock
 @li The macro checks only consecutive obs, non-consecutive duplicate obs will 
 remain in the output data set
 
 @li You can remove all duplicates with this macro by sorting on all variables 
 using the XTRASORT option
 @endparblock 
 
 @param mainData The dataset you wish to check out
 
 @param byVar The variable(s) by which you want to check for dups, usually an 
 ID variable

 @param xtraSort Extra Variables to sort by
 @n Optional, Default is None
 
 @param otherVar List of other variables you want in the print out of the data 
 @n Optional, Default is None
 
 @param countMax The number of records you expect for each variable
 @n Will print a listing of vars not meeting this criteria
 @n Optional, Default is 1
 
 @param sort A logical for whether the data should be sorted or not
 @n Valid options are: 1=Sort the data, 0=Data already sorted 
 @n Optional, Default is 1
 
 @param mergeAll Merge the total count variable back with the original data. 
 @n Valid options are: 1=Merge, 0=Dont Merge 
 @n Optional, Default is 0

 @param totlName Name for the new variable for total count when mergeAll=1 
 @n Optional, Default is NperID
 
 @param ithName Name for the new variable for the rank count when mergeAll=1 
 @n Optional, Default is NwithnID
 
 @param print Allow/Supress the proc print of the data
 @n Valid options are: 0=Do not print, 1=Print the data 
 @n Optional, Default is 0
  
 @par Example(s)
 @code{.sas}
 **Example 1;
  %dup(
       mainData = sashelp.comet
      ,byVar = Sample
      ,xtraSort = _all_
      );
 @endcode
 
 @par Revision History
 @showdate "%m-%d-%Y" 2022-9-16 Rocio modifed documentation for Doxygen use.
 @n @showdate "%m-%d-%Y" 2018-6-13 Rocio added notes for users.
 @n @showdate "%m-%d-%Y" 2004-4-20 Matt made modifications to Ed's macro.
**/

%macro dup(
           maindata = ,
           byvar    = ,
           othervar = , 
           XtraSort = , 
           countmax = 1,
           sort     = 1, 
           mergeall = 0, 
           TotlName = NperID, 
           IthName  = NwithnID, 
           Print    = 0
          );

    %let __first=%scan(&byvar,1);

    %let __p=0;
    %do %while(%scan(&byvar,&__p + 1) ^=    ); 
        %let __p=%sysevalf(&__p + 1); 
    %end;
    %let __last=%scan(&byvar,&__p);

    data __DupWrk;
        set &maindata;        
    run;

    
    %if &sort=1 %then %do;
        proc sort nodup data=__DupWrk;  
            by &byvar. &XtraSort.; 
        run;

        proc sql noprint;
            select count(*) into:__DupwoExact
               from __DupWrk;
            select count(*) into:__Totals
               from &maindata;
            quit;
        run;
        
        %let __ExactDupsRemoved = %sysevalf(&__Totals - &__DupwoExact);
        
        data _null_;
            file print;
            %if &__ExactDupsRemoved gt 0 %then 
                Put "&__ExactDupsRemoved exact duplicates will be deleted.";
            %if &__ExactDupsRemoved eq 0 %then 
                Put "No exact duplicates to be deleted.";
            ;
        run;    
    %end;

    data __DupWrk;
        set __DupWrk;
        by &byvar.; 
        retain __NwthIn;
        if first.&__Last. then __NwthIn = 0;
        __NwthIn =  __NwthIn + 1;
        output;
    run;

    data __DupCnt;
        set __DupWrk;
        by &byvar.; 
        if Last.&__Last. then do;
            __nperid = __NwthIn;
            output;
        end;
        keep  &byvar. __nperid;
    run;
    
    data __DupWrk; 
        merge __DupWrk __DupCnt; 
        by &byvar.; 
        Label __NwthIn="Number Within"
              __NperID="N per ID"
        ;
    run;

    %if &Print = 1 %then 
        %do;
           proc print data=__DupWrk label;
               title1 "Dataset &maindata: IDS WITH other than &countmax records per (&byvar)";
               var &byvar __NwthIn __nperid &othervar;
               where __nperid NE &countmax;
           run;
        %end;

    title1 "&maindata.: Count number of &byvar.s";
    proc means data=__DupCnt N MIN MAX; 
        var __nperid; 
    run;

    data __first; 
        set __DupWrk;  
        by &byvar.; 
        if first.&__last;
    run;
    
    title1 "&maindata.: Distribution of Records per ID (&byvar)";
    proc freq data=__first; 
        table __nperid;
    run;

    %if &mergeall eq 1 %then
        %do;
           data &maindata;
               set __DupWrk (rename=(__nperid = &TotlName.
                                     __NwthIn = &IthName.));
           run;
        %end;
    title1;     
%mend;
