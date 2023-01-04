/**
 @file
 @brief Creates dummy variables
 
 @details 
 @par Purpose
 This macro creates dummy variables for one or more numeric and/or character
 variables

 @author Rocio Lopez

 @date 11-20-2020
 
 @version SAS 9.4
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %dummyVars(
	 data = 
	 ,varList = 
	 ,outData = 
	 ,debug = 0
	 ) ;
 @endcode

 @returns
 A SAS data set with all variables in original data set + dummy variables added.
 The results window will show a list of names for the new dummy variables and
 frequency tables showing original vs dummy variables.
 
 @note 
 Variable names in VARLIST must have a length of 30 or less; this is because 
 otherwise GLMSELECT uses the original name as the 1st dummy variable and the 
 original column gets overwritten
 
 @param data Name of sas dataset that contains variables in VARLIST
 
 @param varList List of variables for which to create dummy variables; can 
 be numeric and/or character
 
 @param outData Name for the output dataset that contains the dummy variables
 @n If no value provided, defaults to YourDBName_DummyVars
 
 @param debug Turns on/off several internal logging techniques
 @n Allowed options are:
 @n 0 for off (DEFAULT)
 @n 1 for on
  
 @par Example(s)
 @code{.sas}
 %dummyvars(
	 data = sashelp.cars
	,varlist = Origin DriveTrain Type
	);
 @endcode
 
 @par Revision History
 none
**/

%macro dummyVars(
    data = , 
    varlist = , 
    outdata = ,
    debug = 0) ;

%put ************************************************************;
%put NOTE:  Macro &sysmacroname is starting to run. ;
%put ************************************************************;

%put ************************************************************;
%put ************************************************************;
%put *                0.0:SET UP                                *;
%put ************************************************************;
%put ************************************************************;

options validvarname = v7 ; /* ensure no spaces in SAS Studio dummy var names */
%if &debug %then %do ;
    options symbolgen mlogic mprint mlogicnest ; 
%end ;

%local errorflag _nvars _dbname newlist icount cvar nextvar nmvalues dsid rc;

proc datasets lib=work memtype = (data view) nodetails nolist nowarn ;
    delete __AddFakeY __NewVars: __names: ;
run ;
quit ;

%put ************************************************************;
%put ************************************************************;
%put *                0.1:ERROR CHECKING                        *;
%put ************************************************************;
%put ************************************************************;

%let errorflag = 0 ;

%put *************************************************;
%put * CHECKING DATA PARAMETER                       *;
%put *************************************************;
%if not %length(&data) %then %do ;
    %put ERROR: Required parameter DATA not provided. Macro will stop executing. ;
    %let errorflag = 1 ;
%end ; 

%if %length(&data) and not %sysfunc(exist(&data)) %then %do ;
    %PUT ERROR: Data set %upcase(&data) does not exist. Macro will stop executing. ;
    %let errorflag = 1 ;
%end ; 

%put *************************************************;
%put * CHECKING VARLIST PARAMETER                    *;
%put *************************************************;
%if %length(&data) and %sysfunc(exist(&data)) and %length(&varlist) %then %do ;
 
    %let newlist = ;
    %let icount = 1 ;
    %let cvar = %scan(&varlist, &icount) ;
    %let nextvar = &cvar ;
    %let nmvalues = 0;
    
    %let dsid = %sysfunc(open(&data)) ;
    %if %length(&varlist) %then %do %while(&nextvar ne ) ;
        
        %let cvar = %upcase(&nextvar) ;
        %let icount = %eval(&icount + 1) ;
        %let nextvar = %scan(&varlist, &icount) ;
        
        %let exist = %sysfunc(varnum(&dsid, &cvar)) ;
        %if &exist %then %do ;
            proc sql noprint ;
                select count(&cvar) into :nmvalues
                from &data ;
            quit ;
        %end ;
        
        %if %length(&cvar) ge 31 %then %do;
            %put ERROR: The variable named %upcase(&cvar) contains more than 30 bytes. Macro will stop executing. ;
            %let errorflag = 1 ;
        %end;
        
        %if &exist and &nmvalues %then %let newlist = &newlist &cvar ;
        %else %do ;
            %let newlist = &newlist ;
            %if not &exist %then
                %put WARNING: %upcase(&cvar) in VARLIST list does not exist in data set %upcase(&data) and will be dropped.;      
            %else %if not &nmvalues %then
                %put WARNING: %upcase(&cvar) in VARLIST is completely missing and will be dropped. ;    
        %end ;
    
    %end ; ** end to if length(var) > 0 then do while ;
    %let rc = %sysfunc(close(&dsid)) ;
    
    %LET varlist = &newlist ;

%end ; **end to if length(data) gt 0 and sysfunc(exist(&data)) = 1 ;

%let _NVars = 0 ;
%do %while(%scan(&VARLIST, &_NVars + 1) ^=    ) ; 
    %let _NVars = %eval(&_NVars + 1) ; 
%end;

%if not &_NVars %then %do;
    %put ERROR: Required parameter VARLIST not provided or variables given do not exist/are 100% missing. Macro will stop executing. ;
    %let errorflag = 1 ;
%end; 

%if &errorflag %then %do ;
    %put *************************************************;
    %put * ABORT EXECUTION BECAUSE OF ERROR              *;
    %put *************************************************;
    data _null_ ;
        abort 3 ;
    run ;
%end ;

%put *************************************************;
%put * PROCEEDING WITH MACRO, NO ERRORS FOUND        *;
%put *************************************************;

%if not %length(&outdata) %then %do;
    %if not %sysfunc(findc(".", &data)) %then %let outdata = &data._DummyVars ;    
    %else %do;
        %let _dbname =  %scan(&data, 2, ".") ;
        %let outdata = &_dbname._DummyVars ;
    %end;        
%end;

%if %sysfunc(exist(&outdata)) %then %do ;
    %PUT NOTE: Data set %upcase(&outdata) already exists and will be overwritten. ;
%end ; 

%put ************************************************************;
%put ************************************************************;
%put *                1:ADD FAKE RESPONSE VARIABLE              *;
%put ************************************************************;
%put ************************************************************;
data __AddFakeY / view = __AddFakeY ;
    set &data ;
    __Y = 0 ;
run ;

%put ************************************************************;
%put ************************************************************;
%put *                2:CREATE DUMMY VARIABLES                  *;
%put ************************************************************;
%put ************************************************************;

%if &debug %then %do ;
    ods select ClassLevelInfo(persist) ; 
%end ;
%if not &debug %then %do ; 
    ods select none; 
%end ;

%do i = 1 %to &_NVars ;

    %let ThisVar = %scan(&varlist, &i) ;

    proc glmselect 
      data = %if &i = 1 %then %do ; __AddFakeY %end ; %else %do ; &outdata %end;
      outdesign(addinputvars names) = &outdata %if &i = &_NVars %then %do ; (drop = __Y) %end ;
      namelen=200 ;
       class      &thisvar ;   
       model __Y = &thisvar /  noint selection=none ;
       ods output ParameterNames = __names&i ; 
    run ;

%end;

data __NewVars1;
    informat Name Parameter $32767. ;
    set __names1-__names&_NVars ;
run;    

ods select all;

%put ************************************************************;
%put ************************************************************;
%put *                3:CHECK DUMMY VARIABLES                   *;
%put ************************************************************;
%put ************************************************************;

%put *************************************************;
%put * LIST NAMES FOR NEWLY CREATED DUMMY VARIABLES  *;
%put *************************************************;
title "Names of newly creted dummy variables" ;
proc sql ;
    select Name label = "Dummy Variables"
    from __NewVars1 ;
quit ;    
title ;

%put *************************************************;
%put * LISTING OF NEWLY CREATED DUMMY VARIABLES      *;
%put *************************************************;
data __NewVars2 (keep = DummyVar Variable) / view = __NewVars2 ;
    set __NewVars1 (rename = (Name = DummyVar)) ;
    informat Variable $35.;
    Variable = trim(scan(Parameter, 1, " ")) ;
run; 

proc transpose data = __NewVars2 out = __NewVars3 (drop = _Name_ ) ; 
    by Variable notsorted ;
    var DummyVar ;
run ;

data __NewVars4 / view = __NewVars4 ;
    set __NewVars3 ;
    informat _list $32767. ;
    array col[*] col: ;
    _List = trim(Variable)||"*"||trim(Col1) ;
    do i = 2 to dim(col) ;
        if not missing(col[i]) then
          _list = trim(_List)||"*"||trim(col[i]) ;
    end;
run;    

proc sql noprint ;
    select _list 
    into :_list1-:_list&_NVars 
    from __NewVars4 ;
quit ;

title "Listing of newly created dummy variables" ;
ods noproctitle ;
proc freq data =  &outdata ;
    tables 
      %do i = 1 %to &_NVars ; &&_list&i %end ;
      / list missing nocum nopercent ;
run ;
ods proctitle ;
title ;

%put ************************************************************;
%put ************************************************************;
%put *                4.CLEAN UP                                *;
%put ************************************************************;
%put ************************************************************;

%if &debug %then %do ;
    options nosymbolgen nomlogic nomprint nomlogicnest ; 
%end ;

%if not &debug %then %do ;
    proc datasets lib=work memtype = (data view) nodetails nolist nowarn ;
        delete __AddFakeY __NewVars: __names: ;
    run ;
    quit ;
%end ;
ods select all ;

%put ************************************************************;
%put NOTE:  Macro &sysmacroname has finished. ;
%put ************************************************************;

%mend dummyVars;