/**
 @file
 @brief Generates a missing values report
 
 @details 
 @par Purpose
 This macro generates a report of missing data, overall or by group. It can 
 handle numeric and character data and identifies all valid SAS missing values.

 @author Rocio Lopez

 @date 12-06-2018
 
 @version SAS 9.2 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %missing(
	 data = 
	,byVar = 
	,vList = _all_
	,highLightVal = 30
	,missPattern = 0
	,printAll = 0
	,sortBy = descending _nmiss
	,reverseCol = 0
	,out = _miss_
	,rtffile = 
	,page = portrait
	,xlsxfile = 
	,debug = 0);
 @endcode

 @returns
 A table displaying % of all observations that are missing each particular
 variable
 
 @param data The dataset you wish to check out

 @param byVar Optional group variable by which to report missing values

 @param list List of variables to check for missing values
 @n Default is _ALL_

 @param missPattern Allows to request a description of missing pattern
 @n Allowed options are:
 @n 0 = No, do not incude (DEFAULT)
 @n 1 = Yes, include

 @param highLightVal This is a single numeric value (0-100) that specifies which 
 % missing values to highlight; anything with % missing greater than or 
 equal to HIGHLIGHTVAL will appear in red bolded font
 @n Default is 30

 @param printAll Request a report including all variables in LIST
 @n Allowed options are:
 @n 0 = No, only include those with >0% missing (DEFAULT)
 @n 1 = Yes, include all variables

 @param sortBy Allows sorting of OUT data set generated for report
 @n Default is DESCENDING _NMISS
 
 @param reverseCol Reverse order of display for group variable (byVar)
 @n Allowed options are:
 @n 0 = No, display in ascending order (DEFAULT)
 @n 1 = Yes, reverse and display in descending order
 
 @param out Name of output dataset
 @n Default is _missing_
 
 @param rtfFile Location and name for output RTF file sourrounded by double quotes
 (e.g. "~userid/consult/missReport.rtf")

 @param page Page orientation of RTF file
 @n Allowed options are:
 @n PORTRAIT (DEFAULT)
 @n LANDSCAPE
 
 @param xlsxFile Location and name for output RTF file sourrounded by double quotes
 (e.g. "~userid/consult/missReport.xlsx")

 @param debug Debugging option
 @n Allowed options are:
 @n 0 = Off (DEFAULT)
 @n 1 = On
   
 @par Example(s)
 @code{.sas}
 data cars;
    set sashelp.cars;
    if ranuni(987) le .5 then call missing(msrp); 
    if ranuni(987) le .2 then call missing(mpg_city, mpg_highway);
    if ranuni(987) le .15 then call missing(drivetrain); 
    if ranuni(987) le .05 then call missing(origin, weight);
    label DriveTrain = "Drive Train"
          MPG_City = "City MPG"
          MPG_Highway = "Highway MPG";
 run;

 %missing(data=cars);
 @endcode
 
 @par Revision History
 @b 01-22-2022 Added highLightVal and printAll options
 @n @b 01-31-2023 1) Corrected labels for _PctMissi columns. 2) Added auto-filter and
 freeze panes to excel output. 3) Aded REVERSECOL option to control order of BYVAR 
 columns.
**/

%macro missing(
    data = ,
    byvar = ,
    vlist = _all_,
    highlightval = 30,
    misspattern = 0,
    printall = 0,
    sortby = descending _nmiss, /* other options would be varname, _nmiss, _n descending allowed*/
    reverseCol = 0,
    out = _missing_,
    rtffile = ,
    xlsxfile = ,
    page = portrait,
    debug = 0);

%*****************************************************************************;
%* DELETE DBS FROM WORK LIB                                                   ;
%*****************************************************************************;
proc datasets lib=work nodetails nowarn nolist;
    delete _parsevs_ _tempd_ _ngroups_ _vtype_ _pattern_ &out;
run; quit;

%*****************************************************************************;
%* FORMATS                                                                    ;
%*****************************************************************************;
proc format;
     picture __Pctf (round)
              low    -  0     = "9"
                 0  <-< 0.01  = [e8.2]
                0.01 -< 1     = "9.99"
                1    -  high    = "00000000000009.9";
     picture __Countf (round)
             low-999   = "009"
             1000-high = "0,000,000,000,000,009";
     value $__missf
        '.', 'O' = 'Missing'
        'X'      = 'Non-missing';
run;
quit;

%*****************************************************************************;
%* ERROR CHECKING                                                             ;
%*****************************************************************************;
%put **********************************************;
%put ******          Error Checking          ******;
%put **********************************************;

%let errorflag = 0;
options minoperator;

%* Check debug option and turn on macro checking options;
%if not(%upcase(&debug) in (0 1)) %then %do;
    %put WARNING: DEBUG must be either 0 or 1.;
    %put WARNING: DEBUG = **&debug**;
    %put WARNING: Default value of 0 will be used for DEGUG;
    %let degug=0;
%end; %*end to debug check;

%if &debug eq 1 %then %do;
    options mlogic mlogicnest symbolgen varinitchk=warn mergenoby=warn;
%end; %* end to debug eq 1;

%* Check DATA is given and exists;
%if %length(&data) eq 0 %then %do;
    %PUT ERROR: Required parameter DATA not provided. Macro will stop executing;
    %LET ERRORFLAG=1;
%end; %*end to if length(data) =  0;

%if %length(&data) gt 0 and %SYSFUNC(exist(&data)) ne 1 %then %do;
    %PUT ERROR: Data set &data does not exist. Macro will stop executing;
    %LET ERRORFLAG=1;
%end; %*end to if length(data) ne  0;

%* If BYVAR given, check it exists;
%if %length(&data) gt 0 and %sysfunc(exist(&data)) eq 1 and %length(&byvar) gt 0 %then %do;

    ** Check if VAR exists ;
    %let dsid  = %sysfunc(open(&data));
    %let exist = %sysfunc(varnum(&DSID, &byvar));
    %let rc    = %sysfunc(close(&DSID));

    ** variable must exist;
    %if &EXIST=0 %then %do;
        %put ERROR: Variable %upcase(&byvar) does not exist in dataset %upcase(&data). Macro will stop executing.;
        %let ERRORFLAG=1;
    %end; **end to if Exist=0 then do;

%end; ** end to byvar exist check;

%* Parse VLIST to allow for x1-x99 or _all_ naming;
proc contents noprint
    data = &data (obs=0 keep = &vlist)
    out  = _parsevs_ (keep = name);
run;

proc sql noprint;
    select name into :LongVlist separated by ' '
    from _parsevs_;
quit;

%* If VLIST given, check each exists;
%if %length(&data) gt 0 and %sysfunc(exist(&data)) eq 1 and %length(&vlist) gt 0 %then %do;

    %let newlist = ;
    %let icount = 1;
    %let cvar = %scan(&longvlist, &icount);
    %let nextvar = &cvar;

    %let dsid = %sysfunc(open(&data));
    %do %while(&nextvar ne );

        %let cvar    = %upcase(&nextvar);
        %let icount  = %eval(&icount + 1);
        %let nextvar = %scan(&longvlist, &icount);

        %let exist = %sysfunc(varnum(&dsid, &cvar));

        %if &exist gt 0 %then %let newlist = &NEWLIST &CVAR;
        %else %if &exist eq 0 %then %do;
            %let newlist = &newlist;
            %put WARNING: %upcase(&cvar) in VLIST list does not exist in data set %upcase(&data) and will be dropped.;
        %end;

    %end; ** end to if length(VLIST) > 0 then do while;
    %let rc = %sysfunc(close(&dsid));

    %LET vlist  = &newlist;

%end; **end to vlist exist;

*% If no vars left on VLIST then abort;
%if %length(&vlist) eq 0 %then %do;
    %put ERROR: VLIST is now empty. At least one existing variable must be specified. Macro will stop excecuting.;
    %let ErrorFlag = 1;
%end;

%* Check MISSPATTERN valid options;
%if not(%upcase(&misspattern) in (0 1)) %then %do;
    %put WARNING: MISSPATTERN must be either 0 or 1.;
    %put WARNING: MISSPATTERN = **&misspattern**;
    %put WARNING: Default value of 0 will be used for MISSPATTERN;
    %let misspattern=0;
%end; %*end to misspattern check check;

%* Check PRINTALL valid options;
%if not(%upcase(&printall) in (0 1)) %then %do;
    %put WARNING: PRINTALL must be either 0 or 1.;
    %put WARNING: PRINTALL = **&printall**;
    %put WARNING: Default value of 0 will be used for PRINTALL;
    %let printall=0;
%end; %*end to printall check check;

%* Check SORTBY options;

%* Check REVERSECOL valid options;
%if not(%upcase(&reversecol) in (0 1)) %then %do;
    %put WARNING: REVERSECOL must be either 0 or 1.;
    %put WARNING: REVERSECOL = **&reversecol**;
    %put WARNING: Default value of 0 will be used for REVERSECOL;
    %let reversecol=0;
%end; %*end to misspattern check check;

%* Check PAGE options;
%if not(%upcase(&page)  in (PORTRAIT LANDSCAPE)) %then %do;
    %put WARNING: PAGE must be either PORTRAIT or LANDSCAPE.;
    %put WARNING: PAGE = **&page**;
    %put WARNING: Default value of PORTRAIT will be used for PAGE.;
    %let page=portrait;
%end; %*end to page check;

%* Check rtffile;
%if %length(&rtffile) gt 0 and %sysevalf(%sysfunc(indexc(&rtffile, '"')) ne 1) %then %do;
    %put
    ERROR: Use of double quotes is required for RTFFILE. Macro will stop executing.;
    %let ErrorFlag=1;
%end;

%* Check xlsxfile;
%if %length(&xlsxfile) gt 0 and %sysevalf(%sysfunc(indexc(&xlsxfile, '"')) ne 1) %then %do;
    %put
    ERROR: Use of double quotes is required for XLSXFILE. Macro will stop executing.;
    %let ErrorFlag=1;
%end;

%*****************************************************************************;
%* IF ANY ERRORS FOUND THEN ABORT EXECUTION, OTHERWISE CONTINUE               ;
%*****************************************************************************;
%if &errorflag eq 1 %then %do;
    data _null_;
        abort 3;
    run;
%end;

%*****************************************************************************;
%* SET UP                                                                     ;
%*****************************************************************************;

%put **********************************************;
%put ******    Working DB and __class var    ******;
%put **********************************************;
%*number observations and group var type;
%let dsid = %sysfunc(open(&data));
    %let nobs  = %sysfunc(attrn(&dsid, nlobs));

    %if %length(&byvar) gt 0 %then %do;
        %let gnum  = %sysfunc(varnum(&dsid, &byvar));
        %let gtype = %sysfunc(vartype(&dsid, &gnum));
        %let gfmt  = %sysfunc(varfmt(&dsid, &gnum));
    %end;

%let rc = %sysfunc(close(&dsid));

%if %length(&byvar) gt 0 %then %do;
    %if &gtype eq N and %length(&gfmt) eq 0 %then %let gfmt = BEST.;
    %else %if &gtype eq C and %length(&gfmt) eq 0 %then %let gfmt = $CHAR.;
%end;

%*Create working data set;
data _tempd_;
    informat __class $256.;
    set &data;
    
    
    %* avoid the class variable name causing problems in ods;
    %* if no class specified then set to "Total" for ease of management;
    %if %length(&byvar) eq 0 %then %do;
        __class="Total";
    %end;

    %else %if %length(&byvar) gt 0 and &gtype eq N %then %do;
        __class=strip(put(&byvar, &gfmt.));
    %end;

    %else %do;
        __class = &byvar;
    %end;
    
    if missing(__class) then __class = "_Missing_";
run;

%put **********************************************;
%put ******      Getting Variable Types      ******;
%put **********************************************;

%* Get labels and var type;
proc contents noprint
    data = _tempd_   (keep = &vlist obs = 0)
    out  = _vtype_ (keep = Name Label Type);
run;

%* Get list of character and numeric vars and max length of labels;
proc sql noprint;
    select Name into :_num_ separated by ' '
    from _vtype_
    where Type=1
      %if %length(&byvar) gt 0 %then %do; and upcase(Name) ne %upcase("&byvar") %end; ;

    select Name into :_char_ separated by ' '
    from _vtype_
    where Type=2
      %if %length(&byvar) gt 0 %then %do; and upcase(Name) ne %upcase("&byvar") %end; ;
    
    select max(length(Label)) into :llen
    from _vtype_ ;
quit;

%put **********************************************;
%put ****** Getting Group Numbers and Labels ******;
%put **********************************************;
proc freq data = _tempd_ noprint;
    table __class /out=_ngroups_;
run;

proc sql noprint;
    
    %* num of groups;
    select count(distinct(__class)) format 8. into :NGroups
    from _ngroups_;

    %* groups labels;
    select strip(__class) into :GroupLab1-:GroupLab99
    from _ngroups_;

    %* N for each group;
    %do i=1 %to &NGroups;
        select Count into :GroupN&i
        from _ngroups_
        where left(__class) = "&&GroupLab&i";
    %end;
    
    %* Total N;
    select sum(Count) into :TotalN
    from _ngroups_;

quit;
%let NGroups=&NGroups; **remove leading/trailing blanks;


%*****************************************************************************;
%* OBTAIN N AND NMISS                                                         ;
%*****************************************************************************;

%put **********************************************;
%put ******     Getting N and N missing      ******;
%put **********************************************;
%* Get N and Nmiss (overall and by group);
data &out;
    informat VarName $32. VarLabel $&llen.. VarType $6.;

    set _tempd_ end = eof;

    %if %symexist(_num_) %then %do;  array n_vars[*] &_num_;  %end;
    %if %symexist(_char_) %then %do; array c_vars[*] &_char_; %end;

    %do i=1 %to &NGroups;
        %if %symexist(_num_) %then %do;
            array n_count&i[&nobs] _temporary_ (&nobs*0);
        %end;
        %if %symexist(_char_) %then %do;
            array c_count&i[&nobs] _temporary_ (&nobs*0);
        %end;
    %end;

    %if %symexist(_num_) %then %do;
        do i = 1 to dim(n_vars);
            %do i = 1 %to &NGroups;
                if missing(n_vars[i]) and __class = "&&GroupLab&i" then n_count&i[i] + 1;
            %end;
        end;
    %end;

    %if %symexist(_char_) %then %do;
        do i = 1 to dim(c_vars);
            %do i = 1 %to &NGroups;
                if missing(c_vars[i]) and __class = "&&GroupLab&i" then c_count&i[i] + 1;
            %end;
        end;
    %end;

    if eof then do;
        %if %symexist(_num_) %then %do;
           do i = 1 to dim(n_vars);
              VarName = vname(n_vars[i]);
              VarLabel = vlabel(n_vars[i]);
              if VarLabel = " " then VarLabel = vname(n_vars[i]);
              VarType = vtype(n_vars[i]);
              _N = .; _NMiss = .; _PctMiss = .; **place holders;
              %do i = 1 %to &NGroups;
                  _N&i = &&GroupN&i - n_count&i[i];
                  _NMiss&i = n_count&i[i];
                  _PctMiss&i = (_NMiss&i / (_N&i + _NMiss&i))*100;
              %end;
              _N = sum(of _N1-_N&NGroups);
              _NMiss = sum(of _NMiss1-_NMiss&NGroups);
              _PctMiss = (_NMiss / (_N + _NMiss))*100;
              output;
           end;
        %end;

        %if %symexist(_char_) %then %do;
           do i = 1 to dim(c_vars);
              VarName = vname(c_vars[i]);
              VarLabel = vlabel(c_vars[i]);
              if VarLabel = " " then VarLabel = vname(c_vars[i]);
              VarType = vtype(c_vars[i]);
              _N = .; _NMiss = .; _PctMiss = .; **place holders;
              %do i = 1 %to &NGroups;
                  _N&i = &&GroupN&i - c_count&i[i];
                  _NMiss&i = c_count&i[i];
                  _PctMiss&i = (_NMiss&i / (_N&i + _NMiss&i))*100;
              %end;
              _N = sum(of _N1-_N&NGroups);
              _NMiss = sum(of _NMiss1-_NMiss&NGroups);
              _PctMiss = (_NMiss / (_N + _NMiss))*100;
              output;
           end;
        %end;

    end; **end of if eof;

    keep VarName VarLabel VarType _N _NMiss _PctMiss
      %if &NGroups gt 1 %then %do i=1 %to &NGroups; _N&i _NMiss&i _PctMiss&i %end; ;

    format _PctMiss
      %if &NGroups gt 1 %then %do i=1 %to &NGroups; _PctMiss&i %end; __pctf.;

    label VarName  = "Name"
          VarLabel = "Label"
          VarType  = "Type"
          _N       = "Total: N obs"
          _NMiss   = "Total: N missing"
          _PctMiss = "Total: % missing / %left(%qsysfunc(putn(&TotalN, __countf.)))"
          %if &NGroups gt 1 %then %do i=1 %to &NGroups;
            _N&i     = "&&GroupLab&i: N obs"
            _NMiss&i = "&&GroupLab&i: N missing"
            _PctMiss&i = "&&GroupLab&i: % missing / %left(%qsysfunc(putn(&&GroupN&i, __countf.)))"
          %end; ;
run;

proc sort data=&out;
    by &sortby varname;
run;

%*****************************************************************************;
%* IF ANY MISSING AND MISSPATTERN = 1 THEN DESCRIBE                           ;
%*****************************************************************************;
%* list of variables with missing values;
proc sql noprint;
    select VarName into :_misslist separated by ' '
    from &out
    where _NMiss gt 0;
    
    select count(*) into :_missanychar
    from &out
    where _NMiss gt 0 and VarType = "C"; 
quit;

%if &misspattern eq 1 and %symexist(_misslist) %then %do;

    %put **********************************************;
    %put ******     Getting N and N missing      ******;
    %put **********************************************;

    data _keepmissing_;
        set _tempd_;
        keep &_misslist;
    run; 
    
    ods exclude all; 
    proc mi data=_keepmissing_ nimpute=0 displaypattern=nomeans;
        %if &_missanychar gt 0 %then %do;
            class _character_; 
            fcs logistic(_character_) reg(_numeric_);
        %end;
        var &_misslist;
        ods output misspattern = _pattern_;
    run;
    ods exclude none;

%end;

%*****************************************************************************;
%* REPORT                                                                     ;
%*****************************************************************************;

%* Determine if data set has any missing values. If not, then put message and skip report;
proc sql noprint;
    select sum(_NMiss) > 0 into :AnyMissing
    from &out;
quit;

%if &AnyMissing = 0 %then %do;
    data _null_;
        file print;
        put _page_;
        put "Data set %upcase(&data) has no missing values, no report is generated.";
    run;        
%end;

%else %if &AnyMissing = 1 %then %do;

    %if %length(&rtffile) gt 0 %then %do;
    
        %* Change the margins;
        ods path reset;
        ODS path (REMOVE) WORK.TEMPLAT;
        ODS path (PREPEND) work.TEMPLAT(update);
        
        proc template;
            define style work.myrtf;
            parent=styles.rtf;
            style body from document /
                leftmargin=0.5in
                rightmargin=0.5in
                topmargin=0.5in
                bottommargin=0.5in;
            end;
        run;
        
        ODS path (REMOVE) sasuser.TEMPLAT;
        ods path reset;    
        
        %*Options;
        options nocenter nodate nonumber orientation = &page;
    
        %*Start file;
        ods noproctitle;
        ods rtf file = &rtffile style = myrtf bodytitle;  
    %end;
    
    %if %length(&xlsxfile) gt 0 %then %do;
        ods excel file = &xlsxfile style = myrtf 
        	options(autofilter="all"  
            frozen_headers="on"
            frozen_rowheaders="3"
            flow="Tables"
        	sheet_name = "Missing value report");
    %end;
    
    %put **********************************************;
    %put ******          Missing Values          ******;
    %put **********************************************;
    
    %* Format N values;
    %let TotalN = %left(%qsysfunc(putn(&TotalN, __countf.)));
    %do i = 1 %to &NGroups;
        %let GroupN&i = %left(%qsysfunc(putn(&&GroupN&i, __countf.)));
    %end;
    
    %* Order of byVar columns;
    %if &NGroups gt 1 %then %do;
    	%if &reversecol=1 %then %do;
    		%let startVal = &NGroups;
    		%let endVal = 1;
    		%let byVal = -1;    	
    	%end;
    	%else %do;
    		%let startVal = 1;
    		%let endVal = &NGroups;
    		%let byVal = 1;
    	%end;
    %end;
    
    *ods listing close;
    proc report data= &out %if &printall = 0 %then %do; (where = (_PctMiss gt 0)) %end; nowd list;
    
        columns ("Variable Description" VarName VarLabel VarType)
                ("% Missing" _PctMiss 
                  %if &NGroups gt 1 %then %do;
                    ("By %upcase(&byvar)" %do i=&startVal %to &endVal %by &byVal; _PctMiss&i %end;)
                  %end;
                )
        ;
    
        compute before _PAGE_ /
          style = {font_weight = bold just = left
          borderbottomcolor = black bordertopcolor = white 
          borderrightcolor = white borderleftcolor = white };
            line "Missing value report for data set %upcase(&data)";
        endcomp; 
        
        define VarName / display left
          style(column) = {just = left }  style(header) = {just = left };
        define VarLabel / display left
          style(column) = {just = left }  style(header) = {just = left };
        define VarType / display left
          style(column) = {just = left }  style(header) = {just = left };
    
        define _PctMiss / display "All / (N = &TotalN)" center
          style(column) = {just = center }  style(header) = {just = center } ;
        compute _PctMiss ;
            if _PctMiss gt &highlightval then call define(_col_, "style", "style={foreground=red font_weight=bold}");
        endcomp;
    
        %if &NGroups gt 1 %then %do i=&startVal %to &endVal %by &byVal;
            define _PctMiss&i / display "&&GroupLab&i / (n = &&GroupN&i)"
              style(column) = {just = center }  style(header) = {just = center };
            compute _PctMiss&i ;
                if _PctMiss&i gt &highlightval then call define(_col_, "style", "style={foreground=red font_weight=bold}");
            endcomp;        
        %end;
    
    run;
    
    %* If requested and any missing, missing data pattern report;
    %if &misspattern eq 1 and %symexist(_misslist) %then %do;
    
        %put **********************************************;
        %put ******          Missing Pattern         ******;
        %put **********************************************;
 
        %if %length(&xlsxfile) gt 0 %then %do;
            ods excel options(sheet_name = "Missing data pattern");
        %end;

        proc report data= _pattern_ nowd list
          style(column) = {just = left }  style(header) = {just = left };
            
            columns _all_;
            
            compute before _PAGE_ /
              style = {font_weight = bold just = left
              borderbottomcolor = black bordertopcolor = white 
              borderrightcolor = white borderleftcolor = white };
                line "Missing data pattern for data set %upcase(&data) (N = &nobs)";
            endcomp;                 
               
            format _character_ $__missf. Percent __Pctf.;
        run;
    
    %end; **end to missing pattern;
    
    %if %length(&rtffile) gt 0 %then %do;
        ods rtf close;
    %end;
    
    %if %length(&xlsxfile) gt 0 %then %do;
        ods excel close;
    %end;        

%end; %* end to anymissing;

%*****************************************************************************;
%* DELETE DBS FROM WORK LIB                                                   ;
%*****************************************************************************;
%if &debug ne 1 %then %do;

    proc datasets lib=work nodetails nowarn nolist;
        delete _tempd_ _ngroups_ _parsevs_ _vtype_ _pattern_;
    run; quit;

%end;

%*****************************************************************************;
%* MEND MACRO                                                                 ;
%*****************************************************************************;
options nomlogic nomlogicnest;
*ods listing;

%mend missing;
