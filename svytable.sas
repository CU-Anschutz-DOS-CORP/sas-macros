/**
 @file
 @brief Creates a descriptive table using SURVEY procedures

 @details
 This program computes summaries for one or more group(s) and test 
 statistics for differences between two (or more) independent samples 
 using SAS SURVEY procedures. It can output well-formatted tables 
 into RTF files.

 @author Rocio Lopez

 @date 07-09-2018

 @version SAS 9.4

 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %svytable(
	data = , class = , 
	domain = , strata = , cluster = , weight = ,
	con1 = , con3 = , cat1 = , cat2 = , 
	alpha = 0.05, colpct = 1, secl = se, seopt = 1, oddsratios = 0,
	pvalues = 1, totalcol = 1, ncol = 0, misscol = 0,
	desclabel = 1, boldlabel = 0, boldsigp = 1, printptype = 1,
	lablen = $100., outputwidth = , tbltitle = , sortby = , list = , 
	f1 = __countf., f2 = __estf., pfmt = pvaluef., orfmt = __estf.,
	cwidth1 = 85, cwidth2 = , cwidth3 = , 
	out = _out_, rtfout = _rtf_,
	rtffile = , style = journal, page = portrait,
	bdytitle = 0, daytime = 0, debug=0);
 @endcode

 @returns
 SAS output listing (if running in SAS Studio) and data sets of
 summary statistics, as well as an RTF file, if requested

 @param data The name of the input dataset to be used
 
 @param domain Domain variable that identifies the subset of interest
 @n The domain of interest must be coded as 1
 @n By default all records will be used

 @param strata Variable that defines sampling strata

 @param cluster Variable that defines sampling clusters

 @param weight Variable that defines sampling weight

 @param con1 List of continouos variables to summarize as mean 
 +/- SE or CL and compare using linear regression

 @param con3 List of continouos variables to summarize as 
 median [Q1, Q3] and compare using linear regression with log transformation

 @param cat1 List of binary variables to summarize as % (SE or CL)
 and compare using Rao-Scott chi-square
 @n Variables should be numeric and coded 0/1 or 1/2; only level=1 will be displayed

 @param cat2 List of nominal variables to summarize as % (SE or CL)
 and compare using Rao-Scott chi-square
 @n Can be numeric or character; all levels will be displayed

 @param class The variable which defines the groups
 @n Optional; must be a numeric variable

 @param alpha Specify overall significance level
 @n This will be used to bold p-values and to calculate post-hoc significance criterion, 
 if applicable
 @n Default is 0.05, value should be between 0 and 0.99

 @param colpct Choose between column or row percentages for cat1, cat2 or ord1 variables
 @n Allowed options are: 
 @n 0 = Row percent
 @n 1 = Column percent (DEFAULT)

 @param oddsratios Include unadjusted odds ratios when class variable has 2 groups.
 @n These are calculated with CLASS var as the outcome and the following options: 
 order=internal, event descending, ref=first
 @n Allowed options are:
 @n 0 = No, do not include ORs (DEFAULT)
 @n 1 = Yes, include ORs 

 @param secl Choose between standard error or 1-alpha% confidence interval
 @n Allowed options are:
 @n SE = standard error (DEFAULT)
 @n CL = confidence limits

 @param cltype Choose method to calculate confidence limits for proportions
 @n Allowed options are: WALD (DEFAULT), CLOPPERPEARSON, CP, LOGIT, WILSON, SCORE

 @param seopt Choose how the standard error should be displayed
 @n Allowed options are:
 @n 1 = with +/- sign (DEFAULT)
 @n 2 = in parentheses

 @param pvalues Include p-values for group comparisons when 2 or more groups in 
 CLASS var
 @n Allowed options are:
 @n 0 = No, only include summary statistics
 @n 1 = Yes, include p-values (DEFAULT)

 @param totalcol Include Overall summary statistics when class variable specified
 @n Allowed options are: 
 @n 0 = No, do not include overall columns
 @n 1 = Yes, include overall columns (DEFAULT)

 @param ncol Include a column with N for each variable
 @n Allowed options are:
 @n 0 = No N column included (DEFAULT)
 @n 1 = Overall N
 @n 2 = N Per class level
 @n 3 = Both Overall and per group Ns

 @param misscol Include a column with n missing for  each variable
 @n If ncol is also specified, misscol will be ignored
 @n Allowed options are:
 @n 0 = No n missing column included  (DEFAULT)
 @n 1 = Overall n missing
 @n 2 = Missing per class level
 @n 3 = Both Overall and per group missing

 @param desclabel Use variable label with information on values presented 
 [e.g. Male, No. (%) (instead of Male)]
 @n Allowed options are:
 @n 0 = No, use variable label only (DEFAULT)
 @n 1 = Yes, use more descriptive label 

 @param BoldedLabel Specify whether label column should use bold font
 @n Allowed options are:
 @n 0 = No, use normal font (DEFAULT)
 @n 1 = Yes, use bold font

 @param printptype Denote type of test used with a superscript for each p-value
 @Regardless of choice, a footnote with definition of superscripts will be printed
 @n Allowed options are:
 @n 0 = No, do not denote type of test
 @n 1 = Yes, denote type of test (DEFAULT)

 @param BoldedSigP Specify whether significant p-values use bold font
 @n You can use ALPHA option to set your significance level
 @n Allowed options are:
 @n 0 = No, use normal font
 @n 1 = Yes, use bold font for significant p (DEFAULT)

 @param labLen Maximum Label length captured
 @n Default is $100.

 @param tbltitle A title for table surronded by double quotes

 @param addfn Optional user-defined footnote surrounded by double quotation marks

 @param sortby Choose how to sort rows in final output
 @n Allowed options are:
 @n list    : Sort by the list given (see LIST)
 @n pval    : Sort by the p-value
 @n varname : Sort by variable names
 @n <blank> : In input order for CON1, CON2, CON3, CAT1, CAT2, ORD1 (DEFAULT)
 
 @param list List the variables in the order they should be reported when SORTBY=LIST
 @n If you do not include all variables, the rest will be included in input order

 @param f1 Format applied to counts (n) in RTFOUT
 @n Default is __COUNTF.

 @param f2 Format applied to mean, SE, medians, P25, P75, %, CL in RTFOUT
 @n Default is __ESTF.

 @param orFmt Format applied to OR and CL in RTFOUT
 @n Default is __ESTF.

 @param pFmt Format applied to p-values in RTFOUT
 @n Default is PVALUEF.

 @param cwidth1 Column width for N total/N missing/p-value columns
 @n Default is 85

 @param cwidth2 Column width for summary stats columns
 @n Default is 200 when CL is requested, 175 otherwise

 @param cwidth3 Column width for variable labels
 @n Default is 375 when desclabel=1 or any CON3 variables included,
 350 when desclabel=1 without con3, and 300 otherwise

 @param out Name for the data set in which the results are stored
 @n Default is _OUT_

 @param rtfout The data set in which results for RTF printing are stored
 @n Default is _RTF_

 @param rtffile Path and filename for optional RTF output file
 @n It should be surrounded in quotes, e.g. "~userid/consult/out.rtf"

 @param style Define a style for ods output
 @n Default is JOURNAL

 @param page Specify page orientation for output files
 @n Allowed options are:
 @n PORTRAIT (DEFAULT)
 @n LANDSCAPE

 @param outputwidth Set percentage of the page the table occupies in 
 the output report
 @n Value must be 1-100 and if not specified PROC REPORT will decide
 best value

 @param bdytitle Location of SAS system titles/footnotes on RTF file
 @n Allowed options are:
 @n 0=Put Titles in Header and Footnotes in Footer (DEFAULT) 
 @n 1=Put Main Title and Footnotes in the Body of the RTF file

 @param daytime Add a date/time stamp as a footnote to the ods files.
 @n Allowed options are:
 @n 0 = No date stamp (DEFAULT)
 @n 1 = Yes, include date/time stamp

 @param debug Toggle between debugging options on/off
 @n Allowed options are:
 @n 0 for Off (DEFAULT)
 @n 1 for On

 @note
 @parblock
 @li  It is a good idea to have all variables formatted, especially the
 CLASS variable
 
 @li You can sort the results by any combination of variables found in
 the &OUT dataset and use the DESCENDING option as well
 
 @li CAT2 may contain SAS numeric or character variables, all other
 variable lists should contain SAS numeric variables
 
 @li The RTFOUT dataset is still created even if RTFFILE is blank
 
 @li Variables which have all missing values will not be analyzed; a warning 
 of this occurrence is sent to the SAS log
 
 @li Created data sets that are NOT deleted:  &OUT &RTFOUT
 
 @li If NTOTALCOL=0, variable labels are flagged if there are missing
 values and a footnote is added stating # for each var
 
 @li BDYTITLE parameter only works with RTF files
 
 @li If no class variable specified, TotalCol is ignored. If you want
 to print the N or N miss columns use NCOL=2 or MISSCOL=2 otherwise
 it will be ignored
 
 @li If running the interactive SAS in Linux, no output will be seen
 on the listing window but the RTF file will be created
 @endparblock

 @par Example
 @code{.sas}
 filename xptIn url "https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_I.xpt"; 
 libname xptIn xport;

 data demo_i;
   set xptIn.demo_i;
   analysisSubset = (RIDAGEYR ge 18); **adults;
 run;

 libname xptIn clear;

 %svytable(
     data = demo_i
    ,class = RIAGENDR
    ,domain = analysisSubset
    ,strata = SDMVSTRA
    ,cluster = SDMVPSU
    ,weight = WTINT2YR    
    ,con1 = INDFMPIR
    ,cat2 = RIDRETH1 DMDEDUC2
    ,tbltitle = "Example of SVYTABLE"     
    ,rtffile = "./exampleSvyTable.rtf"
    );
 @endcode
 
 @par Revision History
 @b 08-13-2021 Added option CLTYPE to control method for calculating CL 
 for proportions
 @n @b 05-12-2021 Fixed bug that was causing the N columns to be blank for 
 continuous vars
 @n @b 03-04-2021 Updated label of Overall column
 @n @b 01-18-2021 Fixed bug that was generating error message when no class 
 var given
 @n @b 01-12-2021 Fixed bug that was causing the missing N columns to be blank
 @n @b 12-18-2020 Updates for added efficiency and to better deal with 
 domain issues in PROC SURVEYFREQ
 @n @b 11-04-2020 Fixed problem with overall means for Domain=1 and missing 
 class; Now the total will be the combination of the class groups
 @n @b 07-15-2020 Fixed bug that was causing the p-value for CON3 vars to 
 be misssig.
 @n @b 07-08-2020 Fixed bug which was causing the p-values not to appear when 
 domain = 0 has all CAT variables mising or = to the same level.
 @n @b 07-10-2019 Log transformation for CON3 vars are now only performed on 
 non-missing values; this is to avoid the note about operation on missing values
 @n @b 06-20-2019 Update on how overall cat levels are handled when no CLASS
 var given 
 @n @b 03-14-2019 Fixed a problem where the CL was printing as (LCL, LCL)
 @n @b 11-07-2018 Fixed a bug that was causing problems with a sort when no cat
 variables were included
 @n @b 08-24-2018 Added a warning message for when observations are omitted
 due to non-positive weights
 @n @b 08-17-2018 Made modification to allow for x: syntax when referencing 
 variable lists (previous version already allowed x1-x10 syntax)
 @n @b 07-26-2018 Added functionality to treat a 2-level class variable as the
 outcome and report OR, CIs and p-values from logistic regression
 **/

%macro svytable(
    data = ,
    class = ,
    
    /* Design options */
    domain = , 
    strata = ,
    cluster = ,
    weight = ,
    
    /* Variable lists to summarize */
    con1 = ,
    con3 = ,
    cat1 = ,
    cat2 = ,
    
    /* reporting options */
    alpha = 0.05,
    colpct = 1,
    oddsratios = 0,
    secl = se,
    cltype = wald,
    seopt = 1, 
    pvalues = 1,
    totalcol = 1,
    ncol = 0,
    misscol = 0,
    desclabel = 1,
    boldlabel = 0,
    boldsigp = 1,
    printptype = 1,
    lablen = $100., 
    tbltitle = , 
    addfn= ,

    /* sorting */
    sortby = , 
    list = ,
    
    /* formats */
    f1 = __countf.,
    f2 = __estf.,
    orfmt = __estf.,
    pfmt = pvaluef.,
    
    /* column widths for table */
    cwidth1 = 85,
    cwidth2 = ,
    cwidth3 = ,
    
    /* Output data sets */
    out = _out_,
    rtfout = _rtf_,
    
    /* rtf file */
    rtffile = ,
    style = journal,
    page = portrait,
    outputwidth = , 
    bdytitle = 0,
    daytime = 0,
    
    debug=0);

%*****************************************************************************;
%* DELETE DBS FROM WORK LIB                                                   ;
%*****************************************************************************;
proc datasets lib=work nodetails nolist nowarn;
    delete _ngroups_ _n_ _n_1 _keepn_ _keepn_1-_keepn_99 _ods_ _ods2_ _order_
    _con1_ _con1_1 _keepcon1_ _keepcon1_1-_keepcon1_99 &con1
    _con3_ _con3_1 _keepcon3_ _tkeepcon3_ _keepcon3_1-_keepcon3_99 
    _tkeepcon3_1-_tkeepcon3_99 _templog_ &con3 _cat1_ _keepcat1_ 
    _keepcat1_1-_keepcat1_99 _cat2_ _keepcat2_ _keepcat2_1-_keepcat2_99
    _oddsratios_ _outds_ _outcon1_ _outcon3_ _outcat1_ _outcat2_
    _ttest1_ _ttest2_ _chisq1_ _chisq2_ _summ_ _pval_ _tempd_;
run;
quit;

%*****************************************************************************;
%* FORMATS                                                                    ;
%*****************************************************************************;
proc format;
     picture Pvaluef (round)
             0.985   -   high    = "0.99"    (NoEdit)
             0.10    -<  0.985   = "9.99"
             0.001   -<  0.10    = "9.999"
             0       -<  0.001   = "<0.001"  (NoEdit)
             . = " ";
     picture __Estf (round)
             low   -  -1.1  = "00000000000009.9" (prefix='-')
            -1.1  <-<  0    = "9.99" (prefix='-')
               0   -<  1.1  = "9.99"
             1.1   -   high = "00000000000009.9";
     picture __Countf (round)
             low-999   = "009"
             1000-high = "0,000,000,000,000,009"
             . = " ";             
run;
quit;

%*****************************************************************************;
%* ERROR CHECKING                                                             ;
%*****************************************************************************;
%let errorflag=0;
options minoperator varinitchk=nonote;

%* Check debug option and turn on macro checking options;
%if not(%upcase(&debug)  in (0 1)) %then %do;
    %put WARNING: &debug must be either 0 or 1.;
    %put WARNING: DEBUG = **&debug**;
    %put WARNING: Default value of 0 will be used for DEGUG;
    %let degug=0;
%end; %*end to debug check;

%if &debug eq 1 %then %do;
    options mlogic mlogicnest varinitchk=warn mergenoby=warn;
%end; %* end to debug eq 1;

%* Check DATA is given and exists;
%if %length(&data) = 0 %then %do;
    %PUT ERROR: Required parameter DATA not provided. Macro will stop executing;
    %LET ERRORFLAG=1;
%end; %*end to if length(data) =  0;

%if %length(&data) > 0 and %SYSFUNC(exist(&data)) ne 1 %then %do;
    %PUT ERROR: Data set &data does not exist. Macro will stop executing;
    %LET ERRORFLAG=1;
%end; %*end to if length(data) ne  0;

%* Make sure at least 1 variable list provided;
%if %length(&con1 &con3 &cat1 &cat2)=0 %then %do;
    %put ERROR: At least one of CON1, CON3, CAT1, CAT2 must be provided. Macro will stop executing;
    %let errorflag=1;
%end;    

%* If data exists, parse variable lists in case x1-x10 syntax used;
%if %length(&data) > 0 and %SYSFUNC(exist(&data))=1 %then %do;

    %macro parselist(vlist= , name= );
    
        %* parse variable list to long format;
        data __parsevs;
            retain &vlist;
            set &data;
            if _n_=1;
            keep &vlist;
        run;            
            
        proc contents 
            data=__parsevs(obs=0) 
            out=__parsevs2(keep=name label varnum) noprint; 
        run;  
        
        proc sql noprint;
            select name into :NewList separated by ' '
            from __parsevs2
            order by varnum;
        quit;
        
        %* set original list to new parsed list;
        %let &&name = &newlist;
       
        proc datasets lib=work nolist nodetails nowarn;
            delete __parsevs __parsevs2;
        run; quit;            
       
    %mend parselist;
    
    %if %length(&con1)>0 %then %do;
        %parselist(vlist=&con1, name=con1);
    %end;  
    %if %length(&con3)>0 %then %do;  
        %parselist(vlist=&con3, name=con3);
    %end;
    %if %length(&cat1)>0 %then %do;
        %parselist(vlist=&cat1, name=cat1);
    %end;
    %if %length(&cat2)>0 %then %do;
        %parselist(vlist=&cat2, name=cat2);
    %end;
    %if %length(&list)>0 %then %do;
        %parselist(vlist=&list, name=list);
    %end;
    
%end; %*end to parsing vars;    

%* Check and update pcttype. 
   Because of they way I set up SURVEYFREQ group*var
   COL and ROW will be the opposite of requested;
%if not(&colpct in (0 1)) %then %do;
    %put WARNING: COLPCT must be either 1 or 0.;
    %put WARNING: COLPCT = **colpct**;
    %put WARNING: Default value of 1 will be used for COLPCT.;
    %let colpct = 1;
%end; %*end to secl check;
%if &colpct=1 %then %let pcttype=row;
%else %if &colpct=0 %then %let pcttype=col;

%* Check valid options are given for other parameters;
%if &alpha le 0 or &alpha ge 1 %then %do;
    %put WARNING: ALPHA must be a value between 0 and 1.;
    %put WARNING: ALPHA = **&alpha**;
    %put WARNING: Default value of 0.05 will be used for ALPHA.;
    %let alpha=0.05;
%end; %* end to alpha check;

%if %length(&sortby)>0 %then %do;
%if not(%upcase(&sortby) in (LIST PVAL VARNAME)) %then %do;
    %put WARNING: SORTBY must be either blank or equal to LIST, PVAL OR VARNAME.;
    %put WARNING: SORTBY = **&SORTBY**;
    %put WARNING: Sorting will be done according to listing order.;
    %let sortby = ;
%end; %*end to if sortby check;
%end;

%if not(%upcase(&secl)  in (SE CL)) %then %do;
    %put WARNING: SECL must be either SE or CL.;
    %put WARNING: SECL = **&secl**;
    %put WARNING: Default value of SE will be used for SECL.;
    %let secl=se;
%end; %*end to secl check;

%if not(%upcase(&cltype)  in (WALD CLOPPERPEARSON CP LOGIT WILSON SCORE)) %then %do;
    %put WARNING: CLTYPE must be either WALD, CLOPPERPEARSON, CP, LOGIT, WILSON or SCORE.;
    %put WARNING: CLTYPE = **&cltype**;
    %put WARNING: Default value of WALD will be used for CLTYPE.;
    %let cltype=wald;
%end; %*end to setype check;

%if not(&seopt  in (1 2)) %then %do;
    %put WARNING: SEOPT must be either 1 or 2.;
    %put WARNING: SEOPT = **&seopt**;
    %put WARNING: Default value of 1 will be used for SEOPT.;
    %let seopt=1;
%end; %*end to seopt check;

%if not(&ncol  in (0 1 2 3)) %then %do;
    %put WARNING: NCOL must be either 0, 1, 2 or 3.;
    %put WARNING: NCOL = **&ncol**;
    %put WARNING: Default value of 0 will be used for NCOL.;
    %let ncol=0;
%end; %*end to ncol check;

%if not(&misscol  in (0 1 2 3)) %then %do;
    %put WARNING: MISSCOL must be either 0, 1, 2 or 3.;
    %put WARNING: MISSCOL = **&misscol**;
    %put WARNING: Default value of 0 will be used for MISSCOL.;
    %let misscol=0;
%end; %*end to misscol check;

%if not(%upcase(&page)  in (PORTRAIT LANDSCAPE)) %then %do;
    %put WARNING: PAGE must be either PORTRAIT or LANDSCAPE.;
    %put WARNING: PAGE = **&page**;
    %put WARNING: Default value of PORTRAIT will be used for PAGE.;
    %let page=portrait;
%end; %*end to page check;

%* Check all 0/1 options;
%macro optcheck(option= , newoption= , default= );
    %if not(&option  in (0 1)) %then %do;
        %put WARNING: &NEWOPTION must be either 0 or 1.;
        %put WARNING: &NEWOPTION = **&option**;
        %put WARNING: Default value of &default will be used for &newoption.;
        %let &newoption=&default;
    %end; %*end to option check;
%mend optcheck;

%optcheck(option=&oddsratios, newoption=pvalues, default=1);
%optcheck(option=&pvalues, newoption=pvalues, default=1);
%optcheck(option=&totalcol, newoption=totalcol, default=1);
%optcheck(option=&desclabel, newoption=desclabel, default=1);
%optcheck(option=&boldlabel, newoption=boldlabel, default=0);
%optcheck(option=&boldsigp, newoption=boldsigp, default=1);
%optcheck(option=&printptype, newoption=printptype, default=1);
%optcheck(option=&bdytitle, newoption=bdytitle, default=0);
%optcheck(option=&daytime, newoption=daytime, default=0);

%* RTFFILE, TBLTITLE and ADDFN should have quotes;
%if %length(&rtffile)>0 and %sysevalf(%sysfunc(indexc(&rtffile., '"'))^= 1) %then %do;
    %put
    ERROR: Double quotes are required for RTFFILE. Macro will stop executing.;
    %let ErrorFlag=1;
%END;

%if %length(&tbltitle)>0 and %sysevalf(%sysfunc(indexc(&tbltitle., '"'))^= 1) %then %do;
    %put
    ERROR: Double quotes are required for TBLTITLE. Macro will stop executing.;
    %let ErrorFlag=1;
%END;

%if %length(&addfn)>0 and %sysevalf(%sysfunc(indexc(&addfn., '"'))^= 1) %then %do;
    %put
    ERROR: Double quotes are required for ADDFN. Macro will stop executing.;
    %let ErrorFlag=1;
%END;

%* If data set exist, check variables exist;
%if %length(&data) > 0 and %SYSFUNC(exist(&data))=1 %then %do;
    
    %* Check that variables in list(s) exit, are numeric and not missing;
    %macro vexist(list= , name=);
        
        %let new&NAME= ;
        %let icount=1;
        %let cvar=%scan(&LIST,&ICOUNT);
        %let nextvar=&CVAR;
        
        %let dsid=%sysfunc(open(&data));
        %if %length(&LIST) > 0 %then %do %while(&NEXTVAR NE );
            
            %let cvar=%upcase(&NEXTVAR);
            %let icount=%eval(&ICOUNT+1);
            %let nextvar=%scan(&LIST,&ICOUNT);
            
            %* variable exits?;
            %let exist=%sysfunc(varnum(&DSID, &CVAR));
           
            %* If yes, type and missing;
            %if &exist>0 %then %do;
                %let vartype=%sysfunc(vartype(&DSID, &EXIST)); 
        
                proc sql noprint;
                    select count(&CVAR) into :nmvalues
                    from &data;
                quit;
            %end;
        
            %if &EXIST > 0 and &VARTYPE = N and &NMVALUES > 0 
              %then %let new&NAME=&&NEW&NAME &CVAR;
            %else %do;
                %let new&NAME=&&NEW&NAME;
                %if &EXIST = 0 %then
                    %put WARNING: %upcase(&CVAR) in VLIST list does not exist in data set &data and will be dropped.;  
                %else %if &VARTYPE ne N %then
                    %put WARNING: %upcase(&CVAR) in VLIST is not a numeric variable and will be dropped.;    
                %else %if &NMVALUES = 0 %then
                    %put WARNING: %upcase(&CVAR) in VLIST is completely missing and will be dropped.;    
            %end;
        
        %end; ** end to if length(VLIST) > 0 then do while;
        %let rc=%sysfunc(close(&DSID));
        
        %LET &&NAME=&&NEW&NAME;
    
    %mend VEXIST;   
    
    %vexist(list=&con1, name=con1);
    %vexist(list=&con3, name=con3);
    %vexist(list=&cat1, name=cat1);
    
    %* Check that variables in CAT2 exit and not missing;    
    %let newlist= ;
    %let icount=1;
    %let cvar=%scan(&CAT2,&ICOUNT);
    %let nextvar=&CVAR;
    
    %let dsid=%sysfunc(open(&DATA));
    %if %length(&CAT2) > 0 %then %do %while(&NEXTVAR NE );
        
        %let cvar=%upcase(&NEXTVAR);
        %let icount=%eval(&ICOUNT+1);
        %let nextvar=%scan(&CAT2,&ICOUNT);
        
        %let exist=%sysfunc(varnum(&DSID, &CVAR));
        %if &exist>0 %then %do;    
            proc sql noprint;
                select count(&CVAR) into :nmvalues
                from &data;
            quit;
        %end;
    
        %if &EXIST > 0 and &NMVALUES > 0 
          %then %let newlist=&NEWLIST &CVAR;
        %else %do;
            %let newlist=&NEWLIST;
            %if &EXIST = 0 %then
                %put WARNING: %upcase(&CVAR) in CAT2 list does not exist in data set &data and will be dropped.;    
            %else %if &NMVALUES = 0 %then
                %put WARNING: %upcase(&CVAR) in CAT2 is completely missing and will be dropped.;    
        %end;
    
    %end; ** end to if length(CAT2) > 0 then do while;
    %let rc=%sysfunc(close(&DSID));
    
    %LET CAT2=&newlist;
   
    %* Check design variables exist, are not missing;
    %macro svexist(var= );
        %if %length(&var)>0 %then %do;
        
            %* Check if VAR exists and if so type;
            %let dsid=%sysfunc(open(&data));
            %let exist=%sysfunc(varnum(&dsid, &var));
            %let rc=%sysfunc(close(&dsid)); 
            
            %* variable must exist;
            %if &exist=0 %then %do;
                %put ERROR: Variable %upcase(&VAR) does not exist in dataset &data. Macro will stop executing.;
                %let ERRORFLAG=1;
            %end; %*end to if EXIST then do;
                
            %* If it var exists, check that it is not completely missing.;
            %if &exist > 0 %then %do;
                
                %* Check missingness;
                proc sql noprint;
                    select count(&var) into :nmvalues
                    from &data;
                
                %* if provided, must not be completely missing;
                %if &nmvalues=0 %then %do;
                    %put ERROR: Variable %upcase(&VAR) is completely missing. Macro will stop executing.;
                    %let ERRORFLAG=1;
                %end; %* end to if NMVALUES then do;
                
            %end; %*end to if EXIST >0 and VARTYPE = N then do;
        
        %end; %* end to if length(&VAR)>0;    
    %mend svexist;
    
    %svexist(var=&domain);
    %svexist(var=&strata);
    %svexist(var=&cluster);

    %* Check design variables exist, are not missing and are numeric;
    %macro svexist2(var= );
        %if %length(&var)>0 %then %do;
        
            %* Check if VAR exists and if so type;
            %let dsid=%sysfunc(open(&data));
            %let exist=%sysfunc(varnum(&dsid, &var));
            %let vartype=%sysfunc(vartype(&DSID, &EXIST)); **input var num, not name;
            %let rc=%sysfunc(close(&dsid)); 
            
            %* variable must exist;
            %if &exist=0 %then %do;
                %put ERROR: Variable %upcase(&VAR) does not exist in dataset &data. Macro will stop executing.;
                %let ERRORFLAG=1;
            %end; %*end to if EXIST then do;

            ** variable must be numeric (can switch to C if character required);
            %if &exist gt 0 and &VARTYPE ne N %then %do;
                %put ERROR: Variable &VAR must be numeric. Macro will stop executing.;
                %let ERRORFLAG=1;
            %end; ** end to if VARTYPE ne N then do;
                
            %* If it var exists, check that it is not completely missing.;
            %if &exist gt 0 %then %do;
                
                %* Check missingness;
                proc sql noprint;
                    select count(&var) into :nmvalues
                    from &data;
                
                %* if provided, must not be completely missing;
                %if &nmvalues=0 %then %do;
                    %put ERROR: Variable %upcase(&VAR) is completely missing. Macro will stop executing.;
                    %let ERRORFLAG=1;
                %end; %* end to if NMVALUES then do;
                
            %end; %*end to if EXIST >0 and VARTYPE = N then do;
        
        %end; %* end to if length(&VAR)>0;    
    %mend svexist2;
    
    %svexist2(var=&class);
    %svexist2(var=&weight);
       
%end; %* end to length(data)>0 and sysfunc(exit(data))=1;

%* If list not given, create;
%if %length(&list)=0 %then %let list=&con1 &con3 &cat1 &cat2;

%*****************************************************************************;
%* IF ANY ERRORS FOUND THEN ABORT EXECUTION                                                   ;
%*****************************************************************************;
%if &errorflag eq 1 %then %do;
    data _null_;
        abort 3;
    run;
%end;

%*****************************************************************************;
%* IF NO ERRORS START MACRO                                                   ;
%*****************************************************************************;
ods graphics off;
%if &debug ne 1 %then %do;
    ods listing close;
    ods select none;
%end;

%*****************************************************************************;
%*  SET UP                                                                  ;
%*****************************************************************************;
%*Get class format to apply in workding data set;
%if %length(&class) gt 0 %then %do;
    data _null_;
        set &data (obs=1);
        call symput('classfmt', vformat(&class));
    run;
%end;

%* If all cat vars are missing for Domain=0 it causes issues in the ods output;
%* Will set to most frequent value in order to avoid problems; 
%let allvars = &cat1 &cat2;
%let i = 1;
%let thisvar = %scan(&allvars, &i);
%do %until(&thisvar= );
    data _null_;
        set &data (where = (not missing(&thisvar)) obs=1) ;
        call symput("tp&thisvar", vtype(&thisvar));
        call symput("mcv&thisvar", &thisvar);
    run;
        
    %let i = %sysevalf(&i + 1);
    %let thisvar = %scan(&allvars, &i);
%end; 

%*Create working data set;
data _tempd_;
    set &data;
    %* avoid the class variable name causing problems in ods;
    %* if no domain or class specified then set to "Total" for ease of management;
    %if %length(&domain)=0 %then %do;
        __domain=1;
    %end; 
    %else %do;
        __domain=&domain;
    %end;        
    %if %length(&class)=0 %then %do;
        __class=1;
        label __class="All";
    %end;      
    %else %do;
        __class=&class;
    %end;
    
    %* This will help manage the output;
    if missing(__domain) or missing(__class) then __domain = 0;

    %* Setting to most frequent value if missing for Domain=0 in order to avoid problems;
    %let i = 1;
    %let thisvar = %scan(&allvars, &i);
    %do %until(&thisvar= );
        %if &&tp&thisvar eq N %then %do;
            if __domain=0 and missing(&thisvar) then &thisvar = &&mcv&thisvar;
        %end;
        %else %if &&tp&thisvar eq C %then %do;
            if __domain=0 and missing(&thisvar) then &thisvar = left(trim("&&mcv&thisvar"));
        %end;
        %let i = %sysevalf(&i + 1);
        %let thisvar = %scan(&allvars, &i);
    %end;       
    
    %if %length(&class) gt 0 %then %do;
        format __class &classfmt ;
    %end;
run;

%*upper case class var;
%let class=%upcase(&class);

%*count vars in list;
%let nlist=0;
%do %while(%scan(&list,&nlist+1) ^=    ); %let nlist=%eval(&nlist+1); %end;

%* count vars in each list;
%let ncon=0;
%do %while(%scan(&con1 &con3,&ncon+1) ^=    ); %let ncon=%eval(&ncon+1); %end;

%let ncat1=0;
%do %while(%scan(&cat1,&ncat1+1) ^=    ); %let ncat1=%eval(&ncat1+1); %end;

%*manage the nominal vars;
%*count vars;
%let ncat2=0;
%do %while(%scan(&cat2,&ncat2+1) ^=    ); %let ncat2=%eval(&ncat2+1); %end;

%*create macro vars for each;
%do i=1 %to &ncat2;
    %let nomcat&i=%upcase(%scan(&cat2, &i));
%end;

%*****************************************************************************;
%* ANY OBSERVATIONS WITH NONPOSITIVE WEIGHTS?                                 ;
%*****************************************************************************;
proc sql noprint;
    select count(&weight) into :_nonpos
    from _tempd_
    where __domain=1 and &weight le 0;

%if &_nonpos gt 0 %then %put
WARNING: &_nonpos observations are omitted due to non-positive weights;

%*****************************************************************************;
%* NUMBER (UNWEIGHTED) AND LABEL FOR CLASS VAR                                ;
%*****************************************************************************;
%put ****** Getting Group Numbers and Labels ******;

proc freq data=_tempd_;
    where __domain=1;
    tables __class;
    ods output OneWayFreqs=_ngroups_;
run;    

%* num of groups;
proc sql noprint;
    select count(distinct(__class)) format 8. into :NGroups
    from _ngroups_ ;
quit;
%let NGroups=&NGroups;

%* if oddsratios requested make sure only 2 groups;
%if &oddsratios eq 1 and &ngroups ne 2 %then %do;
    %put WARNING: CLASS variable &class has &ngroups levels. Odds Ratios will NOT be reported.;
    %let oddsratios = 0;
%end; %*end to oddsratios eq 1 and ngroups ne 2;

%* groups levels;
proc sql noprint;
    select __class format 8. into :loop1-:loop&NGroups
    from _ngroups_;
quit;   

%* groups labels;
proc sql noprint;
    select f___class into :GroupLab1-:GroupLab&NGroups
    from _ngroups_;
quit;

%if %length(&class)=0 and &NGroups=1 %then %do;
    %let GroupLab1=Total;
%end;

%* Unweighted N for each group;    
proc sql noprint;
    select sum(Frequency) format=&f1 into :TotalNLab
    from _ngroups_;
quit;
%let TotalNLab=&TotalNLab; **remove leading blanks;

%do i=1 %to &NGroups;

    proc sql noprint;
        select Frequency format=&f1 into :GroupN&i
        from _ngroups_
        where __class=&&loop&i;
    quit;
    %let GroupN&i=&&GroupN&i;

%end;

%* clear temp dbs;
%if &debug ne 1 %then %do;
    proc datasets lib=work nolist nodetails nowarn;
        delete _ngroups_ ;
    run;
    quit;
%end;            

%*****************************************************************************;
%* GET N (UNWGT), NMISS(UNWGT), LABEL AND ORDER                               ;
%*****************************************************************************;
%put ****** Getting Numbers and Labels ******;

proc surveymeans data=_tempd_ nobs nmiss;
    domain __domain('1') __domain('1')*__class;
    var &con1 &con3 &cat1 &cat2;
    ods output domain(match_all)=_n_;
run;

%* check f there are any character variables (VarLevel will exist);
%let dsid=%sysfunc(open(_n_));
%let vlexist=%sysfunc(varnum(&DSID, VarLevel));
%let rc=%sysfunc(close(&DSID));

%* manage output;
proc sql ;
    create table _keepn_ as select
    unique(upcase(VarName)) as VarName,
    case
      when missing(VarLabel) then VarName
      else VarLabel
    end as VarLabel,
    sum(N) as N, sum(Nmiss) as NMiss    
    from _n_ 
    where __domain=1
    group by VarName;
    
    %do i=1 %to &NGroups;
        create table _keepn_&i as select
        unique(upcase(VarName)) as VarName,
        case
          when missing(VarLabel) then VarName
          else VarLabel
        end as VarLabel,
        sum(N) as N, sum(Nmiss) as NMiss    
        from _n_1 
        where __domain=1 and __class=&&loop&i
        group by VarName;
    %end;
    
quit;    

data _ods_;
    merge _keepn_(rename=(N=_NObs NMiss=_NMiss))
        %do i=1 %to &NGroups; 
            _keepn_&i(rename=(N=_NObs&i NMiss=_Nmiss&i)) 
        %end; ;
    by VarName ;
run;

%*Order;
options varinitchk = nonote ;
data _order_; informat VarName $256.; run;
%if &debug eq 1 %then %do; options varinitchk = warn; %end;

%do i=1 %to &nlist;
    %let var=%scan(&list, &i.);
    data _torder_;
        informat VarName $256.;
        VarName=upcase("&var.");
        _order=&i;
    run;
    data _order_; set _order_ _torder_; run;
    proc datasets library=work nolist nodetails nowarn; delete _torder_; run; quit;
%end;

data _order_; set _order_; if _n_>1; run;
proc sort data=_order_; by VarName; run;
data _outds_;
    informat VarName $256.;
    merge _ods_ _order_;
    by VarName;
    if _order=. then _order=&nlist+_n_;
run;    
    
%* clear temp dbs;
%if &debug ne 1 %then %do;
    proc datasets lib=work nolist nodetails nowarn;
        delete _n_ _n_1 _keepn_ _ods_ _ods2_ _order_
            %do i=1 %to &NGroups; _keepn_&i %end; ;
    run;
    quit;
%end;            

%*****************************************************************************;
%* SUMMARIZE CON1 VARIABLES                                                   ;
%*****************************************************************************;
%if %length(&con1)>0 %then %do;
    
    %put ****** SUMMARIZING CON1 ******;
    
    proc surveymeans data=_tempd_ mean clm alpha=&alpha;
        %if %length(&weight)>0 %then %do; 
          weight &weight; 
        %end;
        %if %length(&strata)>0 %then %do; 
          strata &strata; 
        %end;
        %if %length(&cluster)>0 %then %do; 
          cluster &cluster; 
        %end;
        domain __domain('1') __domain('1')*__class;
        var &con1;
        ods output domain(match_all)=_con1_;
    run;
    
    %* manage overall output;
    data _keepcon1_;
        set _con1_;
        where __domain=1;
        keep VarName Mean StdErr LowerCLMean UpperCLMean;
    run;
    proc sort data=_keepcon1_; by VarName; run; 
    
    %* manage by group output;
    data %do i=1 %to &NGroups; _keepcon1_&i %end; ;
        set _con1_1;
        where __domain=1;
        keep VarName Mean StdErr LowerCLMean UpperCLMean;
        %do i=1 %to &NGroups;
            if __class=&&loop&i then output _keepcon1_&i;
        %end;    
    run;
    %do i=1 %to &NGroups; 
        proc sort data=_keepcon1_&i; by VarName; run; 
    %end;
    
    %* combine all output;
    data _outcon1_;
        merge _keepcon1_(rename=(Mean=_m StdErr=_s LowerCLMean=_lcl UpperCLMean=_ucl))
            %do i=1 %to &NGroups; 
                _keepcon1_&i(rename=(Mean=_m&i StdErr=_s&i LowerCLMean=_lcl&i UpperCLMean=_ucl&i)) 
            %end; ;
        by VarName;
        VarName=upcase(VarName);
        type="CON1";
    run;
    
    %* clear temp dbs;
    %if &debug ne 1 %then %do;
        proc datasets lib=work nolist nodetails nowarn;
            delete _con1_ _con1_1 _keepcon1_
                %do i=1 %to &NGroups; _keepcon1_&i %end; ;
        run;
        quit;
    %end;            

%end; %* end to if con1;

%*****************************************************************************;
%*  P-VALUES FOR CON1 VARS                                                    ;
%*****************************************************************************; 
%if %length(&class)>0 and &pvalues eq 1 and %length(&con1) gt 0 and &oddsratios eq 0
  %then %do;
    
    %put ****** GETTING P-VALUES FOR CON1 ******;

    %*count vars;
    %let ncon1=0;
    %do %while(%scan(&con1,&ncon1+1) ^=    ); 
        %let ncon1=%eval(&ncon1+1); 
    %end;
    
    %* Loop through vars;
    %do num=1 %to &ncon1;
    
        %let var=%scan(&con1, &num);
    
        proc surveyreg data=_tempd_ nomcar alpha=&alpha;
            %if %length(&weight)>0 %then %do; 
              weight &weight; 
            %end;
            %if %length(&strata)>0 %then %do; 
              strata &strata; 
            %end;
            %if %length(&cluster)>0 %then %do; 
              cluster &cluster; 
            %end;
            domain __domain('1');
            class __class;
            model &var = __class/vadjust=none;
            ods output Effects=&var;
        run;
    
        data &var;
            set &var;
            if __domain=1 and upcase(Effect)="__CLASS";
            VarName=upcase("&var.");
        run;

    %end;
    
    /*Save file**/
    data _ttest1_;
       informat VarName $256.;
       set &con1 ;
       pval=ProbF;
       keep VarName pval;
    run;
    
    /**Delete temp dbs**/
    proc datasets library=work nolist nodetails nowarn;
        delete &con1;
    run;

%end; %*end to if class>0 and pvalues=1;

%*****************************************************************************;
%* SUMMARIZE CON3 VARIABLES                                                   ;
%*****************************************************************************;
%if %length(&con3)>0 %then %do;

    %put ****** SUMMARIZING con3 ******;

    proc surveymeans data=_tempd_ median q1 q3 alpha=&alpha;
        %if %length(&weight)>0 %then %do; 
          weight &weight; 
        %end;
        %if %length(&strata)>0 %then %do; 
          strata &strata; 
        %end;
        %if %length(&cluster)>0 %then %do; 
          cluster &cluster; 
        %end;
        domain __domain('1') __domain('1')*__class;
        var &con3;
        ods output DomainQuantiles(match_all)=_con3_;
    run;
    
    %* Manage overall output;
    data _keepcon3_;
        set _con3_;
        where __domain=1;
        keep VarName PercentileLabel Estimate;
    run;
    proc sort data=_keepcon3_; by VarName; run; 
    proc transpose data=_keepcon3_ out=_tkeepcon3_(drop=_name_);
        by VarName;
        id PercentileLabel;
        var Estimate;
    run;        
        
    %* manage by group output;
    data %do i=1 %to &NGroups; _keepcon3_&i %end; ;
        set _con3_1;
        where __domain=1;
        keep VarName PercentileLabel Estimate;
        %do i=1 %to &NGroups;
            if __class=&&loop&i then output _keepcon3_&i;
        %end;    
    run;
    %do i=1 %to &NGroups; 
        proc sort data=_keepcon3_&i; by VarName; run; 
        proc transpose data=_keepcon3_&i out=_tkeepcon3_&i(drop=_name_);
            by VarName;
            id PercentileLabel;
            var Estimate;
        run;          
    %end;
    
    %* combine all output;
    data _outcon3_;
        merge _tkeepcon3_(rename=(Median=_m Q1=_s Q3=_t))
            %do i=1 %to &NGroups; 
                _tkeepcon3_&i(rename=(Median=_m&i Q1=_s&i Q3=_t&i)) 
            %end; ;
        by VarName;
        VarName=upcase(VarName);
        type="con3";
    run;
    
    %* clear temp dbs;
    %if &debug ne 1 %then %do;
        proc datasets lib=work nolist nodetails nowarn;
            delete _con3_ _con3_1 _keepcon3_ _tkeepcon3_
                %do i=1 %to &NGroups; _keepcon3_&i _tkeepcon3_&i %end; ;
        run;
        quit;
    %end; 
        
%end; %* end to if con3; 

%*****************************************************************************;
%*  P-VALUES FOR CON3 VARS                                                     ;
%*****************************************************************************;  
%if %length(&class)>0 and &pvalues eq 1 and %length(&con3)>0 and &oddsratios eq 0
  %then %do;

    %put ****** GETTING P-VALUES FOR con3 ******;
    
    %*count vars;
    %let ncon3=0;
    %do %while(%scan(&con3,&ncon3+1) ^=    ); 
        %let ncon3=%eval(&ncon3+1); 
    %end;
    
    %* Loop through vars;
    %do num=1 %to &ncon3;
    
        %let var=%scan(&con3, &num);
        
        %* translate and log transform con3 var;
        proc sql noprint;
            select min(&var) into :min
            from _tempd_;
        quit;
        
        data _templog_;
            set _tempd_;
            if not missing(&var) then log_&var=log(&var + 1 - &min);
        run;            
                    
        %* t-test using reg;
        proc surveyreg data=_templog_ nomcar alpha=&alpha;
            %if %length(&weight)>0 %then %do; 
              weight &weight; 
            %end;
            %if %length(&strata)>0 %then %do; 
              strata &strata; 
            %end;
            %if %length(&cluster)>0 %then %do; 
              cluster &cluster; 
            %end;
            domain __domain('1');
            class __class;
            model log_&var = __class/vadjust=none;
            ods output Effects=&var;
        run;
    
        data &var;
            set &var;
            if __domain=1 and upcase(Effect)="__CLASS";
            VarName=upcase("&var.");
        run;
        
        proc datasets lib=work nolist nodetails nowarn;
            delete _templog_;
        run; quit;
        %let min= ;            

    %end;
    
    /*Save file**/
    data _ttest2_;
       informat VarName $256.;
       set &con3;
       pval=ProbF;
       keep VarName pval;
    run;
    
    /**Delete temp dbs**/
    proc datasets library=work nolist nodetails nowarn;
        delete &con3;
    run;

%end; %*end to if class>0 and pvalues=1;

%*****************************************************************************;
%* SUMMARIZE CAT1 VARIABLES                                                   ;
%*****************************************************************************;
%if %length(&cat1)>0 %then %do;
    
    %put ****** SUMMARIZING CAT1 ******;

    proc surveyfreq data=_tempd_;
        %if %length(&weight)>0 %then %do; 
          weight &weight; 
        %end;
        %if %length(&strata)>0 %then %do; 
          strata &strata; 
        %end;
        %if %length(&cluster)>0 %then %do; 
          cluster &cluster; 
        %end;
        table __domain*__class*(&cat1)
            / &pcttype cl(type=&cltype) alpha=&alpha
            %if %length(&class)>0 %then %do; chisq chisq1 %end; ;
        ods output crosstabs=_cat1_ 
           %if %length(&class)>0 and &pvalues eq 1 and &oddsratios eq 0 %then %do; 
             chisq=_chisq1r_ chisq1=_chisq1mr_
           %end; ;      
    run;
       
    %* Manage output;
    data _keepcat1_ %do i=1 %to &NGroups; _keepcat1_&i %end; ;
        informat VarName $256.;
        set _cat1_;
        where __domain=1 and _SkipLine ne "1";
        
        Varname=upcase(compress(scan(Table, 2, "*")));
                    
        %* only want to keep the var=1 line;
        array yn &cat1; 
        do over yn;
            if yn in (0 2) then delete;
        end;
        
        %* for total I need the Percent;
        if &NGroups gt 1 and __class=. then do;
            &PctType.Percent=Percent;
            &PctType.StdErr=StdErr;
            &PctType.LowerCL=LowerCL;
            &PctType.UpperCL=UpperCL;
        end;            
      
        keep Table Varname Frequency /*WgtFreq*/ &PctType.Percent 
        &PctType.StdErr &PctType.LowerCL &PctType.UpperCL;

        if (&NGroups gt 1 and __class=.) or (&NGroups eq 1 and __class=1)
          then output _keepcat1_;        
        %do i=1 %to &NGroups;
            if __class=&&loop&i then output _keepcat1_&i;
        %end;
    run;
    
    %* combine into 1 file;
    proc sort data=_keepcat1_; by Table VarName; run;
    %do i=1 %to &NGroups;
        proc sort data=_keepcat1_&i; by Table varname; run;
    %end;
    data _outcat1_ (drop = Table);
        merge _keepcat1_(rename=(Frequency=_n &PctType.Percent=_m &PctType.StdErr=_s 
          &PctType.LowerCL=_lcl &PctType.UpperCL=_ucl))
            %do i=1 %to &NGroups; 
                _keepcat1_&i(rename=(Frequency=_n&i &PctType.Percent=_m&i &PctType.StdErr=_s&i 
                &PctType.LowerCL=_lcl&i &PctType.UpperCL=_ucl&i)) 
            %end; ;
        by Table VarName;
        type="CAT1";
    run;
    
    %* select correct p-values;
    %if %length(&class)>0 and &pvalues eq 1 and &oddsratios eq 0 %then %do;
        proc sql;
            create table _tabname1_ as 
            select unique(Table) 
            from _keepcat1_;
        quit;            
        
        proc sql;
            create table _chisq1_ as select
            a.Table, upcase(compress(scan(a.Table, 2, "*"))) as Varname,            
            case
              when not missing(b.nvalue1) then b.nvalue1
              else c.nvalue1
              end as pval
            from _tabname1_ a 
            left join _chisq1r_ (where = (name1="P_RSCHI")) b 
            on compress(a.Table)=compress(b.Table)  
            left join _chisq1mr_ (where = (name1="P_RSCHI")) c 
            on compress(a.Table)=compress(c.Table);
        quit;
    %end;
    
    %* clear temp dbs;
    %if &debug ne 1 %then %do;
        proc datasets lib=work nolist nodetails nowarn;
            delete _cat1_ _keepcat1_
              %do i=1 %to &NGroups; _keepcat1_&i %end; 
              _chisq1r_ _chisq1mr_ _tabname1_;
        run;
        quit;
    %end; 

%end; %* end to if cat1;

%*****************************************************************************;
%* SUMMARIZE CAT2 VARIABLES                                                   ;
%*****************************************************************************;
%if %length(&cat2)>0 %then %do;
    
    %put ****** SUMMARIZING CAT2 ******;
    
    proc surveyfreq data=_tempd_;
        %if %length(&weight)>0 %then %do; 
          weight &weight; 
        %end;
        %if %length(&strata)>0 %then %do; 
          strata &strata; 
        %end;
        %if %length(&cluster)>0 %then %do; 
          cluster &cluster; 
        %end;
        table __domain*__class*(&cat2)
            / &pcttype cl(type=&cltype) alpha=&alpha
            %if %length(&class)>0 %then %do; chisq chisq1 %end; ;
        ods output crosstabs=_cat2_ 
           %if %length(&class)>0 and &pvalues eq 1 and &oddsratios eq 0 %then %do;
             chisq=_chisq2r_ chisq1=_chisq2mr_
           %end; ;
    run;
    
    %* Manage output;
    data _keepcat2_ %do i=1 %to &NGroups; _keepcat2_&i %end; ;
        informat VarName VarLevel $256.;
        set _cat2_;
        where __domain=1 and _SkipLine ne "1";
        
        Varname=upcase(compress(scan(Table, 2, "*")));
                    
        **Create labels for formats;
        %do i=1 %to &ncat2;
            if F_&&nomcat&i ne " " then do;
                VarLevel=".    "||left(trim(F_&&nomcat&i));
                _levelorder=(_n_)/100;
            end;            
        %end;

        %* for total I need the Percent;
        if &NGroups gt 1 and __class=. then do;
            &PctType.Percent=Percent;
            &PctType.StdErr=StdErr;
            &PctType.LowerCL=LowerCL;
            &PctType.UpperCL=UpperCL;
        end;            
      
        keep Table Varname VarLevel Frequency /*WgtFreq*/ &PctType.Percent 
        &PctType.StdErr &PctType.LowerCL &PctType.UpperCL _levelorder;
        
        if (&NGroups gt 1 and __class=.) or (&NGroups eq 1 and __class=1)
          then output _keepcat2_;
        %do i=1 %to &NGroups;
            if __class=&&loop&i then output _keepcat2_&i;
        %end;
    run;
    
    %* combine into 1 file;
    proc sort data=_keepcat2_; by Table VarName VarLevel; run;
    %do i=1 %to &NGroups;
        proc sort data=_keepcat2_&i; by Table varname varlevel; run;
    %end;
    data _outcat2_ (drop = Table);
        merge _keepcat2_(rename=(Frequency=_n &PctType.Percent=_m &PctType.StdErr=_s 
          &PctType.LowerCL=_lcl &PctType.UpperCL=_ucl))
            %do i=1 %to &NGroups; 
                _keepcat2_&i(rename=(Frequency=_n&i &PctType.Percent=_m&i &PctType.StdErr=_s&i 
                &PctType.LowerCL=_lcl&i &PctType.UpperCL=_ucl&i) drop=_levelorder) 
            %end; ;
        by Table VarName VarLevel;
        type="CAT2";
    run;
    
    %* Add row where label and p-value will be;
    proc sort data=_keepcat2_1; by Varname; run;
    data _lab_;
        set _keepcat2_1;
        by varname;
        if first.varname;
        type="CAT2";
        _levelorder=0;
        keep varname type _levelorder;
    run;
    data _outcat2_;
        set _outcat2_ _lab_;
    run;
    proc sort data=_outcat2_; by varname _levelorder; run;
    
    %* select correct p-values;
    %if %length(&class)>0 and &pvalues eq 1 and &oddsratios eq 0 %then %do;
        proc sql;
            create table _tabname2_ as 
            select unique(Table) 
            from _keepcat2_;
        quit;            
        
        proc sql;
            create table _chisq2_ as select
            upcase(compress(scan(a.Table, 2, "*"))) as Varname,
            case
              when not missing(b.nvalue1) then b.nvalue1
              else c.nvalue1
              end as pval
            from _tabname2_ a 
            left join _chisq2r_ (where = (name1="P_RSCHI")) b 
            on compress(a.Table)=compress(b.Table)  
            left join _chisq2mr_ (where = (name1="P_RSCHI")) c 
            on compress(a.Table)=compress(c.Table);
        quit;        
    %end;
    
    %* clear temp dbs;
    %if &debug ne 1 %then %do;
        proc datasets lib=work nolist nodetails nowarn;
            delete _cat2_ _keepcat2_
              %do i=1 %to &NGroups; _keepcat2_&i %end; _lab_
              _chisq2r_ _chisq2mr_ _tabname2_;
        run;
        quit;
    %end; 

%end; %* end to if cat2;

%*****************************************************************************;
%* IF ODDSRATIOS=1, GET OR, CI AND P-VALUES                                   ;
%*****************************************************************************;
%if &oddsratios eq 1 %then %do;

    %put ****** RUNNING LOGISTIC FOR ORS (CON1 AND CON3)******;
   
    %* data set in which to store ors;
    options varinitchk = nonote;
    data _oddsratios_;
        informat VarName VarLevel $256. OddsRatioEst LowerCL UpperCL pval best.;
    run;
    %if &debug eq 1 %then %do; options varinitchk = warn; %end;

        
    %if &ncon gt 0 %then %do i=1 %to &ncon; 
    
        %let var=%scan(&con1 &con3, &i);
   
        proc surveylogistic data=_tempd_ alpha=&alpha namelen=200 nomcar;
            %if %length(&weight)>0 %then %do; 
              weight &weight; 
            %end;
            %if %length(&strata)>0 %then %do; 
              strata &strata; 
            %end;
            %if %length(&cluster)>0 %then %do; 
              cluster &cluster; 
            %end;
            domain __domain('1');
            model __class(order=internal desc) = &var /vadjust=none;
            ods output ParameterEstimates=_pe_ OddsRatios=_or_;
        run;    
        
        proc sql;
            create table _orp_ as select
            upcase(Variable) as VarName, 
            OddsRatioEst, LowerCL, UpperCL, probt  as pval
            from _or_ a inner join _pe_ b
            on Variable=Effect and a.__domain=b.__domain
            where a.__domain=1;
        
        data _oddsratios_;
            set _oddsratios_ _orp_;
        run;
        
        proc datasets lib=work nolist nodetails nowarn;
            delete _pe_ _or_ _orp_;
        run; quit;
        
    %end; %*end to i to ncon;
    
    %put ****** RUNNING LOGISTIC FOR ORS (CAT1)******;
    %* want to allow for 1=yes/2=no and 1=yes/0=no coding;
    %* because of labeling it will be easier to handle separately from cat2;
    
    %if &ncat1 gt 0 %then %do i=1 %to &ncat1;
        
        %let var=%scan(&cat1, &i);
          
        proc surveylogistic data=_tempd_ alpha=&alpha namelen=200 nomcar;
            %if %length(&weight)>0 %then %do; 
              weight &weight; 
            %end;
            %if %length(&strata)>0 %then %do; 
              strata &strata; 
            %end;
            %if %length(&cluster)>0 %then %do; 
              cluster &cluster; 
            %end;
            domain __domain('1');
            class &var/order=internal ref=first param=ref;
            model __class(order=internal desc) = &var /vadjust=none;
            ods output ParameterEstimates=_pe_ OddsRatios=_or_;
        run;    
        
        proc sql;
            create table _orp_ as select
            upcase(Variable) as VarName, 
            OddsRatioEst, LowerCL, UpperCL, probt  as pval
            from _or_ a inner join _pe_ b
            on Variable=scan(Effect, 1) and a.__domain=b.__domain
            where a.__domain=1;
        
        data _oddsratios_;
            set _oddsratios_ _orp_;
        run;
        
        proc datasets lib=work nolist nodetails nowarn;
            delete _pe_ _or_ _orp_;
        run; quit;
        
    %end; %*end to i to ncat1;
    
    %put ****** RUNNING LOGISTIC FOR ORS (CAT2)******;  
    %if &ncat2 gt 0 %then %do i=1 %to &ncat2;
        
        %let var=%scan(&cat2, &i);
        
        proc surveylogistic data=_tempd_ alpha=&alpha namelen=200 nomcar;
            %if %length(&weight)>0 %then %do; 
              weight &weight; 
            %end;
            %if %length(&strata)>0 %then %do; 
              strata &strata; 
            %end;
            %if %length(&cluster)>0 %then %do; 
              cluster &cluster; 
            %end;
            domain __domain('1');
            class &var/order=internal ref=first param=ref;
            model __class(order=internal desc) = &var /vadjust=none;
            ods output ParameterEstimates=_pe_ OddsRatios=_or_;
        run;    
        
        proc sql;
            create table _orp_ as select
            upcase(Variable) as VarName, 
            ".    "||left(trim(ClassVal0)) as VarLevel,
            OddsRatioEst, LowerCL, UpperCL, probt  as pval
            from _or_ a inner join _pe_ b
            on Variable=scan(Effect, 1) and a.__domain=b.__domain and
            left(trim(tranwrd(substr(Effect,1,index(Effect,"vs") - 1), 
            scan(Effect, 1), "")))=ClassVal0
            where a.__domain=1;
        
        data _oddsratios_;
            set _oddsratios_ _orp_;
        run;
        
        proc datasets lib=work nolist nodetails nowarn;
            delete _pe_ _or_ _orp_;
        run; quit;
    
    %end; %*end to i to ncat2;
    
    %*remove empty row;
    data _oddsratios_;
        set _oddsratios_;
        if _n_>1;
    run;
      
    %* label for CL;
    %let cllab=%sysevalf((1-&alpha)*100);
    
%end; %*end to if oddsratios=1;

%*****************************************************************************;
%* STORE ALL OUTPUT IN _OUT_                                                  ;
%*****************************************************************************;
%put ****** CREATING _OUT_ DB ******;

%* combine summary stats;
data _summ_;
    informat VarName $256.;
    set 
      %if %length(&con1)>0 %then %do; _outcon1_ %end;
      %if %length(&con3)>0 %then %do; _outcon3_ %end;
      %if %length(&cat1)>0 %then %do; _outcat1_ %end;
      %if %length(&cat2)>0 %then %do; _outcat2_ %end; ;
run;      

%* combine p-values;
%if %length(&class)>0 and &pvalues eq 1 and &oddsratios eq 0 %then %do;
    data _pval_;
        informat VarName $256.;
        set 
          %if %length(&con1)>0 %then %do; _ttest1_ %end;
          %if %length(&con3)>0 %then %do; _ttest2_ %end;
          %if %length(&cat1)>0 %then %do; _chisq1_ %end;
          %if %length(&cat2)>0 %then %do; _chisq2_ %end; ;
    run; 
%end;
    
%* merge all files;
proc sort data=_summ_; 
    by varname 
      %if %length(&cat2)>0 %then %do; varlevel %end; ; 
run;

*% if ors requested, merge _summ_ and _oddsratios 1st;
%if &oddsratios eq 1 %then %do;
    proc sort data=_oddsratios_; 
        by varname 
          %if %length(&cat2)>0 %then %do; varlevel %end; ;  
    run;
    data _summ_;
        merge _summ_ _oddsratios_;
        by varname 
          %if %length(&cat2)>0 %then %do; varlevel %end; ; 
    run; 
%end;

%if %length(&class)>0 and &pvalues eq 1 and &oddsratios eq 0 %then %do;
    proc sort data=_pval_; by varname; run;
%end;

data &out;
    informat VarName $256. 
        %if %length(&cat2)>0 %then %do; VarLabel &lablen %end; ;
   
    retain VarName VarLabel
        %if &NGroups gt 1 %then %do;
           _Nmiss _n _m _s _lcl _ucl
           %if %length(&con3)>0 %then %do; _t %end; 
        %end;
        %do i=1 %to &NGroups; 
            _Nmiss&i _n&i _s&i _m&i _lcl&i _ucl&i 
            %if %length(&con3)>0 %then %do; _t&i %end; 
        %end;
        %if &oddsratios=1 %then %do;
            OddsRatioEst LowerCL UpperCL
        %end;
        %if %length(&class)>0 and &pvalues eq 1 %then %do;
            p pval 
        %end;        
        type _order _levelorder;
    
    merge _outds_ _summ_ 
        %if %length(&class)>0 and &pvalues eq 1 and &oddsratios eq 0 %then %do; _pval_ %end;;
   
    by varname;   
    
    %if %length(&class)>0 and &pvalues=1 %then %do;
        p=pval;
    %end;     
    
    if type ne "CAT2" then do;
        _n=_NObs;
        %do i=1 %to &NGroups; _n&i=_nobs&i; %end;
        _levelorder=0;
    end;   
    
    if type="CAT2" and _levelorder>0 then do;
        VarLabel=VarLevel;
        _NObs=.;
        _Nmiss=.;
        %do i=1 %to &NGroups;
            _NObs&i=.;
            _Nmiss&i=.;        
        %end;
        %if %length(&class)>0 and &pvalues=1 and &oddsratios eq 0 %then %do; 
            p=.; 
        %end;
    end;   
    if type="CAT2" and _levelorder=0 and &oddsratios=1 then p=.;
    
    if upcase(type) in ("CON1" "CON2" "CON3") then do;
        _n = _NObs;
        %do i=1 %to &NGroups; _n&i=_NObs&i; %end;
    end;
    
    %if %length(&cat2)>0 %then %do;
    drop VarLevel;      
    %end;
run;    
proc sort data=&out; by _order _levelorder; run;  

%if &debug eq 1 %then %do;
    title3 "PROC PRINT: %upcase(&out)";
    proc print data=&out noobs;
    run;
    title3;
%end;

%if &debug ne 1 %then %do;
    proc datasets lib=work nolist nodetails nowarn;
        delete _outds_ _outcon1_ _outcon3_ _outcat1_ _outcat2_
             _ttest1_ _ttest2_ _chisq1_ _chisq2_ _summ_ _oddsratios_ _pval_;
    run;
    quit;
%end;    

%*****************************************************************************;
%* PREPARE RTF DATA FOR REPORT                                                ;
%*****************************************************************************;
%put ****** CREATING _RTF_ DB ******;

%let cl=%sysevalf((1-&alpha)*100);
/* %let PlMi=%Str(±); */
%let PlMi=(*ESC*){unicode 00B1};

data &rtfout;
    set &out;
        
    %*descritive labels;
    informat DescLabel $256.;
    if type="con3" then DescLabel=left(trim(VarLabel))||", median [Q1, Q3]";
    else if type in ("CAT1" "CAT2") and _levelorder gt 0 then DescLabel=VarLabel;
    else do;
        %if %upcase(&secl) eq CL %then %do;
            if type="CON1" then DescLabel=left(trim(VarLabel))||", mean (&cl.% CI)";
            else if type in ("CAT1" "CAT2") and _levelorder eq 0  then 
                DescLabel=left(trim(VarLabel))||", % (&cl.% CI)";
        %end;
        %if %upcase(&secl) eq SE %then %do;
            %if &seopt eq 1 %then %do;
                if type="CON1" then 
                    DescLabel=left(trim(VarLabel))||", mean &PlMi. se ";
                else if type in ("CAT1" "CAT2") and _levelorder eq 0 then 
                    DescLabel=left(trim(VarLabel))||", % &PlMi. se";
            %end; 
            %if &seopt eq 2 %then %do;
                if type="CON1" then 
                    DescLabel=left(trim(VarLabel))||", mean (se) ";
                else if type in ("CAT1" "CAT2") and _levelorder eq 0 then 
                    DescLabel=left(trim(VarLabel))||", % (se)";
            %end;                             
        %end;            
    end;
    
    %*flag label if any missing values;
    if _nmiss gt 0 then do;
        VarLabel2=left(trim(VarLabel))||"*";
        DescLabel2=left(trim(DescLabel))||"*";
    end;
    else if _nmiss le 0 then do;
        VarLabel2=left(trim(VarLabel));
        DescLabel2=left(trim(DescLabel));
    end;   
            
    %*Summary stats - Overall;
    %if &NGroups gt 1 and &totalcol eq 1 %then %do;
        informat Overall $56.;
        if type="con3" then 
            Overall=compress(put(_m, &f2))||" ["||
              compress(put(_s, &f2))||", "||
              compress(put(_t, &f2))||"]";
        else  do;
            %if %upcase(&secl) eq CL %then %do;
                Overall=compress(put(_m, &f2))||" ("||
                  compress(put(_lcl, &f2))||", "||
                  compress(put(_ucl, &f2))||")";            
            %end;
            %else %if %upcase(&secl) eq SE %then %do;
                %if &seopt eq 1 %then %do;
                    Overall=compress(put(_m, &f2))||" &PlMi. "||
                        compress(put(_s, &f2));
                %end;
                %else %if &seopt eq 2 %then %do;
                    Overall=compress(put(_m, &f2))||" ("||
                        compress(put(_s, &f2))||")";                
                %end;            
            %end;           
        end; 
        if type="CAT2" and _levelorder=0 then Overall="";          
    %end;
    
    %*Summary  stats - By Group;
    %do i=1 %to &NGroups;
        informat A&i $56.;
        if not missing(_m&i) then do;
        if type="con3" then 
            A&i=compress(put(_m&i, &f2))||" ["||
              compress(put(_s&i, &f2))||", "||
              compress(put(_t&i, &f2))||"]";
        else  do;
            %if %upcase(&secl) eq CL %then %do;
                A&i=compress(put(_m&i, &f2))||" ("||
                  compress(put(_lcl&i, &f2))||", "||
                  compress(put(_ucl&i, &f2))||")";            
            %end;
            %else %if %upcase(&secl) eq SE %then %do;
                %if &seopt eq 1 %then %do;
                    A&i=compress(put(_m&i, &f2))||" &PlMi. "||
                        compress(put(_s&i, &f2));
                %end;
                %else %if &seopt eq 2 %then %do;
                    A&i=compress(put(_m&i, &f2))||" ("||
                        compress(put(_s&i, &f2))||")";                
                %end;            
            %end;          
        end;
        end;
        if type="CAT2" and _levelorder=0 then A&i="";
        if type="CAT1" and _n&i gt 0 and missing(A&i) then A&i="0";
        if type="CAT2" and _n&i eq 0 and missing(A&i) then A&i="0";
    %end;
    
    %*Odds ratios;
    %if &oddsratios eq 1 %then %do;
        informat OR_CL $40.;
        if OddsRatioEst ne . then 
        OR_CL=compress(put(OddsRatioEst, &orfmt))||" ("||
            compress(put(LowerCL, &orfmt))||", "||
            compress(put(UpperCL, &orfmt))||")";
        if type="CAT2" and _levelorder ne 0 and oddsratioest=.
          then OR_CL="reference";            
    %end;
    
    %*p-value with superscript; 
    %if &NGroups gt 1 and &pvalues eq 1 and &oddsratios eq 0 %then %do;
        %if &printptype eq 1 %then %do;
            if type="CON1" then p2=compress(put(p, &pfmt))||"^{super a}";
            else if type="con3" then p2=compress(put(p, &pfmt))||"^{super b}";
            else if type in ("CAT1" "CAT2") and _levelorder eq 0 then 
                p2=compress(put(p, &pfmt))||"^{super c}";
            if (type in ("CON1" "CON3") or (type in ("CAT1" "CAT2") and _levelorder eq 0)) and missing(p) 
                then p2 = "---";                
        %end;
        format p &pfmt;
    %end;
    
    %* empty col for report;
    _empty = "";
    
    keep varname varlabel _Nmiss _n 
        %if &NGroups gt 1 and &totalcol eq 1 %then %do; Overall %end;
        %do i=1 %to &NGroups; _Nmiss&i _n&i A&i %end;
        %if &oddsratios eq 1 %then %do; or_cl %end;
        %if %length(&class)>0 and &pvalues eq 1 %then %do;
            p 
            %if &printptype eq 1 and &oddsratios eq 0 %then %do; p2 %end;
            pval 
        %end;        
        type _order _levelorder desclabel varlabel2 desclabel2 _empty;
run;

%* sort;
proc sort data=&rtfout;
    by 
      %if %length(&sortby)=0 or %upcase(&sortby)=LIST %then %do; _order %end;
      %else %do; &sortby %end;
      _levelorder ;
run;

%*****************************************************************************;
%* GENERATE FOOTNOTE FOR MISSING VALUES IF NCOL=0 AND MISSCOL=0               ;
%*****************************************************************************;
%if &ncol eq 0 and &misscol eq 0 %then %do;

    %put ****** CREATING MISSING VALUES FOOTNOTE ******;
   
    proc sql noprint;
        select trim(varlabel)||" = "||compress(put(_Nmiss, comma32.))
        into :MissNote separated by '; ' 
        from &rtfout
        where _nmiss gt 0;
    %if %symexist(missnote) eq 1 %then %do;
        %let MissNote=&MissNote; %*remove trailing blanks;
    %end;   
    
%end; %*end to if ncol=0 and miscol=0;

%*****************************************************************************;
%* GENERATE REPORT                                                            ;
%*****************************************************************************;
%put ****** CREATING REPORT TABLE ******;

%* return printing to window in SAS Studio;
%* will not return in interactive sas because PROC REPORT generates an error
   about a column width (that does not affect output);
ods select all;
ods listing close;

%* escape char;
ods escapechar="^";

*% clear date stamp if requested;
%if &daytime=0 %then %do;
    options nodate;
%end;
options nonumber nocenter noquotelenmax;

%* Define column widths;
%if %length(&cwidth3)=0 %then %do;
    %if &desclabel=1 and %length(&con3)>0 %then %let cwidth3=375;
    %else %if &desclabel=1 and %length(&con3)=0 %then %let cwidth3=350;
    %else %let cwidth3=300;
%end;  
%if %length(&cwidth2)=0 %then %do;  
    %if %upcase(&secl)=CL %then %let cwidth2=200;
    %else %let cwidth2=175;
%end;

%* if no title given, generic one;
%if %length(&tbltitle)=0 %then 
  %let tbltitle="SVYTABLE OUTPUT";

%* main title/footnote location;
%if &bdytitle=1 %then %let bd=bodytitle;
%else %let bd=;

%* Change margins;
ods path reset;
ODS path (REMOVE) WORK.TEMPLAT;
ODS path (PREPEND) work.TEMPLAT(update);

proc template;
    define style my&style;
    parent=styles.&style;
    style body from document /
        leftmargin=0.5in
        rightmargin=0.5in
        topmargin=0.5in
        bottommargin=0.5in;
    end;
run;

%* ODS destinatin;
%if %length(&rtffile) gt 0 %then %do;
    
    %* Paper orientation;
    options orientation=&page;
    
    %* Open RTF destination;
    ods rtf file=&RTFFILE  style=My&STYLE &BD;
    
%end;

%* Report;
proc report data= &RTFOUT nowd
    style(report)={borderwidth=3 bordercolor=black cellpadding=3
        %if %length(&outputwidth) ne 0 %then %do; outputwidth=&outputwidth.% %end;
        font_size=11pt font_face=Times  FONTSTYLE= ROMAN}

    style(lines)={background=white foreground=black
        font_size=9pt font_face=Times FONTSTYLE= ROMAN
        protectspecialchars=off}

    style(column)={background=white foreground=black
        font_size=11pt font_face=Times FONTSTYLE= ROMAN
        font_weight=medium}

    style(header)={background=white foreground=black borderbottomstyle=double
        font_weight=bold FONTSTYLE= ROMAN
        font_size=11pt font_face=Times};
    
    columns (
      /* Label */
      ('^S={borderbottomcolor=white}'
        %if &ncol eq 0 and &misscol eq 0  and &desclabel=1 %then %do;
          desclabel2
        %end;
        %else %if &ncol eq 0 and &misscol eq 0 and &desclabel=0 %then %do;
          varlabel2
        %end;
        %else %if (&ncol ne 0 or &misscol ne 0) and &desclabel=1 %then %do;
          desclabel
        %end;
        %else %if (&ncol ne 0 or &misscol ne 0) and &desclabel=0 %then %do;
          varlabel
        %end; )
      /* Overall columns */
      %if &NGroups gt 1 and &totalcol eq 1 and &ncol in (0 2) and  &misscol in (0 2)
       %then %do;
        ('^S={borderbottomcolor=white}' Overall) %end;          
      %if &NGroups gt 1 and &totalcol ne 1 and (&ncol in (1 3) or  &misscol in (1 3))
       %then %do;
        ('^S={borderbottomcolor=white}'
          %if &ncol in (1 3) %then %do; _n %end;
          %if &misscol in (1 3) %then %do; _nmiss %end;
          ) %end;
      %if &NGroups gt 1 and &totalcol eq 1 and (&ncol in (1 3) or  &misscol in (1 3))
       %then %do;
        ("^S={borderbottomcolor=black borderbottomwidth=1} Overall/(N=&TOTALNLAB.)"
          %if &ncol in (1 3) %then %do; _n %end;
          %if &misscol in (1 3) %then %do; _nmiss %end;
          Overall) 
          ('^S={borderbottomcolor=white}' _empty)
          %end;
      /*By groups */
      %do i=1 %to &NGroups;
        %if &ncol in (0 1) and &misscol in (0 1) %then %do;
          ('^S={borderbottomcolor=white}' A&i) %end;
        %if &ncol in (2 3) or &misscol in (2 3) %then %do;
          ("^S={borderbottomcolor=black borderbottomwidth=1} &&GroupLab&i./(N=&&GroupN&i.)"
          %if &ncol in (2 3) %then %do; _n&i %end;
          %if &misscol in (2 3) %then %do; _nmiss&i %end;
          A&i) 
          ('^S={borderbottomcolor=white}' _empty)
          %end;  
      %end; %* end to i to ngroups;
      /*OR*/
      %if &oddsratios eq 1 %then %do;
        ('^S={borderbottomcolor=white}' or_cl) 
      %end;        
      /* p-value */
      %if &NGroups gt 1 and &pvalues eq 1 %then %do;
        ('^S={borderbottomcolor=white}' 
          p
          %if &printptype eq 1 and &oddsratios eq 0 %then %do; p2 %end;
          _pvalue) %end;
      );
    
    %* Title;
    compute before _PAGE_ /style = {font_size=11pt font_face=Times
      FONTSTYLE=ROMAN font_weight=bold
      just=left borderbottomwidth=3
      borderbottomcolor=black bordertopcolor=white};
        line &TBLTITLE;
    endcomp;
                
    %* Label columns;
    %if &ncol eq 0 and &misscol eq 0 %then %do;
        
        %if &desclabel eq 1 %then %do;
            define desclabel2 /order = data "Factor"  left
              style(header) = {just = left}
              style(column) = {cellwidth = &CWIDTH3
              %if &boldlabel=1 %then %do; font_weight=bold %end; };        
        %end;

        %if &desclabel ne 1 %then %do;
            define varlabel2 /order = data "Factor"  left
              style(header) = {just = left}
              style(column) = {cellwidth = &CWIDTH3
              %if &boldlabel=1 %then %do; font_weight=bold %end; };        
        %end;        

    %end; %* end to ncol=0 and nmiss=0;
    
    %else %if &ncol ne 0 or &misscol ne 0 %then %do;

        %if &desclabel eq 1 %then %do;
            define desclabel /order = data "Factor"  left
              style(header) = {just = left}
              style(column) = {cellwidth = &CWIDTH3
              %if &boldlabel=1 %then %do; font_weight=bold %end; };        
        %end;

        %if &desclabel ne 1 %then %do;
            define varlabel /order = data "Factor"  left
              style(header) = {just = left}
              style(column) = {cellwidth = &CWIDTH3
              %if &boldlabel=1 %then %do; font_weight=bold %end; };        
        %end;        

    %end; %* end to else if ncol ne 0 or miss ne 0;    
 
    %* Overall n, n miss, summary columns;
    %if &NGroups gt 1 and (&totalcol eq 1 or &ncol in (1 3) or  &misscol in (1 3)) %then %do;    
        
        %if &totalcol ne 1 and &ncol in (1 3) %then %do;  
            define _n / "Overall N" format=&f1
              style(column)={just = center vjust=center cellwidth = &CWIDTH1};
        %end;
       
        %if &totalcol ne 1 and &misscol in (1 3) %then %do; 
            define _nmiss / "Overall N missing" format=&f1
              style(column)={just = center vjust=center cellwidth = &CWIDTH1};        
        %end;
       
        %if &totalcol eq 1 and &ncol in (0 2) and &misscol in (0 2) %then %do; 
            define Overall / display "Overall/(N=&TOTALNLAB.)"
              style(column)={just = center vjust=center cellwidth = &CWIDTH2};        
        %end;
        
        %if &totalcol eq 1 and (&ncol in (1 3) or  &misscol in (1 3)) %then %do;
            %if &ncol in (1 3) %then %do;
              define _n / "N" format=&f1
                style(column)={just = center vjust=center cellwidth = &CWIDTH1};            
            %end;
            %if &misscol in (1 3) %then %do;
              define _nmiss / "N missing" format=&f1
                style(column)={just = center vjust=center cellwidth = &CWIDTH1};            
            %end;            
            define Overall / display "Statistics"
              style(column)={just = center vjust=center cellwidth = &CWIDTH2};   
            define _empty / ' ' style(column)={cellwidth=1%};       
        %end;
        
    %end; %* end to overall section;
    
    %* N, n miss, summary by group;
    %do i=1 %to &NGroups;
       
        %if &ncol in (0 1) and &misscol in (0 1) %then %do;
            define A&i / display "&&GroupLab&i./(N=&&GroupN&i.)"
              style(column)={just = center vjust=center cellwidth = &CWIDTH2
              protectspecialchars=off};        
        %end;
        
        %if &ncol in (2 3) or &misscol in (2 3) %then %do;
            %if &ncol in (2 3) %then %do;
              define _n&i / "N" format=&f1
                style(column)={just = center vjust=center cellwidth = &CWIDTH1};            
            %end;
            %if &misscol in (2 3) %then %do;
              define _nmiss&i / "N missing" format=&f1
                style(column)={just = center vjust=center cellwidth = &CWIDTH1};            
            %end;          
            define A&i / display "Statistics"
              style(column)={just = center vjust=center cellwidth = &CWIDTH2
              protectspecialchars=off}; 
            define _empty / ' ' style(column)={cellwidth=1%};
        %end;        
    
    %end; %* end to i to NGroups;
    
    %* Odds Ratio column;
    %if &oddsratios eq 1 %then %do;
        define or_cl/display "OR (&CLLAB.% CI)"
          style(column)={just = center vjust=center cellwidth = &CWIDTH2
          protectspecialchars=off}; 
    %end; %* end to odds ratio column;
    
    %* P-value column;
    %if &NGroups gt 1 and &pvalues eq 1 %then %do;

        define p/display noprint;
        
        %if &printptype eq 1 and &oddsratios eq 0 %then %do;
          define p2/display noprint;
        %end;
        
        define  _pvalue / computed "p-value"
          %if &ncol ne 0 or &misscol ne 0 %then %do;
            style(header)={bordertopcolor=white}
          %end;
          style(column) = {just = center vjust=center cellwidth = &CWIDTH1};

        compute _pvalue/character length=16;
            %if &printptype eq 1 and &oddsratios eq 0 %then %do;
                _pvalue=p2;
            %end;
            %else %do;
                _pvalue=compress(put(p, &pfmt));
            %end;                
            %if &boldsigp eq 1 %then %do;
                if 0 le p lt &alpha then call define(_col_,
                "style", "style=[font_weight=bold fontstyle=italic]");
            %end;
        endcomp;
            
    %end; %* end to pval section;
    
    %* Footnotes;
    compute after/style(lines)={just=left
      borderbottomcolor=white bordertopcolor=black bordertopwidth=3};
        
        %if %symexist(missnote) eq 1 %then %do;
            line "*Data not available for all subjects. &MissNote..";
        %end;
    
        line "Frequencies presented are unweighted counts.";
        
        %if &desclabel ne 1 %then %do;
          %if %upcase(&secl)=SE %then %do;
            %if &seopt=1 %then %do;
              line "Statistics presented as Mean or Percent &plmi. standard error or Median [Q1, Q3].";
            %end;
            %else %if &seopt=2 %then %do;
              line "Statistics presented as Mean or Percent (standard error) or Median [Q1, Q3].";
            %end;
          %end;
          %else %if %upcase(&secl)=CL %then %do;
            line "Statistics presented as Mean or Percent (&cl.% CI) or Median [Q1, Q3].";
          %end;         
        %end; %*end to desclabel ne 1 footnote;
        
        %if &Ngroups gt 1 and &pvalues eq 1 and &oddsratios eq 0 %then %do;
            %if &printptype eq 1 %then %do;
                line "P-values: a=linear regression; b=linear regression with log transformation; c=Rao-Scott chi-square test.";
            %end;
            %else %if &printptype ne 1 %then %do;
                line "P-values correspond to Rao-Scott chi-square tests for categorical factors and linear regression for continuous variables (a log transformation for non-normally distributed variables).";
            %end;
        %end;
        
        %if &pvalues eq 1 and &oddsratios eq 1 %then %do;
            line "Odds Ratios (OR), Confidence Intervals (CI) and p-values correspond to univariate logistic regression models.";
        %end;
        %if &pvalues eq 0 and &oddsratios eq 1 %then %do;
            line "Odds Ratios (OR) and Confidence Intervals (CI) correspond to univariate logistic regression models.";
        %end;
                            
        %if %length(&addfn)>0 %then %do;
            line &addfn;
        %end;
        
        line "SAS Survey Procedures used for all analyses.";
                    
    endcomp;
            
run;

%* Close ODS destination;
%if %length(&rtffile) gt 0 %then %do;
    ods rtf close;
    options orientation=portrait;
%end;
options quotelenmax;

%*****************************************************************************;
%* MEND MACRO                                                                 ;
%*****************************************************************************;
ods listing;
options nomlogic nomlogicnest varinitchk=note; 
ods path (REMOVE) sasuser.TEMPLAT;
ods path reset;

%if &debug ne 1 %then %do;
    proc datasets lib=work nolist nodetails nowarn;
        delete _tempd_;
    run;
    quit;        
%end;

%mend svytable;