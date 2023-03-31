/**
 @file
 @brief Creates a descriptive table

 @details
 This program summarizes multiple variables for one or more independent 
 samples and generates a report-ready table. 
 In the case of two or more groups, test statistics for 
 differences between groups are included.

 @author Rocio Lopez

 @date 02-04-2019

 @version SAS 9.4

 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %summarize(
    data=, subset=, class=,
    con1=, con2=, con3=, cat1=, cat2=, ord1=,
    sdopt=1, unequalvar=, 
    colpct=1, cutexact=20,
    pvalues=1, alpha = 0.05,  
    adhoc=0, adhoc_method=bon,  
    oddsratios=0, unitscon1=, unitscon2=, unitscon3=,                           
    out=_out_, rtfout=_rtf_,  
    sortby=, list=,
    f1= __Countf., f2= __Estf., f3= __Estf., f4= __Estf., pfmt= Pvaluef.,     
    tbltitle=, totalcol=1, ncol=0, misscol=0, desclabel=0, BoldedLabel=0, 
    printptype=1, boldedsigp=1, daytime=0, printfn=1, addfn=,     
    rtffile=, pdffile=, xlsxfile=, xmlfile=, bdytitle=0, page=portrait,
    cwidth1=, cwidth2=, cwidth3=, fontsize=11, fontface=Times, outputWidth=,    
    debug=0);
 @endcode

 @returns
 SAS output listing (if running in SAS Studio) and data sets of
 summary statistics, as well as RTF, PDF and/or XLSX files, if 
 requested.

 @param data The name of the input dataset to be used

 @param con1 List of continouos variables to summarize as mean 
 +/- SD and compare using t-tes or ANOVA

 @param con2 List of continouos variables to summarize as 
 median (min, max) and compare using Wilcoxon Rand Sum or 
 Krukal-Wallis tests

 @param con3 List of continouos variables to summarize as 
 median [Q1, Q3] and compare using Wilcoxon Rand Sum or 
 Krukal-Wallis tests

 @param cat1 List of binary variables to summarize as n (%)
 and compare using Pearson's chi-square or Fisher's Exact test
 @n Only level=1 will be displayed

 @param cat2 List of nominal variables to summarize as n (%)
 and compare using Pearson's chi-square or Fisher's Exact test
 @n All levels will be displayed
 
 @param ord1 List of ordinal variables to summarize as n (%)
 and compare using Wilcoxon Rand Sum or Krukal-Wallis tests
 
 @param class The variable which defines the groups
 @n Optional; must be a numeric variable and categorical (do not use range formats)

 @param subset Optional expression to be used in a WHERE statement to in order 
 to subset the data set.

 @param sdopt Allows to choose how the standard deviation should be displayed
 @n Allowed options are:
 @n 1 = with +/- sign (DEFAULT) 
 @n 2 = within parentheses

 @param unequalvar Choose what types of tests are performed for variables in CON1 list
 @n Allowed options are:
 @n 0 = Equal variances (t-test or ANOVA) (DEFAULT if CLASS var has 3+ groups)
 @n 1 = Unequal variances (Satterthwaite t-test or Welch's ANOVA)
 @n 2 = Let the macro determine which to use based on tests of equality of 
 variances (DEFAULT if CLASS var has 2 groups)

 @param colpct Choose between column or row percentages for cat1, cat2 or ord1 variables
 @n Allowed options are: 
 @n 0 = Row percent
 @n 1 = Column percent (DEFAULT)

 @param cutexact Fisher's Exact test is performed when the % of cells with expected 
 counts<5 in categorical vars is greater to or equal to CUTEXACT.
 @n Default is 20, value should be between 0 and 100

 @param pvalues Include p-values for group comparisons when 2 or more groups in CLASS var
 @n Allowed options are:
 @n 0 = No, only include summary statistics
 @n 1 = Yes, include p-values (DEFAULT)

 @param alpha Specify overall significance level
 @n This will be used to bold p-values and to calculate post-hoc significance criterion, 
 if applicable
 @n Default is 0.05, value should be between 0 and 0.99

 @param adhoc Perform all possible pairwise post-hoc comparisons when CLASS has 3+ groups
 @n This is only done for variables with a significant overall p-value
 @n Allowed ptions are: 
 @n 1 = Yes, perform pairwise comparisons
 @n 0 = No, do not perform pairwise comparisons (DEFAULT)
 
 @param adhoc_method Method used to adjust pair-wise comparisons
 @n Several options from PROC MULTTEST is allowed, see PROC MULTTEST documentation 
 for details
 @n Resampling-based adjustments are not permitted
 @n Default is: BON

 @param oddsratios Include unadjusted odds ratios when class variable has 2 groups.
 @n These are calculated with CLASS var as the outcome and the following options: 
 order=internal, event descending, ref=first
 @n Allowed options are:
 @n 0 = No, do not include ORs (DEFAULT)
 @n 1 = Yes, include ORs 

 @param unitscon1 If odds ratios requested, specify units for CON1 variables
 @n If you specify 1 single number, it will be used for all variables
 @n If you specify >1 numbers, you must include 1 number per CON1 variable 
 and in the the same order as the CON1 list
 @n Default is 1 for all variables

 @param unitscon2 If odds ratios requested, specify units for CON2 variables
 @n If you specify 1 single number, it will be used for all variables
 @n If you specify >1 numbers, you must include 1 number per CON2 variable 
 and in the the same order as the CON2 list
 @n Default is 1 for all variables

 @param unitscon3 If odds ratios requested, specify units for CON3 variables
 @n If you specify 1 single number, it will be used for all variables
 @n If you specify >1 numbers, you must include 1 number per CON3 variable 
 and in the the same order as the CON3 list
 @n Default is 1 for all variables

 @param out Name for the data set in which the results are stored
 @n Default is _OUT_

 @param rtfout The data set in which results for RTF printing are stored
 @n Default is _RTF_

 @param sortby Choose how to sort rows in final output
 @n Allowed options are:
 @n _list   : Sort by the list given (see LIST)
 @n pval    : Sort by the p-value
 @n varname : Sort by variable names
 @n varlabel: Sort by variable label
 @n <blank> : In input order for CON1, CON2, CON3, CAT1, CAT2, ORD1 (DEFAULT)
 
 @param list List the variables in the order they should be reported when SORTBY=_LIST
 @n If you do not include all variables, the rest will be included in input order
 @n You can include section headers in the LIST parameter
 @n Each section header must start with TblHdr_ and be no more than 25 
 characters in length; 
 use underscore (_) instead of space

 @param f1 Format applied to counts (n) in RTFOUT
 @n Default is __COUNTF.

 @param f2 Format applied to mean, SD, medians, P25, P75, min, max in RTFOUT
 @n Default is __ESTF.

 @param f3 Format applied to percents (underlying value is 0-100) in RTFOUT
 @n Default is __ESTF.
 
 @param f4 Format applied to OR and 95% CI in RTFOUT
 @n Default is __ESTF.

 @param pfmt Format applied to p-values in RTFOUT
 @n Default is PVALUEF.

 @param tbltitle A title for table surronded by double quotes

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

 @param printfn Include default footnotes in table
 @n Allowed options are:
 @n 0 = No, do not include
 @n 1 = Yes, include (DEFAULT)
 
 @param addfn Optional user-defined footnote surrounded by double quotation marks

 @param daytime Add a date/time stamp as a footnote to the ods files.
 @n Allowed options are:
 @n 0 = No date stamp (DEFAULT)
 @n 1 = Yes, include date/time stamp

 @param rtffile Path and filename for optional RTF output file
 @n It should be surrounded in quotes, e.g. "~userid/consult/out.rtf"

 @param pdffile Path and filename for optional PDF output file
 @n It should be surrounded in quotes, e.g. "~userid/consult/out.pdf"
 
 @param xlsxfile Path and filename for optional XLSX output file
 @n It should be surrounded in quotes, e.g. "~userid/consult/out.xlsx"

 @param xmlfile Path and filename for optional XML output file
 @n It should be surrounded in quotes, e.g. "~userid/consult/out.xml"

 @param bdytitle Location of SAS system titles/footnotes on RTF file
 @n Allowed options are:
 @n 0=Put Titles in Header and Footnotes in Footer (DEFAULT) 
 @n 1=Put Main Title and Footnotes in the Body of the RTF file

 @param page Specify page orientation for output files
 @n Allowed options are:
 @n PORTRAIT (DEFAULT)
 @n LANDSCAPE

 @param cwidth1 Column width for N total/N missing/p-value columns
 @n Default is 95 if printptype=0 and 90 otherwise

 @param cwidth2 Column width for summary stats columns
 @n Default is 275 when CON2 or CON3 are used, 175 otherwise

 @param cwidth3 Column width for variable labels
 @n Default is 375 when desclabel=1 and 275 otherwise

 @param fontsize Set font size (pt) used for output report
 @ Default is 11

 @param fontface Set font face used for output report
 @n Any font face supported by SAS can be used, default is Times

 @param outputwidth Set percentage of the page the table occupies in 
 the output report
 @n Value must be 1-100 and if not specified PROC REPORT will decide
 best value

 @note
 @parblock
 @li At least one list of variables (CON1, CON2, CON3, CAT1, CAT2, ORD1) 
 must be provided.
  @li It is a good idea to have all variables formatted, especially the
    CLASS variable
 @li CAT2 may contain SAS numeric or character variables, all other
    variable lists should contain SAS numeric variables.
 @li A variable can occur in more than one display parameter list.
    If doing so, it is recommended you use the DESCLABEL=1 option.
 @li Variables which have all missing values will not be analyzed.
    Notification of this occurrence is sent to the SAS log.
 @li If NTOTALCOL=0, variable labels are flagged if there are missing
    values and a footnote is added stating # missing for each var.
 @li BDYTITLE parameter only works with RTF and XML files
 @li Default is to perform Exact tests for categorical variables if more
    than 50% of class have counts less than 5 but this can be manipulated
    with the CUTEXACT option. To suppress exact tests then set CUTEXACT
    to a value greater than 100. A value of 0 would produce Exact
    tests for all variables.
 @li If no class variable specified, TotalCol is ignored. If you want
    to print the N or N miss columns, use NCOL=2 or MISSCOL=2 otherwise
    it will be ignored. *3/20/2020: Now either 1 or 2 will work.
 @li You can list variables with the X1-X99. The X: or FirstCol -- LastCol 
    syntax are not supported.
 @li You can include section headers in the LIST parameter. These will
    be included in the label column. Each section header must start
    with TblHdr_ and be no more than 25 characters in length.
    Use an underscore (_) instead of a space.
    To insert blank lines use TblHdr__ (2 underscores).
 @li If running on interactive Linux SAS, no output is listed on
    results window. OUT and RTFOUT are generated and if you specify
    an output file that will be created.
 @li Unless running on SAS Studio, output might not look right in results 
    window but styling in RTF file will be fine   
 @li When comparing 3+ groups, it is recommended that the user make
    the decision of which type of ANOVA (equal vs. unequal variances)
    to use. Homogeneity of variance tests have too little power to be
    relied upon to always detect when Welch’s ANOVA is appropriate.
    Unless the group variances are extremely different or the number
    of groups is large, the usual ANOVA test is relatively robust
    when the groups are all about the same size.
    Graphical diagnostics can be a useful informal tool for monitoring
    whether your data meet the assumptions of a GLM analysis.
 @li OUTPUTWIDTH option does not work with PDF and should not be changed if
    requesting a PDF file. 
 @li Data sets (potentially) created:
    _ngroups_ _vtype_ _n_ _order_ _con1_ _tcon1_ _con2_ _tcon2_ _con3_
    _tcon3_ _cat1_ _cat2_ _ord1_ _keepcat_: p_info  _ttest_ _ttest2_ _eqvar_
    _anova_: _welch_: _parp_: _npar_: _fish1-_fish3 _csq_ _fisher_ _chisq_:
    _outds_ _outcon1_ _outcon2_ _outcon3_ _outcat_ _oddsratios_ _summ_  _pval_
    _adhoc_pval_ _adhoc_pval2_ _add_ _tout_ _tempd_ _f
 @li Created data sets that are NOT deleted:  &OUT &RTFOUT
 @endparblock

 @sa outsummarize.sas

 @par Example
 @code{.sas}
 title1 'Sample execution of SUMMTABLE';
 footnote "Randomly generated data";

 proc format;
   value groupf  1="Group A" 2="Group B";
   value x5f     0="Failure" 1="Success";
  run;

 data a;
   do Group=1 to 2;
     do i=1 to 100;
        X1 = rannor(0);
        X2 = 100*ranexp(0);
        X3 = exp(rannor(0));
        X4 = ranbin(0, 1, 0.5);
        X5 = ranbin(0, 1, 0.7);
        X6 = ranbin(0, 4, 0.8);
        output;
      end;
   end;
   label X1="Normal variate"
         X2="Exponential variate"
         X3="Log-normal variate"
         X4="Bernoulli variate"
         X5="Formatted Bernoulli"
         X6="Ordinal variate";
    format group groupf. X5 x5f.;
  run;

 %summarize(data=a
          ,class=group
          ,rtffile="summary.rtf"
          ,page=landscape
          ,con1=x1
          ,con2=x2
          ,con3=x3
          ,cat1=x4
          ,cat2=x5
          ,ord1=x6
          ,sortby=_list
          ,list=x2 x1 x6
          ,tbltitle="Data summaries"
        );
 @endcode
 
 @par Revision History
 @b 03-17-2023 Updated documentation to exclude special characters causing
 issues when running session with ENCODING=WLATIN1 
 @n @b 03-13-2023 Added flow="Tables" option to ODS EXCEL
 @n @b 07-22-2022 Fixed bug incorporated on previous update that was causing
 Fisher's Exact test p-val to be flagged as Pearson's chi-sq
 @n @b 06-29-2022 Class variable can now have >9 levels or be coded with
 more than 1 digit
 @n @b 03-18-2022 Fixed footnotes for p-values so that Fisher's get 
 excluded if not used
 @n @b 04-13-2021 Fixed bug that could prevent ORs of CAT2 variables from 
 being properly displayed
 @n @b 11-16-2020 Fix for running in 9.4M6 or later (ODS output for 
 NPAR1WAY changed)
 @n @b 07-09-2020 Saving OUT or RTFOUT files in a permanent SAS library is 
 now allowed
 @n @b 03-13-2020 Several bugs fixed: 1) Array subscript error when 
 calculating N missing f # vars > # obs. 2) Removed duplicate entries 
 from missing footnote that appeared when listing single var in several
 lists. 3) If no CLASS var given, NCOL and MISSCOLL can now be either 
 1 or 2 to produce columns. 4) If single var specified as both CAT2 
 and ORD1 both labels will now appear.
 @n @b 02-19-2020 Fixed a bug that was causing the user defined section
 headers (TblHdr_) to not appear when desclabel = 1 option specified
 @n @b 12-19-2019 Changed how the class levels are handled when ORs are 
 requested so that formats that include the variable name are handled
 correctly
 @n @b 12-10-2020 Fixed bug that was causing adhoc footnotes to be 
 omitted when no CON1 variables included or met criteria
 @n @b 12-09-2019 Fixed a bug that was causing CAT1 vars with a 0 count cell 
 to appear twice
 @n @b 11-13-2019 Fixed an issue with footnotes in XLSX files
 @n @b 09-13-2019 Did some table cleanup for situations when N=0 and also
 made imporvements for when a variable is specified on more than one list
 @n @b 09-12-2019 1) Removed the period used for indentation of CAT2 or ORD1
 var levels 2) Added a check for CAT1 vars - if these are not coded 0/1 or 
 1/2 they will be summarized as CAT2 and all levels displayed 3) Fixed a display
 issue with CAT1 vars that had 0% or 100% in level=1
 @n @b 09-11-2019 Added warning message if any observations are excluded 
 because of missing CLASS variable
 @n @b 06-18-2019 Fixed an issue with adhoc superscripts when requesting 
 CAT1 variables 
 @n @b 05-29-2019 Fixed a bug that was not allowing to have variables on 
 multiple lists when requesting adhoc tests
 @n @b 05-16-2019 Made small change to RTFOUT file in order to better 
 manage footnotes generated by OUTSUMMARIZE
 @n @b 05-08-2019 Fixed ordering for CAT2 and ORD1 vars on final table also
 fixed a bug that was adding blank lines for single level CAT2 vars
 @n @b 04-30-2019 Fixed a bug that was affecting the display of p-values 
 when >2 groups
 @n @b 04-23-2019  Updated the format for the N headers
 **/

%macro summarize(
        data=,
/*lists*/
        con1=,
        con2=,
        con3=,
        cat1=,
        cat2=,
        ord1=,

/*group var*/        
        class=,

/*db options*/
        subset=,
             
/*options for con1 vars*/        
        sdopt=1, 
        unequalvar=, 

/*options for cat1 or cat2 vars*/        
        colpct=1,
        cutexact=20,

/*pvalues*/        
        pvalues=1, 
        alpha = 0.05,

/*pairwise comps*/           
        adhoc=0,
        adhoc_method=bon, 

/*ors*/        
        oddsratios=0,
        unitscon1=,
        unitscon2=,
        unitscon3=,
                        
/*out dbs names*/        
        out=_out_,
        rtfout=_rtf_,

/*sorting*/
        sortby=,
        list=,
        
/*formats*/        
        f1= __Countf.,
        f2= __Estf.,
        f3= __Estf.,
        f4= __Estf.,
        pfmt= Pvaluef.,
        
/*report options*/        
        tbltitle=,
        
        totalcol=1, 
        ncol=0,
        misscol=0,
        
        desclabel=0,
        BoldedLabel=0,
        
        printptype=1,
        boldedsigp=1,
        
        daytime=0,
        printfn=1,
        addfn=,

/*file options*/        
        rtffile=,
        pdffile=,
        xlsxfile=,
        xmlfile=,
        
        bdytitle=0,
        page=portrait,
        
        
        cwidth1=,
        cwidth2=,
        cwidth3=,
              
        fontsize=11,
        fontface=Times,
        OutputWidth=,

/*debugging*/        
        debug=0);
                 
%*****************************************************************************;
%* DELETE DBS FROM WORK LIB                                                   ;
%*****************************************************************************;
proc datasets lib=work nodetails nolist nowarn;
    delete  _ngroups_ _vtype_ _n_ _order_ _type_ 
    _con1_ _tcon1_ _con2_ _tcon2_ _con3_ 
    _tcon3_ _cat1_ _cat2_ _ord1_ _keepcat_: p_info  _ttest_ _ttest2_ _eqvar_ 
    _anova_: _welch_: _parp_: _npar_: _nparp_: _fish: _csq_: _fisher_ _chisq_: 
    _outds_ _outcon1_ _outcon2_ _outcon3_ _outcat_ _oddsratios_ _summ_  _pval_ 
    _adhoc_pval_ _adhoc_pval2_ _add_ _tout_ _tempd_ _f _tests_ _tests2_;
run;
quit;

%*****************************************************************************;
%* FORMATS                                                                    ;
%*****************************************************************************;
%put ****** DEFINE FORMATS ******;

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
    value _yesnof
        1 = 'Yes'
        0 = 'No'
        2 = 'No';
run;
quit;

%*****************************************************************************;
%* ERROR CHECKING                                                             ;
%*****************************************************************************;
%put ****** ERROR CHECKING ******;

%let errorflag=0;
options minoperator;

%* Check debug option and turn on macro checking options;
%if not(%upcase(&debug)  in (0 1)) %then %do;
    %put WARNING: DEBUG must be either 0 or 1.;
    %put WARNING: DEBUG = **&debug**;
    %put WARNING: Default value of 0 will be used for DEBUG;
    %let debug=0;
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
    %PUT ERROR: Data set %upcase(&data) does not exist. Macro will stop executing;
    %LET ERRORFLAG=1;
%end; %*end to if length(data) ne  0;

%* Make sure at least 1 variable list provided;
%if %length(&con1 &con2 &con3 &cat1 &cat2 &ord1)=0 %then %do;
    %put ERROR: At least one of CON1, CON2, CON3, CAT1, CAT2, ORD1 must be provided. Macro will stop executing;
    %let errorflag=1;
%end;

%* If data exists, check syntax;
%if %length(&data) > 0 and %SYSFUNC(exist(&data))=1 %then %do;

    %macro allowedlist(vlist= , name= );
    
        %* The list syntax first--last or var: are not supported;
        %* Will make sure these characters are not given;
        %let dh_exist = %sysfunc(index(&vlist, --));
        %let c_exist  = %sysfunc(index(&vlist, :));
            
        %if &dh_exist gt 0 %then %do;
            %put ERROR: The First--Last syntax used in %upcase(&name) is not supported. Macro will stop executing.;
            %let ErrorFlag=1;
        %end;    
        
        %if &c_exist gt 0 %then %do;
            %put ERROR: The var1:var99 syntax used in %upcase(&name) is not supported. Macro will stop executing.;
            %let ErrorFlag=1;
        %end; 
    
    %mend;
    
    %allowedlist(vlist=&con1, name=con1);
    %allowedlist(vlist=&con2, name=con2);
    %allowedlist(vlist=&con3, name=con3);
    %allowedlist(vlist=&cat1, name=cat1);
    %allowedlist(vlist=&cat2, name=cat2);
    %allowedlist(vlist=&ord1, name=ord1);
    %allowedlist(vlist=&list, name=list);

%end; %* end to syntax check;

%* If data exists, parse variable lists in case x1-x10 syntax used;
%if %length(&data) > 0 and %SYSFUNC(exist(&data))=1 %then %do;

    %macro parselist(vlist= , name= );
    
        %*Check syntax;
    
        %*Parse variable list to long format;
        options varinitchk = nonote;
        data __parsevs;
            informat &vlist best.;
        run;            
        %if &debug ne 1 %then %do; options varinitchk = note; %end;
        %else %if &debug eq 1 %then %do; options varinitchk = warn; %end;
   
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
        %if %symexist(newlist) eq 0 %then %let newlist=; **in case not created bc of syntax error;
        %let &&name = &newlist;
       
        proc datasets lib=work nodetails nolist nowarn;
            delete __parsevs __parsevs2;
        run; quit;            
       
    %mend parselist;
    
    %if %length(&con1)>0 %then %do;
        %parselist(vlist=&con1, name=con1);
    %end;  
    %if %length(&con2)>0 %then %do;  
        %parselist(vlist=&con2, name=con2);
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
    %if %length(&ord1)>0 %then %do;
        %parselist(vlist=&ord1, name=ord1);
    %end;
    %if %length(&list)>0 %then %do;
        %parselist(vlist=&list, name=list);
    %end;
    
%end; %*end to parsing vars;    

%* Check valid options are given for other parameters;
%if %length(&unequalvar) gt 0 %then %do;
    %if not(&unequalvar in (0 1 2)) %then %do;
        %put WARNING: UNEQUALVAR must 0, 1 or 2.;
        %put WARNING: UNEQUALVAR = **&unequalvar**;
        %put WARNING: Default value of 2 for 2 group class variable or 0 for 3+ groups will be used for UNEQUALVAR.;
        %let unequalvar=;
    %end;
%end; %* end to unequalvar check;

%if &alpha le 0 or &alpha ge 1 %then %do;
    %put WARNING: ALPHA must be a value between 0 and 1.;
    %put WARNING: ALPHA = **&alpha**;
    %put WARNING: Default value of 0.05 will be used for ALPHA.;
    %let alpha=0.05;
%end; %* end to alpha check;

%if %length(&outputwidth) gt 0 and (&outputwidth lt 0 or &outputwidth gt 100) %then %do;
    %put WARNING: OUTPUTWIDTH must be a value between 0 and 100.;
    %put WARNING: OUTPUTWIDTH = **&outputwidth**;
    %put WARNING: Default value determined by PROC REPORT will be used for OUTPUTWIDTH.;
    %let outputwidth=;
%end; %* end to outputwidth check;

%if &fontsize le 0 %then %do;
    %put WARNING: FONTSIZE must be a non-zero positive value.;
    %put WARNING: FONTSIZE = **&fontsize**;
    %put WARNING: Default value of 11 will be used for FONTSIZE.;
    %let fontsize=11;
%end; %* end to fontsize check;

%if %length(&sortby)>0 %then %do;
%if not(%upcase(&sortby) in (_LIST PVAL VARNAME VARLABEL)) %then %do;
    %put WARNING: SORTBY must be either blank or equal to _LIST, PVAL, VARNAME or VARLABEL.;
    %put WARNING: SORTBY = **&SORTBY**;
    %put WARNING: Sorting will be done according to listing order.;
    %let sortby = ;
%end; %*end to if sortby check;
%end;

%if not(&sdopt  in (1 2)) %then %do;
    %put WARNING: SDOPT must be either 1 or 2.;
    %put WARNING: SDOPT = **&sdopt**;
    %put WARNING: Default value of 1 will be used for SDOPT.;
    %let sdopt=1;
%end; %*end to sdopt check;

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

%if not(%upcase(&adhoc_method)  in 
  (ADAPTIVEFDR AFDR ADAPTIVEHOCHBERG AHOCHBERG ADAPTIVEHOLM AHOLM 
  BONFERRONI BON BOOTSTRAP BOOT DEPENDENTFDR DFDRFDR LSU 
  FDRBOOT FDRPERM FISHER_C FIC HOCHBERG HOC HOLM HOMMEL HOM
  PERMUTATION PERM PFDR SIDAK SID STEPBON STEPBOOT STEPPERM 
  STEPSID STOUFFER LIPTAK NONE)) %then %do;
    %put WARNING: ADHOC_METHOD must be either NONE or an option accepted by PROC MULTTEST.;
    %put WARNING: ADHOC_METHOD = **&adhoc_method**;
    %put WARNING: Default value of BON will be used for ADHOC_METHOD.;
    %let adhoc_method=bon;
%end; %*end to adhoc_method check;

%* Check all 0/1 options;
%macro optcheck(option= , newoption= , default= );
    %if not(&option  in (0 1)) %then %do;
        %put WARNING: &NEWOPTION must be either 0 or 1.;
        %put WARNING: &NEWOPTION = **&option**;
        %put WARNING: Default value of &default will be used for &newoption.;
        %let &newoption=&default;
    %end; %*end to option check;
%mend optcheck;

%optcheck(option=&colpct,      newoption=colpct,      default=1);
%optcheck(option=&oddsratios,  newoption=pvalues,     default=1);
%optcheck(option=&pvalues,     newoption=pvalues,     default=1);
%optcheck(option=&totalcol,    newoption=totalcol,    default=1);
%optcheck(option=&adhoc,       newoption=adhoc,       default=0);
%optcheck(option=&desclabel,   newoption=desclabel,   default=1);
%optcheck(option=&boldedlabel, newoption=boldedlabel, default=0);
%optcheck(option=&boldedsigp,  newoption=boldedsigp,  default=1);
%optcheck(option=&printptype,  newoption=printptype,  default=1);
%optcheck(option=&bdytitle,    newoption=bdytitle,    default=0);
%optcheck(option=&daytime,     newoption=daytime,     default=0);

%* RTFFILE, PDFFILE, XMLFILE, XLSXFILE, TBLTITLE and ADDFN should have quotes;
%macro quotecheck(option=, name=);

    %if %length(&option)>0 and %sysevalf(%sysfunc(indexc(&option, '"'))^= 1) %then %do;
        %put
        ERROR: Double quotes are required for %upcase(&name). Macro will stop executing.;
        %let ErrorFlag=1;
    %end;

%mend;

%quotecheck(option = &rtffile,  name= rtffile);
%quotecheck(option = &pdffile,  name= pdffile);
%quotecheck(option = &xmlfile,  name= xmlfile);
%quotecheck(option = &xlsxfile, name= xlsxfile);
%quotecheck(option = &tbltitle, name= tbltitle);
%quotecheck(option = &addfn,    name= addfn);

%* CLASS should have single variable name;
%let nclass=0;
%do %while(%scan(&class,&nclass+1) ^=    ); 
    %let nclass=%eval(&nclass+1); 
%end;
%if &nclass gt 1 %then %do;
    %put ERROR: Only 1 CLASS variable allowed. Macro will stop executing.;
    %let ErrorFlag=1;
%end;
    
%* If data set exists, check variables exist;
%if %length(&data) > 0 and %SYSFUNC(exist(&data))=1 %then %do;
    
    %* Check that variables in list(s) exit, are numeric and not missing;
    %macro vexist(vlist= , name=);
        
        %let new&NAME= ;
        %let icount=1;
        %let cvar=%scan(&VLIST,&ICOUNT);
        %let nextvar=&CVAR;
        
        %let dsid=%sysfunc(open(&data));
        %if %length(&VLIST) > 0 %then %do %while(&NEXTVAR NE );
            
            %let cvar=%upcase(&NEXTVAR);
            %let icount=%eval(&ICOUNT+1);
            %let nextvar=%scan(&VLIST,&ICOUNT);
            
            %* variable exits?;
            %let exist=%sysfunc(varnum(&DSID, &CVAR));
           
            %* If yes, type and missing;
            %if &exist gt 0 %then %do;
                %let vartype=%sysfunc(vartype(&DSID, &EXIST)); 
        
                proc sql noprint;
                    select count(&CVAR) into :nmvalues
                    from &data;
                quit;
            %end;
            %else %do;
                %let vartype=;
                %let nmvalues=;
            %end;
        
            %if &EXIST gt 0 and &VARTYPE eq N and &NMVALUES gt 0 
              %then %let new&NAME=&&NEW&NAME &CVAR;
            %else %do;
                %let new&NAME=&&NEW&NAME;
                %if &EXIST = 0 %then
                    %put WARNING: %upcase(&CVAR) in %upcase(&name) list does not exist in data set &data and will be dropped.;  
                %else %if &VARTYPE ne N %then
                    %put WARNING: %upcase(&CVAR) in %upcase(&name) is not a numeric variable and will be dropped.;    
                %else %if &NMVALUES = 0 %then
                    %put WARNING: %upcase(&CVAR) in %upcase(&name) is completely missing and will be dropped.;    
            %end;
        
        %end; ** end to if length(VLIST) > 0 then do while;
        %let rc=%sysfunc(close(&DSID));
        
        %LET &&NAME=&&NEW&NAME;
    
    %mend VEXIST;   
    
    %if %length(&con1) gt 0 %then %do; %vexist(vlist=&con1, name=con1); %end;
    %if %length(&con2) gt 0 %then %do; %vexist(vlist=&con2, name=con2); %end;
    %if %length(&con3) gt 0 %then %do; %vexist(vlist=&con3, name=con3); %end;
    %if %length(&cat1) gt 0 %then %do; %vexist(vlist=&cat1, name=cat1); %end;
    %if %length(&ord1) gt 0 %then %do; %vexist(vlist=&ord1, name=ord1); %end;
    %if %length(&class) gt 0 %then %do; %vexist(vlist=&class, name=class); %end;
     
    %* Check that variables in CAT2 exit and not missing, these can be character;    
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
    
%end; %* end to length(data)>0 and sysfunc(exit(data))=1;

%* If data set exist and CAT1 vars given, check they are coded as 0/1 or 1/2;
%if %length(&data) gt 0 and %SYSFUNC(exist(&data)) eq 1 and %length(&cat1) gt 0 %then %do;

    %let newCAT1= ;
    %let NewCAT2= &CAT2;
    %let icount=1;
    %let cvar=%scan(&CAT1,&ICOUNT);
    %let nextvar=&CVAR;
    
    %do %while(&NEXTVAR NE );
    
        %let cvar=%upcase(&NEXTVAR);
        %let icount=%eval(&ICOUNT+1);
        %let nextvar=%scan(&CAT1,&ICOUNT);    
    
        %*Check values of cvar;
        proc sql noprint;
            select distinct(&cvar) format best.
            into :values separated by ''
            from &data
            where not missing(&cvar);
        quit;

        %*If these are 0/1 or 1/2 then keep in CAT1, else add to CAT2;
        %if &values in (01 12 0 1 2) %then %let newCAT1 = &newCAT1 &CVAR;
        %else %do;
            %put WARNING: %upcase(&cvar) in CAT1 list is not coded 0/1 or 1/2 and has been switched to CAT2 list.;
            %let newCAT2 = &newCAT2 &CVAR;
        %end;
        
    %end; %*end to do while;

    %let CAT1 = &newcat1;
    %let CAT2 = &newcat2;

%end; %*end to cat1 checks;

%* Check that units list for ORS have same length as the variable lists;
**IF OR=1 AND UNITS GIVEN;
%macro countlist(vlist= , unitlist= , name=);

    %let na=0;
    %do %while(%scan(&vlist,&na+1) ^=    ); 
        %let na=%eval(&na+1); 
    %end;
    
    %let nb=0;
    %do %while(%scan(&unitlist,&nb+1) ^=    ); 
        %let nb=%eval(&nb+1); 
    %end;
    
    %if &na ne &nb %then %do;
        %put WARNING: %upcase(&name) and UNITS%upcase(&name) should have the same length. UNITS%upcase(&name) will be ignored.;
        %put WARNING: Length of %upcase(&name)= **&na**;
        %put WARNING: Length of UNITS%upcase(&name)= **&nb**;
        %let UNITS%upcase(&name)=;
    %end;

%mend;

%if &oddsratios eq 1 %then %do;

    %if %length(&unitscon1) gt 0 %then  %do;
        %countlist(vlist=&con1, unitlist=&unitscon1, name=con1);
    %end;

    %if %length(&unitscon2) gt 0 %then  %do;        
        %countlist(vlist=&con2, unitlist=&unitscon2, name=con2);
    %end;
    
    %if %length(&unitscon3) gt 0 %then  %do;    
        %countlist(vlist=&con3, unitlist=&unitscon3, name=con3);
    %end;

%end;

%* If list not given, create;
%if %length(&list)=0 %then %let list=&con1 &con2 &con3 &cat1 &cat2 &ord1;

%* If no class var given and NCOL = 1 then set to 2 so column is generated;
%if %length(&class) eq 0 %then %do;
    %if &ncol eq 1 %then %let ncol=2;
    %if &misscol eq 1 %then %let misscol=2;
%end;

%*****************************************************************************;
%* IF ANY ERRORS FOUND THEN ABORT EXECUTION                                   ;
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
    ods noresults;
%end;

%*****************************************************************************;
%*  MANAGE TITLES AND FOOTNOTES                                               ;
%*****************************************************************************;
%put ****** MANAGE TITLES AND FOOTNOTES ******;
%* Store Footnotes;
proc sql ;
    create table work._f as select *
    from dictionary.titles where type='F';
    reset noprint; 
quit;

proc sql;
    reset noprint;
    select nobs into :FN from dictionary.tables
    where libname="WORK" & memname="_F";
quit;

%if(&fn>=1) %then %do floop=1 %to &fn;
    %local foot&floop;
%end;

%* Store footnotes in macro variables;
%let foot1= ;** Initialize at least one title **;
data _null_;
    set _f;
    %if (&fn ge 1) %then %do floop = 1 %to &fn;
        if number = &floop then call symput("FOOT&FLOOP", trim(left(text)));
    %end;
run;

%* If no base title stored, do not want to print "The SAS System";
proc sql noprint;
    select upcase(compress(text,  ,"scp")) into :CurrentTitle
    from  dictionary.titles
    where type="T" and number=1;
quit;

%if %symexist(currenttitle) = 1 %then %do;
    %if &currenttitle = THESASSYSTEM %then %do;
        title;
    %end;
%end;

%*****************************************************************************;
%*  DETERMINE SAS VERSION                                                     ;
%*****************************************************************************;
data _null_ ; 

   sysvlong = symget("SYSVLONG");                /* system macro variable */
   
   /* major version */
   pos1 = find(sysvlong, ".");
   major = input(substr(sysvlong, 1, pos1-1), best.);          

   /* minor version */
   pos2 = find(sysvlong, ".", 'i', pos1+1);
   minor = input(substr(sysvlong, pos1+1, pos2-pos1-1), best.);

   /* iteration version */
   pos3 = find(sysvlong, "M", 'i', pos2+1);
   iteration = input(substr(sysvlong, pos2+1, pos3-pos2-1), best.);

   /* maintenance level */
   pos4 = notdigit(sysvlong, pos3+1);
   maint = input(substr(sysvlong, pos3+1, pos4-pos3-1), best.);   

   b = ( major<9 ) 
      | ( major=9 & minor<4 )
      | ( major=9 & minor=4 & iteration<1 )
      | ( major=9 & minor=4 & iteration=1 & maint<=5 );
    if b then put "Running SAS 9.4m5 or earlier"; 
    else      put "Running After SAS 9.4m5";
    
    call symput('After94m5', b=0);
    
run; 

%*****************************************************************************;
%*  SET UP                                                                    ;
%*****************************************************************************;
%put ****** SET UP ******;
%*Create working data set;
data _tempd_;
    set &data;
    
    %*subset if requested;
    %if %length(&subset)>0 %then %do;
        where &subset;
    %end;        
    
    %* avoid the class variable name causing problems in ods;
    %* if no class specified then set to "Total" for ease of management;      
    %if %length(&class)=0 %then %do;
        __class=1;
        label __class="All";
    %end;      
    %else %do;
        if missing(&class) then delete;
        rename &class=__class;
    %end;           
run;

%*If any observations deleted because of missing class var then put warning;
proc sql noprint;
    select count(*) into :OrigN
    from &data
    %if %length(&subset) gt 0 %then %do;
      where &subset
    %end;
    ;
    
    select count(*) into :NewN
    from _tempd_;
quit; 

%if &OrigN gt &NewN %then %put WARNING: Observations deleted due to missing %upcase(&CLASS) variable.;

%*upper case class var;
%let class=%upcase(&class);

%*number observations;
%let dsid=%sysfunc(open(_tempd_));
%let nobs=%sysfunc(attrn(&DSID, nlobs));
%let rc=%sysfunc(close(&DSID));

%*count vars in list;
%let nlist=0;
%do %while(%scan(&list,&nlist+1) ^=    ); %let nlist=%eval(&nlist+1); %end;

%* count vars in each list;
%let ncon1=0;
%do %while(%scan(&con1, &ncon1+1) ^=    ); %let ncon1=%eval(&ncon1+1); %end;

%let ncon2=0;
%do %while(%scan(&con2, &ncon2+1) ^=    ); %let ncon2=%eval(&ncon2+1); %end;

%let ncon3=0;
%do %while(%scan(&con3, &ncon3+1) ^=    ); %let ncon3=%eval(&ncon3+1); %end;

%let ncon=0;
%do %while(%scan(&con1 &con2 &con3, &ncon+1) ^=    ); %let ncon=%eval(&ncon+1); %end;

%let nconord=0;
%do %while(%scan(&con1 &con2 &con3 &ord1, &nconord+1) ^=    ); %let nconord=%eval(&nconord+1); %end;

%let ncat1=0;
%do %while(%scan(&cat1, &ncat1+1) ^=    ); %let ncat1=%eval(&ncat1+1); %end;

%let ncat2=0;
%do %while(%scan(&cat2, &ncat2+1) ^=    ); %let ncat2=%eval(&ncat2+1); %end;

%let nord1=0;
%do %while(%scan(&ord1, &nord1+1) ^=    ); %let nord1=%eval(&nord1+1); %end;

%*manage the nominal vars;
%*count vars;
%let nnomcat=0;
%do %while(%scan(&cat2 &ord1,&nnomcat+1) ^=    ); %let nnomcat=%eval(&nnomcat+1); %end;

%*create macro vars for each and also for the formats;
%do i=1 %to &nnomcat;
    %let nomcat&i=%upcase(%scan(&cat2 &ord1, &i));
    
    %let dsid=%sysfunc(open(_tempd_));
    %let vnum=%sysfunc(varnum(&dsid, &&nomcat&i));
    %let vtype=%sysfunc(vartype(&dsid, &vnum));
    %let nomcatfmt&i=%sysfunc(varfmt(&DSID, &vnum));
    %let rc=%sysfunc(close(&dsid));   

    %if %length(&&nomcatfmt&i)=0 %then %do;
        %if &vtype=N %then %let nomcatfmt&i=best.;
        %else %let nomcatfmt&i=$char.;
    %end;
    
%end;

%*****************************************************************************;
%* NUMBER AND LABEL FOR CLASS VAR                                             ;
%*****************************************************************************;
%put ****** Getting Group Numbers and Labels ******;

title9 "CLASS VAR LEVELS";
proc freq data=_tempd_;
    table __class;
    ods output OneWayFreqs=_ngroups_;
run;

%* num of groups;
proc sql noprint;
    select count(distinct(__class)) format 8. into :NGroups
    from _ngroups_;
quit;
%let NGroups=&NGroups; **remove leading/trailing blanks;

%*If unequalvar not given or invalid value given, set default;
%if %length(&unequalvar) eq 0 %then %do;
    %if &NGroups eq 2 %then %let unequalvar=2;
    %else %let unequalvar=0;
%end; **end to setting default for unequalvar;

%* if oddsratios requested make sure only 2 groups;
%if &oddsratios eq 1 and &ngroups ne 2 %then %do;
    %put WARNING: CLASS variable &class has &ngroups levels. Odds Ratios will NOT be reported.;
    %let oddsratios = 0;
%end; %*end to oddsratios eq 1 and ngroups ne 2;

%*If adhoc requested and 2 or less groups set adhoc to 0;
%if &adhoc eq 1 and &ngroups le 2 %then %do;
    %put WARNING: CLASS variable &class has &ngroups levels. No ad hoc comparisons will be made.;
    %let adhoc = 0;
%end;

%* groups levels;
proc sql noprint;
    select __class format 8. into :loop1-:loop99
    from _ngroups_;
quit;   

%* groups labels;
proc sql noprint;
    select f___class into :GroupLab1-:GroupLab99
    from _ngroups_;
quit;

%if %length(&class)=0 and &NGroups=1 %then %do;
    %let GroupLab1=Total;
%end;

%* N for each group;    
proc sql noprint;
    select sum(Frequency) format=8. into :TotalN
    from _ngroups_;
quit;

%do i=1 %to &NGroups;

    proc sql noprint;
        select Frequency format=8. into :GroupN&i
        from _ngroups_
        where __class=&&loop&i;
    quit;
    %let GroupN&i=&&GroupN&i;

%end;

%* clear temp dbs;
%if &debug ne 1 and &adhoc ne 1 %then %do;
    proc datasets lib=work nodetails nolist nowarn;
        delete _ngroups_ ;
    run;
    quit;
%end; 
title9;

%*****************************************************************************;
%* SET UP FOR POST-HOC COMPARISONS, IF REQUESTED                              ;
%*****************************************************************************;
%if &adhoc eq 1 %then %do;

    %put ****** Set up for post-hoc comparisons ******;
    
    %*List of values;
    proc sql noprint;
        select __class format best. into :_ValueList separated by ' '
        from _ngroups_;
    quit;
    
    %*In _NGroups_ want to have a column with the group number 1 to x;
    data _NGroups_;
        set _NGroups_;
        %do i=1 %to &Ngroups;
            if __class = &&loop&i then __adclass = &i;
        %end;
    run;        
    
    %* Number of combinations;
    data _null_;
        call symput('_NumComb', comb(&NGroups, 2));
    run;
    %let _NumComb=&_NumComb;
            
    %*Combination subsets and lab;
    proc sql noprint;
        select
        v1.__adclass format best., v2.__adclass format best.,
        compress(put(v1.__class, best.))||" "||compress(put(v2.__class, best.)),
        compress(put(v1.__class, best.))||"vs"||compress(put(v2.__class, best.))
        into 
        :_AAD separated by ' ', :_BAD separated by ' ',
        :_AdHocSubset1 - :_AdHocSubset&_NumComb, 
        :_AdHocLab1 - :_AdHocLab&_NumComb
        from _ngroups_ v1, _ngroups_ v2
        where v1.__class<v2.__class
        order by v1.__class, v2.__class;    
    quit;   
    
    %* clear temp dbs;
    %if &debug ne 1 %then %do;
        proc datasets lib=work nodetails nolist nowarn;
            delete _ngroups_ ;
        run;
        quit;
    %end; 

%end; %* end to if adhoc eq 1;

%*****************************************************************************;
%* GET N, NMISS, LABEL AND ORDER                                              ;
%*****************************************************************************;
%put ****** Getting Numbers and Labels ******;

%* Get labels and var type;
proc contents data=_tempd_(keep=&con1 &con2 &con3 &cat1 &cat2 &ord1 obs=0)
    noprint out=_vtype_(keep=Name Type);
run; 

%* Get list of character and numeric vars;
proc sql noprint;
    select Name into :_num_ separated by ' '
    from _vtype_
    where Type=1;
%let _ncolnum_ = &SQLObs;

    select Name into :_char_ separated by ' '
    from _vtype_
    where Type=2;
%let _ncolchar_ = &SQLObs;    
quit;  

%* Get N and Nmiss (overall and by group);
data _n_;
    set _tempd_ end = eof;
   
    %if %symexist(_num_) %then %do; array n_vars[*] &_num_; %end;
    %if %symexist(_char_) %then %do; array c_vars[*] &_char_; %end;
    
    %do i=1 %to &NGroups;
        %if %symexist(_num_) %then %do; 
            array n_count&i[&_ncolnum_] _temporary_ (&_ncolnum_*0);
        %end;
        %if %symexist(_char_) %then %do; 
            array c_count&i[&_ncolchar_] _temporary_ (&_ncolchar_*0);
        %end;
    %end;
   
    %if %symexist(_num_) %then %do; 
        do i = 1 to dim(n_vars);
            %do i=1 %to &NGroups;
                if missing(n_vars[i]) and __class=&&loop&i then n_count&i[i] + 1;
            %end;            
        end;
    %end;
    
    %if %symexist(_char_) %then %do; 
        do i = 1 to dim(c_vars);
            %do i=1 %to &NGroups;
                if missing(c_vars[i]) and __class=&&loop&i then c_count&i[i] + 1;
            %end;  
        end;
    %end;
    
    if eof then do;
        %if %symexist(_num_) %then %do; 
           do i = 1 to dim(n_vars);
              VarName = upcase(vname(n_vars[i]));
              VarLabel = strip(vlabel(n_vars[i]));
              if VarLabel = " " then VarLabel = vname(n_vars[i]);
              _N = .; _NMiss = .; **place holders;
              %do i=1 %to &NGroups;
                  _N&i = &&GroupN&i - n_count&i[i];
                  _NMiss&i = n_count&i[i];
              %end;
              _N = sum(of _N1-_N&NGroups);
              _NMiss = sum(of _NMiss1-_NMiss&NGroups);
              output;
           end;
        %end;
       
        %if %symexist(_char_) %then %do; 
           do i = 1 to dim(c_vars);
              VarName = upcase(vname(c_vars[i]));
              VarLabel = vlabel(c_vars[i]);
              if VarLabel = " " then VarLabel = vname(c_vars[i]);
              _N = .; _NMiss = .; **place holders;
              %do i=1 %to &NGroups;
                  _N&i = &&GroupN&i - c_count&i[i];
                  _NMiss&i = c_count&i[i];
              %end;
              _N = sum(of _N1-_N&NGroups);
              _NMiss = sum(of _NMiss1-_NMiss&NGroups);
              output;
           end;
        %end;
        
    end; **end of if eof;
   
    keep VarName VarLabel _N _NMiss 
      %do i=1 %to &NGroups; _N&i _NMiss&i %end; ;
run;
proc sort data=_n_; by VarName; run;

%* Add order;
options varinitchk = nonote;
data _order_; informat VarName $256.; run;
%if &debug ne 1 %then %do; options varinitchk = note; %end;
%else %if &debug eq 1 %then %do; options varinitchk = warn; %end;

%do i=1 %to &nlist;
    %let var=%scan(&list, &i.);
    data _torder_;
        informat VarName $256.;
        VarName=upcase("&var.");
        _order=&i;
    run;
    data _order_; set _order_ _torder_; run;
    proc datasets library=work nolist; delete _torder_; run; quit;
%end;

%*Include Type so it resolves properly if any specified > 1;
data _type_;
    informat VarName $256. Type $25.;
    %if &ncon1>0 %then %do i=1 %to &ncon1;
        VarName=upcase(scan("&con1", &i, ' '));
        Type="CON1";
        output;
    %end;
    %if &ncon2>0 %then %do i=1 %to &ncon2;
        VarName=upcase(scan("&con2", &i, ' '));
        Type="CON2";
        output;
    %end;   
    %if &ncon3>0 %then %do i=1 %to &ncon3;
        VarName=upcase(scan("&con3", &i, ' '));
        Type="CON3";
        output;
    %end;
    %if &ncat1>0 %then %do i=1 %to &ncat1;
        VarName=upcase(scan("&cat1", &i, ' '));
        Type="CAT1";
        output;
    %end;
    %if &ncat2>0 %then %do i=1 %to &ncat2;
        VarName=upcase(scan("&cat2", &i, ' '));
        Type="CAT2";
        output;
    %end;  
    %if &nord1>0 %then %do i=1 %to &nord1;
        VarName=upcase(scan("&ord1", &i, ' '));
        Type="ORD1";
        output;
    %end;     
run;

data _order_; set _order_; if _n_>1; run;
proc sort data=_order_; by VarName; run;
proc sort data=_type_; by VarName; run;
data _outds_;
    informat VarName $256.;
    merge _n_(in = inn) _order_(in = inorder) _type_;
    by VarName;
    if inorder and not inn then do;
        if scan(VarName, 1, "_") =  "TBLHDR" then do;
            VarLabel = strip(propcase(tranwrd(tranwrd(VarName, "TBLHDR_", ""),"_", " ")));
            SectionHeaderFlag = 1;
        end;
        else if scan(VarName, 1, "_") ne  "TBLHDR" then delete;
    end;        
    if _order=. then _order=&nlist+_n_;
run; 

%*Message about section headers;
proc sql noprint;
    select VarName
    into :HeaderVars separated by ' '
    from _outds_
    where SectionHeaderFlag=1
    order by _order;
quit;
%if %symexist(HeaderVars) %then %put
NOTE: The following variables in LIST are treated as section headers: &HeaderVars.. 
 If this was unintentional, please remove from LIST.;

%* clear temp dbs;
%if &debug ne 1 %then %do;
    proc datasets lib=work nodetails nolist nowarn;
        delete _vtype_ _n_ _order_ _type_;
    run;
    quit;
%end;

%*****************************************************************************;
%* SUMMARIZE CON1 VARIABLES                                                   ;
%*****************************************************************************;
%if %length(&con1)>0 %then %do;
    
    %put ****** SUMMARIZING CON1 ******;
    title9 "SUMMARIZING CON1 VARS";
    
    ods output 
        Summary(persist)=_con1_(rename=(Variable=VarName));    
    
    title10 "Overall";
    proc means data=_tempd_ n nmiss mean stddev nolabels stackods;
        var &con1;
    run;
    
    title10 "BY CLASS";
    proc means data=_tempd_ n nmiss mean stddev nolabels stackods;
        class __class;
        var &con1;
    run; 
    
    ods output close;
    
    %*Manage output;
    data _con1_;
        set _con1_;
        VarName=upcase(VarName);
        informat _lab $50.;
        if __class=. then _lab='_all';
        %do i=1 %to &NGroups;
            if __class=&&loop&i then _lab="_&&loop&i";
        %end;
        keep VarName __class Mean StdDev _lab;
    run;
    proc sort data=_con1_; by VarName;
    
    proc transpose data=_con1_ out=_tcon1_(drop=_label_);
        by VarName;
        var Mean StdDev;
        id _lab;
    run;     
    
    data _outcon1_ (drop=_name_);
        merge _tcon1_(
                    where = (_Name_="Mean")
                    rename = (_all=_m %do i=1 %to &Ngroups; _&&loop&i=_m&i %end;))
            _tcon1_(
                    where = (_Name_="StdDev")
                    rename = (_all=_s %do i=1 %to &Ngroups; _&&loop&i=_s&i %end;));
        by VarName;
        type="CON1";
    run;        
    
    %*Clear temp dbs;
    %if &debug ne 1 %then %do;
        proc datasets lib=work nodetails nolist nowarn;
            delete _con1_ _tcon1_;
        run;
        quit;
    %end;   
    
%end; %* end to if con1;
title9;
title10;

%*****************************************************************************;
%* SUMMARIZE CON2 VARIABLES                                                   ;
%*****************************************************************************;
%if %length(&con2)>0 %then %do;
    
    %put ****** SUMMARIZING CON2 ******;
    title9 "SUMMARIZING CON2 VARS";
    
    ods output 
        Summary(persist)=_con2_(rename=(Variable=VarName));    
    
    title10 "OVERALL";
    proc means data=_tempd_ n nmiss median min max nolabels stackods;
        var &con2;    
    run;
    
    title10 "BY CLASS"; 
    proc means data=_tempd_ n nmiss median min max nolabels stackods;
        class __class;
        var &con2;
    run; 
    
    ods output close;
    
    %*Manage output;
    data _con2_;
        set _con2_;
        VarName=upcase(VarName);
        informat _lab $50.;
        if __class=. then _lab='_all';
        %do i=1 %to &NGroups;
            if __class=&&loop&i then _lab="_&&loop&i";
        %end;
        keep VarName __class Median Min Max _lab;
    run;
    proc sort data=_con2_; by VarName;
    
    proc transpose data=_con2_ out=_tcon2_(drop=_label_);
        by VarName;
        var Median Min Max;
        id _lab;
    run;     
    
    data _outcon2_ (drop=_name_);
        merge _tcon2_(
                    where = (_Name_="Median")
                    rename = (_all=_m %do i=1 %to &Ngroups; _&&loop&i=_m&i %end;))
              _tcon2_(
                    where = (_Name_="Min")
                    rename = (_all=_s %do i=1 %to &Ngroups; _&&loop&i=_s&i %end;))
              _tcon2_(
                    where = (_Name_="Max")
                    rename = (_all=_t %do i=1 %to &Ngroups; _&&loop&i=_t&i %end;));
        by VarName;
        type="CON2";
    run;        
    
    %*Clear temp dbs;
    %if &debug ne 1 %then %do;
        proc datasets lib=work nodetails nolist nowarn;
            delete _con2_ _tcon2_;
        run;
        quit;
    %end;   
    
%end; %* end to if con2;
title9;
title10;

%*****************************************************************************;
%* SUMMARIZE CON3 VARIABLES                                                   ;
%*****************************************************************************;
%if %length(&con3)>0 %then %do;
    
    %put ****** SUMMARIZING CON3 ******;
    title9 "SUMMARIZING CON3 VARS";
    
    ods output 
        Summary(persist)=_con3_(rename=(Variable=VarName));    
    
    title10 "OVERALL";
    proc means data=_tempd_ n nmiss median p25 p75 nolabels stackods;
        var &con3;    
    run;
    
    title10 "BY CLASS";
    proc means data=_tempd_ n nmiss median p25 p75 nolabels stackods;
        class __class;
        var &con3;
    run; 
    
    ods output close;
    
    %*Manage output;
    data _con3_;
        set _con3_;
        VarName=upcase(VarName);
        informat _lab $50.;
        if __class=. then _lab='_all';
        %do i=1 %to &NGroups;
            if __class=&&loop&i then _lab="_&&loop&i";
        %end;
        keep VarName __class Median P25 P75 _lab;
    run;
    proc sort data=_con3_; by VarName;
    
    proc transpose data=_con3_ out=_tcon3_(drop=_label_);
        by VarName;
        var Median P25 P75;
        id _lab;
    run;     
    
    data _outcon3_ (drop=_name_);
        merge _tcon3_(
                    where = (_Name_="Median")
                    rename = (_all=_m %do i=1 %to &Ngroups; _&&loop&i=_m&i %end;))
              _tcon3_(
                    where = (_Name_="P25")
                    rename = (_all=_s %do i=1 %to &Ngroups; _&&loop&i=_s&i %end;))
              _tcon3_(
                    where = (_Name_="P75")
                    rename = (_all=_t %do i=1 %to &Ngroups; _&&loop&i=_t&i %end;));
        by VarName;
        type="CON3";
    run;        
    
    %*Clear temp dbs;
    %if &debug ne 1 %then %do;
        proc datasets lib=work nodetails nolist nowarn;
            delete _con3_ _tcon3_;
        run;
        quit;
    %end;   
    
%end; %* end to if con3;
title9;

%*****************************************************************************;
%*  SUMMARIZE CAT VARS                                                        ;
%*****************************************************************************;   
%if %length(&cat1 &cat2 &ord1)>0 %then %do;
    
    %put ****** SUMMARIZING CAT1, CAT2, ORD1 ******;    
    title9 "SUMMARIZING CAT1, CAT2 AND ORD1 VARS";
    
    %if %length(&cat1)>0 %then %do;
        proc freq data=_tempd_;
            table (&cat1)*__class;
            ods output CrossTabFreqs=_cat1_;                    
        run;  
    %end;
    
    %if %length(&cat2)>0 %then %do;
        proc freq data=_tempd_;
            table (&cat2)*__class;
            ods output CrossTabFreqs=_cat2_;                    
        run;  
    %end;
    
    %if %length(&ord1)>0 %then %do;
        proc freq data=_tempd_;
            table (&ord1)*__class;
            ods output CrossTabFreqs=_ord1_;                    
        run;      
    %end;
    
    %* Manage output;
    data _keepcat_ %do i=1 %to &NGroups; _keepcat_&i %end; ;
        informat VarName VarLevel $256.;
        set 
            %if %length(&cat1)>0 %then %do; _cat1_(in=inc1) %end;
            %if %length(&cat2)>0 %then %do; _cat2_(in=inc2) %end;
            %if %length(&ord1)>0 %then %do; _ord1_(in=ino1) %end; ;
        
        where _type_ in ("11" "10");
        
        Varname=upcase(trim(scan(Table, 2)));
        
        %if %length(&cat1)>0 %then %do; if inc1 then Type="CAT1"; %end;
        %if %length(&cat2)>0 %then %do; if inc2 then Type="CAT2"; %end;
        %if %length(&ord1)>0 %then %do; if ino1 then Type="ORD1"; %end;
        
                    
        %* for CAT1 only want to keep the var=1 line;
        %if %length(&cat1)>0 %then %do;
            array yn[*] &cat1; 
            do j=1 to dim(yn);
                if yn[j] in (0 2) then delete;
            end;
        %end;
        
        %* for CAT2 and ORD1 need all levels and a VarLevel Option;
        **Create labels for formats;
        %if &nnomcat gt 0 %then %do i=1 %to &nnomcat;  
            if cmiss(&&nomcat&i)=0 then do;
                VarLevel="    "||left(trim(compbl(put(&&nomcat&i, &&nomcatfmt&i..))));
            end;            
        %end;
        _lorder=(_n_)/100;
 
        %* for total I need the Percent;
        if __class=. then MyPercent=Percent;
        
        %*Else if colpct=1 I need ColPercent and if =0 I need RowPercent;
        else if __class ne . and &colpct = 1 then MyPercent=ColPercent;
        else if __class ne . and &colpct = 0 then MyPercent=RowPercent;
        
        keep Varname Type VarLevel _lorder Frequency MyPercent ;
        
        if __class=. then output _keepcat_;
        %do i=1 %to &NGroups;
            if __class=&&loop&i then output _keepcat_&i;
        %end;
    run;
    
    %* combine into 1 file;
    proc sort data=_keepcat_; by VarName VarLevel _lorder; run;
    %do i=1 %to &NGroups;
        proc sort data=_keepcat_&i; by varname VarLevel _lorder; run;
    %end;
    data _outcat_;
        merge _keepcat_(rename=(Frequency=_m MyPercent=_s))
            %do i=1 %to &NGroups; 
                _keepcat_&i(rename=(Frequency=_m&i MyPercent=_s&i)) 
            %end; ;
        by VarName VarLevel;
    run;      
    
    %*Recode _levelorder;
    proc sort data=_outcat_; by VarName _lorder; run;
    data _outcat_;
        set _outcat_;
        by VarName;
        retain _levelorder;
        *if first.VarName and last.VarName then _levelorder = .;
        if type ne "CAT1" then do;
            if first.VarName then _levelorder = 1;
            else _levelorder = _levelorder + 1;
        end;
        drop _lorder;
    run;
    
    %* clear temp dbs;
    %if &debug ne 1 %then %do;
        proc datasets lib=work nodetails nolist nowarn;
            delete _cat1_ _cat2_ _ord1_ _keepcat_
              %do i=1 %to &NGroups; _keepcat_&i %end; ;
        run;
        quit;
    %end;     
    **define TYPE when i concatenate;

%end; %*end to cat1 cat2 ord1;
title9;

%*****************************************************************************;
%*  P-VALUES FOR CON1 VARS WHEN 2 GROUPS                                      ;
%*****************************************************************************; 
%if &NGroups eq 2 and &pvalues eq 1 and %length(&con1) gt 0 and &oddsratios eq 0
  %then %do;
    
    %put ****** GETTING OVERALL P-VALUES FOR CON1 (2 groups) ******;
    title9 "GETTING OVERALL P-VALUES FOR CON1 (2 groups)";
    
    %*get t-test pvalues and test equality of variances;
/*     %if &debug eq 1 %then %do; ods select TTests Equality; %end; */
    proc ttest data=_tempd_;
        class __class;
        var &con1;
        ods output 
            TTests=_ttest_(keep=Variable Variances ProbT
                rename=(Variable=VarName ProbT=pval))
            Equality=_eqvar_(keep=Variable ProbF
                rename=(Variable=VarName ProbF=EqVarP))
        ;
    run;   
    
    %*combine ttests and equality of var;
    proc sql;
        create table _ttest2_ as select
        a.VarName, Variances, pval, EqVarP
        from _ttest_ a full join _eqvar_ b
        on a.VarName=b.VarName;
    quit; 

    %*select appropriate p-value;
    data _parp_;
        set _ttest2_;
        
        %if &unequalvar eq 0 %then %do;
            where Variances="Equal";    
        %end;
        
        %else %if &unequalvar eq 1 %then %do;
            where Variances="Unequal";    
        %end;
        
        %if &unequalvar eq 2 %then %do;
            if EqVarP le &alpha and Variances="Equal" then delete;
            else if EqVarP gt &alpha and Variances="Unequal" then delete;
        %end;
        
        PvalFlag=substr(Variances,1,1);
        
        type='CON1';
        
        drop Variances;
    run;
    
    %if &debug ne 1 %then %do;
        proc datasets lib=work nodetails nolist nowarn;
            delete _ttest_ _ttest2_ _eqvar_;
        run;
        quit;
    %end;
    
%end; %*end to 2 group con1 pvalues;
title9;

%*****************************************************************************;
%*  P-VALUES FOR CON1 VARS WHEN 3+ GROUPS                                     ;
%*****************************************************************************; 
%if &NGroups gt 2 and &pvalues eq 1 and %length(&con1) gt 0 
  %then %do;
    
    %put ****** GETTING OVERALL P-VALUES FOR CON1 (3+ groups) ******;
    title9 "GETTING OVERALL P-VALUES FOR CON1 (3+ groups)";
    
    %*Parse through variables;
    ods output
        ModelAnova(persist)=_anova_(where=(HypothesisType=3)
              keep= HypothesisType Dependent ProbF
              rename=(Dependent=VarName ProbF=pval))
        HOVFTest(persist)=_eqvar_(where=(Source="__class")
            keep=Dependent Source ProbF
            rename=(Dependent=VarName ProbF=EqVarP))
        Welch(persist)=_welch_(where=(Source="__class")
            keep=Dependent Source ProbF
            rename=(Dependent=VarName ProbF=pval))              
    ;

    %do i=1 %to &NCON1;
        
        %let thiscon1 = %scan(&con1, &i);
        
        %put ****** GETTING OVERALL P-VALUES FOR CON1 (3+ groups): %upcase(&thiscon1) ******;
        title10 "%upcase(&thiscon1)";
        
        proc glm data=_tempd_;
            class __class;
            model &thiscon1 = __class;
            means __class/ hovtest welch;
        run; 
        quit;
       
    %end; %* end to ncon1 loop;
    ods output close; 
    
    %*combine tests and equality of var;
    data _tests_;
        set _anova_(in=ina drop=HypothesisType)
            _welch_(in=inw drop=Source);
        if inw then Variances="Unequal";
        else if ina then Variances="Equal";  
    run;    
    
    proc sql;
        create table _tests2_ as select
        a.VarName, Variances, pval, EqVarP
        from _tests_ a full join _eqvar_ b
        on a.VarName=b.VarName;
    quit; 
    
    %*select appropriate p-value;
    data _parp_;
        set _tests2_;
        
        %if &unequalvar eq 0 %then %do;
            where Variances="Equal";    
        %end;
        
        %else %if &unequalvar eq 1 %then %do;
            where Variances="Unequal";    
        %end;
        
        %if &unequalvar eq 2 %then %do;
            if EqVarP le &alpha and Variances="Equal" then delete;
            else if EqVarP gt &alpha and Variances="Unequal" then delete;
        %end;
        
        PvalFlag=substr(Variances,1,1);
        
        type='CON1';
        
        drop Variances;
    run;    
    
    %if &debug ne 1 %then %do;
        proc datasets lib=work nodetails nolist nowarn;
            delete _anova_ _eqvar_ _welch_ _tests_ _tests2_;
        run;
        quit;
    %end;
    
    %put ****** GETTING PAIRWISE P-VALUES FOR CON1 ******;
    title9 "GETTING PAIRWISE P-VALUES FOR CON1";
    
    %*List of significant variables;
    proc sql noprint;
        select VarName, PValFlag 
        into :Con1ADList separated by ' ', :Con1ADType separated by ' '
        from _parp_
        where pval lt &alpha;
    %let NCon1AdList=&SQLObs;
    quit;
    
    %* If requested and any is signifcant then do post-hoc;
    %if &adhoc eq 1 and %symexist(Con1AdList) %then %do;
   
        %* Parse through all combinations;
        %do j=1 %to &_NumComb;

            ods output
                ModelAnova(persist)=
                      _anova_adhoc_unadj&j(where=(HypothesisType=3)
                      keep= HypothesisType Dependent ProbF
                      rename=(Dependent=VarName ProbF=Raw_P))
                Welch(persist)=_welch_adhoc_unadj&j(where=(Source="__class")
                    keep=Dependent Source ProbF
                    rename=(Dependent=VarName ProbF=Raw_p))            
            ;
        
            %do i=1 %to &NCon1AdList;
                
                %let thiscon1 = %scan(&Con1AdList, &i);
                %let thisptype = %scan(&Con1AdType, &i);
                
                %put ****** GETTING PAIRWISE (p_&&_AdHocLab&j) P-VALUES FOR CON1: %upcase(&thiscon1) ******;
                title10 "%upcase(&thiscon1) (p_&&_AdHocLab&j)";
                
                proc glm data=_tempd_;
                    where __class in (&&_AdHocSubset&j);
                    class __class;
                    model &thiscon1 = __class;
                    means __class / hovtest welch;
                run; 
                quit;
               
            %end; %* end to ncon1 loop;
            ods output close;

            
            %* identify test on db;
            data _anova_adhoc_unadj&j;
                set _anova_adhoc_unadj&j;
                informat pLab pType $50.;
                pLab="p_&&_AdHocLab&j"; 
                pType="&thisptype";
            data _welch_adhoc_unadj&j;
                set _welch_adhoc_unadj&j;
                informat pLab pType $50.;
                pLab="p_&&_AdHocLab&j";
                pType="&thisptype";
            run;
            
        %end; %*end to j=1 to numcomb;
        
        %*Select the test we want;
        %*Raw p-values;
        data %if %upcase(&adhoc_method) eq NONE %then %do; _parp_adhoc_ %end;
             %else %do; _parp_mult_ %end; ;   
            set %do j=1 %to &_NumComb; _anova_adhoc_unadj&j _welch_adhoc_unadj&j %end; ;            
            if pType="E" and Source="__class" then delete;
            else if pType="U" and HypothesisType=3 then delete;
            drop HypothesisType Source;
        run;  
        
        %* If requested, then adjust;
        %if %upcase(&adhoc_method) ne NONE %then %do;
            
            title10 "ADJUSTING PAIRWISE P-VALUES FOR CON1";
            
            proc sort data=_parp_mult_; by VarName plab; run;      
            
            ods output pValueInfo=p_info
                    pValues=_parp_adhoc_(drop=test raw rename=(_id1=pLab));
            proc multtest inpvalues=_parp_mult_ &adhoc_method;
                by VarName;
                id plab;
            run;  
            ods output close;
            
            %*Add type of var;
            data _parp_adhoc_;
                set _parp_adhoc_;
                type='CON1';
            run;
            
            %*Save label for adjusment method;
            data _null_;
                set p_info;
                call symput('padj_method', trim(Value));
            run;
        
        %end;
        
    %end; %*end to adhoc eq 1;    

    %*Clear temp dbs;
    %if &debug ne 1 %then %do;
        proc datasets lib=work nodetails nolist nowarn;
            delete p_info
            %if &adhoc eq 1 %then %do j=1 %to &_NumComb; _anova_adhoc_unadj&j _welch_adhoc_unadj&j %end; 
            _parp_mult_;
        run;
        quit;
    %end; 
    
%end; %*end to 3+ group con1 pvalues;
title9;
title10;

%*****************************************************************************;
%*  P-VALUES FOR CON2, CON3 AND ORD1 VARS                                     ;
%*****************************************************************************;
%macro nparpval(list= , type=);
    
    %put ****** GETTING OVERALL P-VALUES FOR &type ******;
    title9 "GETTING OVERALL P-VALUES FOR &type";
    
    proc npar1way data=_tempd_ wilcoxon;
        class __class;
        var &list;
        ods output
            %if &NGroups eq 2 %then %do;
                %if not(&After94m5) %then %do;
                    WilcoxonTest=_nparp_&type._ (
                        where=(Name1="PT2_WIL")
                        keep=Name1 Variable nValue1
                        rename=(Variable=VarName nValue1=pval))       
                %end;
                %if &After94m5 %then %do;
                    WilcoxonTest=_nparp_&type._ (                        
                        keep=Variable tProb2
                        rename=(Variable=VarName tProb2=pval))       
                %end;                
            %end;
            %else %if &NGroups gt 2 %then %do;
                %if not(&After94m5) %then %do;
                    KruskalWallisTest=_nparp_&type._ (
                        where=(Name1="P_KW")
                        keep=Name1 Variable nValue1
                        rename=(Variable=VarName nValue1=pval))
                %end;
                %if &After94m5 %then %do;
                    KruskalWallisTest=_nparp_&type._ (
                        keep=Variable Prob
                        rename=(Variable=VarName Prob=pval))
                %end;                
            %end;
       ;
    run;  
    
    %*Add type;
    data _nparp_&type._;
        set _nparp_&type._;
        type="&type";
    run;        
        
    %*List of significant variables;
    proc sql noprint;
        select VarName into :ConkwADList separated by ' '
        from _nparp_&type._
        where pval lt &alpha;
    quit;
    
    %* If requested and any is signifcant then do post-hoc;
    %if &adhoc eq 1 and %symexist(ConkwAdList) %then %do;
    
        %put ****** GETTING PAIRWISE P-VALUES FOR &type ******;
        title9 "GETTING PAIRWISE P-VALUES FOR &type";
   
        %* Parse through all combinations;
        %do j=1 %to &_NumComb;
        
        %put ****** GETTING PAIRWISE P-VALUES FOR &type (p_&&_AdHocLab&j) ******;
        title10 "p_&&_AdHocLab&j";
        
            ods output
                %if &NGroups eq 2 %then %do;
                    %if not(&After94m5) %then %do;
                        WilcoxonTest=_nparp_adhoc_&type._unadj&j (
                            where=(Name1="PT2_WIL")
                            keep=Name1 Variable nValue1
                            rename=(Variable=VarName nValue1=Raw_P))       
                    %end;
                    %if &After94m5 %then %do;
                        WilcoxonTest=_nparp_adhoc_&type._unadj&j (                        
                            keep=Variable tProb2
                            rename=(Variable=VarName tProb2=Raw_P))       
                    %end;                
                %end;
                %else %if &NGroups gt 2 %then %do;
                    %if not(&After94m5) %then %do;
                        KruskalWallisTest=_nparp_adhoc_&type._unadj&j (
                            where=(Name1="P_KW")
                            keep=Name1 Variable nValue1
                            rename=(Variable=VarName nValue1=Raw_P))
                    %end;
                    %if &After94m5 %then %do;
                        KruskalWallisTest=_nparp_adhoc_&type._unadj&j (
                            keep=Variable Prob
                            rename=(Variable=VarName Prob=Raw_P))
                    %end;                
                %end;
           ;
  
            proc npar1way data=_tempd_ wilcoxon;
                where __class in (&&_AdHocSubset&j);
                class __class;
                var &ConkwAdList;
            run;
            
            ods output close;
            
            %* identify test on db;
            data _nparp_adhoc_&type._unadj&j;
                set _nparp_adhoc_&type._unadj&j;
                informat pLab pType $50.;
                pLab="p_&&_AdHocLab&j";
            run;                

        %end; %*end to j=1 to numcomb;

        %*Raw p-values;
        data %if %upcase(&adhoc_method) eq NONE %then %do; _nparp_adhoc_&type._ %end;
             %else %do; _nparp_mult_&type._ %end; ;   
            set %do j=1 %to &_NumComb; _nparp_adhoc_&type._unadj&j %end; ;
            %if not(&After94m5) %then %do; drop name1; %end;
        run;  
        
        %* If requested, then adjust;
        %if %upcase(&adhoc_method) ne NONE %then %do;
            
            title10 "ADJUSTING PAIRWISE P-VALUES FOR &type";
            
            proc sort data=_nparp_mult_&type._; by VarName plab; run;      
            
            ods output 
                %if %symexist(padj_method) eq 0 %then %do; pValueInfo=p_info %end;
                pValues=_nparp_adhoc_&type._(drop=test raw rename=(_id1=pLab));
            proc multtest inpvalues=_nparp_mult_&type._ &adhoc_method;
                by VarName;
                id plab;
            run;  
            ods output close;
            
        %end;
    
        %*Add type;
        data _nparp_adhoc_&type._;
            set _nparp_adhoc_&type._;
            type="&type";
        run;
    
    %end; %*end to adhoc eq 1;    
        
    %*Clear temp dbs;
    %if &debug ne 1 %then %do;
        proc datasets lib=work nodetails nolist nowarn;
            delete
            %if &adhoc eq 1 %then %do j=1 %to &_NumComb; _nparp_adhoc_&type._unadj&j %end; 
            _nparp_mult_&type._;
        run;
        quit;
    %end; 
    
title9;
title10;

%mend nparpval;

%if &NGroups gt 1 and &pvalues eq 1 and &oddsratios eq 0 %then %do;

    %if %length(&con2) gt 0 %then %do; %nparpval(list=&con2, type=CON2); %end;
    %if %length(&con3) gt 0 %then %do; %nparpval(list=&con3, type=CON3); %end;
    %if %length(&ord1) gt 0 %then %do; %nparpval(list=&ord1, type=ORD1); %end;
    
    %*Combine;
    %if %length(&con2 &con3 &ord1) gt 0 %then %do;
    
        data _nparp_;
            set 
              %if %length(&con2) gt 0 %then %do; _nparp_con2_ %end;
              %if %length(&con3) gt 0 %then %do; _nparp_con3_ %end;
              %if %length(&ord1) gt 0 %then %do; _nparp_ord1_ %end; ;
        run; 
        
        %if &adhoc eq 1 and (%SYSFUNC(exist(_nparp_adhoc_con2_)) or 
          %SYSFUNC(exist(_nparp_adhoc_con3_)) or %SYSFUNC(exist(_nparp_adhoc_ord1_)))
          %then %do;
        
        data _nparp_adhoc_;
            set 
              %if %SYSFUNC(exist(_nparp_adhoc_con2_)) and %length(&con2) gt 0 %then %do; _nparp_adhoc_con2_ %end;
              %if %SYSFUNC(exist(_nparp_adhoc_con3_)) and %length(&con3) gt 0 %then %do; _nparp_adhoc_con3_ %end;
              %if %SYSFUNC(exist(_nparp_adhoc_ord1_)) and %length(&ord1) gt 0 %then %do; _nparp_adhoc_ord1_ %end; ;
        run;
        
        %end;
    
        %*If not already created, save label for adjusment method;           
        %if &adhoc eq 1 and %SYSFUNC(exist(p_info)) and %symexist(padj_method) eq 0 %then %do;
            data _null_;
                set p_info;
                call symput('padj_method', trim(Value));
            run;            
        %end;       
    
    %end; %*end to if con2 con3 ord gt 0; 
    
    %*Clear temp dbs;
    %if &debug ne 1 %then %do;
        proc datasets lib=work nodetails nolist nowarn;
            delete p_info _nparp_con2_ _nparp_con3_ _nparp_ord1_
            _nparp_adhoc_con2_ _nparp_adhoc_con3_ _nparp_adhoc_ord1_;
        run;
        quit;
    %end; 

%end; %*end to if class>0 and pvalues=1 for non-parametric;

%*****************************************************************************;
%*  P-VALUES FOR CAT1 AND CAT2 VARS                                           ;
%*****************************************************************************;
%macro catpval(list= , type=);

    %global CHISQ&TYPE.LIST FISHER&TYPE.LIST;
   
    %put ****** GETTING OVERALL P-VALUES FOR &type ******;
    title9 "GETTING OVERALL P-VALUES FOR &type";
    
    %*Determine if Fishers Exact test needed for any var;
    title10 "DETERMINE IF FISHER'S EXACT TEST NEEDED";
    proc freq data=_tempd_;
        tables __class*(&list) / sparse expected nopercent nocol norow;
        ods output CrossTabFreqs=_fish1_&type._(where=(_type_="11"));
    run;
    
    data _fish2_&type._;
        set _fish1_&type._;
        VarName=compress(scan(Table,2,"*"));
        _ExpectedLess5= expected<5;
    run;
    
    proc means data=_fish2_&type._ n sum noprint;
        class VarName;
        var _ExpectedLess5;
        output out=_fish3_&type._(where=(_type_=1)) n=n sum=sum;
    run;    
    
    proc sql noprint;
        select VarName into :Fisher&type.List separated by ' '
        from _fish3_&type._
        where round((sum/n)*100,0.01) ge &cutexact;
    proc sql noprint;
        select VarName into :ChiSq&type.List separated by ' '
        from _fish3_&type._
        where round((sum/n)*100,0.01) lt &cutexact;    
    quit;
        
    %*Get chisq pvalues;
    %if %length(&&ChiSq&type.List) gt 0 %then %do;
        title10 "GETTING CHI-SQ P";
        %if &debug=1 %then %do; ods select chisq; %end;
        ods output chisq=_csq_&type._(
                         where=(Statistic="Chi-Square")
                         keep=Table Statistic Prob 
                         rename=(prob=pval));
        proc freq data=_tempd_;
            tables __class*(&&ChiSq&type.List)/chisq;
        run;
        ods output close;
    %end; %* end to chisq;
    
    %*Get Fishers pvalues; 
    %if %length(&&Fisher&type.List) gt 0 %then %do;
        title10 "GETTING FISHER'S P";
        %if &debug=1 %then %do; ods select FishersExact; %end;
        ods output FishersExact=_fisher_&type._(
                               where=(name1="XP2_FISH")
                               keep=Name1 Table nValue1
                               rename=(nvalue1=pval));    
        proc freq data=_tempd_;
            tables __class*(&&Fisher&type.List)/fisher;
        run;
        ods output close;    
    %end; %* end to fisher;
    
    %*if the p-val dbs were not created (tests could not be done), then do not want the lists to have vars;  
    %if %SYSFUNC(exist(_csq_&type._)) ne 1 %then %let ChiSq&type.List=;
    %if %SYSFUNC(exist(_fisher_&type._)) ne 1 %then %let Fisher&type.List=;

    %*Combine and add type;
    data _chisq_&type._;
        informat VarName $256.;
        set 
          %if %length(&&ChiSq&type.List) gt 0 %then %do; _csq_&type._ %end;
          %if %length(&&Fisher&type.List) gt 0 %then %do; _fisher_&type._(in=inf) %end; ;
        VarName=compress(scan(Table,2,"*"));
        %if %length(&&Fisher&type.List) gt 0 %then %do;
            if inf then PvalFlag="F";
        %end;
        type="&type";
        drop table 
          %if %length(&&Fisher&type.List) gt 0 %then %do; name1 %end;
          %if %length(&&ChiSq&type.List) gt 0 %then %do; statistic %end; ;
    run;     
   
    %put ****** GETTING PAIRWISE P-VALUES FOR &type ******;
    
    %*List of significant variables;
    proc sql noprint;
        select VarName into :ChiSq&type.ADList separated by ' '
        from _chisq_&type._
        where pval lt &alpha
        %if %length(&&Fisher&type.List) gt 0 %then %do; and PvalFlag ne "F" %end; ;
    quit;
    %if %length(&&Fisher&type.List) gt 0 %then %do;
        proc sql noprint;
            select VarName into :Fisher&type.ADList separated by ' '
            from _chisq_&type._
            where pval lt &alpha and PvalFlag eq "F";
        quit;    
    %end;
    %* If requested and any is signifcant then do post-hoc;
    %if &adhoc eq 1 and (%symexist(ChiSq&type.ADList) or %symexist(Fisher&type.ADList))
        %then %do;
   
        %* Parse through all combinations;
        %do j=1 %to &_NumComb;

            %put ****** GETTING PAIRWISE P-VALUES FOR &type (p_&&_AdHocLab&j) ******;
            title9 "GETTING PAIRWISE P-VALUES FOR &type (p_&&_AdHocLab&j)";
            
            %*Get chisq pvalues;
            %if %symexist(ChiSq&type.ADList) %then %do;
                title10 "CHI-SQ";
                %if &debug eq 1 %then %do; ods select chisq; %end;
                ods output chisq=_csq_adhoc_&type._unadj&j(
                                 where=(Statistic="Chi-Square")
                                 keep=Table Statistic Prob 
                                 rename=(prob=Raw_P));
                proc freq data=_tempd_;
                    where __class in (&&_AdHocSubset&j);
                    tables __class*(&&ChiSq&type.ADList)/chisq;
                run;
                ods output close;
            %end; %* end to chisq;
        
            %*Get Fishers pvalues;
            %if %symexist(Fisher&type.ADList) %then %do;
                title10 "FISHER'S";
                %if &debug eq 1 %then %do; ods select FishersExact; %end;
                ods output FishersExact=_fisher_adhoc_&type._unadj&j(
                                       where=(name1="XP2_FISH")
                                       keep=Name1 Table nValue1
                                       rename=(nvalue1=Raw_P));    
                proc freq data=_tempd_;
                    where __class in (&&_AdHocSubset&j);
                    tables __class*(&&Fisher&type.AdList)/fisher;
                run;
                ods output close;    
            %end; %* end to fisher;
        
            %*Combine;
            data _chisq_adhoc_&type._unadj&j;
                informat VarName $256. pLab $50.;
                set 
                  %if %symexist(ChiSq&type.AdList) %then %do; _csq_adhoc_&type._unadj&j %end;
                  %if %symexist(Fisher&type.AdList) %then %do; _fisher_adhoc_&type._unadj&j(in=inf) %end; ;
                VarName=compress(scan(Table,2,"*"));
                %if %symexist(Fisher&type.AdList) %then %do;
                    if inf then PvalFlag="F";
                %end;
                pLab="p_&&_AdHocLab&j";
                drop table 
                  %if %symexist(Fisher&type.AdList) %then %do; name1 %end;
                  %if %symexist(ChiSq&type.AdList) %then %do; statistic %end; ;
            run;  

        %end; %*end to j=1 to numcomb;

        %*Raw p-values;
        data %if %upcase(&adhoc_method) eq NONE %then %do; _chisq_adhoc_&type._ %end;
             %else %do; _chisq_mult_&type._ %end; ;   
            set %do j=1 %to &_NumComb; _chisq_adhoc_&type._unadj&j %end; ;
        run;  
        
        %* If requested, then adjust;
        %if %upcase(&adhoc_method) ne NONE %then %do;
            
            title9 "ADJUSTING PAIRWISE P-VALUES FOR &type VARS";
            
            proc sort data=_chisq_mult_&type._; by VarName plab; run;      
            
            ods output 
                %if %symexist(padj_method) eq 0 %then %do; pValueInfo=p_info %end;
                pValues=_chisq_adhoc_&type._(drop=test raw rename=(_id1=pLab));
            proc multtest inpvalues=_chisq_mult_&type._ &adhoc_method;
                by VarName;
                id plab;
            run;  
            ods output close;
            
        %end;

        %*Add type;
        data _chisq_adhoc_&type._;
            set _chisq_adhoc_&type._;
            type="&type";
        run;
        
    %end; %*end to adhoc eq 1;    

    %*Clear temp dbs;
    %if &debug ne 1 %then %do;
        proc datasets lib=work nodetails nolist nowarn;
            delete _fish: _csq_&type._ _fisher_&type._
            %if &adhoc eq 1 %then %do j=1 %to &_NumComb; 
              _chisq_adhoc_&type._unadj&j _csq_adhoc_&type._unadj&j 
              _fisher_adhoc_&type._unadj&j
            %end; 
            _chisq_mult_;
        run;
        quit;
    %end; 


title9;
title10;

%mend catpval;

%if &NGroups gt 1 and &pvalues eq 1 and &oddsratios eq 0 %then %do;
    
    %if %length(&cat1) gt 0 %then %do; %catpval(list=&cat1, type=CAT1); %end;
    %if %length(&cat2) gt 0 %then %do; %catpval(list=&cat2, type=CAT2); %end;

    %*Reset lists;
    %if %symexist(FisherCAT1List) and %symexist(FisherCAT2List) %then
        %let FisherCatList=&FisherCAT1List &FisherCAT2List; 
    %else %if %symexist(FisherCAT1List) and %symexist(FisherCAT2List) eq 0 %then
        %let FisherCatList=&FisherCAT1List;
    %else %if %symexist(FisherCAT1List) eq 0 and %symexist(FisherCAT2List) %then
        %let FisherCatList=&FisherCAT2List;         
    %else %let FisherCatList=;

    %if %symexist(ChiSqCAT1List) and %symexist(ChiSqCAT2List) %then
        %let ChiSqCatList=&ChiSqCAT1List &ChiSqCAT2List; 
    %else %if %symexist(ChiSqCAT1List) and %symexist(ChiSqCAT2List) eq 0 %then
        %let ChiSqCatList=&ChiSqCAT1List;
    %else %if %symexist(ChiSqCAT1List) eq 0 and %symexist(ChiSqCAT2List) %then
        %let ChiSqCatList=&ChiSqCAT2List;         
    %else %let ChiSqCatList=;
    
    %*Combine;
    %if %length(&cat1 &cat2) gt 0 %then %do;
    
        data _chisq_;
            set 
              %if %length(&cat1) gt 0 %then %do; _chisq_cat1_ %end;
              %if %length(&cat2) gt 0 %then %do; _chisq_cat2_ %end; ;
        run; 
        
        %if &adhoc eq 1 and (%SYSFUNC(exist(_chisq_adhoc_cat1_)) or %SYSFUNC(exist(_chisq_adhoc_cat2_)))
          %then %do;
        
        data _chisq_adhoc_;
            set 
              %if %SYSFUNC(exist(_chisq_adhoc_cat1_)) %then %do; _chisq_adhoc_cat1_ %end;
              %if %SYSFUNC(exist(_chisq_adhoc_cat2_)) %then %do; _chisq_adhoc_cat2_ %end; ;
        run;
        
       %end;
       
        %*If not already created, save label for adjusment method;           
        %if &adhoc eq 1 and %SYSFUNC(exist(p_info)) and %symexist(padj_method) eq 0 %then %do;
            data _null_;
                set p_info;
                call symput('padj_method', trim(Value));
            run;            
        %end;          
    
    %end; %*end to if con2 con3 ord gt 0;
    
    %*Clear temp dbs;
    %if &debug ne 1 %then %do;
        proc datasets lib=work nodetails nolist nowarn;
            delete p_info _chisq_cat1_ _chisq_cat2_ 
            _chisq_adhoc_cat1_ _chisq_adhoc_cat2_;
        run;
        quit;
    %end; 

%end; %* end to cat1 cat2 pvalues loop;


%*****************************************************************************;
%*  ODDS RATIOS IF REQUESTED                                                  ;
%*****************************************************************************;
%if &NGroups eq 2 and &oddsratios eq 1 %then %do;

    %* data set in which to store ors;
    options varinitchk = nonote;
    data _oddsratios_;
        informat VarName VarLevel $200. ORUnit OddsRatioEst LowerCL UpperCL pval best.;
    run;
    %if &debug ne 1 %then %do; options varinitchk = note; %end;
    %else %if &debug eq 1 %then %do; options varinitchk = warn; %end;
    
    %macro conor(list=, type=);
    
    %put ****** RUNNING LOGISTIC FOR ORS (&type) ******;
    title9 "RUNNING LOGISTIC FOR ORS (&type)";
    
    %if &&n&type gt 0 %then %do i=1 %to &&n&type; 
    
        %let var=%scan(&list, &i);
        %if &type ne ORD1 %then %let unit=%scan(&&units&type, &i);
        %else %if &type eq ORD1 %then %let unit=;
        
        %put ****** RUNNING LOGISTIC FOR ORS (&type): %upcase(&var) ******;
        title10 "%upcase(&var)";
        
        proc logistic data=_tempd_ alpha=&alpha namelen=200;
            model __class(order=internal desc) = &var /clodds=wald orpvalue;
            %if &type ne ORD1 and %length(&unit) gt 0 %then %do;
              units &var = &unit;
            %end;              
            ods output CLOddsWald=_or_(rename=(Effect=VarName Unit=ORUnit pvalue=pval));
        run;    

        data _oddsratios_;
            informat VarName $200.;
            set _oddsratios_ _or_(in=inor);
            if inor then type="&type";
        run;
        
        proc datasets lib=work nodetails nolist nowarn;
            delete _or_;
        run; quit;
        
    %end; %*end to i to ncon;
    
    %mend conor;
    
    %conor(list=&con1, type=CON1);
    %conor(list=&con2, type=CON2);
    %conor(list=&con3, type=CON3);
    %conor(list=&ord1, type=ORD1);
    
    %put ****** RUNNING LOGISTIC FOR ORS (CAT1) ******;
    title9 "RUNNING LOGISTIC FOR ORS (CAT1)";
    
    %* want to allow for 1=yes/2=no and 1=yes/0=no coding;
    %* because of labeling it will be easier to handle separately from cat2;
    
    %if &ncat1 gt 0 %then %do i=1 %to &ncat1;
        
        %let var=%scan(&cat1, &i);
        
        %put ****** RUNNING LOGISTIC FOR ORS (CAT1): %upcase(&var) ******;
        title10 "%upcase(&var)";
        
        proc logistic data=_tempd_ alpha=&alpha namelen=200;
            format &var _yesnof.;
            class &var(ref="No")/param=ref;
            model __class(order=internal desc) = &var /clodds=wald orpvalue;
            ods output CLOddsWald=_or_(rename=(Effect=VarName pvalue=pval) drop=Unit);
        run;    
        
        data _oddsratios_;
            set _oddsratios_ _or_(in=inor);
            if inor then do;
                VarName=scan(VarName,1, " ");
                type="CAT1";
            end;
        run;
        
        proc datasets lib=work nodetails nolist nowarn;
            delete _or_;
        run; quit;
        
    %end; %*end to i to ncat1;
    
    %put ****** RUNNING LOGISTIC FOR ORS (CAT2) ******;  
    title9 "RUNNING LOGISTIC FOR ORS (CAT2)";
    
    %if &ncat2 gt 0 %then %do i=1 %to &ncat2;
        
        %let var=%scan(&cat2, &i);
        
        %put ****** RUNNING LOGISTIC FOR ORS (CAT2): %upcase(&var) ******;
        title10 "%upcase(&var)";
        
        proc logistic data=_tempd_ alpha=&alpha namelen=200;
            class &var/order=internal ref=first param=ref;
            model __class(order=internal desc) = &var /clodds=wald orpvalue;
            ods output CLOddsWald=_or_(rename=(pvalue=pval) drop=Unit);
        run;    
        
        data _oddsratios_;
            set _oddsratios_ _or_(in=inor);
            if inor then do;
                VarName=scan(Effect,1, " ");
                VarLevel="    "||
                strip(substr(
                  compbl(substr(Effect,1,index(Effect,"vs") - 1)), 
                  length(compbl(scan(Effect, 1))) + 1,
                  length(compbl(substr(Effect,1,index(Effect,"vs") - 1))) - length(compbl(scan(Effect, 1)))
                )) ;
                type="CAT2";
            end;
            drop effect;
        run;
        
        proc datasets lib=work nodetails nolist nowarn;
            delete _or_;
        run; quit;
    
    %end; %*end to i to ncat2;
    
    %*remove empty row;
    data _oddsratios_;
        set _oddsratios_;
        if _n_>1;
        VarName=upcase(VarName);
    run;
    
%end; %* end to odds ratios section;   
title9;
title10;

%*****************************************************************************;
%* STORE ALL OUTPUT IN _OUT_                                                  ;
%*****************************************************************************;
%put ****** CREATING _OUT_ DB ******;

%* combine summary stats;
data _summ_;
    informat VarName $256. Type $25.;
    set 
      %if %length(&con1)>0 %then %do; _outcon1_ %end;
      %if %length(&con2)>0 %then %do; _outcon2_ %end;
      %if %length(&con3)>0 %then %do; _outcon3_ %end;
      %if %length(&cat1 &cat2 &ord1)>0 %then %do; _outcat_ %end; ;
    VarName=upcase(VarName);     
run;      
proc sort data=_summ_; 
    by varname type
      %if %length(&cat2 &ord1)>0 %then %do; varlevel %end; ; 
run;

%* combine p-values;
%if &NGroups gt 1 and &pvalues eq 1 and &oddsratios eq 0 %then %do;
    data _pval_ (keep = VarName pval %if %length(&con1)>0 %then %do; EqVarP %end; PvalFlag type);
        informat VarName $256.;
        set 
          %if %length(&con1)>0 %then %do; _parp_ %end;
          %if %length(&con2 &con3 &ord1)>0 %then %do; _nparp_ %end;
          %if %length(&cat1 &cat2)>0 %then %do; _chisq_ %end; ;
        VarName=upcase(VarName);
    run;
    proc sort data=_pval_; by VarName type; run;
%end;

%*Combine post hoc p-values;
%if &adhoc eq 1 and (%SYSFUNC(exist(_parp_adhoc_)) or %SYSFUNC(exist(_nparp_adhoc_))
  or %SYSFUNC(exist(_chisq_adhoc_))) %then %do;
    
    %*Flag for existence;
    %let adhoc_flag=1;
    
    %*Combine;
    data _adhoc_pval_;
       informat VarName $256.;
       set
         %if %SYSFUNC(exist(_parp_adhoc_)) %then %do; _parp_adhoc_ %end;
         %if %SYSFUNC(exist(_nparp_adhoc_)) %then %do; _nparp_adhoc_ %end;
         %if %SYSFUNC(exist(_chisq_adhoc_)) %then %do; _chisq_adhoc_ %end; ;
       VarName=upcase(VarName);
    run; 
    
    %*Transpose;
    proc sort data=_adhoc_pval_; by VarName type pLab;
    proc transpose data=_adhoc_pval_ out=_adhoc_pval2_(rename=(_name_=PostHoc_AdjMethod));
        by VarName type;
        id pLab;
    run;        
    
%end;

%* merge all files;
%* if ors requested, merge _summ_ and _oddsratios 1st;
%if &oddsratios eq 1 %then %do;
    proc sort data=_oddsratios_; 
        by varname type
          %if %length(&cat2)>0 %then %do; varlevel %end; ;  
    run;
    data _summ_;
        merge _summ_ _oddsratios_;
        by varname type
          %if %length(&cat2)>0 %then %do; varlevel %end; ; 
    run; 
%end;

%*Merge the rest;
data _tout_;
    informat VarName $256.;
    merge _summ_ 
      %if &NGroups ge 2 and &pvalues eq 1 and &oddsratios eq 0 %then %do; _pval_ %end;
      %if &NGroups ge 3 and &adhoc eq 1 and %sysfunc(exist(_adhoc_pval2_)) %then %do; _adhoc_pval2_ %end;  ;
    by varname type;
run;

proc sort data=_outds_; by VarName Type;
proc sort data=_tout_; by VarName Type;
data _tout_;
    informat VarName VarLabel $256.;
    merge _outds_ _tout_;
    by varname type;
run;

%*For CAT2 and ORD1 need the label row;
%if %length(&cat2 &ord1) gt 0 %then %do;
    data _add_;
        set _tout_;
        by VarName Type;
        if first.VarName or first.type;
        where type in ("CAT2" %if &oddsratios eq 0 %then %do; "ORD1" %end;); 
        _levelorder=0;
        keep VarName VarLabel _Nmiss _n 
          %do i=1 %to &NGroups; _Nmiss&i _n&i %end; 
          _order _levelorder 
          %if &NGroups gt 1 and &pvalues eq 1 %then %do; 
            pval %if %length(&cat1 &cat2) gt 0 and %length(&FisherCatList) gt 0 %then %do; PvalFlag %end;
          %end;
          type;
    run;
    
    data _tout_; set _tout_ _add_; run; 
%end;

%*Final prep of _out_ file;
data &out;
    retain VarName VarLabel type
        %if &NGroups gt 1 %then %do;
           _Nmiss _n _m _s 
           %if %length(&con2 &con3)>0 %then %do; _t %end; 
        %end;
        %do i=1 %to &NGroups; 
            _Nmiss&i _n&i _s&i _m&i 
            %if %length(&con2 &con3)>0 %then %do; _t&i %end; 
        %end;
        %if &oddsratios=1 %then %do;
            ORUnit OddsRatioEst LowerCL UpperCL
        %end;
        %if &NGroups gt 1 and &pvalues eq 1 %then %do;
            p pval 
            %if (%length(&con1) gt 0 and &unequalvar eq 2 and &oddsratios eq 0) 
              or %length(&FisherCatList) gt 0 or %length(&ord1) gt 0 %then %do; 
                PvalFlag 
            %end;
            %if %length(&con1) gt 0 and &unequalvar eq 2 and &oddsratios eq 0 
              %then %do; 
                EqVarP 
            %end;            
        %end; 
        %if &NGroups ge 3 and &adhoc eq 1 and %sysfunc(exist(_adhoc_pval2_)) %then %do;
            PostHoc_AdjMethod %do j=1 %to &_NumComb; p_&&_AdHocLab&j %end;
        %end;
        _order _levelorder SectionHeaderFlag;
    
    set _tout_;
    
    %*pval column for sorting;
    %if &NGroups gt 1 and &pvalues=1 %then %do;
        p = pval;
    %end;     
    
    %*Clean up;
    if type not in ("CAT2" "ORD1") then do;
        _levelorder = 0;
        VarLevel = " ";
    end;
    
    if type eq "ORD1" and &oddsratios eq 1 and _levelorder=.
      then _levelorder=0;
      
    %*For CAT1 want to put 0 when all are no;
    if type = "CAT1" then do;
        if _n>0 and nmiss(_m, _s)=2 then do; _m=0; _s=0; end;
        %do i = 1 %to &NGroups;
            if _n&i>0 and nmiss(_m&i, _s&i)=2 then do; _m&i=0; _s&i=0; end;
        %end;
    end;
    
    %*For cat2 and or1 vars only want certain estimates on 1st row;
    if type in ("CAT2" "ORD1") and _levelorder > 0 then do;
        %if &ncat1 gt 0 or &ncat2 gt 0 or &nord1 gt 0 %then %do;
          VarLabel = VarLevel;
        %end;
        _N = .;
        _Nmiss = .;
        %do i = 1 %to &NGroups;
            _N&i = .;
            _Nmiss&i = .;        
        %end;        
        %if &Ngroups gt 1 and &pvalues = 1 and &oddsratios eq 0 %then %do; 
            p = .; 
            %if %length(&FisherCatList) gt 0 %then %do; PvalFlag=""; %end;
        %end;            
        %if &NGroups ge 3 and &adhoc eq 1 and %sysfunc(exist(_adhoc_pval2_)) %then %do;
            if _levelorder gt 1 then do;
                PostHoc_AdjMethod="";
                %do j = 1 %to &_NumComb; p_&&_AdHocLab&j = .; %end;
            end;                
        %end;
    end;   
        
    if type="CAT2" and _levelorder = 0 and &oddsratios = 1 then p = .;
    
    if type="ORD1" and _levelorder > 0 and &oddsratios = 1 then do;
        OddsRatioEst = .; LowerCL = .;  UpperCL = .; p = .;
    end;
    
    %*Flag ordinal p-values;
    if type="ORD1" and _levelorder = 0 and &pvalues=1 then do;
        if &NGroups = 2 then PValFlag="W";
        else if &NGroups > 2 then PValFlag="K";
    end;
    
    %*Drop uneccesary vars;
    %if &NGroups=1 %then %do;
        drop p;
    %end;        
    %if %length(&cat2 &ord1)>0 %then %do;
        drop VarLevel;      
    %end;
    %if &oddsratios ne 1 %then %do;
        drop OddsRatioEst LowerCL UpperCL;
    %end;
    %if &pvalues eq 1 and &NGroups gt 1 and %length(&con1) gt 0 and &unequalvar ne 2 and &oddsratios eq 0 
      %then %do;
        drop EqVarP; 
    %end;
run;    
proc sort data=&out; by _order _levelorder; run;  

%if &debug eq 1 %then %do;
    title9 "PROC PRINT: %upcase(&out)";
    proc print data=&out noobs;
    run;
    title9;
%end;

%if &debug ne 1 %then %do;
    proc datasets lib=work nodetails nolist nowarn;
        delete _outds_ _outcon1_ _outcon2_ _outcon3_ _outcat_ 
            _parp_ _parp_adhoc_ _nparp_ _nparp_adhoc_ _chisq_ _chisq_adhoc_
             _oddsratios_ _summ_  _pval_ _adhoc_pval_ _adhoc_pval2_ _add_ _tout_;
    run;
    quit;
%end;    

%*****************************************************************************;
%* DEFINE PVALUE TYPES FOR CONTINUOUS/ORD VARS (TO USE IN FOOTNOTES)          ;
%* THE NPVALTYPE VAR IS USED TO PICK SUPERSCRIPTS IN _RTF_                    ;
%*****************************************************************************;
%if &printfn eq 1 and &Ngroups gt 1 and &pvalues eq 1 and &oddsratios eq 0 and 
  %length(&con1 &con2 &con3 &ord1) gt 0 %then %do;

    %put ****** DEFINING P-VALUES USED ******;      
    
    %*Parametric tests;
    %if %length(&con1) gt 0 %then %do;

        %*If unequalvar eq 2 then check if we have a mix or all are the same;
        %*If only 1 type determine which;    
        %if &unequalvar eq 2 %then %do;
            
            proc sql noprint;
                select distinct(PvalFlag)
                into :PvalType separated by ' '
                from &out
                where PvalFlag not in ("F" "O" "");
            %let NPvalType=&SQLObs;
            quit; 
           
        %end;
        %else %if &unequalvar in (0 1) %then %do;
            %let NPvalType=1;
            %if &unequalvar eq 0 %then %let PValType=E;
            %if &unequalvar eq 1 %then %let PValType=U;
        %end;     
    
        %*Define p-values used;
        %if &NGroups eq 2 %then %do;
            %if &unequalvar eq 0 or (&unequalvar eq 2 and &NPvalType eq 1 and &PValType eq E) 
                %then %let par_p=t-test;
            %if &unequalvar eq 1 or (&unequalvar eq 2 and &NPvalType eq 1 and &PValType eq U) 
                %then %let par_p=Satterthwaite t-test;
        %end;
        %else %if &NGroups gt 2 %then %do;
            %if &unequalvar eq 0 or (&unequalvar eq 2 and &NPvalType eq 1 and &PValType eq E) 
                %then %let par_p=ANOVA;
            %if &unequalvar eq 1 or (&unequalvar eq 2 and &NPvalType eq 1 and &PValType eq U) 
                %then %let par_p=%str(Welch%'s ANOVA);            
        %end;
    %end;
     
    %*Non-parametric tests;
    %if %length(&con2 &con3 &ord1) gt 0 %then %do;
        %if &Ngroups eq 2 %then %let npar_p=Wilcoxon Rank Sum test;
        %else %if &Ngroups gt 2 %then %let npar_p=Kruskal-Wallis test;
    %end;

%end; %*end to type of p;

%*If unequal var does not exist, initiate;
%if %symexist(NPvalType) eq 0 %then %let NPvalType=0;
%if %symexist(PvalType) eq 0 %then %let PvalType=;

%*****************************************************************************;
%* DEFINE COLUMN WIDTHS FOR REPORT                                            ;
%* WANT TO STORE THESE IN _RTF_ IN CASE OUTSUMMARIZE IS USED                  ;
%*****************************************************************************;
%* Define column widths;
%if %length(&cwidth1) eq 0 %then %do;
    %if &printptype eq 1 %then %let cwidth1 = 90;
    %else %if &printptype eq 0 %then %let cwidth1 = 95;
%end;

%if %length(&cwidth2) eq 0 %then %do;  
    %if %length(&con2 &con3) gt 0 or &oddsratios eq 1 %then %do;
        %if &adhoc = 1 or &oddsratios eq 1 %then %let cwidth2 = 300;
        %else %let cwidth2 = 225;
    %end;        
    %else %if %length(&con2 &con3)=0 and &adhoc = 1 %then %let cwidth2=275;
    %else %let cwidth2=175;
%end;

%if %length(&cwidth3) eq 0 %then %do;
    %if &desclabel = 1 and %length(&con2 &con3) gt 0 %then %let cwidth3 = 375;
    %else %if &desclabel = 1 and %length(&con2 &con3) = 0 %then %let cwidth3 = 350;
    %else %let cwidth3 = 275;
%end;  

%*****************************************************************************;
%* PREPARE RTF DATA FOR REPORT                                                ;
%*****************************************************************************;
%put ****** CREATING _RTF_ DB ******;

%let cllab=%sysevalf((1-&alpha)*100);
/* %let PlMi=%Str(±); */
%let PlMi=(*ESC*){unicode 00B1};

options varinitchk = nonote;
data &rtfout;    
    set &out;
        
    %*descriptive labels;
    informat DescLabel $256.;
    if missing(type) then DescLabel = trim(VarLabel);
    else if type = "CON1" then do;
        if &sdopt = 1 then DescLabel = trim(VarLabel)||", mean &PlMi. sd";
        else if &sdopt = 2 then DescLabel = trim(VarLabel)||", mean (sd)";
    end;    
    else if type = "CON2" then DescLabel = trim(VarLabel)||", median (min, max)";
    else if type = "CON3" then DescLabel = trim(VarLabel)||", median [Q1, Q3]";
    else if type in ("CAT1" "CAT2" "ORD1") then do;
        if _levelorder eq 0 then DescLabel = trim(VarLabel)||", n (%)"; 
        else if _levelorder gt 0 then DescLabel = VarLabel;    
    end;
    
    %*flag label if any missing values;
    if _nmiss gt 0 then do;
        VarLabel2 = trim(VarLabel)||"*";
        DescLabel2 = trim(DescLabel)||"*";
    end;
    else if _nmiss le 0 then do;
        VarLabel2 = trim(VarLabel);
        DescLabel2 = trim(DescLabel);
    end;   

    %*Summary stats - Overall;
    %if &NGroups gt 1 and &totalcol eq 1 %then %do;
        informat Overall $65.;
        if (_n=0 and type not in ("CAT2" "ORD1")) or 
           (type in ("CAT2" "ORD") and missing(_s) and _levelorder gt 0)
           then Overall="---"; %*clean up table;
        else if type = "CON1" then do;            
            if &sdopt = 1 then 
              Overall = compress(put(_m, &f2))||" &PlMi "||
                        compress(put(_s, &f2));
            else if &sdopt = 2 then
              Overall = compress(put(_m, &f2))||" ("||
                        compress(put(_s, &f2))||")";
        end;
        else if type = "CON2" then 
          Overall = compress(put(_m, &f2))||" ("||
                    compress(put(_s, &f2))||", "||
                    compress(put(_t, &f2))||")";
        else if type = "CON3" then 
          Overall = compress(put(_m, &f2))||" ["||
                    compress(put(_s, &f2))||", "||
                    compress(put(_t, &f2))||"]";
        else if type = "CAT1" or (type in ("CAT2" "ORD1") and _levelorder gt 0) then 
          Overall = compress(put(_m, &f1))||" ("||
                    compress(put(_s, &f3))||")";                  
    %end; %*end to overall;
    
    %*Summary  stats - By Group;
    %do i=1 %to &NGroups;
        informat A&i $75.;
        if (_n&i=0 and type not in ("CAT2" "ORD1")) or 
           (type in ("CAT2" "ORD1") and missing(_s&i) and _levelorder gt 0)
           then A&i="---"; %*clean up table;
        else if type = "CON1" then do;
            if &sdopt = 1 then 
              A&i = compress(put(_m&i, &f2))||" &PlMi "||
                    compress(put(_s&i, &f2));
            else if &sdopt = 2 then
              A&i = compress(put(_m&i, &f2))||" ("||
                    compress(put(_s&i, &f2))||")";
        end;
        else if type  ="CON2" then 
          A&i = compress(put(_m&i, &f2))||" ("||
                compress(put(_s&i, &f2))||", "||
                compress(put(_t&i, &f2))||")";
        else if type = "CON3" then 
          A&i = compress(put(_m&i, &f2))||" ["||
                compress(put(_s&i, &f2))||", "||
                compress(put(_t&i, &f2))||"]";
        else if type = "CAT1" or (type in ("CAT2" "ORD1") and _levelorder gt 0) then 
          A&i = compress(put(_m&i, &f1))||" ("||
                compress(put(_s&i, &f3))||")";    
    %end; %* end to summatyr stats;
    
    %*Odds ratios, flag if units ne 1;
    %if &oddsratios eq 1 %then %do;
        informat OR_CL $65.;
        if OddsRatioEst ne . then 
        OR_CL = compress(put(OddsRatioEst, &f4))||" ("||
                compress(put(LowerCL, &f4))||", "||
                compress(put(UpperCL, &f4))||")";
        if type = "CAT2" and _levelorder ne 0 and oddsratioest = .
          then OR_CL = "reference";
        if ORUnit gt 1 then  OR_CL = trim(OR_CL)||"**";        
    %end; %* end to ors;
     
    %*p-value with superscript; 
    %if &NGroups gt 1 and &pvalues eq 1 and &oddsratios eq 0 %then %do;
        %if &printptype eq 1 %then %do;
            if type = "CON1" and p ne . then do;
                %if &unequalvar in (0 1) or &NPValType eq 1 %then %do;
                    p2 = compress(put(p, &pfmt))||"^{super a}";
                %end;
                %else %if &unequalvar eq 2 %then %do;
                    if PvalFlag="E" then p2 = compress(put(p, &pfmt))||"^{super a1}";
                    else if PvalFlag="U" then p2 = compress(put(p, &pfmt))||"^{super a2}";
                %end;                
            end;
            else if (type in ("CON2" "CON3") or (type="ORD1" and _levelorder eq 0)) and p ne .
              then p2 = compress(put(p, &pfmt))||"^{super b}";
            else if type in ("CAT1" "CAT2") and _levelorder eq 0 and p ne . then do;
                if not %length(&FisherCatList) gt 0
                  then p2 = compress(put(p, &pfmt))||"^{super c}";
                %if %length(&FisherCatList) gt 0 %then %do;
                    if PvalFlag ne "F" then p2 = compress(put(p, &pfmt))||"^{super c}";
                    else if PvalFlag eq "F" then p2 = compress(put(p, &pfmt))||"^{super d}";
                %end;
            end; 
        %end; %* end to printptype;
        format p &pfmt;
    %end; %*end to p-value superscripts;
    
    %*if post hoc comparisons done, flag sig comparisons;
    %if %symexist(adhoc_flag) %then %do;
      
        %* initiate supersript columns;
        informat %do i=1 %to &NGroups; Super&i %end; $50. ;
       
        %do i=1 %to &_NumComb;
            %let A=%scan(&_AAD, &i);
            %let B=%scan(&_BAD, &i);        

            if . lt p_&&_AdHocLab&i lt &alpha then do;
              Super&A = catx(',', Super&A, put(&B, best.));
              Super&B = catx(',', Super&B, put(&A, best.)); 
            end;              
        %end;

        %*If appropriate, add supersript denoting sign.;
        %do LP=1 %to &NGroups;
            if Super&LP ne " " then Super&LP="^{super "||compress(Super&LP)||"}";
            if Super&LP ne " " and ( type not in ("CAT2" "ORD1") or
              (type in ("CAT2" "ORD1") and _levelorder=1))
                then A&LP=catx('',trim(A&LP),trim(Super&LP));
        %end;
    %end; **end to post hoc section;
    
    %*Combine _order and _levelorder into one variable;
    _roworder = _order + (_levelorder/100);
    
    %*Data to store for OUTSUMMARIZE use;
    if _N_ = 1 then do;
        _nlev_ = &NGroups;
        _NTotal_ = &TotalN;
        %do i=1 %to &NGroups;
            _label&i = "&&GroupLab&i";
            _GroupN&i = &&GroupN&i;
        %end;
        _totalcol_=&totalcol;
        _ncol_=&NCOL;
        _misscol_=&MISSCOL;
        _subset_="&SUBSET.";
        _colpct_=&COLPCT;
        _adhoc_=&ADHOC;
        %if %symexist(padj_method) %then %do; _padj_method_="&padj_method"; %end;
        %else %do; _padj_method_=""; %end;
        _oddsratios_=&oddsratios;
        _orunits_=%length(&unitscon1)>0 or %length(&unitscon2)>0 or %length(&unitscon3)>0;
        %if %symexist(ChiSqCatList) %then %do;  _chisqcatlist_=%length(&ChiSqCatList) gt 0; %end;
        %else %do; _chisqcatlist_=0; %end;
        %if %symexist(FisherCatList) %then %do; _fishercatlist_=%length(&FisherCatList) gt 0; %end;
        %else %do; _fishercatlist_=0; %end;
        _unequalvar_=&unequalvar;
        _desclabel_=&desclabel;
        _boldedsigp_=&boldedsigp;
        _boldedlabel_=&boldedlabel;
        _alpha_=&alpha;
        _pvalues_=&pvalues;
        _printptype_=&printptype;
        _f1_="&f1";
        _pfmt_="&pfmt";
        _cwidth1_=&cwidth1;
        _cwidth2_=&cwidth2;
        _cwidth3_=&cwidth3;
        _sdopt_=&sdopt;
        _con1_=%length(&con1) gt 0;
        _con2_=%length(&con2) gt 0;
        _con3_=%length(&con3) gt 0;
        _cat1_=%length(&cat1) gt 0;
        _cat2_=%length(&cat2) gt 0;
        _ord1_=%length(&ord1) gt 0;
        _printfn_=&printfn;
    end;
    
run;
options varinitchk = note;

%*order and drop vars;
data &rtfout;
    retain varname varlabel type  _Nmiss _n 
        %if &NGroups gt 1 and &totalcol eq 1 %then %do; Overall %end;
        %do i=1 %to &NGroups; _Nmiss&i _n&i A&i %end;
        %if &oddsratios eq 1 %then %do; orunit or_cl %end;
        %if &NGroups gt 1 and &pvalues eq 1 %then %do;
            p 
            %if &printptype eq 1 and &oddsratios eq 0 %then %do; p2 %end;
            %if (%length(&con1) gt 0 and &unequalvar eq 2 and &oddsratios eq 0) 
              or %length(&FisherCatList) gt 0 or %length(&ord1) gt 0 %then %do; 
                PvalFlag 
            %end;
            %if (%length(&con1) gt 0 and &unequalvar eq 2 and &oddsratios eq 0) %then %do; EqVarP %end;
        %end;
        %if %symexist(adhoc_flag) %then %do; 
            PostHoc_AdjMethod %do j=1 %to &_NumComb; p_&&_AdHocLab&j %end;
          %end;     
        %if &NGroups gt 1 and &pvalues eq 1 %then %do; pval %end; 
        _roworder SectionHeaderFlag desclabel varlabel2 desclabel2 
        _nlev_ _NTotal_ _label1-_label&NGroups _GroupN1-_GroupN&NGroups 
        _subset_ _colpct_ _totalcol_ _ncol_ _misscol_ _adhoc_ _padj_method_ 
        _oddsratios_ _orunits_ _chisqcatlist_ _fishercatlist_ _unequalvar_ 
        _desclabel_ _boldedsigp_ _boldedlabel_ _alpha_ _pvalues_ 
        _printptype_ _f1_ _pfmt_ _cwidth1_ _cwidth2_ _cwidth3_ _sdopt_ 
        _con1_ _con2_ _con3_ _cat1_ _cat2_ _ord1_ _printfn_;
    set &rtfout;
    keep varname varlabel type  _Nmiss _n 
        %if &NGroups gt 1 and &totalcol eq 1 %then %do; Overall %end;
        %do i=1 %to &NGroups; _Nmiss&i _n&i A&i %end;
        %if &oddsratios eq 1 %then %do; orunit or_cl %end;
        %if &NGroups gt 1 and &pvalues eq 1 %then %do;
            p 
            %if &printptype eq 1 and &oddsratios eq 0 %then %do; p2 %end;
            %if (%length(&con1) gt 0 and &unequalvar eq 2 and &oddsratios eq 0) or 
              %length(&FisherCatList) gt 0 or %length(&ord1) gt 0 %then %do; 
                PvalFlag 
            %end;
            %if (%length(&con1) gt 0 and &unequalvar eq 2 and &oddsratios eq 0) %then %do; EqVarP %end;
        %end;
        %if %symexist(adhoc_flag) %then %do; 
            PostHoc_AdjMethod %do j=1 %to &_NumComb; p_&&_AdHocLab&j %end;
          %end;     
        %if &NGroups gt 1 and &pvalues eq 1 %then %do; pval %end; 
        _roworder SectionHeaderFlag desclabel varlabel2 desclabel2 
        _nlev_ _NTotal_ _label1-_label&NGroups _GroupN1-_GroupN&NGroups 
        _subset_ _colpct_ _totalcol_ _ncol_ _misscol_ _adhoc_ _padj_method_ 
        _oddsratios_ _orunits_ _chisqcatlist_ _fishercatlist_ _unequalvar_ 
        _desclabel_ _boldedsigp_ _boldedlabel_ _alpha_ _pvalues_ 
        _printptype_ _f1_ _pfmt_ _cwidth1_ _cwidth2_ _cwidth3_ _sdopt_ 
        _con1_ _con2_ _con3_ _cat1_ _cat2_ _ord1_ _printfn_;
run;    

%* sort;
proc sort data=&rtfout;
    by 
      %if %length(&sortby) gt 0 and %upcase(&sortby) ne _LIST %then %do; &sortby %end;
      _roworder ;
run;

%*****************************************************************************;
%* GENERATE FOOTNOTE FOR MISSING VALUES IF NCOL=0 AND MISSCOL=0               ;
%*****************************************************************************;
%if &ncol eq 0 and &misscol eq 0 %then %do;

    %put ****** CREATING MISSING VALUES FOOTNOTE ******;
   
    proc sql noprint;
        select 
        unique trim(varlabel)||" = "||compress(put(_Nmiss, comma32.)), 
        min(_roworder) as ord 
        into :MissNote separated by '; ', :_dummy
        from &rtfout
        where _nmiss gt 0
        group by varlabel
        order by ord;
    quit;    
    %if %symexist(missnote) eq 1 %then %do;
        %let MissNote=&MissNote; %*remove trailing blanks;
    %end;   
    
%end; %*end to if ncol=0 and miscol=0;

%*****************************************************************************;
%* GENERATE FOOTNOTE FOR ODDS RATIOS IF ANY UNIT ne 1 SPECIFIED               ;
%*****************************************************************************;
%if &oddsratios eq 1 and (%length(&unitscon1)>0 or %length(&unitscon2)>0 or %length(&unitscon3)>0 )
  %then %do;

    %put ****** CREATING OR UNITS VALUES FOOTNOTE ******;
   
    proc sql noprint;
        select trim(varlabel)||" = "||compress(put(ORUnit, best.))
        into :ORNote separated by '; ' 
        from &rtfout
        where ORUnit not in (1 .);
    quit;        
    %if %symexist(ornote) eq 1 %then %do;
        %let ORNote=&ORNote; %*remove trailing blanks;
    %end;   
    
%end; %*end to or units footnote;

%*****************************************************************************;
%* MISC SET UP FOR REPORT                                                     ;
%*****************************************************************************;
%put ****** SET UP OF REPORT OPTIONS ******;

%* Change margins;
ods path reset;
ODS path (REMOVE) WORK.TEMPLAT;
ODS path (PREPEND) work.TEMPLAT(update);

proc template;
    define style myrtf;
    parent=styles.rtf;
    style body from document /
        leftmargin=0.5in
        rightmargin=0.5in
        topmargin=0.5in
        bottommargin=0.5in;
    end;
run;

%* Format the N macro vars for the headers;
data _null_;
    call symput('TotalN', strip(put(&TotalN, &f1)));
    %do i=1 %to &NGroups; 
        call symput("GroupN&i", strip(put(&&GroupN&i, &f1)));
    %end;
run;

%* clear date, page number options;
options nodate nonumber nocenter noquotelenmax;

%* date stamp in case it is requested;
data _null_;
    datenow = today(); timenow=datetime();
    call symput('datenow', trim(left(put(datenow, weekdate.))));
    call symput('timenow', trim(right(put(timenow, tod5.2))));
run;

%* Define font size for footnotes;
%let linesize = %sysevalf(&fontsize - 2);

%* if no title given, generic one;
%if %length(&tbltitle)=0 %then %let tbltitle="Data Summary";

%* main title/footnote location;
%if &bdytitle=1 %then %let bd=bodytitle;
%else %let bd=;

%* Add footnotes if appropriate;
%if (&fn ge 1) %then %do floop = 1 %to &FN;
    footnote&floop "&&foot&floop";
%end;
%let fplus1 = %eval(&fn + 1);
%let fplus2 = %eval(&fn + 2);

%if &daytime eq 1 %then %do;
    footnote&FPLUS1 h=8pt j=r "&timenow  &datenow";
%end;


%if %length(&rtffile) gt 0 %then %do; %let file=&rtffile; %let minus=4; %end;
%else %if %length(&pdffile) gt 0 %then %do; %let file=&pdffile; %let minus=4; %end;
%else %if %length(&xlsxfile) gt 0 %then %do; %let file=&xlsxfile; %let minus=5; %end;
%else %if %length(&xmlfile) gt 0 %then %do; %let file=&xmlfile; %let minus=4; %end;

%if %symexist(file) %then %do;
    data _null_;
         call symput('filepath', substr(dequote(&file), 1, length(dequote(&file))-&minus));
    run;
    
    footnote&FPLUS2 h=8pt j=l "id: &filepath";
%end; %*end to file path footnote;

%* ODS escape character so that the superscripts work;
ods escapechar="^";

%* Paper orientation ;
options orientation = &PAGE;

%*****************************************************************************;
%* GENERATE REPORT                                                            ;
%*****************************************************************************;
%put ****** CREATING REPORT TABLE ******;

%* return printing to window in SAS Studio;
%* will not return in interactive sas because PROC REPORT generates an error
   about a column width (that does not affect output);
ods select all;
ods results;
ods listing close;

%* Open ODS Destinations;
%if %length(&rtffile)>0 %then %do;
    ods rtf file = &rtffile style = myrtf &bd;
%end;

%if %length(&pdffile)>0 %then %do;
    ods pdf file = &pdffile style = myrtf bookmarkgen = no bookmarklist = no title = &tbltitle;
%end;

%if %length(&xmlfile)>0 %then %do;
    ods tagsets.ExcelXP file = &xmlfile
        options(sheet_name = &tbltitle
          %if &bdytitle eq 1 %then %do; embedded_titles='yes' embedded_footnotes='yes' %end;
        );
%end;

%if %length(&xlsxfile)>0 %then %do;
    ods excel file = &xlsxfile
        options(sheet_name = &tbltitle flow='Tables'
          %if &bdytitle eq 1 %then %do; embedded_titles='yes' embedded_footnotes='yes' %end;
        );
%end;

%* Report;
proc report data= &RTFOUT nowd /*list*/ ls=256
    style(report)={/*borderwidth=3*/ bordercolor=black cellpadding=3
        %if %length(&outputwidth) ne 0 %then %do; outputwidth=&outputwidth.% %end;
        font_size=&fontsize.pt font_face=&fontface  FONTSTYLE= Roman}

    style(lines)={background=white foreground=black
        font_size=&linesize.pt font_face=&fontface  FONTSTYLE= Roman
        protectspecialchars=off}

    style(column)={background=white foreground=black bordercolor=black
        font_size=&fontsize.pt font_face=&fontface  FONTSTYLE= Roman
        font_weight=medium}

    style(header)={background=white foreground=black font_weight=bold bordercolor=black
        font_size=&fontsize.pt font_face=&fontface  FONTSTYLE= Roman};

    columns
    
     /* LABEL */   
     (%if &ncol eq 0 and &misscol eq 0 %then %do; 
         %if &desclabel eq 1 %then %do; desclabel2 %end;
         %else %do; varlabel2 %end;
      %end;
      %else %if &ncol ne 0 or &misscol ne 0 %then %do;
         %if &desclabel eq 1 %then %do; desclabel %end;
         %else %do; varlabel %end;
      %end; SectionHeaderFlag MyLabel)   
    
    /* TOTAL COLUMNS (the ^S is to trick it to put a black line top to bottom) */
    %if &NGroups gt 1 and (&totalcol eq 1 or &ncol in (1 3) or  &misscol in (1 3)) %then %do;
        (%if &totalcol eq 1 and (&ncol in (1 3) or  &misscol in (1 3))
           %then %do; "Overall/(N=&TotalN)" %end;
        %if &ncol in (1 3) %then %do; _N %end;
        %if &misscol in (1 3) %then %do; _NMiss %end;
        %if &ncol eq 2 or &misscol eq 2 %then %do; "^S={foreground=white}." %end;
        %if &totalcol eq 1 %then %do; Overall %end; )
    %end;
    
    /* GROUP COLUMNS */
    %do i=1 %to &NGroups;
    (%if &ncol in (2 3) or &misscol in (2 3) %then %do; "&&GroupLab&i./(N=&&GroupN&i.)" %end;
    %if &ncol in (2 3) %then %do; _n&i %end;
    %if &misscol in (2 3) %then %do; _nmiss&i %end;
    %if &ncol eq 1 or &misscol eq 1 %then %do; "^S={foreground=white}." %end; A&i)  
    %end;
    
    /* ODDS RATIOS */
    %if &oddsratios eq 1 %then %do; ("^S={foreground=white}." or_cl) %end;
    
    /* P-VALUES */
    %if &NGroups gt 1 and &pvalues eq 1 %then %do;
    (%if &oddsratios eq 1 %then %do; "^S={foreground=white}." %end;
    p %if &printptype eq 1 and &oddsratios eq 0 %then %do; p2 %end; 
    %if &printptype eq 0 and &oddsratios eq 0 and 
      (%length(&FisherCatList) gt 0 or %length(&ord1) gt 0 or &NPvalType gt 1)
      %then %do; Type PvalFlag 
    %end;
    _pvalue) 
    %end;    
    ; **closing columns statement;

    %* Title;
    compute before _PAGE_ /
      style = {font_weight = bold
      font_size = &fontsize.pt font_face = &fontface  FONTSTYLE = Roman
      just = left /*borderbottomwidth = 3*/
      borderbottomcolor = black bordertopcolor = white 
      borderrightcolor = white borderleftcolor = white };
        line &TBLTITLE;
    endcomp;  
    
    %* Label column;
    define MyLabel / computed "Factor"  left order = data
       style(header) = {just = left 
         %if &ncol in (2 3) or &misscol in (2 3) or
          (&NGroups gt 1 and &totalcol eq 1 and (&ncol in (1 3) or  &misscol in (1 3)))
           %then %do; bordertopcolor = white %end; }
       style(column) = {asis=on cellwidth = &CWIDTH3
         %if &boldedlabel=1 %then %do; font_weight = bold %end;} ;

    define SectionHeaderFlag / display noprint;    
    define
      %if &ncol eq 0 and &misscol eq 0 %then %do; 
         %if &desclabel eq 1 %then %do; desclabel2 %end;
         %else %do; varlabel2 %end;
      %end;
      %else %if &ncol ne 0 or &misscol ne 0 %then %do;
         %if &desclabel eq 1 %then %do; desclabel %end;
         %else %do; varlabel %end;
      %end; /display noprint;
    
    compute MyLabel / character length = 256 ;
        MyLabel = 
          %if &ncol eq 0 and &misscol eq 0 %then %do; 
             %if &desclabel eq 1 %then %do; desclabel2 %end;
             %else %do; varlabel2 %end;
          %end;
          %else %if &ncol ne 0 or &misscol ne 0 %then %do; 
             %if &desclabel eq 1 %then %do; desclabel %end;
             %else %do; varlabel %end;
          %end; ;
        if SectionHeaderFlag = 1 then call define(_col_, "style",
          "style=[font_weight=bold fontstyle=italic]");     
    endcomp;
    
    %* Total N, N missing, summary;
    %if &NGroups gt 1 and 
      (&ncol in (1 3) or &misscol in (1 3) or &totalcol eq 1) %then %do;    
        
        %if &ncol in (1 3) %then %do;  
            define _n / 
              %if &totalcol eq 1 %then %do; "N" %end;
              %else %do; "Total N" %end;
              format=&f1
              style(column)={just = center vjust=center cellwidth = &CWIDTH1};
        %end;
       
        %if &misscol in (1 3) %then %do;  
            define _nmiss / 
              %if &totalcol eq 1 %then %do; "N missing" %end;
              %else %do; "Total N missing" %end;
              format=&f1
              style(column)={just = center vjust=center cellwidth = &CWIDTH1};
        %end;       
       
        %if &totalcol eq 1 %then %do; 
            define Overall / display 
              %if &ncol in (1 3) or  &misscol in (1 3) %then %do; "Statistics" %end;
              %else %do; "Total/(N=&TotalN.)" %end;
              %if &ncol eq 2 or &misscol eq 2 %then %do; 
                style(header) = {bordertopcolor = white }
              %end;              
              style(column)={just = center vjust=center cellwidth = &CWIDTH2};        
        %end;
        
    %end; %* end to overall section;

    %* N, n miss, summary by group;
    %do i=1 %to &NGroups;
    
        %if &ncol in (2 3) %then %do;
            define _n&i / "N" format=&f1
              style(column)={just = center vjust=center cellwidth = &CWIDTH1};            
        %end;
        
        %if &misscol in (2 3) %then %do;
            define _nmiss&i / "N missing" format=&f1
              style(column)={just = center vjust=center cellwidth = &CWIDTH1};            
        %end;          

        define A&i / display 
          %if &ncol in (0 1) and &misscol in (0 1) %then %do; "&&GroupLab&i./(N=&&GroupN&i.)" %end;
          %else %do; "Statistics" %end; 
          %if &ncol eq 1 or &misscol eq 1 %then %do; 
            style(header) = {bordertopcolor = white }
          %end;
          style(column)={just = center vjust=center cellwidth = &CWIDTH2
          protectspecialchars=off};   
    
    %end; %* end to i to NGroups;
    
    %*Odd ratios, if requested;
    %if &oddsratios eq 1 %then %do;
        
        define or_cl/display "OR (&CLLab.% CI)"
          %if &ncol ne 0 or &misscol ne 0 %then %do;
           style(header) = {bordertopcolor = white }
          %end;
          style(column)={just = center vjust=center cellwidth = &CWIDTH2
          protectspecialchars=off}; 
    
    %end; %* end to odds ratio column;

    %* P-value column;
    %if &NGroups gt 1 and &pvalues eq 1 %then %do;

        define p/display noprint;
        
        %if &printptype eq 1 and &oddsratios eq 0 %then %do;
          define p2/display noprint;
        %end;
        
        %if &printptype eq 0 and &oddsratios eq 0 and 
          (%length(&FisherCatList) gt 0 or %length(&ord1) gt 0 or &NPvalType gt 1) %then %do;
            define Type/display noprint;
            define PvalFlag/display noprint;
        %end;
        
        define  _pvalue / computed "p-value"
          %if &ncol ne 0 or &misscol ne 0 %then %do;
            style(header)={bordertopcolor=white}
          %end;
          style(column) = {just = center vjust=center cellwidth = &CWIDTH1};

        compute _pvalue/character length=36;
            %if &printptype eq 1 and &oddsratios eq 0 %then %do;
                _pvalue=p2;
            %end;
            %else %if &printptype eq 0 and &oddsratios eq 0 %then %do;
                if (type in ("CAT1" "CAT2") and %length(&FisherCatList) gt 0) or
                    type="ORD1" or
                   (type="CON1" and &NPvalType gt 1)
                    then _pvalue=compress(put(p, &pfmt))||PvalFlag;
                else _pvalue=compress(put(p, &pfmt));
            %end;            
            %else %do;
                _pvalue=compress(put(p, &pfmt));
            %end;                
            %if &boldedsigp eq 1 %then %do;
                if p < &alpha then call define(_col_,
                "style", "style=[font_weight=bold fontstyle=italic]");
            %end;
        endcomp;
           
    %end; %* end to pval section;
    
    %*Footnotes;
    compute after/style(lines)={just=left /*bordertopwidth=3 */
      borderbottomcolor = white borderrightcolor = white borderleftcolor = white };
        
        %if %length(&subset)>0 %then %do;
            line "Subset of population used: &SUBSET..";
        %end;
        
        %if %symexist(missnote) eq 1 %then %do;
            line "*Data not available for all subjects. Missing values: &MissNote..";
        %end;
    
        %if &printfn eq 1 and (&NGroups eq 1 or &pvalues eq 0 or &printptype eq 1) %then %do;
            line "Statistics presented as "
              %if %length(&con1)>0 %then %do;
                %if &sdopt eq 1 %then %do; "Mean &PlMi. SD" %end;
                %else %do; "Mean (SD)" %end;
              %end;
              %if %length(&con2)>0 %then %do; 
                %if %length(&con1)>0 %then %do; ", " %end;
                "Median (min, max)" 
              %end;
              %if %length(&con3)>0 %then %do; 
                %if %length(&con1 &con2)>0 %then %do; ", " %end;
                "Median [P25, P75]" 
              %end;
              %if %length(&cat1 &cat2 &ord1)>0 %then %do;
                %if %length(&con1 &con2 &con3)>0 %then %do; ", " %end;
                %if &colpct eq 1 %then %do; "N (column %)" %end;
                %else %do; "N (row %)" %end;
              %end;
              ".";
        %end;
        
        %if &NGroups gt 1 and &printfn eq 1 and &pvalues eq 1 and &printptype eq 1 and &oddsratios eq 0 %then %do;
            line "p-values: "
              %if %length(&con1)>0 %then %do; 
                %if &NPvalType eq 1 %then %do; 
                    "a=&par_p" 
                  %end; 
                %else %if &NGroups eq 2 and &NPvalType gt 1 %then %do;
                   "a1=t-test, a2=Satterthwaite t-test" 
                %end;
                %else %if &NGroups gt 2 and &NPvalType gt 1 %then %do;
                   "a1=ANOVA, a2=Welch's ANOVA" 
                %end;                
              %end;
              %if %length(&con2 &con3 &ord1)>0 %then %do; 
                %if %length(&con1)>0 %then %do; ", " %end;
                "b=&npar_p"
              %end;
              %if %length(&cat1 &cat2)>0 %then %do;
                %if %length(&con1 &con2 &con3 &ord1)>0 %then %do; ", " %end;
                %if %length(&ChiSqCatList) gt 0 %then %do; 
                  "c=Pearson's chi-square test" 
                %end;
                %if %length(&FisherCatList) gt 0 %then %do;
                  %if %length(&ChiSqCatList) gt 0 %then %do;  ", " %end;
                  "d=Fisher's Exact test" 
                %end;
              %end;
              ".";
        %end;

        %if &NGroups gt 1 and &printfn eq 1 and &pvalues eq 1 and &printptype eq 0 and &oddsratios eq 0 %then %do;
            line "Statistics presented as "
              %if %length(&con1)>0 %then %do; 
                "Mean &PlMi. SD with " 
                %if &NPvalType eq 1
                  %then %do;  
                    "&par_p" 
                %end;
                %else %if &NGroups eq 2 and &NPvalType gt 1 %then %do;
                   "pooled-variance (E) or Satterthwaite (U) t-test" 
                %end;
                %else %if &NGroups gt 2 and &NPvalType gt 1 %then %do;
                   "ANOVA (E) or Welch's ANOVA (U)" 
                %end;                   
              %end;            
              %if %length(&con2 &con3)>0 %then %do;
                %if %length(&con1)>0 %then %do; "; " %end;
                %if %length(&con2) eq 0 %then %do; "Median [P25, P75]" %end;
                %if %length(&con3) eq 0 %then %do; "Median (min, max)" %end;
                %else %if %length(&con2)>0 and %length(&con3)>0 %then %do; 
                  "Median [P25, P75] or Median (min, max)" 
                %end;
                " with &npar_p" 
              %end;
              %if %length(&cat1 &cat2 &ord1)>0 %then %do;
                %if %length(&con1 &con2 &con3)>0 %then %do; "; " %end;
                %if &colpct eq 1 %then %do; "N (column %) with " %end;
                %else %do; "N (row %) with " %end;
                %if %length(&ord1)>0 %then %do; 
                  "&npar_p "
                  %if &NGroups eq 2 %then %do; "(W)" %end;
                  %else %if &NGroups gt 2 %then %do; "(K)" %end;
                %end;                
                %if %length(&FisherCatList) gt 0 %then %do; 
                  %if %length(&ord1)>0 %then %do; ", " %end;
                  "Fisher's Exact test (F)" 
                %end;
                %if (%length(&ord1)>0 or %length(&FisherCatList) gt 0) and %length(&ChiSqCatList) gt 0 %then %do; 
                  " or" 
                %end;
                %if %length(&ChiSqCatList) gt 0 %then %do; 
                  " Pearson's chi-square test" 
                %end;
              %end;
              ".";
        %end;
 
        %if &NGroups gt 2 and &adhoc eq 1 and &pvalues eq 1 and %symexist(padj_method) eq 1 %then %do;
            %do fnloop = 1 %to &NGroups;
                line "^{super &FNLoop}: Significantly different from &&GroupLab&FNLoop";
            %end;
                line "Post-hoc pairwise comparisons were done using &padj_method adjustment.";
        %end;        
        
        %if &ngroups eq 2 and &oddsratios eq 1 %then %do;
            %if &printfn eq 1 %then %do;
                line "Odds Ratios (OR)"
                  %if &pvalues eq 1 %then %do; ", " %end;
                  %else %do; " and " %end;
                  "Confidence Intervals (CI)"
                  %if &pvalues eq 1 %then %do; " and p-values" %end;
                  " correspond to univariate logistic regression models.";
            %end;
            %if %symexist(ORNote) %then %do;
                line "**Odds ratio correspond to following unit increments: &ORNote..";
            %end;     
        %end;

        %if %length(&addfn)>0 %then %do;
            line &addfn;
        %end;        
                    
    endcomp;
    
run;

%*Close ODS destinations (do not want to use _all_ option as it closes the output window in SAS Studio;        
%if %length(&rtffile)>0 %then %do;
    ods rtf close;
%end;

%if %length(&pdffile)>0 %then %do;
    ods pdf close;
%end;

%if %length(&xmlfile)>0 %then %do;
    ods tagsets.ExcelXP close;
%end;

%if %length(&xlsxfile)>0 %then %do;
    ods excel close;
%end;

%*****************************************************************************;
%* IF 3+ GROUPS AND UNEQUALVAR=2 PUT DISCLAIMER                               ;
%*****************************************************************************;
%if &NGroups gt 2 and &unequalvar eq 2 and 
  (&NPvalType eq 2 or (&NPvalType eq 1 and &PValType eq U))
  %then %put 
WARNING: Homogeneity of variance tests have too little power to be relied upon
to always detect when Welch’s ANOVA is appropriate. Unless the group variances
are extremely different or the number of groups is large, the usual ANOVA test
is relatively robust when the groups are all about the same size.
Graphical diagnostics can be a useful informal tool for monitoring whether
your data meet the assumptions of a GLM analysis.;

%*****************************************************************************;
%* MEND MACRO                                                                 ;
%*****************************************************************************;
ods listing;
ods select all;
ods results;
ods path (REMOVE) sasuser.TEMPLAT;
ods path reset;
options nomlogic nomlogicnest orientation = portrait; 
footnote&FPLUS1;
footnote&FPLUS2;

%let FisherCAT1List=; %let FisherCAT2List=;
%let ChiSqCAT1List=; %let ChisqCAT2List=;

%if &debug ne 1 %then %do;
    proc datasets lib=work nodetails nolist nowarn;
        delete _tempd_ _f;
    run;
    quit;        
%end;

%mend summarize;
