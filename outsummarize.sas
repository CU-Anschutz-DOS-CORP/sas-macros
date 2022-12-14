/**
 @file
 @brief Output multiple tables created by %summarize to a single file

 @details
 This macro reads in one or more datasets created by %SUMMARIZE (RTFOUT=) 
 and outputs a single RTF, PDF and/or XLSX file with all the tables.

 @author Rocio Lopez

 @date 02-12-2019

 @version SAS 9.4

 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %outsummarize (
       rtfin= 
      ,rtffile= 
      ,pdffile= 
      ,xlsxfile= 
      ,xmlfile= 
      ,titles= 
      ,page=portrait
      ,newpage=1
      ,bdytitle=0
      ,addfn= 
      ,daytime=0
      ,fontsize=11
      ,fontface=Times
      ,OutputWidth=
      );
 @endcode

 @returns
 SAS output listing and RTF, PDF and/or XLSX files, if requested.

 @param rtfin A list of one or more RTFOUT dataset(s) created by %SUMMARIZE

 @param titles Provide optional titles for each table, each title should 
 be surrounded by double quotes

 @param rtffile Path and filename for optional RTF output file
 @n It should be surrounded in quotes, e.g. "~userid/consult/out.rtf"

 @param pdffile Path and filename for optional PDF output file
 @n It should be surrounded in quotes, e.g. "~userid/consult/out.pdf"
 
 @param xlsxfile Path and filename for optional XLSX output file
 @n It should be surrounded in quotes, e.g. "~userid/consult/out.xlsx"

 @param xmlfile Path and filename for optional XML output file
 @n It should be surrounded in quotes, e.g. "~userid/consult/out.xml"

 @param page Specify page orientation for output files
 @n Allowed options are:
 @n PORTRAIT (DEFAULT)
 @n LANDSCAPE

 @param newpage Control when new pages are started
 @n Allowed options are:
 @n 1=Start each table on a new page (DEFAULT)
 @n 0=New tables start on same page as last table finished

 @param bdytitle Location of SAS system titles/footnotes on RTF file
 @n Allowed options are:
 @n 0=Put Titles in Header and Footnotes in Footer (DEFAULT) 
 @n 1=Put Main Title and Footnotes in the Body of the RTF file

 @param addfn Optional user-defined footnotes for each table, each 
 footnote should be surrounded by double quotation marks

 @param daytime Add a date/time stamp as a footnote to the ods files.
 @n Allowed options are:
 @n 0 = No date stamp (DEFAULT)
 @n 1 = Yes, include date/time stamp

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
 @li You will use the parameters in the %SUMMARIZE macro to control the
 analysis display and ordering of the variables
 @li Using NEWPAGE=0 will turn off main titles (not table titles) and
 footnote (used to report day and time of running the macro)
 @li Each table will appear on a separate sheet of the Excel file, the 
 NEWPAGE option has no effect on this type of file
 @li Unless running on SAS Studio, output might not look right in results 
 window but styling in RTF file will be fine   
 @endparblock

 @sa summarize.sas

 @par Example
 @code{.sas}
 title1 'Sample execution of OUTSUMMARIZE';
 footnote "Randomly generated data";

 proc format;
   value groupf  1="Group A" 2="Group B";
  run;

 data a1 a2 a3;
   do type=1 to 3;
      do Group=1 to 2;
        do i=1 to 100;
           X1 = rannor(0);
           X2 = 100*ranexp(0);
           X4 = ranbin(0, 1, 0.5);
           if type=1 then output a1;
           if type=2 then output a2;
           if type=3 then output a3;
         end;
       end;
     end;
   label X1="Normal variate"
         X2="Exponential variate"
         X4="Bernoulli variate";
    format group groupf.;
  run;

 %summarize(ds=a1, rtfout=rtf1, class=group,
            con1=x1, con2=x2, cat1=x4);

 %summarize(ds=a2, rtfout=rtf2, class=group,
            con1=x1, con3=x2, cat1=x4);

 %summarize(ds=a3, rtfout=rtf3, class=group,
            con1=x1 x2, cat1=x4);

 %outsummarize(rtfin=rtf1 rtf2 rtf3,
               rtffile="summ.rtf",
               titles="Type 1" "Type 2" "Type 3");
 @endcode
 
 @par Revision History
 @b 3-18-2022 Can now specify library name in RTFIN
 @n @b 04-20-2021 Corrected typo on one of the automatically generated
  footnotes
  @n @b 12-05-2019 Handles removed period used for indentation of CAT var
  levels in SUMMARIZE. 
  @n @b 06-18-2019 Fixed sheet titles on Excel output files
  @n @b 06-07-2019 Fixed issue where missing value footnotes were being 
  carried to subsequent tables.
  @n @b 05-16-2019 Fixed footnotes
**/

%macro outsummarize (
    rtfin=,
    
    rtffile=,
    pdffile=,
    xlsxfile=,
    xmlfile=,
    
    titles= ,
    
    page=portrait,
    newpage=1,
    bdytitle=0,
    
    addfn= ,
    daytime=0,
    
    fontsize=11,
    fontface=Times,
    OutputWidth=);
             
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
options minoperator noquotelenmax;

%*At least one RTFIN file given;
%if %length(&rtfin) eq 0 %then %do;
    %PUT ERROR: Required parameter RTFIN not provided. Macro will stop executing.;
    %LET ERRORFLAG=1;
%end;

%*There should be no quotes in RTFIN;
%if %length(&rtfin)>0 and %sysevalf(%sysfunc(indexc(&rtfin, '"')) gt 0) %then %do;
  %put
    ERROR: Double quotes should not be used in RTFIN. Macro will stop executing.;
    %let ErrorFlag=1;
%end;

%*All RTFIN files exist;
%let rtfi=1;
%let rtfnow=%qscan(&rtfin, &rtfi, " ");
%do %until(&rtfnow= );
    %let OK=%sysfunc(exist(&rtfnow, data));
    %if &OK=0 %then %do;
        %put ERROR: Data set &RTFNOW does not exist. Macro will stop excecuting.;
        %let ERRORFLAG=1;
    %end;
    %let rtfi=%eval(&rtfi+1);
    %let rtfnow=%qscan(&rtfin, &rtfi, " ");
%end;

%* If no ods destination given, then put note but proceed;
%if %length(&rtffile) eq 0 and %length(&pdffile) eq 0 and 
  %length(&xlsxfile) eq 0 and %length(&xmlfile) eq 0 %then %do;
    %put NOTE: No external file destination specified. Output will only be printed in results window.;
%end;

%* RTFFILE, PDFFILE, XMLFILE, XLSXFILE, TITLES and ADDFN should have quotes;
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
%quotecheck(option = &titles,   name= titles);
%quotecheck(option = &addfn,    name= addfn);

%* check 0/1 parameters;
%macro optcheck(option= , newoption= , default= );
    %if not(&option  in (0 1)) %then %do;
        %put WARNING: &NEWOPTION must be either 0 or 1.;
        %put WARNING: &NEWOPTION = **&option**;
        %put WARNING: Default value of &default will be used for &newoption.;
        %let &newoption=&default;
    %end; %*end to option check;
%mend optcheck;

%optcheck(option=&newpage,  newoption=newpage,  default=1)
%optcheck(option=&bdytitle, newoption=bdytitle, default=0);
%optcheck(option=&daytime,  newoption=daytime,  default=0);

%* Check valid options given for other parameters;
%if not(%upcase(&page)  in (PORTRAIT LANDSCAPE)) %then %do;
    %put WARNING: PAGE must be either PORTRAIT or LANDSCAPE.;
    %put WARNING: PAGE = **&page**;
    %put WARNING: Default value of PORTRAIT will be used for PAGE.;
    %let page=portrait;
%end; %*end to page check;

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

%*****************************************************************************;
%* IF ANY ERRORS FOUND THEN ABORT EXECUTION, OTHERWISE PROCEED                ;
%*****************************************************************************;
%if &errorflag eq 1 %then %do;
    data _null_;
        abort 3;
    run;
%end;

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

%* clear date, page number options;
options nodate nonumber nocenter noquotelenmax;

%* date stamp in case it is requested;
data _null_;
    datenow = today(); timenow=datetime();
    call symput('datenow', trim(left(put(datenow, weekdate.))));
    call symput('timenow', trim(right(put(timenow, tod5.2))));
run;

%if &daytime eq 1 %then %do;
    footnote9 "&datenow" "&timenow";
%end;

%* Define font size for footnotes;
%let linesize = %sysevalf(&fontsize - 2);

%* main title/footnote location;
%if &bdytitle=1 %then %let bd=bodytitle;
%else %let bd=;

%* start new pages;
%if &newpage eq 1 %then %let stpage=YES;
%else %let stpage=NO;

%* ODS escape character so that the superscripts work;
ods escapechar="^";

%* Paper orientation ;
options orientation = &PAGE;

%* Plus/Minus sign;
%let PlMi=(*ESC*){unicode 00B1};

%*****************************************************************************;
%* OPEN ODS DESTINATION(S)                                                    ;
%*****************************************************************************;
%put ****** OPENING ODS DESTINATIONS ******;

%* Close listing destination;
ods listing close;

%* Open ODS Destinations;
%if %length(&rtffile)>0 %then %do;
    ods rtf file = &rtffile style = myrtf &bd startpage=&stpage;
%end;

%if %length(&pdffile)>0 %then %do;
    ods pdf file = &pdffile style = myrtf bookmarkgen = no bookmarklist = no ;
%end;

%if %length(&xmlfile)>0 %then %do;
    ods tagsets.ExcelXP file = &xmlfile
     %if &bdytitle eq 1 %then %do; options(embedded_titles='yes' embedded_footnotes='yes') %end; ;
%end;

%if %length(&xlsxfile)>0 %then %do;
    ods excel file = &xlsxfile
      %if &bdytitle eq 1 %then %do; options(embedded_titles='yes' embedded_footnotes='yes') %end; ;
%end;

%*****************************************************************************;
%* LOOP THROUGH EACH RTFIN FILE AND CREATE TABLE                              ;
%*****************************************************************************;
%put ****** LOOPING THROUGH DATASETS ******;

%let missnote=;
%let rtfi=1;
%let rtfnow=%qscan(&rtfin, &rtfi, " ");
%do %until(&rtfnow eq );

    %put ****** GENERATING TABLE FOR %upcase(&RTFNOW) ******;
    
    %* Get title;
    %if %length(&titles) gt 0 %then %do;
    
        %put ****** %upcase(&rtfnow): GETTING TITLE ******;
        %let titles=&titles ' ';
    
        %let tstart=%index(&titles, %str(%"));
        %if &tstart>0 %then %do;
            %let t2=%qsubstr(&titles, %eval(&tstart+1));
            %let tstop=%index(&t2, %str(%"));
            %LET titlenow=%substr(&titles, %eval(&tstart+1), %eval(&tstop-1));
            %LET titles=%substr(&titles, %eval(&tstart+&tstop+1));
        %end;    
    
    %end;
    %else %let titlenow=;
    
    %* Get user-defined footnote;
    %if %length(&addfn) gt 0 %then %do;
    
        %put ****** %upcase(&rtfnow): GETTING USER-DEFINED FOOTNOTE ******;
        %let addfn=&addfn ' ';
    
        %let fstart=%index(&addfn, %str(%"));
        %if &fstart>0 %then %do;
            %let f2=%qsubstr(&addfn, %eval(&fstart+1));
            %let fstop=%index(&f2, %str(%"));
            %LET addfnnow=%substr(&addfn, %eval(&fstart+1), %eval(&fstop-1));
            %LET addfn=%substr(&addfn, %eval(&fstart+&fstop+1));
        %end;    
    
    %end;
    %else %let addfnnow=;
    
    %* Get necessary info from _rtf_ file;
    %put ****** %upcase(&rtfnow): GETTING INFO FROM _RTF_ FILE ******;
    data _null_;
        set &rtfnow;
        if _n_ = 1;
        call symput('f1', trim(_f1_));
    run;
    
    data _null_;
        set &rtfnow;
        if _n_ = 1;
        call symput('TotalN', trim(left(put(_ntotal_, &f1))));
        call symput('NGroups', trim(_nlev_));
        call symput('TotalCol', trim(_totalcol_));
        call symput('NCol', trim(_ncol_));
        call symput('MissCol', trim(_misscol_));
        call symput('ColPct', trim(_colpct_));
        call symput('Adhoc', trim(_adhoc_));     
        call symput('PAdj_Method', trim(_padj_method_));
        call symput("Subset", trim(_subset_));
        call symput("oddsratios", trim(_oddsratios_));       
        call symput("orunits", trim(_orunits_));  
        call symput("ChiSqCatList", trim(_chisqcatlist_));
        call symput("FisherCatList", trim(_fishercatlist_));
        call symput("UnequalVar", trim(_unequalvar_));        
        call symput("ParTest", trim(_partest_));
        call symput("NParTest", trim(_npartest_));
        call symput("DescLabel", trim(_desclabel_));
        call symput("BoldedSigP", trim(_boldedsigp_));
        call symput("BoldedLabel", trim(_boldedlabel_));       
        call symput("Alpha", trim(_alpha_));
        call symput("PValues", trim(_pvalues_));
        call symput("PrintPType", trim(_printptype_));        
        call symput('pfmt', trim(_pfmt_));
        call symput('cwidth1', trim(_cwidth1_));
        call symput('cwidth2', trim(_cwidth2_));
        call symput('cwidth3', trim(_cwidth3_));
        call symput('sdopt', trim(_sdopt_));
        call symput('con1', trim(_con1_));
        call symput('con2', trim(_con2_));
        call symput('con3', trim(_con3_));
        call symput('cat1', trim(_cat1_));
        call symput('cat2', trim(_cat2_));
        call symput('ord1', trim(_ord1_));
        call symput('printfn', trim(_printfn_));
    run;

    data _null_;
        set &rtfnow;
        if _n_=1;
        %do i=1 %to &NGroups;
            call symput("GroupLab&i", trim(left((_label&i))));
            call symput("GroupN&i", trim(left(put(_groupn&i, &f1))));
        %end;
    run;
 
    %* Get n missing footnote if misscol and ncol equal 0;
    %if &ncol eq 0 and &misscol eq 0 %then %do;
    
        %put ****** %upcase(&rtfnow): CREATING MISSING VALUES FOOTNOTE ******;
       
        proc sql noprint;
            select trim(varlabel)||" = "||compress(put(_Nmiss, comma32.))
            into :MissNote separated by '; ' 
            from &rtfnow
            where _nmiss gt 0;
        quit;        
        %if %length(&missnote) ge 1 %then %do;
            %let MissNote=&MissNote; %*remove trailing blanks;
        %end;       
        
    %end; %*end to if ncol=0 and miscol=0;
    
    %* If ORS included and units other than 1, generate footnote;
    %if &oddsratios eq 1 and &orunits eq 1 %then %do;
    
        %put ****** %upcase(&rtfnow): CREATING OR UNITS VALUES FOOTNOTE ******;
       
        proc sql noprint;
            select trim(varlabel)||" = "||compress(put(ORUnit, best.))
            into :ORNote separated by '; ' 
            from &rtfnow
            where ORUnit not in (1 .);
        quit;        
        %if %symexist(ornote) eq 1 %then %do;
            %let ORNote=&ORNote; %*remove trailing blanks;
        %end;   
        
    %end; %*end to or units footnote;    
    
    %* CL lab for OR;
    %let cllab=%sysevalf((1-&alpha)*100);
    
    %* Define p-values used;    
    %if &printfn eq 1 and &Ngroups gt 1 and &pvalues eq 1 and &oddsratios eq 0 and 
      (&con1 eq 1 or &con2 eq 1 or &con3 eq 1 or &ord1 eq 1) %then %do;
    
        %put ****** %upcase(&rtfnow): DEFINING P-VALUES USED ******;
        
        %*Parametric tests;
        %if &con1 eq 1 %then %do;
    
            %*If unequalvar eq 2 then check if we have a mix or all are the same;
            %*If only 1 type determine which;    
            %if &unequalvar eq 2 %then %do;
                
                proc sql noprint;
                    select distinct(PvalFlag)
                    into :PvalType separated by ' '
                    from &rtfnow
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
        %if &con2 eq 1 or &con3 eq 1 or &ord1 eq 1 %then %do;
            %if &Ngroups eq 2 %then %let npar_p=Wilcoxon Rank Sum test;
            %else %if &Ngroups gt 2 %then %let npar_p=Kruskal-Wallis test;
        %end;
    
    %end; %*end to type of p;
    
    %*If unequal var does not exist, initiate;
    %if %symexist(NPvalType) eq 0 %then %let NPvalType=0;
    %if %symexist(PvalType) eq 0 %then %let PvalType=;

    %*Titles for excel sheets;
    %if %length(&xmlfile) gt 0 %then %do;
        ods tagsets.ExcelXP options(sheet_name="&TITLENOW.");
    %end;    

    %if %length(&xlsxfile)>0 %then %do;
        ods excel options(sheet_name="&TITLENOW."); 
    %end;

    %* Run report;
    %put ****** %upcase(&RTFNOW): GENERATING TABLE ******;
    proc report data= &RTFNOW nowd /*list*/ ls=256
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
          (&FisherCatList eq 1 or &ord1 eq 1 or &NPvalType gt 1)
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
            line "&TITLENOW";
        endcomp;  
        
        %* Label column;
        define MyLabel / computed "Factor"  left order = data
           style(header) = {just = left 
             %if &ncol in (2 3) or &misscol in (2 3) or
              (&NGroups gt 1 and &totalcol eq 1 and (&ncol in (1 3) or  &misscol in (1 3)))
               %then %do; bordertopcolor = white %end; }
           style(column) = {asis=on  cellwidth = &CWIDTH3
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
              (&FisherCatList eq 1 or &ord1 eq 1 or &NPvalType gt 1) %then %do;
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
                    if (type in ("CAT1" "CAT2") and &FisherCatList eq 1) or
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
            
            %if %length(&subset) gt 0 %then %do;
                line "Subset of population used: &SUBSET..";
            %end;
            
            %if %length(&missnote) gt 0 %then %do;
                line "*Data not available for all subjects. Missing values: &MissNote..";
            %end;
        
            %if &printfn eq 1 and (&NGroups eq 1 or &pvalues eq 0 or &printptype eq 1) %then %do;
                line "Statistics presented as "
                  %if &con1 eq 1 %then %do;
                    %if &sdopt eq 1 %then %do; "Mean &PlMi. SD" %end;
                    %else %do; "Mean (SD)" %end;
                  %end;
                  %if &con2 eq 1 %then %do; 
                    %if &con1 eq 1 %then %do; ", " %end;
                    "Median (min, max)" 
                  %end;
                  %if &con3 eq 1 %then %do; 
                    %if &con1 eq 1 or &con2 eq 1 %then %do; ", " %end;
                    "Median [P25, P75]" 
                  %end;
                  %if &cat1 eq 1 or &cat2 eq 1 or &ord1 eq 1 %then %do;
                    %if &con1 eq 1 or &con2 eq 1 or &con3 eq 1 %then %do; ", " %end;
                    %if &colpct eq 1 %then %do; "N (column %)" %end;
                    %else %do; "N (row %)" %end;
                  %end;
                  ".";
            %end;
            
            %if &NGroups gt 1 and &printfn eq 1 and &pvalues eq 1 and &printptype eq 1 and &oddsratios eq 0 %then %do;
                line "p-values: "
                  %if &con1 eq 1 %then %do; 
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
                  %if &con2 eq 1 or &con3 eq 1 or &ord1 eq 1 %then %do; 
                    %if &con1 eq 1 %then %do; ", " %end;
                    "b=&npar_p"
                  %end;
                  %if &cat1 eq 1 or &cat2 eq 1 %then %do;
                    %if &con1 eq 1 or &con2 eq 1 or &con3 eq 1 or &ord1 eq 1 %then %do; ", " %end;
                    %if &ChiSqCatList eq 1 %then %do; 
                      "c=Pearson's chi-square test" 
                    %end;
                    %if &FisherCatList eq 1 %then %do;
                      %if &ChiSqCatList eq 1 %then %do;  ", " %end;
                      "d=Fisher's Exact test" 
                    %end;
                  %end;
                  ".";
            %end;
    
            %if &NGroups gt 1 and &printfn eq 1 and &pvalues eq 1 and &printptype eq 0 and &oddsratios eq 0 %then %do;
                line "Statistics presented as "
                  %if &con1 eq 1 %then %do; 
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
                  %if &con2 eq 1 or &con3 eq 1 %then %do;
                    %if &con1 eq 1 %then %do; "; " %end;
                    %if &con2 eq 0 %then %do; "Median [P25, P75]" %end;
                    %if &con3 eq 0 %then %do; "Median (min, max)" %end;
                    %else %if &con2 eq 1 and &con3 eq 1 %then %do; 
                      "Median [P25, P75] or Median (min, max)" 
                    %end;
                    " with &npar_p" 
                  %end;
                  %if &cat1 eq 1 or &cat2 eq 1 or &ord1 eq 1 %then %do;
                    %if &con1 eq 1 or &con2 eq 1 or &con3 eq 1 %then %do; "; " %end;
                    %if &colpct eq 1 %then %do; "N (column %) with " %end;
                    %else %do; "N (row %) with " %end;
                    %if &ord1 eq 1 %then %do; 
                      "&npar_p "
                      %if &NGroups eq 2 %then %do; "(W)" %end;
                      %else %if &NGroups gt 2 %then %do; "(K)" %end;
                    %end;                
                    %if &FisherCatList eq 1 %then %do; 
                      %if &ord1 eq 1 %then %do; ", " %end;
                      "Fisher's Exact test (F)" 
                    %end;
                    %if (&ord1 eq 1 or &FisherCatList eq 1) and &ChiSqCatList eq 1 %then %do; 
                      " or" 
                    %end;
                    %if &ChiSqCatList eq 1 %then %do; 
                      " Pearson's chi-square test" 
                    %end;
                  %end;
                  ".";
            %end;
     
            %if &NGroups gt 2 and &adhoc eq 1 and &pvalues eq 1 and %length(&padj_method) gt 0 %then %do;
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

            %if %length(&addfnnow)>0 %then %do;
                line "&addfnnow";
            %end;        
                        
        endcomp;
        
    run;
    
    %*reset missnote for next loop;
    %let MissNote=;
    
    %*next file;
    %let rtfi=%eval(&rtfi+1);
    %let rtfnow=%qscan(&rtfin, &rtfi, " ");

%end; %*end to rtf loop;

%*****************************************************************************;
%* CLOSE ODS DESTINATION(S)                                                   ;
%*****************************************************************************;
%put ****** CLOSING ODS DESTINATIONS ******;

%*Close ODS destinations (do not want to use _all_ option as it closes the 
  output window in SAS Studio;        

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
%* MEND MACRO                                                                 ;
%*****************************************************************************;
ods listing;
ods select all;
ods results;
ods path (REMOVE) sasuser.TEMPLAT;
ods path reset;
options nomlogic nomlogicnest orientation = portrait quotelenmax;
footnote9;

%mend outsummarize;