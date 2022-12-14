/**
 @file
 @brief Generates a simple report-ready table
 
 @details 
 @par Purpose
 Runs PROC REPORT and creates a simple report-ready table

 @author Rocio Lopez
 
 @date 02-01-2016
 
 @version SAS 9.2 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %CreateTable(
         ds = 
        ,title = 
        ,labelCol =
        ,columns = 
        ,pValue = 
        ,pFmt = pvaluef.
        ,footnote = 
        ,cWidth1 = 350
        ,cWidth2 = 250
        ,outputWidth = 
        ,fontSize = 11
        ,fontStyle = Roman
        ,fontFace= Times
        ,boldedLabel = 0
        ,boldedSigP = 1
        ,sigP = 0.05
        ,style = rtf
        ,newPage = 0
        ,page = portrait
        ,rtfFile = 
        ,xmlFile = 
        ,pdfFile = 
        );
 @endcode

 @returns
 A table produced by proc report

 @note 
 @parblock
 @li It is recommended that all variables be formatted and labeled

 @li Unless running on SAS Studio, output might not look right in results 
 window but styling in RTF file will be fine

 @li On occasion you might see an error message about the width of a column not 
 being between 1 and 78. This has to do with the listing destination being 
 open and does not affect the output. If you wish to save an error-free copy 
 of your log, use "ods listing close;" prior to calling the macro. 
 @endparblock

 @param ds The data set to be used to generate table
 
 @param labelCol Optional name of a single variable to be used as the label column;
 column will be left justified

 @param columns List of 1+ variable names to include in table; columns will be 
 center justified

 @param title Optional title for table; should be surrounded by double quotes

 @param pValue The numeric variable that contains the p-vlaues; column will be 
 center justified

 @param pFmt Format for p-value column

 @param footNote Optional footnote to be included under the table; should be 
 surrounded by double quotes

 @param cWidth1 Specify width of label column in pixels, default is 350

 @param cWidth2 Specify width of columns in pixels
 @n Default is 300

 @param outputWidth Specify output width of the table in terms of % of RTF
 page; value should be 1-100 with 100 occupying 100% of the page
 @n Default size is determined by PROC REPORT

 @param fontSize Font size in pt
 @n Default is 11

 @param fontFace Specify font face
 @n Default is Times

 @param fontStyle Specify font style
 @n Default is Roman

 @param boldedLabel Specify whether label column uses bold font
 @n Allowed options are:
 @n 0 = Use normal font (DEFAULT)
 @n 1 = Use bold font

 @param boldedSigP Specify whether significant p-values use bold font
 @n Allowed options are:
 @n 0 = Use normal font
 @n 1 = Use bold font (DEFAULT)

 @param sigP Specify significance level for bold p-values
 @n Default is 0.05

 @param style ODS style to be used
 @n Default is rtf

 @param newPage Select to start new page; this only takes effect if inluding in 
 a larger ODS output file and not using the rtfFile, xlmFile or pdfFile options 
 within the macro
 @n Allowed options are:
 @n 0 = No, continue on same page
 @n 1 = Yes, start new page (DEFAULT)

 @param page Specify page orientation for ods. If Landscape used, user must
 start new page in next section/figure/table call to revert to portrait
@n Allowed options are:
@n portrait (DEFAULT)
@n landscape

 @param rtffile Optional RTF name in which to store the output, it should be
 surrounded in quotes, e.g. "~userid/consult/out.rtf"

 @param xmlfile Optional Excel XML file name in which to store the output, it 
 should be surrounded in quotes, e.g. "~userid/consult/out.xml"
 @n This has some stylying issues and is not recommended

 @param pdffile Optional PDF file name in which to store the output, it should
 be surrounded in quotes, e.g. "~userid/consult/out.pdf"
  
 @par Example(s)
 @code{.sas}
 proc reg data=sashelp.class;
    model weight = height age;
    ods output parameterEstimates=pe;
 run;

 %CreateTable(
       ds = pe,
       title = "Example",
       labelCol = Variable,
       columns = estimate stdErr,
       pValue = probT,
       footnote = "My footnote here");
 @endcode
 
 @par Revision History
 @b 03-18-2022 Changed style of output table to better match that of summarize 
 and fixed border color around title and footnotes
 @n @b 08-26-2016 Added options to change the font size, font face, and the 
 output width of the table in RTF 
**/

******************************************************************************;
**Start macro                                                                 ;
******************************************************************************;
%macro createTable(
       Ds = ,
       Title = ,
       LabelCol = ,
       Columns = ,
       pvalue = ,
       pFmt = pvaluef.,
       Footnote = ,
       CWidth1 = 350,
       CWidth2 = 250,
       OutputWidth = ,
       fontsize = 11,
       fontstyle = Roman,
       fontface= Times,
       boldedlabel = 0,
       boldedsigp = 1,
       sigp = 0.05,
       style = rtf,
       NewPage = 0,
       Page = portrait,
       rtffile = ,
       xmlfile = ,
       pdffile = 
       );

******************************************************************************;
**Local macro variables                                                       ;
******************************************************************************;
%LOCAL
   DS TITLE LABELCOL COLUMNS PVALUE FOOTNOTE CWIDTH NEWPAGE STPAGE PAGE ERRORFLAG;

******************************************************************************;
**Error checks                                                                ;
******************************************************************************;
%LET ErrorFlag=0;

****************;
**Verify dataset;
****************;
/**Check if data set specified in call **/
%IF (&Ds= ) %THEN %DO;
    %PUT ERROR: No input data set identified in DATA parameter.
    Macro will stop executing.;
    %LET ErrorFlag=1;
%END;

/** Check if data set exists**/
%IF %SYSFUNC(exist(&Ds)) ne 1 %THEN %DO;
    %PUT ERROR: Data set &Ds does not exist. Macro will stop executing;
    %LET ErrorFlag=1;
%END;

***************************************************;
**Verify all other required parameters are provided;
***************************************************;
%IF (&Columns= ) %THEN %DO;
    %PUT ERROR: Required parameter COLUMNS not provided. Macro will stop executing.;
    %LET ErrorFlag=1;
%END;

********************************;
**Check use of "" where required;
********************************;
%IF %length(&Title)>0 and %SYSEVALF(%SYSFUNC(indexc(&Title., '"'))^= 1) %THEN %DO;
    %PUT
    ERROR: Double quotes are required for TITLE. Macro will stop executing.;
    %LET ErrorFlag=1;
%END;

%IF %length(&Footnote)>0 and %SYSEVALF(%SYSFUNC(indexc(&Footnote., '"'))^= 1) %THEN %DO;
    %PUT
    ERROR: Double quotes are required for FOOTNOTE. Macro will stop executing.;
    %LET ErrorFlag=1;
%END;

%IF %length(&RTFFILE)>0 and %SYSEVALF(%SYSFUNC(indexc(&RTFFILE., '"'))^= 1) %THEN %DO;
    %PUT
    ERROR: Double quotes are required for RTFFILE. Macro will stop executing.;
    %LET ErrorFlag=1;
%END;

******************************************************************************;
**If no errors, start macros                                                  ;
******************************************************************************;
%IF &ErrorFlag=0 %THEN %DO;

***************;
**Define Format;
***************;
proc format;
     picture Pvaluef (round)
             0.985   -   high    = "0.99"    (NoEdit)
             0.10    -<  0.985   = "9.99"
             0.001   -<  0.10    = "9.999"
             0       -<  0.001   = "<0.001"  (NoEdit)
             . = " ";
run;

***********************************;
**Set macro variable for start page;
***********************************;
%IF &NEWPAGE=1 %THEN %LET STPAGE=YES;
%ELSE %LET STPAGE=NO;

************************************************************;
**Orientation of page (start new page even if not requested);
**New Page                                                  ;
************************************************************;
options nodate nonumber;
%IF &NewPage.=1 or %UPCASE(&Page.)=LANDSCAPE %THEN %DO;
    options orientation=&Page.;
    ods rtf startpage=NOW;
%END;

*********************************;
**Open requested ODS destinations;
*********************************;
%IF (&RTFFILE^= ) %THEN %DO;
    ods rtf file=&RTFFILE  style=&style startpage=&STPAGE;
%END;

%IF (&XMLFILE^=) %THEN %DO;
    ods tagsets.ExcelXP file=&XMLFILE  style=&style;
%END;

%IF (&PDFFILE^= ) %THEN %DO;
    ods pdf file=&PDFFILE  style=&style bookmarkgen=no bookmarklist=none startpage=&STPAGE;
%END;

***************************;
**Count # of columns needed;
***************************;
%let NumCols=%sysfunc(countw(&Columns));

***********;
**Add Table;
***********;
proc report data=&Ds nowd
    style(report)={bordercolor=black cellpadding=3 
        %if (&outputwidth^= ) %then %do; outputwidth=&outputwidth.% %end;
        font_size=&fontsize.pt font_face=&fontface. FONTSTYLE=&fontstyle.}

    style(lines)={background=white foreground=black
        font_size=9pt font_face=&fontface. FONTSTYLE=&fontstyle.
        protectspecialchars=off}

    style(column)={background=white foreground=black bordercolor=black
        font_size=&fontsize.pt font_face=&fontface. FONTSTYLE=&fontstyle.
        font_weight=medium}

    style(header)={background=white foreground=black bordercolor=black
        font_weight=bold FONTSTYLE=&fontstyle.
        font_size=&fontsize.pt font_face=&fontface.};

    column (%IF (&LabelCol.^= ) %THEN %DO; &LabelCol. %END;
            &Columns.
            %IF (&pvalue.^= ) %THEN %DO; &pvalue. _p %END;
            );

    /** Title **/
    %IF (&Title.^= ) %THEN %DO;
        compute before _PAGE_ /style = {font_weight=bold font_size=&fontsize.pt
                              just=left borderbottomwidth=3
                              borderbottomcolor=black bordertopcolor=white
                              borderleftcolor=white borderrightcolor=white};
            line &Title.;
        endcomp;
    %END;

    /** Variable name column **/
    %IF (&LabelCol.^= ) %THEN %DO;
        define &LabelCol/order=data left
                     style(header)={just=left}
                     style(column)={cellwidth=&cwidth1. 
                     %if &boldedlabel=1 %then %do; font_weight=bold %end;
                     };
    %END;

    /** Loop through columns **/
    %DO Loop=1 %TO &NumCols.;
        define %QSCAN(&Columns, &Loop.)
                     / display
                     style(column)={just = center vjust = center cellwidth = &cwidth2.
                     protectspecialchars=off};
    %END;

    /** p-value column **/
    %IF (&pvalue.^= ) %THEN %DO;
        define &pvalue/display noprint;
        define _p/computed  "p-value"
                  format=&pfmt.
                  style(header)={just=center}
                  style(column)={just=center vjust = center cellwidth=85};
        compute _p;
            _p=&pvalue;
            %if &boldedsigp=1 %then %do;
                if _p ne . and _p < &sigp. then call define(_col_,
                "style", "style=[font_weight=bold fontstyle=italic]");
            %end;
        endcomp;
    %END;

    /**Add Footnote**/
    %IF (&Footnote.^= ) %THEN %DO;
        compute after/style(lines)={font_size=9pt
               just=left bordertopwidth=3
               borderbottomcolor=white bordertopcolor=black 
               borderleftcolor=white borderrightcolor=white};
            line &Footnote.;
        endcomp;
    %END;

run;
************************;
**Close ODS destinations;
************************;
%IF (&RTFFILE^= ) %THEN %DO;
    ods rtf close;
%END;

%IF (&XMLFILE^=) %THEN %DO;
    ods tagsets.ExcelXP close;
%END;

%IF (&PDFFILE^= ) %THEN %DO;
    ods pdf close;
%END;

******************************************************************************;
**END if errorflag=0 loop                                                     ;
******************************************************************************;
%END;

******************************************************************************;
**Close macros definition                                                     ;
******************************************************************************;
%MEND createTable;