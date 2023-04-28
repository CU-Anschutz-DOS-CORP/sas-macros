/**
 @file
 @brief Adds titles and creates a table of contents data set for your SAS program
 
 @details 
 @par Purpose
 This macro allows to put a section title in both the .LOG and .LST 
 files. It also creates a work.__TOC dataset that can be used to 
 print a table of contents to include at the top of your program.
 Within each section you can use TITLE2-TITLEn to add further detail
 to your sub-steps.
 
 @author Rocio Lopez

 @date 06-25-2021
 
 @version SAS 9.2 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %sectionHeader(myTitle);
 @endcode

 @returns
 SAS data set.
 
 @param myTitle The text you want to include as your section header
 @n Do not use quotation marks

 @note 
 Data set created: __toc

 @par Example(s)
 @code{.sas}
 %sectionHeader(Data Analysis Section A) ;
 @endcode
 
 @par Revision History
 @b 07-09-2021 - Added ToC functionality
**/

%macro sectionheader(myTitle) ;

/* Store Title for ToC */
%* If __toc does not exist, create. Else, append ;
options nonotes ;
%if not(%SYSFUNC(exist(__toc))) %then %do ;
    data __toc ;
        informat Section $32767. ;
        Section = "&myTitle" ; 
        _order = _n_ ;
    run ;
%end;
%else %do ;
    data __toc ;
        set __toc end=eof ;
        output ; 
        if eof then do ; 
            Section = "&myTitle" ;
            _order = _n_ + 1;
            output ;
        end ;
    run ;
%end ;
%* Remove dups ;
proc sort data = __toc nodupkey ; by section ;
proc sort data = __toc ; by _order ;
run ;  
options notes ;

/* Print title on LOG file */
%Put ******************************************************************************;
%Put ******************************************************************************;
%Put * &myTitle *;
%Put ******************************************************************************;
%Put ******************************************************************************;

/* Print title on .LST file */
title "&myTitle" ;

%mend ;
