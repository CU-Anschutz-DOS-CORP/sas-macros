/**
 @file
 @brief Downloads HCUP SAS Format files and creates a format catalog
 
 @details 
 @par Purpose
 This macro will download all HCUP SAS format text files and will save
 them as .SAS files. You can also create a SAS format catalog.

 @author Rocio Lopez
 
 @date 09-15-2023
 
 @version SAS 9.4 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %getHcupFormats(
	saveDir = 
	,overwrite = 1
	,createCatalog = 1
 ); 
 @endcode

 @returns
 .SAS files with PROC FORMAT code and, if requested, a formats.sas7bcat cataglog.
 
 @param saveDir Specify the location where the format files are to be saved

 @param overwrite Logical stating whether to overwrite existing files
 @n Allowed options are:
 @n 0 for no
 @n 1 for yes (DEFAULT)

 @param createCatalog Logical stating whether to create a format catalog or not
 @n Allowed options are:
 @n 0 for no
 @n 1 for yes (DEFAULT)

 @note 
 @parblock
 @li This macro has been tested on a Windows environment only. User permissions
 and working environment might affect the ability to use this macro.
 
 @li The log file might have the following warning message: Apparent symbolic 
 reference C not resolved. This is because of a format label containing the 
 ampersand sign; please ignore.
 
 @li The following files will be downloaded from https://hcup-us.ahrq.gov/db/tools/:
 DRG_Formats.TXT, HCUP_Formats.TXT, Dx_Pr_Grps_Formats.TXT, I9_Formats.TXT,
 I10_Formats.TXT, and Severity_Formats.txt

 @li In order to access the format catalog in future SAS sessions, you can use the 
 following code:
 
 @code{.sas}
 libname fmt ".\hcup\hcupFormats";
 options fmtsearch=(work, fmt);
 @endcode
 
 @endparblock  
  
 @par Example(s)
 @code{.sas}
 %getHcupFormats(
	saveDir = .\hcup\hcupFormats
	,overwrite = 1
	,createCatalog = 1
 ); 
 @endcode
 
 @par Revision History
 n/a
 
**/

%macro getHcupFormats(
	saveDir =
	,overwrite = 1
	,createCatalog = 1
) / minoperator;

%put ************************************************************;
%put ************************************************************;
%put *                GETTING HCUP SAS FORMATS                  *;
%put ************************************************************;
%put ************************************************************;

%put ************************************************************;
%put ************************************************************;
%put *                0.1:SET-UP                                *;
%put ************************************************************;
%put ************************************************************;
/* Clear any formats stored in WORK library */
proc datasets lib=work memtype=catalog kill nolist; run;

/* Base URL for HCUP tools website (where format files are) */
%let hcupToolsUrl = https://hcup-us.ahrq.gov/db/tools;

%put ************************************************************;
%put ************************************************************;
%put *                0.2:ERROR CHECKING                        *;
%put ************************************************************;
%put ************************************************************;
%let errorFlag = 0 ;

%put *************************************************;
%put * CHECKING ALL REQUIRED PARAMETERS ARE PROVIDED *;
%put *************************************************;
%if not(%length(&saveDir)) %then %do;
    %put ERROR: Required parameter SAVEDIR is not provided. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%put *************************************************;
%put * CHECKING SAVEDIR EXISTS			             *;
%put *************************************************;
%if not(%length(&saveDir)) and not(%sysfunc(fileexist(&saveDir))) %then %do;
    %put ERROR: Directory &SAVEDIR does not exist. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%put *************************************************;
%put * CHECKING PARAMETER OPTIONS ARE VALID          *;
%put *************************************************;
%if not(&overwrite in (0 1)) %then %do;
    %put ERROR: OVERWRITE must be either 0 or 1. Macro will stop executing.;
    %let errorFlag = 1;	
%end; 

%if not(&createCatalog in (0 1)) %then %do;
    %put ERROR: CREATECATALOG must be either 0 or 1. Macro will stop executing.;
    %let errorFlag = 1;	
%end; 

%if &errorFlag %then %do ;
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

%put ************************************************************;
%put ************************************************************;
%put *             1:GETTING FORMAT FILES                       *;
%put ************************************************************;
%put ************************************************************;
%let fmtFiles = 
	DRG_Formats.TXT
	HCUP_Formats.txt 
	Dx_Pr_Grps_Formats.txt
	I9_Formats.TXT
	I10_Formats.TXT
	Severity_Formats.txt;
	
%do loop = 1 %to 6;
	%let thisFmtFile = %qscan(&fmtFiles, &loop, " ");
	%let thisFmtFileName = %scan(&thisFmtFile, 1);
		
    %if %sysfunc(fileexist("&saveDir/&thisFmtFileName..sas"))
      and not(&overwrite) %then %do;
		%put NOTE: &saveDir/&thisFmtFileName..sas already exists and will not be overwritten.;
		%put **To allow overwriting files, use OVERWRITE=1.;
	%end;		
		
    %if not(%sysfunc(fileexist("&saveDir/&thisFmtFileName..sas")))
      or &overwrite %then %do;
	
		%put *************************************************;
		%put * CHECKING IF &thisFmtFile FILE EXISTS 	     *;
		%put *************************************************;
		filename headout TEMP;
		proc http 
		    url="&hcupToolsUrl/&thisFmtFile" 
		    headerout=headout;
		run;
	   
		data _null_;
		    infile headout scanover truncover;
		    input @'HTTP/1.1' code 4. message $255.;
		    if _n_ = 1 then call symput("message", strip(message));
		run;     
		
		%if "&message" ne "OK" %then %do;
		    %put WARNING: &thisFmtFile could not be downloaded.;
		    %put WARNING: Reason not downloaded is: &message.. ;
		%end;	**end to message not ok;
	
		%if "&message" eq "OK" %then %do;
			filename download "&saveDir/&thisFmtFileName..sas";
		    proc http
		    	url="&hcupToolsUrl/&thisFmtFile"
		        out=download;
		    run;
		    filename download clear;
		%end; **end to message ok;
		
	%end; **end to file download;
	
	%* Read in SAS file;
	%if &createCatalog %then %do;
		%include "&saveDir/&thisFmtFileName..sas";
	%end;		
		
%end; **end to loop 1 to 6;

%if &createCatalog %then %do;

	%put ************************************************************;
	%put ************************************************************;
	%put *             2: CREATE SAS FORMAT CATALOG                 *;
	%put ************************************************************;
	%put ************************************************************;
	libname hcupFmt "&saveDir";
	
	proc catalog cat=work.Formats;
	   copy out=hcupFmt.Formats;
	run;
	
	ods html path = "&saveDir" file = "HCUP SAS format catalog.html";
	proc format lib=hcupFmt fmtlib;
	run;
	ods html close;
	
	libname hcupFmt;

%end; **end to createcatalog;

%put ************************************************************;
%put NOTE:  Macro &sysmacroname has finished. ;
%put ************************************************************;

%mend getHcupFormats;
