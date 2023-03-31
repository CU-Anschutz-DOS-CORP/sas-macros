/**
 @file
 @brief Imports *.XPT files
 
 @details 
 @par Purpose
 This macro will import all *.XPT files found in the specified directory

 @author Rocio Lopez
 
 @date 09-19-2018
 
 @version SAS 9.4 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %imptallxpt(
      directory= 
      ,library=work
      );
 @endcode

 @returns
 One SAS data set per each imported XPT file

 @note 
 Subfolders of directory will not be searched

 @param directory Folder where all .XPT files are found; do not use quotes
 
 @param library SAS library in which to store imported files
 @n Default is WORK

 @sa getnhanesdata.sas, nhanesfmtscrape.sas

 @par Example(s)
 @code{.sas}
 %imptallxpt(directory=./nhanes2015to16);
 @endcode
 
 @par Revision History
 @b 03-31-2023 Change to allow use of backward or forward slashes in directory name
 @n @b 12-14-2022 Updated to run in PC SAS as well as Linux SAS
 @n @b 03-21-2019 Changed how upper/lower case files are searched for in Linux
 and added check for when folder has no .XPT files
**/

%macro imptAllXpt(directory= ,library=work);

%***********************;
%* Error check and abort;
%***********************;
%let errorflag=0;

%* Directory is required;
%if %length(&directory) eq 0 %then %do;
    %put ERROR: Required parameter DIRECTORY is not provided. Macro will stop executing.;
    %let ERRORFLAG=1;
%end; %* end to if length(directory) = 0 ;

%* Directory should not be surrounded by quotes;
%if %length(&directory) gt 0 and %sysevalf(%sysfunc(indexc(&directory, '"')) eq 1)
  %then %do;
    %let directory = %qsysfunc(dequote(&directory));
    %put WARNING: Quotation marks removed from DIRECTORY parameter.;
%end;

%* If directory provided, check it exists;
%if %length(&directory) gt 0 %then %do;
    %if %sysfunc(fileexist(&directory)) ne 1 %then %do;
        %put ERROR: DIRECTORY &directory does not exist. Macro will stop executing.;
        %let ERRORFLAG=1;
    %end;
%end;

%* If directory exists, check at least 1 .XPT file exists;
%if %length(&directory) gt 0 and %sysfunc(fileexist(&directory)) eq 1 %then %do;
    %if (%substr(&sysscp.,1,3) ne WIN and %sysfunc(fileexist("&directory/*.[Xx][Pp][Tt]")) eq 0) or
        (%substr(&sysscp.,1,3) eq WIN and %sysfunc(fileexist("&directory/*.xpt")) eq 0)
      %then %do;
        %put ERROR: No .XPT files found in DIRECTORY &directory. Macro will stop executing.;
        %let ERRORFLAG=1;
    %end;
%end;

%* Check if requested library is assigned;
%if (%sysfunc(libref(&library))) ne 0 %then %do;
    %put ERROR: Library %upcase(&library) has not been assigned. Macro will stop executing.;
    %let errorflag=1;
%end;  

%if &ERRORFLAG %then %do;
    data _null_;
        abort 3;
    run;
%end;

%**************************************************;
%* Create data set with list of all files to import;
%**************************************************;
%* Get list of .XPT files in directory;
%if %substr(&sysscp.,1,3) eq WIN %then %do;
    filename udir  "&directory/*.xpt"; **case insensitive;
%end;
%else %if %substr(&sysscp.,1,3) ne WIN %then %do;
    filename udir  "&directory/*.[Xx][Pp][Tt]";
%end;
data _files;
    length filename fname $ 256;
    infile udir  eof=last filename=fname;
    input ;
    last: filename=fname;
run;

%* Remove dups, above code creates some;
proc sort data=_files nodupkey; by filename; run;

%* Num. files found;
proc sql noprint;
    select count(*) into :NumFiles
    from _files;
%let NumFiles=&NumFiles; %*remove leading blanks;   

%********************;
%* Loop through files;
%********************;
%do i=1 %to &NumFiles;

    %* Get ith file name;
    proc sql noprint;
        select strip(filename) into :file trimmed
        from _files
        where monotonic()=&i.;

     data _null_;
        filePath = tranwrd("&file", "\", "/");
        call symput("fname&i", scan(left(tranwrd("&file.","&directory./"," ")),1,".") );
     run;

    %* Import file;
    libname XP xport "&file.";
    proc copy in=XP out=&library; run;
    libname xp clear;

     %* Clear macro variables for next loop;
     %let file=;

%end;

%* Delete files created in process;
proc datasets library=work nolist;
    delete _files _files1 _files2;
run; quit;

%* List all data files imported to the log;
%put NOTE: A total of %trim(&NumFiles) files have been imported from &directory. into the %upcase(&library) library.;
%do i=1 %to &NumFiles; %put &&fname&i; %end;

%*************;
%* Close macro;
%*************;
%mend imptAllXpt;
