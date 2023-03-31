/**
 @file
 @brief Imports *.CSV files

 @details
 @par Purpose
 This macro will import all *.CSV files found in the specified directory

 @author Rocio Lopez

 @date 11-21-2014

 @version SAS 9.4 or later

 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %imptallcsv(
      directory =
      ,library = work
      ,guessingrows = all
      );
 @endcode

 @returns
 One SAS data set per each imported CSV file

 @note
 Subfolders of directory will not be searched

 @param directory Folder where all .CSV files are found; do not use quotes

 @param library SAS library in which to store imported files
 @n Default is WORK

 @param guessingrows Specifies the number of rows of the file to scan
 to determine the appropriate data type and length for the variables.
 @n Default is ALL rows.

 @par Example(s)
 @code{.sas}
 %imptallcsv(directory=./csvFiles);
 @endcode

 @par Revision History
 @b 03-31-2023 Updated to run in PC SAS as well as Linux SAS
 @n @b  03-21-2019 - Changed how upper/lower case files are called and
 added an error check for when folder has no .csv files
 @n @b 09-19-2018 - Added LIBRARY parameter and error checks
 @n @b 07-10-2017 Added GUESSINGROWS option
**/

%macro imptallcsv(directory= , library=work, guessingrows=all);

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

%* If directory exists, check at least 1 .CSV file exists;
%if %length(&directory) gt 0 and %sysfunc(fileexist(&directory)) eq 1 %then %do;
    %if %sysfunc(fileexist("&directory/*.[Cc][Ss][Vv]")) eq 0 and
        %sysfunc(fileexist("&directory/*.csv")) eq 0
      %then %do;
        %put ERROR: No .CSV files found in DIRECTORY &directory. Macro will stop executing.;
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
%* Get list of .CSV files in directory;
%if %substr(&sysscp.,1,3) eq WIN %then %do;
    filename udir  "&directory/*.csv"; **case insensitive;
%end;
%else %if %substr(&sysscp.,1,3) ne WIN %then %do;
    filename udir  "&directory/*.[Cc][Ss][Vv]";
%end;

data _files;
    length filename fname $ 200;
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
%let NumFiles=&NumFiles; %* remove leading blanks;

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
         call symput("fname&i", scan(left(tranwrd(filePath,"&directory./"," ")),1,"."));
     run;

    %* Import file;
    proc import datafile="&file."
        out=&library..&&fname&i
        dbms=csv replace;
        guessingrows=&guessingrows.;
    run;

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
%mend;
