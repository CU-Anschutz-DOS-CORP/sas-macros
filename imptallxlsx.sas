/**
 @file
 @brief Imports *.XLSX files

 @details
 @par Purpose
 This macro will import all sheets within all *.XLSX files found 
 in the specified directory.

 @author Rocio Lopez

 @date 03-20-2019

 @version SAS 9.4 or later

 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %imptallxlsx(directory= , library=work);
 @endcode

 @returns
 One SAS data set per each sheet in each imported XLSX file

 @param directory Folder where all .XLSX files are found; do not use quotes

 @param library SAS library in which to store imported files
 @n Default is WORK

 @note
 @li Subfolders of directory will not be searched
 @li Existing files in the destination library will be overwritten 
 if they share the same name with any of the files/sheets being 
 read. It is recommended you import to an empty library.
 @li If a single sheet is found in the file then the output SAS 
 data file will have the name of the file. If multiple sheets, 
 then the names will be file_sheet.
 @li Datasets created and deleted by the macro: _files1 _files2 _files _members

 @par Example(s)
 @code{.sas}
 %imptallcsv(directory=./xlsxFiles);
 @endcode

 @par Revision History
 @b 03-31-2023 Updated to run in PC SAS as well as Linux SAS
**/

%macro imptallxlsx(directory= , library=work);

%************************;
%* Error checks and abort;
%************************;
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

%* If directory exists, check at least 1 .XLSX file exists;
%if %length(&directory) gt 0 and %sysfunc(fileexist(&directory)) eq 1 %then %do;
    %if (%substr(&sysscp.,1,3) ne WIN and %sysfunc(fileexist("&directory/*.[Xx][Ll][Ss][Xx]")) eq 0) or
        (%substr(&sysscp.,1,3) eq WIN and %sysfunc(fileexist("&directory/*.xlsx")) eq 0)
      %then %do;
        %put ERROR: No .XLSX files found in DIRECTORY &directory. Macro will stop executing.;
        %let ERRORFLAG=1;
    %end;
%end;

%* Check if requested library is assigned;
%if (%sysfunc(libref(&library))) ne 0 %then %do;
    %put ERROR: Library %upcase(&library) has not been assigned. Macro will stop executing.;
    %let errorflag=1;
%end;

%* If any errors found, then abort;
%if &ERRORFLAG %then %do;
    data _null_;
        abort 3;
    run;
%end;

%**************************************************;
%* Create data set with list of all files to import;
%**************************************************;
%* Get list of .XLSX files in directory;
%if %substr(&sysscp.,1,3) eq WIN %then %do;
    filename udir  "&directory/*.xlsx"; **case insensitive;
%end;
%else %if %substr(&sysscp.,1,3) ne WIN %then %do;
    filename udir  "&directory/*.[Xx][Ll][Ss][Xx]";
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

%*******************************;
%* Loop through files and import;
%*******************************;
%do i=1 %to &NumFiles;

    %* Get ith file name;
    proc sql noprint;
        select strip(filename) into :file trimmed
        from _files
        where monotonic()=&i.;

     data _null_;
         filePath = tranwrd("&file", "\", "/");
         call symput("fname&i", strip(scan(tranwrd(filePath,"&directory/"," "),1,".")) );
     run;
     %let file=&file; **remove trailing blanks;

    %* Import all sheets in file;
    libname xl xlsx "&file" access=read;

    %* Identify sheets in file;
    ods listing close;
    ods select none;
    ods output Members=_members;
    proc datasets library=xl;
    run;
    ods listing;
    ods select all;

    %* Count number of sheets;
    %let dsid=%sysfunc(open(_members));
    %let nsheets=%sysfunc(attrn(&dsid,nlobs));
    %let rc=%sysfunc(close(&dsid));

    %* Create macro vars with sheet names;
    proc sql noprint;
        select Name into :DB1 - :DB&NSheets
        from _members;
    quit;

    %* It is common for Excel sheets to be named Sheet1-Sheetx. So as not to worry about
       files getting overwritten in next loops, I will change the name after copying;
    proc datasets memtype=data nolist;
        copy in=xl out=work;
    run;
        change
            %if &NSheets eq 1 and &db1 ne &&fname&i %then %do; &db1 = &&fname&i %end;
            %else %if &NSheets gt 1 %then %do loop=1 %to &NSheets;
                &&db&loop = &&fname&i.._&&db&loop
            %end; ;
    run;
    quit;

    %* Disconnect;
    libname xl clear;

    %* Clear file for next loop;
    proc datasets library=work nolist;
       delete _members;
    run; quit;

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
