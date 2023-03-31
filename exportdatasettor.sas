/**
 @file
 @brief Transfers data from SAS to R

 @details
 @par Purpose
 This macro facilitates data transfer from SAS to R. It will generate 
 a .CSV or .XLSX data file along with a .R code file to import the 
 specified sas data set into R. The R code will apply labels and create 
 factor variables if applicable.
   
 @author Rocio Lopez

 @date 07-15-2022

 @version SAS 9.4 or later

 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %exportDataSetToR(
            libName = WORK
            ,data = 
            ,dataFile = 
            ,codeFile = 
            ,dataName = 
            ,factorVars = 
            ,fmtLib = WORK
            ,overWrite = 0
            ,debug = 0
 );
 @endcode

 @returns
 One CSV file and one R code file to import the desired SAS data set into R.

 @note
 @li Factor variables should not have range formats. If so, the generated 
 code will not be identified as factors in R.
 @li Data and code files will be overwritten if they already exist.

 @param data Specifies the dataset to be used in the macro

 @param library SAS library in which to store imported files
 @n Default is WORK
 
 @param libname Specifies the library where the dataset is found
 @n Default is WORK

 @param dataFile Identifies a file path and name to directly save the data to; 
 file extension must be .CSV or .XLSX
 @n Default is <dir>/data<date>_FromSAS_&dataName..csv where dir is either
 your current working directory or /home/userid if default is /sas/studioconfig
 and the date is a run date stamp

 @param codeFile Identifies a file path and name in which to save the R code
 to import the data file in R
 @n Default is <dir>/r<date>_importFromSAS_&dataName..R where dir is either
 your current working directory or /home/userid if default is /sas/studioconfig
 and the date is a run date stamp

 @param dataName Specifies the name that will be assigned to the dataset in R
 @n Default is the same name used in SAS (specified in DATA)
        
 @param factorVars Specfies which variables will be identified as factor 
 variables in R
 
 @param fmtLib Specifies the library where the formats for the factor 
 variables are found
 @n Default is WORK.

 @param overWrite Determines whether files should be overwitten if they 
 already exist
 @n Options are:
 @n 0, n, no (DEFAULT): Do not overwrite files
 @n 1, y, yes: Overwrite files

 @param debug Determines if temporary datasets cleaned up and macro logic 
 is suppressed.  
 @n Options are:
 @n 0, n, no (DEFAULT): Macro logic are suppressed and temporary datasets 
 are cleaned up
 @n 1, y, yes: Macro logic is printed in log and temporary datasets are left 
 behind

 @par Example(s)
 @code{.sas}
    *EXAMPLE 1;
    %exportDataSetToR(libname = sashelp, data = class);

    *EXAMPLE 2;
    proc format;
        value armf 1 = 'Arm 1' 2 = 'Arm2';
        value sexf 1 = 'Male' 2 = 'Female';
        value outcomef 0 = 'No Response' 1 ='Response';
    run;    
    
    data random;
        call streaminit(123);
        array u {50};
        do i=1 to 500+floor(rand("Uniform")*500); *Patients;
           do j=1 to dim(u);*Variables;
               u(j)=rand("Uniform");
           end;
           arm=1+round(u1,1);
           age=floor(18+62*u2);
           gender=ifn(u3>=0.5, 1, 2);
           if arm=1 then
               response=ifn(u7>0.5, 1, 0);
           else if arm=2 then
               response=ifn(u7>0.7, 1, 0);
           output;
        end;
        drop u: i j;
        label arm='Treatment Arm' age='Age' gender='Gender' response='Outcome Status';
        format arm armf. gender sexf. response outcomef.;
    run;
    
    %exportDataSetToR(
        libName = work
        ,data = random
        ,dataFile = ./importDataFromSAS.csv
        ,codeFile = ./importDataFromSAS.R
        ,dataName = dataFromSAS
        ,factorVars = arm gender response
        ,fmtLib = work
        ,overWrite = 1
        ,debug = 0
    );     
 @endcode

 @par Revision History
 n/a
**/

%macro exportDataSetToR(
    libName = WORK
    ,data = 
    ,dataFile = 
    ,codeFile = 
    ,dataName = 
    ,factorVars = 
    ,fmtLib = WORK 
    ,overWrite = 0
    ,debug = 0
    ) / MINOPERATOR;

%put ************************************************************;
%put ************************************************************;
%put *        SET DEFAULTS IF PARAMETERS NOT PROVIDED           *;
%put ************************************************************;
%put ************************************************************;
%if not %length(&dataName) %then %let dataName = &data;

%if not %length(&dataFile) or not %length(&codeFile) %then %do;
    **Find current directory;
    %let rc = %sysfunc(filename(fr,.));
    %let curDir = %sysfunc(pathname(&fr));
    %let rc = %sysfunc(filename(fr));
    
    **If default /sas/studioconfig then send to home directory. Otherwise, use curdir as output path;
    data _null_;
        call symput('dateStamp', left(put("&SYSDATE9."d, YYMMDDn8.)));
    run;
    %if &curDir = /sas/studioconfig %then %let curDir = /home/&sysuserid;
    %if not %length(&dataFile) %then %let dataFile = &curDir/data&dateStamp._FromSAS_&dataName..csv;
    %if not %length(&codeFile) %then %let codeFile = &curDir/r&dateStamp._importFromSAS_&dataName..R;S
%end;

%if not %length(&fmtLib) %then %let fmtLib = &libName;

%put ************************************************************;
%put ************************************************************;
%put *                ERROR CHECKING                            *;
%put ************************************************************;
%put ************************************************************;   

    %if &debug %then %do;
        options symbolgen mlogic mlogicnest;
    %end;        

    %let errorFlag = 0;

    %put *************************************************;
    %put * DATA SET EXISTS AND IS NOT EMPTY              *;
    %put *************************************************;    
    %if not %length(&data) %then %do;
        %put ERROR: Required parameter DATA not provided. Macro will stop executing;
        %let errorFlag=1;
    %end; 
    
    %if %sysfunc(libref(&libname)) %then %do;
        %put %sysfunc(sysmsg());  
        %let errorFlag = 1;
    %end;      
    
    %if not %sysfunc(libref(&libname)) and %length(&data) and not %sysfunc(exist(&libname..&data)) %then %do;
        %put ERROR: Data set %upcase(&libname..&data) does not exist. Macro will stop executing;
        %let ERRORFLAG=1;
    %end; 
    
    %put *************************************************;
    %put * FMTLIB IS ASSIGNED                            *;
    %put *************************************************; 
    %if %length(&fmtLib) %then %do;
        %if %sysfunc(libref(&fmtLib)) %then %do;
            %put %sysfunc(sysmsg());  
            %let errorFlag = 1;
        %end;
    %end;     
   
    %put *************************************************;
    %put * EXPORT DIRECTORIES EXIST                      *;
    %put *************************************************;    
    %if %length(&dataFile) %then %do;
        %let dataFile = %sysfunc(dequote(&dataFile));
        %let dataFileName = %scan(&dataFile, -1, /);
        %let dataFolder = %substr(&dataFile, 1, %index(&dataFile, &dataFileName) - 2);
        
        %if not %sysfunc(fileexist(&dataFolder)) %then %do;
            %put ERROR: Directory &dataFolder does not exist. Macro will stop executing.;
            %let errorFlag = 1;
        %end;
    %end;
    
    %if %length(&codeFile) %then %do;
        %let codeFile = %sysfunc(dequote(&codeFile));
        %let codeFileName = %scan(&codeFile, -1, /);
        %let codeFolder = %substr(&codeFile, 1, %index(&codeFile, &codeFileName) - 2);
    
        %if dataFolder ne codeFolder and not %sysfunc(fileexist(&codeFolder)) %then %do;
            %put ERROR: Directory &codeFolder does not exist. Macro will stop executing.;
            %let errorFlag = 1;
        %end;    
    %end;
    
    %put *************************************************;
    %put * VALID OPTIONS FOR OVERWRITE                   *;
    %put *************************************************;
    %if not(%upcase(&overwrite) in (0 1 YES NO Y N)) %then %do;
        %put ERROR: OVERWRITE must be either 0, 1, n, y, no or yes.;
        %put        OVERWRITE = **&overwrite**;
        %let errorFlag = 1;
    %end;
    
    %put *************************************************;
    %put * IF OVERWRITE = 0, FILE SHOULD NOT EXIST       *;
    %put *************************************************;    
    %if %upcase(&overwrite) in (0 NO N) %then %do;    
        %if %length(&dataFile) and %sysfunc(fileexist(&dataFile)) %then %do;
            %put ERROR: &dataFile already exists and OVERWRITE is &overwrite.. Macro will stop executing.; 
            %put NOTE: Please change DATAFILE name or set OVERWITE to YES.;
            %let errorFlag = 1;
        %end;
 
        %if %length(&codeFile) and %sysfunc(fileexist(&codeFile)) %then %do;
            %put ERROR: &codeFile already exists and OVERWRITE is &overwrite.. Macro will stop executing.;
            %put NOTE: Please change CODEFILE name or set OVERWITE to YES.;
            %let errorFlag = 1;
        %end;    
    %end;
    
    %put *************************************************;
    %put * CSV or XLSX EXTENSION FOR  DATAFILE           *;
    %put *************************************************;
    %if %upcase(%sysfunc(substr(&dataFile, %sysfunc(length(&dataFile))-2, 3))) = CSV %then %let outFmt = CSV;
    %else %if %upcase(%sysfunc(substr(&dataFile, %sysfunc(length(&dataFile))-3, 4))) = XLSX %then %let outFmt = XLSX;
    %else %do;
        %put ERROR: dataFile must be CSV or XLSX file. Macro will stop executing.;
        %let errorFlag = 1;
    %end;      
    
    %put *************************************************;
    %put * VALID OPTIONS FOR DEBUG                       *;
    %put *************************************************;
    %if not(%upcase(&debug) in (0 1 YES NO Y N)) %then %do;
        %put ERROR: DEBUG must be either 0, 1, y, n, no or yes.;
        %put        DEBUG = **&debug**;
        %let errorFlag = 1;
    %end;

    %if &errorflag %then %do ;
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
%put *             PREPARING AND EXPORTING DATA                 *;
%put ************************************************************;
%put ************************************************************;
    
    %put *************************************************;
    %put * CREATING WORKING DATA SET                     *;
    %put *************************************************;    
    data _out_;
        set &libname..&data;
    run;

    %put *************************************************;
    %put * REMOVING LABELS AND FORMATS (EXCEPT DATE/TIME) ;
    %put *************************************************;    
    proc contents data=_out_ out=_contents_ noprint;
    run;
    
    proc sql noprint;
        select name into :notDtTimeVars separated by ' ' from _contents_ where 
            fmtinfo(format, 'cat') not in ('date' 'datetime' 'time');
    quit;
    
    proc datasets lib=work memtype=data nolist;
       modify _out_;
         attrib _all_ label=' ';
         attrib &notDtTimeVars format=;
    run;
    quit;
    
    %put *************************************************;
    %put * EXPORTING DATA SET                            *;
    %put *************************************************;   
    %if &outFmt = CSV %then %do;
        proc export 
            data=_out_
            outfile="&dataFile"
            dbms=csv replace;
        run;
    %end;
    
    %else %if &outFmt = XLSX %then %do;
        libname xlout xlsx "&dataFile";
        data xlout.&dataName;
            set _out_;
        run;
        libname xlout clear;    
    %end;

    %put *************************************************;
    %put * GETTING LABEL INFORMATION                     *;
    %put *************************************************;      
    proc sql noprint;
        select count(*) 
        into :numLabs trimmed
        from sashelp.vcolumn 
        where libname = %upcase("&libname") and memname = %upcase("&data") and not missing(label);
        
        %if &numLabs %then %do;
            select cats("label(&dataName.$", name, ")='", label, "'")
            into :lab1-:lab&numLabs
            from sashelp.vcolumn 
            where libname = %upcase("&libname") and memname = %upcase("&data") and not missing(label);
        %end;
    quit;

    %put *************************************************;
    %put * GETTING FORMAT INFORMATION                    *;
    %put *************************************************;       
    %if %length(&factorVars) = 0 %then %let numFactorVars=0;
    %else %let numFactorVars = %sysfunc(countw(&factorVars));
    
    %if &numFactorVars gt 0 %then %do;
        proc format library=&fmtLib cntlout=_formats_; run;
        
        %let newList = ;
        %let dsid = %sysfunc(open(&libname..&data));    
        %do i = 1 %to &numFactorVars;
            %let thisVar = %scan(&factorVars, &i);
            %let thisName = %sysfunc(varname(&dsid, %sysfunc(varnum(&dsid, &thisVar)))); **ensure proper case for R;
            %let thisFmt = %sysfunc(varfmt(&dsid, %sysfunc(varnum(&dsid, &thisVar))));
            proc sql noprint;
                
                **Get formats;
                %if %length(&thisFmt) %then %do;
                    **Is this a range format?;
                    select 
                    case
                        when start = end then 0
                        else 1
                    end into :thisRangeFmt trimmed
                    from _formats_ (where = (cats(fmtName, ".") = "&thisFmt" ));

                    **If not, get formats, levels;
                    %if not &thisRangeFmt %then %do;
                        select start into :&thisVar._levels separated by "%nrstr(%'), %nrstr(%')"
                        from _formats_ (where = (cats(fmtName, ".") = "&thisFmt" ));
                        
                        select label into :&thisVar._levelLabs separated by "%nrstr(%'), %nrstr(%')"
                        from _formats_ (where = (cats(fmtName, ".") = "&thisFmt" ));
                    %end;                        
                %end;
                
                **If no format, then use the values;
                %if not %length(&thisFmt) %then %do;
                    select distinct &thisVar into :&thisVar._levels separated by "%nrstr(%'), %nrstr(%')"
                    from &libname..&data (where = (not missing(&thisVar)));
                    
                    select distinct &thisVar into :&thisVar._levelLabs separated by "%nrstr(%'), %nrstr(%')"
                    from &libname..&data (where = (not missing(&thisVar)));                
                %end;
            quit;
            
            **If range format then skip and remove variable from list;
            %if &thisRangeFmt %then %do;
                %let newList = &newList;
                %put WARNING: %upcase(&thisVar) in FACTORVARS has a range format and will not be treated as a factor variable.;
            %end;
                        
            **If not, then add quotes to levels and labs;
            %if not &thisRangeFmt %then %do;
                %let newList = &newList &thisName;
                %let &thisVar._levels = %nrstr(%')%nrbquote(&&&thisVar._levels)%nrstr(%');
                %let &thisVar._levelLabs = %nrstr(%')%nrbquote(&&&thisVar._levelLabs)%nrstr(%');
            %end;                
        %end;
        %let rc = %sysfunc(close(&dsid)); 
        
        **Reset factorVars;
        %let factorVars = &newList;
        %if %length(&factorVars) = 0 %then %let numFactorVars=0;
        %else %let numFactorVars = %sysfunc(countw(&factorVars));
    %end;
    
    %put *************************************************;
    %put * GENERATING R CODE FILE                        *;
    %put *************************************************;     
    data _null_;
        file "&codeFile";          
        put "# Written by SAS:";
        put "# %nrstr(%%)exportDataSetToR(";
        put "#    libName = &libName";
        put "#    ,data = &data";
        put "#    ,dataFile = &dataFile";
        put "#    ,codeFile = &codefile";
        put "#    ,dataName = &dataName";
        put "#    ,factorVars = &factorVars";
        put "#    ,fmtLib = &fmtLib";
        put "#    )";
        put " ";
        put " ";
        put "# Clear existing data and graphics";
        put "rm(list=ls())";
        put "graphics.off()";
        put " ";
        put " ";    
        put "# Load libraries";
        put "library(Hmisc)";
        %if &outFmt = XLSX %then %do;
        put "library(openxlsx)";
        %end;
        put " ";
        put " ";    
        put "# Read Data";
        %if &outFmt = CSV %then %do;
        put "&dataName <-";
        put "  read.csv(%nrstr(%')&dataFile%nrstr(%'),";
        put "           na.strings = c('.', ''))";
        %end;
        %else %if &outFmt = XLSX %then %do;
        put "dataFromSAS <-";
        put "  read.xlsx(%nrstr(%')&dataFile%nrstr(%'),";
        put "            na.strings = c('.', ''),";
        put "            sheet = %nrstr(%')&dataName%nrstr(%'),"; 
        put "            startRow = 1, colNames = TRUE, detectDates = TRUE)";      
        %end;
        put " ";
        put " ";
        %if &numLabs %then %do;
            put "# Setting Labels";
            %do i = 1 %to &numLabs;
                put "&&lab&i";
            %end;
            put " ";  
            put " "; 
        %end;
        %if &numFactorVars %then %do;
        put "# Setting Factors(will create new variable for factors)";        
        %do i = 1 %to &numFactorVars;
            %let thisVar = %scan(&factorVars, &i);
            put "&dataName.$&thisVar..factor = factor(&dataName.$&thisVar.,levels=c(&&&thisVar._levels))";
            put "levels(&dataName.$&thisVar..factor)=c(&&&thisVar._levelLabs)";
        %end; 
        %end;
        put " ";
    run; 
    
    /* Clear work lib */
    %if not &debug %then %do;
        proc datasets lib=work memtype=data nodetails nolist nowarn;
            delete _contents_ _out_ _formats_;
        run; quit;
    %end;

%put ************************************************************;
%put NOTE:  Macro &sysmacroname has finished. ;
%put ************************************************************;
options nosymbolgen nomlogic nomlogicnest;

%mend exportDataSetToR;
