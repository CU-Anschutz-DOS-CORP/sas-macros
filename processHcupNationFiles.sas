/**
 @file
 @brief Process HCUP nationwide ZIP files to create SAS databases
 
 @details 
 @par Purpose
 This macro processes HCUP nationwide data files for a specified registry and year by executing 
 the corresponding .ZIP files. Additionally, the macro retrieves and runs the associated SAS 
 load files from the HCUP website, saving the resulting .SAS7BDAT files in the specified 
 output directory (OUTDIR).

 @author Rocio Lopez
 
 @date 09-22-2023
 
 @version SAS 9.4 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %processHcupNationFiles(
     nwDatabase =  
    ,year = 
    ,password = 
    ,baseDir =
    ,outDir = 
    ,overwrite = 0
    ,getDataFiles = 1 
    ,getSasFiles = 1
    ,runSas = 1
    ,debug = 0
 );

 @endcode

 @returns
 .CSV, .SAS and .SAS7BDAT files for the specified database/year
 will be stored in the <OUTDIR>/<NWDATABASE>/<YEAR> folder.

 @param nwDatabase Specify the HCUP nationwide database to process
 @n Allowed options are: NASS, NEDS, NIS, NRD, or KID
  
 @param year Year of data that will be processed (yyyy)

 @param password HCUP-provided case-sensitive password for the files being processed

 @param baseDir Specify the base directory where required materials are saved 
 @n Files should be saved under <BASEDIR>/<NWDATABASE/<YEAR>

 @param outDir Specify the base directory where the extracted and downloaded files will 
 be saved. The macro will create a <OUTDIR>/<NWDATABASE>/<YEAR> folder under this directory,
 if it does not already exist. 
 @n If not provided, then files will be stored under <BASEDIR>//<NWDATABASE/<YEAR>

 @param overwrite Logical stating whether to overwrite existing files
 @n Allowed options are:
 @n 0 for no (DEFAULT)
 @n 1 for yes

 @param getDataFiles Logical indicating whether to extract and copy the .CSV files from
 the original ZIP files

 @param getSasFiles Logical stating whether to download all available SAS load programs
 @n Allowed options are:
 @n 0 for no
 @n 1 for yes (DEFAULT)

 @param runSas Logical stating whether to run SAS load programs and save permanent
 copies of the data files as .SAS7BDAT
 @n Allowed options are:
 @n 0 for no
 @n 1 for yes (DEFAULT)
 @n Note that this will overwrite any existing files in <BASEDIR>/<NWDATABASE>/<YEAR> regardless
 of whether you set overwrite to 0
 
 @param debug Turns on/off several internal logging techniques
 @n Allowed options are: 
 @n 0 for off (DEFAULT)
 @n 1 for on
 
 @note 
 @parblock
 @li This macro has been tested with NRD data on a Windows environment only. User 
 permissions and working environment might affect the ability to use this macro.
  
 @li You must have 7-Zip installed in your computer. You can download from 
 https://www.7-zip.org/. Also, the path to the 7z.exe file must be defined
 in your PATH environmental variable.

 @li This macro does not run correctly in SAS Enterprise Guide.

 @li Unzipping some files can take time. You will see a Windows EXE window with 
 the progress. Please be patient and allow time to run.
  
 @li All extracted and downloaded files will be stored in <OUTDIR>/<NWDATABASE>/<YEAR>

 @li If requesting that the data files be extracted from the .ZIP files, the user 
 must have purchased and saved the files in <BASEDIR>/<NWDATABASE>/<YEAR>. Do not rename 
 or unzip file(s).
 @n  The user should also know the password for the files as this will need to 
 be included in the macro call.

 @li This macro assumes consistency by HCUP in file naming and storage in their website.
 In instances were they deviate from these, the macro will not work properly.

 @li Go to https://hcup-us.ahrq.gov/ to purchase data and/or download supporting materials

 @endparblock  
  
 @par Example(s)
 @code{.sas}
 %processHcupNationFiles(
     nwDatabase = NRD
    ,year = 2020
    ,password = xxxxxx
    ,baseDir = ./hcup
    ,outDir = D:/hcup
 ); 
 @endcode
 
 @par Revision History
 @b 11/14/2024 Added functionality to unzip the HCUP-provided ZIP files that contain the data files
 
**/

%macro processHcupNationFiles(
     nwDatabase =       /* Nationwide database to process e.g. NIS */
    ,year =             /* year of data yyyy */ 
    ,password =         /* Password to unzip data files */
    ,baseDir =          /* location where zip files are e.g. data/hcup/data */
    ,outDir =           /* Location where unziped and downloaded files will be stored */
    ,overwrite = 0      /* 1 to overwrite existing files */
    ,getDataFiles = 1   /* 1 to unzip the dat files */
    ,getSasFiles = 1    /* 1 to extract and copy load programs, will overwrite */
    ,runSas = 1         /* 1 to run SAS load programs and save data, will overwrite */
    ,debug = 0
) / minoperator;

%put ************************************************************;
%put ************************************************************;
%put *             PROCESSING &year &nwDatabase DATA            *;
%put ************************************************************;
%put ************************************************************;

%put ************************************************************;
%put ************************************************************;
%put *                0.1:SET-UP                                *;
%put ************************************************************;
%put ************************************************************;
options noxwait xsync
    %if &debug %then %do ; symbolgen mlogic mprint mlogicnest %end; 
    ;

/* Get current directory */
%let rc = %sysfunc(filename(fr,.));
%let curdir = %sysfunc(pathname(&fr));
%let rc = %sysfunc(filename(fr));

%put ************************************************************;
%put ************************************************************;
%put *                0.2:ERROR CHECKING                        *;
%put ************************************************************;
%put ************************************************************;
%let errorFlag = 0 ;

%put *************************************************;
%put * CHECKING IF 7-ZIP IS AN EXECUTABLE COMMAND    *;
%put *************************************************;
%let temp_output = %sysfunc(getoption(work))\7z_check_output.txt;
%sysexec 7z > "&temp_output" 2>&1;
%put NOTE: CMD message "&SYSRC";

data _null_;
    infile "&temp_output" firstobs=2 obs=2 length = len lrecl = 32767;
    input line $varying32767. len;
    if index(line, '7-Zip') then call symputx('sevenzip_available', '1');
    else call symputx('sevenzip_available', '0');
run;
%if not(&sevenzip_available) %then %do;
    %put ERROR: 7-Zip not found, make sure you have installed 7-Zip and 
added this to your PATH variable. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%put *************************************************;
%put * CHECKING ALL REQUIRED PARAMETERS ARE PROVIDED *;
%put *************************************************;
%if not(%length(&nwDatabase)) %then %do;
    %put ERROR: Required parameter NWDATABASE is not provided. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%if not(%length(&baseDir)) %then %do;
    %put ERROR: Required parameter BASEDIR is not provided. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%if not(%length(&outDir)) %then %do;
    %let outDir = &baseDir;
    %if %length(baseDir) and %length(&nwDatabase) %then %do;
        %put NOTE: Required parameter OUTDIR is not provided. Files will be stored under &baseDir/&nwDatabase/&year.;
    %end;
    %else %do;
        %put ERROR: Required parameter OUTDIR is not provided. Macro will stop executing.;
        %let errorFlag = 1;
    %end;
%end;

%if not(%length(&year)) %then %do;
    %put ERROR: Required parameter YEAR is not provided. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%if &getDataFiles and not(%length(&password)) %then %do;
    %put ERROR: Required parameter PASSWORD is not provided. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%put *************************************************;
%put * CHECKING DIRECTORIES EXIST                    *;
%put *************************************************;
%if %length(&baseDir) and %length(&nwDatabase) and %length(&year) %then %do;
    %if not(%sysfunc(fileexist(&baseDir/&nwDatabase/&year))) %then %do;
        %put ERROR: Directory &baseDir/&nwDatabase/&year does not exist. Macro will stop executing.;
        %let errorFlag = 1;
    %end;

%end;

%if %length(&outDir) and not(%sysfunc(fileexist(&outDir))) %then %do;
    %put ERROR: Directory OUTDIR &outDir does not exist. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%put *************************************************;
%put * CHECKING PARAMETER OPTIONS ARE VALID          *;
%put *************************************************;
%if not(%upcase(&nwDatabase) in (NASS NEDS NIS NRD KID)) %then %do;
    %put ERROR: NWDATABASE must be either NASS, NEDS, NIS, NRD, or KID. Macro will stop executing.;
    %let errorFlag = 1;
%end;

data _null_;
    call symput('todayYYYY', left(year("&sysDate9"d)));
run;
%if not(%length(&year)) %then %do;
    %if &year < 1989 or &year gt &todayYYYY %then %do;
        %put ERROR: &year is not a valid data year (YYYY). Macro will stop executing.;
        %let errorFlag = 1; 
    %end;
%end;

%if not(&overwrite in (0 1)) %then %do;
    %put ERROR: OVERWRITE must be either 0 or 1. Macro will stop executing.;
    %let errorFlag = 1; 
%end; 

%if not(&getDataFiles in (0 1)) %then %do;
    %put ERROR: GETDATAFILES must be either 0 or 1. Macro will stop executing.;
    %let errorFlag = 1; 
%end; 

%if not(&getSasFiles in (0 1)) %then %do;
    %put ERROR: GETSASFILES must be either 0 or 1. Macro will stop executing.;
    %let errorFlag = 1; 
%end; 

%if not(&runSas in (0 1)) %then %do;
    %put ERROR: RUNSAS must be either 0 or 1. Macro will stop executing.;
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
%put *                0.3:DEFINING OUTPUT PATH                  *;
%put ************************************************************;
%put ************************************************************;
%let fileDir = &outDir/%lowcase(&nwDatabase)/&year;

%put ************************************************************;
%put ************************************************************;
%put *                0.4: DEFINING HCUP URL                    *;
%put ************************************************************;
%put ************************************************************;
%if %upcase(&nwDatabase) = NIS %then %do;
    %let hcupToolsUrl = https://hcup-us.ahrq.gov/db/nation/nis/tools/pgms;
%end;
%else %do; 
    %let hcupToolsUrl = https://hcup-us.ahrq.gov/db/nation/%lowcase(&nwDatabase)/pgms;
%end;

%put ************************************************************;
%put ************************************************************;
%put *            1:CREATING OUTPUT DIRECTORY                   *;
%put ************************************************************;
%put ************************************************************;
options dlcreatedir; **This will NOT overwrite if the folders exist;
libname new ("&outDir/%lowcase(&nwDatabase)", 
             "&fileDir");
libname new clear;
options nodlcreatedir;

%if &getDataFiles %then %do;
    
    %put ************************************************************;
    %put ************************************************************;
    %put *             2:EXECUTING ALL ZIP FILES                    *;
    %put ************************************************************;
    %put ************************************************************;
    data __exeZipFiles;
        length fref $8 fname $200;
        did = filename(fref, "&baseDir/&nwDatabase/&year");
        did = dopen(fref);
        do i = 1 to dnum(did);
            fname = dread(did,i);
            if index(upcase(fname), 'EXE') and index(fname, "&year") or
               index(upcase(fname), 'ZIP') and index(fname, "&year") then output;
        end;
        did = dclose(did);
        did = filename(fref);
        keep fname;
    run;

    proc sql noprint;
         select nobs into :numExeZipFiles separated by ' ' 
         from dictionary.tables
         where libname='WORK' and memname='__EXEZIPFILES';
    quit;
    %put NOTE: Total EXE and/or ZIP files to be executed = &numExeZipFiles;   
    %if not(&numExeZipFiles) %then %put WARNING: No EXE or ZIP files in directory.;

    %if &numExeZipFiles %then %do i=1 %to &numExeZipFiles;
        data _null_;
            set __exeZipFiles (obs = &i firstobs = &i);
            call symput('thisFile', strip(fname));
            call symput('thisCSVFile', catx(".", scan(fname, 1, "."), "CSV"));
        run;

        %if %sysfunc(fileexist("&fileDir/&thisCsvFile")) 
        and not(&overwrite) %then %do;
            %put NOTE: &fileDir/&thisCsvFile already exists and will not be overwritted.; 
            %put **To allow overwriting files, use OVERWRITE=1.;
        %end;
        
        %else %do;
           %if &overwrite %then %do;
                %sysexec 7z x "&baseDir/&nwDataBase/&year/&thisFile" -o"&fileDir" -p&password -aoa;
            %end;
           %else %do;
                %sysexec 7z x "&baseDir/&nwDataBase/&year/&thisFile" -o"&fileDir" -p&password;
            %end;
            %put NOTE: CMD message "&SYSRC";
        %end;

    %end; **end to numExeFiles loop;

    %if not(&debug) %then %do ;
        proc datasets lib=work nodetails nolist nowarn;
            delete __exeZipFiles;
        run; quit;  
    %end;   

%end; **end to step 2;

%if &getSASFiles %then %do;

    %put ************************************************************;
    %put ************************************************************;
    %put *             3:GETTING SAS LOAD FILES                     *;
    %put ************************************************************;
    %put ************************************************************;
    %let sasFiles = Core Hospital DX_PR_GRPS Severity;
    %do loop = 1 %to 4;
        %let thisSasFile = %scan(&sasFiles, &loop);

        %put *************************************************;
        %put * DOWNLOADING &thisSasFile LOAD FILE IF IT EXISTS;
        %put *************************************************;
        filename headout TEMP;
        proc http 
            url="&hcupToolsUrl/SASLoad_%upcase(&nwDatabase)_&year._&thisSasFile..SAS" 
            headerout=headout;
        run;
       
        data _null_;
            infile headout scanover truncover;
            input @'HTTP/1.1' code 4. message $255.;
            if _n_ = 1 then call symput("message", strip(message));
        run;            
        
        %if "&message" eq "OK" %then %do;
            
            %if %sysfunc(fileexist("&fileDir/SASLoad_%upcase(&nwDatabase)_&year._&thisSasFile..sas"))
              and not(&overwrite) %then %do;
                %put NOTE: &fileDir/SASLoad_%upcase(&nwDatabase)_&year._&thisSasFile..sas already exists and will not be overwritten.;
                %put **To allow overwriting files, use OVERWRITE=1.;
            %end;       
        
            %if not(%sysfunc(fileexist("&fileDir/SASLoad_%upcase(&nwDatabase)_&year._&thisSasFile..sas")))
              or &overwrite %then %do;          
            
                filename download "&fileDir/SASLoad_%upcase(&nwDatabase)_&year._&thisSasFile..sas";
                proc http
                    url="&hcupToolsUrl/SASLoad_%upcase(&nwDatabase)_&year._&thisSasFile..SAS"
                    out=download;
                run;
                filename download clear;
            
            %end; **end to file download;   
            
        %end; **end to message ok;
                
        %if "&message" ne "OK" %then %do;
        
            %put NOTE: SASLoad_%upcase(&nwDatabase)_&year._&thisSasFile..SAS could not be downloaded.;
            %put NOTE: Reason not downloaded is: &message.. ;
            %put NOTE: Will search for SASLoad_%upcase(&nwDatabase)_&year._&thisSasFile._V2.SAS ;
                
            filename headout TEMP;
            proc http 
                url="&hcupToolsUrl/SASLoad_%upcase(&nwDatabase)_&year._&thisSasFile._V2.SAS" 
                headerout=headout;
            run;
           
            data _null_;
                infile headout scanover truncover;
                input @'HTTP/1.1' code 4. message $255.;
                if _n_ = 1 then call symput("message2", strip(message));
            run;            
            
            %if "&message2" eq "OK" %then %do;
                
                %if %sysfunc(fileexist("&fileDir/SASLoad_%upcase(&nwDatabase)_&year._&thisSasFile._V2.sas"))
                  and not(&overwrite) %then %do;
                    %put NOTE: &fileDir/SASLoad_%upcase(&nwDatabase)_&year._&thisSasFile._V2.sas already exists and will not be overwritten.;
                    %put **To allow overwriting files, use OVERWRITE=1.;
                %end;       
            
                %if not(%sysfunc(fileexist("&fileDir/SASLoad_%upcase(&nwDatabase)_&year._&thisSasFile._V2.sas")))
                  or &overwrite %then %do;          
                
                    filename download "&fileDir/SASLoad_%upcase(&nwDatabase)_&year._&thisSasFile._V2.sas";
                    proc http
                        url="&hcupToolsUrl/SASLoad_%upcase(&nwDatabase)_&year._&thisSasFile._V2.SAS"
                        out=download;
                    run;
                    filename download clear;
                
                %end; **end to file download;   
                
            %end; **end to message2 ok;
                
            %if "&message2" ne "OK" %then %do;    
                %put NOTE: SASLoad_%upcase(&nwDatabase)_&year._&thisSasFile._V2.SAS could not be downloaded.;
                %put NOTE: Reason not downloaded is: &message.. ;
            %end;               
        %end;   **end to message not ok;                        
                
    %end; **end to loop 1 to 4;

%end;  *end to step 3;

%if &runSAS %then %do;
    
    %put ************************************************************;
    %put ************************************************************;
    %put *           4: RUNNING SAS CODE AND SAVING SAS7BDAT        *;
    %put ************************************************************;
    %put ************************************************************;  
    /* Change working directory to &fileDir */
    /* This is so the SAS load programs can run without editing */
    %let rc = %sysfunc(dlgcdir("&fileDir"));
    
    data __sasFiles __dataFiles;
        length fref $8 fname $200;
        did = filename(fref, "&fileDir");
        did = dopen(fref);
        do i = 1 to dnum(did);
            fname = dread(did,i);
            if index(fname, 'SASLoad_') = 1 and findw(upcase(fname), 'SAS') then output __sasFiles;
            else if findw(upcase(fname), 'CSV') 
                or findw(upcase(fname), 'ASC') 
                then output __dataFiles;
        end;
        did = dclose(did);
        did = filename(fref);
        keep fname;
    run;

    proc sql noprint;
         select nobs into :numSasFiles separated by ' ' 
         from dictionary.tables
         where libname='WORK' and memname='__SASFILES';

         select strip(scan(fname, 1, '.')) into :dataFiles separated by ' '
         from __dataFiles;
         %let numDataFiles = &sqlObs;

        select strip(scan(a.fname, 1, '.')) into :missingDataFile separated by ' '
        from __sasFiles a
        full join __dataFiles b
        on strip(tranwrd(tranwrd(scan(a.fname, 1, '.'), "SASLoad_", ""), "_V2", "")) = strip(scan(b.fname, 1, '.')) 
        where b.fname is NULL;
        %let numMissDataFiles = &sqlObs;
    quit;

    %put NOTE: Total SAS files to be executed = &numSasFiles;   
    %if not(&numSasFiles) %then %put WARNING: No SAS files in directory.;
    
    %if &numDataFiles %then %put NOTE: Data files in directory are: &dataFiles;
    %else %put WARNING: No data files found in directory. No SAS programs will run.;

    %if &numMissDataFiles %then %do;
        %put WARNING: The following SAS load program(s) will not run because data file(s) not found: &missingDataFile;
    %end;
    
    %if &numSasFiles and &numDataFiles %then %do;   
        %do i = 1 %to &numDataFiles;    
            data _null_;
                set __sasFiles (obs = &i firstobs = &i);
                call symput('thisFile', strip(scan(fname, 1, '.')));
                call symput('thisCsvFile', strip(tranwrd(tranwrd(scan(fname, 1, '.'), "SASLoad_", ""), "_V2", "")));
            run;
            
            %if %sysfunc(fileexist("&fileDir/&thisFile..sas"))
                and &thisCsvFile in (&dataFiles)
                %then %do;                               
                
                %include "&fileDir/&thisFile..sas"; 

                libname hcupSave "&fileDir";
                proc copy in=work out=hcupSave memtype=(data);
                    select &thisCsvFile ;
                run;        
                libname hcupSave clear;  

                %if not(&debug) %then %do ;
                proc datasets lib=work memtype=data nodetails nolist nowarn;
                    delete &thisCsvFile ;
                run; quit;  
                %end;
                
            %end;
            %else %do;
                %put WARNING: &thisCsvFile CSV file not found in directory;
            %end;
        %end; **end to do 1 to numDataFiles loop;

    %end; **end to if numSasFiles loop;

    %if not(&debug) %then %do ;
        proc datasets lib=work nodetails nolist nowarn;
            delete __sasFiles __dataFiles ;
        run; quit;  
    %end;   
    
%end; **end to step 4;

%put ************************************************************;
%put ************************************************************;
%put *                3.CLEAN UP                                *;
%put ************************************************************;
%put ************************************************************;
%let rc = %sysfunc(dlgcdir("&curDir"));

options xwait 
    %if &debug %then %do ; nosymbolgen nomlogic nomprint nomlogicnest %end; 
    ;

%put ************************************************************;
%put NOTE:  Macro &sysmacroname has finished. ;
%put ************************************************************;

%mend processHcupNationFiles;
