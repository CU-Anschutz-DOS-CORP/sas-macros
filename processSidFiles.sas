/**
 @file
 @brief Process HCUP SID raw files to create SAS databases
 
 @details 
 @par Purpose
 This macro will execute the .EXE HCUP SID data files
 for the specified state and year. It will also download and unzip
 the AHAL file if it exits. Lastly, it will download and run the
 associated SAS files from the HCUP website saving .SAS7BDAT files
 in the specified OUTDIR.    

 @author Rocio Lopez
 
 @date 11-04-2024
 
 @version PC SAS 9.4 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %processSidFiles(
     sidDir =
    ,outDir = 
    ,st = 
    ,year =
    ,password
    ,overwrite = 0
    ,getSasFiles = 1
    ,getAhalFiles = 1
    ,getDataFiles = 1
    ,runSas = 1
    ,deleteAhalExeFile = 1
    ,debug = 1
 );
 @endcode

 @returns
 .ZIP, .SAS, .EXE, .SAS7BDAT, .ASC and/or .LOC files for the specified state/year.
 The macro will also create a __logOut file that tracks each processed file.
 
 @param sidDir Specify the base directory where compressed SID materials are saved.
 Files should be stored under <SIDDIR>/<ST> 

 @param outDir Specify the base directory where the output files will be saved. 
 The macro will create a <OUTDIR>/<ST>/<YEAR> folder under this directory.

 @param st Two-letter abbreviation for the state of interest
 
 @param year Year of data that will be processed (yyyy)

 @param password HCUP-provided case-sensitive password for the state/year SID files

 @param overwrite Logical stating whether to overwrite exising files
 @n Allowed options are:
 @n 0 for no (DEFAULT)
 @n 1 for yes 

 @param getAhalFiles Logical stating whether to download AHAL zip file, if available, 
 and save it in <SIDDIR>/<ST>
 @n Allowed options are:
 @n 0 for no
 @n 1 for yes (DEFAULT) 

 @param getDataFiles Logical stating whether to extract and copy data .ASC files
 @n Allowed options are:
 @n 0 for no
 @n 1 for yes (DEFAULT)
 @n User must have purchased and saved the files in <SIDDIR>/<ST>
 where yyyymmmdd is the date stamp for the date of purchase
 
 @param getSasFiles Logical stating whether to download SAS load programs for the
 .ASC extracted from compressed files and save them in <OUTDIR>/<ST>/<YEAR> 
 @n Allowed options are:
 @n 0 for no
 @n 1 for yes (DEFAULT)

 @param runSas Logical stating whether to run SAS load programs and save permanent
 .SAS7BDAT copies in <OUTDIR>/<ST>/<YEAR>
 @n Allowed options are:
 @n 0 for no
 @n 1 for yes (DEFAULT)
 @n Note that this will overwrite any existing files in <OUTDIR>/<ST>/<YEAR> regardless
 of whether you set overwrite to 0
 
 @param deleteAhalExeFile Logical stating whether to delete the AHAL .EXE file if it exists 
 (in some years the zip has an exe which has the asc file)
 @n Allowed options are 0 for no (DEFAULT) and 1 for yes

 @param debug Turns on/off several internal logging options
 @n Allowed options are: 
 @n 0 for off (DEFAULT)
 @n 1 for on
 
 @note 
 @parblock
 @li This macro has been tested on a Windows environment only. User permissions
 and working environment might affect the ability to use this macro.
  
 @li You must have 7-Zip installed in your computer. You can download from 
 https://www.7-zip.org/. Also, the path to the 7z.exe file must be defined
 in your PATH environmental variable.

 @li This does not run correctly in SAS Enterprise Guide.
 
 @li All extracted files will be stored in <OUTDIR>/<ST>/<YEAR>

 @li If requesting data files, user must have purchased and saved the files in 
 <SIDDIR>/<ST>. Do not rename or unzip file(s).
 @n  The user should also know the passwords for each file as these will need to 
 be included in the macro call.

 @li Go to https://hcup-us.ahrq.gov/ to purchase data and/or download supporting materials

 @li At the moment this macro only handles SID data but I might evolve this in the 
 future to %processStateFiles to also allow handling of SEDD or SASD files.

 @endparblock  
  
 @par Example(s)
 @code{.sas}
 %processSidFiles(
     sidDir = ./hcup/sid
    ,st = AR
    ,year = 2020
 );
 @endcode
 
 @par Revision History
 @b 11/07/2024 Some AHAL ZIP files have an EXE file inside so an additional step to
 execute these has been added
**/

%macro processSidFiles(
     sidDir =           /* location where sid materials are */
    ,outDir =           /* location for output files */
    ,st =               /* state 2 letter abbreviation */ 
    ,year =             /* year of data yyyy */ 
    ,password =         /* Password to exctract the SID files */
    ,overwrite = 0      /* 1 to overwrite existing files */
    ,getAhalFiles = 1   /* 1 to download and extract AHAL files, will overwrite */
    ,getDataFiles = 1   /* 1 to extract and copy data files, will overwrite */
    ,getSasFiles = 1    /* 1 to extract and copy load programs, will overwrite */
    ,runSas = 1         /* 1 to run SAS load programs and save data, will overwrite */
    ,deleteAhalExeFile = 1 /* 1 to delete the unzipped AHAL .EXE file */
    ,debug = 0
) / minoperator;

%put ************************************************************;
%put ************************************************************;
%put *                PROCESSING &year &st DATA                 *;
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

/* State */
%let st = %upcase(&st);
data _null_;
    call symput('state', compress(stnamel("&st")));
run;

/* Define file directory */
%let fileDir = &outDir/&st/&year;

/* Base URL for HCUP tools website (where SAS and AHAL files are) */
%let hcupToolsUrl = https://hcup-us.ahrq.gov/db/state/sidc/tools;

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
%if not(%length(&sidDir)) %then %do;
    %put ERROR: Required parameter SIDDIR is not provided. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%if not(%length(&outDir)) %then %do;
    %put ERROR: Required parameter OUTDIR is not provided. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%if not(%length(&st)) %then %do;
    %put ERROR: Required parameter ST is not provided. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%if not(%length(&year)) %then %do;
    %put ERROR: Required parameter YEAR is not provided. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%if not(%length(&password)) %then %do;
    %put ERROR: Required parameter PASSWORD is not provided. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%put *************************************************;
%put * CHECKING DIRECTORIES EXIST                    *;
%put *************************************************;
%if not(%length(&sidDir)) and not(%sysfunc(fileexist(&sidDir))) %then %do;
    %put ERROR: Directory SIDDIR &SIDDIR does not exist. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%if not(%length(&outDir)) and not(%sysfunc(fileexist(&outDir))) %then %do;
    %put ERROR: Directory OUTDIR &outDir does not exist. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%put *************************************************;
%put * CHECKING PARAMETER OPTIONS ARE VALID          *;
%put *************************************************;
%if not(%length(&state)) %then %do;
    %put ERROR: &st is not a valid state abbreviation. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%let todayYYYY = %sysfunc(putn("&SYSDATE9."d, year4));
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

%if not(&getSasFiles in (0 1)) %then %do;
    %put ERROR: GETSASFILES must be either 0 or 1. Macro will stop executing.;
    %let errorFlag = 1; 
%end; 

%if not(&getAhalFiles in (0 1)) %then %do;
    %put ERROR: GETAHALFILES must be either 0 or 1. Macro will stop executing.;
    %let errorFlag = 1; 
%end; 

%if not(&getDataFiles in (0 1)) %then %do;
    %put ERROR: GETDATAFILES must be either 0 or 1. Macro will stop executing.;
    %let errorFlag = 1; 
%end; 

%if not(&runSas in (0 1)) %then %do;
    %put ERROR: RUNSAS must be either 0 or 1. Macro will stop executing.;
    %let errorFlag = 1; 
%end; 

%if not(&deleteAhalExeFile in (0 1)) %then %do;
    %put ERROR: DELETEAHALEXEFILE must be either 0 or 1. Macro will stop executing.;
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
%put *                0.3:CREATING LOG FILE                     *;
%put ************************************************************;
%put ************************************************************;
options varinitchk=nonote;
data __logOut;
    informat state $2. year 8. file $50. message $150. errorFlag 8.;
run;
options varinitchk=note;
%let logFlag = 0;

%put ************************************************************;
%put ************************************************************;
%put *                1:CREATING DIRECTORY                      *;
%put ************************************************************;
%put ************************************************************;
options dlcreatedir; **This will NOT overwrite if the folders exist;
libname new ("&outDir/&st", 
             "&outDir/&st/&year");
libname new clear;
options nodlcreatedir;

%if &getAHALFiles %then %do;

    %put ************************************************************;
    %put ************************************************************;
    %put *            2:GETTING AHAL FILE IF AVAILABLE              *;
    %put ************************************************************;
    %put ************************************************************;
    
    %if %sysfunc(fileexist("&fileDir/&st._SIDC_&year._AHAL.zip"))
      and not(&overwrite) %then %do;
        %put NOTE: &fileDir/&st._SIDC_&year._AHAL.zip already exists and will not be overwritten.;
        %put **To allow overwriting files, use OVERWRITE=1.;
        %let logMessage = File already exists in directory.;
    %end; **end to if ahal.zip exists;

    %if not(%sysfunc(fileexist("&fileDir/&st._SIDC_&year._AHAL.zip")))
      or &overwrite %then %do;
        %put *************************************************;
        %put * CHECKING IF AHAL FILE EXISTS IN WEBSITE       *;
        %put *************************************************;
        filename headout TEMP;
        proc http 
            url="&hcupToolsUrl/ahalinkage/&st._SIDC_&year._AHAL.zip" 
            headerout=headout;
        run;
            
        data _null_;
            infile headout scanover truncover;
            input @'HTTP/1.1' code 4. message $255.;
            if _n_ = 1 then call symput("message", strip(message));
        run;     
        
        %if "&message" ne "OK" %then %do;
            %put WARNING: &st._SIDC_&year._AHAL.zip could not be downloaded.;
            *%put NOTE: Reason not downloaded is: &message.. ;
            %let logMessage = File could not be downloaded. Reason not downloaded is: &message..;
            %let logFlag = 1;
        %end;   

        %if "&message" eq "OK" %then %do;
            %put *************************************************;
            %put * DOWLOADING AHAL ZIP                           *;
            %put *************************************************;
            filename download "&fileDir/&st._SIDC_&year._AHAL.zip";
            proc http
                url="&hcupToolsUrl/ahalinkage/&st._SIDC_&year._AHAL.zip"
                out=download;
            run;
            filename download clear;

            %if %sysfunc(fileexist("&fileDir/&st._SIDC_&year._AHAL.zip")) %then 
                %let logMessage = File downloaded successfuly.;
            %else %do;
                %let logMessage = File exists online but unable to download.;
                %let logFlag = 1;
            %end;
        %end; **end if message OK;
    %end; **end to download zip file;

    %if (%sysfunc(fileexist("&fileDir/&st._SIDC_&year._AHAL.exe"))
        or %sysfunc(fileexist("&fileDir/&st._SID_&year._AHAL.asc")))
      and not(&overwrite) %then %do;
        %put NOTE: &fileDir/&st._SIDC_&year._AHAL.zip has already been unzipped and will not be overwritten.;
        %put **To allow overwriting files, use OVERWRITE=1.;
        %let logMessage = &logMessage File already unzipped;
    %end; **end to if ahal.exe exists;
    
    %if (not(%sysfunc(fileexist("&fileDir/&st._SIDC_&year._AHAL.exe")))
        and not(%sysfunc(fileexist("&fileDir/&st._SID_&year._AHAL.asc"))))
      or &overwrite %then %do;
        %if %sysfunc(fileexist("&fileDir/&st._SIDC_&year._AHAL.zip")) %then %do;
            %put *************************************************;
            %put * EXTRACTING AHAL ZIP FILE                      *;
            %put *************************************************;         
            %if &overwrite %then %do;
                %sysexec 7z x "&fileDir/&st._SIDC_&year._AHAL.zip" -o"&fileDir" -aoa;
            %end;
            %else %do;
                %sysexec 7z x "&fileDir/&st._SIDC_&year._AHAL.zip" -o"&fileDir";
            %end;
            %put NOTE: CMD message "&SYSRC";

            %if %sysfunc(fileexist("&fileDir/&st._SIDC_&year._AHAL.exe")) %then %do;
                %if &overwrite %then %do;
                    %sysexec 7z x "&fileDir/&st._SIDC_&year._AHAL.exe" -o"&fileDir" -aoa;
                %end;
                %else %do;
                    %sysexec 7z x "&fileDir/&st._SIDC_&year._AHAL.exe" -o"&fileDir";
                %end;
                %put NOTE: CMD message "&SYSRC";
            %end;

            %if %sysfunc(fileexist("&fileDir/&st._SID_&year._AHAL.asc"))
              %then %let logMessage = &logMessage File unzipped successfuly;
            %else %do;
                %let logMessage = &logMessage Unable to unzip file;
                %let logFlag = 1;
            %end;
        %end; **end to if zip exists;   
 
        proc sql;
            insert into work.__logOut
                set state = "&st",
                    year = &year,
                    file = "&st._SIDC_&year._AHAL.zip",
                    message = "&logMessage",
                    errorFlag = &logFlag;
        quit;   
        %let logMessage=;
        %let logFlag = 0;
    %end; **end to extracting zip file;
%end; **end to step 2;

%if &getDataFiles %then %do;
    
    %put ************************************************************;
    %put ************************************************************;
    %put *        3:EXECUTING ALL EXE AND ZIP FILES                 *;
    %put ************************************************************;
    %put ************************************************************;
    data __exeZipFiles;
        length fref $8 fname $200;
        did = filename(fref, "&sidDir/&st");
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
    %put NOTE: Total EXE and ZIP files to be executed = &numExeZipFiles;   
    %if not(&numExeZipFiles) %then %do;
        %put WARNING: No EXE or ZIP files in directory.;
        proc sql;
            insert into work.__logOut
                set state = "&st",
                    year = &year,
                    file = "EXE/ZIP",
                    message = "No Exe or Zip files in directory.",
                    errorFlag = 1;
        quit; 
    %end;

    %if &numExeZipFiles %then %do i=1 %to &numExeZipFiles;
        data _null_;
            set __exeZipFiles (obs = &i firstobs = &i);
            call symput('thisFile', strip(fname));
            if index(fname, 'SIDC') > 0 then fname2 = catx(".", tranwrd(strip(scan(fname, 1, ".")), 'SIDC', 'SID'), "asc");
            else if index(fname, 'sidc') > 0 then fname2 = catx(".", tranwrd(strip(scan(fname, 1, ".")), 'sidc', 'sid'), "ASC");
            call symput('thisAscFile', fname2);
        run;

        %if %sysfunc(fileexist("&fileDir/&thisAscFile")) 
        and not(&overwrite) %then %do;
            %put NOTE: &fileDir/&thisFile already exists and will not be overwritted.; 
            %put **To allow overwriting files, use OVERWRITE=1.;
            %let logMessage = File already executed.;
        %end;
        
        %else %do;
           %if &overwrite %then %do;
                %sysexec 7z x "&sidDir/&st/&thisFile" -o"&fileDir" -p&password -aoa;
            %end;
           %else %do;
                %sysexec 7z x "&sidDir/&st/&thisFile" -o"&fileDir" -p&password;
            %end;
            %put NOTE: CMD message "&SYSRC";

            %if %sysfunc(fileexist("&fileDir/&thisAscFile")) %then 
                %let logMessage = File executed successfuly.;
            %else %do;
                %let logMessage = Unable to execute file.;
                %let logFlag = 1;
            %end;
        %end;

        proc sql;
            insert into work.__logOut
                set state = "&st",
                    year = &year,
                    file = "&thisFile",
                    message = "&logMessage",
                    errorFlag = &logFlag;
        quit;   
        %let logMessage=;
        %let logFlag = 0;
    %end; **end to numExeFiles loop;

    %if not(&debug) %then %do ;
        proc datasets lib=work nodetails nolist nowarn;
            delete __exeZipFiles;
        run; quit;  
    %end;   

%end; **end to step 3;

%put ************************************************************;
%put ************************************************************;
%put *             4:GETTING LIST OF ASC FILES                  *;
%put ************************************************************;
%put ************************************************************;
data __ascFiles;
    length fref $8 fname $200;
    did = filename(fref, "&fileDir");
    did = dopen(fref);
    do i = 1 to dnum(did);
        fname = dread(did,i);
        if index(upcase(fname), 'ASC') then output;
    end;
    did = dclose(did);
    did = filename(fref);
    keep fname;
run;

proc sql noprint;
     select nobs into :numAscFiles separated by ' ' 
     from dictionary.tables
     where libname='WORK' and memname='__ASCFILES';

    select strip(scan(fname, 1, '.')) into :ascFiles separated by ' '
    from __ascFiles;
quit;

%if &numAscFiles %then %put NOTE: ASC files in directory are: &ascFiles;
%else %do;
    %put WARNING: No ASC files in directory. No SAS programs will downloaded.;
    proc sql;
        insert into work.__logOut
            set state = "&st",
                year = &year,
                file = "ASC",
                message = "No ASC files in directory. No SAS programs will downloaded.",
                errorFlag = 1;
    quit; 
%end;

%if &numAscFiles and &getSASFiles %then %do;

    %put ************************************************************;
    %put ************************************************************;
    %put *             5:GETTING SAS LOAD FILES                     *;
    %put ************************************************************;
    %put ************************************************************;
    %do loop = 1 %to &numAscFiles;
        %let thisSasFile = %upcase(%scan(&ascFiles, &loop));
        
        %if %sysfunc(fileexist("&fileDir/&thisSasFile..sas"))
          and not(&overwrite) %then %do;
            %put NOTE: &fileDir/&thisSasFile..sas already exists and will not be overwritten.;
            %put **To allow overwriting files, use OVERWRITE=1.;
            %let logMessage = File already exists in directory.;
        %end;       
        
        %if not(%sysfunc(fileexist("&fileDir/&thisSasFile..sas")))
          or &overwrite %then %do;
        
            %put *************************************************;
            %put * DOWNLOADING &thisSasFile LOAD FILE IF IT EXISTS;
            %put *************************************************;
            filename headout TEMP;
            proc http 
                url="&hcupToolsUrl/sasload/&thisSasFile..sas" 
                headerout=headout;
            run;
       
            data _null_;
                infile headout scanover truncover;
                input @'HTTP/1.1' code 4. message $255.;
                if _n_ = 1 then call symput("message", strip(message));
            run;     
        
            %if "&message" ne "OK" %then %do;
                %put NOTE: &thisSasFile..sas could not be downloaded.;
                %put NOTE: Reason not downloaded is: &message.. ;
                %let logMessage = File could not be downloaded.;
                %let logFlag = 1;
            %end;   **end to message not ok;
    
            %if "&message" eq "OK" %then %do;
                filename download "&fileDir/&thisSasFile..sas";
                proc http
                    url="&hcupToolsUrl/sasload/&thisSasFile..sas"
                    out=download;
                run;
                filename download clear;

                %if %sysfunc(fileexist("&fileDir/&thisSasFile..sas")) %then
                    %let logMessage = File downloaded successfuly.;
                %else %do;
                    %let logMessage = File exists online but unable to download.;
                    %let logFlag = 1;
                %end;

            %end; **end to message ok;
        
        %end; **end to file download;
        
    proc sql;
        insert into work.__logOut
        set state = "&st",
            year = &year,
            file = "&thisSasFile..sas",
            message = "&logMessage",
            errorFlag = &logFlag;
    quit;   
    %let logMessage=;
    %let logFlag = 0;

    %end; **end to numAscFiles loop;

%end;  *end to step 5;

%if &numAscFiles and &runSAS %then %do;
    
    %put ************************************************************;
    %put ************************************************************;
    %put *           6: RUNNING SAS CODE AND SAVING SAS7BDAT        *;
    %put ************************************************************;
    %put ************************************************************;  
    %do loop = 1 %to &numAscFiles;

        data _null_;
            set __ascFiles (obs = &loop firstobs = &loop);
            fname2 = strip(scan(fname, 1, "."));
            call symput('thisSASFile', strip(upcase(fname2)));
            if index(fname, 'skinny') then fname3 = fname2;
            else if index(fname, 'SID') > 0 then fname3 = tranwrd(fname2, 'SID', 'SIDC');
            else if index(fname, 'sid') > 0 then fname3 = tranwrd(fname2, 'sid', 'sidc');
            call symput('this7BdatFile', fname3);
        run;

        %if not(%sysfunc(fileexist("&fileDir/&thisSasFile..sas"))) %then %do;
            %let logMessage = SAS load file not found.; 
            %let logFlag = 1;
        %end;

        %if %sysfunc(fileexist("&fileDir/&this7BdatFile..sas7bdat")) and not(&overwrite) %then 
            %let logMessage = &logMessage SAS data set alreay exists in directory.;

        %if %sysfunc(fileexist("&fileDir/&thisSasFile..sas"))
            and (not(%sysfunc(fileexist("&fileDir/&this7BdatFile..sas7bdat")))  or &overwrite)
            %then %do;      
 
            /* Change working directory to &fileDir */
            /* This is so the SAS load programs can run without editing */
            %let rc = %sysfunc(dlgcdir("&fileDir"));
                                
            %include "&fileDir/&thisSasFile..sas";
                
            %let rc = %sysfunc(dlgcdir("&curDir"));
                        
            libname hcupSave "&fileDir";

            proc copy in=work out=hcupSave memtype=(data);
                select &this7BdatFile ;
            run;        

            %if %sysfunc(exist(hcupSave.&this7BdatFile))  %then 
                %let logMessage = SAS data file created successfully.;
            %else %do;
                %let logMessage = SAS data file not created.;
                %let logFlag = 1;
            %end;

            libname hcupSave clear;

            proc datasets lib=work nodetails nolist nowarn;
                delete &this7BdatFile ;
            run; quit;
        
        %end; **end to inc do loop;

        proc sql;
            insert into work.__logOut
                set state = "&st",
                    year = &year,
                    file = "&thisSasFile..sas",
                    message = "&logMessage",
                    errorFlag = &logFlag;
        quit;   
        %let logMessage=;
        %let logFlag = 0; 


    %end; **end to do loop;

    %if not(&debug) %then %do ;
        proc datasets lib=work nodetails nolist nowarn;
            delete __ascFiles;
        run; quit;
    %end;  

%end; **end to step 6;

%if &deleteAhalExeFile %then %do;

    %put ************************************************************;
    %put ************************************************************;
    %put *                7. DELETING AHAL EXE FILE                 *;
    %put ************************************************************;
    %put ************************************************************;
    %if &getAhalFiles and %sysfunc(fileexist("&fileDir/&st._SIDC_&year._AHAL.exe")) %then %do;
        %let rc=%sysfunc(filename(temp, &fileDir/&st._SIDC_&year._AHAL.exe));
        %let rc=%sysfunc(fdelete(&temp));
    %end;       

%end; **end to step 7;

%put ************************************************************;
%put ************************************************************;
%put *                7.CLEAN UP                                *;
%put ************************************************************;
%put ************************************************************;
data __logOut;
    set __logOut (firstObs=2);
run;

options xwait 
    %if &debug %then %do ; nosymbolgen nomlogic nomprint nomlogicnest %end; 
    ;

%put ************************************************************;
%put NOTE:  Macro &sysmacroname has finished. ;
%put ************************************************************;

%mend processSidFiles;
