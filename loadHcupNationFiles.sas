/**
 @file
 @brief Download and run HCUP SAS load files for nationwide databases
 
 @details 
 @par Purpose
 This macro will download any available SAS load files from the HCUP 
 website and proceed to execute these. The macro can also be used to 
 run any manually downloaded SAS load programs. 

 @author Rocio Lopez
 
 @date 09-22-2023
 
 @version SAS 9.4 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %loadHcupNationFiles(
	 nwDatabase =  
	,year = 
	,baseDir =
	,overwrite = 1  
	,getSasFiles = 1
	,runSas = 1
	,debug = 0
 );

 @endcode

 @returns
 .SAS and .SAS7BDAT files for the specified database/year
 will be stored in the <BASEDIR>/<NWDATABASE>/<YEAR> folder.

 @param nwDatabase Specify the HCUP nationwide database to process
 @n Allowed options are: NASS, NEDS, NIS, NRD, or KID
 
 @param baseDir Specify the base directory where required materials are saved and 
 where the output will be stored
 @n Do not include the database name and year in this path
 
 @param year Year of data that will be processed (yyyy)

 @param overwrite Logical stating whether to overwrite existing files
 @n Allowed options are:
 @n 0 for no
 @n 1 for yes (DEFAULT)

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
 @li This macro has been tested with NRD data on a Windows environment only. 
 

 @li If requesting to run the SAS load programs, user must have purchased,
 extracted, and saved the HCUP files in <BASEDIR>/<NWDATABASE>/<YEAR>. Do not rename any file(s).

 @li .SAS and .SAS7BDAT files files will be saved in the <BASEDIR>/<NWDATABASE>/<YEAR> folder.

 @li Go to https://hcup-us.ahrq.gov/ to purchase data and/or download supporting materials

 @endparblock  
  
 @par Example(s)
 @code{.sas}
 %processHcupNationFiles(
     nwDatabase = NRD
    ,year = 2020
	,baseDir = ./hcup
 ); 
 @endcode
 
 @par Revision History
 n/a
 
**/

%macro loadHcupNationFiles(
	 nwDatabase = 		/* Nationwide database to process e.g. NIS */
	,year = 			/* year of data yyyy */ 
    ,baseDir =          /* location where files are e.g. data/hcup/data */
	,overwrite = 1      /* 1 to overwrite existing files */
	,getSasFiles = 1	/* 1 to extract and copy load programs, will overwrite */
	,runSas = 1			/* 1 to run SAS load programs and save data, will overwrite */
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
%if &debug %then %do ;
    options symbolgen mlogic mprint mlogicnest ; 
%end ;

/* Define directory where files will be stored will be */
%let fileDir = &baseDir/%lowcase(&nwDatabase)/&year;

/* Base URL for HCUP tools website (where SAS files are) */
%if %upcase(&nwDatabase) = NIS %then %do;
    %let hcupToolsUrl = https://hcup-us.ahrq.gov/db/nation/nis/tools/pgms;
%end;
%else %do; 
    %let hcupToolsUrl = https://hcup-us.ahrq.gov/db/nation/%lowcase(&nwDatabase)/pgms;
%end;

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

%if not(%length(&year)) %then %do;
    %put ERROR: Required parameter YEAR is not provided. Macro will stop executing.;
    %let errorFlag = 1;
%end;

%put *************************************************;
%put * CHECKING FILEDIR EXISTS			             *;
%put *************************************************;
%if not(%length(&fileDir)) and not(%sysfunc(fileexist(&fileDir))) %then %do;
    %put ERROR: Directory &fileDir does not exist. Macro will stop executing.;
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

%if &getSASFiles %then %do;

	%put ************************************************************;
	%put ************************************************************;
	%put *             1:GETTING SAS LOAD FILES                     *;
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
		%end;	**end to message not ok;						
				
	%end; **end to loop 1 to 4;

%end;  *end to step 1;

%if &runSAS %then %do;
	
	%put ************************************************************;
	%put ************************************************************;
	%put *         	 2: RUNNING SAS CODE AND SAVING SAS7BDAT	    *;
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
        on strip(tranwrd(scan(a.fname, 1, '.'), "SASLoad_", "")) = strip(scan(b.fname, 1, '.')) 
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
		        set __dataFiles (obs = &i firstobs = &i);
		        call symput('thisFile', strip(scan(fname, 1, '.')));
		    run;
			
			%if %sysfunc(fileexist("&fileDir/SASLoad_&thisFile..sas")) %then %do;								
				
			    %include "&fileDir/SASLoad_&thisFile..sas";	

				libname hcupSave "&fileDir";
				proc copy in=work out=hcupSave memtype=(data);
					select &thisFile ;
				run;		
				libname hcupSave clear;  

				%if not(&debug) %then %do ;
				proc datasets lib=work memtype=data nodetails nolist nowarn;
					delete &thisFile ;
				run; quit;	
				%end;
				
			%end;
            %else %do;
                %put WARNING: &thisFile SAS load file not found in directory;
            %end;
		%end; **end to do 1 to numDataFiles loop;

	%end; **end to if numSasFiles loop;

	%if not(&debug) %then %do ;
    	proc datasets lib=work nodetails nolist nowarn;
    		delete __sasFiles __dataFiles ;
    	run; quit;	
	%end;	
	
%end; **end to step 2;

%put ************************************************************;
%put ************************************************************;
%put *                3.CLEAN UP                                *;
%put ************************************************************;
%put ************************************************************;
%let rc = %sysfunc(dlgcdir("&curDir"));

%if &debug %then %do ;
    options nosymbolgen nomlogic nomprint nomlogicnest ; 
%end ;

%put ************************************************************;
%put NOTE:  Macro &sysmacroname has finished. ;
%put ************************************************************;

%mend loadHcupNationFiles;
