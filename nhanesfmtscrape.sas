/**
 @file
 @brief Creates  a format catalog from NHANES documentation
 
 @details 
 @par Purpose
 This macro will read NHANES HTML documentation pages and create a format 
 catalog and/or code file for the data

 @author Rocio Lopez
 
 @date 09-18-2018
 
 @version SAS 9.4 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %nhanesFmtScrape(
       cycle = 
      ,files = 
      ,omitmissing = 1
      ,fmtlibrary = work
      ,fmtcatalog = nhanesFormats
      ,fmtcodefile = 
      ,prints = 1
      );
 @endcode

 @returns
 SAS format catalog and/or .SAS file with PROC FORMAT code

 @note 
 @parblock
 @li Each format will be named VarNameF.
 
 @li It is possible for format labels to have % or & in which case a 
 Warning message will be seen in the log (e.g.  WARNING: Apparent 
 symbolic reference LT not resolved.); this can be ignored.

 @endparblock

 @param cycle NAHES cycle for html file(s), format should be YYYY-YYYY
 
 @param files List of 1 or more NHANES data files for which to extract formats

 @param omitMissing If yes, will omit formats for missing values
 @n Allowed options are: 
 @n 1, yes, y for omitting missing values (DEFAULT)
 @n 0, no, n for including missing values

 @param fmtLibrary SAS library in which to store the format catalog
 @n Default is WORK

 @param fmtCatalog Name of generated SAS format catalog
 @ Default is nhanesFormats

 @param fmtCodeFile Optional name of .SAS file in which to save PROC FORMAT code

 @param prints If yes, prints a summary of all formats in catalog
 @n Allowed are are: 
 @n Allowed options are: 
 @n 1, yes, y to print catalog (DEFAULT)
 @n 0, no, n to not print catalog

 @sa getNhanesData.sas, imptallxpt.sas

 @par Example(s)
 @code{.sas}
 libname fmt "./nhanes2015to16";
 %NHANESFmtScrape(
       cycle = 2015-2016
      ,files = DEMO_I BMX_I
      ,fmtLibrary = fmt
      ,fmtCodeFile = "./nhanes2015to16/formats.sas"
      );
 @endcode
 
 @par Revision History
 @b 03-25-2021 Changes to allow for $format
 @b 02-13-2020 Added option to omit formats for missing values
**/

%macro nhanesFmtScrape(
    cycle = ,
    files = ,
    omitmissing = 1,
    fmtlibrary = work,
    fmtcatalog = nhanesFormats,
    fmtcodefile = ,
    prints = 1);

%*****************************************************************************;
%* ERROR CHECKING                                                             ;
%*****************************************************************************;
%let errorflag=0;
options minoperator; 

%* Required parameters provided;
%if %length(&cycle) = 0 %then %do;
    %put ERROR: Required parameter CYCLE is not provided. Macro will stop executing.;
    %let errorflag=1;
%end; **end to if length(cycle) = 0 ;

%if %length(&files) = 0 %then %do;
    %put ERROR: Required parameter FILES is not provided. Macro will stop executing.;
    %let errorflag=1;
%end; **end to if length(cycle) = 0 ;

%* Check if requested library is assigned;
%if (%sysfunc(libref(&fmtlibrary))) ne 0 %then %do;
    %put ERROR: Library %upcase(&fmtlibrary) has not been assigned. Macro will stop executing.;
    %let errorflag=1;
%end;  

%* PrintS is 0 1 y n yes no;
%if not(%upcase(&prints)  in (0 1 YES NO Y N)) %then %do;
    %put WARNING: PRINTS must be either 0, 1, no, yes, n or y.;
    %put          PRINTS = **&prints**;
    %put          Default value of YES will be used instead.;
    %let prints=1;
%end; **end to if prints not in ();

%* OmitMissing is 0 1 y n yes no;
%if not(%upcase(&omitmissing)  in (0 1 YES NO Y N)) %then %do;
    %put WARNING: OMITMISSING must be either 0, 1, no, yes, n or y.;
    %put          OMITMISSING = **&prints**;
    %put          Default value of YES will be used instead.;
    %let omitmissing=1;
%end; **end to if omitmissing not in ();

%*****************************************************************************;
%* IF ANY ERRORS FOUND THEN ABORT EXECUTION                                                   ;
%*****************************************************************************;
%if &errorflag eq 1 %then %do;
    data _null_;
        abort 3;
    run;
%end;

%*****************************************************************************;
%* IF NO ERRORS, START MACRO                                                  ;
%*****************************************************************************;

%* Count number of files;
%let NumFiles=0;
%do %while(%scan(&files,&NumFiles+1) ^=    ); %let NumFiles=%eval(&NumFiles+1); %end;
 
%* Loop through each file and get formats;
%do i=1 %to &NumFiles;
    
    %* Get current file;
    %let file=%scan(&files, &i);

    %* Fetch webpage ;
    filename src temp;
    proc http
     method="GET"
     url="https://wwwn.cdc.gov/Nchs/Nhanes/&cycle/&file..htm"
     out=src;
    run;
    
    %* Read the entire file and skip the blank lines ;
    %* the LEN indicator tells us the length of each line ;
    data doc;
        infile src length=len lrecl=99999;
        input line $varying32767. len;
        line = strip(line);
        if len>0;
    run;

    %* Parse the lines and keep codebook info ;
    data codebook (keep=Name Value Code);
        length Name Value Code $ 32767;
        set doc;
        
        %* Get Variable Names (3 rows after vartitle is seen);
        if find(line, 'class="vartitle"') then do;
            
            pickup=_n_+3;
            
            set doc point=pickup;
              Name=
                strip(tranwrd(tranwrd(line, '<dd class="info">', ""), "</dd>", ""));
            
            output;
        end; 
        
        %* Get Values and Label (1 row after value) ;
        if find(line, 'scope="row" class="values"') then do;
            
            pickup1=_n_;
            pickup2=_n_+1;
            
            set doc point=pickup1;
              Value=
                strip(tranwrd(tranwrd(line, '<td scope="row" class="values">', ""), "</td>", ""));
            
            set doc point=pickup2;
              Code=
                strip(tranwrd(tranwrd(line, '<td class="values">', ""), "</td>", ""));
              %* Change apostrophe to do not;
              Code=tranwrd(Code, "Don't", "Do not");
            output;
        end; 
            
    run;
    
    %* If CODEBOOK has no observations put warning to check site;
    %let dsid=%sysfunc(open(codebook));
    %let nobs =%sysfunc(attrn(&dsid,nobs));
    %let rc=%sysfunc(close(&DSID));
    
    %if &nobs eq 0 %then %put WARNING: No data read from https://wwwn.cdc.gov/Nchs/Nhanes/&cycle/&file..htm. Please check existance of site.;
        
    
    %* Data clean up ;
    data file&i (keep=FmtName start end label type);
        length FmtName start end Label $ 32767 type $ 1;
        set codebook;
        
        %* Carry VarName to all rows ;
        retain VarName;
        if Name ne "" then VarName=Name;
        
        %*Missing blanks;
        if Value = "%str(&)lt; blank %str(&)gt;" then Value = " ";
        
        %* Do not want SEQN in code book, also delete if value and code are blank;
        %* If requested, omit formats for missing values;
        if VarName="SEQN" or (Value="" and Code="") 
          %if %upcase(&omitmissing) in (Y YES 1) %then %do; 
            or (Value in ("." " ") and Code = "Missing")
          %end;
          then delete;    
        
        %* Format Name ;
        FmtName=strip(VarName)||"F";
        
        %* Create Start and End Values ;
        if Code="Range of Values" then do;
            start=strip(scan(Value,1, " "));
            end=strip(scan(Value,3, " "));
            label="Valid value";
            type="N";
        end;    
        else do;    
            start=strip(Value);
            end=start;
            label=Code;
            if notdigit(compress(Value, ".-")) le length(compress(Value, ".-"))
              then Type = "C";
            else Type  = "N";
        end;      
    run;    

%end; **end to do i to numfiles;

%* Concatante all files;
data cntlin;
    set %do i=1 %to &NumFiles; file&i %end; ;
run; 
        
%* Read as formats ;
proc format cntlin=cntlin; run;

%* Create Catalog - this way I can control the name of the catalog;
proc catalog cat=work.Formats;
   copy out=&fmtlibrary..&fmtcatalog;
run;

%* Print formats if requested;
%if %upcase(&prints) in (1 Y YES) %then %do;
    proc format library=&fmtlibrary..&fmtcatalog fmtlib; run;
%end;

%* If requested, create a code based format file;
%if %length(&fmtcodefile)>0 %then %do;
    
    %* Add order so that I can print in same order;
    data cntlin;
        set cntlin;
        FmtOrder=_n_;     
    run;        
    
    %* List of variables;
    proc sql noprint;
        select FmtName, min(fmtorder) as fmtOrder
        into :flist separated by ' ' , :dummy
        from cntlin
        group by fmtname
        order by fmtorder; 
        %let NumFmts = &SQLObs.; 
    quit;  
    %put &numfmts;      
    
    %* Create macro vars with info needed ;
    %* (fmtname, levels, # of levels);
    %do i=1 %to &NumFmts;
        **fmt;
        %let fmt=%scan(&flist, &i);
    
        **fmt levels and labels;
        proc sql noprint;
            select left(trim(fmtname)), start, end, label
            into :&fmt.1-:&fmt.9999, :&fmt.s1-:&fmt.s9999, 
            :&fmt.e1-:&fmt.e9999, :&fmt.lab1-:&fmt.lab9999
            from cntlin
            where fmtname=upcase("&fmt");
            **fmt num levels;
            %let count_&fmt = &SqlObs.;
        quit;            
                        
    %end; **end to do i to numfmts;
    
    **Create File;
    data _null_;
        file &fmtcodefile;
        put "proc format; ";
        %do i=1 %to &NumFmts;
            %let fmt=%scan(&flist, &i);
            
            **formats;
            put "    value &fmt." ;
            %do j=1 %to &&count_&fmt.;
                %if "&&&fmt.s&j" eq "&&&fmt.e&j" %then %do;   
                    put "      &&&fmt.s&j = '&&&fmt.lab&j'";
                %end;
                %if "&&&fmt.s&j" ne "&&&fmt.e&j" %then %do;   
                    put "      &&&fmt.s&j - &&&fmt.e&j = '&&&fmt.lab&j'";
                %end;                    
              
            %end; 
                       
            put "      ;";
        %end;
        
        put "run;";
        put "quit;";
    run;


%end; **end to if fmtcodefile;

%* Clear work lib;
proc datasets lib=work memtype=data nolist;
    delete doc codebook cntlin
      %do i=1 %to &NumFiles; file&i %end;  ;
run; quit;

%*****************************************************************************;
%* CLOSE MACRO                                                                ;
%*****************************************************************************;
%mend nhanesFmtScrape;
