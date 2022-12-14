/**
 @file
 @brief downloads *.XPT files from NHANES website
 
 @details 
 @par Purpose
 This macro downloads and saves specified .XPT files from NHANES website

 @author Rocio Lopez
 
 @date 01-22-2019
 
 @version SAS 9.4 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %getNhanesData(
      cycle = 
      ,files = 
      ,saveIn = 
      );
 @endcode

 @returns
 One or more .XPT files saved in the specified directory

 @note 
 @parblock
 @li Go to https://wwwn.cdc.gov/nchs/nhanes/Default.aspx to view available
 cycles and data sets

 @li We recommend saving the XPT you use as online files are sometimes updated 
 and this will ensure you can reproduce results, if needed
 @endparblock

 @param cycle  NHANES cycle you want to download (format XXXX-XXXX)
 @param files  Name of file(s) you want to download
 @param saveIn Location in which to save the .XPT file(s)
 
 @sa imptallxpt.sas, nhanesfmtscrape.sas

 @par Example(s)
 @code{.sas}
 %getNhanesData(
       cycle  = 2015-2016
      ,files  = DEMO_I BMX_I
      ,saveIn = ./nhanes2015to16
      );
 @endcode
 
 @par Revision History
 @b 09-04-2021 If file does not exist, will now print out warning message and 
 continue to next file
**/

%macro getNhanesData(cycle = , files = , savein = );
    
/****************************************************************************************************
 * READ IN AND SAVE XPT DATA FILEs
 ****************************************************************************************************/
%* Count number of files;
%let NumFiles=0;
%do %while(%scan(&files,&NumFiles+1) ^=    ); %let NumFiles=%eval(&NumFiles+1); %end;
 
%* Loop through each file and get formats;
%do i=1 %to &NumFiles;
    
    %* Get current file;
    %let file=%upcase(%scan(&files, &i));

    %* Before getting data, make sure the website exists;
    filename headout TEMP;
    proc http url="https://wwwn.cdc.gov/Nchs/Nhanes/&cycle/&file..XPT" 
        headerout=headout;
    run;
    
    data _null_;
        infile headout scanover truncover;
        input @'HTTP/1.1' code 4. message $255.;
        if _n_ = 1 then call symput("message&i", strip(message));
    run;           

    %* If file exists then get, otherwise put a message and continue;
    %if "&&message&i" eq "OK" %then %do;

        %* detect proper delim for UNIX vs. Windows;
        %let delim=%sysfunc(ifc(%eval(&sysscp. = WIN),\,/));
         
        %* create a name for our downloaded ZIP;
        %let fileloc = &savein.&delim.&file..XPT;
        filename download "&fileloc";
        
        %* Download the file from the Internet;
        proc http
         method='GET'
         url="https://wwwn.cdc.gov/Nchs/Nhanes/&cycle/&file..XPT"
         out=download;
        run;
    
    %end;
    
    %else %put WARNING: &file could not be downloaded. Reason: &&message&i... ;
    
%end;

%mend getNhanesData;
