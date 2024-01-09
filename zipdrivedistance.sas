/**
 @file
 @brief Gets driving distance between zip codes
 
 @details 
 @par Purpose
 This macro accesses Google Maps and returns the driving distance between 
 two 5-digit zip codes 
   
 @author Rocio Lopez

 @date 10-15-2018
 
 @version SAS 9.4
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %zipDriveDistance(
     data= 
    ,zipcode1= 
    ,zipcode2= 
    ,outdata = 
    ,straightline=1
    ,debug=0
	);
 @endcode

 @returns
 A sas data set with columns added for driving distance, driving distance units, 
 driving time, and, if requested, straight line distance
 
 @note 
 @parblock
 @li This is based on 
 <a href="http://support.sas.com/resources/papers/proceedings10/050-2010.pdf" 
 target="_blank"> SGF Paper 050-2010</a> and 
 <a href="https://communities.sas.com/t5/SAS-Communities-Library/Driving-Distances-and-Drive-Times-using-SAS-and-Google-Maps/ta-p/475839"
 target="_blank"> this SAS communities post</a>

 @li Zip codes should be 5 digits but can be numeric or character
 
 @li Finding the desired values of distance within the web page produced by Google Maps 
 is based on the current structure of those web pages; these structure is subject to 
 change at any time and this macro will probably need to be updated to accommodate 
 any changes
 
 @li Takes 1-3 seconds per query (observation)
 
 @li The user should revise observations without reported distances as sometimes 
 they are returned as blank but a manual check of Google will give a distance
 @endparblock 
 
 @param data Name of SAS dataset containing zip codes
 
 @param zipCode1 Name of column with zip code of origin
 
 @param zipCode2 Name of column with destination zip code
 
 @param straightLine Include the straight line distance (ZIPCITYDISTANCE function)?
 @n Allowed options are:
 @n 0 for no 
 @n 1 for yes (DEFAULT)

 @param outData Name of new SAS dataset that will have original data + driving distance
 @n If not provided, default will be <data>_drive_distance

 @param debug Debugging on/off
 @n Allowed options are:
 @n 0 for off (DEFAULT)
 @n 1 for on

 @par Example(s)
 @code{.sas}
 data zip_info;
	input z1 @@;
	if z1=00966 then z2=00969;
	else z2=12203;
	format z1 z2 z5.;
	datalines;
	02138 13502 20037 94117 12144 12203 00966
	;
 run;

 %zipDriveDistance(
	 data=zip_info
	,zipcode1=z1
	,zipcode2=z2
	);

 proc print data=zip_info_drive_distance; run;
 @endcode
 
 @par Revision History
 @b 12-01-2023 Updated URL call to exclude comma between city and state
 @n @b 03-07-2023 Skipping zip codes not found in sashelp.zipcode (need zip+city for search)
 @n @b 03-06-2023 Optimized the way the data is read in and processed
 @n @b 03-02-2023 Output dataset now includes drive time in seconds (with time6. format)
 @n @b 05-15-2019 Updated to also search for distances given in kilometers, 
 these are transformed into miles
**/

%macro zipDriveDistance(
    data= ,
    zipcode1= ,
    zipcode2= ,
    outdata = ,
    straightline=1,
    debug=0);

%*****************************************************************************;
%* ERROR CHECKING                                                             ;
%*****************************************************************************;
%let errorflag=0;
options minoperator;

%* Check debug option and turn on macro checking options;
%if not(%upcase(&debug)  in (0 1)) %then %do;
    %put WARNING: &debug must be either 0 or 1.;
    %put WARNING: DEBUG = **&debug**;
    %put WARNING: Default value of 0 will be used for DEGUG;
    %let degug=0;
%end; %*end to debug check;

%if &debug eq 1 %then %do;
    options mlogic mlogicnest varinitchk=warn mergenoby=warn;
%end; %* end to debug eq 1;

%* Check DATA is given and exists;
%if %length(&data) = 0 %then %do;
    %PUT ERROR: Required parameter DATA not provided. Macro will stop executing;
    %LET ERRORFLAG=1;
%end; %*end to if length(data) =  0;

%if %length(&data) > 0 and %SYSFUNC(exist(&data)) ne 1 %then %do;
    %PUT ERROR: Data set %upcase(&data) does not exist. Macro will stop executing;
    %LET ERRORFLAG=1;
%end; %*end to if length(data) ne  0;

%* Check that both zipcodes are proived and exist on the database;
%if %length(&zipcode1) = 0 %then %do;
    %put ERROR: Required parameter ZIPCODE1 is not provided. Macro will stop executing.;
    %let ERRORFLAG=1;
%end; **end to if length(zipcode1) = 0 ;

%if %length(&zipcode2) = 0 %then %do;
    %put ERROR: Required parameter ZIPCODE2 is not provided. Macro will stop executing.;
    %let ERRORFLAG=1;
%end; **end to if length(zipcode2) = 0 ;

%if %length(&data) > 0 and %SYSFUNC(exist(&data))=1 and %length(&zipcode1) > 0 %then %do;

    %* Check if VAR exists;
    %let dsid=%sysfunc(open(&data));
    %let exist1=%sysfunc(varnum(&DSID, &zipcode1));
    %let vartype1=%sysfunc(vartype(&DSID, &exist1));
    %let exist2=%sysfunc(varnum(&DSID, &zipcode2));
    %let vartype2=%sysfunc(vartype(&DSID, &exist2));
    %let rc=%sysfunc(close(&DSID));
        
    %* both must exist;
    %if &exist1 eq 0 %then %do;
        %put ERROR: Variable %upcase(&zipcode1) does not exist in dataset %upcase(&data). Macro will stop executing.;
        %let ERRORFLAG=1;
    %end; **end to if Exist1=0 then do;
    
    %if &exist2 eq 0 %then %do;
        %put ERROR: Variable %upcase(&zipcode2) does not exist in dataset %upcase(&data). Macro will stop executing.;
        %let ERRORFLAG=1;
    %end; **end to if Exist1=0 then do;
    
%end; ** end to check zipcodes exit;

%* If outdata not provided create default;
%if %length(&outdata) eq 0 %then %let outdata=&data._drive_distance;

%* Check straightline;
%if not(%upcase(&straightline)  in (0 1 YES NO Y N)) %then %do;
    %put WARNING: STRAIGHTLINE must be either 0, 1, no or yes.;
    %put          STRAIGHTLINE = **&straightline**;
    %put          Defuault value of STRAIGHTLINE = 1 (yes) will be used instead.;
%end; **end to if straightline not in ();

%*****************************************************************************;
%* IF ANY ERRORS FOUND THEN ABORT EXECUTION                                   ;
%*****************************************************************************;
%if &errorflag eq 1 %then %do;
    data _null_;
        abort 3;
    run;
%end;

%*****************************************************************************;
%* DELETE ANY DATA SET NAMED _DISTANCE_ THAT MIGHT EXIST IN THE WORK LIBRARY  ;
%*****************************************************************************;
proc datasets lib=work nolist;
    delete _distance_;
quit;

%*****************************************************************************;
%* OTHERWISE PROCEED TO CALCULATION                                           ;
%*****************************************************************************;
%* Get unique combinations of zips;
proc freq data=&data noprint;
    where 
     %if &vartype1 eq N %then %do; put(&zipcode1, z5.) %end;
      %else %if &vartype1 eq C %then %do; put(&zipcode1, $5.) %end;
      ne
      %if &vartype2 eq N %then %do; put(&zipcode2, z5.) %end;
      %else %if &vartype2 eq C %then %do; put(&zipcode2, $5.) %end; ;
    tables &zipcode1*&zipcode2/list out=_zips_;
run;    

%* Place number of zip in a macro variable;
data _null_;
    call symputx('nzips',obs);
    stop;
    set _zips_ nobs=obs;
run;
 
%* Create a loop to access Google Maps multiple time;
%let zipcity1 = ;
%let zipcity2 = ;
%do j=1 %to &nzips;
    data _null_;
        nrec = &j;
        set _zips_ point=nrec;
        
        %* Make sure they are both the same type;
        %if &vartype1 eq N %then %do;
            __zip1=put(&zipcode1,z5.);
        %end;
        %else  %if &vartype1 eq C %then %do;
            __zip1=put(&zipcode1, $5.);
        %end;
        
        %if &vartype2 eq N %then %do;
            __zip2=put(&zipcode2,z5.);
        %end;
        %else  %if &vartype2 eq C %then %do;
           __zip2=put(&zipcode2, $5.);
        %end;
        
        %* Store zip code;
        call symputx('z1', __zip1);
        call symputx('z2', __zip2);
                
        %* Some searches do not work unless you include city and date;
        %* Example syntax for query: North+Olmsted,+OH+44070;
        if not missing(zipcity(__zip1)) then 
            call symputx('zipcity1', tranwrd(catx(' ', compress(zipcity(__zip1), ","), __zip1), " ", "+"));
        if not missing(zipcity(__zip2)) then 
        call symputx('zipcity2', tranwrd(catx(' ', compress(zipcity(__zip2), ","), __zip2), " ", "+"));
        
        stop;
    run;

    %* If city not found, skip to next pair;
    %if %length(&zipcity1)=0 or %length(&zipcity2)=0 %then
    %put NOTE: At least one zip code not found in SASHELP.ZIPCODE, skipping to next pair.;

    %if %length(&zipcity1) and %length(&zipcity2) %then %do; 
 
        filename src temp;

        %* Before getting data, make sure the website exists;
        filename headout TEMP;
        proc http url="https://www.google.com/maps/dir/&zipcity1/&zipcity2/?force=lite"
            headerout=headout;
        run;
        
        data _null_;
            infile headout scanover truncover;
            input @'HTTP/1.1' code 4. message $255.;
            if _n_ = 1 then call symput("message", strip(message));
        run;           

        %* If website exists then get data, otherwise put a message and continue;
        %if "&message" eq "OK" %then %do;

            proc http
                url="https://www.google.com/maps/dir/&zipcity1/&zipcity2/?force=lite"
                out=src;
            run;

            data _temp_ (keep = zip1 zip2 text--drive_duration);
                infile src length = len lrecl = 32767;
                input line $varying32767. len;
                line = strip(line);
                if index(line, 'mile') then indx = index(line, 'mile');
                else if index(line, ' km') then indx = index(line, ' mi');     
                retain zip1 &z1  zip2 &z2;
                if indx then do;
                    lineLen = length(line);  
                    startPos = find(reverse(substr(line, 1, indx - 1)), '"');
                    endPos = 0;
                    do i=1 to 3 until(endPos = 0); 
                       endPos = find(substr(line, indx, lineLen - indx), '"', endPos + 1);
                    end;
                    text = substr(line, indx - startPos, startPos + endPos);
                    drive_distance = input(compress(scan(text,1,'\'), '",', 'ai'), ?? best.);
                    drive_distance_units    = compress(scan(text,1,'\'), , "kai");
                    drive_duration     = compress(scan(text,3,'"'), "\");
                    format drive_distance comma32.2;                    
                end;
                if not missing(drive_distance) then output;
            run;

        %end;

        %else %put NOTE: Website for &z1 and &z2 could not be accessed. Reason: &message.. ;

        filename src clear;
        filename headout clear;
             
        %* add an observation to the data set _DISTANCE_;
        proc append base=_distance_ data=_temp_;
        run;            

    %end; %* end to if zipcity1 and zipcity2;

    %* delete temp;
    proc datasets lib=work nolist;
        delete _temp_;
    quit; 

	%let z1 = ;
	%let z2 = ;
    %let zipcity1 = ;
    %let zipcity2 = ; 
    
%end; %* end to j to nzips;

%*****************************************************************************;
%* ADD DISTANCE TO ORIGINAL DATA (ALLOW CHARACTER TO NUMERIC MERGE)           ;
%*****************************************************************************;
proc sort data=_distance_ nodupkey;
	by zip1 zip2;
run;

proc sql;
    create table &outdata 
    as select a.*, b.drive_distance, b.drive_distance_units, b.drive_duration
    %if %upcase(&straightline) in (1 YES Y) %then %do;
    , zipcitydistance(a.&zipcode1, a.&zipcode2) as straight_line_distance_mi
    %end;
    from &data a left join _distance_ b
    on %if &vartype1 eq N %then %do; put(a.&zipcode1, z5.) %end;
       %else %if &vartype1 eq C %then %do; put(a.&zipcode1, $5.) %end;
       =put(b.zip1, z5.) and 
       %if &vartype2 eq N %then %do; put(a.&zipcode2, z5.) %end;
       %else %if &vartype2 eq C %then %do; put(a.&zipcode2, $5.) %end; 
       =put(b.zip2, z5.);
quit;

%* Additional tidy up;
data &outdata (drop = drive_distance drive_distance_units drive_duration);
    set &outdata;
    
    **If same origin and destination put 0;
    if 
      %if &vartype1 eq N %then %do; put(&zipcode1, z5.) %end;
      %else %if &vartype1 eq C %then %do; put(&zipcode1, $5.) %end;
      eq
      %if &vartype2 eq N %then %do; put(&zipcode2, z5.) %end;
      %else %if &vartype2 eq C %then %do; put(&zipcode2, $5.) %end; 
     then do;
        drive_distance=0;
        drive_distance_units='miles';
        drive_duration='0 min';
    end;
    
    **convert km to miles;
    if compress(drive_distance_units) in ("mile" "miles") then
    	drive_miles = drive_distance;
    else if compress(drive_distance_units) = "km" then
    	drive_miles = round(divide(drive_distance, 1.609), 0.1);
    	
	**drive_time as numeric;
	if find(upcase(drive_duration), 'DAY') then do;	
		if find(upcase(drive_duration), 'HR') and 
		   find(upcase(drive_duration), 'MIN') then
			drive_time = (input(scan(drive_duration, 1), 8.) * 86400) + 
						 (input(scan(drive_duration, 3), 8.) * 3600) + 
						 (input(scan(drive_duration, 5), 8.) * 60);
		else if find(upcase(drive_duration), 'HR') and 
				not find(upcase(drive_duration), 'MIN') then
			drive_time = (input(scan(drive_duration, 1), 8.) * 86400) + 
						 (input(scan(drive_duration, 3), 8.) * 3600);		
		else if not find(upcase(drive_duration), 'HR') and 
				    find(upcase(drive_duration), 'MIN') then
			drive_time = (input(scan(drive_duration, 1), 8.) * 86400) + 
					     (input(scan(drive_duration, 3), 8.) * 60);		
		else drive_time = input(scan(drive_duration, 1), 8.) * 86400;	
	end;
	
	else if find(upcase(drive_duration), 'HR') then do;
		if find(upcase(drive_duration), 'MIN') then
			drive_time = (input(scan(drive_duration, 1), 8.) * 3600) + 
					 	 (input(scan(drive_duration, 3), 8.) * 60);
		else drive_time = input(scan(drive_duration, 1), 8.) * 3600;					 	 
	end;
	
	else if find(upcase(drive_duration), 'MIN') then do;
		drive_time = input(scan(drive_duration, 1), 8.) * 60;
	end;
	
	format drive_miles comma32.2 drive_time time6.;
	label drive_miles = "Driving distance (miles)"
		  drive_time = "Driving time (seconds, formated as hh:mm)"
		  straight_line_distance_mi = "Geodetic distance (miles)";
run;

%*****************************************************************************;
%* DELETE DBS                                                                 ;
%*****************************************************************************;
%if &debug ne 1 %then %do;
    proc datasets lib=work nolist;
        delete _zips_ _distance_;
    quit;        
%end;

%*****************************************************************************;
%* MEND MACRO                                                                 ;
%*****************************************************************************;
ods listing;
options nomlogic nomlogicnest; 

%mend zipDriveDistance;
