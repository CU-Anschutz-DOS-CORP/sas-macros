/**
 @file
 @brief Checks .LOG files
 
 @details 
 @par Purpose
 This program retrieves all the .LOG files in the specified directory and
 scans them for errors, warnings and notes of concern.
 
 @author Richann Watson, Experis
 
 @par Updates/Maintenance
 Rocio Lopez

 @date 2017
 
 @version SAS 9.4
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %logCheck(
     loc =
    ,loc2 =
    ,fnm =
    ,delm = @
    ,out =
    );
 @endcode

 @returns
 An RTF file with an entry for each .LOG file specifying whether the file is
 clean or listing any errors, warning or notes of concern
 
 @note 
 Please see the following SAS Global Forum and MSUG papers for details:
 @n This macro: <http://support.sas.com/resources/papers/proceedings17/1173-2017.pdf>
 @n Updated version from author: <https://www.mwsug.org/proceedings/2017/TT/MWSUG-2017-TT02.pdf>
 
 @param loc Directory where the log files can be found, do no 
 use quotes
 
 @param loc2 Directory where report is to be stored, do no 
 use quotes
 @n Default is same directory where log files are (LOC)

 @param fnm Allows to speify which types of files to look at
 @n e.g., Only .log files starting with t_ (tables); separate types of files 
 by delimiter indicated in the delm macro parameter (e.g., t_@f_)
 @n By default it will look at all .log files

 @param delm Delimiter used to separate types of files
 @n Default is @

 @param out  Optional name for output report
 @n If left blank, ALL_CHECKLOGS is used
  
 @par Example(s)
 @code{.sas}
 %logCheck(loc = ./logFiles);
 @endcode
 
 @par Revision History
 @b 05-17-2019 Added a check for ERROR xx-yyyy: syntax (RL)
 @n @b 03-22-2019 Added a check for whether a WHERE clause is stated >1 (RL)
 @n @b 12-04-2022 Added more terms to search for in the log files (RL)
**/

%macro logCheck(
			loc=,  /* location of where the log files are stored       */
   	 	    loc2=, /* location of where report is stored (optional)    */
   	 	    fnm=,  /* which types of files to look at (optional)       */
   	 	    	   /* e.g., Tables – t_, Figures – f_, Listings – l_   */
   	 	    	   /* separate types of files by delimiter indicated in*/
   	 	    	   /* the delm macro parameter (e.g., t_@f_)           */
   	 	  	delm=@,/* delimiter used to separate types of files (opt'l)*/
   	 	  	out=   /* log report name (optional)                       */); 
 	 	 	 	  
  /* need to determine the environment in which this is executed   */ 
  /* syntax for some commands vary from environment to environment */ 
  /* end macro call if environment not Windows or Linux/Unix       */ 
  %if &sysscp = WIN %then %do; 
    %let ppcmd = %str(dir); 
    %let slash = \; 
  %end; 
  %else %if &sysscp = LIN X64 %then %do; 
    %let ppcmd = %str(ls -l); 
    %let slash = /; 
  %end; 
  %else %do; 
    %put ENVIRONMENT NOT SPECIFIED; 
    abort abend; 
  %end; 
  /* if a filename is specified then build the where clause */ 
  %if "&fnm" ne ""  %then %do; 
    data _null_;       
      length fullwhr $2000.;       
      retain fullwhr; 
 
      /* read in each log file and check for undesired messages */ 
      %let f = 1; 
      %let typ = %scan(&fnm, &f, "&delm"); 
  
      /* loop through each type of filename to build the where clause */ 
      /* embed &typ in double quotes in case filename has any special */ 
      /* characters or spaces                                         */ 
      %do %while ("&typ" ne ""); 
 
        partwhr = catt("index(flog, '", "&typ", "')");  	 
        fullwhr = catx(" or ", fullwhr, partwhr); 
 
 	 call symputx('fullwhr', fullwhr); 
 
        %let f = %eval(&f + 1); 
        %let typ = %scan(&fnm, &f, "&delm"); 
      %end;      
    run;
  %end; 
 
  /* need to build pipe directory statement as a macro var  */ 
  /* because the statement requires a series of single and  */ 
  /* double quotes - by building the directory statement    */ 
  /* this allows the user to determine the directory rather */ 
  /* than it being hardcoded into the program               */   
  /* macro var will be of the form:'dir "directory path" '  */ 
  data _null_;     
    libnm = "&loc"; 
    dirnm = catx(" ", "'", "&ppcmd", quote(libnm), "'"); 
    call symputx('dirnm', dirnm); 
  run; 
 
  /* read in the contents of the directory containing the logs */   
  filename pdir pipe &dirnm lrecl=32727; 
 
  data logs (keep = flog fdat ftim filename numtok); 
    infile pdir truncover scanover;     
    input filename $char1000.; 
 
    length flog $100 fdat ftim $10; 
 
    /* keep only the logs */     
    if index(filename, ".log"); 
 
    /* count the number of tokens (i.e., different parts of filename) */ 
    /* if there are no spaces then there should be 5 tokens for WIN   */
    /* or 9 tokens for LIN X64                                        */ 
    numtok = countw(filename,' ','q'); 
 
    /* need to build the flog value based on number of tokens */ 
    /* if there are spaces in the log name then need to grab  */ 
    /* each piece of the log name                             */ 
    /* the first token that is retrieved will have '.log' and */ 
    /* it needs to be removed by substituting a blank         */ 
    /* also need to parse out the date and time these are in  */ 
    /* specific spots within the filename so aren't based on  */ 
    /* number of tokens but will have different locations     */ 
    /* depending on environment - so parsing of each piece of */ 
    /* information will be environment dependent              */ 
    /* note on the scan function a negative # scans from right*/ 
    /* and a positive # scans from the left                   */ 
 
    /*********** WINDOWS ENVIRONMENT ************/ 
    /* the pipe will read in the information in */ 
    /* the format of: date time am/pm size file */ 
    /* e.g. 08/24/2015 09:08 PM 18,498 ae.log   */ 
    /*    '08/24/2015' is first token from left */ 
    /*    'ae.log' is first token from right    */ 
    %if &sysscp = WIN %then %do;       
      do j = 5 to numtok; 
 	   tlog = tranwrd(scan(filename, 4 - j, " "),  ".log", "");
 	   flog = catx(" ", tlog, flog); 
      end; 
      ftim = catx(" ", scan(filename, 2, " "), scan(filename, 3, " "));
      fdat = put(input(scan(filename, 1, " "), mmddyy10.), date9.); 
    %end; 
 	 
    /***************************** UNIX ENVIRONMENT ******************************/ 
    /* the pipe will read in the information in the format of: permissions, user,*/ 
    /* system environment, file size, month, day, year or time, filename         */ 
    /* e.g. -rw-rw-r-- 1 userid sysenviron 42,341 Oct 22 2015 ad_adaapasi.log    */ 
    /*    '-rw-rw-r--' is first token from left                                  */ 
    /*    'ad_adaapasi.log' is first token from right                            */ 
    %else %if &sysscp = LIN X64 %then %do;
      do j = 9 to numtok; 
 	    tlog = tranwrd(scan(filename, 8 - j, " "),  ".log", ""); 
 	    flog = catx(" ", tlog, flog);
 	  end; 
      _ftim = scan(filename, 8, " "); 
 
      /* in Unix if year is current year then time stamp is displayed */ 
      /* otherwise the year last modified is displayed                */ 
      /* so if no year is provided then default to today's year and if*/       
      /* no time is provided indicated 'N/A'                          */       
      if anypunct(_ftim) then do; 
        ftim = put(input(_ftim, time5.), timeampm8.); 
        yr = put(year(today()), Z4.); 
      end;       
      else do;         
        ftim = 'N/A';
        yr = _ftim;
      end;  
      fdat = cats(scan(filename, 7, " "), upcase(scan(filename, 6, " ")), yr); 
    %end;   
  run; 
 
  /* create a list of logs, dates, times and store in macro variables */ 
  proc sql noprint;     
  	select flog, fdat, ftim 
           into : currlogs separated by "&delm", 
 	 	        : currdats separated by " ", 
 	 	 	 	: currtims separated by "@"     
 	 	   from logs 
    %if "&fnm" ne "" %then where &fullwhr;     ; /* need to keep extra semicolon */ 
  quit; 
  /* need to make sure the alllogs data set does not exist before getting into loop */ 
  proc datasets nolist;      
  	delete alllogs; 
  quit; 
 
  /* read in each log file and check for undesired messages */ 
  %let x = 1; 
  %let lg = %scan(&currlogs, &x, "&delm"); 
  %let dt = %scan(&currdats, &x); 
  %let tm = %scan(&currtims, &x, '@'); 
 
  /* loop through each log in the directory and look for undesirable messages */ 
  /* embed &lg in double quotes in case filename has special characters/spaces*/ 
  %do %while ("&lg" ne ""); 
    /* read the log file into a SAS data set to parse the text */ 
    data logck&x; 
      infile "&loc.&slash.&lg..log" missover pad; 
      input line $1000.; 
 	  
 	  /*Added by Rocio: Add line number 
 	    (this will have the last numbered line prior to message*/
 	  length LineNum $25.;
 	  rec=compress(scan(line,1),,"dk");
	  retain LineNum;
	  if rec ne "" then LineNum=rec;
	  drop rec;
 
      /* keep only the records that had an undesirable message */ 
      if index(upcase(line), "WARNING") or
         index(upcase(line), "ERROR:") or
         index(upcase(line), "UNINITIALIZED") or
         index(upcase(line), "NOTE: MERGE") or 
         index(upcase(line), "MORE THAN ONE DATA SET WITH REPEATS OF BY") or
         index(upcase(line), "VALUES HAVE BEEN CONVERTED") or
         index(upcase(line), "MISSING VALUES WERE GENERATED AS A RESULT") or
         index(upcase(line), "INVALID DATA") or
         index(upcase(line), "INVALID NUMERIC DATA") or
         index(upcase(line), "AT LEAST ONE W.D FORMAT TOO SMALL") or
         index(upcase(line), "ORDERING BY AN ITEM THAT DOESN'T APPEAR IN") or
         index(upcase(line), "OUTSIDE THE AXIS RANGE") or
         index(upcase(line), "RETURNING PREMATURELY") or
         index(upcase(line), "UNKNOWN MONTH FOR") or
         index(upcase(line), "QUERY DATA") or
         index(upcase(line), "??") or 
         index(upcase(line), "QUESTIONABLE") or
         /* added by Rocio*/
		 index(upcase(line), "ERROR") eq 1 or
         index(upcase(line), "NOT FOUND") or
         index(upcase(line), "COULD NOT BE LOADED") or
         index(upcase(line), "HAS 0 OBSERVATIONS") or
         index(upcase(line), "STOPPED PROCESSING THIS STEP") or
         index(upcase(line), "DIVISION BY ZERO") or
         index(upcase(line), "MATHEMATICAL OPERATIONS COULD NOT") or
         index(upcase(line), "NO STATISTICS ARE COMPUTED") or
         index(upcase(line), "OVERWRITTEN") or
		 index(upcase(line), "WHERE CLAUSE HAS BEEN REPLACED")
         ; 
 
      /* create variables that will contain the log that is being scanned */       
      /* as well as the and date and time that the log file was created   */ 
      length lognm $100. logdt logtm $10.; 
      lognm = upcase("&lg");
      logdt = "&dt";
      logtm = "&tm"; 
 
      /* create a dummy variable to create a column on the report that will allow */       
      /* users to enter a reason if the message is allowed                        */ 
      logrs = ' ';     
    run; 
 
    /* because there are sometimes issues with SAS certificate */ 
    /* there will be warnings in the logs that are expected    */     
    /* these need to be removed                                */ 
    data logck&x._2;       
      set logck&x.; 
      if index(upcase(line), 'UNABLE TO COPY SASUSER') or
         index(upcase(line), 'BASE PRODUCT PRODUCT') or
         index(upcase(line), 'EXPIRE WITHIN') or 
         (index(upcase(line), 'BASE SAS SOFTWARE') and index(upcase(line), 'EXPIRING SOON')) or
         index(upcase(line), 'UPCOMING EXPIRATION') or
         index(upcase(line), 'SCHEDULED TO EXPIRE') or
         index(upcase(line), 'SETINIT TO OBTAIN MORE INFO') then delete; 
    run; 
 
    /* determine the number of undesired messages were in the log */ 
    data _null_; 
      if 0 then set logck&x._2 nobs=final;       
      call symputx('numobs',left(put(final, 8.))); 
    run; 
 
    /* if there is no undesired messages in log create a dummy record for report */ 
    %if &numobs = 0 %then %do;       
      data logck&x._2; 
        length lognm $100. LineNum $25. line $1000. logdt logtm $10.;
        linenum = " ";
        line = "No undesired messages.  Log is clean."; 
        lognm = upcase("&lg");         logdt = "&dt";
        logtm = "&tm"; 
 		
        /* create a dummy variable to create a column on the report that will allow */         
        /* users to enter a reason if the message is allowed                        */ 
        logrs = ' ';
        output;
      run;
    %end; 
 
    /* append all the results into one data set */ 
    %if &x = 1 %then %do;       
      data alllogs;         
        set logck&x._2; 
      run;     
    %end;     
    %else %do; 
      proc append base=alllogs new=logck&x._2; 
      run;     
    %end; 
 
    %let x = %eval(&x + 1); 
    %let lg = %scan(&currlogs, &x, "&delm"); 
    %let dt = %scan(&currdats, &x); 
    %let tm = %scan(&currtims, &x, '@'); 
  %end; 
 
  /* since a list of files can be provided then the files may not be in order */ 
  proc sort data=alllogs presorted; 
    by lognm line; 
  run; 
 
  /* if the name of the output file is not specified then default to the name */ 
  %if "&out" =  "" %then %do; 
    %let out=all_checklogs; 
  %end; 
 
  /* if the name of the output file is not specified then default to the name */ 
  %if "&loc2" = "" %then %do; 
    data _null_; 
 	  call symputx("loc2", "&loc");  	
    run;   
  %end; 
  
  /*Added by Rocio: sort by linenum*/
  proc sort data=alllogs sortseq=linguistic (numeric_collation=on);
  	by lognm linenum;
  run;  
 
  /* create the report */   
  ods listing close;   
  options orientation=landscape;   
  ods rtf file="&loc2.&slash.&out..rtf"; 
 
  proc report data=alllogs ls=140 ps=43 spacing=1 missing nowindows headline
  	style(column)=[just=left] style(header)=[just=left]; 
    column lognm logdt logtm linenum line logrs;  
    define lognm / order   style(column)=[width=15%]      "Log Name";
    define logdt / display style(column)=[width=10%]      "Log Date";
    define logtm / display style(column)=[width=10%]      "Log Time";
   	define linenum/display style(column)=[width=10%]	  "Line number";
    define line  / display style(column)=[width=30%] flow "Log Message"; 
    define logrs / display style(column)=[width=20%] flow "Reason Message is Allowed";  
    /* force a blank line after each file */ 
    compute after lognm;
      line " ";
    endcomp;
  run;   
  
  ods rtf close;
  ods listing;

%mend logCheck; 
