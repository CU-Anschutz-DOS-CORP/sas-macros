/**
 @file
 @brief Summarizes information about current session
 
 @details 
 @par Purpose
 This macro prints information regarding the current session in the log.
 Information printed: date, time, user, platform, version, application 
 (if appicable), sort size, memory size and CPUs.

 @author Rocio Lopez

 @date 03-15-2019
 
 @version SAS 9.2 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %runinfo;
 @endcode

 @returns
 Log file entry with information regarding current SAS session.
 
 @note 
 Macro has no parameters.

 @par Example(s)
 @code{.sas}
 **Example 1;
  %runinfo;
 @endcode
 
 @par Revision History
 @b 12-08-2020 - Added performance metrics
 @n @b 04-23-2021 - Added hostname and updated client version variable
 **/

%macro runinfo ;

%put ****************************************************;
%put ****************************************************;
%put ** Date:        &SYSDATE9 ;
%put ** Time:        %sysfunc(time(), timeampm8.) ;
%put ** User:        &SYSUSERID ;
%put ** Platform:    &SYSSCPL ;
%put ** Server:      &SYSHOSTNAME;
%put ** Version:     &SYSVLONG ;
%if %symexist(_ClientApp)=1 %then %do ;
%put ** Application: &_ClientApp &_ClientVersion ;
%end ;
%let megs = 1048576;
%put ** Sort Size:   %eval(%sysfunc(getoption(sortsize))/&megs)MB ;
%put ** Mem Size:    %eval(%sysfunc(getoption(memsize))/&megs)MB ;
%put ** CPUs:        %sysfunc(getoption(cpucount));

%put ****************************************************;
%put ****************************************************;

%mend runinfo ;