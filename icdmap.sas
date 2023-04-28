/**
 @file
 @brief Maps ICD-9 to ICD-10 and vice-versa
 
 @details 
 @par Purpose
 This macro uses CMS GEMs and ICD code lists to map ICD-9 to ICD-10 
 and ICD-10 to ICD-9 diagnosis or procedure codes. 
 It can also be used to generate a list of select codes with 
 descriptions and can also provide a list of code changes 
 for ICD-10 codes.
 
 @author Rocio Lopez

 @date 12-10-2020
 
 @version SAS 9.2 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %icdmap(
        libname = 
        ,icd9 = 
        ,icd10 = 
        ,codetype = dx
        ,map = 1
        ,checkconversion10 = 0
 );
 @endcode

 @returns
 One or more tables printed to the results window.
 
 @param libName Library name assigned to the directory where the CMS
 tables are stored
   
 @param icd9 List of ICD-9 codes to map, each enclosed by ''
 @n This expression will be used with the IN () operator

 @param icd10 List of ICD-10 codes to map, each enclosed by ''
 @n This expression will be used with the IN () operator

 @param codeType Type of codes provided
 @n Valid options are: DX for diagnosis (default) or PRC for 
 procedures
 
 @param map Logical for whether or not to map across versions
 @n Valid options are: 0 for No, 1 for Yes (default)

 @param checkConversion10 If ICD-10 list given, print a list of 
 changes in specified codes, if any
 @n Valid options are: 0 for No, 1 for Yes (default)

 @note 
 @parblock
 @li User must have necessary CMS tables stored as .sas7bdat files 
 and connect to the location using LIBNAME statement. Go to 
 https://github.com/lopezr/public-files/blob/main/icdMap/CMS%20ICD%20Tables%20Used%20by%20%25icdMap%20SAS%20macro.pdf
 to see a list of the tables we use. You may need to update the 
 table and/or column names within the macro depending on your set-up.
 
 @li You can omit the periods from the codes.

 @li If you have a full range of codes to use (456.xx, for example), 
 you can query the tables prior to using the macro and create a 
 macro list containing the codes. See Example 3 below.
 @endparblock

 @par Example(s)
 @code{.sas}
 **Example 1 - Mapping ICD-9-CM to ICD-10-CM ;
 %icdmap(
    libname = cms 
    ,icd9 = "456.0" "456.20"
 );
 
 **Example 2 - Mapping ICD-9-CM to ICD-10-CM and vice vers;
 %icdmap(
    libname = cms 
    ,icd9 = "456.0" "456.20"
    ,icd10 = "I85.01" "I85.11"
 ) ;

 **Example 3 - Creating a macro variable with list of codes corresponding to a range ;
 *Create list of all 456.xx ICD-9-CM codes;
 proc sql noprint;
     select compress(ICD9_CODE, " .")
     into :list456 separated by '" "'
     from cms.CMS_ICD9_DX_DESC_2014
     where substr(ICD9_CODE, 1, 3) eq "456" 
     order by ICD9_CODE;
 quit ;   
 %put ***LIST: "&list456"***; 
  
 *Map ICD-9-CM 456.xx to ICD-10-CM" ;
 %icdmap(
     libname = cms
     ,icd9 = "&list456") ;
 @endcode
 
 @par Revision History
 n/a 
**/

%macro icdmap(
    libname = ,
    icd9 = , 
    icd10 = , 
    codetype = dx, 
    map = 1,
    checkconversion10 = 0) / minoperator;

%put ************************************************************;
%put NOTE:  Macro &sysmacroname is starting to run. ;
%put ************************************************************;

%local ERRORFLAG DX9TO10 DX10TO9 DXDESC9 DXDESC10 PRC9TO10 PRC10TO9 
PRCDESC9 PRCDESC10 LAB9 LAB10 SUFFIX9 SUFFIX10;

%put ************************************************************;
%put ************************************************************;
%put *                ERROR CHECKING                            *;
%put ************************************************************;
%put ************************************************************;

%let errorflag = 0 ;

%put *************************************************;
%put * CHECKING LIBNAME GIVEN AND EXISTS             *;
%put *************************************************;

%if %length(&libname) eq 0 %then %do ; 
    %put ERROR: Parameter LIBNAME must be provided. Macro will stop executing. ; 
    %let errorflag = 1 ;
%end ;

%else %if %length(&libname) gt 0 %then %do ;
    %if %sysfunc(libref(&libname)) gt 0 %then %do ;
        %put ERROR: Library %upcase(&libname) does not exist. Macro will stop executing. ; 
        %let errorflag = 1 ;    
    %end ;
    %if %sysfunc(libref(&libname)) lt 0 %then %do ;
        %put WARNING: Library %upcase(&libname) exists, but the pathname is in question. Macro will continue but this might cause errors. ;    
    %end ;    
%end ;

%put *************************************************;
%put * CHECKING AT LEAST 1 CODE LIST PROVIDED        *;
%put *************************************************;

%if %length(&icd9) eq 0 and %length(&icd10) eq 0 %then %do ;
    %put ERROR: Either ICD9LIST or ICD10LIST must be provided. Macro will stop executing. ; 
    %let errorflag = 1 ;
%end ;

%put *************************************************;
%put * CHECKING CODETYPE PARAMETER                   *;
%put *************************************************;

%if not(%upcase(&codetype)  in (DX PRC)) %then %do ;
    %put ERROR: CODETYPE must be either DX or PRC. ;
    %put        CODETYPE = **&codetype** ;
    %let errorflag = 1 ;
%end ; 

%put *************************************************;
%put * CHECKING MAP PARAMETER                        *;
%put *************************************************;

%if not(%upcase(&map)  in (0 1 NO YES)) %then %do ;
    %put WARNING: MAP must be either 0, 1, no or yes. ;
    %put          MAP = **&map** ;
    %put          Default value of 1 (YES) will be used instead. ;
    %let map = 1 ;
%end; 

%put *************************************************;
%put * CHECKING CHECKCONVERSION10 PARAMETER          *;
%put *************************************************;

%if not(%upcase(&checkconversion10)  in (0 1 NO YES)) %then %do ;
    %put WARNING: CHECKCONVERSION10 must be either 0, 1, no or yes. ;
    %put          CHECKCONVERSION10 = **&checkconversion10** ;
    %put          Default value of 1 (YES) will be used instead. ;
    %let map = 1 ;
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
%put *                DEFINE TABLES AND NAMES                   *;
%put ************************************************************;
%put ************************************************************;

%* If you want to use different versions, change the names here but be mindful that column names match ;
%let codetype = %upcase(&codetype);

%* CPRH Teradata Data Lab Table names ;
%let map9to10   = &libname..CMS_GEM_&codetype._ICD9_ICD10_2018 ;
%let map10to9   = &libname..CMS_GEM_&codetype._ICD10_ICD9_2018 ;
%let desc10     = &libname..CMS_ICD10_&codetype._DESC_2018 ;
%let desc9      = &libname..CMS_ICD9_&codetype._DESC_2014 ;
%let conversion = &libname..CMS_ICD10_&codetype._CONVERSION_2021 ;

%* Misc set-up for column names and title labels  ;
%if %upcase(&codetype) eq DX %then %do;     
    %let lab9  = ICD9 ;
    %let lab10 = ICD10 ;
    %let suffix9 = CM diagnosis;
    %let suffix10 = CM ;
%end;

%else %if %upcase(&codetype) eq PRC %then %do;     
    %let lab9  = PROCEDURE ;
    %let lab10 = PROCEDURE ;
    %let suffix9 = CM procedure;
    %let suffix10 = PCS ;
%end;

%let icd9 = %upcase(%sysfunc(compress(&icd9, .)));
%let icd10 = %upcase(%sysfunc(compress(&icd10, .)));

%if %length(&icd9) %then %do;

    %put ************************************************************;
    %put ************************************************************;
    %put *                MANAGE ICD9 CODE LIST                     *;
    %put ************************************************************;
    %put ************************************************************;

    %* List code with description ;
    %if %upcase(&map) in (0 NO) %then %do ;
        title3 "Description of ICD-9-&suffix9 codes" ;
        proc sql ;
            select *
            from &desc9
            where compress(&lab9._CODE, ". ") in (&icd9)
            order by &lab9._CODE ;
        quit ;
        title3 ;
    %end ;
        
    %* Map 9 to 10 ;
    %else %do ;
        title3 "ICD-9-&suffix9 to ICD-10-&suffix10 mapping" ;
        proc sql ;
            select 
            %if %upcase(&codetype) eq DX %then %do ; 
                a.ICD9_Code, 
            %end ;
            %else %if %upcase(&codetype) eq PRC %then %do ; 
                compress(substr(a.ICD9_Code,1,2))||"."||compress(substr(a.ICD9_Code,3,2)) as ICD9_code, 
            %end ; 
            b.Long_Description as ICD9_Description label = "ICD9_Description",
            a.ICD10_Code, c.Long_Description as ICD10_Description label = "ICD10_Description",
            a.Flag as MappingFlags label = "Mapping Flags"
            /* , a.ApproximateFlag, a.NoMapFlag, a.CombinationFlag, a.ScenarioFlag, a.ChoiceListFlag */
            %if %length(&icd10) gt 0 %then %do ;
                , 
                case
                  when compress(a.ICD10_CODE, ". ") not in (&icd10) then "1"
                  else " "
                  end as NotInICD10List
            %end ;            
            from &map9to10 (where = (compress(ICD9_CODE, ". ") in (&icd9))) a
            left join &desc9 b
              on a.ICD9_CODE = b.&lab9._Code       
            left join &desc10 c
              on a.ICD10_CODE = c.&lab10._Code  
            order by a.ICD9_CODE, a.Flag ;
        quit ;
        title3 ;
    %end ;
        
%end ; %* end to icd9 loop ;

%if %length(&icd10) %then %do;

    %put ************************************************************;
    %put ************************************************************;
    %put *                MANAGE ICD10 CODE LIST                    *;
    %put ************************************************************;
    %put ************************************************************;

    %* List code with description ;
    %if %upcase(&map) in (0 NO) %then %do ;
        title3 "Description of ICD-10-&suffix10 codes" ;
        proc sql;
            select 
            &lab10._CODE as ICD10_Code label = "ICD10_Code", 
            /*%if %upcase(&codetype) eq DX %then %do; SHORT_DESCRIPTION, %end ; */
            LONG_DESCRIPTION
            from &desc10
            where compress(&lab10._CODE, ". ") in (&icd10)
            order by &lab10._CODE;;
        quit;
        title3 ;
    %end ;
    
    %* Map 10 to 9 ;
    %else %do ;    
        title3 "ICD-10-&suffix10 to ICD-9-&suffix9 mapping" ;
        proc sql;
            select 
            a.ICD10_Code, c.Long_Description as ICD10_Description label = "ICD10_Description",
            %if %upcase(&codetype) eq DX %then %do ; 
                a.ICD9_Code, 
            %end ;
            %else %if %upcase(&codetype) eq PRC %then %do ; 
                compress(substr(a.ICD9_Code,1,2))||"."||compress(substr(a.ICD9_Code,3,2)) as ICD9_code, 
            %end ;             
            b.Long_Description as ICD9_Description label = "ICD9_Description",    
            a.Flag as MappingFlags label = "Mapping Flags"
            /* , a.ApproximateFlag, a.NoMapFlag, a.CombinationFlag, a.ScenarioFlag, a.ChoiceListFlag */
            %if %length(&icd9) gt 0 %then %do ;
                , 
                case
                  when compress(a.ICD9_CODE, ". ") not in (&icd9) then "1"
                  else " "
                  end as NotInICD9List
            %end ;             
            from &map10to9 (where = (compress(ICD10_CODE, ". ") in (&icd10))) a
            left join &desc9 b
              on a.ICD9_CODE = b.&lab9._Code       
            left join &desc10 c
              on a.ICD10_CODE = c.&lab10._Code  
            order by a.ICD10_CODE, a.Flag ;
        quit;
        title3 ;
    %end ;

    %* Changes in codes ;
    %if %upcase(&checkconversion10) in (1 YES) %then %do ;
        title3 "Checking for newer or older ICD-10-&suffix10 codes" ;
        proc sql ; 
            select *
            from &conversion
            where 
              %if %upcase(&codetype) eq DX %then %do ;
                compress(CURRENT_CODE, ". ") in (&icd10) or compress(OLD_CODE, ". ") in (&icd10) 
              %end;
              %else %if %upcase(&codetype) eq PRC %then %do ;
                compress(CURRENT_ICD10_CODE, ". ") in (&icd10) or compress(OLD_ICD10_CODE, ". ") in (&icd10) 
              %end;            
            order by 
              %if %upcase(&codetype) eq DX %then %do ; CURRENT_CODE, OLD_CODE %end ;    
              %else %if %upcase(&codetype) eq PRC %then %do ; CURRENT_ICD10_CODE, OLD_ICD10_CODE %end ; ;
        quit;
        title3 ;
    %end ;
%end ; %* end to icd10 loop ;

%put ************************************************************;
%put NOTE:  Macro &sysmacroname has finished. ;
%put ************************************************************;

%mend icdmap ;
