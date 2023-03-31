/**
 @file
 @brief Asseses associations between 1 continuous outcome and 
 1+ variables
 
 @details 
 @par Purpose
 This macro generates a table assessing associationg between 1 
 continuous outcome and categorical or continuous variables.
 
 @author Rocio Lopez
 
 @date 06-10-2016
 
 @version SAS 9.4 or later
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %ctable(
     ds= 
    ,outcome= 
    ,catList= 
    ,stat=mean
    ,contList=
    ,corrType=P 
    ,subset= 
    ,outSs=_outds
    ,nFmt= __countf.
    ,pFmt= pvaluef.
    ,estFmt= __estf.
    ,rhoFmt=8.2 
    ,tblTitle= 
    ,rtfFile=     
    ,cWidth1=250
    ,cWidth2=50 
    ,cWidth3=225
    ,nTables=1 
    ,deBug=0
 );
 @endcode

 @returns
 One or 2 report-ready tables and a SAS data set with all results.

 @param ds SAS data set name
 
 @param outcome Name of continuous outcome variable (only 1 allowed)
 
 @param catlist List of categorical variable(s) to analyze
 
 @param contlist List of continuous variable(s) to analyze

 @param subset Expression to subset population. This will be used 
 in a WHERE statement.

 @param stat Outcome summary statistics presented for categorical 
 variables. 
 @n Options are:
 @n MEAN for mean +/- sd and ANOVA (DEFAULT)
 @n MEDIAN for median [P25, P75] with Kruska-Wallis test

 @param corrtype Correlation type for association between outcome 
 and continuous variables
 @n Options are:
 @n P for Pearson's correlation (95% CI) (DEFAULT)
 @n S for Spearman's correlation (95% CI)

 @param outds SAS data set that will contain final output

 @param nfmt Format for N column
 @n Default is __countf. (defined within macro)

 @param pfmt Format for p-value
 @n Default is pvaluef. (defined within macro)
 
 @param estfmt Format for mean, sd, percentiles
 @n Default is __esttf. (defined within macro)
 
 @param rhofmt Format for correlations
 @n Default is 8.2
 
 @param tbltitle Tile for table surrounded by ""
 @n Default is "Univariate relationships between &outcome and 
 variables of interest"
 
 @param rtffile RTF output location and file name; 
 surrounded by " " (e.g. "./Example.rtf")

 @param cwidth1 Width for label column in .rtf file
 @n Default is 250
 
 @param cwidth2 Width for N column in .rtf file
 @n Default is 50
 
 @param cwidth3 Width for statistics column in .rtf file
 @n Default is 225
 
 @param ntables Number of output tables; this will only take 
 effect if BOTH catlist and contlist are provided
 @n Options are:
 @n 1 for 1 output table with catlist and contlist (DEFAULT)
 @n 2 for 2 output tables, 1 with catlist and another with contlist
 
 @param debug Turn on/off mlogic and mlogicnest for error 
 checking
 @n Options are: 
 @n 0 for off (DEFAULT)
 @n 1 for on

 @note 
 @parblock
 @li Variables should be labeled and formatted.  
 @li Note that if you are runing in a windowing environment the 
 plus/minus sign when reporting means and sds  will appear as
 (*ESC*){unicode '00B1'x}. This will be properly displayed in RTF.
 @li This macro uses the AUTOCALL CMPRES macro. Make sure this 
 is accessible via sasautos = (sasautos) or, if not, loaded into 
 your session
 @li Datasets created and deleted during run:
 _null _med _p _temp _rho _rho2 _rho3 _lbs _varlabels _misout_ 
 _tmiss_ _subset
 @endparblock

 @par Example(s)
 @code{.sas}
 %ctable(
      ds=sashelp.cars
     ,outcome=MSRP
     ,catList=Type DriveTrain
     ,stat=median
     ,contList=MPG_City MPG_Highway
     ,cwidth3=350
     ,rtfFile="./CTABLE.rtf"
 );
 @endcode
 
 @par Revision History
 @b 11-16-2020 Fix for running in 9.4M6 or later because the name
 for the ODS output for NPAR1WAY changed.                 
 @n @b 11-06-2020 Fixed bug that was throwing an errror when 
 you requested medians for 1 level categorical variable. 
**/

%macro ctable(
     ds=                 /* data set name */
    ,outcome=            /* continuous variable to summarize */
    ,catList=            /* list of cat variables */
    ,stat=mean           /* mean or median , only 1 allowed*/ 
    ,contList=           /* list of cont variables */
    ,corrType=P          /* P or S, only 1 allowed*/
    ,subset=             /* subset population*/
    ,outDs=_outds        /* output ds */
    ,nFmt= __countf.     /* format for N */
    ,pFmt= pvaluef.      /* format for p-value */
    ,estFmt= __estf.     /* format for means/medians*/
    ,rhoFmt=8.2          /* format for rho */
    ,tblTitle=           /* title, use ""*/
    ,rtfFile=            /* e.g. "/home/lopez/Example.rtf" */    
    ,cWidth1=250         /* for label*/
    ,cWidth2=50          /* for N*/
    ,cWidth3=225         /* for estimates*/
    ,nTables=1           /* number of tables (1 or 2) created */
    ,deBug=0             /* debugging */
);

*******************************************************************;
*** Set options                                                    ;
*******************************************************************;
ods graphics off; 
ods results off;  

options 
    sasautos = (sasautos)
    mautosource mrecall
    minoperator; **this allows using in operator with the macro vars;

%if &DEBUG=1 %then %do;
    options mlogic mlogicnest;
%end;

*******************************************************************;
*** Define some macro vars                                         ;
*******************************************************************;
%let style = journal; **later on might add option to change in call;
%if %upcase(&CORRTYPE)=PEARSON %then %let corrtype=P;
%if %upcase(&CORRTYPE)=SPEARMAN %then %let corrtype=S;

*******************************************************************;
*** Error checks                                                   ;
*******************************************************************;
%* Set error flag to 0;
%let ERRORFLAG=0;

%* If title provided, make sure there are "" marks;
%if (&TBLTITLE ^= ) and %SYSEVALF(%SYSFUNC(indexc(&TBLTITLE, '"')) ^= 1) 
    %then %do;
    %PUT ERROR: Double quotes are required for Title. Macro will stop executing.;
    %LET ERRORFLAG=1;
%end;

%* Check dataset provided;
%if (&ds= ) %then %do;
    %put ERROR: Requiered parameter DS is not provided. Macro will stop executing.;
    %let ERRORFLAG=1;
%end;

%* Check Dataset exists;
%if (&ds ^= ) and %SYSFUNC(exist(&ds)) ne 1 %then %do;
    %PUT ERROR: Data set &ds does not exist. Macro will stop executing;
    %LET ERRORFLAG=1;
%end;

%* Check outcome is provided. Only 1 variable allowed here.;
%if (&OUTCOME= ) %then %do;
    %put ERROR: Requiered parameter OUTCOME is not provided. Macro will stop executing.;
    %let ERRORFLAG=1;
%end;

%if (&OUTCOME ^= ) %then %do;
    %let o=0;
    %do %while(%scan(&OUTCOME,&o+1) ^=    ); %let o=%eval(&o+1); %end;
    %if &O > 1 %then %do;
        %put ERROR: Only 1 OUTCOME variable allowed. Macro will stop executing.;
        %let ERRORFLAG=1;   
    %end;
%end;     

%* If catlist given, make sure stat is either mean or median and only 1 is given;
%* If not revert to default and put warning;
%if (&CATLIST ^= ) %then %do;
    %let s=0;
    %do %while(%scan(&STAT,&s+1) ^=    ); %let s=%eval(&s+1); %end;
    %if &S > 1 %then %do;
        %put WARNING: Only 1 STAT allowed. The default value of MEAN will be used.;
        %let stat=mean;
    %end;   

    %if &s=1 and not(%upcase(&STAT) in (MEAN MEDIAN)) %then %do;
        %put WARNING: STAT must be equal to MEAN or MEDIAN. The default value of MEAN will be used.;
        %let stat=mean;
    %end;
%end;

%* If contlist given, make sure corrtype is S or P and only one is given;
%* If not revert to default and put warning;
%if (&CONTLIST ^= ) %then %do;
    %let c=0;
    %do %while(%scan(&CORRTYPE,&c+1) ^=    ); %let c=%eval(&c+1); %end;
    %if &C > 1 %then %do;
        %put WARNING: Only 1 CORRTYPE allowed. The default value of P will be used.;
        %let corrtype=p;
    %end;   
    
    %if not(%upcase(&CORRTYPE) in (S P)) %then %do;
        %put WARNING: STAT must be equal to P, PEARSON, S or SPEARMAN. The default value of P will be used.;
        %let corrtype=p;
    %end;
%end;

%* ntables must be 1 or 2, if other value given revert to default;
%if not(&NTABLES in (1 2)) %then %do;
    %put WARNING: NTABLES must be equal to 1 or 2. The default value of 1 will be used.;
    %let ntables=1;
%end;

%* If ntables=2, make sure that both cat and cont lists are given;
%* If not, then set ntables to 1 and put a warning;
%if &NTABLES=2 and ((&CATLIST= ) OR (&CONTLIST= )) %then %do;
    %put WARNING: NTABLES=2 is only allowed when both CATLIST and CONTLIST are provided. Only 1 table will be generated.;
    %let ntables=1;
%end;

%*If data set exists, check if outcome and varlists exist;
%if (&ds ^= ) and %SYSFUNC(exist(&ds))=1 %then %do;

    %* Check if outcome exists;
    %if (&OUTCOME ^= ) and &O=1 %then %do;
        data _null; 
            dsid=open("&ds"); 
            call symput ('otcmchk',varnum(dsid,"&OUTCOME"));
            rc=close(dsid);
        run;
        %if &OTCMCHK=0 %then %do;
            %put ERROR: Outcome &OUTCOME does not exist in dataset &ds. Macro will stop executing.;
            %let ERRORFLAG=1;
        %end;
        
        %* If it exists, check that it is not completely missing. Also check that it is numeric;
        %if &OTCMCHK > 0 %then %do;
            %* Check type;
            proc contents data=&ds noprint out=_type; run;
            proc sql noprint;
                select type into :_type
                from _type
                where upcase(Name)=%upcase("&outcome");     
            %if &_TYPE ne 1 %then %do;
                %put ERROR: Outcome &OUTCOME is a character variable. Macro will stop executing.;
                %let ERRORFLAG=1;
            %end;
        
            %* If numeric, check missingness;
            %if &_TYPE=1 %then %do;
                proc means data=&ds n noprint;
                    var &outcome;
                    output out=__misso;
                run;
                proc sql noprint;
                    select &OUTCOME. into :__no
                    from __misso 
                    where _stat_="N";
                %if &__NO=0 %then %do;
                    %put ERROR: Outcome &OUTCOME is completely missing. Macro will stop executing.;
                    %let ERRORFLAG=1;
                %end;
            %end; %* end to if __type=1;                
        %end; %* end to if otcmche > 0; 
            
        proc datasets lib=work nolist;
            delete _null __misso _type;
        run;
        quit;
    %end;
    
    %* Check if all specified variables exist;
    %macro vexist(List, type);
    
        %LET space1=%SCAN(&ds,1);
        %LET space2=%SCAN(&ds,2);
        
        %IF (&SPACE2= ) %THEN %DO;
            %LET MEMNAME_=%UPCASE(&SPACE1);
            %LET LIBNAME_=WORK;
        %END;
        %ELSE %IF (&SPACE2^= ) %THEN %DO;
            %LET MEMNAME_=%UPCASE(&SPACE2);
            %LET LIBNAME_=%UPCASE(&SPACE1);
        %END;
    
        %LET NEW&TYPE= ;
        %LET ICOUNT=1;
        %LET CVAR=%SCAN(&LIST,&ICOUNT);
        %LET NEXVAR=&CVAR;
    
        %IF (&LIST^= ) %THEN %DO %WHILE(&NEXVAR NE );
    
            %LET CVAR=%UPCASE(&NEXVAR);
            %LET ICOUNT=%EVAL(&ICOUNT+1);
            %LET NEXVAR=%SCAN(&LIST,&ICOUNT);
    
            proc sql noprint;
                select left(put(count(*),8.)) into :exist
                from dictionary.columns
                where libname="&&LIBNAME_" and
                memname="&&MEMNAME_" and
                upcase(name)="&&CVAR";
                quit;
    
            %IF &EXIST=1 %THEN %LET NEW&TYPE=&&NEW&TYPE &CVAR;
            %ELSE %IF &EXIST NE 1 %THEN %LET NEW&TYPE=&&NEW&TYPE;
    
            %IF &EXIST=0 %THEN 
               %PUT WARNING: &CVAR in &TYPE list does not exist in data set &ds. and will not be analyzed.; 
        %END;
    
        %LET &&TYPE.=&&NEW&TYPE;
    
    %MEND;
    
    %VEXIST(LIST=&CATLIST, TYPE=CATLIST);
    %VEXIST(LIST=&CONTLIST, TYPE=CONTLIST);
    
    %* Eliminate variables that are completely missing and put Warning;
    %MACRO NEWLIST(DSET, list);
        %LOCAL LIST LIST2 COND NEWCATLIST ICT2 CTVAR NEXVAR OKCAT2 ;

        %LET LIST=%UPCASE(&LIST);
        %LET LIST2=%str(&)&LIST;

        %IF &LIST=CATLIST %THEN %DO;

           %LET NEWCATLIST= ;
           %LET ICT2=1; %PUT &ICT2;
           %LET CTVAR=%SCAN(&CATLIST,&ICT2); %PUT &ctvar;
           %LET NEXVAR=&CTVAR; %PUT &NEXVAR;
           %DO %WHILE(&NEXVAR NE );

             %LET CTVAR=&NEXVAR;
             %LET ICT2=%EVAL(&ICT2+1);
             %LET NEXVAR=%SCAN(&CATLIST,&ICT2);

             %LET OKCAT2= ;
             proc freq data=&DSET noprint;
               table &CTVAR/ out=____cout;
             run;
             proc means noprint data=____cout sum;
               var percent; 
               output out=____s1 sum=_s;
             run;
             data _null_;
               set ____s1;
               if _s=. then ok=0; else ok=1;
               call symput('OKCAT2',put(ok,1.0));
               if ok=0 then do;
                  put
             "WARNING: Variable &CTVAR in list CATLIST is completely missing and will not be analyzed.";
                end;
              run;
             %IF &OKCAT2=1 %THEN %LET NEWCATLIST=&NEWCATLIST &CTVAR;
             proc datasets nolist; delete ____cout ____s1; run; quit;
            %END;

           %LET CATLIST=&NEWCATLIST;
         %END;
        %ELSE %IF &LIST=CONTLIST %THEN %DO;
           proc means noprint data=&DSET n;
             var
               %IF &SYSVER GE 9.4 %THEN %DO; %UNQUOTE(&LIST2.) %END;
               %ELSE %DO; &LIST2. %END;
             ;
             output out=_misout_
               %IF &SYSVER GE 9.4 %THEN %DO; N=%UNQUOTE(&LIST2.) %END;
               %ELSE %DO; N=&LIST2. %END;
             ;
           proc transpose data=_misout_ out=_Tmiss_; run;
           data _tmiss_;
             set _tmiss_;
             if _NAME_ ne '_TYPE_'; if _NAME_ ne '_FREQ_';
            run;

           data _null_;
              ** Get enough variable length for _vlist_ **;
             retain _vlist_ "&CONTLIST.";
             if _N_=1 then _vlist_=" ";
             retain change 0;
             set _Tmiss_ end=eof;
             if col1 ne 0 then _vlist_=trim(left(_vlist_))||" "||_NAME_;
             if col1=0 then do;
                change=1;
                put
        "WARNING: Variable " _NAME_ "in list CONTLIST is completely missing and will not be analyzed.";
               end;
             if eof=1 then do;
               if change=1 then call symput("&LIST", trim(_vlist_));
              end;
            run;
        %END;
        
        proc datasets lib=work nolist;
            delete _misout_ _tmiss_;
        run;
        quit;

    %MEND NEWLIST;

    %IF (&CATLIST NE ) %THEN %DO; %NEWLIST(dset=&ds, list=CATLIST); %END;
    %IF (&CONTLIST NE ) %THEN %DO; %NEWLIST(dset=&ds, list=CONTLIST); %END;

    %* Verify that CATLIST or CONTLIST have varibles listed;
    %if (&CATLIST = ) and (&CONTLIST = ) %then %do;
        %put ERROR: Either CATLIST or CONTLIST must be provided. Macro will stop excecuting.;
        %let ERRORFLAG=1;
    %end;
        
%end; %* end to outcome and varlist check;

*******************************************************************;
*** If no Errros, start macro                                      ;
*******************************************************************;
%if &ERRORFLAG=0 %then %do;
    
*******************************************************************;
*** Define formats                                                 ;
*******************************************************************;
proc format;
   picture Pvaluef (round)
           0.985   -   high    = "0.99"    (NoEdit)
           0.10    -<  0.985   = "9.99"
           0.001   -<  0.10    = "9.999"
           0       -<  0.001   = "<0.001"  (NoEdit)
           . = " ";
   picture __Estf (round)
           low   -  -1.1  = "00000000000009.9" (prefix='-')
          -1.1  <-<  0    = "9.99" (prefix='-')
             0   -<  1.1  = "9.99"
           1.1   -   high = "00000000000009.9";
   picture __Countf (round)
           low-high         = "0000000000000009"
           . = " ";
run;
quit;

%*****************************************************************************;
%*  DETERMINE SAS VERSION                                                     ;
%*****************************************************************************;
data _null_ ; 

   sysvlong = symget("SYSVLONG");                /* system macro variable */
   
   /* major version */
   pos1 = find(sysvlong, ".");
   major = input(substr(sysvlong, 1, pos1-1), best.);          

   /* minor version */
   pos2 = find(sysvlong, ".", 'i', pos1+1);
   minor = input(substr(sysvlong, pos1+1, pos2-pos1-1), best.);

   /* iteration version */
   pos3 = find(sysvlong, "M", 'i', pos2+1);
   iteration = input(substr(sysvlong, pos2+1, pos3-pos2-1), best.);

   /* maintenance level */
   pos4 = notdigit(sysvlong, pos3+1);
   maint = input(substr(sysvlong, pos3+1, pos4-pos3-1), best.);   

   b = ( major<9 ) 
      | ( major=9 & minor<4 )
      | ( major=9 & minor=4 & iteration<1 )
      | ( major=9 & minor=4 & iteration=1 & maint<=5 );
    if b then put "Running SAS 9.4m5 or earlier"; 
    else      put "Running After SAS 9.4m5";
    
    call symput('After94m5', b=0);
    
run; 

*******************************************************************;
*** Supress printing to output window                              ;
*******************************************************************;               
ods listing close;
    
%if %SYMEXIST(_CLIENTAPP) %then %do; 
    %if &_CLIENTAPP='SAS Studio' %then %do;
            ods exclude all;
    %end;
%end;

*******************************************************************;
*** Is subset specified, generate subset db                        ;
*******************************************************************;
%if (&subset^= ) %then %do;

    data _subset;
        set &ds;
        where &subset;
    run;
    %let DS=_subset;

%end; %* end to if subset^= ;

*******************************************************************;
*** Create ds in which to store output                             ;
*******************************************************************;    
data &outds; 
    informat VarName Factor $50. n 8. Statistics $35. pvalue _order 8.;
run;

*******************************************************************;
*** Prepare output for categorical variables                       ;
*******************************************************************;
%if (&catlist ^= ) %then %do;

    %* get labels & Formats;
    data _lbs; set &ds; keep &catlist; run;
    proc contents data=_lbs noprint out=_VarLabels; run;
        
    %* Define +/- sign;
    %* let PlMi=%str(±);
    %let PlMi=(*ESC*){unicode '00B1'x};
    
    %*  count the number of elements in catlist;
    %let p=0;
    %do %while(%scan(&catlist,&p+1) ^=    ); %let p=%eval(&p+1); %end;
        
    %* loop thourgh vars and get medians and pvalues;   
    %do num=1 %to &p.;
    
        %let var=%scan(&catlist, &num);
    
        data _null;
            set _varlabels;
            where upcase(name)=%upcase("&var.");
            call symput('Lab', trim(label));
            call symput('Name', trim(upcase(name)));
            call symput('TYPE', type);
            if type=1 and format ne "" then call symput('FMT', trim(format)||".");
        run;

        %* Get %tiles;
        proc means data=&ds stackods n mean stddev median p25 p75;
            class &var;
            var &outcome.;
            ods output Summary=_Med;
        run;
    
        data _med;
            set _med;
            informat VarName Factor $50. Statistics $35.;
            %* Column with variable name;
            VarName="&Name.";
                        
            %* Create column with formats;
            %if &type=2 %then %do;
                Factor=".    "||left(trim(&var.));
            %end;
            %else %if &type=1 %then %do;
                %if %symexist(FMT)=0 %then %do;
                    Factor=".    "||left(put(&var., 8.));
                %end;
                %else %if %symexist(FMT)=1 %then %do;
                    Factor=".    "||left(put(&var., &FMT)); 
                %end;
            %end;
            
            %* Summary stats;
            %if %cmpres(%upcase("&stat"))="MEAN" %then %do;       
                Statistics=compress(put(Mean, &estfmt))||" &PlMi. "||
                       compress(put(stddev, &estfmt));
            %end;                   
                     
            %else %if %cmpres(%upcase("&stat"))="MEDIAN" %then %do;       
                Statistics=compress(put(Median, &estfmt))||" ["||
                       compress(put(P25, &estfmt))||", "||
                       compress(put(P75, &estfmt))||"]";
            %end;
                    
            %* Create column with input order;
            _order=&num+(_n_/100);
                   
            format N &nfmt;      
        run;
    
        %* Get p-value and attach label to it;
        %if %cmpres(%upcase("&stat"))="MEAN" %then %do;   
            proc glm data=&ds;
                class &var;
                model &outcome = &var;
                ods output ModelAnova=_p;
            run;
                                
            proc sql;
                create table _temp as select
                "&Name." as VarName,
                "&Lab." as Factor,
                 ProbF as pvalue format &pfmt label="p-value",
                 &num as _order
                 from _p
                 where HypothesisType=3;                        
        %end;  %* end to mean section;          
            
        %else %if %cmpres(%upcase("&stat"))="MEDIAN" %then %do;             
            proc npar1way data=&ds wilcoxon;
                class &var;
                var &outcome;
                ods output KruskalWallisTest=_p;
            run;

            **for 1 level cat vars, _p is not created;
            %if not %SYSFUNC(exist(_p)) %then %do;
                data _p;
                    nvalue1=.;
                    name1="P_KW";
                run;
            %end;
            
            proc sql;
                create table _temp as select
                "&Name." as VarName,
                "&Lab." as Factor,
                 %if not(&After94m5) %then %do; nvalue1 %end;
                 %else %if &After94m5 %then %do; Prob %end;
                   as pvalue format &pfmt label="p-value",
                 &num as _order
                 from _p
                 %if not(&After94m5) %then %do; where name1="P_KW" %end; 
                 ;
       %end; %* end to median section;
    
       %* Concatanate;
       data &outds;
           set &outds _temp(in=inlab) _med;
           if inlab and Factor="" then Factor=Varname;
           _type=1;
           keep varname factor n Statistics pvalue _order _type;
       run;
    
       proc datasets library=work nolist;
           delete _med _p _temp;
       run;
       quit;
    
    %end; %* end of do num=1 to p loop;
        
        proc datasets library=work nolist;
           delete _lbs _varlabels;
        run;
        quit;       
        
%end; %* end to if catlist;
    
%* Delete blank row at top of outds;
data &outds; set &outds; if _n_>1; run;
    
*******************************************************************;
*** Prepare output for continuous variables                        ;
*******************************************************************;
%if (&contlist ^= ) %then %do;

    %* If no catlist given then set p to 0;
    %if (&catlist= ) %then %let p=0;

    %* Get correlations;
    proc corr data=&ds fisher(biasadj=no) 
        %if %cmpres(%upcase("&corrtype"))="P" %then %do; pearson %end;
        %else %if %cmpres(%upcase("&corrtype"))="S" %then %do; spearman %end;;
        var &outcome. &contlist.;
        ods output 
          %if %cmpres(%upcase("&corrtype"))="P" %then %do; FisherPearsonCorr=_rho %end;
          %else %if %cmpres(%upcase("&corrtype"))="S" %then %do; FisherSpearmanCorr=_rho %end; ;
    run;    
    
    data _rho2;
        informat VarName $50. n 8. Statistics $35. pvalue _order 8.;
        set _rho;
        where upcase(var)=upcase("&outcome.");
        VarName=upcase(WithVar);
        N=Nobs;
        Statistics=compress(put(corr, &rhofmt))||" ("||
                compress(put(lcl, &rhofmt))||", "||
                compress(put(ucl, &rhofmt))||")";
        _order=&p+_n_;
        _type=2;
        format pvalue pvaluef.;
        keep VarName n Statistics pvalue _order _type;
    run;    

    %* Attach labels;
    data _lbs; set &ds; keep &contlist; run;
    proc contents data=_lbs noprint out=_VarLabels; run;        

    proc sql;
        create table _rho3 as select
        VarName, 
        Label as Factor informat $50. format $50. length=50 label="Factor", 
        N, Statistics, pvalue, _order, _type
        from _rho2 a left join _varlabels b
        on upcase(a.VarName)=upcase(Name);
    
    data &outds;
        set &outds _rho3;
    run;
        
    proc datasets library=work nolist; 
        delete _rho _rho2 _rho3 _lbs _varlabels; 
    run; quit;
                
%end; %* end to if contlist;

*******************************************************************;
*** Prepare final report                                           ;
*******************************************************************;
%* Return printing to output window;
ods listing;
    
%if %SYMEXIST(_CLIENTAPP) %then %do; 
    %if &_CLIENTAPP='SAS Studio' %then %do;
            ods exclude none;
    %end;
%end;
        
%* Print output;
/*  proc print data=&outds noobs; run; */
    

%* Open requested ODS destinations;
%if (&RTFFILE^= ) %then %do;
    options nodate nonumber
        topmargin=1in bottommargin=1in
        leftmargin=1in rightmargin=1in;
    ods rtf file=&RTFFILE  style=&style;
%end; **end to if rtffile^= ;

%* Note: if ntable=2 will call several reports instead of using by statment because
    that will make managing the titles, headers and footnotes easier;

%* Prepare report - 1 table requested and both cat and cont vars given;
%if &ntables=1 and (&catlist ^= ) and (&contlist ^= ) %then %do;
    proc report data=&outds nowd ls=100
        style(report)={borderwidth=3 bordercolor=black cellpadding=3
            font_size=11pt font_face=Times  FONTSTYLE= ROMAN}
        style(lines)={background=white foreground=black
            font_size=9pt font_face=Times FONTSTYLE= ROMAN
            protectspecialchars=off}        
        style(column)={background=white foreground=black
            font_size=11pt font_face=Times FONTSTYLE= ROMAN
            font_weight=medium}     
        style(header)={background=white foreground=black borderbottomstyle=double
            font_weight=bold FONTSTYLE= ROMAN
            font_size=11pt font_face=Times};
        
        column (Factor N Statistics pvalue _p);
        
        %* Title;  
        compute before _PAGE_ /style={font_weight=bold font_size=11pt
                              just=left borderbottomwidth=3
                              borderbottomcolor=black bordertopcolor=white};
            %if (&TblTitle.^= ) %then %do; line &TblTitle.; %end;
            %if (&TblTitle.= ) %then %do; 
                line "Univariate relationships between &outcome and variables of interest"; 
            %end;
        endcomp;
    
        %* Variable name column;
        define Factor/order=data left
                    style(header)={just=left}
                    style(column)={cellwidth=&cwidth1. font_weight=bold};
        
        %* Count column;
        define N/display "N" style(column)={just=center cellwidth=&cwidth2.};
            
        %* Estimate column;
        define Statistics/display style(column)={just=center cellwidth=&cwidth3.};
        
        %* p-value column;
        define pvalue/display noprint;
        define _p/computed  "p-value"
                  format=pvaluef.
                  style(header)={just=center}
                  style(column)={just=center cellwidth=85};
        compute _p;
            _p=pvalue;
            if _p < 0.05 then call define(_col_,
            "style", "style=[font_weight=bold fontstyle=italic]");
        endcomp;
            
        %*Add Footnote;
        compute after/style(lines)={font_size=9pt
               just=left borderbottomcolor=white
               bordertopcolor=black bordertopwidth=3};
              line "Statistics presented as "
                %if (&catlist ^= ) %then %do; 
                    %if %cmpres(%upcase("&stat"))="MEAN" %then %do;
                      "Mean &PlMi. SD with ANOVA"
                    %end; 
                    %else %if %cmpres(%upcase("&stat"))="MEDIAN" %then %do;
                      "Median [25th, 75th percentiles] with Kruskal-Wallis test"
                    %end; 
                %end;
                %if (&contlist ^= ) %then %do;
                    %if (&catlist ^= ) %then %do; " or " %end;
                    %if %cmpres(%upcase("&corrtype"))="P" %then %do;                
                      "Pearson's correlation (95% CI)"
                    %end; 
                    %else %if %cmpres(%upcase("&corrtype"))="S" %then %do;              
                      "Spearman's correlation (95% CI)"
                    %end;   
                %end;
                ".";                              
        endcomp;    
    run;
%end; %* end of if ntable =1;
    
%* Prepare report - 2 table requested or only one type of vars is given;
%if &ntables=2 or ((&catlist= ) or (&contlist= )) %then %do;

    %* Table for categorical factors;
    %if (&catlist ^= ) %then %do;
    proc report data=&outds nowd ls=100
        style(report)={borderwidth=3 bordercolor=black cellpadding=3
            font_size=11pt font_face=Times  FONTSTYLE= ROMAN}
        style(lines)={background=white foreground=black
            font_size=9pt font_face=Times FONTSTYLE= ROMAN
            protectspecialchars=off}        
        style(column)={background=white foreground=black
            font_size=11pt font_face=Times FONTSTYLE= ROMAN
            font_weight=medium}     
        style(header)={background=white foreground=black borderbottomstyle=double
            font_weight=bold FONTSTYLE= ROMAN
            font_size=11pt font_face=Times};
        
        where _type=1;
        
        column (Factor N Statistics pvalue _p);
        
        %* Title;  
        compute before _PAGE_ /style={font_weight=bold font_size=11pt
                              just=left borderbottomwidth=3
                              borderbottomcolor=black bordertopcolor=white};
            %if (&TblTitle.^= ) %then %do; line &TblTitle. " (categorical variables)"; %end;
            %if (&TblTitle.= ) %then %do; 
                line "Univariate relationships between &outcome and categorical variables of interest"; 
            %end;
        endcomp;
    
        %* Variable name column;
        define Factor/order=data left
                    style(header)={just=left}
                    style(column)={cellwidth=&cwidth1. font_weight=bold};
        
        %* Count column;
        define N/display "N" style(column)={just=center cellwidth=&cwidth2.};
            
        %* Estimate column;
        define Statistics/display 
            %if %cmpres(%upcase("&stat"))="MEAN" %then %do;
                "Mean &PlMi. SD"
            %end;
            %else %if %cmpres(%upcase("&stat"))="MEDIAN" %then %do;
                "Median [P25, P75]"
            %end;               
            style(column)={just=center cellwidth=&cwidth3.};
        
        %* p-value column;
        define pvalue/display noprint;
        define _p/computed  "p-value"
                  format=pvaluef.
                  style(header)={just=center}
                  style(column)={just=center cellwidth=85};
        compute _p;
            _p=pvalue;
            if _p < 0.05 then call define(_col_,
            "style", "style=[font_weight=bold fontstyle=italic]");
        endcomp;
            
        %*Add Footnote;
        compute after/style(lines)={font_size=9pt
               just=left borderbottomcolor=white
               bordertopcolor=black bordertopwidth=3};
              line 
                %if %cmpres(%upcase("&stat"))="MEAN" %then %do;
                  "p-values correspond to ANOVA."
                %end; 
                %else %if %cmpres(%upcase("&stat"))="MEDIAN" %then %do;
                  "p-values correspond to Kruskal-Wallis test."
                %end;  ;                          
        endcomp;    
    run;
    %end;
    
    %* Table for continuous factors;
    %if (&contlist ^= ) %then %do;
    proc report data=&outds nowd ls=100
        style(report)={borderwidth=3 bordercolor=black cellpadding=3
            font_size=11pt font_face=Times  FONTSTYLE= ROMAN}
        style(lines)={background=white foreground=black
            font_size=9pt font_face=Times FONTSTYLE= ROMAN
            protectspecialchars=off}        
        style(column)={background=white foreground=black
            font_size=11pt font_face=Times FONTSTYLE= ROMAN
            font_weight=medium}     
        style(header)={background=white foreground=black borderbottomstyle=double
            font_weight=bold FONTSTYLE= ROMAN
            font_size=11pt font_face=Times};
        
        where _type=2;
        
        column (Factor N Statistics pvalue _p);
        
        %* Title;  
        compute before _PAGE_ /style={font_weight=bold font_size=11pt
                              just=left borderbottomwidth=3
                              borderbottomcolor=black bordertopcolor=white};
            %if (&TblTitle.^= ) %then %do; line &TblTitle. " (continuous variables)"; %end;
            %if (&TblTitle.= ) %then %do; 
                line "Univariate relationships between &outcome and continuous variables of interest"; 
            %end;
        endcomp;
    
        %* Variable name column;
        define Factor/order=data left
                    style(header)={just=left}
                    style(column)={cellwidth=&cwidth1. font_weight=bold};
        
        %* Count column;
        define N/display "N" style(column)={just=center cellwidth=&cwidth2.};
            
        %* Estimate column;
        define Statistics/display "rho (95% CI)" style(column)={just=center cellwidth=&cwidth3.};
        
        %* p-value column;
        define pvalue/display noprint;
        define _p/computed  "p-value"
                  format=pvaluef.
                  style(header)={just=center}
                  style(column)={just=center cellwidth=85};
        compute _p;
            _p=pvalue;
            if _p < 0.05 then call define(_col_,
            "style", "style=[font_weight=bold fontstyle=italic]");
        endcomp;
            
        %*Add Footnote;
        compute after/style(lines)={font_size=9pt
               just=left borderbottomcolor=white
               bordertopcolor=black bordertopwidth=3};
              line 
                %if %cmpres(%upcase("&corrtype"))="P" %then %do;                
                      "rho: Pearson's correlation; CI: confidence interval"
                %end; 
                %else %if %cmpres(%upcase("&corrtype"))="S" %then %do;              
                      "rho: Spearman's correlation; CI: confidence interval"
                %end; ;                               
        endcomp;    
    run;
    %end;
    
%end; %* end of if ntable =2;

*******************************************************************;
*** Close ODS destinations                                         ;
*******************************************************************;    
%if (&RTFFILE^= ) %then %do;
    ods rtf close;
%end;  %* end to if rtffile^= ; 

*******************************************************************;
*** Close macro                                                    ;
*******************************************************************;    
%* if subset then delete db;
%if (&subset^= ) %then %do;
    proc datasets lib=work nolist;
        delete _subset;
    run;
    quit;
%end; %* end to if subset;

%* Shut off macro options;
%if &DEBUG=1 %then %do;
    options nomlogic nomlogicnest;
%end;

%* close errorflag=0 loop;
%end; %* end to if errorflag=0;

%mend ctable;
