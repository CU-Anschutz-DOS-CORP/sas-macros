/**
 @file
 @brief Applies labels and formats
 
 @details 
 @par Purpose
 This macro reads in a data dictionary and applies labels and formats to 
 variables in a SAS data file

 @author Rocio Lopez

 @date 11-21-2014
 
 @version SAS 9.4
 
 @par Usage
 A call to this macro has some or all of these parameters (defaults given):
 @code{.sas}
 %labelandformat(
	 data=
	,newData=
	,dictionary=
	,guessingRows=all
	,reOrder=Y
	);
 @endcode

 @returns
 A SAS data set with labeled and formatted variables. The results window will
 show the contents of the new data set.
 
 @note 
 All variables in data set should be included with a label but the format 
 is optional
 
 @param data SAS data file containing the variables to be labeled and formatted

 @param newData Name of new SAS data file with labels and formats
 @n If left blank, original data set (DATA) will be replaced

 @param dictionary CSV file containing the data dictionary;
 no quotes and no extension (e.g. ~/userid/dataDictionary)
 @n The file must have the following columns:
 @n Name   = names of variables in the data file
 @n Label  = labels to be assigned to said variables
 @n Format = formats to be assigned to said variables (do not include . at end)
  
 @param guessingRows Specifies the number of rows of the dictionary file 
 to scan to determine the appropriate length for the labels
 @ Default is ALL
 
 @param reOrder Re-order variables based on dictionary file
 @ Allowed options are:
 @n 0, no, N for No, use original order
 @n 1, yes, Y for Yes, re-order based on data dictionary order       
  
 @par Example(s)
 @code{.sas}
 %labelandformat(
    data = sashelp.class
   ,newData = class
   ,dictionary = test/myDataDictionary
   );
 @endcode
 
 @par Revision History
 none
**/

%macro labelAndFormat(
     data=
    ,newData=
    ,dictionary=
    ,guessingRows=all
    ,reOrder=Y
    ) / minoperator;

    /*If no newds given, replace original data*/
    %if (&newData= ) %then %let newds=&data;

    /*Bring in Data Dictionary*/
    proc import out=names
        datafile="&dictionary..csv"
        dbms=csv replace;
        guessingrows=&guessingrows;
    run;

    /*Lists of names, labels and formats*/
    %local I Count;

    proc sql noprint;
        select left(trim(Name)), left(trim(Label)), left(trim(Format))
        into  :Name1   - :Name9999,
              :Label1  - :Label9999,
              :Format1 - :Format9999
        from names;
        %let Count = &SqlObs.;
        quit;

    /*Modify Data Set*/
    data &newData;
        attrib
            %do I = 1 %to &Count.;
                &&Name&I label = "&&Label&I" 
                    %if (&&Format&I^= ) %then %do;
                        format = &&Format&I...
                    %end;
            %end;
        ;
        set &data;
    run;
    
    /*Reorder if requested*/
    %if %upcase(&reorder) in (Y YES 1) %then %do;
        proc sql noprint; 
            select name into :order separated by ' '
            from names;
        
        data &newData;
            retain &order;
            set &newData;
        run; 
    %end;
    
    /*Print data contents*/
    proc contents data=&newData varnum; run;

    /*Delete data dictionary*/
    proc datasets library=work nolist;
        delete names;
    run;
    quit;

%mend labelAndFormat;
