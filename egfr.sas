/**
 @file
 @brief Calculates eGFR for adults
 
 @details 
 @par Purpose
 This macro runs within a DATA step and calculates eGFR for adults using the 
 2009 and 2021 CKD-EPI equations
 
 @author Rocio Lopez

 @date 07-21-2023
 
 @version SAS 9.4 or later
 
 @par Usage
 A call to this macro has all or some of these parameters:
 @code{.sas}
 %macro egfr(
  age = 
 ,female = 
 ,creatinine = 
 ,black =
 ,prefix = 
);
 @endcode

 @returns
 The following columns will be added to the data set being created:
 eGFR09 eGFR21
 
 @param age Variable containing age of the subject

 @param female Variable indicating whether the candidate is female
 @n This must be coded as 1 = female and 0 = male

 @param creatinine Variable containing serum creatinine in mg/dL

 @param black Variable indicating whether the donor is of Black race
 @n This must be coded as 1 = black and 0 = other race

 @param prefix Optional prefix for variable names
 @n For example, if prefix = d_ then the 2 variables created will be d_eGFR09 d_eGFR21

 @note 
 @parblock
 @li This macro will only run from within a data step.

 @li eGFR will only be calculated if age, female, creatinine, and race (for 20009 equation) are not missing,
 age is 18 or older, and creatinine level is above 0

 @li CKD-EPI Creatinine Equation (2021) references:
 @n https://www.kidney.org/content/ckd-epi-creatinine-equation-2021
 @n https://www.kidney.org/professionals/kdoqi/gfr_calculator

 @li CKD-EPI Creatinine Equation (2009) references:
 @n https://www.niddk.nih.gov/health-information/professionals/clinical-tools-patient-management/kidney-disease/laboratory-evaluation/glomerular-filtration-rate-calculators/historical
 @n https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2763564/

 @par Example(s)
 @code{.sas}
 
 title "eGFR";
 data egfr;
	input age female scr black;
	
	%egfr(
		 age        = age
		,female     = female 
		,creatinine = scr
		,black      = black
	);
		
	label egfr09 = "eGFR (2009 CKD-EPI formula)"
		  egfr21 = "eGFR (2021 CKD-EPI formula)";

 datalines;
 56 1 6.00 0
 69 0 8.05 0
 49 0 5.55 1
 34 1 2.80 1
 ;
 run;

 proc print data=egfr; *label;
 run; 
 title;

 @endcode
 
 @par Revision History
 n/a
**/

%macro egfr(
	 age = 
	,female = 
	,creatinine = 
	,black =
	,prefix = 
);

    /* 2009 Equation */
    if nmiss(&age, &female, &creatinine, &black) = 0 
        and &age ge 18 and &creatinine gt 0 then do;
        if &female = 0 then &prefix.eGFR09 = 
        	141 * 
        	((min(divide(&creatinine, 0.9), 1))**-0.411) *
        	(max(divide(&creatinine, 0.9), 1)**-1.209) *
        	(0.993**&age) ;
        else if &female = 1 then  &prefix.eGFR09 = 
        	141 * 
        	((min(divide(&creatinine, 0.7), 1))**-0.329) *
        	(max(divide(&creatinine, 0.7), 1)**-1.209) *
        	(0.993**&age) *
        	1.018  ; 
        if black = 1 then  &prefix.eGFR09 =  &prefix.eGFR09 * 1.159; 
    end;

    /* 2021 Equation */
    if nmiss(&age, &female, &creatinine) = 0 
        and &age ge 18 and &creatinine gt 0 then do;
        if &female = 0 then &prefix.eGFR21 = 
        	142 * 
        	((min(divide(&creatinine, 0.9), 1))**-0.302) *
        	(max(divide(&creatinine, 0.9), 1)**-1.2) *
        	(0.9938**&age); 
        else if &female = 1 then &prefix.eGFR21 = 
        	142 * 
        	((min(divide(&creatinine, 0.7), 1))**-0.241) *
        	(max(divide(&creatinine, 0.7), 1)**-1.2) *
        	(0.9938**&age) *
        	1.012; 
    end;

%mend egfr;
