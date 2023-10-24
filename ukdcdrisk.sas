/**
 @file
 @brief Calculates the UK DCD Risk Score
 
 @details 
 @par Purpose
 This macro runs within a DATA step and calculates the UK DeathCD risk 
 score which was developed to define futility in donation-after-
 circulatory-death liver transplantation
 
 @author Rocio Lopez

 @date 08-18-2023
 
 @version SAS 9.4 or later
 
 @par Usage
 A call to this macro has all or some of these parameters:
 @code{.sas}
 %ukdcdrisk(
	 don_age =
	,don_bmi = 
	,wit =
	,cit = 
	,rec_age = 
	,rec_meld = 
	,rec_priorLt =
	,prefix = 
 );

 @endcode

 @returns
 A column named ukDcdRs will be added to the data set being created.
 
 @param don_age Variable containing the donor's age in years

 @param don_bmi Variable containing the donor's BMI
 
 @param don_wit Variable containing the donor warm ischemic time in minutes
 
 @param cit Variable containing the total cold ischemic time in minutes

 @param rec_age Variable containing the recipient's age in years
 
 @param rec_meld Variable containing the recipient's MELD score at time of transplant

 @param rec_priorLt Variable indicating whether the recipient has had a prior
 liver transplant
 @n This must be coded as 1 = yes and 0 = no
 
 @param prefix Optional prefix for meld score variable names

 @note 
 @parblock
 @li This macro will only run from within a data step.

 @li The UK DCD risk score was developed to define futility in 
 donation-after-circulatory-death liver transplantation

 @li Reference article: Schlegel A, Kalisvaart M, Scalera I, Laing RW, Mergental H, 
 Mirza DF, Perera T, Isaac J, Dutkowski P, Muiesan P. The UK DCD Risk Score: A new 
 proposal to define futility in donation-after-circulatory-death liver transplantation. 
 J Hepatol. 2018 Mar;68(3):456-464. doi: 10.1016/j.jhep.2017.10.034. Epub 2017 Nov 15. 
 PMID: 29155020.
 
 @par Example(s)
 @code{.sas}

 **Using examples on Table 3 of Journal of Hepatology 2018 vol. 68 j 456–464
   Note that the score in last line of the table should read 18 and not 19;
   
 title "UK DCD Risk Score";
 data ukDCDrisk;
 	retain DON_TY 'C' DON_NON_HR_BEAT 'Y';
	input REC_priorLT REC_MELD REC_AGE CIT DON_WIT DON_AGE DON_BMI;
	
	if DON_TY = 'C' and DON_NON_HR_BEAT = 'Y' then do;
		%ukdcdrisk(
			 don_age = DON_AGE
			,don_bmi = DON_BMI
			,don_wit = DON_WIT
			,cit = CIT
			,rec_age = REC_AGE 
			,rec_meld = REC_MELD
			,rec_priorLt = REC_PRIORLT
		);
	end;
	
	label ukDCDrs = "UK DCD Risk Score";

 datalines;
 0 20 45 7 25 50 20
 0 30 45 5 35 50 20
 0 30 65 7 25 50 20
 0 30 65 7 25 65 20
 0 30 45 7 35 65 20
 0 30 65 7 35 65 20
 1 30 45 5 15 50 20
 1 30 45 7 25 65 20
 ;
 run;

 proc print data=ukDCDrisk; *label;
 run; title;

 @endcode
 
 @par Revision History
 n/a
**/

%macro ukdcdrisk(
	 don_age = 
	,don_bmi = 
	,don_wit =
	,cit = 
	,rec_age = 
	,rec_meld = 
	,rec_priorLt =
	,prefix = 
);

	if nmiss(&DON_AGE, &DON_BMI, &DON_WIT, &CIT, &REC_AGE, &REC_MELD, &REC_PRIORLT) = 0
		then &prefix.ukDCDrs = 
			(2 * (&DON_AGE gt 60)) + 
			(3 * (&DON_BMI gt 25)) + 
			(3 * (20 lt &DON_WIT le 30)) + 
			(6 * (&DON_WIT gt 30)) + 
			(2 * (&CIT gt 6)) + 
			(3 * (&REC_AGE gt 60)) + 
			(2 * (&REC_MELD gt 25)) + 
			(9 * (&REC_PRIORLT = 1));

%mend ukdcdrisk;
