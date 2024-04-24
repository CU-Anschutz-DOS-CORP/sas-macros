/**
 @file
 @brief Calculates MELD, MELD-Na, and MELD 3.0
 
 @details 
 @par Purpose
 This macro runs within a DATA step and calculates Model For End-Stage Liver 
 Disease (MELD), MELD-Na, and MELD 3.0 scores
 
 @author Rocio Lopez

 @date 07-14-2023
 
 @version SAS 9.4 or later
 
 @par Usage
 A call to this macro has all or some of these parameters:
 @code{.sas}
 %meld(
    ageAtListing = 
   ,female = 
   ,bilirubin = 
   ,sodium = 
   ,inr =
   ,creatinine = 
   ,albumin =
   ,dialysis =
   ,prefix = 
   ,cap = 1
 );
 @endcode

 @returns
 The following columns will be added to the data set being created:
 meld, meldNa, meld3
 
 @param ageAtListing Variable containing age at time of liver transplant listing

 @param female Variable indicating whether the candidate is female
 @n This must be coded as 1 = female and 0 = male

 @param bilirubin Variable containing bilirubin in mg/dL
 
 @param sodium Variable containing serum sodium in mEq/L

 @param inr Variable containing INR

 @param creatinine Variable containing serum creatinine in mg/dL

 @param albumin Variable containing albumin in g/dL

 @param dialysis Variable indicating whether the candidate received two or more 
 dialysis treatments within 7 days prior to the serum creatinine test or received 
 24 hours of continuous veno-venous hemodialysis (CVVHD) prior to the serum creatinine test
 @n This must be coded as 1 = yes and 0 = no
 
 @param prefix Optional prefix for meld score variable names
 @n For example, if prefix = r_ then the 3 variables created will be r_meld r_meldNa r_meld3

 @param cap Requests that the scores be capped between 6-40 as per OPTN policy
 @n Allowed options are:
 @n 0 = No, calcuate actual score without capping 
 @n 1 = Yes, score will be capped betwee 6 and 40 (DEFAULT)

 @note 
 @parblock
 @li This macro will only run from within a data step.
 
 @li If dialysis is missing, calculations will assume no dialysis.

 @li OPTN policy used MELD (meld) prior to 01/2016, MELD-Na (meldNa) from 01/??/2016 to 07/12/2023, 
 and MELD 3.0 starting on 7/13/2023

 @li Notice of OPTN Policy Change (MELD-Na to MELD 3.0: 
  https://optn.transplant.hrsa.gov/media/3idbp5vq/policy-guid-change_impr-liv-alloc-meld-peld-sta-1a-sta-1b_liv.pdf
 
 @li Reference for current policy using MELD 3.0 (as of 7/13/2023)
 @n https://optn.transplant.hrsa.gov/media/eavh5bf3/optn_policies.pdf (section 9.1.D)
 @n https://optn.transplant.hrsa.gov/media/qmsdjqst/meld-peld-calculator-user-guide.pdf
 @n https://optn.transplant.hrsa.gov/data/allocation-calculators/meld-calculator/

 @li MELD 3.0: Kim WR, Mannalithara A, Heimbach JK, Kamath PS, Asrani SK, Biggins SW, Wood NL, 
 Gentry SE, Kwong AJ. MELD 3.0: The Model for End-Stage Liver Disease Updated for the Modern Era. 
 Gastroenterology. 2021 Dec;161(6):1887-1895.e4. doi: 10.1053/j.gastro.2021.08.050. 
 Epub 2021 Sep 3. PMID: 34481845; PMCID: PMC8608337.

 @li The lower limit of Serum Sodium (Na) is capped at 125, and the upper limit is capped at 137.
 @li The upper limit of serum creatinine is capped at 4; in addition, if the patient had dialysis at least 
 twice in the past week, the value for serum creatinine will be automatically adjusted to 4.0
 
 @par Example(s)
 @code{.sas}

 **Using examples on Table 4 of Gastroenterology, 2021-12-01, Volume 161, Issue 6, Pages 1887-1895.e4;
 title "MELD Scores";
 data meld;
	input age_at_listing female bilirubin sodium inr scr albumin dialysis;
	
	%meld( ageAtListing = age_at_listing 
		  ,female = female
		  ,bilirubin = bilirubin
		  ,sodium = sodium
		  ,inr = inr
		  ,creatinine = scr
		  ,albumin = albumin
		  ,dialysis = dialysis
		  ,prefix = r_);
	
	label r_meld = "Recipient MELD at time of transplant"
		  r_meldNa = "Recipient MELDNa at time of transplant"
		  r_meld3 = "Recipient MELD 3.0 at time of transplant";

 datalines;
 20 0 2.5 131 1.0 1.2 3.8 0
 20 1 2.5 131 1.0 1.2 3.8 0
 20 0 6.0 131 1.5 1.5 3.5 0
 20 0 6.0 131 1.5 1.5 2.2 0
 20 1 6.0 131 1.5 1.5 2.2 0
 20 0 12.0 128 2.2 1.8 2.0 0
 20 0 12.0 128 2.2 2.8 2.0 0
 20 1 12.0 128 2.2 2.8 2.0 0

 20 1 12.0 120 0.9 4.5 3.5 0
 10 1 12.0 120 0.9 4.5 3.5 0
 20 0 12.0 120 0.9 4.5 3.5 0
 10 0 12.0 120 0.9 4.5 3.5 0
 ;
 run;

 proc print data=meld; *label;
 run; title;

 @endcode
 
 @par Revision History
 @b 03-14-2024 Added CAP option to allow control on whether 
 scores are capped between 6 and 40 or not
**/

%macro meld(
	 ageAtListing = 
	,female = 
	,bilirubin = 
	,sodium = 
	,inr =
	,creatinine = 
	,albumin =
	,dialysis =
	,prefix = 
    ,cap = 1
);

    informat biliForMeld inrForMeld scrForMeld scrForMeld3 naForMeld albForMeld 
    &prefix.meld &prefix.meldNa &prefix.meld3 best.;

    if nmiss(&bilirubin, &sodium, &inr, &creatinine) = 0 then do;
    	/* Set bounds as per policy */
    	biliForMeld = max(&bilirubin, 1);
    	inrForMELD = max(&inr, 1);

        if &creatinine > 4 or &dialysis = 1 then scrForMeld = 4;
        else scrForMeld = max(&creatinine, 1);
 
        naForMeld = min(max(&sodium, 125), 137);

        /* Calculate MELD */
        &prefix.meld = 
        	round(
        			(0.957 * log(scrForMeld)) +
        			(0.378 * log(biliForMeld)) +
        			(1.120 * log(inrForMeld)) + 
        			0.643
        		, 0.1) * 10;
		
        /* Calcute MELD-Na */
    	if &prefix.meld gt 11 then &prefix.meldNa =
    			round(
    			&prefix.meld +
    			(1.32 * (137 - naForMeld)) -
    			(0.033 * &prefix.meld * (137 - naForMeld))
    			, 1);
    	else &prefix.meldNa = &prefix.meld;
    end; **end of meld, meldna calc;

    if nmiss(&ageAtListing, &female, &bilirubin, &sodium, &inr, &creatinine, &albumin) = 0 then do;
        /* Set SCr and albumin bounds as per policy */
        if &creatinine > 3 or &dialysis = 1 then scrForMeld3 = 3;
        else scrForMeld3 = max(&creatinine, 1);
        
        albForMeld = min(max(&albumin, 1.5), 3.5);

        /* Calculate MELD 3.0 */
        &prefix.meld3 =
                round(
                    (1.33 * &female * (&ageAtListing ge 18)) +
                    (4.56 * log(biliForMeld)) +
                    (0.82 * (137 - naForMeld)) -
                    (0.24 * (137 - naForMeld) * log(biliForMeld)) +
                    (9.09 * log(inrForMeld)) +
                    (11.14 * log(scrForMeld3)) +
                    (1.85 * (3.5 - albForMeld)) -
                    (1.83 * (3.5 - albForMeld) * log(scrForMeld3)) -
                    (1.33 * (&ageAtListing ge 18)) +
                    7.33,
                1);
    end; **end of meld3 calc;

    %if &cap = 1 %then %do;
        if &prefix.meld lt 6 then &prefix.meld = 6;
        else if &prefix.meld gt 40 then &prefix.meld = 40;

        if &prefix.meldNa lt 6 then &prefix.meldNa = 6;
        else if &prefix.meldNa gt 40 then &prefix.meldNa = 40;

        if &prefix.meld3 lt 6 then &prefix.meld3 = 6;
        else if &prefix.meld3 gt 40 then &prefix.meld3 = 40;

    %end;

	drop biliForMeld inrForMeld scrForMeld scrForMeld3 naForMeld albForMeld;
	
%mend meld;
