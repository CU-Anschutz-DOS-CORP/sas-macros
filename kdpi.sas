/**
 @file
 @brief Calculates KDPI
 
 @details 
 @par Purpose
 This macro runs within a DATA step and calculates the Kidney Donor Profile Index (KDPI)
 
 @author Rocio Lopez

 @date 07-21-2023
 
 @version SAS 9.4 or later
 
 @par Usage
 A call to this macro has all or some of these parameters:
 @code{.sas}
 %kdpi(
    version = 2024
   ,donType = 
   ,age =
   ,hgt_cm =
   ,wgt_kg =
   ,black =
   ,htn =
   ,diabetes =
   ,codCVA =
   ,creatinine =
   ,hcv =
   ,dcd =
   ,htnRefProp = 
   ,dmRefProp = 
   ,scalingFactor =
   ,kdpiInformat =
   ,prefix = 
 );
 @endcode

 @returns
 The following columns will be added to the data set being created:
 kdri_rao, kdri_med, and kdpi
 
 @param version Formula version used in calculation
 @n Allowed options are 2024 for current formula (DEFAULT) and 2013 to use the prior formula

 @param donType Variable specifying donor type coded as C for cadaveric
  or L for living

 @param age Variable containing donor age

 @param hgt_cm Variable containing donor height in centimeters

 @param wgt_kg Variable containing donor weight in kilograms

 @param black Variable indicating whether the donor is of Black race
 @n This variable is only used by the 2013 formula and must be
 coded as 1 = black and 0 = other race

 @param htn Variable indicating whether donor has history of hypertension
 @n This must be coded as 1 = yes and 0 = no

 @param diabetes Variable indicating whether donor has history of diabetes
 @n This must be coded as 1 = yes and 0 = no

 @param codCVA Variable indicating whether the donor died due to CVA or no
 @n This must be coded as 1 = CVA and 0 = other cause of death

 @param creatinine Variable containing donor serum creatinine in mg/dL

 @param hcv Variable indicating whether the donor is HCV positive
 @n This variable is only used by the 2013 formula and must be
 coded as 1 = yes and 0 = no

 @param dcd Variable indicating the donor DCD status
 @n This must be coded as 1 = DCD and 0 = DBD

 @param htnRefProp A single number between 0 and 1 representing the proportion of donors 
 in the reference population having a history of hypertension
 @n This is obtained from the KDPI mapping table for the reference population 
 of choice.

 @param dmRefProp A single number between 0 and 1 representing the proportion of donors 
 in the reference population having a history of diabetes
 @n This is obtained from the KDPI mapping table for the reference population 
 of choice.

 @param scalingFactor A single number representing the scaling factor for converting KDRI RAO 
 to scaled KDRI
 @n This is obtained from the KDPI mapping table for the reference population 
 of choice.

 @param kdpiInformat The name of the informat that contains the KDPI mapping table for the
 reference population of choice.
 
 @param prefix Optional prefix for variable names
 @n For example, if prefix = d_ then the 3 variables created will be d_kdpi_rao d_kdpi_med d_kdpi

 @note 
 @parblock
 @li On Oct. 31, 2024, the OPTN implemented a new policy revising the KDPI. The formula was
 updated by removing race and HCV. The score components for the remaining deceased donor 
 characteristics which make up the KDPI were adjusted to compensate for the removal of 
 those two variables.
 @n https://optn.transplant.hrsa.gov/news/race-and-hepatitis-c-status-no-longer-included-in-calculation-used-to-estimate-deceased-donor-kidney-function-policy-change-anticipated-to-increase-equity/
 @n https://optn.transplant.hrsa.gov/media/0l3mligh/mac_refit-kdpi_-june-2024_pn.pdf

 @li This macro will only run from within a data step.

 @li KDPI is only calcualted for cadaveric donors donTyp='C'

 @li The user must have defined an informat with the KDPI mapping table of choice.

 @li KDRIrao (kdri_rao) is interpreted as the relative risk of post-transplant graft failure
 for this donor compared to a reference donor (age=40 years, non-African American, etc.) as 
 defined in Rao, et al.

 @li KDRImedian (kdri_med) is interpreted as the relative risk of post-transplant graft failure 
 (in an average, adult recipient) for this donor compared to the median kidney donor recovered 
 in the reference population year. 

 @li KDRI is a measure of relative risk of post-transplant graft failure in a cumulative 
 percentage scale.

 @li Example interpretation: The estimated risk of kidney graft failure from this donor is higher 
 than <KDPI>% of kidney donors recovered in <reference population year> and <KDPImedian> times 
 that of the median donor recovered in <reference population year>. 

 @li Creatinine values that are outside of the range 0.01 to 40 will be set to missing. Values 
 that are between 10 and 40 may be correct but are questionable, and the user should double 
 check the values before using this macro. Values greater than 8 are capped at 8 for 
 calculating KDRI.

 @li Donor age is restricted to be between 0 and 99. 

 @li Height is limited to a maximum of 7’11” (241.3 cm), and the weight must be between 
 1 lb (0.454 kg) and 650 lb (294 kg).

 @li If history of HTN is unknown, the KDRI calculation assumes that the donor has a probability of 
 having been hypertensive equal to the proportion of donors in the reference population having 
 a history of hypertension. The user must get this prevalence value from the KDPI mapping 
 table for the reference population of choice. 

 @li If history of diabetes is unknown, the KDRI calculation assumes that the donor has a probability of 
 having been diabetic equal to the proportion of donors in the reference population having 
 a history of diabetes. The user must get this prevalence value from the KDPI mapping 
 table for the reference population of choice. 

 @li Guide to calculating KDPI: https://optn.transplant.hrsa.gov/media/j34dm4mv/kdpi_guide.pdf

 @li Online KDPI calculator: https://optn.transplant.hrsa.gov/data/allocation-calculators/kdpi-calculator/
 @n Note that the calculator uses the prior year as the reference population (e.g. if you use it in 2024,
 it uses 2023 as the reference population)

 @li If you are using the 2024 formula please make sure that you use mapping tables created with
 OPTN data as of 09/2024. Currently the one available is using the 2023 reference population 

 @li Most recent KDPI Mapping Table: https://optn.transplant.hrsa.gov/media/wnmnxxzu/kdpi_mapping_table.pdf

 @li Archive of KDPI Mapping Tables: https://optn.transplant.hrsa.gov/data/allocation-calculators/archived-allocation-calculator-resources

 @par Example(s)
 @code{.sas}

 **Ref pop used in example: All kidney donors recovered during 2023;
 **https://optn.transplant.hrsa.gov/media/wnmnxxzu/kdpi_mapping_table.pdf;

**Import 2023 KDPI mapping informat;
 %include "./optnMappingTables/kdpi_mapping_2023.sas";

 **Input data and calculate KDPI;
 title "KDPI";
 data kdpi;
    retain d_type 'C';
    input d_age d_hgt_cm d_wgt_kg d_htn d_diabetes d_codCVA d_scr d_dcd;

    %kdpi(
         version       = 2024
        ,donType       = d_type
        ,age           = d_age
        ,hgt_cm        = d_hgt_cm
        ,wgt_kg        = d_wgt_kg
        ,black         = 
        ,htn           = d_htn
        ,diabetes      = d_diabetes
        ,codCVA        = d_codCVA
        ,creatinine    = d_scr
        ,hcv           = 
        ,dcd           = d_dcd
        ,htnRefProp    = 0.3812
        ,dmRefProp     = 0.1491
        ,scalingFactor = 1.30900852563932
        ,kdpiInformat  = inKdpi23F.
        ,prefix = d_
    );
    
    label d_kdpi_rao = "Donor KDPI RAO"
          d_kdpi_med = "Donor KDPI Median"
          d_kdpi = "Donor KDPI (%)";

    datalines;
 52 183 81 1 0 1 1.7 1
 52 183 81 . 0 1 10 1
 ;
 run;

 proc print data=kdpi; *label;
 run; title;

 @endcode
 
 @par Revision History
 @b 11-26-2024 Incorporated handling of unknown hypertension or diabetes status and extreme 
 values of creatinine, age, height, and weight as per OPTN policy.
 @n @b 11-15-2024 Added 2024 formula
**/

%macro kdpi(
     version = 
    ,donType = 
    ,age =
    ,hgt_cm =
    ,wgt_kg =
    ,black =
    ,htn =
    ,diabetes =
    ,codCVA =
    ,creatinine =
    ,hcv =
    ,dcd =
    ,htnRefProp = 
    ,dmRefProp = 
    ,scalingFactor =
    ,kdpiInformat =
    ,prefix = 
);
    options fmterr; **This will generate an error if the stated format does not exist;

    /* Set bounds as per policy */
    if not missing(&creatinine) then scrForKDPI = min(max(&creatinine, 0.01), 40);
    if scrForKDPI gt 8 then scrForKDPI = 8;
    
    if not missing(&age) then ageForKDPI = min(max(&age, 0), 99);

    if not missing(&hgt_cm) then hgtForKDPI = min(&hgt_cm, 241.3);

    if not missing(&wgt_kg) then wgtForKDPI = min(max(&wgt_kg, 0.454), 294);

    if missing(&htn) then htnForKDPI = &htnRefProp;
    else if &htn in (0 1) then htnForKDPI = &htn;

    if missing(&diabetes) then dmForKDPI = &dmRefProp;
    else if &diabetes in (0 1) then dmForKDPI = &diabetes;

    %if &version = 2013 %then %do;
        if &donType = 'C' and 
            nmiss(ageForKDPI, hgtForKDPI, wgtForKDPI, &black, htnForKDPI, dmForKDPI, &codCVA, scrForKDPI, &hcv, &dcd) = 0 
            then do;
            &prefix.kdri_rao = 
                exp(
                      (0.0128 * (ageForKDPI - 40))
                    - (0.0194 * (ageForKDPI - 18) * (ageForKDPI < 18))
                    + (0.0107 * (ageForKDPI - 50) * (ageForKDPI > 50))
                    - (0.0464 * divide(hgtForKDPI - 170, 10))
                    - (0.0199 * divide(wgtForKDPI - 80, 5) * (wgtForKDPI < 80))
                    + (0.1790 * &black)
                    + (0.1260 * htnForKDPI)
                    + (0.1300 * dmForKDPI)
                    + (0.0881 * &codCVA)
                    + (0.2200 * (scrForKDPI - 1))
                    - (0.2090 * (scrForKDPI - 1.5) * (scrForKDPI > 1.5))
                    + (0.2400 * &hcv)
                    + (0.1330 * &dcd)
                );
        end;
    %end;

    %else %if &version = 2024 %then %do;
        if &donType = 'C' and 
            nmiss(ageForKDPI, hgtForKDPI, wgtForKDPI, htnForKDPI, dmForKDPI, &codCVA, scrForKDPI, &dcd) = 0 
            then do;
            &prefix.kdri_rao = 
                exp(
                      (0.0092 * (ageForKDPI - 40))
                    + (0.0113 * (ageForKDPI - 18) * (ageForKDPI < 18))
                    + (0.0067 * (ageForKDPI - 50) * (ageForKDPI > 50))
                    - (0.0557 * divide(hgtForKDPI - 170, 10))
                    - (0.0333 * divide(wgtForKDPI - 80, 5) * (wgtForKDPI < 80))
                    + (0.1106 * htnForKDPI)
                    + (0.2577 * dmForKDPI)
                    + (0.0743 * &codCVA)
                    + (0.2128 * (scrForKDPI - 1))
                    - (0.2199 * (scrForKDPI - 1.5) * (scrForKDPI > 1.5))
                    + (0.1966 * &dcd)
                );
        end;
    %end;

    &prefix.kdri_med = divide(&prefix.kdri_rao, &scalingFactor);
    &prefix.kdpi = input(&prefix.kdri_med, &kdpiInformat);

    drop scrForKDPI ageForKDPI hgtForKDPI wgtForKDPI htnForKDPI dmForKDPI ;

    options nofmterr;

%mend kdpi;
