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
    donType = 
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
   ,scalingFactor =
   ,kdpiInformat =
   ,prefix = 
 );
 @endcode

 @returns
 The following columns will be added to the data set being created:
 kdri_rao, kdri_med, and kdpi
 
 @param donType Variable specifying donor type coded as C for cadaveric
  or L for living

 @param age Variable containing donor age

 @param hgt_cm Variable containing donor height in centimeters

 @param wgt_kg Variable containing donor weight in kilograms

 @param black Variable indicating whether the donor is of Black race
 @n This must be coded as 1 = black and 0 = other race

 @param htn Variable indicating whether donor has history of hypertension
 @n This must be coded as 1 = yes and 0 = no

 @param diabetes Variable indicating whether donor has history of diabetes
 @n This must be coded as 1 = yes and 0 = no

 @param codCVA Variable indicating whether the donor died due to CVA or no
 @n This must be coded as 1 = CVA and 0 = other cause of death

 @param creatinine Variable containing donor serum creatinine in mg/dL

 @param hcv Variable indicating whether the donor is HCV positive
 @n This must be coded as 1 = yes and 0 = no

 @param dcd Variable indicating the donor DCD status
 @n This must be coded as 1 = DCD and 0 = DBD

 @scalingFactor A single number representing the scaling factor for converting KDRI RAO 
 to scaled KDRI
 @n This is obtained from the KDPI mapping table for the reference population 
 of choice.

 @kdpiInformat The name of the informat that contains the KDPI mapping table for the
 reference population of choice.
 
 @param prefix Optional prefix for variable names
 @n For example, if prefix = d_ then the 3 variables created will be d_kdpi_rao d_kdpi_med d_kdpi

 @note 
 @parblock
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

 @li Guide to calculating KDPI: https://optn.transplant.hrsa.gov/media/j34dm4mv/kdpi_guide.pdf

 @li Online KDPI calculator: https://optn.transplant.hrsa.gov/data/allocation-calculators/kdpi-calculator/
 @n Note that the calculator uses the prior year as the reference population (e.g. if you use it in 2023,
 it uses 2022 as the reference population)

 @li Archive of KDPI Mapping Tables: https://optn.transplant.hrsa.gov/data/allocation-calculators/archived-allocation-calculator-resources

 @par Example(s)
 @code{.sas}

 **Ref pop used in example: All kidney donors recovered during 2022;
 **https://optn.transplant.hrsa.gov/media/wnmnxxzu/kdpi_mapping_table.pdf;

**Import 2022 KDPI mapping informat;
 %include "./optnMappingTables/kdpi_mapping_2022.sas";

 **Input data and calculate KDPI;
 title "KDPI";
 data kdpi;
    retain d_type 'C';
    input d_age d_hgt_cm d_wgt_kg d_black d_htn d_diabetes d_codCVA d_scr d_hcv d_dcd;

    %kdpi(
         donTyp        = d_type
        ,age           = d_age
        ,hgt_cm        = d_hgt_cm
        ,wgt_kg        = d_wgt_kg
        ,black         = d_black
        ,htn           = d_htn
        ,diabetes      = d_diabetes
        ,codCVA        = d_codCVA
        ,creatinine    = d_scr
        ,hcv           = d_hcv
        ,dcd           = d_dcd
        ,scalingFactor = 1.33586831546044
        ,kdpiInformat  = inKdpi22F.
        ,prefix = d_
    );
    
    label d_kdpi_rao = "Donor KDPI RAO"
          d_kdpi_med = "Donor KDPI Median"
          d_kdpi = "Donor KDPI (%)";

    datalines;
 52 183 81 0 1 0 1 1.7 0 1
 ;
 run;

 proc print data=kdpi; *label;
 run; title;

 @endcode
 
 @par Revision History
 n/a
**/

%macro kdpi(
     donType = 
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
    ,scalingFactor =
    ,kdpiInformat =
    ,prefix = 
);
    options fmterr; **This will generate an error if the stated format does not exist;
    if &donType = 'C' and 
        nmiss(&age, &hgt_cm, &wgt_kg, &black, &htn, &diabetes, &codCVA, &creatinine, &hcv, &dcd) = 0 
        then do;
        &prefix.kdri_rao = 
            exp(
                (0.0128 * (&age - 40)) -
                (0.0194 * (&age - 18) * (&age < 18)) +
                (0.0107 * (&age - 50) * (&age > 50)) -
                (0.0464 * divide(&hgt_cm - 170, 10)) -
                (0.0199 * divide(&wgt_kg - 80, 5) * (&wgt_kg < 80)) +
                (0.1790 * &black) +
                (0.1260 * &htn) +
                (0.1300 * &diabetes) +
                (0.0881 * &codCVA) +
                (0.2200 * (&creatinine - 1)) -
                (0.2090 * (&creatinine - 1.5) * (&creatinine > 1.5)) +
                (0.2400 * &hcv) +
                (0.1330 * &dcd)
            );
        
        &prefix.kdri_med = divide(&prefix.kdri_rao, &scalingFactor);

        &prefix.kdpi = input(&prefix.kdri_med, &kdpiInformat);
    end;
    options nofmterr;

%mend kdpi;
