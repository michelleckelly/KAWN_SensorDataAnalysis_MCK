/* Program: sparrow.sas
   Date: 1/21/03
   Written by: Greg Schwarz

   Purpose: Header information for running the sparrow model. This is typically the
            only file that needs to be modified to run the model.
*/

/* Specify the results, data, program, and gis directories */
%let home_results = d:\Greg\sparrow\model\package\sparrow\results ; /* Location of results */
%let home_data = d:\Greg\sparrow\model\package\sparrow\data ; /* Location of data */
%let home_program = d:\Greg\sparrow\model\package\sparrow\master ; /* Location of program code */
%let home_gis = d:\Greg\sparrow\model\package\sparrow\gis ; /* Location of gis coverages */

/* Specify the name of the SAS input data set */
%let indata = SPARROW_DATA1 ;

/* Specify if the input data set (INDATA) is to be created prior to processing.
   If a previous model run (within the same SAS session) has created the input data 
   set and this data set does not require modification to do the current run, then 
   set this variable to no to speed processing. Note, the data set INDATA resides 
   in the WORK directory and is created by SPARROW from the data set identified in 
   the previous command (the "indata" specification).*/
%let if_make_input_data = yes ;

/* ******************************************************************** */
/* Specify bootstrap iterations and seeds                               */
/* ******************************************************************** */

/* Specify the number of bootstrap iterations to be performed */
%let n_boot_iter = 6 ; 
%let n_boot_iter = 0 ; 

/* Specify the starting iteration (enter 0 to do parametric estimation) */
%let start_iter = 0 ; 

/* Specify starting random seed iteration - make same as start_iter if restarting an 
   algorithm - will differ from start_iter if estimation for some jter iterations failed */
%let start_jter = 0 ; 

/* Specify the number of iterations to do in current run - leave blank to set equal to the 
   number of bootstrap iterations (n_boot_iter). */
%let end_iter = ;

/* Specify the number of extra seed values to generate (in case bootstrap algorithm fails to
   obtain good estimates for some initial seed values */
%let n_extra_jter = 20 ;
%let n_extra_jter = 0 ;

/* Specify the master seed */
%let master_seed = 6727775 ;

/* Specify the number of seeds required for each bootstrap iteration (at least 2, but too many
   is better than too few). */
%let n_seeds = 4 ;

/* Specify the coverage probability (IN PERCENT) for the confidence intervals */
%let cov_prob = 50 ;
%let cov_prob = 90 ;

/* ******************************************************************** */
/* Model Specification                                                  */
/* ******************************************************************** */

/* Specify the parameter list with initial values and lower and upper constrained values
   (enter . for an unconstrained bound). Format is: 
     Parameter_name Initial_value Lower_bound:Upper_bound */
%let betailst = bpoint 0.5 0:. batmdep 4.2 0:. bfertilizer 1.0 0:. bwaste 1.0 0:.
                bnonagr 15.0 0:.
                bperm  -0.0263 .:. bdrainden 0.05 .:. btemp -0.01 .:.
                brchdecay1 0.45 .:. brchdecay2 0.12 .:. brchdecay3 0.05 .:.
                bresdecay 6.5 0:.
                ;

/* Specify if the parametric calibration initial beta estimates are to be taken
   from the results of a previous model run. If the switch is set to yes, the program
   searches for the file summary_betaest in the home_results directory from which to 
   load these estimates. If this file of previous estimates is not found, SPARROW 
   uses the initial values specified in the betailst specification, regardless of how
   the switch is set. 
*/
%let if_init_beta_w_previous_est = no ;


/* Specify the variable and coefficient lists for the dependent and independent variables.
   For variables to be read from input data set, user must verify that the variable named 
   in the input data set corresponds to the name shown here, either by altering the column 
   name in the input data set, or by modifying this code. */

/* Dependent variable (specify the variable name in the input data set)  */
%let depvar = DEPVAR ;

/* Specify the units for the dependent variable, either kg/yr, mt/yr, or Bcol/yr.  Note:
   mt/yr signifies metric tons per year; Bcol/yr signifies billions of colonies per year. */
%let load_units = Bcol/yr ;
%let load_units = mt/yr ;
%let load_units = kg/yr ;

/* Specify if the concentration estimates are to be in micrograms per liter
   (as opposed to the default milligrams per liter). Note, if load_units is set
   to Bcol/yr, the concentration units are automatically set to col/100ml. */
%let if_concentration_in_micrograms = no ;

/* Source variables (specify the variable names in the input data set) */
%let srcvar = POINT ATMDEP FERTILIZER WASTE NONAGR ;

/* Source variable coefficients (define corresponding coefficient names) */ 
%let bsrcvar = bpoint batmdep bfertilizer bwaste bnonagr ;

/* Delivery variables (specify the variable names in the input data set) */
%let dlvvar = permave drainden temp ;

/* Delivery variable coefficients (define corresponding coefficient names) */ 
%let bdlvvar =  bperm bdrainden btemp ;

/* Specify the delivery design matrix: each row corresponds to a different
   source (in the same order as they are listed in the srcvar statement);
   each column corresponds to a different delivery variable (in the same 
   order as they are listed in the dlvvar statement). An element is either 
   a 0 or 1. Element r,c is a 1 if source variable r uses delivery variable c.
   Otherwise, element r,c is 0. A space separates columns and a comma separates rows. */
%let dlvdsgn =  0 0 0,
                1 1 1,
                1 1 1,
                1 1 1,
                1 1 1 ;

/* Specify if the delivery variables are to be mean adjusted (recommended). This
   improves the interpretability of the source coefficients and gives meaning to 
   the land-to-water delivery factor. */
%let if_mean_adjust_delivery_vars = yes ;

/* Stream decay variables (specify the variable names in the input data set that determine 
   the amount of reach decay - these variables are used in the SAS IML code (see below) to 
   define the reach decay function). */
%let decvar = rchdecay1 rchdecay2 rchdecay3 ;

/* Stream decay variable coefficients (define corresponding coefficient names). */
%let bdecvar = brchdecay1 brchdecay2 brchdecay3 ;

/* Reservoir decay variables (specify the variable names in the input data set that 
   determine the amount of decay in reservoirs - these variables are used in the 
   reservoir decay function defined in the data modifications section below). */
%let resvar = iresload  ;

/* Reservoir decay variable coefficients (define corresponding coefficient names). */
%let bresvar = bresdecay  ;

/* Other variables in the model (custom programmed in the calibrate and predict macros).
   To reference these coefficients in the IML code use the column identifier jNAME where
   NAME is the name listed in the bothvar list. Leave blank if no additional model variables
   are used. */
%let othvar = ;

/* Other coefficients in the model (custom programmed in the calibrate and predict macros).
   Note that these coefficients need not be associated with the variables listed in the
   othvar list above. To reference these coefficients in the IML code use the column
   identifier jNAME where NAME is the name listed in the bothvar list. Leave blank if no
   additional model variables are used. */
%let bothvar = ;

/* Specify the SAS IML reach decay function code. */
%let reach_decay_specification = exp(-data[,jdecvar]*beta[,jbdecvar]`) ;

/* Specify the SAS IML reservoir decay function code. */
%let reservoir_decay_specification = 1 / (1 + data[,jresvar] * beta[,jbresvar]`) ; 

/* Specify the SAS IML incremental delivery function code. */
%let incr_delivery_specification = exp((beta[,jbdlvvar] # data[,jdlvvar]) * dlvdsgn`) ;

/* ******************************************************************** */
/* Specify the names of additional input variables required for program */
/* execution. The user should verify that variable names in the SAS     */
/* input data set match the names shown here, either by altering the    */
/* name in the input data set, or by modifying the code that defines    */
/* input data set in the data modification section below.               */
/* ******************************************************************** */

/* Specify the name of the variable containing the SPARROW monitoring station 
   identifier (variable must be numeric) */
%let staid = staid ;

/* Specify the names of station information variables to be passed to the output
   files and listings. Helpful variables include, for example, the USGS monitoring
   station identifier and station name. Note that reach variables arcid, waterid, 
   drainage area and mean flow are automatically passed to the residuals file. 
   Leave blank if no additional variables are to be passed. */
%let optional_station_information = station_id station_name demtarea meanq 
   CULTIV PASTURE FOREST RANGE URBAN ;

/* Specify the name of the variable containing the monitoring station latitude 
   (latitude must be in decimal degrees) */
%let lat = lat ;

/* Specify the name of the variable containing the monitoring station longitude 
   (longitude must be in decimal degrees, with negative values for western hemisphere
   longitudes) */
%let lon = lon ;

/* Specify the name of the variable containing the least squares weight This variable 
   determines the amount of weight to apply to a given monitored load in model estimation.
   Larger values of the weight variable receive greater weight in determining the values
   of model coefficients. Monitored loads with high uncertainty generally should receive
   lower weights. In no weighting is to be done, set the weight variable to 1 in the data
   modification section (below). */
%let ls_weight = ls_weight ;

/* Specify the name of the variable containing the unique reach identifier from the ERF1-2 
   file (variable must be numeric) */
%let waterid = waterid ;

/* Specify the variable names for the reach information variables to be passed to the output
   files and listings. Helpful variables include, for example, the 11-digit RF1 reach identifier,
   stream name, and reservoir code. Leave blank if no additional variables are to be passed to
   output. */ 
%let optional_reach_information = rr pname rchtype headflag termflag station_id staid ;

/* Specify the name of the variable containing the area of the incremental watershed for the 
   reach (variable must be numeric). */
%let inc_area = demiarea ;

/* Specify the name of the variable containing the total upstream watershed area for the reach 
   (variable must be numeric) */
%let tot_area = demtarea ;

/* Specify the name of the variable containing the reach flow (variable must be in either cfs
   or 100 liters/s units) */
%let mean_flow = meanq ;

/* Specify if the flow units are in 100 liters/s. Otherwise, the units are presumed to be cfs. */
%let if_flow_units_metric = no ;

/* Specify the name of the variable containing the arcid variable from the ARC/INFO reach coverage */
%let arcid = arcnum ;

/* Specify the name of the variable containing the unique upstream node identifier 
   (variable must be numeric) */
%let fnode = fnode ;

/* Specify the name of the variable containing the unique downstream node identifier 
   (variable must be numeric) */
%let tnode = tnode ;

/* Specify the name of the variable containing the hydrologic sequencing number for ordering 
   reaches in a downstream order (variable must be numeric). Ascended ordering of this variable 
   allows reach processing to accumulate load downstream. */
%let hydseq = hydseq ;

/* Specify the name of the variable containing the fraction of upstream flow entering the 
   current reach. The value of this variable is set to 1 for reaches that have no diversion,
   otherwise the value is the fraction of flow going down the respective branches at a diversion. */
%let frac = frac ;

/* Specify the name of the variable containing the 0/1 code designating if flow is 
   transmitted through the reach segment. This variable takes the value 1 if the reach
   segment transfers load and zero if it does not (e.g., if the reach is always dry) */
%let iftran = iftran ;

/* Specify the name of the variable containing the 0/1 code designating target reaches.
   The identification of target reaches (value of the target variable is 1) enables the 
   model to compute the amount of flux leaving a given reach that is delivered to the 
   nearest downstream target reach (or receiving water body). For applications in which
   the receiving water body is the ocean, this variable takes the value 1 for all estuary
   and coastline reaches (i.e., where termflag = 1 or 2) and takes the value zero for all
   other reaches. */
%let target = delivery_target ;

/* ******************************************************************** */
/* Specify options for model execution                                  */
/* ******************************************************************** */

/* Specify the names of prediction variables (see the list of prediction variables generated by
   the model) that are not adjusted for retransformation bias arising from model error.
   Retransformation bias due to model error arises because the model is estimated in logarithmic
   space whereas predictions are generated in real space. All prediction variables that retain units
   of mass (for example, the predicted incremental load from point sources, or the predicted yield
   and concentration of a contaminant) exhibit this bias and must receive a bias adjustment. List
   here the prediction variables that do not depend on mass units (for example, the fraction of
   load delivered to an estuary - which is unitless) so that they do not receive an adjustment
   for model error retransformation bias. */
%let retrans_exclude_list = del_frac ; 

/* Specify if calibration is to be performed (alternative selects coefficient estimates 
   from a previous calibration run) */
%let if_estimate = yes ;

/* Specify if estimation results are to be plotted in GIS */
%let if_gis = yes ;

/* Specify the criteria for including reaches in the calibration data set. This data set
   can be smaller than the prediction set because reaches downstream from all monitoring
   stations have no effect on calibration results. Therefore, dropping these reaches from
   calibration will speed up the calibration algorithm. */
%let calibrate_selection_criteria = &WATERID > 0 & &waterid < 80000 & &hydseq > 0 ;

/* Set the printing level for the Non-linear optimization procedure:
  0 No printed output is produced. This is the default. 
  1 The summaries for optimization start and termination are produced, as well as the iteration history. 
  2 The initial and final parameter estimates are also printed. 
  3 The values of the termination criteria and other control parameters are also printed. 
  4 The parameter vector, x, is also printed after each iteration. 
  5 The gradient vector, g, is also printed after each iteration. */
%let NLP_printing_option = 3 ; 

/* Specify if the program is to run in calibration test mode. Specifying yes for this switch 
   causes the program to list waterid and relevant stream decay and land to water delivery 
   variables for observations that, when evaluated at initial parameter values, case an error 
   due to exponentiation (due to excessively large values in the operand). Large values in the 
   stream decay or land to water delivery variables typically underlie these errors. In test 
   mode, the program also checks (at the initial parameter values) for negative reach values 
   of the stream and reservoir delivery factors, the incremental watershed load, and for 
   negative values of accumulated load at monitoring stations. Setting this switch to yes 
   automatically turns off the if_accumulate_with_dll option, thereby activating a more robust 
   (albeit more time-consuming) SAS IML accumulation routine that has more informative error 
   flagging. Therefore, this switch is useful following a model run with a "modulei" error in 
   the calibration macro, signifying the dll encountered an error. */
%let if_test_calibrate = no ;

/* Specify if calibration uses a SPARROW dll add-on to do the downstream accumulation (as
   opposed to an iterative matrix procedure in proc iml). The dll reduces calibration time
   by about two thirds. To implement the dll, be sure the path containing the file SPARROW.DLL
   is included in the automatic search path of the operating environment. */
%let if_accumulate_with_dll = yes ;

/* Specify if parametric bootstrapping (as opposed to resampled bootstrapping) is to be done.
   Parametric bootstrapping generates successive iterations of coefficients based on the 
   assumption that the coefficients are distributed multivariate normal with mean and
   covariance matrix given by the parametric model results. Resampled bootstrapping requires
   model reestimation for each boostrap iteration whereas parametric bootstrapping does not.
   However, resampled bootstrapping may be more robust for small sample sizes where the 
   assumption of normally distributed coefficient estimates is not reasonable. */ 
%let if_parm_bootstrap = yes ;

/* Specify if predictions are to be made */
%let if_predict = yes ; 

/* Specify if data sets for testing prediction are to be created. When this option is set to
   yes, the program creates the additional output data sets test_predict and test_data, 
   containing the input data and results of intermediate calculations for a single reach
   (the test reach is specified in the subsequent model specification statement) which 
   facilitates checking of the prediction algorithm. The test_predict data set contains the 
   mean predictions and bootstrap simulations for each iteration (including the parametric 
   prediction performed for the 0th iteration) for the specified test observation (see the 
   following model specification statement). The mean predictions are the raw predictions 
   (predictions without retransformation bias adjustment of any kind) times the mean 
   retransformation factor (except for prediction variables included in the retrans_exclude_list 
   and reaches with monitoring data when if_adjust is yes). The bootstrap simulations (variables 
   having the prefix "boot_" in the test_predict data set) are the raw predictions times the 
   exponential of a randomly selected residual (selected from the parametric residual estimates). 
   Note  that the algorithm constructs the bootstrap simulations by modifying the mean predictions
   so that the randomly selected residuals, defined in logarithmic space, are net of the
   logarithm of the mean retransformation factor. That is, the random residual is computed
   as a randomly selected residual from the 0th-iteration model calibration (in logarithm
   space), say e(i), minus the log of the average of exp(e(j)) over all J residuals. The 
   mean predictions and bootstrap simulations should be equal only for prediction variables
   included in the retrans-exclude_list, and for all predictions made for monitored reaches
   (if the if_adjust switch is set to yes).

   The test_data data set contains the values for the input variables for the test_obs reach,
   along with the model calculated values for the reach and reservoir decay factors, the 
   incremental loads (total and by source, with and without stream decay applied), the 
   reservoir loss, and the upstream node load inputs (total and by source, with and without
   stream decay applied), and reservoir loss. */
%let if_test_predict = yes ;

/* Specify the observation number (the sequential observation of the SAS input data
   set indata stored in the WORK SAS catalog) for the test reach to be included in the
   output SAS data set test_predict. To obtain the most useful diagnostic information
   from the prediction test, select an observation corresponding to a non-target
   reach (i.e., a reach for which prediction of the fraction of its load delivered
   to its nearest downstream receiving water body will be made) and for which some
   (but not all) of its sources - both total and incremental - are zero. */
%let test_obs = 2134 ;

/* Specify if bootstrap predictions are to be printed instead of the parametric
   predictions. The bootstrap predictions require n_boot_iter to be non-zero. These
   predictions are bias-corrected but take much longer to compute than the parametric
   predictions. */
%let if_print_boot_predictions = no ;

/* Specify if predicted yeilds according to land use are to be generated by the model.
   If this option is selected, a variable names LU_class, containing a land-use class
   assignment for each reach, will be created during model execution. The user specifies
   the name and the assignment criteria for each class in the statement following this. */     
%let if_distribute_yield_by_land_use = yes ;

/* Set assignment criteria for generating yield predictions according to land use. In a list,
   specify the following:
   1. the name of the variable containing the area of the incremental watershed for the reach,
   2. the name of the first land use class (as you want it printed in the output),
   3. the name of the variable containing the area of the corresponding land use for the
      incremental watershed of the reach,
   4. the user-selected percentage criteria for assigning the first land-use class to a reach
      (for example, assign the reach to the class Crops if 90 percent of the area in the 
      incremental watershed is cultivated),
   5-7 repeat items 2-4 for the second land-use class, etc.,.
   This list is activated only if the above switch if_distribute_yield_by_land_use is set to yes. */
%let land_class_list = demiarea Crops cultiv 90 Pasture pasture 85 Forest forest 95
                       Range range 95 Urban urban 75 ;

/* Specify if the load predictions are to be adjusted to correspond with the observed load at 
   monitoring stations. Setting this switch to yes implies that predicted loads for monitored 
   reaches will exactly equal monitored loads. To obtain load by source, the monitored load is 
   apportioned to sources according to predicted source shares. Loads downstream from a monitored 
   reach will also be adjusted because these loads depend on the load delivered from the upstream reach. */
%let if_adjust = yes  ; 

/* Specify if SAS notes are to be written to the log file */
%let if_print_details = yes ;

/* Specify if SAS output data sets, which are automatically save as SAS files, are also
   output to tab-delimited text files. Text output is useful for loading results into 
   ArcView projects or into Excel spreadsheets. */
%let if_output_to_tab = yes ;

/* ****************************************************************************** */
/* Specify the modifications to be done to the input data set prior to            */
/* program execution. Modifications consist of modifying and creating additional  */
/* variables, cleaning up missing values, etc. Code in this section must        */
/* adhere to SAS conventions for coding operations in a Data Step.                */
/* ****************************************************************************** */

%let data_modifications = %str(

    /* Select reaches to be included in the analysis (either for prediction or calibration) */
    if &fnode > 0 and &tnode > 0 and DRAINDEN gt 0.0 ;

    /* Modify the monitored load variable */
    IF &depvar = 0 THEN &depvar = .;  /* Set zero monitored loads to missing. */

    /* Designate each reach as either target or non-target.  The model computes for each non-target 
       reach the fraction of flux leaving that reach that is eventually delivered to its nearest 
       downstream receiving water body (target reach). */
    &target = (termflag = 1 or termflag = 3) ;

    /* Specify the condition for transfer of load from upstream node to downstream node */
    &iftran = (meanq > 0 or staid ^= '') ;
    if (termflag = 3 or termflag = 1) then &iftran = 0 ;

    /* Define the reach decay variables. */
    if (rchtot <= 0 or termflag = 3) then rchtot = 0 ;
    rchdecay1 = (meanq <= 500) * (rchtype = 0) * rchtot ;  /* Reach time of travel is set to zero for all coastline and 
                                                              reservoir reaches */
    rchdecay2 = (500 < meanq <= 10000) * (rchtype = 0) * rchtot ;
    rchdecay3 = (meanq > 10000) * (rchtype = 0) * rchtot ;

    /* Define the reservoir decay variable */
    if hload > 0 then rhload = 1.0 / hload ;
    else rhload = 0 ;
    iresload = (rchtype = 2) * rhload ; /* The decay variable IRESLOAD is specified as the inverse of the areal 
                                           hydraulic load. - applicable only to reservoir reaches (reachtype = 2). */  

    int = 1 ;
    lon = -lon ;

    if point = . then point = 0 ;
    if atmdep = . then atmdep = 0 ;
    if fertilizer = . then fertilizer = 0 ;
    if waste = . then waste = 0 ;
    if nonagr = . then nonagr = 0 ;

    /* Set the least squares weights. Conceptually, these are
       1 / (variance of the residual). */
    if &depvar > 0 and &ls_weight = . then &ls_weight = 1 ;

  ) ;


/* ---------------------------------------------------------------------- */

/* Load the main program */
options pagesize = 50 ;
filename main "&home_program.\sparrow_main.sas" ;
%include main ;
