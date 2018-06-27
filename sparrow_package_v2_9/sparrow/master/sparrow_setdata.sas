/* Program: sparrow.sas
   Component: sparrow_setdata.sas
   Date: 12/31/02
   Written by: Greg Schwarz

   Purpose: Macros used for setting up the data to be used in sparrow model.
*/

/* ---------------------------------------------------------------------- */

/* Load the data and make any modifications */

%macro setdata ;

  data indata (keep = &indata_list)
       station_data (keep = &staid &arcid &optional_station_information &waterid
                           &lat &lon &ls_weight) 
       ancillary (keep = &arcid &waterid &optional_reach_information &fnode &tnode &hydseq
                         &inc_area &tot_area &mean_flow &frac &iftran &target &ls_weight 
                         %if %upcase(&if_distribute_yield_by_land_use) = YES %then LU_class ;
                         ) ;
    set dir_data.&indata ; 

    &data_modifications ;

    if &depvar <= 0 then &ls_weight = . ;

    /* Set the LU classes for summarizing predictions */

    %if %upcase(&if_distribute_yield_by_land_use) = YES %then %do ;
      length LU_class $ &LU_class_length ;
      %let total = %scan(&land_class_list,1,' ') ;
      %let i_var = 2 ;
      %let cond = ;
      %let class_var = %scan(&land_class_list,&i_var,' ') ;
      %do %while(%length(&class_var) > 0) ;
        %let pct_var = %scan(&land_class_list,%eval(&i_var + 2),' ') ;
        &cond if %scan(&land_class_list,%eval(&i_var + 1),' ') > (&total * &pct_var / 100) then LU_class = "&class_var" ;
        %let cond = else ;
        %let i_var = %eval(&i_var + 3) ;
        %let class_var = %scan(&land_class_list,&i_var,' ') ;
      %end ;
    %end ;

    /* Define labels for required variables */

    label &staid = SPARROW Monitoring Station Identifier ;
    label &inc_area = Incremental Drainage Area (km2) ;
    label &tot_area = Upstream Drainage Area (km2) ;
    label &mean_flow = Mean Flow (cfs) ;
    label &waterid = SPARROW Reach Identifier ;
    label &fnode = Upstream Reach Node Identifier ;
    label &tnode = Downstream Reach Node Identifier ;
    label &iftran = If Reach Transmits Flux (1=yes, 0=no) ;
    label &frac = Fraction Upstream Flux Diverted to Reach ;
    label &hydseq = SPARROW Reach Hydrologic Sequencing Code ;
    label &ls_weight = Least-Squares Weight ;
    %if %length(&lat) > 0 %then label &lat = Station Latitude (decimal degrees) ;;
    %if %length(&lon) > 0 %then label &lon = Station Longitude (decimal degrees) ;;
    %if %length(&target) > 0 %then label &target = Destination Reach for Delivery (0/1) ;;
    %if %upcase(&if_distribute_yield_by_land_use) = YES %then label LU_class = Watershed Land-Cover Type ;;

    /* Output the data */
    output indata ancillary ;
    if &depvar ^= . then output station_data ;

  run ;

%mend setdata ;

/* ---------------------------------------------------------------------- */

/* Macro sorts the input data */

%macro sort_data ;

  /* Sort the data by downstream order */

  proc sort data = indata ; by &hydseq ;
  run ;

  /* Sort ancillary data by the reach identifier */

  proc sort data = ancillary ; by &waterid ;
  run ;

  /* Sort the station data by the station identifier */
  proc sort data = station_data ; by &staid ;
  run ;

%mend sort_data ;

/* ---------------------------------------------------------------------- */

/* Macro function returns the items in list2 that are not included in list1.
   Both lists are delimited by one or more spaces. */

%macro exclude_list(list1,list2) ;
  %local i var return_list ;
  %let return_list = ;
  %do i = 1 %to %count(&list2) ;
    %let var = %scan(&list2,&i,' ') ;
    %if %sysfunc(indexw(&list1,&var)) = 0 %then %let return_list = &return_list &var ;
  %end ;
  &return_list
%mend exclude_list ;

/* ---------------------------------------------------------------------- */

/* Macro tests if variables, identified by macro variable names in the list
   varnms (if if_macro_nms = yes) or by the items in the varnms list 
   (if_macro_nms ^= yes), are numeric. If a variable is determined to be 
   character, an error is reported and processing is stopped. */

%macro check_numeric(if_macro_nms,varnms) ;

  %local i dsid varnm varnum text_qualifier ;
  %let dsid = %sysfunc(open(indata,i)) ;
  %if &dsid > 0 %then %do ;
    %do i = 1 %to %count(&varnms) ;
      %if &if_macro_nms = yes %then %do ;
        %let cntrlnm = %scan(&varnms,&i,' ') ;
        %let varnm = &&&cntrlnm ;
        %let text_qualifier = Variable &varnm associated with control variable &cntrlnm ;
      %end ;
      %else %do ;
        %let varnm = %scan(&varnms,&i,' ') ;
        %let text_qualifier = Model variable &varnm ;
      %end ;
      %if %length(&varnm) > 0 %then %do ;
        %let varnum = %sysfunc(varnum(&dsid,&varnm)) ;
        %if &varnum > 0 %then %do ;
          %if %sysfunc(vartype(&dsid,&varnum)) = C %then %do ;
            %put_comment(ERROR: &text_qualifier is stored as character but should be numeric - stop processing.) ;
            %let if_error = yes ;
          %end ;
        %end ;
      %end ;
    %end ;
    %let rc = %sysfunc(close(&dsid)) ;
  %end ;

%mend check_numeric ;

/* ---------------------------------------------------------------------- */

/* Macro function returns the elements of the search_vars list that are not
   variable names found in the SAS data set in_file. */

%macro check_vars(in_file,search_vars) ;

  %local in_id not_found file_vars i search_var rc ;
  %let not_found = ;
  %if %sysfunc(exist(&in_file)) %then %do ;
    %let file_vars = ;
    %let in_id = %sysfunc(open(&in_file)) ;
    %do i = 1 %to %sysfunc(attrn(&in_id,nvars)) ;
      %let file_vars = &file_vars %sysfunc(varname(&in_id,&i)) ;
    %end ;
    %let file_vars = %upcase(&file_vars) ;
    %do i = 1 %to %count(&search_vars) ;
      %let search_var = %scan(&search_vars,&i,' ') ;
      %if %index(&file_vars,%upcase(&search_var)) = 0 %then %let not_found = &not_found &search_var ;
    %end ;
    %let rc = %sysfunc(close(&in_id)) ;
  %end ;  
  &not_found

%mend ;

/* ---------------------------------------------------------------------- */

/* Macro checks the data set check_data for any missing values in the
   numeric variables check_vars. The macro returns a SAS data set
   out_data consisting of a single observation containing the
   variables check_vars coded with a 1 if the variable has any missing 
   values and 0 otherwise. */

%macro check_missing_preproc(check_data,out_data,check_vars) ;

  proc iml ;

    use &check_data ;
    read all var {&check_vars} into data ;
    close &check_data ;

    check = ((data = .)[+,] > 0) ;

    create &out_data from check [colname = ({&check_vars})];
    append from check ;
    close &out_data ;

  quit ;
  run ;

%mend check_missing_preproc ;

/* ---------------------------------------------------------------------- */

/* Macro reads the data set in_data created by the macro check_missing_preproc
   and creates a macro variable list of all variables in check_vars that 
   contain missing values. */

%macro check_missing_postproc(in_data,check_vars) ;

  %let dsid = %sysfunc(open(&in_data)) ;
  %let rc = %sysfunc(fetch(&dsid)) ;
  %let missing_vars_list = ;
  %do ivar = 1 %to %count(&check_vars) ;
    %let var = %scan(&check_vars,&ivar,' ') ;
    %let varid = %sysfunc(varnum(&dsid,&var)) ;
    %if %sysfunc(getvarn(&dsid,&varid)) = 1 %then %let missing_vars_list = &missing_vars_list &var ;
  %end ;
  %let rc = %sysfunc(close(&dsid)) ;
  &missing_vars_list

%mend check_missing_postproc ;

/* ---------------------------------------------------------------------- */

/* Macro checks for any model variables not found in indata and, for found 
   model variables, checks for any missing values. If any model variables are 
   not found or if they contain any missing values, the macro turns the error flag
   to yes and prints comments to the comment file identifying variables that are not in
   indata and variables that are in indata but contain missing values. */

%macro check_model_vars ;

  %let vars_not_in_indata = %check_vars(indata,&model_vars_list) ;
  %let vars_in_indata = %check(&model_vars_list,&vars_not_in_indata) ;
  %if %length(&vars_not_in_indata) > 0 %then %do ;
    %let if_error = yes ;
    %put_comment(ERROR: The following model variables were not found: &vars_not_in_indata.. Add to the input data set or specify in a data_modifications statement.) ;
  %end ;
  %if %length(&vars_in_indata) > 0 %then %do ;
    %check_missing_preproc(indata,check,&vars_in_indata) ;
    %let missing_vars = %check_missing_postproc(check,&vars_in_indata) ;
    %if %length(&missing_vars) > 0 %then %do ;
      %let if_error = yes ;
      %put_comment(ERROR: The following model variables require a data_modifications statement to remove missing values: &missing_vars..) ;
    %end ;
  %end ;
  %let numeric_list = depvar ls_weight staid lat lon waterid mean_flow arcid inc_area tot_area fnode tnode hydseq frac iftran target ;
  %check_numeric(yes,&numeric_list) ;
  %let num_vars_list = ;
  %do i = 1 %to %count(&numeric_list) ;
    %let num_vars_list = &num_vars_list &&%scan(&numeric_list,&i,' ') ;
  %end ;
  %let check_not_num_list = %exclude_list(&num_vars_list,&model_vars_list) ;
  %check_numeric(no,&check_not_num_list) ;

%mend check_model_vars ;

/* ---------------------------------------------------------------------- */

/* Macro checks station_data for any missing values in critical station
   variables. If any missing values are found for a critical station variable,
   the error flag is raised. Missing values for non-critical variables are noted
   but do not result in raising the error flag. */

%macro check_station_vars ;

  data test ; set station_data nobs = numobs ;
    if &staid = . then count_staid + 1 ;
    if &ls_weight = . then count_ls_weight + 1 ;
    if lat = . or lon = . then count_lat_lon + 1 ;
    if _n_ = numobs then do ;
      call symput("count_staid",trim(left(count_staid))) ;
      call symput("count_ls_weight",trim(left(count_ls_weight))) ;
      call symput("count_lat_lon",trim(left(count_lat_lon))) ;
    end ;
    delete ;
  run ;

  %if &count_staid > 0 %then %do ;
    %let if_error = yes ;
    %put_comment(ERROR: Reaches (&count_staid) with monitored flux are missing a value for the station identifier &staid - stop processing.) ;
  %end ;
  %if &count_ls_weight > 0 %then %do ;
    %let if_error = yes ;
    %put_comment(ERROR: Reaches (&count_ls_weight) with monitored flux are missing a value for the observation weight variable &ls_weight - stop processing.) ;
  %end ;
  %if &count_lat_lon > 0 %then %put_comment(WARNING: Reaches (&count_lat_lon) with monitored flux do not have a valid lat/lon and will not be represented in the map of station residuals.) ; 

%mend check_station_vars ;

/* ---------------------------------------------------------------------- */

/* Macro initializes the coefficient estimates */

%macro init_beta ;

  /* Initialize the beta coefficient values */

  data betahat0 ; retain &bretain ;
  %if (&iter = 0 and %upcase(&if_init_beta_w_previous_est) = YES) and %sysfunc(exist(dir_rslt.bak_summary_betaest)) %then %do ;
    set dir_rslt.bak_summary_betaest ;
  %end ;
  %else %if &iter > 0 and %sysfunc(exist(dir_rslt.summary_betaest)) %then %do ;
    set dir_rslt.summary_betaest (keep = &betalst) ;
  %end ;
  output ;
  run ;

%mend init_beta ;

/* ---------------------------------------------------------------------- */

/* Macro sets a file of seed numbers */

%macro set_seeds ;

  data seeds ;
    do jter = 0 to &n_boot_iter + &n_extra_jter ;
      %do iseed = 1 %to &n_seeds ;
        seed_&iseed = floor(1E8 * ranuni(&master_seed)) ;
      %end ;
      output ;
    end ;
  run ;

%mend set_seeds ;

/* ---------------------------------------------------------------------- */

/* Macro gets seed numbers */

%macro get_seeds ;

  data temp ; set seeds (firstobs = %eval(&jter + 1) obs = %eval(&jter + 1)) ;
    %do iseed = 1 %to &n_seeds ;
      call symput("seed_&iseed",trim(left(seed_&iseed))) ;
    %end ;
  run ;

%mend get_seeds ;

/* ---------------------------------------------------------------------- */

/* Macro creates the mean_delivery_vars data set containing the means of the 
   delivery variables. This data set must be retained as an estimate data set
   because the source coefficients are contingent on the mean delivery variables
   used in the analysis. Thus, the mean_delivery_vars data set is recomputed
   only if if_estimate is set to yes - otherwise the stored data set is used. 
   A terminating error is generated if a previous data set is needed but not
   found. */

%macro mean_adjust_delivery_vars ;

  %if %upcase(&if_estimate) = YES %then %do ;

    proc iml ;

      use indata ;
      read all var {&dlvvar} into delivery_vars ;
      close indata ;

      mean_dlvvar_list = compress({m_} + {&dlvvar}) ;

      mean_dlvvar = delivery_vars[:,] ;

      create dir_rslt.mean_delivery_vars from mean_dlvvar [colname = mean_dlvvar_list] ;
      append from mean_dlvvar ;
      close dir_rslt.mean_delivery_vars ;

    quit ;

  %end ;
  %if %sysfunc(exist(dir_rslt.mean_delivery_vars)) %then %do ;
    %let mean_dlvvar_list = %prescript(&dlvvar,m_) ;
    data indata ; if _n_ = 1 then set dir_rslt.mean_delivery_vars ; set indata ;
      array dlvvar {*} &dlvvar ;
      array mean_dlvvar {*} &mean_dlvvar_list ;
      do i = 1 to dim(dlvvar) ;
        dlvvar{i} = dlvvar{i} - mean_dlvvar{i} ;
      end ;
      drop i &mean_dlvvar_list ;
    run ;
  %end ;
  %else %do ;
    %let if_error = yes ;
    %put_comment(ERROR: Specification requests using previous estimation but data set mean_delivery_vars could not be found in the results directory - stop processing.) ;
  %end ;

%mend mean_adjust_delivery_vars ;

/* ---------------------------------------------------------------------- */

/* Macro computes the nested area for each monitoring station included in the model */

%macro make_nested_area ;

  proc iml ;

    use indata ;
    read all %if %length(&calibrate_selection_criteria) > 0 %then where (&calibrate_selection_criteria) ;
      var {&fnode &tnode &iftran &frac &inc_area &staid &depvar} into data ;
    close indata ;

    jfnode = 1 ;
    jtnode = 2 ;
    jiftran = 3 ;
    jfrac = 4 ;
    jinc_area = 5 ;
    jstaid = 6 ;
    jdepvar = 7 ;

    nreach = nrow(data) ;
    nnode = max(data[,(jfnode || jtnode)]) ;
    obsloc = loc(data[,jdepvar] ^= .) ;
    nobs = ncol(obsloc) ;

    nest_area = j(nobs,1,.) ;
    node = j(nnode,1,0) ;
    i_obs = 1 ;
    do i = 1 to nreach ;
      if data[i,jdepvar] ^= . then do ;
        nest_area[i_obs] = data[i,jinc_area] + data[i,jfrac] * node[data[i,jfnode]] ;
        i_obs = i_obs + 1 ;
      end ;
      else node[data[i,jtnode]] = node[data[i,jtnode]] + data[i,jiftran] * (data[i,jinc_area] + data[i,jfrac] * node[data[i,jfnode]]) ;
    end ;

    output = data[obsloc,jstaid] || nest_area ;
    
    create nested_area from output [colname = {&staid nested_area}] ;
    append from output ;
    close nested_area ;

  quit ;

  proc sort data = nested_area ; by &staid ;

  data station_data ; merge station_data nested_area ; by &staid ;
    label nested_area = Station Nested Basin Area (km2) ;
  run ;

%mend make_nested_area ;

/* ---------------------------------------------------------------------- */


