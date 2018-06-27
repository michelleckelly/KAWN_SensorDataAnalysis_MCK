/* Program: sparrow.sas
   Component: sparrow_main.sas
   Date: 12/31/02
   Written by: Greg Schwarz

   Purpose: Macros that implement the program control.
*/

/* ---------------------------------------------------------------------- */

/* Specify SAS options */

%macro set_options;

  %if %upcase(&if_print_details) = YES %then %let sas_options = source notes mprint macrogen nosymbolgen noxwait ;
  %else %let sas_options = nosource nonotes nomprint nomacrogen nosymbolgen noxwait ;
  options &sas_options ;

%mend set_options ;

%set_options ;

/* ---------------------------------------------------------------------- */

/* Reference the component files */

filename m_macros "&home_program.\sparrow_makemacros.sas" ;
filename s_macros "&home_program.\sparrow_setdata.sas" ;
filename c_macros "&home_program.\sparrow_calibrate.sas" ;
filename p_macros "&home_program.\sparrow_predict.sas" ;
filename k_macros "&home_program.\sparrow_compile.sas" ;
filename g_macros "&home_program.\sparrow_graphs.sas" ;

/* Reference the summary files */

filename mod_spec "&home_results.\summary_model_specs.txt" mod ;
filename mod_rslt "&home_results.\summary_model_rslts.txt" mod ;

/* ---------------------------------------------------------------------- */

/* Load the component programs */

%include m_macros ; /* Load the makemacros program */
%include s_macros ; /* Load the setdata program */
%include c_macros ; /* Load the calibrate program */
%include p_macros ; /* Load the predict program */
%include g_macros ; /* Load the graphs program */
%include k_macros ; /* Load the compile program */

/* ----------------------------------------------------------- */

/* Macro function modifies a list by appending the text contained in prescript
   to the beginning of every element in the list (delimited by a space). If the
   prescript is blank, macro function returns the original list. */

%macro prescript(list,prescript) ;

  %local i list prescript modified_list ;
  %let modified_list = ;
  %if %length(&prescript) > 0 %then %do i = 1 %to %count(&list) ;
    %let modified_list = &modified_list &prescript%scan(&list,&i,' ') ;
  %end ;
  %else %let modified_list = &list ;
  &modified_list

%mend prescript ;

/* ---------------------------------------------------------------------- */

/* Macro checks for the existence of the directory given by the arguement test_path.
   Returns a 1 if the directory is found and zero otherwise. */

%macro check_dir(test_path) ;
  %local return_val dirrf ;
  %let return_val = 0 ;
  %if %length(&test_path) > 0 %then %do ;
    %let dirrf = mydir ;
    %let rc = %sysfunc(filename(dirrf,&test_path));
    %let did = %sysfunc(dopen(&dirrf));
    %if &did > 0 %then %do ;
      %let return_val = 1 ;
      %let rc=%sysfunc(dclose(&did));
    %end ;
  %end ;
  &return_val 
%mend check_dir ;

/* ---------------------------------------------------------------------- */

/* Directory and filename definitions */

%macro set_libraries ;

  %if %check_dir(&home_data) = 1 %then %do ;
    libname dir_data "&home_data" ;
    %if %check_dir(&home_results) = 1 %then %do ;
      libname dir_rslt "&home_results" ;
    %end ;
    %else %do ;
      %put_comment(ERROR: Results directory not found - check directory specification - stop processing.) ;
      %let if_error = yes ;
    %end ;
  %end ;
  %else %do ;
    %if %upcase(&make_input_data) = YES or %sysfunc(exist(indata)) = 0 or %sysfunc(exist(ancillary)) = 0 or %sysfunc(exist(station_data)) = 0 %then %do ;
      %put_comment(ERROR: Data directory not found - check directory specification - stop processing.) ;
      %let if_error = yes ;
    %end ;
  %end ;
  %if %length(&home_gis) > 0 %then %do ;
    %if %check_dir(&home_gis) = 1 %then %do ;
      libname dir_gis "&home_gis" ;
    %end ;
    %else %do ;
      %put_comment(WARNING: GIS directory is specifed but was not found - check directory specification - analysis proceeding without GIS.) ;
      %let home_gis = ;
      %let if_gis = no ;
    %end ;
  %end ;

%mend set_libraries ;

/* ---------------------------------------------------------------------- */

/* Macro clears temporary files */

%macro clear_data ;
  
  %if &iter = 0 %then %do ;
    proc datasets nolist ;
      delete resids cov_betaest summary_betaest boot_betaest summary_boot_betaest 
             plotdat probdat mapresids predict model_parm_predict comment ;
    run ;
  %end ;
  %else %do ;
    proc datasets nolist ;
      delete boot_betaest predict test ;
    run ;
  %end ;

%mend clear_data ;

/* ---------------------------------------------------------------------- */

/* Macro takes data or catalog files in a SAS library and 
   backs them up with the name prefix bak_. The macro requires
   three entries, the SAS library name, a list of files to be
   backed up, and the type of files - either data for data files
   or catalog for catalogs. */

%macro backup(lib,names,type) ;

  %if %upcase(&type) = DATA %then %do ;
    %let delete_list = ;
    %let change_list = ;
    %let i_name = 1 ;
    %let name = %scan(&names,&i_name,' ') ;
    %do %while (%length(&name) > 0) ;
      %if %sysfunc(exist(&lib..&name)) %then %do ;
        %let delete_list = &delete_list bak_&name ;
        %let change_list = &change_list &name = bak_&name ;
      %end ;
      %let i_name = %eval(&i_name + 1) ;
      %let name = %scan(&names,&i_name,' ') ;
    %end ;
    %if %length(&delete_list) > 0 %then %do ;
      proc datasets library = &lib nolist ;
        delete &delete_list ;
        change &change_list ;
      run ;
    %end ;
  %end ;
  %else %if %upcase(&type) = CATALOG %then %do ;
    %let i_name = 1 ;
    %let name = %scan(&names,&i_name,' ') ;
    %do %while (%length(&name) > 0) ;
      %if %sysfunc(exist(&lib..&name,catalog)) %then %do ;
        proc catalog catalog = &lib..&name ;
          copy out = &lib..bak_&name ;
        proc catalog catalog = &lib..&name kill ;
        run ;
      %end ;
      %let i_name = %eval(&i_name + 1) ;
      %let name = %scan(&names,&i_name,' ') ;
    %end ;
  %end ;

%mend backup ;

/* ---------------------------------------------------------------------- */

/* Macro loops through the bootstrap iterations and creates output */

%macro main ;

  title SPARROW Analysis ;

  %global iter jter if_error ;

  %let iter = 0 ;
  %let jter = 0 ;
  %let if_error = no ;

  %if &start_iter = 0 %then %backup(dir_rslt,comment_all,data) ;
  %set_libraries ;

  /* Create the macro variables used in the program */

  %makelst ;
  %if &if_error = no %then %model_summary ;

  /* Initialize the iteration counters and the random seeds. */

  %if &if_error = no %then %do ;

    %let iter = &start_iter ;
    %let jter = &start_jter ;
    %set_seeds ;

  %end ;

  /* Set the network data */

  %if &if_error = no %then %do ;

    %if %upcase(&if_make_input_data) = YES %then %do ;
      %setdata ; /* Set the network data set */
      %check_model_vars ;
      %check_station_vars ;
      %if &if_error = no %then %do ;
        %sort_data ;
        %make_nested_area ;
        %if %upcase(&if_mean_adjust_delivery_vars) = YES %then %mean_adjust_delivery_vars ;
      %end ;
    %end ;
    %else %if %sysfunc(exist(indata)) and %sysfunc(exist(ancillary)) and %sysfunc(exist(station_data)) %then
      %put_comment(Using indata from a previous run.) ;
    %else %do ;
      %put_comment(ERROR: No input data available. Check that if_make_input_data is set to yes - stop processing) ;
      %let if_error = yes ;
    %end ;

  %end ;

  /* Create backups (only if starting processing from the beginning and there 
     are no errors thusfar) */

  %if &start_iter = 0 %then %do ;

    %if &if_error = no %then %do ;

      %if %upcase(&if_estimate) = YES %then %do ;
        /* Backup the estimation files and then delete, except in the case of mean_delivery_vars, which was
           just created and cannot be deleted */
        data dir_rslt.bak_mean_delivery_vars ; set dir_rslt.mean_delivery_vars ;
        run ;
        %backup(dir_rslt,summary_betaest cov_betaest boot_betaest_all resids test_resids error_report,data) ;
        %backup(dir_rslt,gbt_pred_vs_obs gbt_prob_plot gbt_pred_vs_resid gbt_pred_vs_wresid gbt_pred_yld_vs_wresid,catalog) ;
      %end ;
      %if %upcase(&if_predict) = YES %then 
        %backup(dir_rslt,predict predict_stats test_data test_predict summary_predict LU_yield_percentiles boot_detail,data) ;

    %end ;

  %end ;

  /* Execute the main loop until all iterations are completed or the program
     fails due to an unrecoverable error. */

  %do %while (&iter <= &end_iter and &if_error = no) ;

    title SPARROW Analysis Iteration &iter ;

    %clear_data ;
    %get_seeds ;

    %if %upcase(&if_estimate) = YES %then %do ;
      %init_beta ;
      %calibrate ;
      %if %sysfunc(exist(boot_betaest)) %then %compile_calibrate ;
      %else %do ;
        %put_comment(WARNING: Calibration for iteration &iter and seed &jter failed - going to next seed value) ;
        %let if_error = yes ;
      %end ;
    %end ;
    %else %if %sysfunc(exist(dir_rslt.boot_betaest_all)) %then %do ;
      data boot_betaest ; set dir_rslt.boot_betaest_all ; 
        where iter = &iter ;
        call symput("jter",trim(left(jter))) ;
      run ;
      %if %n_obs(boot_betaest) ^= 1 %then %do ;
        %put_comment(ERROR: Calibration results for iteration &iter not available - stop processing) ;
        %let if_error = yes ;
        %let jter = &end_jter ;
      %end ;
    %end ;
    %else %do ;
      %put_comment(ERROR: No calibration specified but previous calibration results not available - stop processing) ;
      %let if_error = yes ;
      %let jter = &end_jter ;
    %end ;

    %if &if_error = no %then %do ;

      %if %sysfunc(exist(boot_betaest)) %then %do ;
        %if %upcase(&if_predict) = YES %then %do ;
          %predict ;
          %if %sysfunc(exist(predict)) %then %do ;
            /* Note that for iteration 0, only boot_betaest is needed for compilation. Otherwise, must have resids and summary_betaest */
            %if &iter = 0 or (%sysfunc(exist(dir_rslt.resids)) and %sysfunc(exist(dir_rslt.summary_betaest))) %then %compile_predict ;
            %else %do ;
              %let not_found = ;
              %if %sysfunc(exist(dir_rslt.summary_betaest)) = 0 %then %let not_found = dir_rslt.summary_betaest ;
              %if %sysfunc(exist(dir_rslt.resids)) = 0 %then %do ;
                %if %length(&not_found) > 0 %then %let not_found = files &not_found and dir_rslt.resids were ;
                %else %let not_found = file dir_rslt.resids was ;
              %end ;
              %else %let not_found = file &not_found was ;
              %put_comment(ERROR: SAS &not_found not found - no bootstrap predictions compiled, stop processing) ;
              %let if_error = yes ;
              %let jter = &end_jter ;
            %end ;
          %end ;
          %else %do ;
            %put_comment(WARNING: Predictions for iteration &iter and seed &jter failed - going to next seed value) ;
            %let if_error = yes ;
          %end ;
        %end ;
      %end ;
      %else %do ;
        %put_comment(WARNING: Calibration for iteration &iter and seed &jter failed - going to next seed value) ;
        %let if_error = yes ;
      %end ;

    %end ;

    %if &if_error = no %then %do ;

      %put Completed iteration &iter ;
      %let iter = %eval(&iter + 1) ;

    %end ;

    %let jter = %eval(&jter + 1) ;
    %let if_error = no ;

    %if &jter > &end_jter and &iter <= &end_iter %then %do ;
      %put_comment(ERROR: Exhausted seeds - stop processing) ;
      %let if_error = yes ;
    %end ;

  %end ;

  %if &if_error = no %then %do ;

    %if %upcase(&if_estimate) = YES and %n_obs(dir_rslt.boot_betaest_all) = %eval(&n_boot_iter + 1) and &n_boot_iter > 0 and %upcase(&if_parm_bootstrap) = NO %then %summarize_calibrate ;
    %if %upcase(&if_predict) = YES and &iter = %eval(&n_boot_iter + 1) %then %summarize_predict ;

  %end ;

%mend main ;

%main ;