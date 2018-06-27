/* Program: sparrow.sas
   Component: sparrow_compile.sas
   Date: 12/31/02
   Written by: Greg Schwarz

   Purpose: Macros used for compiling calibration and prediction results from the sparrow model.
*/
/* UNDOCUMENTED FEATURE - the output of individual bootstrap iteration estimates for selected prediction variables and reaches */
/* Specify the selected reach &waterid for those reaches to be included in the detailed bootstrap output. Leave blank if no detailed bootstrap
   output is desired. Specify ALL if detailed bootstrap output is desired for all reaches in the analysis. */
%let boot_detail_reaches = 65202 65279 65281 65683 65289 65771 65708 65324 65667 ;
%let boot_detail_reaches = ;
%let boot_detail_reaches = ALL ;
/* Specify the prediction variables to be included in the detailed bootstrap output. If the specification is null or CUM, then the detailed output
   includes the delivery fraction (if a target variable is specified), the bootstrap error (boot_error) applied to all mass predictions at the reach
   for the given bootstrap iteration, and all PLOAD_[] prediction variables (but not INC or ND variables). If the specification is INC, then the
   detailed output includes the delivery fraction, bootstrap model error and all PLOAD_INC_[] prediction variables (but not the CUM or ND pload variables).
   If the specification is CUM INC, then the delivery fraction, the bootstrap model error, and all cumulative and incremental prediction variables (but 
   not ND variables) are included. */
%let boot_detail_predvars = CUM INC ;
%let boot_detail_predvars = ;
%let boot_detail_predvars = INC ;

/* ---------------------------------------------------------------------- */

/* Macro writes SAS files (first input) to tab files (second input) 
   Messages created during the operation are directed to an external
   text file in the home_results directory and then deleted. */

%macro out_tab(sasfile,dbfile) ;
  options nomprint nomacrogen nosource ;
  filename test "&home_results.\temp.txt" ;
  /* Redirect the output to a file */
  proc printto log = test new ;
  run ;  
  proc export data = &sasfile outfile = "&dbfile..txt" dbms = tab replace ;
  run ;
  /* Redirect the output back to the output window */
  proc printto ;
  run ;
  %let rc = %sysfunc(fdelete(test)) ;
  options &sas_options ;
%mend out_tab ;

/* ---------------------------------------------------------------------- */

/* Macro writes comments to the screen and to a comment log file */

%macro put_comment(comment) ;

  %window statmsg rows=5 columns=80
    #1 @1 "Iteration &iter.: &comment" persist = yes ;
  %display statmsg noinput ;
  %put &comment ;
  data comment ; 
    iter = &iter ;
    jter = &jter ;
    length comment $200 ;
    retain comment ;
    comment = "&comment" ;
    label comment = Comment ;
    label iter = Iteration ;
    label jter = Seed iteration ;
    output ;
  proc append base = dir_rslt.comment_all data = comment force ;
  run ;

%mend put_comment ;

/* ---------------------------------------------------------------------- */

/* Macro determines the number of observations in a SAS dataset. Returns
   -1 if file does not exist. */

%macro n_obs(ds_name) ;
  %let ds_id = %sysfunc(open(&ds_name)) ;
  %if &ds_id > 0 %then %let n_obs = %sysfunc(attrn(&ds_id,nobs)) ;
  %else %let n_obs = -1 ;
  %let rc = %sysfunc(close(&ds_id)) ;
  &n_obs
%mend n_obs ;

/* ---------------------------------------------------------------------- */

/* Macro appends a (new) data set to a (orig) master data set. */

%macro append(new_library,new_data_member,orig_library,orig_data_member) ;

  %if %sysfunc(exist(&new_library..&new_data_member)) %then %do ;
    %let if_append = -1 ;
    proc iml ;
      if exist("&orig_library..&orig_data_member") then 
        call symput('if_append',trim(left(char(all(
          xsect(upcase(contents("&orig_library","&orig_data_member")),upcase(contents("&new_library","&new_data_member"))) = 
          union(upcase(contents("&new_library","&new_data_member")))
          ))))) ;
    quit ;

    %if &if_append = 0 %then %do ;
      data &orig_library..&orig_data_member ; set &orig_library..&orig_data_member &new_library..&new_data_member ;
    %end ;
    %else %do ;
      proc append out = &orig_library..&orig_data_member data = &new_library..&new_data_member force ;
    %end ;
    run ;
  %end ;

%mend append ;

/* ---------------------------------------------------------------------- */

/* Macro sets a (new) data set to the end of a (orig) master data set,
   if both the original and new data sets exist. If the new data set exists
   but the orig data set does not then the macro creates the orig data set
   from the new data set. */

%macro addset(new_library,new_data_member,orig_library,orig_data_member) ;

  %if %sysfunc(exist(&new_library..&new_data_member)) %then %do ;
    %if %sysfunc(exist(&orig_library..&orig_data_member)) %then %do ;
      data &orig_library..&orig_data_member ; set &orig_library..&orig_data_member &new_library..&new_data_member ;
    %end ;
    %else %do ;
	  data &orig_library..&orig_data_member ; set &new_library..&new_data_member ;
    %end ;
	run ;
  %end ;

%mend addset ;

/* ---------------------------------------------------------------------- */

/* Macro joins (merges) a new data set with an original data set (if it exists).
   Otherwise it creates the original data set from the new data set. */

%macro join(new_library,new_data_member,orig_library,orig_data_member) ;

  %if %sysfunc(exist(&orig_library..&orig_data_member)) %then %do ;
    data &orig_library..&orig_data_member ; merge &orig_library..&orig_data_member &new_library..&new_data_member ;
  %end ;
  %else %do ;
    data &orig_library..&orig_data_member ; set &new_library..&new_data_member ;
  %end ;
  run ;

%mend join ;

/* ---------------------------------------------------------------------- */

/* Macro compiles calibration results - if they exist */

%macro compile_calibrate ;

  %append(work,boot_betaest,dir_rslt,boot_betaest_all) ;
  %if &iter = 0 %then %do ;
    %if %sysfunc(exist(summary_betaest)) %then %do ;
      data dir_rslt.summary_betaest ; set summary_betaest ;
        label nobs = Number of Observations ;
        label df_error = Degrees of Freedom ;
        label df_model = Number of Estimated Coefficients ;
        label sse = Sum of Squared Errors ;
        label mse = Mean Squared Error ;
        label rmse = Root Mean Squared Error ;
        label r_square = R square ;
        label adj_r_square = Adjusted R Square ;
        label r_square_yield = LN Yield R Square ;
        label e_val_spread = Eigenvalue Spread (max/min) ;
        label ppcc = Normal Prob Plot Correlation Coefficient ;
        label swilk_stat = Shapiro-Wilks W Statistic ;
        label swilk_pval = Shapiro-Wilks W Statistic P-Value ;
      run ;
      %if %upcase(&if_output_to_tab) = YES %then %out_tab(dir_rslt.summary_betaest,&home_results.\summary_betaest) ;
      %if %upcase(&if_test_calibrate) = YES and %sysfunc(exist(dir_rslt.test_resids)) %then %out_tab(dir_rslt.test_resids,&home_results.\test_resids) ;
    %end ;
    %if %sysfunc(exist(cov_betaest)) %then %do ;
      data dir_rslt.cov_betaest ; set cov_betaest ;
        label vif = Variance Inflation Factor ;
        label e_val = Eigenvalues ;
      run ;
      %if %upcase(&if_output_to_tab) = YES %then %out_tab(dir_rslt.cov_betaest,&home_results.\cov_betaest) ;
    %end ;
    %if %sysfunc(exist(resids)) %then %do ;
      proc sort data = resids out = dir_rslt.resids ; by &staid ;
      data dir_rslt.resids ;
        retain &station_priority_list ;
        merge station_data dir_rslt.resids (in = in_resid) ; by &staid ;
        if in_resid ;
        id + 1 ;
        label map_resid = Standardized Residual ;
        label z_map_resid = Nrml Quantile of Standard Resid Ordr Stat ;
        label actual = Monitored Load (&load_units) ;
        label predict = Predicted Load (&load_units) ;
        label ln_actual = Natural Log of Monitored Flux ;
        label ln_predict = Predicted Natural Log of Flux ;
        label ln_pred_yield = Predicted Natural Log of Yield ;
        label ln_resid = Actual to Pred Natural Log Flux Ratio ;
        label weighted_ln_resid = Weighted Value of LN Residual ;
        label leverage = Leverage (h stat, based on gradients) ;
        label boot_resid = "Wt LN Resid/sqrt(1-Lev)" ;
      run ;
      %if %upcase(&if_output_to_tab) = YES %then %out_tab(dir_rslt.resids,&home_results.\resids) ;
      %graph_resids ;
      %if %upcase(&if_gis) = YES and %length(&home_gis) > 0 %then %update_gis ;
    %end ;
  %end ;

%mend compile_calibrate ;

/* ---------------------------------------------------------------------- */

/* Macro inputs parametric and bootstrap coefficient estimates and computes
   bootstrap means, variances and confidence intervals */

%macro summarize_calibrate ;

  proc iml ;

    /* Module computes the variance of columns in a matrix - returns a row vector of variances */

    start varcol(inmatrix) ;
      if nrow(inmatrix) = 1 then var = . ;
      else var = (inmatrix[##,] - nrow(inmatrix) * inmatrix[:,] ## 2) / (nrow(inmatrix) - 1) ;
      return(var) ;
    finish ;

    /* Module combines Kx1 vector of parametric estimates with NxK matrix of bootstrap estimates to
       compute an equal-tailed hybrid confidence interval with specified coverage probability. 
       Returns a 1x2K row vector of lower followed by upper limits of the confidence interval. */

    start boot_ci(p_beta,in_b_beta,cov_prob) ;

      b_beta = in_b_beta ;
      n_lo = floor((1 - cov_prob / 100) * nrow(b_beta) / 2) + 1 ;
      n_hi = nrow(b_beta) - floor((1 - cov_prob / 100) * nrow(b_beta)) - 1 + n_lo ;

      /* Rank the bootstrap estimates and read off the required quantiles */
      do i_col = 1 to ncol(b_beta) ;
        b_beta[rank(b_beta[,i_col]),i_col] = b_beta[,i_col] ;
      end ;

      b_beta = b_beta[n_lo || n_hi,] ;
      ci_lo = 2 * p_beta - b_beta[2,] ;
      ci_hi = 2 * p_beta - b_beta[1,] ;
      ci = ci_lo || ci_hi ;
      return(ci) ;

    finish ;

    /* Module uses the approach of Noreen (1989) to compute bootstrap p values.
       The distribution of the mean-centered bootstrap estimates are compared
       to the parametric estimate to determine number at least as large as the
       parametric estimates. */

    start boot_pval(p_beta,in_b_beta) ;

      b_beta = in_b_beta ;
      pval = j(1,ncol(b_beta),.) ;
      mean_b_beta = b_beta[:,] ;

      /* Rank the bootstrap estimates and read off the required quantiles */
      do i_col = 1 to ncol(b_beta) ;
        b_beta[rank(b_beta[,i_col]),i_col] = b_beta[,i_col] ;
        pval[,i_col] = sum(b_beta[,i_col] >= (p_beta[,i_col] + mean_b_beta[,i_col])) / nrow(b_beta) ;
        if p_beta[,i_col] < 0 then pval[,i_col] = 1 - pval[,i_col] ;
      end ;

      return(pval) ;

    finish ;

    /* Load the coefficient estimates */

    parameters = {&betalst} ;
    std_parm_list = compress({sd_} + parameters) ; 

    use dir_rslt.boot_betaest_all ;
    read all var parameters where (iter = 0) into parm_beta ;
    read all var parameters where (iter > 0) into boot_beta ;
    close dir_rslt.boot_betalst_all ;

    use dir_rslt.summary_betaest ;
    read all var std_parm_list into std_parm ;
    read all var {df_error} into df_error ;
    close dir_rslt.summary_betaest ;

    &makecol ;

    if type(parm_beta) ^= {u} & type(boot_beta) ^= {u} then do ;

      /* Compile bias-corrected parameter estimates, bootstrap estimate of standard deviation of parameter estimates, 
         and lower and upper limits of the confidence interval */

      unbias_parm = 2 * parm_beta - boot_beta[:,] ;
      boot_std_parm = sqrt(varcol(boot_beta)) ;
      boot_ci_parm = boot_ci(parm_beta,boot_beta,&cov_prob) ;
      boot_pval_parm = boot_pval(parm_beta,boot_beta) ;

      estimates = unbias_parm || boot_std_parm || boot_ci_parm ;
      varnames = compress(({unbias_} + parameters) || ({stdev_} + parameters) || ({ci_lo_} + parameters) || ({ci_hi_} + parameters)) ;

      /* Output the bootstrap estimates */

      create summary_boot_betaest from estimates [colname = varnames] ;
      append from estimates ;
      close summary_boot_betaest ;

      /* Determine t-stats and p-values for parametric and bootstrap estimates */

      jncnstrn = loc(std_parm > 0) ;
      jncnstrn_boot = loc(boot_std_parm > 0) ;

      parameters = parameters` ;
      parm_beta = parm_beta` ;
      std_parm = std_parm` ;
      unbias_parm = unbias_parm` ;
      boot_std_parm = boot_std_parm` ;
      boot_ci_parm_lo = boot_ci_parm[,1:nrow(unbias_parm)]` ;
      boot_ci_parm_hi = remove(boot_ci_parm,1:nrow(unbias_parm))` ;
      boot_pval_parm = boot_pval_parm` ;
      t_stat = j(nrow(parm_beta),1,.) ;
      p_value = j(nrow(parm_beta),1,.) ;
      t_stat_boot = j(nrow(parm_beta),1,.) ;
      p_value_boot = j(nrow(parm_beta),1,.) ;
      t_stat[jncnstrn,] = parm_beta[jncnstrn,] / std_parm[jncnstrn,] ;
      p_value[jncnstrn,] = 2*(1-probt(abs(t_stat[jncnstrn,]),df_error)) ;
      t_stat_boot[jncnstrn_boot,] = unbias_parm[jncnstrn_boot,] / boot_std_parm[jncnstrn_boot,] ;
      p_value_boot[jncnstrn_boot,] = boot_pval_parm[jncnstrn_boot,] ;

      /* Produce a printed report summarizing parametric and bootstrap estimates */

      mattrib parameters      label = 'Parameter'
              parm_beta       label = 'Parm Estimate'
              std_parm        label = 'Parm Std Err'
              t_stat          label = 'Parm t-Value'
              p_value         label = 'Parm Pr > |t|'
              unbias_parm     label = 'Estimate'
              boot_std_parm   label = 'Std Err '
              t_stat_boot     label = 't Value '
              p_value_boot    label = 'Pr > |t|'
              boot_ci_parm_lo label = "&cov_prob.% CI LB"
              boot_ci_parm_hi label = "&cov_prob.% CI UB" ;

      title1 SPARROW Model Bootstrap Parameter Estimates ;
      %if %upcase(&if_parm_bootstrap) = YES %then %do ;
        footnote1 "Note that a normal parametric bootstrap method was used to generate the coefficient estimates." ;
        footnote2 "Therefore, the estimates above represent parametric results that are not bias-corrected.      " ;
      %end ;
      %else %do ;
        footnote1 "'Estimate' column consists of bootstrap bias-adjusted coefficient estimates. " ;
      %end ;
      print / parameters unbias_parm boot_std_parm t_stat_boot p_value_boot boot_ci_parm_lo boot_ci_parm_hi ;

    end ;

  quit ;

  title1 Sparrow Analysis ;
  footnote1 ;

  %if %upcase(&if_output_to_tab) = YES %then %out_tab(dir_rslt.boot_betaest_all,&home_results.\boot_betaest_all) ;

  %if %sysfunc(exist(summary_boot_betaest)) %then %do ;

    data dir_rslt.summary_betaest ; merge dir_rslt.summary_betaest summary_boot_betaest ;
    run ;

    %if %upcase(&if_output_to_tab) = YES %then %out_tab(dir_rslt.summary_betaest,&home_results.\summary_betaest) ;

  %end ;

%mend summarize_calibrate ;

/* ---------------------------------------------------------------------- */

/* Macro compiles prediction estimates */

%macro compile_predict ;

  %if %upcase(&if_test_predict) = YES %then %do ;
    proc append base = dir_rslt.test_data data = test_data ; 
    run ;
  %end ;
  %if &iter = 0 %then %do ;
    data dir_rslt.predict ; set predict ;
    run ;
  %end ;
  %else %do ;
    %if %upcase(&if_test_predict) = YES %then %test_predict ;
    %compile_stats ;
    %compile_ci ;
  %end ;

%mend compile_predict ;

/* ---------------------------------------------------------------------- */

/* Macro tests the statistics and confidence interval generation for one
   reach */

%macro test_predict ;

  data test ; retain iter jter ;
    set predict (firstobs = &test_obs obs = &test_obs) ;
    iter = &iter ;
    jter = &jter ;
  run ;
  %if %sysfunc(exist(dir_rslt.test_predict)) %then %do ;
    data dir_rslt.test_predict ; set dir_rslt.test_predict test ;
    run ;
  %end ;
  %else %do ;
    data dir_rslt.test_predict ; set test ;
    run ;
  %end ;

%mend test_predict ;

/* ---------------------------------------------------------------------- */

/* Macro compiles the predictions into the mean and variance estimates */

%macro compile_stats ;

  proc iml ;

    predlst = {&predlst} ;
    statnames = {"iter"} || compress(({mean_} + predlst) || ({var_} + predlst)) ;

    use predict ;
    read all var predlst into predict ;
    close predict ;

    %if &iter = 1 %then %do ;
      stats = j(nrow(predict),1,&iter) || predict || j(nrow(predict),ncol(predict),0) ;
    %end ;
    %else %do ;
      use dir_rslt.predict_stats ;
      read all var statnames into stats ;
      close dir_rslt.predict_stats ;
      /* Update the mean and variance statistics and store in stats */
      stats[,1] = &iter ;
      stats[,2:ncol(stats)] = ((1 - 1 / &iter) * stats[,1 + (1:ncol(predict))] + predict / &iter) ||
        ((1 - 1 / (&iter - 1)) * stats[,(ncol(predict) + 2):ncol(stats)] + ((stats[,1 + (1:ncol(predict))] - predict) ## 2) / &iter) ;
    %end ;

    create dir_rslt.predict_stats from stats [colname = statnames] ;
    append from stats ;
    close dir_rslt.predict_stats ;
 
  quit ;

  %if &iter = &n_boot_iter %then %make_stats ;

%mend compile_stats ;

/* ---------------------------------------------------------------------- */

/* Macro takes parametric and mean bootstrap predictions and computes an
   unbiased estimate. It also takes error variance and bootstrap variance
   and parametric predictions to compute prediction error for each prediction. */

%macro make_stats ;

  proc iml ;

    /* Module determines the column locations within a master list for each element of a row vector search list.
       If the search element is not in the master list, the column location is given as zero. */  

    start locin(search_list,master_list) ;
      if type(master_list) ^= {u} & type(search_list) ^= {u} then do ;
        do i = 1 to ncol(search_list) ;
          if i = 1 then loc_list = loc(master_list = search_list[,i]) ;
          else loc_list = loc_list || loc(master_list = search_list[,i]) ;
        end ;
      end ;
      else loc_list = 0 ;
      return(loc_list) ;
    finish ;

    predlst = {&predlst} ;
    exclude_list = locin({&retrans_exclude_list},predlst) ;

    m_statnames = compress({mean_} + predlst) ;
    v_statnames = compress({var_} + predlst) ;
    s_statnames = compress({se_} + predlst) ;
    statnames = m_statnames || s_statnames ;

    use dir_rslt.predict ;
    read all var predlst into parm_predict ;
    read all var {&depvar} into depvar ;
    close dir_rslt.predict ;

    use dir_rslt.predict_stats ;
    read all var m_statnames into m_boot_predict ;
    read all var v_statnames into v_boot_predict ;
    close dir_rslt.predict_stats ;

    use dir_rslt.summary_betaest ; 
    read all var {mean_exp_weighted_error} into mean_exp_weighted_error ;
    read all var {var_exp_weighted_error} into var_exp_weighted_error ;
    close dir_rslt.summary_betaest ;

    /* Create the unbiased estimate of the mean (note, must set denominator to 1 if the mean
       of the bootstrap estimates is zero or there would be an overflow condition - the parametric
       estimate will be zero as well, so normalizing the denominator to 1 has no effect on the
       bias-corrected estimate) */
    m_boot_predict = (parm_predict ## 2) / choose(m_boot_predict = 0,1,m_boot_predict) ;

    /* The coefficient of variation for the two errors - first the sample error and then
       the model error. Model error is excluded for prediction variables having no model error
       component (the exclude list variables) and for reaches with monitoring stations. */
    cv_model_error = var_exp_weighted_error / mean_exp_weighted_error ## 2 ;
    cv_error = j(nrow(v_boot_predict),ncol(v_boot_predict),cv_model_error) ;
    if any(exclude_list) then cv_error[,exclude_list] = 0 ;
    %if %upcase(&if_adjust) = YES %then %do ;
      cv_error[loc(depvar ^= .),] = 0 ;
    %end ;
    /* Add in the sample error component of the CV - Note, m_boot_predict is zero whenever
       parm_predict is zero. Because the final variance is computed from a product involving 
       m_boot_predict, normalizing the variance here 1 when parm_predict is zero has no effect on
       the final estimate. */
    cv_error = (v_boot_predict # (1 / choose(parm_predict = 0,1,parm_predict) ## 2)) + cv_error ;

    /* Create the bootstrap standard error of the prediction */
    se_boot_predict = m_boot_predict # sqrt(cv_error) ;

    boot_stats = m_boot_predict || se_boot_predict ;

    create dir_rslt.predict_stats from boot_stats [colname = statnames] ;
    append from boot_stats ;
    close dir_rslt.predict_stats ;

  quit ;

%mend make_stats ;

/* ---------------------------------------------------------------------- */

/* Macro creates the model component of the parametric prediction variables - 
   to be used in generating the bootstrap confidence intervals. */

%macro set_model_parm_predict ;

  proc iml ;

    /* Module determines the column locations within a master list for each element of a row vector search list.
       If the search element is not in the master list, the column location is given as zero. */  

    start locin(search_list,master_list) ;
      if type(master_list) ^= {u} & type(search_list) ^= {u} then do ;
        do i = 1 to ncol(search_list) ;
          if i = 1 then loc_list = loc(master_list = search_list[,i]) ;
          else loc_list = loc_list || loc(master_list = search_list[,i]) ;
        end ;
      end ;
      else loc_list = 0 ;
      return(loc_list) ;
    finish ;

    predlst = {&predlst} ;
    exclude_list = locin({&retrans_exclude_list},predlst) ;

    use dir_rslt.predict ;
    read all var predlst into pred_values ;
    close predict ;

    use boot_betaest ;
    read var {mean_exp_weighted_error} into mean_exp_weighted_error ;
    close boot_betaest ;

    use indata ;
    read all var {&depvar} into depvar ;
    close indata ;

    inv_mean_boot_error = j(nrow(pred_values),ncol(pred_values),1/mean_exp_weighted_error) ;
    if any(exclude_list) then inv_mean_boot_error[,exclude_list] = 1 ;
    %if %upcase(&if_adjust) = YES %then %do ;
      inv_mean_boot_error[loc(depvar ^= .),] = 1 ;
    %end ;
    pred_values = pred_values # inv_mean_boot_error ;

    create model_parm_predict from pred_values [colname = predlst] ;
    append from pred_values ;
    close model_parm_predict ;

  quit ;

%mend set_model_parm_predict ;

/* ---------------------------------------------------------------------- */

/* Macro compiles individual bootstrap predictions, storing those 
   predictions that appear in the tails of the empirical distribution.
   After setting the storage data sets, one data set for each predicted
   variable, the macro adds data to the storage data sets. Separate storage
   data sets are constructed for each predicted variable, indexed by the
   name of the predicted variable (value_name). Each storage data set
   contains N variables, indexed by value_1 to value_N, where N is the
   number of values stored in the tails of the empirical distribution. */ 

%macro compile_ci ;

  %if &iter <= &n_store %then %compile_ci_set_storage ;
  %if &iter >= &n_store %then %do ;
    %let i_pred = 1 ;
    %let pred = %scan(&predlst,&i_pred,' ') ;
    %do %while (%length(&pred) > 0) ;
      %if &iter = &n_store %then %compile_ci_sort_storage(&pred) ;
      %else %compile_ci_add_data(&pred) ;
      %if &iter = &n_boot_iter %then %do ;
        %if &i_pred = 1 or %sysfunc(exist(model_parm_predict)) = 0 %then %set_model_parm_predict ;
        %compile_ci_get_bounds(&pred) ;
      %end ;
      %let i_pred = %eval(&i_pred + 1) ;
      %let pred = %scan(&predlst,&i_pred,' ') ;
    %end ;
  %end ;

%mend compile_ci ;

/* ---------------------------------------------------------------------- */

/* Macro sets up the storage datasets used to store tail values from the bootstrap 
   iterative predictions for all variables included in the list predlst. Macro
   creates a set of data sets indexed according to the name of the prediction 
   variable, each data set containing the predictions from the first n iterations
   of the bootstrap procedure, where n is the number of values stored based on
   the coverage probability desired and number of bootstrap replications to be performed. */

%macro compile_ci_set_storage ;

  proc iml ;

    /* Module determines the column locations within a master list for each element of a row vector search list.
       If the search element is not in the master list, the column location is given as zero. */  

    start locin(search_list,master_list) ;
      if type(master_list) ^= {u} & type(search_list) ^= {u} then do ;
        do i = 1 to ncol(search_list) ;
          if i = 1 then loc_list = loc(master_list = search_list[,i]) ;
          else loc_list = loc_list || loc(master_list = search_list[,i]) ;
        end ;
      end ;
      else loc_list = 0 ;
      return(loc_list) ;
    finish ;

    varnames_old = "value_1":"value_%eval(&iter - 1)" ;
    varnames_new = varnames_old || {value_&iter} ;

    predlst = {&predlst} ;
    exclude_list = locin({&retrans_exclude_list},predlst) ;

    use predict ;
    read all var predlst into pred_values ;
    close predict ;

    use dir_rslt.resids ;
    read all var {boot_resid} into errors ;
    close dir_rslt.resids ;

    use boot_betaest ;
    read var {mean_exp_weighted_error} into mean_exp_weighted_error ;
    close boot_betaest ;

    use indata ;
    read all var {&depvar} into depvar ;
    close indata ;

    /* Apply randomly selected bootstrap errors - using consistent errors across variables (within
       a given reach, each variable gets the same applied error). Exclude error application for
       prediction variables that are on the exclude list (variables that are not in mass units).
       Also exclude error application for observed reaches if the if_adjust switch is on. Note,
       the boot_resids factor divides the randomly selected exponentiated residual by the 
       mean of the exponentiated residuals. This negates the presence of the retransformation bias
       correction in the pred_values read in from predict. Note also that the negative value of the
       randomly selected error is used to ensure the derived empirical distribution is for the the 
       log of the model prediction MINUS the model error (see the documentation for details). */
 
    boot_errors = repeat(exp(-errors[ceil(nrow(errors) * ranuni(j(nrow(pred_values),1,&seed_2))) <> 1,]),1,ncol(pred_values)) / mean_exp_weighted_error ;
    if any(exclude_list) then boot_errors[,exclude_list] = 1 ;
    %if %upcase(&if_adjust) = YES %then %do ;
      boot_errors[loc(depvar ^= .),] = 1 ;
    %end ;
    pred_values = pred_values # boot_errors ;

    %let i_var = 1 ;
    %let var = %scan(&predlst,&i_var,' ') ;
    %do %while (%length(&var) > 0) ;
      %if &iter = 1 %then %do ;
        values = pred_values[,&i_var] ;
        create dir_rslt.store_&var from values [colname = {value_&iter}] ;
        append from values ;
        close dir_rslt.store_&var ;
      %end ;
      %else %do ;
        use dir_rslt.store_&var ;
        read all var varnames_old into values ;
        close dir_rslt.store_&var ;
        values = values || pred_values[,&i_var] ;
        create dir_rslt.store_&var from values [colname = varnames_new] ;
        append from values ;
        close dir_rslt.store_&var ;
      %end ;
      %let i_var = %eval(&i_var + 1) ;
      %let var = %scan(&predlst,&i_var,' ') ;
    %end ;

    /* Module takes as input a vector called list and a row vector called items and
       returns a row vector corresponding to the row indices for each element in
       list that matches one of the elements in items. If there are no matches,
       a null vector is returned. */
    start seek(list,items) ;
      y = j(nrow(list),1,0) ;
      do i = 1 to ncol(items) ;
        y = (y | (list = items[i])) ;
      end ;
      return(loc(y)) ;
    finish ;

    %if %length(&boot_detail_reaches) > 0 %then %do ;
      /* Set the list of variables to be output to boot_detail - list always includes del_frac and boot_error. Other variables
         depend on the specification of boot_detail_predvars. */
      sel_pred = loc(upcase(predlst) = 'DEL_FRAC') ;
      %if %length(&boot_detail_predvars) = 0 or %index(%upcase(&boot_detail_predvars),CUM) > 0 %then %do ;
        if type(sel_pred) = {u} then sel_pred = loc(scan(upcase(predlst),1,'_') = "PLOAD" & scan(upcase(predlst),2,'_') ^= 'INC' & scan(upcase(predlst),2,'_') ^= 'ND') ;
        else sel_pred = sel_pred || loc(scan(upcase(predlst),1,'_') = "PLOAD" & scan(upcase(predlst),2,'_') ^= 'INC' & scan(upcase(predlst),2,'_') ^= 'ND') ;
      %end ;
      %if %index(%upcase(&boot_detail_predvars),INC) > 0 %then %do ;
        if type(sel_pred) = {u} then sel_pred = loc(scan(upcase(predlst),1,'_') = "PLOAD" & scan(upcase(predlst),2,'_') = 'INC') ;
        else sel_pred = sel_pred || loc(scan(upcase(predlst),1,'_') = "PLOAD" & scan(upcase(predlst),2,'_') = 'INC') ;
      %end ;
      if type(sel_pred) ^= {u} then do ;
        use indata ;
        read all var {&waterid} into &waterid ;
        close indata ;
        /* Create a row vector of reaches to be included in the boot_detail data set. */
        %if %upcase(&boot_detail_reaches) = ALL %then %let select = ;
        %else %do ;
          %let select = select ;
          select = seek(&waterid,{&boot_detail_reaches}) ;
        %end ;
        /* If there are selected reaches then process */
        if %length(&select) = 0 | type(select) ^= {u} then do ;
          varnames = {iter &waterid varname new_value} ;
          if any(exclude_list) then sel_boot = remove(1:ncol(predlst),exclude_list)[,1] ;
          else sel_boot = 1 ;
          /* Revise the boot_errors to exclude the mean_exp_weighted_error in the denominator - method of
             correction sets the boot_error to 1 if the reach is monitored and if_adjust is set to yes. */
          boot_errors = boot_errors[&select,sel_boot] # mean_exp_weighted_error ;
          %if %upcase(&if_adjust) = YES %then %do ;
            iffix = loc(depvar[&select,] ^= .) ;
            if type(iffix) ^= {u} then do ;
              boot_errors[iffix,] = . ;
            end ;
          %end ;
          &waterid = &waterid[&select,] ;
          nrch = nrow(&waterid) ;
          new_value = boot_errors ;
          varname = j(nrow(new_value),1,"BOOT_ERROR") ;
          &waterid = &waterid // shape(repeat(&waterid,1,ncol(sel_pred)),0,1) ;
          new_value = new_value // shape(pred_values[&select,sel_pred],0,1) ;
          varname = varname // shape(repeat(predlst[,sel_pred],nrch,1),0,1) ;
          iter = j(nrow(new_value),1,&iter) ;
          %if &iter = 1 %then %let control = create ;
          %else %let control = edit ;
          &control dir_rslt.boot_detail var varnames ;
          append ;
          close dir_rslt.boot_detail ;
        end ;
      end ;
    %end ;

  quit ;

%mend compile_ci_set_storage ;

/* ---------------------------------------------------------------------- */

/* Macro sorts values across predicted realizations in a data set of stored predictions for 
   determination of confidence intervals for each observation. Data sets are indexed 
   according to the variable name. Predicted realizations are indexed according to 
   bootstrap iteration number. */

%macro compile_ci_sort_storage(pred_var) ;

  %if %upcase(&if_test_predict) = YES %then %do ;
    data test_boot ; set dir_rslt.store_&pred_var (firstobs = &test_obs obs = &test_obs) ;
      varname = "boot_&pred_var" ;
    run ;
    proc transpose data = test_boot out = test_boot ;
      var value_1 - value_&n_store ;
      id varname ;
    run ;
    data dir_rslt.test_predict ; merge dir_rslt.test_predict test_boot (keep = boot_&pred_var) ;
    run ;
  %end ;

  %if ^(%length(&target) = 0 and %upcase(&pred_var) = DEL_FRAC) %then %do ;

    proc iml ;

      varnames = "value_1":"value_&n_store" ;

      use dir_rslt.store_&pred_var ;
      read all var varnames into value ;
      close dir_rslt.store_&pred_var ;

      do i_reach = 1 to nrow(value) ;
        if all(value[i_reach,] ^= .) then value[i_reach,rank(value[i_reach,])] = value[i_reach,] ;
        else value[i_reach,] = . ;
      end ;

      create dir_rslt.store_&pred_var from value [colname = varnames] ;
      append from value ;
      close dir_rslt.store_&pred_var ;

    quit ;

  %end ;


%mend compile_ci_sort_storage ;

/* ---------------------------------------------------------------------- */

/* Macro takes bootstrap iteration predictions for a given variable (specified as input to the macro) and 
   adds to the storage data set for that variable. Predictions are stored if the value is in the tails of
   the distribution. */

%macro compile_ci_add_data(pred_var) ;

  proc iml ;

    /* Module determines the column locations within a master list for each element of a row vector search list.
       If the search element is not in the master list, the column location is given as zero. */  

    start locin(search_list,master_list) ;
      if type(master_list) ^= {u} & type(search_list) ^= {u} then do ;
        do i = 1 to ncol(search_list) ;
          if i = 1 then loc_list = loc(master_list = search_list[,i]) ;
          else loc_list = loc_list || loc(master_list = search_list[,i]) ;
        end ;
      end ;
      else loc_list = 0 ;
      return(loc_list) ;
    finish ;

    varnames = "value_1":"value_&n_store" ;

    use indata ;
    read all var {&depvar} into depvar ;
    close indata ;

    use predict ;
    read all var {&pred_var} into new_value ;
    close predict ;

    use dir_rslt.resids ;
    read all var {boot_resid} into errors ;
    close dir_rslt.resids ;

    use boot_betaest ;
    read var {mean_exp_weighted_error} into mean_exp_weighted_error ;
    close boot_betaest ;

    use dir_rslt.store_&pred_var ;
    read all var varnames into value ;
    close dir_rslt.store_&pred_var ;

    /* Randomly assign errors to each prediction (if variable is in mass units). If reach
       is monitored, and adjustment to observed values is requested, then set the multiplicative
       error to 1. Also, the predictions that do not depend on mass (e.g., delivery ratio) do 
       not have the error applied to their prediction. Note, the boot_resids factor divides the 
       randomly selected exponentiated residual by the mean of the exponentiated residuals. This 
       negates the presence of the retransformation bias correction in the pred_values read in 
       from predict. Note also that the negative value of the randomly selected error is used
       to ensure the derived empirical distribution is for the the log of the model prediction
       MINUS the model error (see the documentation for details). */

    %if %index(%upcase(&retrans_exclude_list),%upcase(&pred_var)) = 0 %then %do ;
      boot_errors = exp(-errors[ceil(nrow(errors) * ranuni(j(nrow(new_value),1,&seed_2))) <> 1,]) / mean_exp_weighted_error ;
      %if %upcase(&if_adjust) = YES %then %do ;
        boot_errors[loc(depvar ^= .),] = 1 ;
      %end ;
      new_value = new_value # boot_errors ;
    %end ;

    %if %upcase(&if_test_predict) = YES %then %do ;
      test_value = new_value[&test_obs,] ;
      create test from test_value [colname = {boot_&pred_var}] ;
      append from test_value ;
      close test ;
    %end ;
      
    n_reach = nrow(predict) ;
    n_low = &n_low ;
    n_hi = &n_hi ;
    n_drop = n_low + 1 ;

    /* The following code:
       (1) Sets the 'keep' indicies which drops the n_low + 1 element of the 1:(n_store + 1) vector.
       (2) determines row indices where the new randomly generated value (new_value) is less than
           the largest value in the lower tail or greater than the smallest value in the upper tail
       (3) for rows where the above condition is met, the new value is first appended to the end
           of the row, the row is then reordered.
       (4) the reordered row is purged of the 'drop' element corresponding to either the now
           superfluous largest value in the n_low group (if new_value is less than this value),
           or the now superfluous smallest value in the n_hi group (if new_value is greater than
           this value).
       (5) Sets the tails to missing values if either the original tail contains missing values or
           the new value is missing.
       The final value list correctly updates the stored values in light of the new value. */ 
    keep = remove(1:(n_low + n_hi + 1),n_drop) ;
    locval = loc(((value[,n_low] > new_value) | (new_value > value[,n_drop])) & (value[,n_drop] ^= .)) ;
    if type(locval) ^= {u} then do ;
      y = value[locval,] || new_value[locval,] ;
      ncy = ncol(y) ;
      do i = 1 to nrow(y) ;
        if y[i,ncy] = . then y[i,] = . ;
        else y[i,rank(y[i,])] = y[i,] ;
      end ;
      value[locval,] = y[,keep] ;
    end ;

    create dir_rslt.store_&pred_var from value [colname = varnames] ;
    append from value ;
    close dir_rslt.store_&pred_var ;

    /* Module takes as input a vector called list and a row vector called items and
       returns a row vector corresponding to the row indices for each element in
       list that matches one of the elements in items. If there are no matches,
       a null vector is returned. */
    start seek(list,items) ;
      y = j(nrow(list),1,0) ;
      do i = 1 to ncol(items) ;
        y = (y | (list = items[i])) ;
      end ;
      return(loc(y)) ;
    finish ;

    /* Create the detailed bootstrap output file, if requested */
    %if %length(&boot_detail_reaches) > 0 %then %do ;
      predlst = {&predlst} ;
      sel_pred = loc(upcase(predlst) = 'DEL_FRAC') ;
      %if %length(&boot_detail_predvars) = 0 or %index(%upcase(&boot_detail_predvars),CUM) > 0 %then %do ;
        if type(sel_pred) = {u} then sel_pred = loc(scan(upcase(predlst),1,'_') = "PLOAD" & scan(upcase(predlst),2,'_') ^= 'INC' & scan(upcase(predlst),2,'_') ^= 'ND') ;
        else sel_pred = sel_pred || loc(scan(upcase(predlst),1,'_') = "PLOAD" & scan(upcase(predlst),2,'_') ^= 'INC' & scan(upcase(predlst),2,'_') ^= 'ND') ;
      %end ;
      %if %index(%upcase(&boot_detail_predvars),INC) > 0 %then %do ;
        if type(sel_pred) = {u} then sel_pred = loc(scan(upcase(predlst),1,'_') = "PLOAD" & scan(upcase(predlst),2,'_') = 'INC') ;
        else sel_pred = sel_pred || loc(scan(upcase(predlst),1,'_') = "PLOAD" & scan(upcase(predlst),2,'_') = 'INC') ;
      %end ;
      if type(sel_pred) ^= {u} then do ;
        if any({&pred_var} = predlst[,sel_pred]) then do ;
          use indata ;
          read all var {&waterid} into &waterid ;
          close indata ;
          %if %upcase(&boot_detail_reaches) = ALL %then %let select = ;
          %else %do ;
            %let select = select ;
            select = seek(&waterid,{&boot_detail_reaches}) ;
          %end ;
          if %length(&select) = 0 | type(select) ^= {u} then do ;
            &waterid = &waterid[&select,] ;
            new_value = new_value[&select,] ;
            iter = j(nrow(new_value),1,&iter) ;
            varname = j(nrow(new_value),1,"%upcase(&pred_var)") ;
            varnames = {iter &waterid varname new_value} ;
            edit dir_rslt.boot_detail var varnames ;
            append ;
            if upcase("&pred_var") = upcase(predlst[sel_pred[1]]) then do ;
              %if %index(%upcase(&retrans_exclude_list),%upcase(&pred_var)) = 0 %then %do ; 
                new_value = boot_errors[&select,] # mean_exp_weighted_error ;
              %end ;
              %else %do ;
                /* Create the bootstrap errors if the first variable output in the boot_detail data
                   set is in the exclude list, and therefore would not have an applied bootstrap error
                   term. The boot_error is created in this case just so it can be included in the
                   boot_detail data set, not because it is used to modify the exclude list variable. */
                new_value = exp(-errors[ceil(nrow(errors) * ranuni(j(nrow(value),1,&seed_2))) <> 1,])[&select,] ;
              %end ;
              %if %upcase(&if_adjust) = YES %then %do ;
                iffix = loc(depvar[&select,] ^= .) ;
                if type(iffix) ^= {u} then do ;
                  new_value[iffix,] = . ;
                end ;
              %end ;
              varname = j(nrow(new_value),1,"BOOT_ERROR") ;
              append ;
            end ;
            close dir_rslt.boot_detail ;
          end ;
        end ;
      end ;
    %end ;

  quit ;

  /* Merge the bootstrap predictions (having the boot prefix) into the test_predict
     data set. The predictions are merged in at the last observation of the 
     test_predict data set so that the bootstrap predictions (containing the randomly
     applied errors - where appropriate) are aligned with the raw predictions for the
     same bootstrap iteration. */

  %if %upcase(&if_test_predict) = YES %then %do ;
    data dir_rslt.test_predict ; set dir_rslt.test_predict nobs = numobs ;
      if _n_ = numobs then set test ;
    run ;
  %end ;

%mend compile_ci_add_data ;

/* ---------------------------------------------------------------------- */

/* Macro gets the upper and lower bounds of the prediction confidence interval from 
   the storage data sets and merges the mean and variance statistics. Note that 
   the confidence interval numerator is based on squared value of the model 
   component of the parametric prediction - not the squared value of the parametric
   prediction (see the documentation). */

%macro compile_ci_get_bounds(pred_var) ;

  data boot_predict ; 
    merge model_parm_predict (keep = &pred_var)
          dir_rslt.predict_stats (keep = mean_&pred_var se_&pred_var) 
          dir_rslt.store_&pred_var (keep = value_&n_low value_%eval(&n_low + 1)) ;
    if value_%eval(&n_low + 1) ^= 0 then ci_lo_&pred_var = (&pred_var ** 2) / value_%eval(&n_low + 1) ;
    else ci_lo_&pred_var = 0 ;
    if value_&n_low ^= 0 then ci_hi_&pred_var = (&pred_var ** 2) / value_&n_low ;
    else ci_hi_&pred_var = 0 ;
    drop value_&n_low value_%eval(&n_low + 1) &pred_var ;
  run ;

  data dir_rslt.predict ; merge dir_rslt.predict boot_predict ;

  proc datasets library = dir_rslt nolist ;
    delete store_&pred_var ;
  run ;

%mend compile_ci_get_bounds ;

/* ---------------------------------------------------------------------- */

/* Macro merges ancillary reach information with predictions and returns
   prediction file to its original state (sorted by hydseq). Note, only
   reaches in the prediction set are retained to insure compatability
   with subsequent prediction appendage. Also, reaches in indata without
   from-to nodes correspond to closed basins. These reaches will not have
   corresponding line work in e2rf1 and would not be mapped so inclusion
   for mapping purposes is pointless. 

   Macro also prints out two summary reports, the first provides summary
   statistics on reach upstream and incremental flux, delivery fraction
   and source shares. The second gives distributional statistics for
   incremental yield by land-use class. 

   Note that summarized predictions are either the parametric predictions
   or the bootstrap predictions - determined by the header macro variable
   if_print_boot_predictions. */

%macro summarize_predict ;

  proc sort data = dir_rslt.predict force ; by &waterid ;

  proc datasets library = dir_rslt nolist ;
    delete predict_stats ;
  run ;

  data dir_rslt.predict (drop = varnum value)
    %if %upcase(&if_distribute_yield_by_land_use) = YES %then 
      dir_rslt.LU_yield_percentiles (keep = LU_class inc_total_yield) ;
    dir_rslt.summary_predict (keep = varnum value) ;
    retain &reach_priority_list ;
    merge ancillary dir_rslt.predict (in = in_predict) ; by &waterid ;
    if in_predict ;

    /* Create yields and the fraction delivered for mapping */

    if &tot_area > 0 then total_yield = &predict_prefix.pload_total / (&tot_area * 100) ;
    else total_yield = . ;
    label total_yield = "Total Upstream Yield (&numerator_load_units./ha/yr)" ;
    varnum = 1 ;
    value = total_yield ;
    if value ^= . then output dir_rslt.summary_predict ;

    if &inc_area > 0 then inc_total_yield = &predict_prefix.pload_inc_total / (&inc_area * 100) ;
    else inc_total_yield = . ;
    label inc_total_yield = "Total Incremental Yield (&numerator_load_units./ha/yr)" ;
    varnum = 2 ;
    value = inc_total_yield ;
    if value ^= . then output dir_rslt.summary_predict ;

    if &mean_flow > 0 then concentration = &adjust_units * &adjust_conc_units * &predict_prefix.pload_total / (893.585 * &mean_flow) ;
    else concentration = . ;
    %if %upcase(&if_flow_units_metric) = YES %then concentration = concentration * 893.585 / 3153.6 ;;
    label concentration = Mean Flow-Weighted Concentration (&concentration_units) ;
    varnum = 3 ;
    value = concentration ;
    if value ^= . then output dir_rslt.summary_predict ;

    if &predict_prefix.del_frac = 0 then map_del_frac = . ;
    else map_del_frac = 100 * &predict_prefix.del_frac ;
    label map_del_frac = 'Reach Flux Share Delivered (%)' ;
    varnum = 4 ;
    value = map_del_frac ;
    if value ^= . then output dir_rslt.summary_predict ;

    /* Create prediction variable labels */

    label pload_total = "Total Flux (&load_units)" ;
    label pload_nd_total = "Total Non-Decayed (ND) Flux (&load_units)" ;
    label pload_inc_total = "Total Incremental (Inc) Flux (&load_units)" ;
    label del_frac = "Share of Total Flux Delivered" ;
    label res_decay = "Flux Decayed in Reservoir (&load_units)" ;

    %if &n_boot_iter > 0 %then %do ;
      label mean_pload_total  = "Bias-Adjusted Total Flux (&load_units)" ;
      label se_pload_total    = "Std Error Total Flux (&load_units)" ;
      label ci_lo_pload_total = "T Flux Conf Int (CI) Lo Bnd (LB) (&load_units)" ;
      label ci_hi_pload_total = "T Flux Conf Int (CI) Up Bnd (UB) (&load_units)" ;

      label mean_pload_nd_total  = "Bias-Adjusted Total ND Flux (&load_units)" ;
      label se_pload_nd_total    = "Std Error Total ND Flux (&load_units)" ;
      label ci_lo_pload_nd_total = "Total ND Flux CI-LB (&load_units)" ;
      label ci_hi_pload_nd_total = "Total ND Flux CI-UB (&load_units)" ;

      label mean_pload_inc_total  = "Bias-Adjusted Total Inc Flux (&load_units)" ;
      label se_pload_inc_total    = "Std Error Total Inc Flux (&load_units)" ;
      label ci_lo_pload_inc_total = "Total Inc Flux CI-LB (&load_units)" ;
      label ci_hi_pload_inc_total = "Total Inc Flux CI-UB (&load_units)" ;

      label mean_del_frac  = "Bias-Adjusted Share Total Flux Delivered" ;
      label se_del_frac    = "Std Error Share Total Flux Delivered" ;
      label ci_lo_del_frac = "Share Total Flux Delivered CI-LB" ;
      label ci_hi_del_frac = "Share Total Flux Delivered CI-UB" ;

      label mean_res_decay  = "Bias-Adj Flux Decayed in Resrvr (&load_units)" ;
      label se_res_decay    = "Std Err Flux Decayed in Resrvr (&load_units)" ;
      label ci_lo_res_decay = "Flux Decayed in Reservoir CI-LB (&load_units)" ;
      label ci_hi_res_decay = "Flux Decayed in Reservoir CI-UB (&load_units)" ;
    %end ;

    /* Compute the source shares and create labels for source variables */

    %let i_src = 1 ;
    %let src_var = %scan(&srcvar,&i_src,' ') ;
    %let sh_src_var_lst = ;
    %do %while (%length(&src_var) > 0) ;

      if &predict_prefix.pload_inc_total > 0 then 
        sh_&src_var = 100 * &predict_prefix.pload_inc_&src_var / &predict_prefix.pload_inc_total ;
      else sh_&src_var = . ;
      label sh_&src_var = "Share of Inc Flux from %upcase(&src_var) (%)" ;

      label pload_&src_var = "%upcase(&src_var) Flux (&load_units)" ;
      label pload_nd_&src_var = "%upcase(&src_var) Non-Decayed (ND) Flux (&load_units)" ;
      label pload_inc_&src_var = "%upcase(&src_var) Incremenatal Flux (&load_units)" ;

      %if &n_boot_iter > 0 %then %do ;
        label mean_pload_&src_var = "Bias-Adj %upcase(&src_var) Flux (&load_units)" ;
        label se_pload_&src_var = "Std Error %upcase(&src_var) Flux (&load_units)" ;
        label ci_lo_pload_&src_var = "%upcase(&src_var) Flux CI-LB (&load_units)" ;
        label ci_hi_pload_&src_var = "%upcase(&src_var) Flux CI-UB (&load_units)" ;

        label mean_pload_nd_&src_var = "Bias-Adj %upcase(&src_var) ND Flux (&load_units)" ;
        label se_pload_nd_&src_var = "Std Error %upcase(&src_var) ND Flux (&load_units)" ;
        label ci_lo_pload_nd_&src_var = "%upcase(&src_var) ND Flux CI-LB (&load_units)" ;
        label ci_hi_pload_nd_&src_var = "%upcase(&src_var) ND Flux CI-UB (&load_units)" ;

        label mean_pload_inc_&src_var = "Bias-Adj %upcase(&src_var) Inc Flux (&load_units)" ;
        label se_pload_inc_&src_var = "Std Error %upcase(&src_var) Inc Flux (&load_units)" ;
        label ci_lo_pload_inc_&src_var = "%upcase(&src_var) Inc Flux CI-LB (&load_units)" ;
        label ci_hi_pload_inc_&src_var = "%upcase(&src_var) Inc Flux CI-UB (&load_units)" ;
      %end ;

      varnum = varnum + 1 ;
      value = sh_&src_var ;
      if value ^= . then output dir_rslt.summary_predict ;

      %let i_src = %eval(&i_src + 1) ;
      %let src_var = %scan(&srcvar,&i_src,' ') ;
    %end ;

    %if %upcase(&if_distribute_yield_by_land_use) = YES %then 
      if LU_class ^= '' then output dir_rslt.LU_yield_percentiles ; ;
    output dir_rslt.predict ;
  run ;

  proc sort data = dir_rslt.predict ; by &hydseq ;
  run ;

  %if %upcase(&if_output_to_tab) = YES %then %out_tab(dir_rslt.predict,&home_results.\predict) ;
  %if %upcase(&if_output_to_tab) = YES %then %out_tab(dir_rslt.test_data,&home_results.\test_data) ;
  %if %upcase(&if_output_to_tab) = YES %then %out_tab(dir_rslt.test_predict,&home_results.\test_predict) ;

  /* Generate table of summary statistics for yields, share delivered to estuaries
     and source shares */

  data varnames ;
    length varname $ 30 ;
    varnum = 1 ;
    varname = "Upstream Yield (&numerator_load_units./ha/yr)" ;
    output ;
    varnum = 2 ;
    varname = "Incremental Yield (&numerator_load_units./ha/yr)" ;
    output ;
    varnum = 3 ;
    varname = "Flow-Weighted Conc (&concentration_units)" ;
    output ;
    varnum = 4 ;
    varname = "Reach Flux Share Delivered (%)" ;
    output ;
    %let i_src = 1 ;
    %let src_var = %scan(&srcvar,&i_src,' ') ;
    %let sh_src_var_lst = ;
    %do %while (%length(&src_var) > 0) ;
      varnum = varnum + 1 ;
      varname = "%upcase(&src_var) Source Share (%)" ;
      output ;
      %let i_src = %eval(&i_src + 1) ;
      %let src_var = %scan(&srcvar,&i_src,' ') ;
    %end ;
    label varname = 'Variable' ;
  run ;

  proc sort data = varnames ; by varnum ;
    
  proc sort data = dir_rslt.summary_predict ; by varnum ;

  proc univariate data = dir_rslt.summary_predict noprint ;
    var value ;
    by varnum ;
    output out = dir_rslt.summary_predict n = n_watersheds mean = mean std = std
      pctlpts = 10, 25, 50, 75, 90 pctlpre = p_ range = range ;
  run ;

  proc datasets library = dir_rslt nolist ;
    modify summary_predict ;
      label n_watersheds = 'Number of Watersheds'
            mean = 'Mean' 
            std = 'Standard Deviation'
            p_10 = '10th'
            p_25 = '25th'
            p_50 = '50th'
            p_75 = '75th'
            p_90 = '90th'
            range = 'Range' ;
  run ;

  data dir_rslt.summary_predict (drop = varnum) ; merge varnames dir_rslt.summary_predict ; by varnum ;
  run ;

  %if %upcase(&if_output_to_tab) = YES %then %out_tab(dir_rslt.summary_predict,&home_results.\summary_predict) ;

  title1 "Mean, Standard Deviation, Percentiles, and Range of Reach Predictions for" ;
  title2 "Upstream Yield, Incremental Yield, Fraction of Reach Flux Delivered to" ;
  title3 "Estuaries, and Shares of Incremental Flux Attributed to Various Sources" ;

  footnote1 "Incremental flux and yield refer to flux and yield generated in a given reach, excluding" ;
  footnote2 "flux and yield generated in upstream reaches.                                           " ;

  proc print data = dir_rslt.summary_predict label noobs ;
    format n_watersheds comma6.0 mean std p_: comma8.2 range comma10.2 ;
    var n_watersheds mean std p_10 p_25 p_50 p_75 p_90 range ;
    id varname ;
  run ;

  title1 ;
  footnote1 ;
  run ;

  /* Generate percentiles of incremental yield by land-use type - if requested */

  %if %upcase(&if_distribute_yield_by_land_use) = YES %then %do ;

    proc sort data = dir_rslt.LU_yield_percentiles ; by LU_class ;

    proc univariate data = dir_rslt.LU_yield_percentiles noprint ;
      var inc_total_yield ;
      by LU_class ;
      output out = dir_rslt.LU_yield_percentiles n = inc_total_yield_n range = inc_total_yield_range
        pctlpre = inc_total_yield_p pctlpts = 10,25,50,75,90 ;
    run ;

    proc datasets library = dir_rslt nolist ;
      modify LU_yield_percentiles ;
        label inc_total_yield_n = 'Number of Watersheds'
              inc_total_yield_range = 'Range of Values'
              inc_total_yield_p10 = '10th'
              inc_total_yield_p25 = '25th'
              inc_total_yield_p50 = '50th'
              inc_total_yield_p75 = '75th'
              inc_total_yield_p90 = '90th' ;
    run ;

    %if %upcase(&if_output_to_tab) = YES %then %out_tab(dir_rslt.LU_yield_percentiles,&home_results.\LU_yield_percentiles) ;

    title1 "Distribution of Yield Exported from SPARROW Watersheds" ;
    title2 "(&numerator_load_units./ha/yr)" ;
    footnote1 "The land-cover types represent the following percentages of land area in SPARROW watersheds:" ;
    footnote2 "&LU_class_footnote" ;

    proc print data = dir_rslt.LU_yield_percentiles label noobs ;
      format inc_total_yield_: comma10.2 ;
      format inc_total_yield_n comma5.0 ;
      var inc_total_yield_n   inc_total_yield_p10 inc_total_yield_p25 inc_total_yield_p50 inc_total_yield_p75
          inc_total_yield_p90 inc_total_yield_range ;
      id lu_class ;
    run ;

    title1 "SPARROW Analysis" ;
    title2 ;
    footnote1 ;
    footnote2 ;
    run ;

  %end ;

  /* Reconfigure the boot_detail file to have individual columns for each included prediction variable. */
  %if %sysfunc(exist(dir_rslt.boot_detail)) %then %do ;
    proc sort data = dir_rslt.boot_detail ; by &waterid iter varname ;
    proc transpose data = dir_rslt.boot_detail out = dir_rslt.boot_detail ;
      by &waterid iter ;
      id varname ;
      var new_value ;
    run ;
    data dir_rslt.boot_detail ; set dir_rslt.boot_detail (drop = _name_) ;
    run ;
  %end ;

%mend summarize_predict ;

/* ---------------------------------------------------------------------- */
