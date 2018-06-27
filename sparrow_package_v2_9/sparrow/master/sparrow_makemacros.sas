/* Program: sparrow.sas
   Component: sparrow_makemacros.sas
   Date: 12/31/02
   Written by: Greg Schwarz

   Purpose: Macros used for defining macro variables.
*/

/* ---------------------------------------------------------------------- */

/* Macro counts the number of objects separated by a blank in a list */

%macro count(list) ;

  %let i_list = 1 ;
  %let var = %scan(&list,&i_list,' ') ;
  %do %while (%length(&var) > 0) ;
    %let i_list = %eval(&i_list + 1) ;
    %let var = %scan(&list,&i_list,' ') ;
  %end ;
  %eval(&i_list - 1)

%mend count ;

/* ---------------------------------------------------------------------- */

/* Macro checks that each element of ListA is in ListB. Returns the elements
   of ListA that are not in ListB. */

%macro check(ListA,ListB) ;
  %let ListB = %upcase(&ListB) ;
  %let excluded = ;
  %do ivar = 1 %to %count(&ListA) ;
    %let var = %scan(&ListA,&ivar,' ') ;
    %if %sysfunc(indexw(%quote(&ListB),%quote(%upcase(&var)))) = 0 %then %let excluded = &excluded &var ;
  %end ;
  &excluded

%mend check ;

/* ---------------------------------------------------------------------- */

/* Macro returns a list of numbers corresponding to the column
   positions of each element of "list" within "in". */

%macro col(in,list) ;
  %let return = ;
  %let ilist = 1 ;
  %let vlist = %scan(&list,&ilist,' ') ;
  %do %while (%length(&vlist) > 0) ;
    %let intret = ;
    %let imaster = 1 ;
    %let vmaster = %scan(&&&in,&imaster,' ') ;
    %do %while (%length(&vmaster) > 0) ;
      %if %upcase(&vlist) = %upcase(&vmaster) %then %do ;
        %let intret = &imaster ;
        %let vmaster = ;
      %end ;
      %else %do ;
        %let imaster = %eval(&imaster + 1) ;
        %let vmaster = %scan(&&&in,&imaster,' ') ;
      %end ;
    %end ;
    %let ilist = %eval(&ilist + 1) ;
    %let vlist = %scan(&list,&ilist,' ') ;
    %if %length(&intret) > 0 %then %let return = &return &intret ;
    %else %let return = &return 0 ;
  %end ;
  %if %length(&return) = 0 %then %let return = 0 ;
  {&return.} ;
%mend col ;

/* ---------------------------------------------------------------------- */

/* Macro takes the text intext and returns a quoted text that has
   replaced all occurrences of n_space or more successive spaces with 
   a new line slash (/). */

%macro form_it(intext,n_space) ;

  %let outtext = ;
  %let count = 0 ;
  %do i = 1 %to %length(&intext) ;
    %let charc = %substr(&intext,&i,1) ;
    %if %quote(&charc) = %quote( ) %then %do ;
      %let count = %eval(&count + 1) ;
    %end ;
    %else %do ;
      %if &count = 0 %then %let outtext = %str(&outtext&charc) ;
      %else %if &count < &n_space %then %let outtext = %str(&outtext &charc) ;
      %else %let outtext = %str(&outtext%" / %"&charc) ;
      %let count = 0 ;
    %end ;
  %end ;
  %let outtext = %str(%"&outtext%") ;
  &outtext

%mend form_it ;

/* ---------------------------------------------------------------------- */

/* Macro reads intext and delimits it into separate quoted strings with
   line breaks according to the delimiting character del_char. If intext
   contains numerous commas, it must be entered as a bquote string. If
   del_char is a special character (e.g. ";" or ",") it must be entered
   as a str string. */

%macro delim_it(intext,del_char) ;

  %let istat = 1 ;
  %let outtext = ;
  %let stat = %scan(&intext,&istat,%str(&del_char)) ;
  %do %while (%length(%bquote(&stat)) > 0) ;
    %if &istat = 1 %then %let outtext = %str(&stat) ;
    %else %let outtext = %str(&outtext &del_char%" / %"&stat) ;
    %let istat = %eval(&istat + 1) ;
    %let stat = %scan(&intext,&istat,%str(&del_char)) ;
  %end ;
  %let outtext = %str(%"&outtext%") ;
  &outtext

%mend delim_it ;

/* ---------------------------------------------------------------------- */

/* Macro appends a synopsis of the model specification to the model_specs.txt
   file in the results directory. Each synopsis is anontated by the date and
   time the model was run. Most, but not all, control variable specifications
   are included. */

%macro model_summary ;

  %let blist = %form_it(&betailst,3) ;
  %let dmlist = %delim_it(%bquote(&data_modifications),%str(;)) ;
  %let dldsgnlist = %delim_it(%bquote(&dlvdsgn),%str(,)) ;

  %global today now ;

  data temp ;
    file mod_spec mod ;
    today = date() ;
    now = time() ;
    call symput('today',trim(left(today))) ;
    call symput('now',trim(left(now))) ;
    put // "DATE: " today date9. " TIME: " now time8. // ;
    put "HOME_RESULTS: &home_results" ;
    put "HOME_DATA: &home_data" ;
    put "INDATA: &indata  IF_MAKE_INPUT_DATA: &if_make_input_data" / ;
    put "BETAILST:" / %unquote(&blist) / ;
    put "DEPVAR: &depvar  LS_WEIGHT: &ls_weight" ;
    put "SRCVAR: &srcvar" ;
    put "BSRCVAR: &bsrcvar" ;
    put "DLVVAR: &dlvvar" ;
    put "BDLVVAR: &bdlvvar" ;
    put "IF_MEAN_ADJUST_DELIVERY_VARS: &if_mean_adjust_delivery_vars" / ;
    put "DLVDSGN:" / %unquote(&dldsgnlist) / ;
    put "DECVAR: &decvar" ;
    put "BDECVAR: &bdecvar" ;
    put "RESVAR: &resvar" ;
    put "BRESVAR: &bresvar" ;
    put "OTHVAR: &othvar" ;
    put "BOTHVAR: &bothvar" / ;
    put "REACH_DECAY_SPECIFICATION: &reach_decay_specification" / ;
    put "RESERVOIR_DECAY_SPECIFICATION: &reservoir_decay_specification" / ;
    put "INCR_DELIVERY_SPECIFICATION: &incr_delivery_specification" / ;
    put "IF_ESTIMATE: &if_estimate  IF_TEST_CALIBRATE: &if_test_calibrate  IF_ACCUMULATE_WITH_DLL: &if_accumulate_with_dll  IF_PREDICT: &if_predict" ;
    put "CALIBRATE_SELECTION_CRITERIA: &calibrate_selection_criteria" / ;
    put "DATA_MODIFICATIONS:" / %unquote(&dmlist) ' ;' / ;
    put "______________________________________________________________________________________________________" ;
  run ;

%mend model_summary ;

/* ---------------------------------------------------------------------- */

/* Macro makes various lists */

%macro makelst ;

  /* Specify selected macro variables created in this macro that need to be global */

  %global sas_options indata_list betalst bretain blubnd datalst model_vars_list globvar makecol end_jter n_low n_hi n_store 
          numerator_load_units adjust_units adjust_conc_units concentration_units predict_prefix station_priority_list 
          reach_priority_list LU_class_footnote LU_class_length ;
  
  /* Make list of indata variables */
  %let variable_list = &waterid &arcid &optional_reach_information &fnode &tnode &hydseq &inc_area &tot_area
                      &mean_flow &frac &iftran &target &ls_weight &staid &optional_station_information &lat &lon
                      &depvar &srcvar &dlvvar &decvar &resvar &othvar ;
  %if %upcase(&if_distribute_yield_by_land_use) = YES %then %let variable_list = &variable_list LU_class ; ;

  %let indata_list = ;
  %do i_var = 1 %to %count(&variable_list) ;
    %let var = %scan(&variable_list,&i_var,' ') ;
    %if %sysfunc(indexw(&indata_list,&var)) = 0 %then %let indata_list = &indata_list &var ;
  %end ;

  /* Make list of beta coefficients, list of beta initial values,
     and lower and upper bound matrix. */

  %let betalst = ;
  %let bretain = ;
  %let blbnd = ;
  %let bubnd = ;

  %let ib = 1 ;
  %let bvar = %scan(&betailst,&ib,' ') ;
  %do %while (%length(&bvar) > 0) ;
    %let betalst = &betalst &bvar ;
    %let bretain = &bretain &bvar %scan(&betailst,%eval(&ib + 1),' ') ;
    %let blbnd = &blbnd %scan(%scan(&betailst,%eval(&ib + 2),' '),1,':') ;
    %let bubnd = &bubnd %scan(%scan(&betailst,%eval(&ib + 2),' '),2,':') ;
    %let ib = %eval(&ib + 3) ;
    %let bvar = %scan(&betailst,&ib,' ') ;
  %end ;

  /* List of lower and upper bounds for the parameters */

  %let blubnd = &blbnd, &bubnd ;

  /* Make list of variables to be read from the SAS indata data set and loaded
     into a matrix. Detect and remove duplicates. */ 

  %let addlist = &depvar &srcvar &dlvvar &decvar &resvar &othvar ;
  %let datalst = &waterid &staid &fnode &tnode &frac &iftran &target &tot_area ;
  
  %let iadd = 1 ;
  %let addvar = %scan(&addlist,&iadd,' ') ;
  %do %while (%length(&addvar) > 0) ;
    %let isin = n ;
    %let idata = 1 ;
    %let datavar = %scan(&datalst,&idata,' ') ;
    %do %while (%length(&datavar) > 0) ;
      %if %upcase(&datavar) = %upcase(&addvar) %then %do ;
        %let isin = y ;
        %let datavar = ;
      %end ;
      %else %do ;
        %let idata = %eval(&idata + 1) ;
        %let datavar = %scan(&datalst,&idata,' ') ;
      %end ;
    %end ;
    %if &isin = n %then %let datalst = &datalst &addvar ;
    %let iadd = %eval(&iadd + 1) ;
    %let addvar = %scan(&addlist,&iadd,' ') ;
  %end ;

  /* Specify a list of model variables that must not contain missing values */

  %let model_vars_list = %check(&datalst,&staid &target &depvar &tot_area) ;

  /* Make a list of vectors to be declared as global in the feval module */

  %let globvar = data,node,obsloc,nreach,nnode,nobs,ndef,est,if_final_pass,weights,
      jncnstrn,jfnode,jtnode,jfrac,jtarget,jiftran,jdepvar,jdlvvar,jdecvar,jresvar,
      jsrcvar,jbdlvvar,jbdecvar,jbresvar,jbsrcvar,jwaterid,jstaid,jtotarea,dlvdsgn ;

  /* Assign a list of column reference vectors (used in proc iml) */ 

  %let makecol = 
    jwaterid = %col(datalst,&waterid) %str(;)
    jstaid   = %col(datalst,&staid) %str(;)
    jfnode   = %col(datalst,&fnode) %str(;)
    jtnode   = %col(datalst,&tnode) %str(;)
    jfrac    = %col(datalst,&frac) %str(;)
    jtarget  = %col(datalst,&target) %str(;)
    jiftran  = %col(datalst,&iftran) %str(;)
    jdepvar  = %col(datalst,&depvar) %str(;)
    jsrcvar  = %col(datalst,&srcvar) %str(;)
    jdlvvar  = %col(datalst,&dlvvar) %str(;)
    jdecvar  = %col(datalst,&decvar) %str(;)
    jresvar  = %col(datalst,&resvar) %str(;)
    jtotarea = %col(datalst,&tot_area) %str(;)
    jbsrcvar = %col(betalst,&bsrcvar) %str(;)
    jbdlvvar = %col(betalst,&bdlvvar) %str(;)
    jbdecvar = %col(betalst,&bdecvar) %str(;)
    jbresvar = %col(betalst,&bresvar) %str(;)
  ;

  /* Augment the globvar and makecol macro variables to include other variables */
 
  %let ioth = 1 ;
  %let voth = %scan(&othvar,&ioth,' ') ;
  %do %while (%length(&voth) > 0) ;
    %let globvar = &globvar,j&voth ;
    %let makecol = &makecol 
      j&voth = %col(datalst,&voth) %str(;)
    ;
    %let ioth = %eval(&ioth + 1) ;
    %let voth = %scan(&othvar,&ioth,' ') ;
  %end ;

  %let ioth = 1 ;
  %let both = %scan(&bothvar,&ioth,' ') ;
  %do %while (%length(&both) > 0) ;
    %let globvar = &globvar,j&both ;
    %let makecol = &makecol 
      j&both = %col(betalst,&both) %str(;)
    ;
    %let ioth = %eval(&ioth + 1) ;
    %let both = %scan(&bothvar,&ioth,' ') ;
  %end ;

  /* Check model specifcations */

  /* Check that all coefficients in each of bsrcvar, bdlvvar, bdecvar, bresvar,
     and bothvar are also in betailst. */

  %let excld_bsrcvar = %check(&bsrcvar,&betalst) ;
  %if %length(&excld_bsrcvar) > 0 %then %do ;
    %put_comment(ERROR: Coefficient(s) &excld_bsrcvar of bsrcvar not in the betailst - stop processing) ;
    %let if_error = yes ;
  %end ;
  %let excld_bdlvvar = %check(&bdlvvar,&betalst) ;
  %if %length(&excld_bdlvvar) > 0 %then %do ;
    %put_comment(ERROR: Coefficient(s) &excld_bdlvvar of bdlvvar not in the betailst - stop processing) ;
    %let if_error = yes ;
  %end ;
  %let excld_bdecvar = %check(&bdecvar,&betalst) ;
  %if %length(&excld_bdecvar) > 0 %then %do ;
    %put_comment(ERROR: Coefficient(s) &excld_bdecvar of bdecvar not in the betailst - stop processing) ;
    %let if_error = yes ;
  %end ;
  %let excld_bresvar = %check(&bresvar,&betalst) ;
  %if %length(&excld_bresvar) > 0 %then %do ;
    %put_comment(ERROR: Coefficient(s) &excld_bresvar of bresvar not in the betailst - stop processing) ;
    %let if_error = yes ;
  %end ;
  %let excld_bothvar = %check(&bothvar,&betalst) ;
  %if %length(&excld_bothvar) > 0 %then %do ;
    %put_comment(ERROR: Coefficient(s) &excld_bothvar of bothvar not in the betailst - stop processing) ;
    %let if_error = yes ;
  %end ;

  /* Provide warning if there are some elements of betailst that are not in the
     bsrcvar, bdlvvar, bdecvar, or bothvar lists. */

  %let bunion = &bsrcvar &bdlvvar &bdecvar &bresvar &bothvar ;
  %let excld_betalst = %check(&betalst,&bunion) ;
  %if %length(&excld_betalst) > 0 %then %put_comment(WARNING: The following coefficient(s) of betailst are not in the bsrcvar/bdlvvar/bdecvar/bresvar/other lists: &excld_betalst) ;

  /* Check that there are an equal number of variables and coefficients for the
     source/delivery/decay/resesrvoir macro specifications. */
  %let nsrcvar = %count(&srcvar) ;
  %if %count(&srcvar) ^= %count(&bsrcvar) %then %do ;
    %put_Comment(ERROR: Number of source variables (%count(&srcvar)) and source variable coefficients (%count(&bsrcvar)) are unequal - stop processing) ;
    %let if_error = yes ;
  %end ;
  %let ndlvvar = %count(&dlvvar) ;
  %if %count(&dlvvar) ^= %count(&bdlvvar) %then %do ;
    %put_comment(ERROR: Number of delivery variables (%count(&dlvvar)) and delivery variable coefficients (%count(&bdlvvar)) are unequal - stop processing) ;
    %let if_error = yes ;
  %end ;
  %if %count(&decvar) ^= %count(&bdecvar) %then %do ;
    %put_comment(ERROR: Number of decay variables (%count(&decvar)) and decay variable coefficients (%count(&bdecvar)) are unequal - stop processing) ;
    %let if_error = yes ;
  %end ;
  %if %count(&resvar) ^= %count(&bresvar) %then %do ;
    %put_comment(ERROR: Number of reservoir variables (%count(&resvar)) and reservoir variable coefficients (%count(&bresvar)) are unequal - stop processing) ;
    %let if_error = yes ;
  %end ;

  /* Check that the delivery variable design matrix has the same number of rows as the number of 
     source variables and same number of columns as the number of delivery variables */
  %let isrc = 1 ;
  %let test = %scan(%quote(&dlvdsgn),&isrc,",") ;
  %do %while (%length(&test) > 0) ;
    %if &isrc > &nsrcvar %then %do ;
      %put_comment(ERROR: Too many rows in the delivery design (dlvdsgn) specification: srcvar only has &nsrcvar variables - stop processing) ;
      %let if_error = yes ;
      %let test = ;
    %end ;
    %else %do ;
      %let ntest = %count(&test) ;
      %if &ntest ^= &ndlvvar %then %do ;
        %put_comment(ERROR: Incorrect number of column elements in the delivery design (dlvdsgn) specification for row &isrc (set at &ntest - should be &ndlvvar) - stop processing) ; 
        %let if_error = yes ;
      %end ;
      %let isrc = %eval(&isrc + 1) ;
      %let test = %scan(%quote(&dlvdsgn),&isrc,",") ;
    %end ;
  %end ;
  %if &isrc < %eval(&nsrcvar + 1) %then %do ;
    %put_comment(ERROR: Too many rows in the delivery design (dlvdsgn) specification: srcvar only has &nsrcvar variables - stop processing) ;
    %let if_error = yes ;
  %end ;

  /* Set some default actions for starting and ending iterations */

  %if %upcase(&if_test_calibrate) = YES and %upcase(&if_estimate) = YES %then %let n_boot_iter = 0 ;
  %if %length(&start_iter) = 0 %then %let start_iter = 0 ;
  %if %length(&start_jter) = 0 %then %let start_jter = 0 ;
  %if %length(&end_iter) = 0 %then %let end_iter = &n_boot_iter ;
  %let end_jter = %eval(&n_boot_iter + &n_extra_jter) ;

  /* Turn on estimation if it has been turned off and program cannot find existing extimation files */

  %if %upcase(&if_estimate) = NO %then %do ;
    %if &n_boot_iter = 0 %then %do ;
      %if %sysfunc(exist(dir_rslt.boot_betaest_all)) = 0 %then %do ;
        %put_comment(WARNING: Requested prediction without estimation but dir_rslt.boot_betaest_all was not found - setting if_estimate to yes.) ;
        %let if_estimate = yes ;
      %end ;
    %end ;
    %else %if &n_boot_iter > 0 %then %do ;
      %let not_found = ;
      %if %sysfunc(exist(dir_rslt.boot_betaest_all)) = 0 %then %let not_found = dir_rslt.boot_betaest_all ;
      %if %sysfunc(exist(dir_rslt.summary_betaest)) = 0 %then %let not_found = &not_found dir_rslt.summary_betaest ;
      %if %sysfunc(exist(dir_rslt.resids)) = 0 %then %let not_found = &not_found dir_rslt.resids ;
      %let n_not_found = %count(&not_found) ;
      %if &n_not_found > 0 %then %do ;
        %let verb = was ;
        %if &n_not_found > 1 %then %do ;
          %let verb = were ;
          %if &n_not_found = 2 %then %let not_found = %scan(&not_found,1,' ') and %scan(&not_found,2,' ') ;
          %else %if &n_not_found = 3 %then %let not_found = %scan(&not_found,1,' ')%str(,) %scan(&not_found,2,' ') and %scan(&not_found,3,' ') ;
        %end ;
        %put_comment(WARNING: Requested bootstrap prediction without estimation but &not_found &verb not found - setting if_estimate to yes.) ;
        %let if_estimate = yes ;          
      %end ;
    %end ;        
  %end ;

  /* Set the number of storage locations and number of locations in the upper and lower tails
     for determining prediction confidence intervals. */

  %let n_low = %eval(%sysfunc(floor((1 - &cov_prob / 100) * &n_boot_iter / 2)) + 1) ;
  %let n_hi = %eval(%sysfunc(floor((1 - &cov_prob / 100) * &n_boot_iter)) + 2 - &n_low) ;
  %let n_store = %eval(&n_low + &n_hi) ;

  /* Create some global variables that are created elsewhere in the program */

  %do i = 1 %to &n_seeds ;
    %global seed_&i ;
  %end ;
  %global predlst ;

  /* Set the units adjust factor depending on the dependent variable units */

  %let numerator_load_units = %scan(&load_units,1,'/') ;

  %if %quote(&load_units) = %quote(kg/yr) or %quote(&load_units) = %quote(Bcol/yr) %then %let adjust_units = 1 ;
  %else %if %quote(&load_units) = %quote(mt/yr) %then %let adjust_units = 1000 ;

  /* Set the concentration units adjustment for micrograms per liter rather than milligrams per liter. */

  %if %upcase(&if_concentration_in_micrograms) = YES %then %do ;
    %let adjust_conc_units = 1000 ;
    %let concentration_units = ug/L ;
  %end ;
  %else %do ;
    %let adjust_conc_units = 1 ; 
    %let concentration_units = mg/L ;
  %end ;

  /* Code to automatically set concentration units to col/100ml if the load units are
     Bcol/yr. */

  %if %quote(&load_units) = %quote(Bcol/yr) %then %do ;
    %let adjust_conc_units = 100 ;
    %let concentration_units = col/100ml ;
  %end ;

  /* Automatically turn the dll processor off in calibrate if calibration is in testing mode */

  %if %upcase(&if_test_calibrate) = YES %then %let if_accumulate_with_dll = no ;

  /* Set the prefix for the determination of predictions that are summarized in 
     the summarize_predict macro */

  %if &n_boot_iter > 0 and %upcase(&if_print_boot_predictions) = YES %then %let predict_prefix = mean_ ;
  %else %let predict_prefix = ;

  /* Specify the ordering of informational variables in the residuals and prediction files */

  %let station_priority_list = &staid &optional_station_information &tot_area &mean_flow 
    &lat &lon &waterid &arcid &ls_weight ;

  %let reach_priority_list = &waterid &optional_reach_information &tot_area &inc_area &mean_flow &arcid 
    &fnode &tnode &hydseq &frac &iftran &target &ls_weight LU_class ;

  /* Specify the footnote for the Land Use report, if it has been requested. */

  %if %upcase(&if_distribute_yield_by_land_use) = YES %then %do ;
    %let n_LU_class = 1 ;
    %let i_var = 2 ;
    %let class_var = %scan(&land_class_list,&i_var,' ') ;
    %do %while(%length(&class_var) > 0) ;
      %let n_LU_class = %eval(&n_LU_class + 1) ;
      %let i_var = %eval(&i_var + 3) ;
      %let class_var = %scan(&land_class_list,&i_var,' ') ;
    %end ;
    %let i_var = 2 ;
    %let i_LU_class = 1 ;
    %let LU_class_footnote = ;
    %let LU_class_length = 0 ;
    %let class_var = %scan(&land_class_list,&i_var,' ') ;
    %let pct_var = (>%scan(&land_class_list,%eval(&i_var + 2),' ')%) ;
    %do %while(%length(&class_var) > 0) ;
      %if &i_LU_class = 1 %then %let connect = ;
      %else %if &i_LU_class = &n_LU_class %then %let connect = , and ;
      %else %let connect = , ;
      %let LU_class_footnote = &LU_class_footnote.&connect &class_var &pct_var ;
      %if %length(&class_var) > &LU_class_length %then %let LU_class_length = %length(&class_var) ;
      %let i_var = %eval(&i_var + 3) ;
      %let class_var = %scan(&land_class_list,&i_var,' ') ;
      %let pct_var = (>%scan(&land_class_list,%eval(&i_var + 2),' ')%) ;
      %let i_LU_class = %eval(&i_LU_class + 1) ; 
    %end ;
    %let LU_class_footnote = &LU_class_footnote.. ;
  %end ;

%mend makelst ;

/* ---------------------------------------------------------------------- */
