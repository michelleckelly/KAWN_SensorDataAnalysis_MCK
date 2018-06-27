/* Program: sparrow.sas
   Component: sparrow_graphs.sas
   Date: 12/31/02
   Written by: Greg Schwarz

   Purpose: Macros used for creating graphs of sparrow results.
*/

/* ---------------------------------------------------------------------- */

/* Macro produces graphs of predicted versus observed, probability plots
   and spatial plots of residuals. */

%macro graph_resids ;

  %let if_differ = no ;

  data plotdat (keep = ln_actual ln_predict ln_pred_yield ln_resid weighted_ln_resid disp) 
       probdat (keep = map_resid z_map_resid) 
       minmax (keep = ln_actual ln_predict disp)
       mapresids (keep = id &lat &lon 
         %if %length(&lat) > 0 %then rename = (&lat = y) ;
         %if %length(&lon) > 0 %then rename = (&lon = x) ;
         ) ;
    set dir_rslt.resids nobs = numobs ;
    retain minload ;
    if ln_actual ^= . ;
    if ln_resid ^= weighted_ln_resid then differ + 1 ;
    disp = 2 ;
    if minload = . then minload = ln_actual ;
    if ln_actual < minload then minload = ln_actual ;
    if ln_actual > maxload then maxload + (-maxload + ln_actual) ;
    output plotdat probdat mapresids ;
    if _n_ = numobs then do ;
      disp = 1 ;
      ln_predict = minload ;
      ln_actual = minload ;
      output minmax ;
      ln_predict = maxload ;
      ln_actual = maxload ;
      output minmax ;
      call symput("nobs",trim(left(numobs))) ;
      /* Determines if there is a difference between ln_resid and weighted_ln_resid */
      if differ > 0 then call symput("if_differ","yes") ;
    end ;
  run;

  /* Predicted versus Observed Plot */

  filename gsafile "&home_results.\predvsobs.ps" ;

  data plotdat ; set minmax plotdat ;

  goptions reset=global
           device=cljps
           interpol=join
           gaccess='sasgaedt'
           gsfmode=replace
           rotate=landscape
           gprolog='25210A'X ;

  axis1
     color=BLACK
     width=2.0
     minor = none
     major = none
     label=(justify=r a=90 font=swiss height=1.7 "Observed") 
     value=(font=swiss height=1.1)
     ;

  axis2
     color=BLACK
     width=2.0
     minor = none
     label=(font=swiss height=1.7 "Predicted")
     value=(font=swiss height=1.1)
     ;

  symbol1
    color = black
    interpol = join
    line = 1
    ;

  symbol2
    color = black
    interpol = none
    value = dot
    height = .5
    ;

  title font = swiss height = 2 "Predicted Relative to Observed Flux at &nobs Sites" ;
  title2 font = swiss height = 1 "(Natural logarithm transformation applied to predicted and observed values)" ;

  proc gplot data = plotdat gout = dir_rslt.gbt_pred_vs_obs ;
    plot ln_actual * ln_predict = disp /
      nolegend
      vaxis=axis1
      haxis=axis2
      frame ;
  goptions gsfname = gsafile ;
  run ;

  /* Raw Residual versus Predicted Plot */

  goptions gsfmode = append ;

  axis1
    color=BLACK
    width=2.0
    minor = none
    major = none
    label=(justify=r a=90 font=swiss height=1.7 "Residual")
    value=(font=swiss height=1.1)
    ;

  symbol1
    color = black
    interpol = none
    value = dot
    height = .5
    ;

  title1 font = swiss height = 2 "Predicted Relative to Residual Flux at &nobs Sites" ;
  title2 font = swiss height = 1 "(Natural logarithm transformation applied to predicted and residual values)" ;

  proc gplot data = plotdat gout = dir_rslt.gbt_pred_vs_resid ;
    plot ln_resid * ln_predict /
      nolegend
      vaxis = axis1
      haxis = axis2
      vref = 0
      frame ;
  run ;

  /* Weighted residual versus predicted plot */

  %if &if_differ = yes %then %do ;

    axis1
      color=BLACK
      width=2.0
      minor = none
      major = none
      label=(justify=r a=90 font=swiss height=1.7 "Weighted Residual")
      value=(font=swiss height=1.1)
      ;

    title1 font = swiss height = 2 "Predicted Relative to Weighted Residual Flux at &nobs Sites" ;
    title2 font = swiss height = 1 "(Natural logarithm transformation applied to predicted and residual values)" ;

    proc gplot data = plotdat gout = dir_rslt.gbt_pred_vs_wresid ;
      plot weighted_ln_resid * ln_predict /
        nolegend
        vaxis = axis1
        haxis = axis2
        vref = 0
        frame ;
    run ;

    title1 font = swiss height = 2 "Predicted Yield Relative to Weighted Residual Flux at &nobs Sites" ;
    title2 font = swiss height = 1 "(Natural logarithm transformation applied to predicted yield and residual values)" ;

    axis2
     color=BLACK
     width=2.0
     minor = none
     label=(font=swiss height=1.7 "Predicted Yield")
     value=(font=swiss height=1.1)
     ;

    proc gplot data = plotdat gout = dir_rslt.gbt_pred_yld_vs_wresid ;
      plot weighted_ln_resid * ln_pred_yield /
        nolegend
        vaxis = axis1
        haxis = axis2
        vref = 0
        frame ;
    run ;

  %end ;

  %else %do ;

    title1 font = swiss height = 2 "Predicted Yield Relative to Residual Flux at &nobs Sites" ;
    title2 font = swiss height = 1 "(Natural logarithm transformation applied to predicted yield and residual values)" ;

    axis2
     color=BLACK
     width=2.0
     minor = none
     label=(font=swiss height=1.7 "Predicted Yield")
     value=(font=swiss height=1.1)
     ;

    proc gplot data = plotdat gout = dir_rslt.gbt_pred_yld_vs_wresid ;
      plot weighted_ln_resid * ln_pred_yield /
        nolegend
        vaxis = axis1
        haxis = axis2
        vref = 0
        frame ;
    run ;

  %end ;

  /* Standardized Residual Normal Probability Plot */

  proc sort data = probdat ; by map_resid ;

  proc means data = probdat noprint ;
    var map_resid ;
    output out = mean_std mean = mn_map_resid std = sd_map_resid ;

  data probdat (keep = map_resid z_map_resid disp) ref_line (keep = map_resid z_map_resid disp) ; 
    if _n_ = 1 then set mean_std (keep = mn_map_resid sd_map_resid) ; 
    set probdat nobs = numobs ;
    disp = 2 ;
    output probdat ;
    if _n_ = 1 or _n_ = numobs then do ;
      map_resid = mn_map_resid + z_map_resid * sd_map_resid ;
	  disp = 1 ;
      output ref_line ;
    end ;
  run ;

  data probdat ; set ref_line probdat ;

  goptions gsfmode=append ;

  axis1
     color=BLACK
     width=2.0
     minor = none
     major = none
     label=(font=swiss height=1.7 "Normal Quantiles") 
     value=(font=swiss height=1.1)
     ;
  axis2
     color=BLACK
     width=2.0
     minor = none
     label=(justify=r a=90 font=swiss height=1.7 "Standardized Residual")
     value=(font=swiss height=1.1)
     ;

  symbol1
    color = black
    interpol = join
    value = none
    line = 1
    ;

  symbol2
    color = black
    interpol = none
    value = dot
    height = .5
    ;

  title font = swiss height = 2 "Probability Plot of Residuals" ;
  title2 font = swiss height = 1 "(Residuals are shown in natural logarithm units)" ;

  proc gplot data = probdat gout = dir_rslt.gbt_prob_plot ;
    plot map_resid * z_map_resid = disp /
      nolegend
	  name='Prbplt'
	  des="Probability plot of residuals"
      haxis=axis1
      vaxis=axis2
      frame ;
  run ;
  quit ;

  title ;
  title2 ;
  run ;

%mend graph_resids ;

/* ---------------------------------------------------------------------- */

%macro update_gis ;

  /* The following code updates a pre-existing SAS/GIS coverage. To create the original coverage,
     run the utility sparrow_create_gis.sas included with this set of programs. */

  %let IMP_TYPE = genpoint ;
  %let INFILE = mapresids ;
  %let NIDVARS = 0 ;
  %let MAPLIB = dir_gis ;
  %let MAPCAT = resids ;
  %let MAPNAME = resids ;
  %let CATHOW = update ;
  %let SPALIB = dir_gis ;
  %let SPANAME = resids ;
  %let SPAHOW = update ; 

  options nomprint nomacrogen nosource ;

  DM 'AF C=SASHELP.GISIMP.BATCH.SCL';

  options &sas_options ;

%mend update_gis ;

/* ---------------------------------------------------------------------- */

/* Macro produces a spatial graph of predictions */
 
%macro graph_predict ;


%mend graph_predict ;

/* ---------------------------------------------------------------------- */
