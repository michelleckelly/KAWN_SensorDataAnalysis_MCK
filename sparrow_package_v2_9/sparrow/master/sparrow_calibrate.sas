/* Program: sparrow.sas
   Component: sparrow_calibrate.sas
   Date: 12/31/02
   Written by: Greg Schwarz

   Purpose: Macro calibrates a sparrow model.
*/

/* ---------------------------------------------------------------------- */

/* Macro calibrates the model */

%macro calibrate ;

  %let flag_zero_load = 0 ;

  proc iml ;

    /* Specify the SPARROW evaluation function that climbs down the reach network,
       accumulating loads, making comparisons to actual loads at monitored reaches
       and returning a vector of weighted errors. If the variable if_final_pass is
       set to one prior to running feval then the function returns a matrix consisting
       of actual load, predicted load, LN actual load, LN predicted load, error, and
       weighted error. If the macro variable if_test_calibrate is set to yes then 
       the function returns an additional column representing the number of reaches
       in each monitoring station sub-basin. */

    start feval(beta) global(&globvar,error_report) ;

      if ncol(beta) < ncol(est) then do ;
        est[jncnstrn] = beta ;
        beta = est ;
      end ;

      /* Compute the reach decay factors */
      %if %length(%bquote(&reach_decay_specification)) > 0 %then rchdcayf = &reach_decay_specification ;
      %else rchdcayf  = j(nreach,1,1) ;;

      /* Compute the reservoir decay factors */
      %if %length(%bquote(&reservoir_decay_specification)) > 0 %then resdcayf = &reservoir_decay_specification ;
      %else resdcayf = j(nreach,1,1) ;;

      carryf = data[,jfrac] # rchdcayf # resdcayf ;

      /* Compute the incremental delivered and decayed sources */
      if jsrcvar > 0 then do ;
        %if %length(%bquote(&incr_delivery_specification)) > 0 %then 
          incddsrc = rchdcayf ## .5 # resdcayf # ((&incr_delivery_specification # data[,jsrcvar]) * beta[,jbsrcvar]`) ;
        %else incddsrc = rchdcayf ## .5 # resdcayf # (data[,jsrcvar] * beta[,jbsrcvar]`) ;;
      end ;
      else incddsrc = j(nreach,1,0) ;

      %if %upcase(&if_test_calibrate) = YES %then %do ;
        run test_neg(rchdcayf,"Reach Delivery Factor") ;
        run test_neg(resdcayf,"Reservoir Delivery Factor") ;
        run test_neg(incddsrc,"Incremental Source Delivery") ;
      %end ;

      e = j(nobs,1,0) ;

      e_flag = 0 ;
      if all(beta[,jbsrcvar] <= 0) then e_flag = 1 ;

      /* Climb down the reach network, compute and accumulate incremental rchld */
      %if %upcase(&if_accumulate_with_dll) = YES and %upcase(&if_test_calibrate) ^= YES %then %do ;
        call modulei('tnode_a',ndef,data[,jfnode || jtnode],data[,jiftran],incddsrc,carryf,data[,jdepvar],e) ;
      %end ;
      %else %do ;
        node = j(nnode,1,0) ;
        i_obs = 1 ;
        %if %upcase(&if_test_calibrate) = YES %then n_rch = e ; ;
        do i = 1 to nreach ;
          %if %upcase(&if_test_calibrate) = YES %then if i_obs <= nobs then n_rch[i_obs,] = n_rch[i_obs,] + 1 ; ;
          /* Determine the amount of load leaving the reach */
          rchld = (incddsrc[i,] + carryf[i,] * node[data[i,jfnode]]) ;
          if data[i,jdepvar] ^= . then do ;
            /* Determine the residual for the observation */
            %if %upcase(&if_test_calibrate) = YES %then %do ;
              if rchld <= 0 then do ;
                e[i_obs,] = 0 ;
                print "Non-positive rchld (" (trim(left(char(rchld)))) ") for obs:" (trim(left(char(i_obs)))) " station:" 
                       (trim(left(char(data[i,jstaid])))) "at reach ID:" (trim(left(char(data[i,jwaterid])))) ;
                print "Iftran:" (trim(left(char(data[i,jiftran])))) "carryf:" (trim(left(char(carryf[i,])))) "Flux upnode:" (trim(left(char(node[data[i,jfnode],])))) ;
              end ;
              else e[i_obs,] = log(data[i,jdepvar] / rchld) ;
            %end ;
            %else %do ;
              if rchld <= 0 then do ;
                if e_flag <= 1 then do ;
                  x_rprt = data[i,jstaid || jwaterid] || node[data[i,jfnode],] || rchld || data[i,jsrcvar] || incddsrc[i,] || rchdcayf[i,] || resdcayf[i,] || beta ;
                  if type(error_report) = {u} then error_report = x_rprt ;
                  else error_report = error_report // x_rprt ;
                  e_flag = 2 ;
                  print "Error Report (Iteration &iter, Seed &jter)" ;
                  print "A negative value for predicted flux has been detected at a monitored reach." ;
                  error_report_header = {&staid &waterid &fnode "Predicted flux" &srcvar "Incremental delivered flux" "Instream attenuation factor" "Reservoir attenuation factor" &betalst} ;
                  print (error_report_header`) (error_report`) ;
                end ;
                rchld = . ;
              end ;
              e[i_obs,] = log(data[i,jdepvar] / rchld) ;
            %end ;
            /* Set the reach load to the observed value for further accumulation downstream */
            rchld = data[i,jdepvar] ;
            i_obs = i_obs + 1 ;
          end ;
          node[data[i,jtnode]] = node[data[i,jtnode]] + data[i,jiftran] * rchld ; /* Accumulate load to nodes */
        end ;
      %end ;

      f = sqrt(weights) # e ;

      if if_final_pass = 1 then do ;
        lactual = log(data[obsloc,jdepvar]) ;
        lpredict = lactual - e ;
        lpredyld = lpredict - log(data[obsloc,jtotarea]) ;
        f = data[obsloc,jdepvar] || exp(lpredict) || lactual || lpredict || lpredyld || e || f ;
      end ;
      %if %upcase(&if_test_calibrate) = YES %then f = f || n_rch ; ;
      return(f) ;

    finish ;

    /* Module tests vector to determine if there are any negative elements and writes
       to output the number of negative elements, and the element number and value for the
       first occurrance. */

    start test_neg(test_vector,name) ;
      if_neg = (test_vector < 0) ;
      if any(if_neg) then do ;
        example = loc(if_neg)[1] ;
        print "Warning: negative values for" (trim(left(char(sum(if_neg))))) "elements of" (trim(left(name))) "(example: element" (trim(left(char(example)))) "has value" (trim(left(char(test_vector[example])))) ")." ;
      end ;
    finish ;

    /* Module returns a 1xnr row vector with 1 in ith column and zeros elsewhere. */

    start e(nr,i) ;
      z = j(1,nr,0) ;
      z[,i] = 1 ;
      return(z) ;
    finish ;

    /* Module takes a square matrix and determines any colinear rows. Method is by
       determining non-zero off-diagonal elements or zero diagonal elements of the
       ginv(A) * A matrix, where A is the square input matrix. The function returns
       S rows where S is the number of linearly independent colinearities there are.
       Each row contains 0/1 elements where a 1 signifies a colinear variable. If
       matrix is singular then returns the null matrix. Module calls the module e. */

    start test_singular(square_matrix) ;
      y = (ginv(square_matrix) * square_matrix) ;
      x = (abs(y) > 1E-9) ;
      do i = 1 to ncol(x) ;
        if type(xdif) = {u} then do ;
          if sum(x[i,]) > 1 then xdif = x[i,] ;
          else if abs(x[i,i]) < 1E-9 then xdif = e(ncol(x),i) ;
        end ;
        else do ;
          do j = 1 to nrow(xdif) ;
            if x[i,] = xdif[j,] then goto pass ;
          end ;
          if sum(x[i,]) > 1 then xdif = xdif // x[i,] ;
          else if abs(x[i,i]) < 1E-9 then xdif = xdif // e(ncol(x),i) ;
        end ;
        pass: ;
      end ;
      return(xdif) ;
    finish ;

    /* Module returns k x 1 vector of n-th order polynomial evaluations for k x n
       coefficient matrix c (each row is a set of n coefficients associated with a
       given element of the polynomial result) and arguement x (a scaler) */

    start poly (c,x) ;
      n = ncol(c) - 1 ;
      p = c[,1] ;
      if n > 0 then p = p + c[,2:n + 1] * x ## (1:n)` ;
      return(p) ;
    finish ;

    /* Module returns a two element row vector, the first element being the
       Wilks statistic for normality, and the second element being the p-value
       for testing the null hypothesis that the input vector is from a normal
       distribution. The Wilks statistic is the squared correlation coefficient
       between the ordered X vector and a series of corresponding coefficients
       representing the expected order statistics from a standard normal population.
       The module takes as input the column vector X, corresponding to the unordered
       and uncensored values of the variable being tested, and N, the total number
       of observations in the sample - including censored values. The algorithm is 
       modified from Royston (1992) to assume left censoring rather than right 
       censoring (this modification is achieved by evaluating the negative of X). 
       Note that the algorithm assumes single censoring. Multiple censoring levels
       may be accomodated  - with some loss of power - by including in X only those
       values greater than the largest censoring level (but keep N the same). */

    start swilk (x_in,n) ;
      n1 = nrow(x_in) ;
      range = max(x_in) - min(x_in) ;
      ncens = n - n1 ;
      /* Test restrictions on the algorithm */
      if n > 5000 | (n1 < n & n < 20) | n < n1 | n > 5 * n1 | range = 0 | n1 < 3 then do ;
        w = {. .} ;
        return(w) ;
      end ;
      else do ;
        /* Initialize variables */
        c12 = { 0 0.221157 -0.147981 -2.071190 4.434685 -2.706056,
                0 0.042981 -0.293762 -1.752461 5.682633 -3.582633 } ;
        c34 = { 0.5440 -0.39978 0.025054 -0.0006714,
                1.3822 -0.77857 0.062767 -0.0020322 } ;
        c56 = { -1.5861 -0.31082  -0.083751  0.0038915,
                -0.4803 -0.082676  0.0030302 0.0       } ;
        c7 = {0.164   0.533} ;
        c8 = {0.1736  0.315} ;
        c9 = {0.256  -0.00635} ;
        g = {-2.273 0.459} ;
        z = probit({.9 .95 .99}) ;
        zm = z[,:] ;
        zss = ssq(z - zm) ;
        bf1 = 0.8378 ;
        xx90 = 0.556 ;
        xx95 = 0.622 ;
        pi6 = 1.909859 ;
        stqr = 1.047198 ;
        small = 1E-19 ;
        log_n = log(n) ;
        delta = ncens / n ;
        /* Order and normalize x - take negative x to accomodate left censoring */
        x = - x_in ;
        x[rank(x),] = - x_in / range ;
        n2 = floor(n / 2) ;
        if_n_even = (((-1) ## n) > 0) ;
        /* Compute the coefficients (expected order statistics) */
        if n = 3 then a = sqrt(0.5) ;
        else do ;
          a = probit(((1:n2)` - 0.375) / (n + 0.25)) ;
          summ2 = 2 * ssq(a) ;
          i1 = 1 + (n > 5) ;
          a_1 = poly(c12[1:i1,],1/sqrt(n)) - a[1:i1,] / sqrt(summ2) ;
          fac = sqrt((summ2 - 2 * ssq(a[1:i1,])) / (1 - 2 * ssq(a_1))) ;
          a[1:i1,] = a_1 ;
          a[(i1+1):n2,] = - a[(i1+1):n2,] / fac ;
        end ;
        /* Compute Wilks W - the squared Pearson correlation between x and a */
        if if_n_even then a = (a // -a[n2:1,])[1:n1,] ;
        else a = (a // 0 // -a[n2:1,])[1:n1,] ;
        a = a - a[:,] ;
        x = x - x[:,] ;
        ssa = ssq(a) ;
        ssx = ssq(x) ;
        sax = a` * x ;
        sr_ssa_ssx = sqrt(ssa * ssx) ;
        w = 1 - (sr_ssa_ssx - sax) * (sr_ssa_ssx + sax) / (ssa * ssx) ;
        /* Calculate the significance level */
        if n = 3 then do ;
          w = w || (pi6 * (arsin(sqrt(w)) - stqr)) ;
          return(w) ;
        end ;
        else do ;
          y = log(1 - w) ;
          if n <= 11 then do ;
            gamma = poly(g,n) ;
            if y >= gamma then do ;
              w = w || small ;
              return(w) ;
            end ;
            y = -log(gamma - y) ;
            parm = poly(c34,n) ;
          end ;
          else parm = poly(c56,log_n) ;
          m = parm[1,] ;
          s = exp(parm[2,]) ;
        end ;
        if ncens > 0 then do ;
          ld = -log(delta) ;
          bf = 1 + log_n * bf1 ;
          z_f = z + bf * (poly(c7,xx90 ## log_n) || poly(c8,xx95 ## log_n) || poly(c9,log_n)) ## ld ;
          z_fm = z_f[,:] ;
          zsd = z * (z_f - z_fm)` / zss ;
          zbar = z_fm - zsd * zm ;
          m = m + zbar * s ;
          s = s * zsd ;
        end ;
        w = w || probnorm((m - y) / s) ;
        return(w) ;
      end ;
    finish ;

    /* Module computes the variance of columns in a matrix */

    start varcol(inmatrix) ;
      if nrow(inmatrix) = 1 then var = . ;
      else var = (inmatrix[##,] - nrow(inmatrix) * inmatrix[:,] ## 2) / (nrow(inmatrix) - 1) ;
      return(var) ;
    finish ;

    /* Module computes the correlation between two vectors */

    start corr(x,y) ;
      n = nrow(x) ;
      xc = x - x[:,] ;
      yc = y - y[:,] ;
      if n = 1 then corr = . ;
      else corr = xc` * yc / (sqrt(ssq(xc)) * sqrt(ssq(yc))) ;
      return(corr) ;
    finish ;

    /* Determine the columns of various variables */

    &makecol ;

    /* Create the delivery design matrix */
    dlvdsgn = {&dlvdsgn} ;

    filename sascbtbl "&home_program.\sparrow_at.txt";

    /* Load the initial parameter estimates */
    use betahat0 ;
    read next var {&betalst} into beta0 ;
    close betahat0 ;
    parameters = {&betalst}` ;

    /* Load the data */
    use indata ;
    read all %if %length(&calibrate_selection_criteria) > 0 %then where (&calibrate_selection_criteria) ;
     var {&datalst} into data ;
    read all where ( %if %length(&calibrate_selection_criteria) > 0 %then &calibrate_selection_criteria & ;
     &ls_weight ^= .) var {&ls_weight} into weights ;
    close indata ;

    /* Specify the number of reaches, nodes and observations */
    nreach = nrow(data) ;
    nnode = max(data[,(jfnode || jtnode)]) ;
    obsloc = loc(data[,jdepvar] ^= .) ;
    nobs = ncol(obsloc) ;
    ndef = nreach || nnode || 1 || nobs ;

    /* Specify the (normalized) weights used for weighted least squares */
    weights = weights # ((1 / weights)[:,]) ;

    /* Specify weights for bootstrap analysis */
    %if &iter > 0 and %upcase(&if_parm_bootstrap) ^= YES %then %do ;
      sampid = ceil(nobs * ranuni(j(nobs,1,&seed_1))) <> 1 ;
      create counts from sampid [colname = {sampid}] ;
      append from sampid ;
      summary class {sampid} opt {noprint save} ;
      close counts ;
      boot_weights = j(nobs,1,0) ;
      boot_weights[sampid,] = _nobs_ ;
    %end ;
    %else %do ;
       boot_weights = j(nobs,1,1) ;
    %end ;

    /* Combine mormalized weights with bootstrap weights */
    weights = weights # boot_weights ;

    /* Specify the Optimization Algorithm control vectors */

    /* Options: {number of values returned by function for least squares or 0 for minimization or 1 for maximization, 
       printing code (0 = none, 1 = iteration history and final gradients, 2 = final parameters, 
       3 = termination criteria details 4 = iteration parameters 5 = iteration gradients)} */
    opt = nobs || &NLP_printing_option ; 
    /* Constraint matrix */
    bc = {&blubnd} ;
    /* Termination criteria: {maximum iterations, 
         maximum function calls, ,Gradient convergence criteria (leave third switch set to missing), ...} */
    tc = {100 . . 0.000000000001 0 0 0 0 0 0} ; 
    /* Optimization steps */
    par = {. . . . . . . .} ;
    if_final_pass = 0 ;

    /* Perform nonlinear least squares */

    %if %upcase(&if_test_calibrate) = YES %then %do ;
      rc = . ;
      title SPARROW Calibration Test Results ;
      if jdecvar > 0 then do ;
        loc_rch_decay = loc(abs(-data[,jdecvar] * beta0[,jbdecvar]`) > 709) ;
        if type(loc_rch_decay) ^= {u} then n_rch_decay = ncol(loc_rch_decay) ;
        else n_rch_decay = 0 ;
        if n_rch_decay > 0 then do ;
          big_rch_decay = data[loc_rch_decay[,1:(min(n_rch_decay,5))],jwaterid || jdecvar] ;
          varnames = {waterid} || {&datalst}[,jdecvar] ;
          bdecvar = beta0[,jbdecvar] ;
          betanames = {&betalst}[,jbdecvar] ;
          mattrib big_rch_decay label = 'Reaches with Big Reach Decay Vars' colname = varnames
                  n_rch_decay   label = 'Number of Reaches' 
                  bdlvvar       label = 'Decay Variable Coefficients' colname = betanames ; 
          print n_rch_decay bdecvar big_rch_decay ;
          if min(abs(eigval(data[,jdecvar]` * data[,jdecvar]))) <= 1E-10 then print 'Warning: decay variables are colinear' ; 
        end ;
      end ;
      else n_rch_decay = 0 ;
      if jdlvvar > 0 then do ;
        loc_incddsrc  = loc(abs((beta0[,jbdlvvar] # data[,jdlvvar]) * {&dlvdsgn}`) > 709) ;
        if type(loc_incddsrc) ^= {u} then n_incddsrc  = ncol(loc_incddsrc) ;
        else n_incddsrc = 0 ;
        if n_incddsrc > 0 then do ;
          big_incddsrc = data[loc_incddsrc[,1:(min(n_incddsrc,5))],jwaterid || jdlvvar] ;
          varnames = {waterid} || {&datalst}[,jdlvvar] ;
          bdlvvar = beta0[,jbdlvvar] ;
          betanames = {&betalst}[,jbdlvvar] ;
          mattrib big_incddsrc label = 'Reaches with Big Delivery Variables' colname = varnames
                  n_incddsrc   label = 'Number of Reaches' 
                  bdlvvar      label = 'Delivery Var Coefficients' colname = betanames ; 
          print n_incddsrc bdlvvar big_incddsrc ;
        end ;
        if min(abs(eigval(data[,jdlvvar]` * data[,jdlvvar]))) <= 1E-10 then print 'Warning: delivery variables are colinear' ; 
      end ;
      else n_incddsrc = 0 ;
      if min(abs(eigval(data[,jsrcvar]` * data[,jsrcvar]))) <= 1E-10 then print 'Warning: source variables are colinear' ; 
      if n_rch_decay = 0 & n_incddsrc = 0 then do ;
        if_final_pass = 1 ;
        outdat = data[loc(data[,jdepvar]^=.),(jwaterid || jstaid)] || feval(beta0) ;
        create dir_rslt.test_resids from outdat [colname={waterid staid actual predict ln_actual ln_predict ln_resid weighted_ln_resid n_rch}];
        append from outdat ;
        close dir_rslt.test_resids ;
      end ;
    %end ;
    %else %if &iter > 0 and %upcase(&if_parm_bootstrap) = YES %then %do ;
      rc = 1 ;
      use dir_rslt.summary_betaest ;
      read all var {&betalst} into beta0 ;
      close dir_rslt.summary_betaest ;

      use dir_rslt.cov_betaest ;
      read all var {&betalst} into cov_estimate ;
      close dir_rslt.cov_betaest ;

      /* Generate the parameters using monte carlo methods */
      estimate = beta0 + rannor(j(1,ncol(beta0),&seed_1)) * root(cov_estimate) ;

      /* Ensure parameters are consistent with constraints */
      bc = {&blubnd} ;
      lb = repeat(bc[1,],nrow(estimate),1) ;
      ub = repeat(bc[2,],nrow(estimate),1) ;
      estimate = choose(lb ^= .,estimate <> lb,estimate) ;
      estimate = choose(ub ^= .,estimate >< ub,estimate) ;
    %end ;
    %else %do ;
      call nlplm(rc,estimate,"feval",beta0,opt,bc,tc,par) ;
      if type(error_report) ^= {u} then do ;
        call symput('flag_zero_load',trim(left(char(1)))) ;
        create dir_rslt.error_report from error_report [colname = {i i_obs rchld &staid &waterid &srcvar upnode_flux incr_deliv rch_deliv_factor res_deliv_factor &betalst}] ;
        append from error_report ;
        close dir_rslt.error_report ;
      end ;
    %end ;

    /* Output the results */
    if rc > 0 then do ;

      /* Determine which parameter estimates are not constrained */
      jncnstrn = loc(estimate ^= bc[1,] & estimate ^= bc[2,]) ;
      est = estimate ;

      /* Get the first- and second-order finite differences wrt the non-constrained parameters */
      call nlpfdd(f,g,h,"feval",estimate[jncnstrn],nobs) ;

      /* Evaluate the following only if the derivatives can be evaluated (otherwise
         boot_betaest is not created causing a new bootstrap selection of coefficients) */

      if type(f) ^= 'U' then do ;
  
        /* Generate residuals and predictions for output */        
        if_final_pass = 1 ;
        outdat = feval(estimate) ;
        weighted_resid = outdat[,ncol(outdat)] ;

        /* Make leverage statistics from the gradients. Note that for bootstrap iterations, the gradients are premultipled by the 
           bootstrap weight, which is the number of times the observation appears in the bootstrap sample. Thus, to get the leverage
           for an individual observation under bootstrapping you must divide by the number of occurrences in the bootstrap sample. */
        g_full = j(nrow(g),ncol(estimate),0) ;
        g_full[,jncnstrn] = g ;
        h_lev = ((g_full * ginv(g_full` * g_full) # g_full)[,+]) ;

        /* Create errors for bootstrapping. The weighted_resid includes the square root of the observation weight and the bootstrap
           occurance weight. h_lev includes the number of occurrences in the bootstrap sample. Therefore, to remove the occurrence
           factor it is necessary to substitute the maximum of the boot_weight or 1 in place of the 1 in the denominator term. This
           effectively removes the boot_weight from weighted_resid. */
        boot_resid = weighted_resid / sqrt((boot_weights <> 1) - h_lev) ;

        /* Compute the mean of exp(weighted_resid) */
        mean_exp_weighted_error = (boot_weights` * exp(boot_resid)) / sum(boot_weights) ;
        var_exp_weighted_error = boot_weights` * ((exp(boot_resid) - mean_exp_weighted_error) ## 2) / sum(boot_weights) ;

        /* Output the bootstrap estimates */
        boot_estimate = {&iter &jter} || estimate || mean_exp_weighted_error ; 
        create boot_betaest from boot_estimate [colname = ({iter jter} || parameters` || {mean_exp_weighted_error})] ;
        append from boot_estimate ;
        close boot_betaest ; 

      end ;

      /* The following output is only required for the parametric estimates */

      %if &iter = 0 %then %do ;

        /* Generate predictions for output */
        depvar = log(data[obsloc,jdepvar]) ;
        larea = log(data[obsloc,jtotarea]) ;
        lyield = depvar - larea ;

        /* Determine singularity of h matrix */
        singular_vars = test_singular(h) ;
        if type(singular_vars) ^= {u} then do i = 1 to nrow(singular_vars) ;
          print "Warning: colinear variable(s):" (compbl(rowcat(parameters`[,jncnstrn][,loc(singular_vars[i,])] + " "))) ;
        end ;

        /* Generate summary statistics for output */
        df_model = ncol(jncnstrn) ;
        df_error = nobs - df_model ;
        sse = ssq(weighted_resid) ;
        mse = sse / df_error ;
        rmse = sqrt(mse) ;
        r_square = 1 - sse / (ssq(depvar) - sum(depvar)##2/nobs) ;
        adj_r_square = 1 - ((nobs - 1)/df_error)*(1 - r_square) ;
        r_square_yield = 1 - sse / (ssq(lyield) - sum(lyield)##2/nobs) ;
        cov_estimate = j(ncol(estimate),ncol(estimate),0) ;
        cov_estimate[jncnstrn,jncnstrn] = mse * inv(h) ;
        cor_estimate = j(ncol(estimate),ncol(estimate),.) ;
        jcor = loc(vecdiag(cov_estimate)` > 0) ;
        normalize = diag(vecdiag(cov_estimate[jcor,jcor]) ## -.5) ;
        cor_estimate[jcor,jcor] = normalize * cov_estimate[jcor,jcor] * normalize ;
        sd_estimate = sqrt(vecdiag(cov_estimate)) ;
        if all(sd_estimate) = 0 then sd_estimate[loc(sd_estimate = 0)] = . ;
        estimate = estimate` ;
        t_stat = j(nrow(estimate),1,.) ;
        p_value = j(nrow(estimate),1,.) ;
        t_stat[jncnstrn,] = estimate[jncnstrn,] / sd_estimate[jncnstrn,] ;
        p_value[jncnstrn,] = 2*(1-probt(abs(t_stat[jncnstrn,]),df_error)) ;

        /* Derivation of Variance Inflation Factors, and X`X 
           correlation matrix, eigenvalues and eigenvectors */
        if any(((g[##,] / nrow(g) - g[:,] ## 2) = 0) & g[:,] <> 0) then if_center = 1 ;
        else if_center = 0 ;
        if if_center then gc = g - repeat(g[:,],nrow(g),1) ; /* Center the gradients */
        else gc = g ;
        c = gc` * gc ; /* Compute the covariances */
        c = diag(vecdiag(c) ## (-.5)) * c * diag(vecdiag(c) ## (-.5)) ; /* Compute the correlation matrix */

        /* Initialize the VIF and eigenvalue vectors and corr and 
           eigenvector matrices */
        vif = j(nrow(estimate),1,.) ;
        e_val = vif ;
        corr = j(nrow(estimate),nrow(estimate),.) ;
        e_vec = corr ;
        vif[jncnstrn,] = vecdiag(ginv(c)) ;
        corr[jncnstrn,jncnstrn] = c ;
        corr = vif` // corr ;
        call eigen(eigval,eigvec,c) ;
        e_val[jncnstrn,] = eigval ;
        e_vec[jncnstrn,jncnstrn] = eigvec ;
        e_vec = e_val` // e_vec ;
        min_eval = e_vec[1,ncol(e_vec)] ;
        min_eval_evec = e_vec[2:nrow(e_vec),ncol(e_vec)] ;
        e_val_spread = eigval[<>,] / eigval[><,] ;
        varnames = {'VIF'} || parameters` ;
        varnames1 = {'EigVal'} || parameters` ;

        /* Compute the standardized residuals for mapping */
        map_resid = boot_resid / rmse ;
        /* Compute the quantile probabilities for the probability plot and compute the
           probability plot correlation coefficient and confidence interval */
        z_map_resid = probit((rank(map_resid) - .4) / (nobs + .2)) ;
        ppcc = corr(map_resid,z_map_resid) ;
        if nobs > 3 then do ;
          z_up = 2 * probit((1 + &cov_prob / 100) / 2) / sqrt(nobs - 3) ;
          lb = ((1 + ppcc) * exp(-z_up) - (1 - ppcc)) / ((1 + ppcc) * exp(-z_up) + (1 - ppcc)) ;
          ub = ((1 + ppcc) * exp(z_up) - (1 - ppcc)) / ((1 + ppcc) * exp(z_up) + (1 - ppcc)) ;
        end ;
        else do ;
          lb = . ;
          ub = . ;
        end ;
        ppcc_ci = rowcatc({'['} || char(lb) || {','} || char(ub) || {']'}) ;
        swilk_stat = swilk(map_resid,nobs) ;
        swilk_pval = swilk_stat[,2] ;
        swilk_stat = swilk_stat[,1] ;

        mattrib nobs           label = 'N Obs'
                df_model       label = 'DF Model'
                df_error       label = 'DF Error'
                sse            label = 'SSE   '
                mse            label = 'MSE   '
                rmse           label = 'Root MSE'
                r_square       label = 'R-Square'
                adj_r_square   label = 'Adj R-Sq'
                r_square_yield label = 'Yld R-Sq'
                parameters     label = 'Parameter'
                estimate       label = 'Estimate'
                sd_estimate    label = 'Std Err '
                t_stat         label = 't Value '
                p_value        label = 'Pr > |t|'
                vif            label = 'VIF   '
                cov_estimate   label = 'Parameter Covariances'
                cor_estimate   label = 'Parameter Correlations'
                corr           label = 'Variance Inflation Factors and X''X Correlation Matrix'
                e_vec          label = 'X''X Eigenvalues and Eigenvectors'
                e_val_spread   label = 'Eigen Sprd' 
                ppcc           label = 'Norm PPCC' 
                ppcc_ci        label = "PPCC &cov_prob.% C.I." 
                swilk_stat     label = 'SWilks W' 
                swilk_pval     label = 'P-Value ' 
                min_eval       label = 'Min Eigval' 
                min_eval_evec  label = 'Min EigVec' ;

        if if_center = 0 then mattrib vif label = 'VIF (NC)' e_vec label = 'X''X Eigenvalues and Eigenvectors (NC)' ;

        title SPARROW Model Nonlinear Least Squares Results ;
  
        print / nobs df_model df_error sse mse rmse r_square adj_r_square r_square_yield ; 
        print parameters estimate sd_estimate t_stat p_value vif ;
        print e_val_spread ppcc swilk_stat swilk_pval ;
        print / cov_estimate [rowname=parameters colname=parameters] ;
        print / cor_estimate [rowname=parameters colname=parameters] ;
        /* print / corr [rowname = varnames colname = parameters] ;*/
        blanks = j(1,nrow(parameters),"") ;
        print / e_vec [rowname = varnames1 colname = blanks] ;

        /* Following code writes a summary of the results to an external file */
        space = max(length(parameters)) + 1 ;
        begin = 22 - ceil(space / 2) ;
        file mod_rslt ;
        today = &today ;
        now = &now ;
        put // "DATE: " today date9. " TIME: " now time8. // ;
        put @11 "N Obs  DF Model  DF Error    SSE       MSE     Root MSE  R-Square  Adj R-Sq  Yld R-Sq" / ;
        put @7 nobs @17 df_model @27 df_error @37 sse @47 mse @57 rmse @67 r_square @77 adj_r_square @87 r_square_yield // ;
        if if_center = 0 then 
          put @begin "Parameter" @(begin+space) " Estimate  Std Err   t Value   Pr > |t|  VIF (NC) Min EigVec" / ;
        else 
          put @begin "Parameter" @(begin+space) " Estimate  Std Err   t Value   Pr > |t|    VIF    Min EigVec" / ;
        do i = 1 to nrow(parameters) ;
          put @begin (parameters[i,]) @(begin+space) (estimate[i,]) @(begin+space+10) (sd_estimate[i,])
              @(begin+space+20) (t_stat[i,]) @(begin+space+30) (p_value[i,]) @(begin+space+40) (vif[i,])
              @(begin+space+51) (min_eval_evec[i,]) ;
        end ;
        put // @26               "Eigen Sprd Norm PPCC  SWilks W  P-Value  Min Eigval" / ;
        put @27 e_val_spread @37 ppcc @47 swilk_stat @57 swilk_pval @68 min_eval // ;
        put "______________________________________________________________________________________________________" ;
        closefile mod_rslt ;

        outdat = data[obsloc,(jwaterid || jstaid)] || outdat || map_resid || boot_resid || h_lev || z_map_resid || g_full ;
        resid_names = {&waterid &staid actual predict ln_actual ln_predict ln_pred_yield ln_resid weighted_ln_resid map_resid boot_resid leverage z_map_resid} || parameters` ;
        create resids from outdat [colname=resid_names] ;
        append from outdat ;
        close resids ;

        summary_betaest = estimate` || sd_estimate` || mean_exp_weighted_error || var_exp_weighted_error || 
                          nobs || df_error || df_model || sse || mse || rmse || r_square || adj_r_square || r_square_yield ||
                          e_val_spread || ppcc || swilk_stat || swilk_pval ;
        varnames = compress(parameters` || ({sd_} + parameters`) || {mean_exp_weighted_error var_exp_weighted_error
                          nobs df_error df_model sse mse rmse r_square adj_r_square r_sq_yld
                          e_val_spread ppcc swilk_stat swilk_pval}) ;
        create summary_betaest from summary_betaest [colname = varnames] ;
        append from summary_betaest ;
        close summary_betaest ;

        cov_estimate = cov_estimate || vif || e_val ;
        create cov_betaest from cov_estimate [colname = (parameters` || {vif e_val})] ;
        append from cov_estimate ;
        close cov_betaest ;
        
      %end ;

    end ;

  quit ;

  %if &flag_zero_load = 1 %then %do ;
    %put WARNING: Non-positive loads were predicted at one or more monitoring stations during the iterative
estimation of the model. Probable causes were: 
(1) all source coefficients were non-positive for one or more intermediate steps of the estimation algorithm,
(2) one or more source coefficients was sufficiently negative during model estimation to cause predicted 
flux to be negative for at least one monitoring station, (3) one or more monitoring stations located in
headwater reaches have no sources, or have positive sources only for those sources with non-positive source 
coefficients, or (4) the reservoir inverse hydraulic load specification allows negative reservoir decay.
See the file ERROR_REPORT in the WORK directory for additional information regarding the 
conditions which caused the reporting of this warning. The file includes a detailed listing for the offending
monitored reaches including: reach and station ids, incremental sources, flux passed from upstream node, 
incremental delivery factors, reach and reservoir delivery factors, and the values of all model coefficients.
Note that if model convergence was successfully attained, and the residuals in DIR_RSLT.RESIDS are non-missing, 
then model estimation can be presumed to be valid. This warning can usually be circumvented by setting positive, 
non-zero lower bounds on the source coefficients (e.g., .00001), and a zero lower bound on the reservoir inverse
hydraulic load coefficient. ;
  %end ;

%mend calibrate ;

/* ---------------------------------------------------------------------- */
