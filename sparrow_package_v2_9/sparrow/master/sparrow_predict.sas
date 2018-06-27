/* Program: sparrow.sas
   Component: sparrow_predict.sas
   Date: 12/31/02
   Written by: Greg Schwarz

   Purpose: Macro predicts loads for all reaches in a sparrow model.
*/

/* ---------------------------------------------------------------------- */

%macro predict ;

  proc iml ;

    /* Specify the evaluation function */

    start feval_predict(beta) global(&globvar) ;

	  /* Compute the reach decay factors */
      %if %length(%bquote(&reach_decay_specification)) > 0 %then rchdcayf = &reach_decay_specification ;
      %else rchdcayf  = j(nreach,1,1) ;;

      /* Compute the reservoir decay factors */
      %if %length(%bquote(&reservoir_decay_specification)) > 0 %then resdcayf = &reservoir_decay_specification ;
      %else resdcayf = j(nreach,1,1) ;;

      /* Compute the incremental delivered and decayed sources */
      if jsrcvar > 0 then do ;
        %if %length(%bquote(&incr_delivery_specification)) > 0 %then 
          incddsrc = &incr_delivery_specification # data[,jsrcvar] # beta[,jbsrcvar] ;
        %else incddsrc = data[,jsrcvar] # beta[,jbsrcvar] ;;
      end ;
      else incddsrc = j(nreach,1,0) ;

      /* Build incremental delivered loads, total and by source */
      incddsrc = incddsrc[,+] || incddsrc ;
      n_src = ncol(incddsrc) ;

      /* Compute incremental decay - the decay applied to incremental load to get incremental load leaving the reach */
      inc_decay = rchdcayf ## .5 # resdcayf ;
 
      /* Pre-concatenate incremental delivered loads (delivered to the reach outlet) to incremetal delivered loads (to the reach - without instream decay),
         and append a vector of zeros for the total load decayed in reservoirs */ 
      incddsrc = inc_decay # incddsrc || incddsrc || j(nreach,1,0) ;
 	  n_vars = ncol(incddsrc) ;

      /* Initialize variables */
      node = j(nnode,n_vars,0) ;
      rchld = j(nreach,n_vars,0) ;

      /* Expand carryf and iftran to cover all columns */
      carryf = repeat(data[,jfrac] # rchdcayf # resdcayf,1,n_src) || repeat(data[,jfrac],1,n_src + 1) ;
	  iftran = repeat(data[,jiftran],1,n_src) || j(nreach,n_src + 1,1) ;

	  /* Loads are grouped as:
	            1                                   Total load (fully decayed)
	            2               to (nsource + 1)    Source load (fully decayed)
           (nsource + 2)                            Total load - no stream decay
           (nsource + 3)        to (2(nsource + 1)) Source load - no stream decay
           (2(nsource + 1) + 1)                     Total load decayed in reservoir (does not accumulate)
      */ 

      /* Climb down the reach network, compute and accumulate incremental rchld. Note, the concept
         of transfer of load downstream for reaches with no flow is that load is delivered to the
         stream but that there is no flow at the stream outlet. Therefore, the stream has load, but
         none of this load is transferred to the downstream node. For delivery (see below), we retain
         the interpretation that flow is zero at the downstream node so the reach does not deliver
         any load to a downstream estuary. */
      do i = 1 to nreach ;
        /* Determine the amount of load leaving the reach */
        rchld[i,] = incddsrc[i,] + carryf[i,] # node[data[i,jfnode],] ;

        /* If test mode is on for prediction, output intermediate results */
        %if %upcase(&if_test_predict) = YES %then %do ;
          if any(i = {&test_obs}) then do ;
            test = data[i,] || rchdcayf[i,1] || resdcayf[i,1] || incddsrc[i,] || node[data[i,jfnode],] ;
            if type(test_out) = {u} then test_out = test ;
            else test_out = test_out // test ;
          end ;
        %end ;

        /* Determine load lost in reservoirs (column nvars). Note, this estimate does not accumulate downstream.
           Load lost in reservoir = Load entering reservoir - Load leaving reservoir
           Now, Load leaving reservoir = Load entering reservoir * resdecayf
           Therefore, Load lost in reservoir = Load entering reservoir * ( 1 - resdecayf)
                                             = (Load leaving reservoir / resdecayf) * (1 - resdecayf) */
		rchld[i,n_vars] = ((1 / resdcayf[i,]) - 1) # rchld[i,1] ;
        %if %upcase(&if_adjust) = YES %then %do ;
          if data[i,jdepvar] ^= . then rchld[i,1:n_src] = (data[i,jdepvar] / rchld[i,1]) * rchld[i,1:n_src] ; 
        %end ;
        node[data[i,jtnode],] = node[data[i,jtnode],] + iftran[i,] # rchld[i,] ; /* Accumulate load to nodes */
      end ;

      if type(test_out) ^= {u} then do ;
        testnames = {&datalst rchdcayf resdcayf} ||
                    ({incddsrc_} + {total &srcvar}) ||
                    ({nd_incddsrc_} + {total &srcvar}) || {incddsrc_res_loss} ||
                    ({node_} + {total &srcvar}) ||
                    ({nd_node_} + {total &srcvar}) || {node_res_loss} ;
        create test_data from test_out [colname = testnames] ;
        append from test_out ;
        close test_data ;
      end ;

      /* Determine the delivery factor and delivered incremental amounts.
         To do this, we set delivery factors for target reaches (reaches
         we wish to compute delivery to) to one and then carry the 
         multiplicative decay factors to the upstream node. NOTE, the delivery
         fraction includes decay within the reach. Thus, incremental load
         delivered to a target is the incremental load times the delivery
         fraction. Note that the algorithm must check for a target twice,
         the first time to set delfrac without the incremental decay. This
         value of delfrac is used in determining the delivery fraction to 
         be transferred upstream (after multiplication by the full decay in
         the reach segment). The second time is to adjust the delivery 
         fraction for the incremental decay. */
	  node = j(nnode,1,0) ;
	  delfrac = j(nreach,1,0) ;

      if jtarget > 0 then do ;
        do i = nreach to 1 by -1 ;
          if data[i,jtarget] then delfrac[i,] = 1 ;
          else delfrac[i,] = data[i,jiftran] # node[data[i,jtnode],] ;
          node[data[i,jfnode],] = node[data[i,jfnode],] + delfrac[i,] # carryf[i,1] ;
          delfrac[i,] = delfrac[i,] # inc_decay[i,] ;
        end ;
	  end ;
      else delfrac = j(nreach,1,.) ;

      /* Concatenate the delivery fraction and incremental loads (total and by 
         source - no stream decay) to reach predictions. */
      rchld = rchld[,1:(n_vars - 1)] || incddsrc[,(n_src + 1):(2 * n_src)] || rchld[,n_vars] || delfrac ;

      /* Format of rchld ouput matrix is:
              Variable              Column Start          Column End          Number of Columns        Description
	         pload_total                 1                     1                      1           Total load (fully decayed)
	        pload_(source)               2               (nsource + 1)             nsource        Source load (fully decayed)
            pload_nd_total         (nsource + 2)         (nsource + 2)                1           Total load delivered to streams - no stream decay
           pload_nd_(source)       (nsource + 3)         (2nsource + 2)            nsource        Source load delivered to streams - no stream decay
           pload_inc_total         (2nsource + 3)        (2nsource + 3)               1           Total incremental load delivered to streams
          pload_inc_(source)       (2nsource + 4)        (3nsource + 3)            nsource        Source incremental load delivered to streams
              res_decay            (3nsource + 4)        (3nsource + 5)               1           Total load decayed in reservoirs
              del_frac             (3nsource + 5)        (3nsource + 5)               1           Fraction of total load delivered to terminal reach
      */
      return(rchld) ;

    finish ;

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

    /* Determine the columns of various variables */

    &makecol ;

    /* Create the delivery design matrix */
    dlvdsgn = {&dlvdsgn} ;

    /* Load the initial parameter estimates */
    use boot_betaest ;
    read next var {&betalst} into beta ;
    read var {mean_exp_weighted_error} into mean_exp_weighted_error ;
    close boot_betaest ;

    /* Load the data */
    use indata ;
    read all var {&datalst} into data ;
    close indata ;

    /* Specify the number of reaches, nodes and observations */
    nreach = nrow(data) ;
    nnode = max(data[,(jfnode || jtnode)]) ;
	obsloc = loc(data[,jdepvar] ^= .) ;
    nobs = ncol(obsloc) ;

    /* Create variable names for labeling output and assign to a macro variable for use elsewhere */
    varnames = {total &srcvar} ;
    predlst = compress(({pload_} + varnames) || ({pload_nd_} + varnames) || ({pload_inc_} + varnames) || {res_decay} || {del_frac}) ;
    varnames = compress({&waterid &staid &depvar} || predlst) ;
    call symput("predlst",compbl(rowcat(predlst + " "))) ;
    %if %length(&retrans_exclude_list) = 0 %then %do ;
      exclude_list = 0 ;
    %end ;
    %else %do ;
      exclude_list = locin({&retrans_exclude_list},predlst) ;
    %end ;
    exclude_list = locin({&retrans_exclude_list},predlst) ;

    /* Compute the results. Note that all results except the delivery fraction get
       inflated by the retransformation bias factor. */
    retransform_factor = j(1,ncol(predlst),mean_exp_weighted_error) ;
    if any(exclude_list) then retransform_factor[,exclude_list] = 1 ;
    predict = retransform_factor # feval_predict(beta) ;
    %if %upcase(&if_adjust) = YES %then %do ;
      predict[obsloc,] = predict[obsloc,] # (1 / retransform_factor) ;
    %end ;
    predict = data[,(jwaterid || jstaid || jdepvar)] || predict ;

    /* Output predictions */
    create predict from predict [colname = varnames] ;
    append from predict ;
    close predict ;

  quit ;

%mend predict ;

/* ---------------------------------------------------------------------- */

