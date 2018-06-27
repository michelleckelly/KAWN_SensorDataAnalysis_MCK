/* Program name: sparrow_compare_control_file_to_default.sas
   Date: 2/7/2006
   Written by: Greg Schwarz

   Purpose: Program compares a specified SPARROW control file to a default SPARROW control
   file to determine if there are any control variables in the default file that are absent from
   the comparison file. Absent control variables are written to the SAS output window along with
   the default specification (which may be abbreviated and could even be blank). Control variables
   that are absent from the comparison control file must be added in order to run SPARROW code
   that the default control file is configured to run. The program is used to update a previously
   created SPARROW control file to accommodate newer versions of the SPARROW code that require
   specification of new control variables. An update is necessary because a control file that has 
   a missing control variable will cause a fatal error in SPARROW execution.

   The program also creates the SAS file 'compare,' stored in the directory 'work,' that contains
   a side-by-side comparison of the (possibly abbreviated) control variable specifications
   for the default and comparison control files.

   Note that control variable specifications using the %str() SAS macro function (to imbed a 
   semicolon (';') in the specification without terminating the specification) will always be truncated
   at the first semicolon. That is, the listed specification may not include the full specification
   appearing in the control file. Also, control variable specifications that are specified on
   multiple lines in the control file will be truncated to contain only the specification
   included on the line with the %let macro command.

   To execute the program, you must edit the default_control_file and compare_control_file macro
   variable specifications immediately below to contain the full path and filenames of the default
   and comparison control files in the following macro statements. Then simply run the program in SAS.
   Generally, you will use the example control file included with the SPARROW software package
   as the default.
*/

%let default_control_file = D:\Greg\Sparrow\model\package\sparrow\master\sparrow_control_example.sas ;
%let compare_control_file = D:\Greg\Sparrow\1987_model\results\tn\sparrow_header_tn.sas ;

options nomacrogen nomprint nonotes nosource ;

/* ---------------------------------------------------------------------- */

/* Macro function determines the number of observations in a SAS dataset. Returns
   -1 if file does not exist. */

%macro n_obs(ds_name) ;

  %local ds_id n_obs rc ;
  %let ds_id = %sysfunc(open(&ds_name)) ;
  %if &ds_id > 0 %then %let n_obs = %sysfunc(attrn(&ds_id,nobs)) ;
  %else %let n_obs = -1 ;
  %let rc = %sysfunc(close(&ds_id)) ;
  &n_obs

%mend n_obs ;

/* ----------------------------------------------------------- */

%macro load_code(inref_name) ;

  proc datasets nolist ;
    delete control_specs ;
  run ;

  data control_specs (keep = control_var value control_id count) ;
    length x $ 1000 control_var $ 100 value result $ 1000 ;
    infile &inref_name lrecl = 1000 truncover ;
    input x $ 1-1000 ;
    if trim(left(x)) = 'Uncomment the following to do a Monte Carlo Analysis' then stop ;
    if index(x,'%let ') > 0 then do ;
      control_id + 1 ;
      control_var = scan(scan(x,1,'='),2,' ') ;
      result = scan(x,2,'=') ;
      if index(result,';') > 0 then do ;
        value = scan(result,1,';') ;
        count + 1 ;
        output ;
      end ;
      else do ;
        value = scan(result,1,';') ;
        count + 1 ;
        output ;
        do until (index(result,';') > 0) ;
          input result $ 1-1000 ;
          value = scan(result,1,';') ;
          count + 1 ;
          output ;
        end ;
      end ;
    end ;
  run ;

  proc sort data = control_specs ; by control_var descending control_id ;
  data control_specs ; set control_specs ; by control_var ;
    if first.control_var then check = control_id ;
    if control_id ^= check then delete ;
    drop check ;
  run ;

  proc sort data = control_specs ; by control_var count ;
  data control_specs ; set control_specs ;
    drop control_id ;
  run ;

%mend load_code ;

/* ----------------------------------------------------------- */

%macro compare_control_files ;

  proc datasets nolist ;
    delete control_specs default compare missing ;
  run ;

  filename default "&default_control_file" ;
  %if %sysfunc(fexist(default)) %then %do ;
    %load_code(default) ;
    data default ; set control_specs (rename = (value = default_value)) ; by control_var ;
      if first.control_var then do ;
        if first.control_var ^= last.control_var then default_value = default_value || ' ...' ;
        output ;
      end ;
    run ;
  %end ;
  %else %put ERROR: Default control file not found. ;
  %if %sysfunc(exist(default)) %then %do ;
    proc datasets nolist ;
      delete control_specs ;
    run ;
    filename compare "&compare_control_file" ;
    %if %sysfunc(fexist(compare)) %then %do ;
      %load_code(compare) ;
      data compare ; set control_specs (rename = (value = compare_value)) ; by control_var ;
        if first.control_var then do ;
          if first.control_var ^= last.control_var then compare_value = compare_value || ' ...' ;
          output ;
        end ;
      run ;
      data missing (keep = control_var default_value count) 
           compare (keep = control_var default_value compare_value count) ;
        merge default (in = indef) compare (in = incom drop = count) ; by control_var ;
        if indef = 1 and incom = 0 then output missing ;
        output compare ;
        label control_var = Control Variable ;
        label default_value = Default Setting ;
        label compare_value = Comparison Setting ;
      run ;
      %if %n_obs(compare) > 0 %then %do ;
        proc sort data = compare ; by count ;
        data compare ; set compare ; drop count ;
        run ;
      %end ;
    %end ;
    %else %put ERROR: Comparison control file not found. ;
  %end ;
  %if %n_obs(missing) > 0 %then %do ;
    %put ;
    %put ERROR: Comparison control file is missing specifications. See SAS output for missing control
variables and their (possibly abbreviated) specifications in the default control file. ;
    proc sort data = missing ; by count ;
    data missing ; set missing ; drop count ;
    run ;
    title Control Variables Missing from the Comparison Control File ;
    proc print data = missing noobs label ;
      var default_value ;
      id control_var ;
    run ;
    title ;
  %end ;
  %else %do ;
    %put ;
    %put There were no missing control variable specifications in the comparison control file. ;
  %end ;
  proc datasets nolist ;
    delete default control_specs ;
  run ;

%mend compare_control_files ;

/* ----------------------------------------------------------- */
 
%compare_control_files ;
