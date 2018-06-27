/* comp_files.sas
   written by: Greg Schwarz
   date: 10/3/05

   Program compares two ascii files and flags lines that differ.
*/

%let comp_file = sparrow_main.sas ;

%let dir1 = Y:\Greg\Sparrow\model\test ; /* Earlier version */
%let dir2 = D:\Greg\Sparrow\model\master ; /* Most recent version */

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

/* ---------------------------------------------------------------------- */

/* Macro compares the two files */

%macro comp_files ;

  filename infile1 "&dir1.\&comp_file" ;
  filename infile2 "&dir2.\&comp_file" ;

  proc datasets nolist ;
    delete file1 file2 ;
  run ;

  data file1 (keep = n x) file2 (keep = n y) ;
    n = _n_ ;
    length x y $ 1000 ;
    infile infile1 lrecl = 1000 truncover ;
    input x $ 1-1000 ;
    infile infile2 lrecl = 1000 truncover ;
    input y $ 1-1000 ;
    if _n_ in(302,365) then input y $ 1-1000 ;
    if x ^= y then output file1 file2 ;
  run ;
  %if %n_obs(file1) > 0 %then %put WARNING: Files are different. See File1 and File2 in the SAS Work Directory. ;
  %else %put The two files are the same. ;

%mend comp_files ;

%comp_files ;

