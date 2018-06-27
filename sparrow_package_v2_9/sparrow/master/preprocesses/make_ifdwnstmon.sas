/* Program name: make_ifdwnstmon.sas
   Written by: Greg Schwarz
   Date: 7/23/07

   Purpose: Program takes input SAS data set for a reach network containing monitoring 
   data and reach navigation variables and determines those reaches that have a
   monitoring station on the given reach or on some reach downstream of the given reach.
   Program returns a SAS data set, in the same directory as the input data set, consisting
   of two variables: the reach identifier and a 0/1 variable called ifdwnstmon that is
   1 if there is a monitoring station on or downstream of the given reach, and 0 otherwise.
   The user is required to specify the names of reach identifier, monitored load, and 
   navigation variables: to node, from node, hydro sequence, the ordering protocol, and 
   divergence fraction. [Note, if the divfrac variable is not present in your data set, 
   and you are using NHDPlus, then the divergence fraction can be constructed from the 
   NHDPlus variable "divergence" using the substitute SAS code used to input the SAS data 
   set (see code below)].
*/     

/* Specify the directory containing the input SAS data set (DATA1) */
%let dir_data = D:\Greg\Sparrow\model\package\sparrow\data ;

/* Specify the data1 file */
%let input_SAS_data = sparrow_data1 ;

/* Specify the reach identifier */
%let waterid = ComID ;
%let waterid = waterid ;

/* Specify the compact tonode variable */
%let tonode = ctonode ;
%let tonode = tnode ;

/* Specify the compact fromnode variable */
%let fromnode = cfromnode ;
%let fromnode = fnode ;

/* Specify the hydro sequence variable */
%let hydroseq = hydrosequence ;
%let hydroseq = hydseq ;

/* Specify the ordering of the hydroseq variable (0 = downstream (e.g., RF1), 1 = upstream (e.g., NHDPlus)) */
%let seqorder = 1 ;
%let seqorder = 0 ;
 
/* Specify the divergence fraction variable */
%let divfrac = divfrac ;
%let divfrac = frac ;

/* Specify the monitored flux variable */
%let depvar = tnflux ;
%let depvar = depvar ;

/* ---------------------------------------------------------- */

libname dir_data "&dir_data" ;

%macro setdata ;

  data indata ; set dir_data.&input_SAS_data (keep = &waterid &tonode &fromnode &hydroseq &divfrac &depvar) ;
    if &tonode > 0 and &fromnode > 0 ;
  run ;

  /* Substitute the following code for the above code if you need to construct the divfrac variable from
     the divergence variable included in NHDPlus 
  data indata ; set dir_data.&input_SAS_data (keep = &waterid &tonode &fromnode &hydroseq divergence &depvar) ;
    if &tonode > 0 and &fromnode > 0 ;
    &divfrac = (divergence <= 2) ;
    drop divergence ;
  run ;
  */

  proc sort data = indata ; 
    by 
    %if &seqorder = 0 %then descending ;
    &hydroseq ;
  run ;

%mend setdata ;

%macro set_ifdwnstmon ;

  proc iml ;

    use indata ;
    read all var {&waterid} into &waterid ;
    read all var {&tonode &fromnode} into navdata ;
    read all var {&divfrac} into divfrac ;
    read all var {&depvar} into ifdwnstmon ;
    close indata ;

    jto = 1 ;
    jfrom = 2 ;

    nreach = nrow(navdata) ;
    nnodes = max(navdata) ;
    nodes = j(nnodes,1,0) ;
    divfrac = (divfrac > 0) ;
    ifdwnstmon = (ifdwnstmon > 0) ;

    do i = 1 to nreach ;
      if nodes[navdata[i,jto]] then ifdwnstmon[i] = 1 ;
      if (nodes[navdata[i,jfrom]] | (divfrac[i] & ifdwnstmon[i])) then nodes[navdata[i,jfrom]] = 1 ;
    end ;

    create dir_data.ifdwnstmon var {&waterid ifdwnstmon} ;
    append ;
    close dir_data.ifdwnstmon ;

  quit ;

%mend set_ifdwnstmon ;

%macro main ;

  %setdata ;
  %set_ifdwnstmon ;

%mend main ;

%main ;
