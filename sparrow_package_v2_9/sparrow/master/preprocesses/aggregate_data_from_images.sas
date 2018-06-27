/* Program name: aggregate_data_from_images.sas
   Date: 5/7/04
   Written by: Greg Schwarz

   Program creates a SAS data set consisting of either sums or means of spatially and topically
   aggregated image file information.

   User must specify:
    - Home directory where most of the input images are stored.
    - List of input images with specification information.
    - List of input images serving to define areas for spatial aggregation.
    - List of names to associate with the spatial aggregation input images.
    - Modifications to any image file variable.
    - List of output variables to create representing spatial and topical aggregations of the input images.
    - Variable labels to be used in the SAS output data set.
    - Designation of whether topical aggregation is a sum or mean.
    - Directory containing the output SAS data set.
    - Name of output SAS data set.
    - (optional) Name of the directory containing the file decompression utility wzunzip.

   Notes:

     All input images must be in the same projection and resolution.

     In specifying the analysis, if a given macro variable specification is repeated,
     only the last specification is implemented in the analysis.

     Input images can be compressed - the program will automatically uncompress them for
     use in the analysis and leave them uncompressed. To uncompress the files, the
     program requires the WINZIP utility "wzunzip". 
*/

/**************************/
/* Analysis Specification */
/**************************/

/* Specify a macro variable designating a root directory in which most of the input images
   are stored. The root directory can then be referenced as "&home_image" in the subsequent
   input image definition statement (in_image_list). */
%let home_image = d:\greg\gis_coverages\images ;

/* Specify the input images and filenames as:

     input_image_filename1:mult_factor1:missing_value1#input_image_home_directory1#input_image_type1:if_zipped1#if_positive1 
     input_image_filename2:mult_factor2:missing_value2#input_image_home_directory2#input_image_type2:if_zipped2#if_positive2 

   Where: 
     input_image_filename  = the file name of the image (not including the root directory)
     mult_factor           = the multiplication factor (10, 100, 1000, etc.) by which the values of the image
                             are multiplied to ensure floating values are integers (can be omitted is the
                             multiplication factor is one and there is no missing value code). If the multiplication
                             factor is one and there is a missing value code, then denote a blank for 
                             the multipliction factor by specifying input_image_filenamek::missing_value1#...
     missing_value         = the value signifying a missing value - leave blank if no missing value (specify 
                             the value AFTER deflating the image value by the multiplication factor) 
     input_image_directory = the directory in which the image is stored (use &home_image if the image is
                             stored in the image root directory - otherwise you must specify a path)
     input_image_type      = the input image format - either bsq or bil
     if_zipped             = zip if the input image is zipped, omit otherwise
     if_positive           = p if the image has only positive values, omit otherwise (if p is selected then the 
                             high bit is assumed to be part of the numerical value and does not indicate a negative)

   Note the delimiters within a specification are the characters ':' and '#' and the delimiter between
   individual input image specifications is a space.

   Example: image name = slope10_lg
            image multiplier = 10
            image missing value = none
            image directory = &home_image
            image type = bsq
            image zipped = no
            image has only positive values = no

   Enter: %let in_image_list = slope10_lg:10#&home_image#bsq ;

*/
%let in_image_list = erf1_2ws_lg:1:-9999#&home_image#bsq
                     fed_barren_lg#&home_image#bsq#p
                     fed_range_lg#&home_image#bsq#p
                     nlcd1k21_lg#&home_image#bsq#p
                     nlcd1k22_lg#&home_image#bsq#p
                     nlcd1k23_lg#&home_image#bsq#p
                     nlcd1k31_lg#&home_image#bsq#p
                     nlcd1k32_lg#&home_image#bsq#p
                     nlcd1k33_lg#&home_image#bsq#p
                     nlcd1k41_lg#&home_image#bsq#p
                     nlcd1k42_lg#&home_image#bsq#p
                     nlcd1k43_lg#&home_image#bsq#p
                     nlcd1k51_lg#&home_image#bsq#p
                     nlcd1k61_lg#&home_image#bsq#p
                     nlcd1k71_lg#&home_image#bsq#p
                     nlcd1k81_lg#&home_image#bsq#p
                     nlcd1k82_lg#&home_image#bsq#p
                     nlcd1k83_lg#&home_image#bsq#p
                     nlcd1k84_lg#&home_image#bsq#p
                     nlcd1k85_lg#&home_image#bsq#p
                     nlcd1k91_lg#&home_image#bsq#p
                     nlcd1k92_lg#&home_image#bsq#p
;
%let in_image_list = erf1_2ws_lg:1:-9999#&home_image#bsq
                     perm100_lg:100:-.1#&home_image#bsq
                     afldfreq10_lg:10:-.1#&home_image#bsq
                     slope10_lg:10:-.1#&home_image#bsq
                     kffact100_lg:100:-.1#&home_image#bsq
                     pctsand10_lg:10:-.1#&home_image#bsq
                     temp1000_lg:1000:0#&home_image#bsq#p
                     runoff_lg#&home_image#bsq#p
                     perdun_lg#&home_image#bsq#p
                     perhor_lg#&home_image#bsq#p
                     rusle_lpg#&home_image#bsq
;
%let in_image_list = county_lg:1:0#&home_image#bsq#p
                     erf1_2ws_lg:1:-9999#&home_image#bsq
                     nlcd1k00_lg#&home_image#bsq#p
                     nlcd1k81_lg#&home_image#bsq#p
                     nlcd1k82_lg#&home_image#bsq#p
                     nlcd1k83_lg#&home_image#bsq#p
                     nlcd1k84_lg#&home_image#bsq#p
;

/* Specify the names of the spatial extent variables and their corresponding input image names (defined in
   the in_image_list above) to be used to define extents for spatial aggregation. Each pixel of each spatial 
   extent image contains a code identifying the spatial entity to which the pixel belongs.

   The format of the specification is a spatial extent variable name, followed by the ':' delimiter, followed
   by the corresponding image name as defined in the in_image_list above. Multiple spatial extent variables
   can be listed, delimited by one or more spaces:

     spatial_extent_variable_name1:spatial_extent_image_name1 spatial_extent_variable_name2:spatial_extent_image_name2 ...

   Example: 

     %let spatial_extent_variables = fips:county_lg e2rf1:erf1_2ws_lg ;

 */
%let spatial_extent_variables = fips:county_lg e2rf1:erf1_2ws_lg ;

/* Specify any modifications to be applied to the input image values. Modification specification
   consists of 

     %let image_modifications = %str(SAS_data_step_modification_statements) ;

   where SAS_data_step_modifcation_statements consist of one or more SAS data step statements 
   to be used to modify the input images specified in the in_image_list. Note that any reference
   to image variables in this statement must use the input_image_filename as specified in the
   in_image_list specification, described above. The data step modifications may reference
   any image declared in the in_image_list. Mulitple SAS data step statements may be included
   by delimiting each by the semicolon, the SAS statement delimiter.

   For example, if the in_image_list is:

     %let in_image_list = fedland_lg:1#&home_image#bsq
                          slope10_lg:10#&home_image#bsq ;

   a valid image_modifications statement could be:

     %let image_modifications = %str(
       fedland_lg = (fedland_lg < 2 or fedland_lg = 9 or slope10_lg = 0) ;
       if fedland_lg = 0 ;
     ) ;

   The first SAS data step statement causes fedland_lg to take the value of 1 if the original feland_lg
   has a value of 9 or less than 2 (the cell is located in federal land), and the value of the slope10_lg 
   image is greater than zero. The second SAS data step statement deletes a cell if the fedland_lg variable
   has a value of 1. Thus, the result is to retain only those cells that are not on federal land and have
   some slope.
*/
%let image_modifications = ;

/* Specify the output variables.

   Format is output_variable_name1:input_image_name11#input_image_name21#...#input_image_nameN1:out_mult_factor1 
             output_variable_name2:input_image_name12#input_image_name22#...#input_image_nameN2:out_mult_factor2
             ...
   where output_variable_name1 is the name of the first variable to appear in the output data set.
   Follow the ':' delimiter with a list of input image variables that are to be aggregated, each 
   delimited by #. If there is a multiplication factor, follow the list of images to be aggregated 
   with a second ':' delimiter followed by the output multiplication factor for the first output 
   variable. If there is no output mulitplication factor, then omit the second ':' delimiter and 
   out_mult_factor. 

   Follow the first ouput variable specification with at least one blank space and then give the
   specification for the second output variable (if needed). Repeat for additional output variable 
   specifications.
   
   Note that each output variable specification consists of three components, delimited by
   a ':', a variable name, a list of input images to be aggregated to form the output variable 
   (each delimited by a '#'), and an ouput multiplication factor (if needed). There should be no
   spaces within a given output variable specification, only spaces between them.

   The equations implied by this specification are:

     output_variable_name1 = out_mult_factor1 * (input_image_name11 + ... + input_image_nameN1)
     output_variable_name2 = out_mult_factor2 * (input_image_name12 + ... + input_image_nameN2)

   Example:

     The following example illustrates a case in which two output variables are to be created,
     fed_barren and range, representing the amount of land in the watershed classified
     as federal barren land and the amount of land in the watershed classified as rangeland. 

     The first variable is constructed by simply performing a spatial aggregation of a single 
     image file - fed_barren_lg. Becuase the image data pertain to the number of 30m cells within
     each 1km pixel that are classified as federal barren land, a multiplication factor must
     be used to convert the cell counts to square kilometers. A single count represents 
     30x30 = 9E2 square meters. There are 1E6 square meters in each square kilometer. Therefore,
     a single count represents 9E-4 (9E2 / 1E6) square kilometers and the appropriate multiplication
     factor is 9E-4.
     
     The second output variable, representing the amount of land in each watershed that is 
     rangeland, is constructed by aggregating two input images - an image for pasture land
     (land use classification code 51) and an image for rangeland (land use classification code
     71). Again, the underlying images are 1km pixels containing counts of 30m cells. Therefore,
     the appropriate multiplication factor is 9E-4.

     The output variable list is:

     %let out_variable_list = fed_barren:fed_barren_lg:9E-4
                              non_fed_range:nlcd1k51_lg#nlcd1k71_lg:9E-4 ;
   
*/
%let out_variable_list = fed_barren:fed_barren_lg:9E-4
                         fed_range:fed_range_lg:9E-4
                         urban:nlcd1k21_lg#nlcd1k22_lg#nlcd1k23_lg#nlcd1k85_lg:9E-4
                         barren:nlcd1k31_lg#nlcd1k32_lg#nlcd1k33_lg:9E-4
                         forest:nlcd1k41_lg#nlcd1k42_lg#nlcd1k43_lg:9E-4
                         range:nlcd1k51_lg#nlcd1k71_lg:9E-4
                         orchard:nlcd1k61_lg:9E-4
                         pasture:nlcd1k81_lg#nlcd1k84_lg:9E-4
                         crop:nlcd1k82_lg#nlcd1k83_lg:9E-4
                         wetland:nlcd1k91_lg#nlcd1k92_lg:9E-4
;
%let out_variable_list = perm:perm100_lg
                         afldfreq:afldfreq10_lg
                         slope:slope10_lg
                         kffact_usle:kffact100_lg
                         pctsand:pctsand10_lg
                         gebert_runoff:runoff_lg
                         mean_air_temp:temp1000_lg
                         rfact_usle:rusle_lpg
                         pct_dunne_runoff:perdun_lg
                         pct_horton_runoff:perhor_lg
;
%let out_variable_list = pasture:nlcd1k81_lg#nlcd1k84_lg:9E-4
                         crop:nlcd1k82_lg#nlcd1k83_lg:9E-4
                         total:nlcd1k00_lg:9E-4
;

/* OPTIONAL - Specify variable labels for output or spatial extent variables using the format:

     variable_name1:variable_label1#variable_name2:variable_label2#...

   where variable_nameK is the name of the Kth output or spatial extent variable, as designated
   by out_variable_list and spatial_extent_variables, and variable_labelK is the label to be 
   associated with the Kth variable. Note that the delimeter between variables is '#', not a 
   space. Note also that labels will be truncated at 40 characters in length. Finally, the
   specification of labels is optional so not all output variables require a label.

   Example:
     Suppose there are two output variables, pasture and crop, and a single spatial extent variable
     e2rf1 having labels. The specification is:

     %let out_variable_labels = pasture:Pasture land area (km2)#crop:Cropland area (km2)#e2rf1:Reach ID ;

*/
%let variable_labels = pasture:Pasture land area (km2)#crop:Cropland area (km2)#total:Total land area (km2)#
fips:State/County 5-digit FIPS code#e2rf1:Reach ID ;

/* Specify if analysis is to produce watershed sums or watershed averages. All output
   variables within a given program run are processed the same way.  */
%let if_watershed_sum = yes ;
%let if_watershed_sum = no ;
%let if_watershed_sum = yes ;

/* Specify the name of the directory in which the output SAS data set is to be stored. */
%let home_output = d:\greg\sparrow\make_data\sediment ;
%let home_output = d:\greg\sparrow\model\flow\ancillary_data ;

/* Specify the name of the output SAS file to be produced */
%let output_file_name = image_watershed_sums ;
%let output_file_name = image_watershed_avgs ;
%let output_file_name = county_e2rf1_sums ;

/* Specify the directory containing the wzunzip software. Leave blank if image files are
   not zipped. */
%let home_wzunzip = ;

/*********************************/
/* End of Analysis Specification */
/*********************************/

/* ------------------------------------------------------------------------------------ */

libname dirout v8 "&home_output" ;

options mprint macrogen ;

/* ------------------------------------------------------------------------------------ */

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

/* Macro sets the error flag and prints an error message to the SAS log. */

%macro error(message) ;

  %let iferr = yes ;
  %put ERROR: &message ;
  %put ERROR: &message ;
  %put ERROR: &message ;

%mend error ;

/* ---------------------------------------------------------------------- */

%macro make_macros ;

  /* Initiate macro variables */
  %global n_in_image n_out_var in_image_name_list spatial_extent_variables_list create_spatial_extent_variables 
          check_missing_list out_var_name_list out_variable_labels ;

  %let in_image_name_list = ;
  %let out_var_name_list = ;
  %let make_output_variables = ;

  /* Determine the number of images to process */
  %let n_in_image = %count(&in_image_list) ;

  /* Loop over images to get the image specifications */
  %do i_image = 1 %to &n_in_image ;

    /* Initiate image-specific macro variables */
    %global inim&i_image innrow&i_image inncol&i_image innbits&i_image inulx&i_image inuly&i_image 
            incelszx&i_image incelszy&i_image inbytes&i_image inmult&i_image inmissval&i_image insign&i_image ;

    /* Define the image file specifications */
    %let image_name = %scan(%scan(%scan(&in_image_list,&i_image,' '),1,'#'),1,':') ;
    %let image_mult = %scan(%scan(%scan(&in_image_list,&i_image,' '),1,'#'),2,':') ;
    %let image_missval = %scan(%scan(%scan(&in_image_list,&i_image,' '),1,'#'),3,':') ;
    %let image_home = %scan(%scan(&in_image_list,&i_image,' '),2,'#') ;
    %let image_type = %scan(%scan(%scan(&in_image_list,&i_image,' '),3,'#'),1,':') ;
    %let image_zip  = %scan(%scan(%scan(&in_image_list,&i_image,' '),3,'#'),2,':') ;
    %let image_sign = %scan(%scan(&in_image_list,&i_image,' '),4,'#') ;

    %let in_image_name_list = &in_image_name_list &image_name ;

    %if %upcase(&image_zip) = ZIP %then %do ;
      x "&home_wzunzip.\wzunzip.exe &image_home.\&image_name.. &image_home.\" ;
    %end ;

    %if %upcase(&image_type) = BSQ %then %let image_spec = bqw ;
    %else %if %upcase(&image_type) = BIL %then %let image_spec = blw ;

    %let insign&i_image = &image_sign ;
    %if %length(&image_mult) > 0 and &image_mult >= 10 %then %let inmult&i_image = %sysfunc(log10(&image_mult)) ;
    %else %let inmult&i_image = ;
    
    /* Create the file references for image header information */
    filename inhdr "&image_home.\&image_name..hdr" ;
    filename inspec "&image_home.\&image_name..&image_spec" ;
    filename inim&i_image "&image_home.\&image_name..&image_type" recfm = n ;
    %if %sysfunc(fexist(inim&i_image)) = 0 %then %error(Image file &image_home.\&image_name..&image_type for input image &image_name does not exist.) ;

    /* Input the image header information */

    %if %sysfunc(fexist(inhdr)) = 0 %then %error(Header file &image_home.\&image_name..hdr for input image &image_name does not exist.) ;
    %else %do ;
      data inhdr ;

        length x code $ 50 ; 
        infile inhdr truncover ;
        input x $ 1-50 ;
        code = upcase(trim(left(scan(x,1,' ')))) ;
        if code = 'NROWS' then call symput("innrow&i_image",trim(left(scan(x,2,' ')))) ;
        else if code = 'NCOLS' then call symput("inncol&i_image",trim(left(scan(x,2,' ')))) ;
        else if code = 'NBITS' then call symput("innbits&i_image",trim(left(scan(x,2,' ')))) ;
        else if code = 'ULXMAP' then call symput("inulx&i_image",trim(left(scan(x,2,' ')))) ;
        else if code = 'ULYMAP' then call symput("inuly&i_image",trim(left(scan(x,2,' ')))) ;
        else if code = 'XDIM' then call symput("incelszx&i_image",trim(left(scan(x,2,' ')))) ;
        else if code = 'YDIM' then call symput("incelszy&i_image",trim(left(scan(x,2,' ')))) ;

      run ;

    %end ;

    %if %length(&&incelszx&i_image) = 0 or
        %length(&&incelszy&i_image) = 0 or
        %length(&&inulx&i_image) = 0 or 
        %length(&&inuly&i_image) = 0 %then %do ;

      %if %sysfunc(fexist(inspec)) = 0 %then %error(Specification file &image_home.\&image_name..&image_spec for input image &image_name does not exist.) ;
      %else %do ;

        data inspec ;
          length x $ 50 ; 
          infile inspec truncover ;
          input x $ 1-50 ;
          if _n_ = 1 then call symput("incelszx&i_image",trim(left(scan(x,1,' ')))) ;
          else if _n_ = 4 then call symput("incelszy&i_image",trim(left(abs(scan(x,1,' '))))) ;
          else if _n_ = 5 then call symput("inulx&i_image",trim(left(scan(x,1,' ')))) ;
          else if _n_ = 6 then call symput("inuly&i_image",trim(left(scan(x,1,' ')))) ;
        run ;

      %end ;

    %end ;

    /* Determine the number of bytes contained in each pixel of the input image */
    %let inbytes&i_image = %sysevalf(&&innbits&i_image / 8) ;
    %if &&inbytes&i_image < 1 %then %let inbytes&i_image = 1 ;
    %let inmissval&i_image = &image_missval ;

  %end ;

  /* Determine the macro variables for the spatial extent variables */

  %let spatial_extent_variables_list = ;
  %let create_spatial_extent_variables = ;
  %let check_missing_list = ;
  %let logical_operator = ;
  %do i_var = 1 %to %count(&spatial_extent_variables) ;
    %let extent = %scan(&spatial_extent_variables,&i_var,' ') ;
    %let var = %scan(&extent,1,':') ;
    %let name = %scan(&extent,2,':') ;
    %if %length(&name) = 0 %then %error(Spatial extent variable/image specifcation is incorrect for variable &var.) ;
    %if %sysfunc(indexw(&in_image_name_list,&name)) = 0 %then %error(Spatial extent image name &name is not included in the in_image_list) ;
    %let spatial_extent_variables_list = &spatial_extent_variables_list &var ;
    %let create_spatial_extent_variables = &create_spatial_extent_variables &var = &name %str(;) ;
    %let check_missing_list = &check_missing_list &logical_operator &var = . ;
    %let logical_operator = or ;
  %end ;

  /* Determine the macro variables for the output variables */

  %let n_out_var = %count(&out_variable_list) ;

  %let out_var_name_list = ;
  %let out_var_labels = ;
  %do i_var = 1 %to &n_out_var ;

    %global out_var_function&i_var ;
    %let var_name = %scan(%scan(&out_variable_list,&i_var,' '),1,':') ;
    %let var_agvars = %scan(%scan(&out_variable_list,&i_var,' '),2,':') ;
    %let mult_fact = %scan(%scan(&out_variable_list,&i_var,' '),3,':') ;

    %let out_var_name_list = &out_var_name_list &var_name ;

    %let out_var_function&i_var = &var_name = ( ; 
    %let i_agvar = 1 ;
    %let agvar = %scan(&var_agvars,&i_agvar,'#') ;
    %if %length(&agvar) = 0 %then %error(Output variable &var_name has no aggregation image names) ;
    %do %while(%length(&agvar) > 0) ;
      %if %sysfunc(indexw(&in_image_name_list,&agvar)) = 0 %then %error(Aggregation image name &agvar is not included in the in_image_list) ;
      %let out_var_function&i_var = &&out_var_function&i_var + &agvar ; 
      %let i_agvar = %eval(&i_agvar + 1) ;
      %let agvar = %scan(&var_agvars,&i_agvar,'#') ;
    %end ;
    %if %length(&mult_fact) > 0 %then %let mult_fact = * &mult_fact ;
    %let out_var_function&i_var = &&out_var_function&i_var) &mult_fact ;

  %end ;

  /* Determine the variable labels */

  %let out_variable_labels = ;
  %let i_var = 1 ; 
  %let out_var_group = %scan(&variable_labels,&i_var,'#') ;
  %do %while (%length(&out_var_group) > 0) ;
    %let var = %scan(&out_var_group,1,':') ;
    %let label = %scan(&out_var_group,2,':') ;
    %if %sysfunc(indexw(&spatial_extent_variables_list &out_var_name_list,&var)) > 0 %then %do ;
      %if %length(&label) > 0 %then %let out_variable_labels = &out_variable_labels label &var = &label %str(;) ;
    %end ;
    %else %put WARNING: Variable &var in the variable_labels list is not a spatial extent or output variable. ;
    %let i_var = %eval(&i_var + 1) ;
    %let out_var_group = %scan(&variable_labels,&i_var,'#') ;
  %end ;

%mend make_macros ;

/* ---------------------------------------------------------------------- */

/* Macro loads the images into a single SAS data set */

%macro process_images ;

  proc datasets nolist ;
    delete images ;
  run ;

  /* Read in images and create the output variables, indexed by the watershed variable. */

  data images ;

    /* Input image file information */
    %do i_image = 1 %to &n_in_image ;
      %let image_name = %scan(&in_image_name_list,&i_image,' ') ;
      infile inim&i_image ;
      input &image_name &&insign&i_image..ib&&inbytes&i_image...&&inmult&i_image @@ ;
      %if %length(&&inmissval&i_image) > 0 %then %do ;
        if &image_name = &&inmissval&i_image then &image_name = . ;
      %end ;
    %end ;

    /* Process image modifications */
    &image_modifications ;

    /* Create the spatial extent variables */
    &create_spatial_extent_variables ;

    if &check_missing_list then delete ;

    %do i_var = 1 %to &n_out_var ;
      &&out_var_function&i_var ;
    %end ;

    keep &spatial_extent_variables_list &out_var_name_list ;

  run ;

  proc sort data = images ; by &spatial_extent_variables_list ;

%mend process_images ;

/* ---------------------------------------------------------------------- */

/* Macro produces the watershed data set consisting of either the mean or sum
   (as defined by the argument "type") of the output variables for each watershed */

%macro make_watershed_data(type) ;

  proc datasets lib = dirout nolist ;
    delete &output_file_name ;
  run ;
  
  proc means data = images noprint ;
    var &out_var_name_list ;
    by &spatial_extent_variables_list ;
    output out = dirout.&output_file_name &type = &out_var_name_list ;
  
  data dirout.&output_file_name ; set dirout.&output_file_name ;
    &out_variable_labels ;
    keep &spatial_extent_variables_list &out_var_name_list ;
  run ;

%mend make_watershed_data ;

/* ---------------------------------------------------------------------- */

/* Macro controls the processing. */

%macro main ;

  %let iferr = no ;
  %make_macros ;
  %if &iferr = no %then %do ;
    %process_images ;
    %if %sysfunc(exist(images)) %then %do ;
      %if %upcase(&if_watershed_sum) = YES %then %make_watershed_data(sum) ;
      %else %make_watershed_data(mean) ;
    %end ;
  %end ;

%mend main ;

%main ;
