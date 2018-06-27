
/* Program name: apportion_data_to_ws_by_image.sas
   Date: 8/3/06
   Written by: Greg Schwarz

   Program creates a SAS data set consisting of either sums or means of input file data
   allocated to watersheds using ancillary land use image files.

   User must specify:
    - Path and name of the input data file.
    - Home directory where most of the input images are stored.
    - List of input images with specification information.
    - Variable name used to identify observations in the input data file and the name of an image that 
      shows the spatial extent of each identifier.
    - Variable name used to identify watersheds and the name of the image that shows the spatial extent
      of each watershed.
    - Modifications to any image file or input data variable.
    - List of output variables to create representing spatial and topical allocations of the input data,
      designated as sums or averages. Alternative allocation methods may be specified for a single
      sum output variable, the alternatives representing backup allocation methods should the primary
      method fail due to a zero value for the allocation variable for a unit of observation in the 
      input data file. Only one allocation method is available for output variables that are averages. 
    - Variable labels to be used in the SAS output data set.
    - Directory containing the output SAS data set.
    - Name of output SAS data set.
    - (optional) Name of the directory containing the file decompression utility wzunzip (required
      only if one or more of the input images is compressed using winzip). If wzunzip is not 
      available, then input images must all be initially uncompressed.

    Output consist of the SAS file specified by the control variable output_file_name stored in the
    directory specified by the control variable home_output. The output file lists the watershed id
    and all the output variables requested in the sum_vars_list and avg_vars_list. A prefix "sum_" is
    affixed to each requested sum variable, and a prefix "avg_" is affixed to each requested average 
    variable. 

    Only watersheds resolved in the watershed image file are allocated sums or averages - other
    watersheds that are not resolved and do not have an observation in the SAS output file.
    Consequently, if these data are merged with others to create a SAS data1 file for SPARROW, it is
    necessary to substitute default values for the non-included watersheds. For output variables
    representing sums, a valid default value is 0; for output variables defined as averages, there is
    no readily available default value other than an average of values from neighboring watersheds.
    Since watersheds that are not resolved by the watershed image are necessarily small, there is 
    little consequence from using a simple regional average (e.g., 8-digit HUC) value. The analysis
    specification includes the option of specifying an additional input file that gives a region
    code for each reach in the network (including non-resolved reaches). If this option is specified,
    the output file will include all reaches in the network, with unresolved reaches receiving 
    default values of 0 for sum variables and area-weighted region averages for average variables.
    A single variable named if_default is included in the output file that has a value of 1 for
    reaches recieving a default value and 0 otherwise. 

    The SAS file alloc_input_data, stored in the WORK library, contains detailed information on the
    variables used to do the allocation. Observations in this file pertain to individual areas formed
    by the intersection of the input data spatial units with watersheds. The included variables are:
    input data id variable, watershed id, the spatial extent of the intersection of the input data 
    spatial units with watersheds (expressed as number of grid cells from the image files), the
    data from the input data file, and the amount of the input data values allocated to each
    watershed, as specified by the sum_vars_list and avg_vars_list. Note that the variables allocated
    as sums have the prefix "sum_" and the variables allocated as averages have the prefix "avg_".
    The file also includes the numerators and denominators of the allocation weights, as specified
    by the sum_vars_list and avg_vars_list. The numerator for the Nth output variable in the
    sum_vars_list is denoted sum_var_functionN and represents the sum of all the allocation 
    variables specified for that sum variable across all grid cells having the specified input data
    id and watershed id. The denominator for the Nth output variable in the sum_vars_list is denoted
    idids_sum_var_functionN and represents the sum of all the allocation variables specified for that
    sum variable across all grid cells having the specified input data id. Similar constructs for
    the output variables formed as averages are also included in the file. The numerators of the 
    output variables allocated as averages are denoted with the prefix "avg_" and the corresponding
    denominators are denoted by the prefix "wsids_avg_". The variables having the prefix "mthd_sum_" 
    give the method used to make tha allocation of a specific sum variable. The value of the method 
    codes represents the allocation alternative used to make the allocation given by the corresponding 
    variable having the "sum_" prefix. For example, if there are 4 alternative allocation specifications 
    for the sum variable, a value of 2 for the mthd_sum_ variable indicates the second alternative was 
    used to make the allocation. An method code of 0 implies the default allocation method (as 
    determined by the spatial extent area) was used.
    
    If the analysis requests the allocation of sums, the SAS ouput listing will include a comparison
    of totals across all observations of the input data with totals computed across all watersheds.

   Notes:

     All input images must be in the same projection and resolution.

     The selection of which variables are to be treated as sums and which as averages is driven by the
     nature of the variable being alloted and the desired interpretation of the outcome. For example, if
     the desired output is population density by watershed based on county estimates, the input county
     data should be population density and the allotment method should be an average. In this way, the
     watershed estimate of population density is interpreted as an area weighted estimate of county
     population density, the weights being the proportion of watershed area that is in each contributing
     county. Another example in which averaging should be used is in the case of soil characteristics,
     such as soil permeability.

     An example of the use of summation involves the estimation of total land area associated with a 
     particular land use class within a watershed. In this case, the county information is the amount of
     county land having the given use class. By requesting summation, the output is the total estimated
     land of the given use class for the watershed.

     Method: Each grid cell in the image file is assigned a watershed id and an input data identifier (e.g.
     FIPS code). Due to slight inconsistencies in coastlines, or due to lack of resolution of very small
     watersheds or small input data spatial units, some cells recieve only a watershed id or only a input 
     data id. Many cells located in the open oceans have neither a watershed id or an input data identifier. 
     The input data information is merged with the image cells according to the input data identifier.
     For allocation of an average variable to a given watershed, a eighted mean of the average 
     variable is taken over all cells assigned to that watershed, the weights being given by the allocation
     variable (which is specified as the sum of values from one or more image files). For allocation of a sum, 
     if the option allocate_all_sums is set to yes, the input data are normalized by dividing
     by the sum of the allocation variable across all cells having the same input data identifier and a 
     non-missing watershed id. If the option allocate_all_sums is set to no, the input data are normalized by 
     dividing by the sum of the allocation variable across all cells having the same input data identifier,
     regardless of whether or not the cell also has a watershed id. The sum for a watershed is obtained by 
     multiplying the sum of the allocation variable for the spatial extent formed by the intersection of 
     the watershed and input data spatial unit by the normalized input data for each unit, and then
     summing these products across input units having the same watershed identifier.

     If allocate_all_sums is set to yes, then normalized sums for all input data spatial units that overlap 
     at least one watershed in the grid will be entierly allocated to one or more watersheds; sums for input 
     spatial units that do not overlap at least one watershed are not allocated to any watershed. Consequently, 
     the grand sum across all watersheds should be close to the grand sum across all input data spatial units, 
     but may be slightly smaller if there are some input data spatial units that have no overlap with any 
     watersheds. If allocate_all_sums is set to no, then a portion of the input data sums are not allocated to 
     watersheds, even if the input spatial unit intersects at least one watershed. Consequently, the grand sum 
     across all watersheds will be lower than with allocate_all_sums set to yes. If the allocated sums are 
     subsequently normalized by dividing each sum by the land area of the watershed, the resulting yields will 
     generally have less upward bias than the case where they are computed from sums with the allocate_all_sums 
     switch set to yes. In general, it is recommended, however, that if the intention is to compute a yield, the 
     calculation should be done in terms of the averaging method, whereby the variable is input data variable is
     first defined as a yield and then processed using the avg_vars_list specification.

     Note, if a watershed has no input data spatial extent, then there is no way to allocate the
     input data to that watershed. Consequently, the allocated value for that watershed is set to
     missing.

     Input data specified as sums that cannot be linked to a watershed are linked to the watershed denoted by
     the missing value code '.'.

     In specifying the analysis, if a given macro variable specification is repeated,
     only the last specification is implemented in the analysis.

*/

/**************************/
/* Analysis Specification */
/**************************/

/* Specify the list of input file path and filenames delimited by one or more spaces or line breaks.
   The input data files must be in either dbase (with a .dbf extension), tab-delimited text file 
   (with a .txt extension), or excel (with a .xls extension) format. */
%let input_data_list = F:\make_data\county_test_rev.dbf ;
%let input_data_list = D:\Greg\Sparrow\model\master\preprocesses\county_test_rev.dbf ;

/* Specify a macro variable designating a root directory in which most of the input images
   are stored. The root directory can then be referenced as "&home_image" in the subsequent
   input image definition statement (in_image_list). */
%let home_image = d:\greg\gis_coverages\images ;
%let home_image = D:\Greg\Sparrow\model\master\preprocesses ;

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
                     county_lg:1:0#&home_image#bsq#p
                     nlcd1k21_lg#&home_image#bsq#p
                     nlcd1k22_lg#&home_image#bsq#p
;

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

/* Specify the name of the input data set identifying variable and the name of the image defining its
   spatial extent. The form of the specification is:

      input_id:input_id_image_name

   where input_id is the name of the input data identifying variable, and input_id_image_name is the
   name of the image (previously defined in the in_image_list) that describes the spatial extent of
   the input data identifying variable.

   Example: If the input data are county data, identified by 5-digit code variable called fips, then 
   an appropriate input_data_id specification is:

     %let input_data_id_specification = fips:county_lg

   where county_lg is the name of the image containing the spatial extent information for the fips code. 
*/
%let input_data_id_specification = fips:county_lg ;

/* Specify the name of the watershed variable identifier and the name of the image defining its
   spatial extent. The form of the specification is:

      watershed_id:watershed_id_image_name

   where watershed_id is the name to be used for the watershed code given in the watershed spatial extent
   image file, previously declared in the in_image_list by the image name watershed_id_image_name.

   Example: If the watershed image name is erf1_2ws_lg, and the desired watershed id variable name is
   e2rf1, then an appropriate watershed_data_id specification is:

     %let watershed_id_specification = e2rf1:erf1_2ws_lg ;
*/
%let watershed_id_specification = e2rf1:erf1_2ws_lg  ;

/* Specify the list of variables from the input data files that are to be summed over watersheds.
   The format of the response is:

     input_variable_name_1:input_image_name_1_1#...#input_image_name_N_1
     input_variable_name_2:input_image_name_1_2#...#input_image_name_N_2 ...

   where input_variable_name_X is the name of the Xth input variable, stored in one of the input data files,
   and to be allocated to watersheds, and input_image_name_X_Y is the name of the Yth image file to be used
   to allocate the Xth input variable. It is assumed that if multiple images are used in an allocation, then
   the allocation is based on the sum of the values of these images. If input_variable_name_X is followed 
   with no input image names, then it is assumed that allocation is based on area of the areas formed by the 
   intersection of the spatial extent variables.

   For example, if the input data sums are for counties, indexed by c, and the allocation is to be for watersheds,
   indexed by w, then the implicit format is,

     input_variable_name_X(w) = Sum (over all c in w) input_variable_name_X(c) * 
       (input_image_name_1_X(w,c) + ... + input_image_name_N_X(w,c)) / 
       (input_image_name_1_X(c) + ... + input_image_name_N_X(c)),

   The allocation of sums sums the N input image values for each pixel, and assigns each pixel value to a given 
   county/watershed intersected area. The allocation of the input variable multiplies the value of the input
   variable for a county by the share of county pixel values that reside in a given watershed; these allocated
   values are summed over all counties in a given watershed. If there are no input variables specified following
   the ":", then it is assumed that the ratio is given by AREA(w,c) / AREA(c), where AREA(w,c) is the number of
   pixels in the watershed/county intersection, and AREA(c) is the number of pixels in the county. 

   Note that for this method to work, it must be the case that the denominator is non-zero; that is, there must
   be some non-zero pixel values for at least one of the allocation variables in each county. To accommodate 
   this contingency, it is possible to specify multiple occurences of the same input image name, each 
   specification requesting a different aggregation of the input image names. The order of these specifications 
   determines the order in which these alternatives are tested for positive values of the denominator. If all 
   specifications fail, then the default is to use the AREA(w,c) / AREA(c) specification.

   Note that For allocation of a sum, if the option allocate_all_sums is set to yes (see below), the denominator sum 
   of the aggregate input image pixel values may include cells that have missing values for the other spatial extent
   variables (the spatial extent variables other than the one defining the extent of the input variable). If the 
   option allocate_all_sums is set to no, the denominator sum of the aggregate input image pixel values includes only 
   pixels that have non-missing values for all the spatial extent variables.

   If allocate_all_sums is set to yes, then sums for all units of the input variable that overlap at least one
   watershed in the grid will be entierly allocated to one or more watersheds; sums for units of the input variable
   that do not overlap at least one watershed are not allocated to any watershed. Consequently, the grand sum across 
   all watersheds should be close to the grand sum across all units of the input variable, but may be slightly smaller 
   due to non-overlap with any watershed. If allocate_all_sums is set to no, then a portion of the input variable unit sums 
   are not allocated to watersheds based on the fraction of cells that have a input variable unit designation but no 
   watershed designation (this tends to occur along the coastline). Consequently, the grand sum across all watersheds
   will be lower than with allocate_all_sums set to yes. In general, it is recommended that if the intention is to 
   compute a sum that will subsequently (as a later step) be normalized by land area (e.g., the allocated input variable 
   will be used to compute a yield by dividing by area), the allocation should be done originally as an average (see 
   below), whereby the input variable is expressed as a yield, or area weighted average, rather than as a sum. This insures
   the allocation does not result in bias due to inconsistencies in the amount of the input variable sum that is included 
   in an allocation due to cases in which there is no overlap with other spatial extent variables. 

   Examples of sum variables are: acres irrigated - allocated by agricultural land, fertilizer application - allocated
   by agricultral land, soil erosion - allocated by type of land the erosion pertains to, and population - allocated by
   urban land.
*/
%let sum_vars_list = pop: area_km2 ;
%let sum_vars_list = pop:nlcd1k21_lg#nlcd1k22_lg 
                     area_km2:
                     pop:nlcd1k21_lg#nlcd1k22_lg#nlcd1k23_lg ;
%let sum_vars_list = pop:nlcd1k21_lg#nlcd1k22_lg 
                     area_km2: ;

/* Specify the list of variables from the input data files that are to be avreaged over watersheds.
   The format of the response is:

     input_variable_name_1:input_image_name_1_1#...#input_image_name_N_1
     input_variable_name_2:input_image_name_1_2#...#input_image_name_N_2 ...

   where input_variable_name_X is the name of the Xth input variable, stored in one of the input data files,
   and to be allocated to watersheds, and input_image_name_X_Y is the name of the Yth image file to be used
   to allocate the Xth input variable. It is assumed that if multiple images are used in an allocation, then
   the allocation is based on the sum of the values of these images. If input_variable_name_X is followed 
   with no input image names, then it is assumed that allocation is based on area of the areas formed by the 
   intersection of the spatial extent variables.

   For example, if the input data averages are for counties, indexed by c, and the allocation is to be for watersheds,
   indexed by w, then the implicit format is,

     input_variable_name_X(w) = Sum (over all c in w) input_variable_name_X(c) * 
       (input_image_name_1_X(w,c) + ... + input_image_name_N_X(w,c)) / 
       (input_image_name_1_X(w) + ... + input_image_name_N_X(w)),

   The allocation of averages sums the N input image values for each pixel, and assigns each pixel value to a given 
   county/watershed intersected area. The allocation of the input variable multiplies the value of the input
   variable for a county by the share of watershed pixel values that reside in a given county; these allocated
   values are summed over all counties in a given watershed. If there are no input variables specified following
   the ":", then it is assumed that the ratio is given by AREA(w,c) / AREA(w), where AREA(w,c) is the number of
   pixels in the watershed/county intersection, and AREA(w) is the number of pixels in the watershed. 

   Note that the allocation of averages is essentially a weighted average, where the weights are given by the 
   sum of the aggregated input image pixel values for each county/watershed intersection. Unlike the case with sums, 
   the allocation of averages to watersheds is always valid; the failure to allocate an average to a watershed
   does not violate any adding-up conditions that arise with the allocation of sums (e.g., the total of the 
   watershed-allocated input variable must equal the total of the input varialbe across all input variable units.
   Consequently, unlike with sums, only a single specification of the allocation formula can be specified for computing
   averages.

   Examples of average variables would be irrigation application by inches applied - allocated by agricultural
   land area, soil characteristics - allocated by land area, and population density - allocated by urban land area.
*/
%let avg_vars_list = pop_den ;

/* Specify if the entire sum is to be allocated to some watershed. A response of yes implies the sum of the sum
   variables across all watersheds equals the sum of these variables across all input variable units. A response
   of no implies a portion of the sum variables could be allocated to no watersheds based on the fraction of
   cells that have a input unit designation but no watershed designation (this tends to occur along the coastline).
   The no response implies the sum of the sum variables across all watersheds will be less than the sum across
   all input variable units. However, the subsequent computation of yield from the watersheds sums (that is the 
   computation of watershed sum divided by watershed area) is likely to be more accurate. It is suggested, however, 
   that if the intended use of the data is to compute yields, thta the allocation be done with the avg_vars_list, 
   rather than by dividing an allocated sum by watershed area.
*/
%let allocate_all_sums = yes ;

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
%let variable_labels = sum_pop:Population (100s)#sum_area_km2:County area (km2)#avg_pop_den:Population density#fips:State/County 5-digit FIPS code#e2rf1:Reach ID ;

/* Specify the name of the directory in which the output SAS data set is to be stored. */
%let home_output = d:\greg\sparrow\model\master\preprocesses ;

/* Specify the name of the output SAS file to be produced */
%let output_file_name = county_e2rf1_alloc ;

/* OPTIONAL - Specify the name of a file (dbase, tab delimited text, or excel spreadsheet) that
   specifies a region code for each reach. The file should contain a listing of all reaches, not
   just those resolved in the watershed image file. The variable designating the reach should be
   named the same as that specified in watershed_id_specification (see above). Specification of a
   valid region_region_file causes the program to include all reaches in the output file, with
   unresolved reaches receiving default values of 0 for sum variables and area-weighted averages
   for average variables. */
%let reach_region_file = F:\make_data\huc_codes_for_e2rf1.dbf ;
%let reach_region_file = d:\greg\sparrow\model\master\preprocesses\huc_codes_for_e2rf1.dbf ;

/* OPTIONAL - Specify the name of the region code used to identify regions in the reach_region_file
   (see above). */
%let region_id = huc ;

/* OPTIONAL - Specify the directory containing the wzunzip software. Leave blank if image files are
   not zipped. */
%let home_wzunzip = ;

/* Specify if the SAS log is to include a detailed listing of program steps. */
%let if_print_details = no ;

/*********************************/
/* End of Analysis Specification */
/*********************************/

/* ------------------------------------------------------------------------------------ */

libname dirout v8 "&home_output" ;

/* ------------------------------------------------------------------------------------ */

%macro print_log_option ;

  %if %upcase(&if_print_details) = YES %then %do ;
    options mprint macrogen notes source ;
  %end ;
  %else %do ;
    options nomprint nomacrogen nonotes nosource ;
  %end ;
  run ;

%mend print_log_option ;

%print_log_option ;

/* ------------------------------------------------------------------------------------ */

/* Macro sets the error flag and prints an error message to the SAS log. */

%macro error(message) ;

  %let iferr = yes ;
  %put ERROR: &message ;
  %put ERROR: &message ;
  %put ERROR: &message ;

%mend error ;

/* ---------------------------------------------------------------------- */

/* Macro counts the number of objects separated by a blank in a list */

%macro count(list) ;
  %local i_list var ;
  %let i_list = 1 ;
  %let var = %scan(&list,&i_list,' ') ;
  %do %while (%length(&var) > 0) ;
    %let i_list = %eval(&i_list + 1) ;
    %let var = %scan(&list,&i_list,' ') ;
  %end ;
  %eval(&i_list - 1)

%mend count ;

/* ---------------------------------------------------------------------- */

/* Macro function determines number of objects in a list */

%macro countd(invec,delimiter) ;
  %local i invec delimiter ;
  %let i = 1 ;
  %do %while (%length(%scan(&invec,&i,"&delimiter")) ^= 0) ;
    %let i = %eval(&i + 1) ;    
  %end ;
  %eval(&i - 1) 
%mend countd ;

/* ---------------------------------------------------------------------- */

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

/* Macro function returns the elements of the search_vars list that are not
   variable names found in the SAS data set in_file. */

%macro check_vars(in_file,search_vars) ;

  %local in_id not_found file_vars i search_var rc ;
  %let not_found = ;
  %if %sysfunc(exist(&in_file)) %then %do ;
    %let file_vars = ;
    %let in_id = %sysfunc(open(&in_file)) ;
    %do i = 1 %to %sysfunc(attrn(&in_id,nvars)) ;
      %let file_vars = &file_vars %sysfunc(varname(&in_id,&i)) ;
    %end ;
    %let file_vars = %upcase(&file_vars) ;
    %do i = 1 %to %count(&search_vars) ;
      %let search_var = %scan(&search_vars,&i,' ') ;
      %if %index(&file_vars,%upcase(&search_var)) = 0 %then %let not_found = &not_found &search_var ;
    %end ;
    %let rc = %sysfunc(close(&in_id)) ;
  %end ;
  %else %let not_found = &search_vars ; 
  &not_found

%mend check_vars ;

/* ---------------------------------------------------------------------- */

/* Macro function takes input text delimited by the delimiter
   denoted in delim and returns a list of items delineated by a 
   single space. */

%macro set_list(text,delim) ;
  %local vec ivec var text delim ;
  %let vec = ;
  %let ivec = 1 ;
  %let var = %scan(&text,&ivec,"&delim") ;
  %do %while (%length(&var) > 0) ;
    %let vec = &vec &var ;
    %let ivec = %eval(&ivec + 1) ;
    %let var = %scan(&text,&ivec,"&delim") ;
  %end ;
  &vec
%mend set_list ;

/* ---------------------------------------------------------------------- */

/* Macro function returns the union of all unique items in each
   of two vectors. The items in each input vector must be delineated
   by one or more spaces or line feeds. */

%macro union(vec1,vec2) ;
  %local vec1 vec2 all_vec result_list ivec iitem jvec jitem ;
  %let all_vec = &vec1 &vec2 ;
  %let ivec = 1 ;
  %let iitem = %scan(&all_vec,&ivec,' ') ;
  %let result_list = &iitem ;
  %let ivec = 2 ;
  %let iitem = %scan(&all_vec,&ivec,' ') ;
  %do %while (%length(&iitem) > 0) ;
    %let jvec = 1 ;
    %let jitem = %scan(&result_list,&jvec,' ') ;
    %do %while (%length(&jitem) > 0) ;
      %if %upcase(&iitem) = %upcase(&jitem) %then %let jitem = ;
      %else %do ;
        %let jvec = %eval(&jvec + 1) ;
        %let jitem = %scan(&result_list,&jvec,' ') ;
        %if %length(&jitem) = 0 %then %let result_list = &result_list &iitem ;
      %end ;
    %end ;
    %let ivec = %eval(&ivec + 1) ;
    %let iitem = %scan(&all_vec,&ivec,' ') ;
  %end ;
  &result_list
%mend union ;

/* ---------------------------------------------------------------------- */

/* Macro determines the unique allocation rules among all the 
   sum and avg input variable specifications. Redundant specifications
   are set equal to the first occurring specification. This saves on
   redundant adding of allocation image values. */

%macro set_unique_allocations ;

  %local id ivar type vec_1 jid jvar jtype vec_2 vec_union n_union ;
  %let id = 0 ;
  %do ivar = 1 %to %eval(&n_sum_var + &n_avg_var) ;
    %if &ivar = %eval(&n_sum_var + 1) %then %let id = 0 ;
    %let id = %eval(&id + 1) ;
    %if &ivar <= &n_sum_var %then %let type = sum ;
    %else %let type = avg ;
    %let vec_1 = %set_list(&&&type._var_function&id,+) ;
    %let jid = 0 ;
    %do jvar = 1 %to %eval(&ivar - 1) ;
      %if &jvar = %eval(&n_sum_var + 1) %then %let jid = 0 ;
      %let jid = %eval(&jid + 1) ;
      %if &jvar <= &n_sum_var %then %let jtype = sum ;
      %else %let jtype = avg ;
      %let vec_2 = %set_list(&&&jtype._var_function&jid,+) ;
      %let vec_union = %union(&vec_1,&vec_2) ;
      %let n_union = %count(&vec_union) ;
      %if &n_union = %count(&vec_1) and &n_union = %count(&vec_2) %then %do ;
        %let &type._var_function&id = &jtype._var_function&jid ;
        %let jvar = &ivar ;
      %end ;
    %end ;
  %end ;

%mend set_unique_allocations ;

/* ---------------------------------------------------------------------- */

/* Create macro variables needed for the analysis */

%macro make_macros ;

  /* Initiate macro variables */
  %global input_data_id watershed_id input_data_id_image_name watershed_id_image_name 
          sum_var_name_list avg_var_name_list sum_var_name_version_list avg_var_name_version_list
          n_in_image n_sum_var n_avg_var in_image_name_list alloc_image_name_list out_variable_labels 
          function_vars sum_function_vars avg_function_vars ;

  /* Determine the macro variables for the spatial extent variables */
  %let input_data_id_image_name = %scan(&input_data_id_specification,2,':') ;
  %let input_data_id = %scan(&input_data_id_specification,1,':') ;
  %let watershed_id_image_name = %scan(&watershed_id_specification,2,':') ;
  %let watershed_id = %scan(&watershed_id_specification,1,':') ;

  /* Process the image information */
  %let in_image_name_list = ;
  %let alloc_image_name_list = ;

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
    %if %upcase(&image_name) ^= %upcase(&input_data_id_image_name) and %upcase(&image_name) ^= %upcase(&watershed_id_image_name) %then
      %let alloc_image_name_list = &alloc_image_name_list &image_name ;

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

  /* Determine if any spatial extent image information has problems. */
  %if %length(&input_data_id_image_name) = 0 %then %error(Image name for the input data id spatial extent was not specified - stop processing.) ;
  %if %length(&watershed_id_image_name) = 0 %then %error(Image name for the watershed id spatial extent was not specified - stop processing.) ;
  %if %length(&input_data_id) = 0 %then %error(Input data id was not specified - stop processing.) ;
  %if %length(&watershed_id) = 0 %then %error(Watershed id was not specified - stop processing.) ;
  %if %sysfunc(indexw(&in_image_name_list,&input_data_id_image_name)) = 0 %then %error(Input data id spatial extent image name &input_data_id_image_name is not included in the in_image_list - stop processing.) ;
  %if %sysfunc(indexw(&in_image_name_list,&watershed_id_image_name)) = 0 %then %error(Watershed id spatial extent image name &watershed_id_image_name is not included in the in_image_list - stop processing.) ;
  %if %length(&reach_region_file) > 0 and %length(&region_id) = 0 %then %error(Reach_region_file was specified but region_id is null - stop processing.) ;

  /* Determine the macro variables for the output variables */

  %let function_vars = ;
  %let sum_function_vars = ;
  %let sum_var_name_list = ;
  %let avg_var_name_list = ;
  %let sum_var_name_version_list = ;
  %let avg_var_name_version_list = ;

  /* Process the sum variables */
  %let n_sum_var = %count(&sum_vars_list) ;
  %do i_var = 1 %to &n_sum_var ;

    %global sum_var_function&i_var ;
    %let var_name = %scan(%scan(&sum_vars_list,&i_var,' '),1,':') ;
    %let allocate_vars = %scan(%scan(&sum_vars_list,&i_var,' '),2,':') ;
    %let version = 1 ;
    %do j = 1 %to %eval(&i_var - 1) ;
      %if %upcase(&var_name) = %upcase(%scan(&sum_var_name_list,&j,' ')) %then %let version = %eval(&version + 1) ;
    %end ;
    %let sum_var_name_list = &sum_var_name_list &var_name ;
    %let sum_var_function&i_var = ; 
    %let i_alvar = 1 ;
    %let alvar = %scan(&allocate_vars,&i_alvar,'#') ;
    %if %length(&alvar) = 0 %then %do ;
      %let sum_var_function&i_var = spatial_extent_area ;
      %let sum_var_name_version_list = &sum_var_name_version_list 0 ;
    %end ;
    %else %do ;
      %do %while(%length(&alvar) > 0) ;
        %if %sysfunc(indexw(&in_image_name_list,&alvar)) = 0 %then %error(Aggregation image name &alvar is not included in the in_image_list - stop processing.) ;
        %let sum_var_function&i_var = &&sum_var_function&i_var + &alvar ; 
        %let i_alvar = %eval(&i_alvar + 1) ;
        %let alvar = %scan(&allocate_vars,&i_alvar,'#') ;
      %end ;
      %let sum_var_name_version_list = &sum_var_name_version_list &version ;
    %end ;
    %let function_vars = &function_vars sum_var_function&i_var ;
    %let sum_function_vars = &sum_function_vars sum_var_function&i_var ;

  %end ;

  /* Process the avg variables */

  %let n_avg_var = %count(&avg_vars_list) ;
  %let avg_function_vars = ;
  %let count = 0 ;
  %do i_var = 1 %to &n_avg_var ;
    %let ifskip = no ;
    %let var_name = %scan(%scan(&avg_vars_list,&i_var,' '),1,':') ;
    %let allocate_vars = %scan(%scan(&avg_vars_list,&i_var,' '),2,':') ;
    %do j = 1 %to &count ;
      %if %upcase(&var_name) = %upcase(%scan(&avg_var_name_list,&j,' ')) %then %do ;
        %put WARNING: Multiple allocation specifications were given for the average variable &var_name - only the first specification is used for computing averages. ;
        %let ifskip = yes ;
      %end ;
    %end ;
    %if &ifskip = no %then %do ;
      %let count = %eval(&count + 1) ;
      %let avg_var_name_list = &avg_var_name_list &var_name ;
      %global avg_var_function&count ;
      %let avg_var_function&count = ; 
      %let i_alvar = 1 ;
      %let alvar = %scan(&allocate_vars,&i_alvar,'#') ;
      %if %length(&alvar) = 0 %then %let avg_var_function&count = spatial_extent_area ;
      %else %do ;
        %do %while(%length(&alvar) > 0) ;
          %if %sysfunc(indexw(&in_image_name_list,&alvar)) = 0 %then %error(Aggregation image name &alvar is not included in the in_image_list - stop processing.) ;
          %let avg_var_function&count = &&avg_var_function&count + &alvar ; 
          %let i_alvar = %eval(&i_alvar + 1) ;
          %let alvar = %scan(&allocate_vars,&i_alvar,'#') ;
        %end ;
      %end ;
      %let function_vars = &function_vars avg_var_function&count ;
      %let avg_function_vars = &avg_function_vars avg_var_function&count ;
    %end ;
  %end ;
  %let n_avg_var = &count ;

  /* Set unique allocation functions across both the sum and avg input variable lists. */
  %set_unique_allocations ;

  /* Determine the variable labels */

  %let out_variable_labels = ;
  %let i_var = 1 ; 
  %let out_var_group = %scan(&variable_labels,&i_var,'#') ;
  %do %while (%length(&out_var_group) > 0) ;
    %let var = %scan(&out_var_group,1,':') ;
    %let label = %scan(&out_var_group,2,':') ;
    %if %sysfunc(indexw(&input_data_id &watershed_id %prescript(&sum_var_name_list,sum_) %prescript(&avg_var_name_list,avg_),&var)) > 0 %then %do ;
      %if %length(&label) > 0 %then %let out_variable_labels = &out_variable_labels label &var = &label %str(;) ;
    %end ;
    %else %put WARNING: Variable &var in the variable_labels list is not an input data id, watershed id, or sum/avg output variable. ;
    %let i_var = %eval(&i_var + 1) ;
    %let out_var_group = %scan(&variable_labels,&i_var,'#') ;
  %end ;

%mend make_macros ;

/* ---------------------------------------------------------------------------- */

/* Macro loads multiple input data files, as specified by 
   data_list, merges them according to the variable id_var,
   and creates the SAS dataset sas_file. */

%macro input_data(sas_file,data_list,id_var) ;

  proc datasets nolist ;
    delete &sas_file ;
  run ;
 
  %local ifile filenm extension method data_list sas_file id_var ;
  %if %length(&data_list) > 0 %then %do ;
    %do ifile = 1 %to %count(&data_list) ;
      %let filenm = %scan(&data_list,&ifile,' ') ;
      %let extension = %scan(&filenm,%countd(&filenm,.),'.') ;
      %let method = ;
      %if %upcase(&extension) = DBF %then %let method = dbf ;
      %else %if %upcase(&extension) = XLS %then %let method = excel ;
      %else %if %upcase(&extension) = TXT %then %let method = tab ;
      %else %error(Input data &filenm does not have a recognizable format (either dbase (.dbf), excel (.xls), or tab-delimited ascii (.txt)) - stop processing.) ;
      %if %sysfunc(fileexist(&filenm)) = 0 %then %error(Input file &filenm was not found - stop processing.) ;
      %if &iferr = no %then %do ;
        proc datasets nolist ;
          delete temp ;
        run ;
        proc import datafile = "&filenm" out = temp dbms = &method replace ;
        run ;
        %if %length(%check_vars(temp,&id_var)) > 0 %then %error(Input file &filenm does not include the input data id variable &id_var - stop processing.) ;
        %if &iferr = no %then %do ;
          proc sort data = temp ; by &id_var ;
          %if %sysfunc(exist(&sas_file)) %then %do ;
            data &sas_file ; merge &sas_file temp ; by &id_var ;
            run ;
          %end ;
          %else %do ;
            data &sas_file ; set temp ;
            run ;
          %end ;
        %end ;
      %end ;
    %end ;
  %end ;
  %else %do ;
    %error(No specification was given for &sas_file - stop processing.) ;
  %end ;

%mend input_data ;

/* ---------------------------------------------------------------------------- */

/* Macro loads the images into a single SAS data set */

%macro process_images ;

  proc datasets nolist ;
    delete images image_sums ;
  run ;

  /* Read in images and create the output variables, indexed by the watershed variable. */

  %put Processing image - this may take a while. ;

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
    &input_data_id = &input_data_id_image_name ;
    &watershed_id = &watershed_id_image_name ;

    if &input_data_id = . then delete ;

    keep &input_data_id &watershed_id &alloc_image_name_list ;

  run ;

  proc sort data = images ; by &input_data_id &watershed_id ;

  /* Compute the sums of the allocation variable images by input data id
     and waterhsed id. */

  proc means data = images noprint ;
    var &alloc_image_name_list ;
    by &input_data_id &watershed_id ;
    output out = image_sums sum = n = spatial_extent_area ;
  run ;

%mend process_images ;

/* ---------------------------------------------------------------------- */

/* Macro produces the watershed data set consisting of either the mean or sum
   (as defined by the argument "type") of the output variables for each watershed */

%macro make_watershed_data ;

  proc datasets lib = dirout nolist ;
    delete &output_file_name ;
  run ;
  proc datasets nolist ;
    delete id_sums function_vars ;
  run ;

  /* Compute the allocation (i.e., function) variables */
  
  data function_vars ; 
    set image_sums (keep = &input_data_id &watershed_id spatial_extent_area &alloc_image_name_list) ;

    %do i_var = 1 %to &n_sum_var ;
      sum_var_function&i_var = &&sum_var_function&i_var ;
    %end ;
    %do i_var = 1 %to &n_avg_var ;
      avg_var_function&i_var = &&avg_var_function&i_var ;
    %end ;

    keep &input_data_id &watershed_id spatial_extent_area &function_vars ;
  run ;

  /* Compute the sums by input data id. If allocate_all_sums is set to yes then only
     include observations in the sums that have a non-missing watershed data id.
     Output variables have the prefix idids_ (for input data id sum). */

  %if &n_sum_var > 0 %then %do ;
    proc means data = function_vars noprint ;
      %if %upcase(&allocate_all_sums) = YES %then where &watershed_id ^= . ;;
      var spatial_extent_area &sum_function_vars ;
      by &input_data_id ;
      output out = input_data_id_sums sum = %prescript(spatial_extent_area &sum_function_vars,idids_) ;
    run ;
  %end ;

  /* Compute the product of the input data value and the allocation weights for output variables
     representing sums. Retain information on the allocation method. */

  data allocate_input_data ;
    merge function_vars (in = in_image_sums) input_data (in = in_input_data)
          %if &n_sum_var > 0 %then input_data_id_sums (keep = &input_data_id idids_: in = in_id_sums) ;;
    by &input_data_id ;
    if in_input_data ;
    %if %upcase(&allocate_all_sums) = YES %then if &watershed_id ^= . ;;

    /* Compute the product of the input data sum variable and the ratio of the allocation
       variable watershed/input_id value to its input_id value. The code automatically
       uses an alternative specification if a previous specification had a non-valid
       denominator. If all alternative allocation specifications fail, the default
       specification based on spatial extent area is used. Create allocation method
       codes given by the version number of the allocation functions. If the default
       allocation method is used, then */
    %if &n_sum_var > 0 %then %do ;
      array alloc_sum_vars {*} %prescript(&sum_var_name_list,sum_) ;
      array sum_vars {*} &sum_var_name_list ;
      array sum_alloc_numerator {*} sum_var_function: ;
      array sum_alloc_denominator {*} idids_sum_var_function: ;
      array alloc_method {*} %prescript(&sum_var_name_list,mthd_sum_) ;
      do i = 1 to dim(alloc_sum_vars) ;
        if alloc_sum_vars{i} = . and sum_alloc_denominator{i} ^= 0 and sum_alloc_denominator{i} ^= . then do ;
          alloc_sum_vars{i} = sum_vars{i} * sum_alloc_numerator{i} / sum_alloc_denominator{i} ;
          alloc_method{i} = scan("&sum_var_name_version_list",i,' ') + 0 ;
        end ;
      end ;
      /* Invoke the default allocation, if necessary */
      do i = 1 to dim(alloc_sum_vars) ;
        if alloc_sum_vars{i} = . and idids_spatial_extent_area > 0 then do ;
          alloc_sum_vars{i} = sum_vars{i} * spatial_extent_area / idids_spatial_extent_area ;
          alloc_method{i} = 0 ;
        end ;
      end ;
      %if &n_avg_var = 0 %then %do ;
        &out_variable_labels ;
      %end ;
      drop i ;
    %end ;

  run ;

  proc sort data = allocate_input_data ; by &watershed_id ;

  /* Compute the product of the input data value and the allocation weights for output
     variables representing averages. */

  %if &n_avg_var > 0 %then %do ;

    proc sort data = function_vars ; by &watershed_id ;
    proc means data = function_vars noprint ;
      where &watershed_id ^= . and &input_data_id ^= . ;
      var &avg_function_vars ;
      by &watershed_id ;
      output out = watershed_id_sums sum = %prescript(&avg_function_vars,wsids_) ;
    run ;

    data allocate_input_data ; merge allocate_input_data watershed_id_sums (keep = &watershed_id wsids_:) ;
      by &watershed_id ;
      array alloc_avg_vars {*} %prescript(&avg_var_name_list,avg_) ;
      array avg_vars {*} &avg_var_name_list ;
      array avg_alloc_numerator {*} avg_var_function: ;
      array avg_alloc_denominator {*} wsids_avg_var_function: ;
      do i = 1 to dim(alloc_avg_vars) ;
        if avg_alloc_denominator{i} ^= 0 and avg_alloc_denominator{i} ^= . then 
          alloc_avg_vars{i} = avg_vars{i} * avg_alloc_numerator{i} / avg_alloc_denominator{i} ;
      end ;
      &out_variable_labels ;
      drop i ;
    run ;

  %end ;

  /* Compute the final allocations for the sum and average variables */

  proc means data = allocate_input_data noprint ;
    var spatial_extent_area %prescript(&sum_var_name_list,sum_) %prescript(&avg_var_name_list,avg_) ;
    by &watershed_id ;
    output out = watershed_allocations sum = ;
  run ;

  /* If a reach/region file is specified, create the default values 
     for unresolved reaches. Default values are zero for sum variables
     and region area-weighted averages for average variables. */

  %if %sysfunc(exist(reach_region_data)) %then %do ;

    %put Creating default values for unresolved reaches. ;

    proc sort data = reach_region_data ; by &watershed_id ;
    data watershed_allocations 
      missing_reaches (keep = &watershed_id &region_id)
      region_allocations (keep = &region_id spatial_extent_area %prescript(&avg_var_name_list,avg_)) ; 
      merge watershed_allocations (in = inws) 
            reach_region_data (keep = &watershed_id &region_id) ;
      by &watershed_id ;
      if inws = 0 then do ;
        output missing_reaches ;
        %if %length(&sum_var_name_list) > 0 %then %do ;
          array sum_vars {*} %prescript(&sum_var_name_list,sum_) ;
          do i = 1 to dim(sum_vars) ;
            sum_vars{i} = 0 ;
          end ;
          drop i ;
        %end ;
      end ;
      else do ;
        if &watershed_id ^= . then output region_allocations ;
        %if %length(&avg_var_name_list) > 0 %then %do ;
          array avg_vars {*} %prescript(&avg_var_name_list,avg_) ;
          ifmiss = 0 ;
          do i = 1 to dim(avg_vars) ;
            if avg_vars{i} = . then ifmiss = 1 ;
          end ;
          if ifmiss then output missing_reaches ;
          drop i ifmiss ;
        %end ;
      end ;
      output watershed_allocations ;
    run ;

    proc sort data = region_allocations ; by &region_id ;
    proc sort data = missing_reaches ; by &region_id ;

    proc means data = region_allocations noprint ;
      var %prescript(&avg_var_name_list,avg_) ;
      weight spatial_extent_area ;
      by &region_id ;
      output out = region_means mean = %prescript(&avg_var_name_list,ravg_) ;
    run ;

    data missing_reaches (keep = &watershed_id %prescript(&avg_var_name_list,avg_)) ;
      merge missing_reaches (in = inreach) 
            region_means (keep = &region_id ravg_:) ;
      by &region_id ;
      if inreach ;
      array avg {*} %prescript(&avg_var_name_list,avg_) ;
      array ravg {*} %prescript(&avg_var_name_list,ravg_) ;
      do i = 1 to dim(avg) ;
        if avg{i} = . then avg{i} = ravg{i} ;
      end ;
    run ;

    proc sort data = missing_reaches ; by &watershed_id ;

    data watershed_allocations ;
      update watershed_allocations missing_reaches (in = inmiss) ;
      by &watershed_id ;
      if_default = inmiss ;
      label if_default = Default allocation used (1 = Y, 0 = N) ;
    run ;

  %end ;

  /* The final output data set has the allocated sums and averages. Observations with
     missing values for watershed_id may have sums but will have no averages. */

  data dirout.&output_file_name ; set watershed_allocations (drop = _type_ _freq_) ;
    keep &watershed_id %prescript(&sum_var_name_list,sum_) %prescript(&avg_var_name_list,avg_) 
         %if %length(%check_vars(watershed_allocations,if_default)) = 0 %then if_default ;;
  run ;

%mend make_watershed_data ;

/* ---------------------------------------------------------------------- */

/* Macro checks to see if the necessary images have been subtotaled
   into the data set image_sums. If the data set does not exist, or
   if at least one variable is not found, then the macro process_images
   is executed to acquire the necessary image information. */

%macro get_image_data ;

  %local missing ;
  %let missing = %check_vars(image_sums,&input_data_id &watershed_id spatial_extent_area &alloc_image_name_list) ;
  %if %length(&missing) > 0 %then %process_images ;
  %else %put Using the previously acquired image files. ;

%mend get_image_data ;

/* ---------------------------------------------------------------------- */

/* Macro determines the grand total of all "sum" variables in the input data set and
   compares to the grand total of these variables across all watersheds. */

%macro compare_totals ;

  data compare_totals ; set dirout.&output_file_name ;
    if &watershed_id = . then disposition = "Unallocated" ;
    else disposition = "Allocated  " ;

  proc sort data = compare_totals ; by disposition ;

  proc means data = compare_totals noprint ;
    var %prescript(&sum_var_name_list,sum_) ;
    by disposition ;
    output out = compare_totals sum = ;
  run ;

  proc means data = input_data noprint ;
    var &sum_var_name_list ;
    output out = input_data_sums sum = ;
  run ;

  proc transpose data = compare_totals (drop = _type_ _freq_) 
    out = compare_totals (rename = (_label_ = Description))
    name = Watershed_variable ;
    id disposition ;
  run ;

  proc transpose data = input_data_sums (keep = &sum_var_name_list) 
    out = input_data_sums (drop = _label_ rename = (col1 = Input_data_total))
    name = Input_variable ;
  run ;

  data compare_totals ; merge input_data_sums compare_totals ;
    %if %upcase(&allocate_all_sums) = YES %then unallocated = input_data_total - allocated ;;
    label watershed_variable = Watershed variable ;
    label input_variable = Input variable ;
    label input_data_total = Input data total ;
    label description = Description ;
    label allocated = Total allocated to watersheds ;
    label unallocated = Total not allocated to watersheds ;
  run ;

  title Comparison of Input Data and Watershed Allocation Totals ;
  proc print data = compare_totals label noobs ;
  run ;

  title ;

%mend compare_totals ; 

/* ---------------------------------------------------------------------- */

/* Macro controls the processing. */

%macro main ;

  %let iferr = no ;
  %make_macros ;
  %if &iferr = no %then %input_data(input_data,&input_data_list,&input_data_id) ;
  %if &iferr = no and %length(&reach_region_file) > 0 %then %input_data(reach_region_data,&reach_region_file,&watershed_id) ;
  %if &iferr = no %then %do ;
    %get_image_data ;
    %if %sysfunc(exist(image_sums)) %then %make_watershed_data ;
    %if &n_sum_var > 0 %then %compare_totals ;
  %end ;

%mend main ;

%main ;
