/* Program name: SPARROW_create_GIS.sas
   Written by: Greg Schwarz
   Date: 1/28/03

   Purpose: Program creates spatial data sets required for mapping residuals and viewing reach
            predictions in SAS/GIS. Note that SAS/GIS does not fully automate all of its GIS
            features. After running this program you must open the resids, residmap, and reach
            maps and perform the following manual edits:

   Edits to resids_map

     Load the Resids_map map into SAS/GIS and edit. In SAS/GIS, click on File, Open Map and 
     select the dir_gis directory, the residmap catalog and then the resids_map map icon (a globe). The map should 
     display with both the States2m and Mapresids layers showing and with the details showing
     for the States2m layer. If this is not the case, then be sure both layer boxes are checked
     and right click on the States2m layer box and select Show Details.

       Create a Theme for the Mapresids layer:

         Right click on the Mapresids layer box and select Edit.
         Click on the Thematic box.
         Click on the Theme Range box - this opens the GIS Thematic Layer Ranges definition window.
         Click on the Specified box.
         Click on Add Break and enter -1.5. Click on Apply.
         Click on Add Break and enter 1.5. Click on Apply.
         Click on Remove Break and select all values except -1.5, 0, and 1.5.
         Click OK - this returns you to the GIS Layer window where you can now edit the symbols (if needed).

       Set the Map Display Projection:

         Click on Tools, Map Properties, Map Options to open the Map Options window.
         Click on the Projections box to open the Projection Options window.
         Set the Display Projection System to Alber's Conical - you may have to reduce the Units Multiplier
           to 10000.
         Click the OK box to confirm selection and answer Yes to the Save Changes request (if you receive
           a notice to overwrite the label, select YES).

       Set the Map Background Color:

         Click on Tools, Map Properties, Colors... to open the Map Styles and Colors window.
         Set the Background color to CXE1E9DA.
         Set the highlight color to cyan.
         Set the anchor color to pink.
         Click on OK and Close and respond Yes to the Save Changes request.

       Create a Legend for the Map:

         Click on View, Legend, New to open the Lengend Options window.
         Click on MAPRESIDS, deselect Frame, be sure Show Missing Values is set.
         Set the Text Attributes to be Font: Ariel, 12 point, Color: CX336CD7 (blue).
         Enter the footnote text: (+) under-predict, (-) over-predict.
         Click OK and place the legend on the map. If you need to edit the location or contents then
           right click over the legned and select Edit.

       If you need to edit the title location, do the following:

         Right click on the existing title and select Move...
         Move the title to the desired location.

       Create an Action that allows you to click on a point and have the point displayed in FSView table.

         Click on Action, Define.
         Select New; click on Type and select View. 
         Click on Save and then Close.

       Make the Mapresids layer selectable by right-clicking on the Mapresids button above the map and
       selecting Make Layer Selectable

       Save the map by clicking on File, Save, All.

   Edits to reach_map map:

     Load the reach_map map into SAS/GIS and edit. In SAS/GIS, click on File, Open Map and 
     select the dir_gis directory, the reach_map catalog and then the reach_map map icon (a globe). 
     The map should display with the layer showing (then name of the layer is the name of the
     ARC coverage). 

       Set the Color of the Missing Category

         Right click on the layer button and select Edit...
         Click on the Missing category symbol (a line)
         In the Color box of the GIS Feature Appearance window, click on the right arrow and
           then enter CXBABABA for the Name.
         Click OK to close the GIS Feature Appearance window, and then close the GIS layer window.

       Set the Map Background Color:

         Click on Tools, Map Properties, Colors... to open the Map Styles and Colors window.
         Set the Background color to CXE1E9DA.
         Click on OK and Close and respond Yes to the Save Changes request.

       Create a Legend for the Map:

         Click on View, Legend, New to open the Lengend Options window.
         Click on the layer to activate, deselect Frame, be sure Show Missing Values is set.
         Set the Text Attributes to be Font: Ariel, 12 point, Color: CX336CD7 (blue).
         Click Close and place the legend on the map. 

         If you need to move the location, resize, or edit the legend then right click over the 
         legend and select Move, Resize or Edit.

       Create an Action that allows you to click on a point and have the point displayed in FSView table.

         Click on Action, Define; select SPATIALINFO and click on Remove.
         Select New; click on Type and select View. 
         Click on Save and then Close.

       Make the layer selectable by right-clicking on the layer button above the map and
       selecting Make Layer Selectable

       Save the map by clicking on File, Save, All.

     This completes the required edits.

/* ---------------------------------------------------------------------------------------------------- */

/* Modify these settings */

/* Specify the path of SPARROW GIS results directory */
%let gis_home = d:\greg\sparrow\model\package\sparrow\gis ;

/* Specify the path of the SPARROW results */
%let results_home = d:\greg\sparrow\model\package\sparrow\results ;

/* Specify if the program is being used to update a previously
   created residuals map coverage with new residuals (either updated
   residual values or new station locations). If you have a previously 
   created residuals map, then set the switch to yes, run the program,
   and view the GIS coverages given in the dir_gis directory. To view, 
   use the SAS explorer feature to open the dir_gis directory, open 
   the resids_map or reach_map catalogs, and click on the coverage icons.
   Note that unless you have changed the reach network, a previously
   created reach map is automatically updated with new reach predictions
   without the need to rerun this program. */
%let if_previous = no ;

/* Specify if a residuals map is to be created */
%let if_create_resids_map = yes ;

/* Specify if a reach map is to be created */
%let if_create_reach_map = yes ;

/* Specify the name of the arc coverage used as background for the residuals map 
   COVERAGE MUST BE IN GEOGRAPHIC PROJECTION (LAT LON COORDINATES). ONLY
   NEEDED FOR FIRST-TIME INSTALLATION OF THE COVERAGES. The export file CANNOT
   be in compressed format. */
%let resids_map_background_arc_cover = d:\greg\sparrow\model\package\gis_exports\states2mprjp.e00 ;

/* Specify the name of the arc coverage used for mapping reaches. ONLY
   NEEDED FOR FIRST-TIME INSTALLATION OF THE COVERAGES. The export file
   CANNOT be in compressed format. Note, we have found that export files
   created with Arc 8 do not load propersly into SAS/GIS. The work-around
   we use involves first converting the Arc 8 coverage to a shapefile,
   loading the shapefile into Arc/info 6, and then exporting the coverage
   into an uncompressed export file. */
%let reach_map_arc_coverage = d:\greg\sparrow\model\package\gis_exports\erf1_2_l.e00 ;

/* No modification is generally required below this point. */

/* ---------------------------------------------------------------------------------------------------- */

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

/* ---------------------------------------------------------------------------------------------------- */

/* Macro gets the layer names from a specified coverage */

%macro get_layer_names(cover) ;

  %local cover i_layer ;
  %global layer_names ;

  /* Point to an external file to be created */
  filename test "&gis_home.\mapcont.txt" ;

  /* Redirect the output to a file */
  proc printto print=test new ;
  run ;

  /* Generate the map contents for a map */

  proc gis ;
    map contents &cover ;
  run ;
  quit ;

  /* Redirect the output back to the output window */
  proc printto ;
  run ;

  /* Process the text file to get the names of the layers in the map.
     Each of the layers is listed after the Layer No. information */
  data _null_ ;
    infile test lrecl = 256 truncover end = last ;
    retain count 0 ;
    input txt $1-200 ;
    if substr(txt,1,8) = 'Layer No' then do ;
      count + 1 ;
      do until (scan(txt,1,' ') = "Name:") ;
        input txt $1-200 ;
      end ;
      layer = scan(txt,-1,'.') ;
      /* Create one layer macro variable for each layer */
      call symput('layer' || (trim(left(count))),layer) ;
    end ;
    /* Get the total number of layers */
    if last then call symput('count',trim(left(count))) ;
  run ;

  %let rc = %sysfunc(fdelete(test)) ;

  %let layer_names = ;
  %do i_layer = 1 %to &count ;
    %let layer_names = &layer_names &&&layer&i_layer ;
  %end ;

%mend get_layer_names ;

/* ---------------------------------------------------------------------------------------------------- */

%macro delete_previous(data_list,cat_list) ;

  %if %length(&data_list) > 0 %then %do ;
    proc datasets library = dir_gis nolist ;
      delete &data_list ;
    run ;
  %end ;

  %if %sysfunc(exist(dir_gis.&cat_list,catalog)) %then %do ;
    proc catalog catalog = dir_gis.&cat_list kill ;
    run ;
  %end ;

  data _null_ ;
  run ; 

%mend delete_previous ;

/* ---------------------------------------------------------------------------------------------------- */

%macro create_resids_map ;

  %if %length(&resids_map_background_arc_cover) > 0 %then %do ;

    /* Wipe-out previous files and catalogs, if they exist */

    %delete_previous(resstate resids_backc resids_backd resids_backn,resids_back) ;

    filename arcin "&resids_map_background_arc_cover" recfm = v lrecl = 80 ;

    %let IMP_TYPE = arc ;
    %let MAPLIB = dir_gis ;
    %let MAPCAT = resids_back ;
    %let MAPNAME = resids_back ;
    %let CATHOW = create ;
    %let SPALIB = dir_gis ;
    %let SPANAME = resids_back ;
    %let SPAHOW = create ; 

    DM 'AF C=SASHELP.GISIMP.batch.SCL';

    %get_layer_names(dir_gis.resids_back.resids_back) ;

    %let resids_map_background_layer_name = %scan(&layer_names,1,' ') ;

    proc gis ;
      catalog dir_gis.resids_back ;
      spatial update resids_back / latlon degrees ;
      map update resids_back / latlon degrees ;
      layer update dir_gis.resids_back.&resids_map_background_layer_name / default = (area = (color = CXBDB8B3)) ;
    run ;
    quit ;

    data dir_gis.resids_backc ; set dir_gis.resids_backc ;
      id = 0 ;
    run ;

    proc gis ;
      spatial update dir_gis.resids_back.resids_back ;
      composite create id / var=id ;
    run ;
    quit ;

  %end ;

  /* Create a dummy resids data set (if one does not already exist) */

  %if %sysfunc(exist(dir_rslt.resids)) = 0 %then %do ;

    data _null_ ; set dir_gis.resids_backc (firstobs = 1 obs = 1) ;
      call symput("lon_min",trim(left(xmin))) ;
      call symput("lon_range",trim(left(xmax - xmin))) ;
      call symput("lat_min",trim(left(ymin))) ;
      call symput("lat_range",trim(left(ymax - ymin))) ;
    run ;

    data dir_rslt.resids ;
      do id = 1 to 10 ;
        lon = &lon_min + &lon_range * ranuni(382883) ;
        lat = &lat_min + &lat_range * ranuni(4773740) ;
        map_resid = rannor(4883882) ;
        output ;
      end ;
    run ;

  %end ;

  data mapresids (keep = id lat lon rename = (lat = y lon = x)) ; set dir_rslt.resids ;
  run ;

  /* Wipe out previous resids files and catalogs, if they exist */

  %delete_previous(residsc residsd residsn,resids) ;

  /* Create the resids coverage */

  %let IMP_TYPE = genpoint ;
  %let INFILE = mapresids ;
  %let NIDVARS = 0 ;
  %let MAPLIB = dir_gis ;
  %let MAPCAT = resids ;
  %let MAPNAME = resids ;
  %let CATHOW = create ;
  %let SPALIB = dir_gis ;
  %let SPANAME = resids ;
  %let SPAHOW = create ; 

  DM 'AF C=SASHELP.GISIMP.BATCH.SCL';

  proc gis ;
    spatial update dir_gis.resids.resids ;
    layer update dir_gis.resids.mapresids / default = (point = (size = 4 font = marker character = 'W')) ;
  run ;
  quit ;

  proc gis ;
    spatial update dir_gis.resids.resids / latlon degrees ;
    map update dir_gis.resids.resids / latlon degrees ;
    layer update dir_gis.resids.mapresids / thematic 
      theme = 
        (create
         themevar = map_resid
         dataset = dir_rslt.resids
         datavar = id
         composite = id
         link = mapresids
         range = levels                                               
         nlevels = 4 
         point = (
           (level = 1 size = 8 color = CXE40000 font = marker character = 'D')
           (level = 2 size = 6 color = CXCA8D00 font = marker character = 'D')
           (level = 3 size = 6 color = CX409C6E font = marker character = 'C')
           (level = 4 size = 8 color = CX4580CF font = marker character = 'C')
            )
          ) ;
  run ;
  quit ;

  /* Wipe out the previous resids_map catalog, if it exists */

  %delete_previous(,resids_map) ;

  /* Create the resids_map coverage */

  data dir_gis.resids_map_label ;
    row = 0 ;
    x = -345 ;
    y = 46 ;
    ftype = 'L' ;
    layer = '' ;
    spatial = '' ;
    ltype = 'T' ;
    onscale = 0 ;
    offscale = 0 ;
    text = 'Standardized Residuals Map' ;
    style = 'Arial-21.7pt-Roman-Normal' ;
    color = 'CX336CD7' ;
    flags = 133634 ;
  run ;

  proc gis ;
    spatial create dir_gis.resids_map.resids_map / merge = (dir_gis.resids_back.resids_back,dir_gis.resids.resids) ;
    coverage create dir_gis.resids_map.resids_map / where = '1' ;
    map create dir_gis.resids_map.resids_map / latlon degrees coverage = dir_gis.resids_map.resids_map 
      layers = (dir_gis.resids_back.&resids_map_background_layer_name,dir_gis.resids.mapresids) details layerson = (_all_) 
      label = dir_gis.resids_map_label ;
  run ;
  quit ; 

%mend create_resids_map ;

/* ---------------------------------------------------------------------------------------------------- */

%macro create_reach_map ;

  /* Wipe out previous reach_map files and catalogs, if they exist */

  %delete_previous(reach_mapc reach_mapd reach_mapn,reach_map) ;

  /* Create the reach_map coverage */

  filename arcin "&reach_map_arc_coverage" recfm = v lrecl = 80 ;

  %let IMP_TYPE = arc ;
  %let MAPLIB = dir_gis ;
  %let MAPCAT = reach_map ;
  %let MAPNAME = reach_map ;
  %let CATHOW = create ;
  %let SPALIB = dir_gis ;
  %let SPANAME = reach_map ;
  %let SPAHOW = create ; 

  DM 'AF C=SASHELP.GISIMP.batch.SCL';

  %get_layer_names(dir_gis.reach_map.reach_map) ;

  %let reach_map_layer_name = %scan(&layer_names,1,' ') ;

  proc gis ;
    spatial update dir_gis.reach_map.reach_map ;
    layer update dir_gis.reach_map.&reach_map_layer_name / default = (line = (width = 1 color = cxbababa)) ;
  run ;
  quit ;

  /* Create a dummy reach predict file with some dummy variables */

  %if %sysfunc(exist(dir_rslt.predict)) = 0 %then %do ;
    data dir_rslt.predict ; 
      do arcnum = 1 to min(100,%n_obs(dir_gis.reach_mapc)) ;
        map_del_frac = ranuni(4883882) ;
        output ;
      end ;
    run ;
  %end ;

  proc gis c = dir_gis.reach_map ;
    spatial update dir_gis.reach_map.reach_map ;
    layer update dir_gis.reach_map.&reach_map_layer_name / thematic 
      theme = 
        (create
         themevar = map_del_frac
         dataset = dir_rslt.predict
         datavar = arcnum
         composite = arcnum
         link = predict
         range = levels                                               
         nlevels = 6 
         line = (
           (level = 1 width = 1 color = cxff00ff)
           (level = 2 width = 1 color = cx0000ff)
           (level = 3 width = 1 color = cx008000)
           (level = 4 width = 2 color = cxffff00)
           (level = 5 width = 2 color = cxff7000)
           (level = 6 width = 3 color = cxe10000)
            )
          ) ;
   run ;
   quit ;

%mend create_reach_map ;

/* ---------------------------------------------------------------------------------------------------- */

/* Macro takes previously created coverages, and loads them with results from
   the predict and residuals files in the specified results directory. */

%macro previous ;

  data mapresids (keep = id lat lon rename = (lat = y lon = x)) ; set dir_rslt.resids ;
  run ;

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

  DM 'AF C=SASHELP.GISIMP.BATCH.SCL';

%mend previous ;

/* ---------------------------------------------------------------------------------------------------- */

%macro main ;

  libname dir_gis "&gis_home" ;
  libname dir_rslt "&results_home" ;

  %if %upcase(&if_previous) = YES %then %previous ;
  %else %do ;
    %if %upcase(&if_create_resids_map) = YES %then %create_resids_map ;
    %if %upcase(&if_create_reach_map) = YES %then %create_reach_map ;
  %end ;

%mend main ;

%main ;
