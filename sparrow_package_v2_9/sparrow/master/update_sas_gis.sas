/* Program: update_sas_gis.sas
   Date: 11/18/04
   Purpose: Program updates SAS GIS to allow import of ARC export files
            into SAS version 8.2 and above.

   Please follow
   these steps:

   1. Download the attached cport file to a directory on your computer.

   2. Please rename the existing GISIMP catalog just in case you run into problems
      and need to reuse it.  In a Windows Explorer window, navigate to the directory
      in which SAS is installed.  Expand the GIS folder, then the SASHELP folder. 
      Right-click on the GISIMP.SAS7BCAT file, and select Rename.  Rename the catalog
      to something like GISIMP2.SAS7BCAT. */

/* Specify the path containing the SAS update gisimp_82.cpo */
%let update_path = d:\greg\sparrow\model\package\sparrow\master ;

/* ------------------------------------ */
/* No editing required below this point */

proc cimport lib = sashelp file = "&update_path.\gisimp_82.cpo" ;
run ; quit ;
