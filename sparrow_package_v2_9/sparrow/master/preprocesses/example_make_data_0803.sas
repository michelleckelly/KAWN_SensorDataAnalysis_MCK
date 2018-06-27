*********************************************************************;
*                                                                   *;
* Program:  make_data_0803.sas                                      *;
* Function: Combines source data with reach attributes for use in   *;
*           calibration and application runs.                       *;
* SPARROW version 2.1                                               *;
*                                                                   *;
*********************************************************************;
LIBNAME DIR 'D:\sparrow_new\software\data' ; 
LIBNAME DIR1 'D:\sparrow_new\software_old\data' ; 
LIBNAME DIR2 'D:\model_data\watershed';
LIBNAME DIR6 'D:\e2rf1';
LIBNAME DIR7 'D:\model_data\BETH\DATA';
LIBNAME DIR8 'D:\model_data\GREG\point';
*********************************************************************;
* Created    : R. Alexander                                         *;
* Date       : 08/14/03                                             *;
* Update     : 11/24/03                                             *;
*********************************************************************;

DATA DATATEMP1;
MERGE DIR2.ATTS_WSHED (IN=A)
      DIR2.NID_SUMS
      DIR2.E2RF1HUC
      DIR2.POP_WSHED_SUMS
      DIR2.ATMDEP_AVGS 
      DIR2.AGR_WSHED_SUMS 
      DIR2.AGCEN_WSHED_SUMS 
      DIR2.AGCEN_CROPS_WSHED_SUMS
	  DIR2.CTILL_WSHED_SUMS
      DIR2.CTILES_WSHED_SUMS
      DIR2.LU_WSHED_SUMS
      DIR2.STATSGO_AVGS
      DIR2.TOPMODEL_AVGS
      DIR2.WTEMP_REACH
      DIR2.WTRAGR87
      DIR2.WTRDEP87 
      DIR2.WTRPNT87
	  DIR1.PREDICT_TN_WRR97
      DIR6.NUTCRIT             /* update */

      DIR7.b_statstgo_watershed_avgs
      DIR7.b_daymet_watershed_avgs 
      DIR7.b_terrain_watershed_avgs 
	  DIR7.b_rf1_watershed_stats

      DIR8.point_source_reach_flow
      DIR8.point_source_reach_nitro
      DIR8.point_source_reach_phos
      DIR8.point_source_reach_bod5

      ; BY E2RF1;

	  IF A;
      WATERID = E2RF1;

/* compute additional reach properties */
	  RCHSLOPE = (max_t_elev5rf1 - min_t_elev5rf1) / RCHLEN;      /* RF1 slope from 1-km DEM cells */
	  RCHSLOPEF = (max_t_felev5rf1 - min_t_felev5rf1) / RCHLEN;   /* RF1 slope from filled 1-km DEM cells */

	  RDEPTH = 0.2612*(MEANQ/35.288)**0.3966;  /* estimated depth in meters from streamflow */
	  RFRICTION = .;
       IF RDEPTH GT 0 AND RCHSLOPE GE 0 AND MEANV GT 0 THEN DO;
          RFRICTION = 8.0 * 9.80665 * RDEPTH * RCHSLOPE / (MEANV*0.3048)**2 ; /* friction factor (g accel=9.80665 m/s2) */
       END;

	  DRAINDEN = .;
      IF DEMIAREA GT 0 THEN DRAINDEN = (RCHLEN/1000) / DEMIAREA;

/* NID reservoir statistics */
      if stortot = . then stortot = 0;
      if storless = . then storless = 0;

/* convert to mass - kg   */
      NO3DEP = NO3DEP * LU00;
      NH4DEP = NH4DEP * LU00;

/* Set longitude for plotting in SAS GIS  */
     LON = LON * -1.0;

/* compute average areal organic matter content (convert g/cm3-in-%weight to kg/km2) - Wolock 1-km data */
     OMA = (BDL+BDH)/2.0 * ROCKDEPAVE*2.54 * omave/100 * 1.0E+07 * awcave ;
     OMAUP = (BDL+BDH)/2.0 * ROCKDEPAVE*2.54 * ((OMLUP+OMHUP)/2.0)/100 * 1.0E+07 * awcave ;
/* organic matter mass for full soil column (approx 50 in.) (convert kg/km2/yr to kg/yr * e+10 */
	 OMM = OMA * LU00 / 10000000000. ;
	 OMMUP = OMAUP * LU00 / 10000000000. ;


/* compute average areal organic matter content (convert g/cm3-in-%weight to kg/km2) - Univ PA 1-km data */
     OMA150 = s_bulkd150 * s_rockdep * omave/100 * 1.0E+07 * s_awc150/100.;
     OMA100 = s_bulkd100 * s_rockdep * omave/100 * 1.0E+07 * s_awc100/100.;
     OMA030 = s_bulkd030 * s_rockdep * ((OMLUP+OMHUP)/2.0)/100 * 1.0E+07 * s_awc100/100.;
/* organic matter mass for full soil column (approx 50 in.) (convert kg/km2/yr to kg/yr * e+10 */
	 OMM150 = OMA150 * LU00 / 10000000000. ;
	 OMM100 = OMA100 * LU00 / 10000000000. ;
	 OMM030 = OMA030 * LU00 / 10000000000. ;

/* set missing for small watersheds to zero  */

     if perdun = . then perdun = 0;
     if perhor = . then perhor = 0;

     if OMA = . then OMA = 0;
     if OMAUP = . then OMAUP = 0; 
	 if OMM = . then OMM = 0;
	 if OMMUP = . then OMMUP = 0;
     if OMA150 = . then OMA150 = 0;
     if OMA100 = . then OMA100 = 0;
     if OMA030 = . then OMA030 = 0;
     if OMM150 = . then OMM150 = 0;
     if OMM100 = . then OMM100 = 0;
     if OMM030 = . then OMM030 = 0;

     IF WFERT_N = . THEN WFERT_N = 0;
     IF WFERT_P = . THEN WFERT_P = 0;
     IF WFFERT_N = . THEN WFFERT_N = 0;
     IF WFFERT_P = . THEN WFFERT_P = 0;
     IF WNFFERT_N = . THEN WNFFERT_N = 0;
     IF WNFFERT_P = . THEN WNFFERT_P = 0;
     IF WFERTAVG_N = . THEN WFERTAVG_N = 0;
     IF WFERTAVG_P = . THEN WFERTAVG_P = 0;

     IF WNTOTAL = . THEN WNTOTAL = 0;
     IF WPTOTAL = . THEN WPTOTAL = 0;
     IF WNE_U = . THEN WNE_U = 0;
     IF WPE_U = . THEN WPE_U = 0;
     IF WNE_C = . THEN WNE_C = 0;
     IF WPE_C = . THEN WPE_C = 0;
     IF WRN_C = . THEN WRN_C = 0;
     IF WRP_C = . THEN WRP_C = 0;

     IF WNTOTAL_A = . THEN WNTOTAL_A = 0;
     IF WPTOTAL_A = . THEN WPTOTAL_A = 0;
     IF WNE_U_A  = . THEN WNE_U_A  = 0;
     IF WPE_U_A  = . THEN WPE_U_A  = 0;
     IF WNE_C_A  = . THEN WNE_C_A  = 0;
     IF WPE_C_A  = . THEN WPE_C_A  = 0;
     IF WRN_C_A  = . THEN WRN_C_A  = 0;
     IF WRP_C_A  = . THEN WRP_C_A  = 0;

     IF WAGCROPS = . THEN WAGCROPS = 0;
     IF WAGPAST = . THEN WAGPAST = 0;
     IF WAGCROPS_NLCD = . THEN WAGCROPS_NLCD = 0;
     IF WAGPAST_NLCD = . THEN WAGPAST_NLCD = 0;
     IF WTCRP_WO = . THEN WTCRP_WO = 0;
     IF WTCRP_WO_NLCD = . THEN WTCRP_WO_NLCD = 0;
     IF WALL_PAST_WO = . THEN WALL_PAST_WO = 0;
     IF WALL_PAST_WO_NLCD = . THEN WALL_PAST_WO_NLCD = 0;

     IF WTCRP = . THEN WTCRP = 0;
     IF WHARV_CRP = . THEN WHARV_CRP = 0;
     IF WCRP_PASTGRZ = . THEN WCRP_PASTGRZ = 0;
     IF WOTHR_CRPLND = . THEN WOTHR_CRPLND = 0;
     IF WALL_PAST = . THEN WALL_PAST = 0;
     IF WPAST_RANG = . THEN WPAST_RANG = 0;
     IF WWDLND_PAST = . THEN WWDLND_PAST = 0;
     IF WTCRP_NLCD = . THEN WTCRP_NLCD = 0;
     IF WHARV_CRP_NLCD = . THEN WHARV_CRP_NLCD = 0;
     IF WCRP_PASTGRZ_NLCD = . THEN WCRP_PASTGRZ_NLCD = 0;
     IF WOTHR_CRPLND_NLCD = . THEN WOTHR_CRPLND_NLCD = 0;
     IF WALL_PAST_NLCD = . THEN WALL_PAST_NLCD = 0;
     IF WPAST_RANG_NLCD = . THEN WPAST_RANG_NLCD = 0;
     IF WWDLND_PAST_NLCD = . THEN WWDLND_PAST_NLCD = 0;

     IF NO3DEP = . THEN NO3DEP = 0;
     IF NH4DEP = . THEN NH4DEP = 0;

     IF RUNOFF = . THEN RUNOFF = 0;
     IF WTEMP = . THEN WTEMP = 0;

     IF TOTPOP = . THEN TOTPOP = 0;
     IF PSEWER = . THEN PSEWER = 0;
     IF PSEPTIC = . THEN PSEPTIC = 0;
     IF POTHER = . THEN POTHER = 0;
     IF PTOT = . THEN PTOT = 0;
     IF PPSEWER = . THEN PPSEWER = 0;
     IF PPSEPTIC = . THEN PPSEPTIC = 0;
     IF PPOTHER = . THEN PPOTHER = 0;

     IF LU00 = . THEN LU00 = 0;
     IF LU11 = . THEN LU11 = 0;
     IF LU12 = . THEN LU12 = 0;
     IF LU21 = . THEN LU21 = 0;
     IF LU22 = . THEN LU22 = 0;
     IF LU23 = . THEN LU23 = 0;
     IF LU31 = . THEN LU31 = 0;
     IF LU32 = . THEN LU32 = 0;
     IF LU33 = . THEN LU33 = 0;
     IF LU41 = . THEN LU41 = 0;
     IF LU42 = . THEN LU42 = 0;
     IF LU43 = . THEN LU43 = 0;
     IF LU51 = . THEN LU51 = 0;
     IF LU61 = . THEN LU61 = 0;
     IF LU71 = . THEN LU71 = 0;
     IF LU81 = . THEN LU81 = 0;
     IF LU82 = . THEN LU82 = 0;
     IF LU83 = . THEN LU83 = 0;
     IF LU84 = . THEN LU84 = 0;
     IF LU85 = . THEN LU85 = 0;
     IF LU91 = . THEN LU91 = 0;
     IF LU92 = . THEN LU92 = 0;

     IF WFERT_N87 = . THEN WFERT_N87 = 0;
     IF WFERT_P87 = . THEN WFERT_P87 = 0;
     IF WNTOTAL87 = . THEN WNTOTAL87 = 0;
     IF WPTOTAL87 = . THEN WPTOTAL87 = 0;
     IF WNFERT87 = . THEN WNFERT87 = 0;
     IF NO3DEP87 = . THEN NO3DEP87 = 0;
     IF WTKN = . THEN WTKN = 0;
     IF WBOD5 = . THEN WBOD5 = 0;
     IF WTOTP = . THEN WTOTP = 0;
     IF WTSS = . THEN WTSS = 0;

     IF WCT_MT = . THEN WCT_MT = 0;
     IF WCT_NT = . THEN WCT_NT = 0;
     IF WCT_RT = . THEN WCT_RT = 0;
     IF WCT_R5 = . THEN WCT_R5 = 0;
     IF WCT_RL = . THEN WCT_RL = 0;
     IF WCT_ALL = . THEN WCT_ALL = 0;
     IF WCT_MT_NLCD = . THEN WCT_MT_NLCD = 0;
     IF WCT_NT_NLCD = . THEN WCT_NT_NLCD = 0;
     IF WCT_RT_NLCD = . THEN WCT_RT_NLCD = 0;
     IF WCT_R5_NLCD = . THEN WCT_R5_NLCD = 0;
     IF WCT_RL_NLCD = . THEN WCT_RL_NLCD = 0;
     IF WCT_ALL_NLCD = . THEN WCT_ALL_NLCD = 0;

     IF WDRAINSKM2_NLCD = . THEN WDRAINSKM2_NLCD = 0;
     IF WDRAINSKM2 = . THEN WDRAINSKM2 = 0;

     IF WHEAT_NLCD = . THEN WHEAT_NLCD = 0;
     IF VEG_NLCD = . THEN VEG_NLCD = 0;
     IF TOBACCO_NLCD = . THEN TOBACCO_NLCD = 0;
     IF SUNFLWRS_NLCD = . THEN SUNFLWRS_NLCD = 0;
     IF SOYBEANS_NLCD = . THEN SOYBEANS_NLCD = 0;
     IF SORGHUM_NLCD = . THEN SORGHUM_NLCD = 0;
     IF SGRBTS_NLCD = . THEN SGRBTS_NLCD = 0;
     IF RICE_NLCD = . THEN RICE_NLCD = 0;
     IF POTATOES_NLCD = . THEN POTATOES_NLCD = 0;
     IF PEANUTS_NLCD = . THEN PEANUTS_NLCD = 0;
     IF FLDGRS_NLCD = . THEN FLDGRS_NLCD = 0;
     IF BEANS_PEAS_NLCD = . THEN BEANS_PEAS_NLCD = 0;
     IF COTTON_NLCD = . THEN COTTON_NLCD = 0;
     IF CORN_NLCD = . THEN CORN_NLCD = 0;
     IF ALF_NLCD = . THEN ALF_NLCD = 0;
     IF ROWCROP_SUM_NLCD = . THEN ROWCROP_SUM_NLCD = 0;
     IF WWHEAT = . THEN WWHEAT = 0;
     IF WVEG = . THEN WVEG = 0;
     IF WTOBACCO = . THEN WTOBACCO = 0;
     IF WSUNFLWRS = . THEN WSUNFLWRS = 0;
     IF WSOYBEANS = . THEN WSOYBEANS = 0;
     IF WSORGHUM = . THEN WSORGHUM = 0;
     IF WSGRBTS = . THEN WSGRBTS = 0;
     IF WRICE = . THEN WRICE = 0;
     IF WPOTATOES = . THEN WPOTATOES = 0;
     IF WPEANUTS = . THEN WPEANUTS = 0;
     IF WFLDGRS = . THEN WFLDGRS = 0;
     IF WBEANS_PEAS = . THEN WBEANS_PEAS = 0;
     IF WCOTTON = . THEN WCOTTON = 0;
     IF WCORN = . THEN WCORN = 0;
     IF WALF = . THEN WALF = 0;
     IF WROWCROP_SUM = . THEN WROWCROP_SUM = 0;

/* EPA point source load estimates */
     IF FLOW_ON_OBS = . THEN FLOW_ON_OBS = 0;
     IF FLOW_TO_OBS = . THEN FLOW_TO_OBS = 0;
     IF FLOW_NEAR_OBS = . THEN FLOW_NEAR_OBS = 0;
     IF FLOW_ON_EST = . THEN FLOW_ON_EST = 0;
     IF FLOW_TO_EST = . THEN FLOW_TO_EST = 0;
     IF FLOW_NEAR_EST = . THEN FLOW_NEAR_EST = 0;

     IF NITRO_ON_OBS = . THEN NITRO_ON_OBS = 0;
     IF NITRO_TO_OBS = . THEN NITRO_TO_OBS = 0;
     IF NITRO_NEAR_OBS = . THEN NITRO_NEAR_OBS = 0;
     IF NITRO_ON_EST = . THEN NITRO_ON_EST = 0;
     IF NITRO_TO_EST = . THEN NITRO_TO_EST = 0;
     IF NITRO_NEAR_EST = . THEN NITRO_NEAR_EST = 0;

     IF PHOS_ON_OBS = . THEN PHOS_ON_OBS = 0;
     IF PHOS_TO_OBS = . THEN PHOS_TO_OBS = 0;
     IF PHOS_NEAR_OBS = . THEN PHOS_NEAR_OBS = 0;
     IF PHOS_ON_EST = . THEN PHOS_ON_EST = 0;
     IF PHOS_TO_EST = . THEN PHOS_TO_EST = 0;
     IF PHOS_NEAR_EST = . THEN PHOS_NEAR_EST = 0;

     IF BOD5_ON_OBS = . THEN BOD5_ON_OBS = 0;
     IF BOD5_TO_OBS = . THEN BOD5_TO_OBS = 0;
     IF BOD5_NEAR_OBS = . THEN BOD5_NEAR_OBS = 0;
     IF BOD5_ON_EST = . THEN BOD5_ON_EST = 0;
     IF BOD5_TO_EST = . THEN BOD5_TO_EST = 0;
     IF BOD5_NEAR_EST = . THEN BOD5_NEAR_EST = 0;
 RUN;

 PROC SORT DATA=DATATEMP1 NODUPKEY; BY E2RF1;  /* duplicate reach records appear to have occurred */
 RUN;

/* fill-in missing land-to-water characteristics (~1400 reaches including ~30 monitoring sites) */

    proc sort data=DATATEMP1; by huc8;

    proc means data = DATATEMP1 noprint ;
     var perdun perhor permave sandave SLOPEL SLOPEH awcave omave ROCKDEPAVE BDL BDH 
     OMA OMAUP OMM OMMUP OMLUP OMHUP 
     OMA150 OMA100 OMA030 OMM150 OMM100 OMM030
     WTDEPL WTDEPH     
     s_hsga s_hsgb s_hsgc s_hsgd s_awc100 s_awc150 s_awc250
     s_perm030 s_perm100 s_perm150 s_poros030 s_poros100 s_poros150 
	 s_ph030 s_ph100 s_ph150 s_sand030 s_sand100 s_sand150
	 s_silt030 s_silt100 s_silt150 s_clay030 s_clay100 s_clay150
	 s_bulkd030 s_bulkd100 s_bulkd150 s_rockf030 s_rockf100 s_rockf150
	 s_kfact s_kffact s_rockdep
     d_airt d_airtvar d_airtmx d_dfrost d_dgrow d_ppt1k d_pptfreq d_pptsize
     d_rad d_tq1 d_tq2 d_tq3 d_tq4
     d_pq1 d_pq2 d_pq3 d_pq4
	 t_slod8 t_slodinf t_ln_sca t_ln_dist t_racc t_ln_ratanb
	 ;
     by huc8 ;
     output out = HUCOUT mean = 
     mperdun mperhor mpermave msandave mSLOPEL mSLOPEH mawcave momave mROCKDEPAVE mBDL mBDH 
     mOMA mOMAUP mOMM mOMMUP mOMLUP mOMHUP 
     MOMA150 MOMA100 MOMA030 MOMM150 MOMM100 MOMM030
     mWTDEPL mWTDEPH     
     ms_hsga ms_hsgb ms_hsgc ms_hsgd ms_awc100 ms_awc150 ms_awc250
     ms_perm030 ms_perm100 ms_perm150 ms_poros030 ms_poros100 ms_poros150 
	 ms_ph030 ms_ph100 ms_ph150 ms_sand030 ms_sand100 ms_sand150
	 ms_silt030 ms_silt100 ms_silt150 ms_clay030 ms_clay100 ms_clay150
	 ms_bulkd030 ms_bulkd100 ms_bulkd150 ms_rockf030 ms_rockf100 ms_rockf150
	 ms_kfact ms_kffact ms_rockdep
     md_airt md_airtvar md_airtmx md_dfrost md_dgrow md_ppt1k md_pptfreq md_pptsize
     md_rad md_tq1 md_tq2 md_tq3 md_tq4
     md_pq1 md_pq2 md_pq3 md_pq4
	 mt_slod8 mt_slodinf mt_ln_sca mt_ln_dist mt_racc mt_ln_ratanb;
    run ;

 /* obtain drainage density excluding coastal shoreline segments */
	DATA DATATEMP2; SET DATATEMP1;
	 IF E2RF1 LT 80000;
	 KEEP E2RF1 HUC8 DRAINDEN;
    RUN;

    proc means data = DATATEMP2 noprint ;
     var DRAINDEN;
     by huc8 ;
     output out = HUCOUT2 mean = MDRAINDEN;
    run ;

 /* create final DATA1 file */
 DATA DIR.DATA1; MERGE DATATEMP1 HUCOUT HUCOUT2; BY HUC8;
  if DRAINDEN = . then DRAINDEN = MDRAINDEN;
  if perdun = . then perdun = mperdun;
  if perhor = . then perhor = mperhor;
  if permave = . then permave = mpermave;
  if sandave = . then sandave = msandave;
  if SLOPEL = . then slopel = mslopel;
  if SLOPEH = . then slopeh = mslopeh;
  if awcave = . then awcave = mawcave;
  if omave = . then omave = momave;
  if ROCKDEPAVE = . then rockdepave = mrockdepave;
  if BDL = . then bdl = mbdl;
  if BDH = . then bdh = mbdh;
  if OMA = . then oma = moma;
  if OMAUP = . then omaup = momaup;
  if OMM = . then omm = momm;
  if OMMUP = . then ommup = mommup;
  if OMLUP = . then omlup = momlup;
  if OMHUP = . then omhup = momhup;
  if OMA150 = . then OMA150 = mOMA150;
  if OMA100 = . then OMA100 = mOMA100;
  if OMA030 = . then OMA030 = mOMA030;
  if OMM150 = . then OMM150 = mOMM150;
  if OMM100 = . then OMM100 = mOMM100;
  if OMM030 = . then OMM030 = mOMM030;
  if WTDEPL = . then wtdepl = mwtdepl;
  if WTDEPH = . then wtdeph = mwtdeph;
  if s_hsga = . then s_hsga = ms_hsga;
  if s_hsgb = . then s_hsgb = ms_hsgb;
  if s_hsgc = . then s_hsgc = ms_hsgc;
  if s_hsgd = . then s_hsgd = ms_hsgd;
  if s_awc100 = . then s_awc100 = ms_awc100;
  if s_awc150 = . then s_awc150 = ms_awc150;
  if s_awc250 = . then s_awc250 = ms_awc250;
  if s_perm030 = . then s_perm030 = ms_perm030;
  if s_perm100 = . then s_perm100 = ms_perm100;
  if s_perm150 = . then s_perm150 = ms_perm150;
  if s_poros030 = . then s_poros030 = ms_poros030;
  if s_poros100 = . then s_poros100 = ms_poros100;
  if s_poros150 = . then s_poros150 = ms_poros150;
  if s_ph030 = . then s_ph030 = ms_ph030;
  if s_ph100 = . then s_ph100 = ms_ph100;
  if s_ph150 = . then s_ph150 = ms_ph150;
  if s_sand030 = . then s_sand030 = ms_sand030;
  if s_sand100 = . then s_sand100 = ms_sand100;
  if s_sand150 = . then s_sand150 = ms_sand150;
  if s_silt030 = . then s_silt030 = ms_silt030;
  if s_silt100 = . then s_silt100 = ms_silt100;
  if s_silt150 = . then s_silt150 = ms_silt150;
  if s_clay030 = . then s_clay030 = ms_clay030;
  if s_clay100 = . then s_clay100 = ms_clay100;
  if s_clay150 = . then s_clay150 = ms_clay150;
  if s_bulkd030 = . then s_bulkd030 = ms_bulkd030;
  if s_bulkd100 = . then s_bulkd100 = ms_bulkd100;
  if s_bulkd150 = . then s_bulkd150 = ms_bulkd150;
  if s_rockf030 = . then s_rockf030 = ms_rockf030;
  if s_rockf100 = . then s_rockf100 = ms_rockf100;
  if s_rockf150 = . then s_rockf150 = ms_rockf150;
  if s_kfact = . then s_kfact = ms_kfact;
  if s_kffact = . then s_kffact = ms_kffact;
  if s_rockdep = . then s_rockdep = ms_rockdep;
  if d_airt = . then d_airt = md_airt;
  if d_airtvar = . then d_airtvar = md_airtvar;
  if d_airtmx = . then d_airtmx = md_airtmx;
  if d_dfrost = . then d_dfrost = md_dfrost;
  if d_dgrow = . then d_dgrow = md_dgrow;
  if d_ppt1k = . then d_ppt1k = md_ppt1k;
  if d_pptfreq = . then d_pptfreq = md_pptfreq;
  if d_pptsize = . then d_pptsize = md_pptsize;
  if d_rad = . then d_rad = md_rad;
  if d_tq1 = . then d_tq1 = md_tq1;
  if d_tq2 = . then d_tq2 = md_tq2;
  if d_tq3 = . then d_tq3 = md_tq3;
  if d_tq4 = . then d_tq4 = md_tq4;
  if d_pq1 = . then d_pq1 = md_pq1;
  if d_pq2 = . then d_pq2 = md_pq2;
  if d_pq3 = . then d_pq3 = md_pq3;
  if d_pq4 = . then d_pq4 = md_pq4;
  if t_slod8 = . then t_slod8 = mt_slod8;
  if t_slodinf = . then t_slodinf = mt_slodinf;
  if t_ln_sca = . then t_ln_sca = mt_ln_sca;
  if t_ln_dist = . then t_ln_dist = mt_ln_dist;
  if t_racc = . then t_racc = mt_racc;
  if t_ln_ratanb = . then t_ln_ratanb = mt_ln_ratanb;

  LABEL
/* reach and catchment attributes */
  E2RF1 = 'REACH ID'
  ERF1 = 'REACH ID (VERSION 1.2)'
  ARCNUM = 'GIS COVER#'
  ARCID = 'GIS COVER-ID'
  FNODE = 'FROM-NODE'
  TNODE = 'TO-NODE'
  RR = 'RIVER REACH ID (11-DIGIT)'
  HUC = 'HUC (8-DIGIT) - DIGITAL OVERLAY'
  MEANQ = 'MEAN STREAMFLOW (CFS)'
  MEANV = 'MEAN WATER VELOCITY (FT/S)'
  RFRICTION = 'REACH FRICTION FACTOR'
  RCHSLOPE = 'REACH SLOPE'
  RCHSLOPEF = 'REACH SLOPE - Filled DEM'
  RDEPTH = 'EST. REACH DEPTH (METERS)'
  FRAC = 'FRACTIONAL DIVERSION'
  RCHTOT = 'REACH TIME-OF-TRAVEL (DAYS)'
  RCHLEN = 'REACH LENGTH (METERS)'
  DEMIAREA = 'INCREMENTAL DRAINAGE AREA (KM2)'
  DEMTAREA = 'TOTAL DRAINAGE AREA (KM2)'
  HYDSEQ = 'HYDROLOGIC ORDERING NUMBER'
  RCHTYPE = 'REACH TYPE CODE'
  HEADFLAG = 'HEADWATER FLAG (0/1)'
  TERMFLAG = 'TERMINAL REACH FLAG (1=estu,2=int,3=coast)'
  PNAME = 'RF1 REACH NAME'
  CONTFLAG = 'CONTINENTAL FLAG (1=ESTUARINE WATERS)'
  RESCODE = 'RESV CODE (1,2=INT; 3,4=EXT)'
  PNMCD = 'RESERVOIR NAME CODE'
  RESAREA = 'RESERVOIR AREA (KM2)'
  GISAREA = 'RESERVOIR AREA (KM2)-RF1COV'
  RESCAP = 'RESERVOIR NORM CAP (AC-FT)'
  QOUT = 'RESERVOIR OUTFLOW (CFS)'
  PREFGIS = 'AREA PREFERENCE (1=RF1COV)'
  RESLEN = 'REACH LENGTH (METERS)'
  station_id = 'USGS Monitoring Station Identifier'
  station_name = 'USGS Monitoring Station Name'
  NUTREG = 'NUTRIENT CRITERIA REGION NUMBER'
  NUTNAME = 'NUTRIENT CRITERIA REGION NAME'

  stortot = 'NID RESERVOIR STORAGE (AC-FT)'
  storless = 'NID RESERVOIR STORAGE (AC-FT;<5000AC-FT)'

/* station attributes */
  STAID = 'STATION ID - SHORT'
  DAREAKM2 = 'STATION DRAINAGE AREA (KM2)'
  DIFAREA = 'AREA DIFFERENCE - % (DEM - STATION AREA)' 

/* reservoir areal water load */
  HLOAD = 'AREAL HYDRAULIC LOAD (M/YR)'
  RHLOAD = 'RECIPROCOL HYDRAULIC LOAD (YR/M)'

/* foreign drainage estimates */
  FADRAIN = 'FOREIGN DRAINAGE AREA (KM2)'
  FADRAIN_PERCENT = 'FOREIGN DRAINAGE PERCENT AREA'

/* 1987 station load estimates */
  NLD87 = 'TN LOAD 1987 (KG/YR)'
  PLD87 = 'TP LOAD 1987 (KG/YR)'
  TNCONC = 'TN CONC - 1987 SPARROW MODEL (MG/L)'

/* simple-average load estimates */
  ML600 = 'TN MEAN (KG/YR)'
  ML665 = 'TP MEAN (KG/YR)'
  MLSED = 'SEDIMENT MEAN (KG/YR)'
  MLCOLI = 'COLIFORM MEAN (COL/SEC)'
  FLAGTN = 'SIMPLE MEAN SUFFICIENT DATA'
  FLAGTP = 'SIMPLE MEAN SUFFICIENT DATA'
  FLAGSED = 'SIMPLE MEAN SUFFICIENT DATA'
  FLAGCOLI = 'SIMPLE MEAN SUFFICIENT DATA'

/* 1992-based load estimates */
  LOAD_A_60000 = 'Av. LOAD (A) for Total Nitrogen (kg/yr)'
  LOAD_A_66500 = 'Av. LOAD (A) for Total Phosphorus (kg/yr)'
  LOAD_A_31625 = 'Av. LOAD (A) for Fecal Coliform (100 Bcol/yr)'
  LOAD_A_00680 = 'Av. LOAD (A) for Total Organic Carbon (kg/yr)'
  LOAD_A_00681 = 'Av. LOAD (A) for Dissolved Organic Carbon (kg/yr)'
  SE_60000 = 'TN STANDARD ERROR (% MEAN)'
  SE_66500 = 'TP STANDARD ERROR (% MEAN)'
  SE_31625 = 'COLIFORM STANDARD ERROR (% MEAN)'
  SE_00680 = 'TOC STANDARD ERROR (% MEAN)'
  SE_00681 = 'DOC STANDARD ERROR (% MEAN)'

/* 1992-based NADP wet-deposition estimates */
   NO3DEP = 'NITRATE DEPOSITION (KG/YR)'
   NH4DEP = 'AMMONIA DEPOSITION (KG/YR)'

/* stream properties */
   RUNOFF = 'RUNOFF (CM/YR)'
   WTEMP = 'WATER TEMPERATURE (DEG C.)'
   DRAINDEN = 'RF1 DRAINAGE DENSITY (KM/KM2)'

/* NLCD land-use */
   LU00 = '00 - TOTAL LU AREA (KM2)'
   LU11 = '11 - OPEN WATER (KM2)'
   LU12 = '12 - PERENNIAL ICE-SNOW (KM2)'
   LU21 = '21 - LOW INTENSITY RESIDENTIAL (KM2)'
   LU22 = '22 - HIGH INTENSITY RESIDENTIAL (KM2)'
   LU23 = '23 - COMMERCIAL-INDUSTRIAL-TRANSP (KM2)'
   LU31 = '31 - BARE ROCK-SAND-CLAY (KM2)'
   LU32 = '32 - QUARRIES-STRIP MINES-GRAVEL PITS (KM2)'
   LU33 = '33 - TRANSITIONAL (KM2)'
   LU41 = '41 - DECIDUOUS FOREST (KM2)'
   LU42 = '42 - EVERGREEN FOREST (KM2)'
   LU43 = '43 - MIXED FOREST (KM2)'
   LU51 = '51 - SHRUBLAND (KM2)'
   LU61 = '61 - ORCHARDS-VINEYARDS-OTHER (KM2)'
   LU71 = '71 - GRASSLANDS-HERBACEOUS (KM2)'
   LU81 = '81 - PASTURE-HAY (KM2)'
   LU82 = '82 - ROW CROPS (KM2)'
   LU83 = '83 - SMALL GRAINS (KM2)'
   LU84 = '84 - FALLOW (KM2)'
   LU85 = '85 - URBAN-RESIDENTIAL GRASSES (KM2)'
   LU91 = '91 - WOODY WETLANDS (KM2)'
   LU92 = '92 - EMERGENT HERBACEOUS WETLANDS (KM2)'

/* Fertilizer - NLCD-area and total landarea-based methods */
   WFFERT_N = 'N FARM FERTILIZER (KG)'
   WFFERT_P = 'P FARM FERTILIZER (KG)'
   WNFFERT_N = 'N NON-FARM FERTILIZER (KG)'
   WNFFERT_P = 'P NON-FARM FERTILIZER (KG)'

   WFERT_N = 'N TOTAL FERTILIZER - LANDAREA BASED (KG)'
   WFERT_P = 'P TOTAL FERTILIZER - LANDAREA BASED (KG)'
   WFERTAVG_N = 'N FERTILIZER - 1990-94 AVG - AREA BASED (KG)'
   WFERTAVG_P = 'P FERTILIZER - 1990-94 AVG - AREA BASED (KG)'

/* USGS livestock wastes - NLCD-area and total landarea-based methods */
   WNTOTAL = 'N MANURE EXCRETED - USGS (KG)'
   WPTOTAL = 'P MANURE EXCRETED - USGS (KG)'
   WNTOTAL_A = 'N MANURE EXCRETED - USGS - LANDAREA BASED (KG)'
   WPTOTAL_A = 'P MANURE EXCRETED - USGS - LANDAREA BASED (KG)'

/* ERS livestock wastes - NLCD-area and total landarea-based methods*/
   WNE_U = 'N EXCRETED - UNCONFINED (KG)'
   WNE_C = 'N EXCRETED - CONFINED (KG)'
   WNE_U_A = 'N EXCRETED - UNCONFINED - LANDAREA BASED (KG)'
   WNE_C_A = 'N EXCRETED - CONFINED - LANDAREA BASED (KG)'
   WPE_U = 'P EXCRETED - UNCONFINED (KG)'
   WPE_C = 'P EXCRETED - CONFINED (KG)'
   WPE_U_A = 'P EXCRETED - UNCONFINED - LANDAREA BASED (KG)'
   WPE_C_A = 'P EXCRETED - CONFINED - LANDAREA BASED (KG)'
   WRN_C = 'N RECOVERABLE APPLIED - CONFINED (KG)'
   WRP_C = 'P RECOVERABLE APPLIED - CONFINED (KG)'
   WRN_C_A = 'N RECOVERABLE APPLIED - CONFINED - LANDAREA BASED (KG)'
   WRP_C_A = 'P RECOVERABLE APPLIED - CONFINED - LANDAREA BASED (KG)'

/* Agr Census - derived items - NLCD-area and total landarea-based methods */
   WAGCROPS = 'AG CENSUS ROW CROP AREA - LANDAREA METHOD (KM2)'
   WAGPAST = 'AG CENSUS PASTURE-HAY AREA - LANDAREA METHOD (KM2)'
   WAGCROPS_NLCD = 'AG CENSUS ROW CROP AREA - NLCD METHOD (KM2)'
   WAGPAST_NLCD = 'AG CENSUS PASTURE-HAY AREA - NLCD METHOD (KM2)'
   WTCRP_WO = 'CENSUS TOTAL CROPLAND W/O PASTURE (KM2)'
   WTCRP_WO_NLCD = 'CENSUS TOTAL CROPLAND W/O PASTURE (KM2) - NLCD METHOD'
   WALL_PAST_WO = 'CENSUS ALL PASTURELAND W/O CROP PASTURELAND (KM2)'
   WALL_PAST_WO_NLCD = 'CENSUS ALL PASTURELAND W/O CROP PASTURELAND (KM2)-NLCD METHOD'

/* Agr Census - non-derived items - NLCD-area and total landarea-based methods */
   WTCRP = 'CENSUS TOTAL CROPLAND (KM2)'
   WHARV_CRP = 'CENSUS HARVESTED CROPLAND (KM2)'
   WCRP_PASTGRZ = 'CENSUS CROPLAND USED PAST/GRAZING (KM2)'
   WOTHR_CRPLND = 'CENSUS OTHER CROPLAND (KM2)'
   WALL_PAST = 'CENSUS ALL PASTURELAND (KM2)'
   WPAST_RANG = 'CENSUS PASTURE/RANGE LANDS (KM2)'
   WWDLND_PAST = 'CENSUS WOODLAND/PASTURE (KM2)'
   WTCRP_NLCD = 'CENSUS TOTAL CROPLAND (KM2) - NLCD METHOD'
   WHARV_CRP_NLCD = 'CENSUS HARVESTED CROPLAND (KM2) - NLCD METHOD'
   WCRP_PASTGRZ_NLCD = 'CENSUS CROPLAND USED PAST/GRAZING (KM2) - NLCD METHOD'
   WOTHR_CRPLND_NLCD = 'CENSUS OTHER CROPLAND (KM2) - NLCD METHOD'
   WALL_PAST_NLCD = 'CENSUS ALL PASTURELAND (KM2) - NLCD METHOD'
   WPAST_RANG_NLCD = 'CENSUS PASTURE/RANGE LANDS (KM2) - NLCD METHOD'
   WWDLND_PAST_NLCD = 'CENSUS WOODLAND/PASTURE (KM2) - NLCD METHOD'

/* 1992 Agr Census crop types */
  WWHEAT = 'AG CENSUS WHEAT AREA - EQUAL METHOD (KM2)'
  WVEG = 'AG CENSUS VEGETABLE AREA - EQUAL METHOD (KM2)'
  WTOBACCO = 'AG CENSUS TOBACCO AREA - EQUAL METHOD (KM2)'
  WSUNFLWRS = 'AG CENSUS SUNFLOWERS AREA - EQUAL METHOD (KM2)'
  WSOYBEANS = 'AG CENSUS SOYBEANS AREA - EQUAL METHOD (KM2)'
  WSORGHUM = 'AG CENSUS SORGHUM AREA - EQUAL METHOD (KM2)'
  WSGRBTS = 'AG CENSUS SUGAR BEETS AREA - EQUAL METHOD (KM2)'
  WRICE = 'AG CENSUS RICE AREA - EQUAL METHOD (KM2)'
  WPOTATOES = 'AG CENSUS POTATOES AREA - EQUAL METHOD (KM2)'
  WPEANUTS = 'AG CENSUS PEANUTS AREA - EQUAL METHOD (KM2)'
  WFLDGRS = 'AG CENSUS FIELD GREENS AREA - EQUAL METHOD (KM2)'
  WBEANS_PEAS = 'AG CENSUS BEANS/PEAS AREA - EQUAL METHOD (KM2)'
  WCOTTON = 'AG CENSUS COTTON AREA - EQUAL METHOD (KM2)'
  WCORN = 'AG CENSUS CORN AREA - EQUAL METHOD (KM2)'
  WALF = 'AG CENSUS ALFALFA AREA - EQUAL METHOD (KM2)'
  WROWCROP_SUM = 'AG CENSUS CROP SUM AREA - EQUAL METHOD (KM2)'

  WHEAT_NLCD = 'AG CENSUS WHEAT AREA - NLCD  METHOD (KM2)'
  VEG_NLCD = 'AG CENSUS VEGETABLE AREA - NLCD  METHOD (KM2)'
  TOBACCO_NLCD = 'AG CENSUS TOBACCO AREA - NLCD  METHOD (KM2)'
  SUNFLWRS_NLCD = 'AG CENSUS SUNFLOWERS AREA - NLCD  METHOD (KM2)'
  SOYBEANS_NLCD = 'AG CENSUS SOYBEANS AREA - NLCD  METHOD (KM2)'
  SORGHUM_NLCD = 'AG CENSUS SORGHUM AREA - NLCD  METHOD (KM2)'
  SGRBTS_NLCD = 'AG CENSUS SUGAR BEETS AREA - NLCD  METHOD (KM2)'
  RICE_NLCD = 'AG CENSUS RICE AREA - NLCD  METHOD (KM2)'
  POTATOES_NLCD = 'AG CENSUS POTATOES AREA - NLCD  METHOD (KM2)'
  PEANUTS_NLCD = 'AG CENSUS PEANUTS AREA - NLCD  METHOD (KM2)'
  FLDGRS_NLCD = 'AG CENSUS FIELD GREENS AREA - NLCD  METHOD (KM2)'
  BEANS_PEAS_NLCD = 'AG CENSUS BEANS/PEAS AREA - NLCD  METHOD (KM2)'
  COTTON_NLCD = 'AG CENSUS COTTON AREA - NLCD  METHOD (KM2)'
  CORN_NLCD = 'AG CENSUS CORN AREA - NLCD  METHOD (KM2)'
  ALF_NLCD = 'AG CENSUS ALFALFA AREA - NLCD  METHOD (KM2)'
  ROWCROP_SUM_NLCD = 'AG CENSUS CROP SUM AREA - NLCD  METHOD (KM2)'

/* Conservation Tillage area - 1992  */
 WCT_MT = 'MULCH TILL (KM2)'
 WCT_NT = 'NO TILL (KM2)'
 WCT_RT = 'RIDGE TILL (KM2)'
 WCT_R5 = 'RESIDUE 15-30% (KM2)'
 WCT_RL = 'RESIDUE <15% (KM2)'
 WCT_ALL = 'ALL TILLAGE (KM2)'
 WCT_MT_NLCD = 'MULCH TILL - NLCD METHOD (KM2)'
 WCT_NT_NLCD = 'NO TILL - NLCD METHOD (KM2)'
 WCT_RT_NLCD = 'RIDGE TILL - NLCD METHOD (KM2)'
 WCT_R5_NLCD = 'RESIDUE 15-30% - NLCD METHOD (KM2)'
 WCT_RL_NLCD = 'RESIDUE <15% - NLCD METHOD (KM2)'
 WCT_ALL_NLCD = 'ALL TILLAGE - NLCD METHOD (KM2)'

/* Tile drained land area - 1978  */
  WDRAINSKM2_NLCD = 'TILE DRAINED AREA 1978 - NLCD METHOD (KM2)'
  WDRAINSKM2 = 'TILE DRAINED AREA 1978 - AREA METHOD (KM2)'

/* 1990 Census of Population */
   TOTPOP = 'TOTAL POPULATION - 1990'
   PSEWER = 'POPULATION USING SEWER SYSTEM'
   PSEPTIC = 'POPULATION USING SEPTIC SYSTEM'
   POTHER = 'POPULATION USING OTHER SYSTEM'
   PPSEWER = 'POPULATION W SEWER (%)'
   PPSEPTIC = 'POPULATION W SEPTIC (%)'
   PPOTHER = 'POPULATION W OTHER SYSTEM (%)'

/* Topmodel variables */
   perdun = 'Dunne overland flow (%)'
   perhor = 'Horton overland flow (%)'

/* STATSGO variables - Wolock 1-km data */
    permave = 'Soil permeability (in per hr)'
    sandave = 'Sand percentage'
    omave = 'Organic matter mean (% by weight)'
    slopel = 'Minimum percent slope'
    slopeh = 'Maximum percent slope'
    awcave = 'Water capacity (fraction)'
    rockdepave = 'Average depth to rock (in)'
    bdl = 'Minimum bulk density (g/cm3)'
    bdh = 'Maximum bulk density (g/cm3)'
    omlup = 'Organic matter minimum - upper layer (% by weight)'
    omhup = 'Organic matter maximum - upper layer (% by weight)'
    wtdepl = 'Seasonally high water table min (ft)'
    wtdeph = 'Seasonally high water table max (ft)'
    OMA = 'Areal organic matter content (kg/km2)'
    OMM = 'Organic matter mass (kg/yr*1.0E+10)'
    OMAUP = 'Areal organic matter content - upper layer (kg/km2)'
    OMMUP = 'Organic matter mass - upper layer (kg/yr*1.0E+10)'

/* STATSGO data from PSU CONUS 1-km gridded 
    Volumetric % is cubic cm of X (water or rocks) per square cm of soil */
    s_hsga = 'Percent of hydrologic soils group class A'
    s_hsgb = 'Percent of hydrologic soils group class B'
    s_hsgc = 'Percent of hydrologic soils group class C'
	s_hsgd = 'Percent of hydrologic soils group class D'
	s_awc100 = 'Available water capacity for top 100 cm soil (volumetric %)'
	s_awc150 = 'Available water capacity for top 150 cm soil (volumetric %)'
	s_awc250 = 'Available water capacity for top 250 cm soil (volumetric %)'
	s_perm030 = 'Mean permeability rate for top 30 cm soil (cm/hr)'
	s_perm100 = 'Mean permeability rate for top 100 cm soil (cm/hr)'
	s_perm150 = 'Mean permeability rate for top 150 cm soil (cm/hr)'
	s_poros030 = 'Mean porosity rate for top 30 cm soil (cm/hr)'
	s_poros100 = 'Mean porosity rate for top 100 cm soil (cm/hr)'
	s_poros150 = 'Mean porosity rate for top 150 cm soil (cm/hr)'
	s_ph030 = 'Mean pH for top 30 cm soil'
	s_ph100 = 'Mean pH for top 100 cm soil'
	s_ph150 = 'Mean pH for top 150 cm soil'
	s_sand030 = 'Sand content of upper 30 cm soil (%)'
	s_sand100 = 'Sand content of upper 100 cm soil (%)'
	s_sand150 = 'Sand content of upper 150 cm soil (%)'
	s_silt030 = 'Silt content of upper 30 cm soil (%)'
	s_silt100 = 'Silt content of upper 100 cm soil (%)'
	s_silt150 = 'Silt content of upper 150 cm soil (%)'
	s_clay030 = 'Clay content of upper 30 cm soil (%)'
	s_clay100 = 'Clay content of upper 100 cm soil (%)'
	s_clay150 = 'Clay content of upper 150 cm soil (%)'
	s_bulkd030 = 'Bulk density of upper 30 cm soil (g/cm3)'
	s_bulkd100 = 'Bulk density of upper 100 cm soil (g/cm3)'
	s_bulkd150 = 'Bulk density of upper 150 cm soil (g/cm3)'
	s_rockf030 = 'Rock fragments in upper 30 cm soil (volumetric %)'
	s_rockf100 = 'Rock fragments in upper 100 cm soil (volumetric %)'
	s_rockf150 = 'Rock fragments in upper 150 cm soil (volumetric %)'
	s_kfact = 'Erodibility factor KFACT, KFFACT adjusted for rock frags'
	s_kffact = 'Erodibility factor KFFACT'
    s_rockdep = 'Depth to bedrock (cm)'
    OMA150 = 'Areal organic matter content - Upper 150cm (kg/km2)'
	OMA100 = 'Areal organic matter content - Upper 100cm (kg/km2)'
	OMA030 = 'Areal organic matter content - Upper 30cm (kg/km2)'
	OMM150 = 'Organic matter mass - Upper 150cm (kg/yr*1.0E+10)'
	OMM100 = 'Organic matter mass - Upper 100cm (kg/yr*1.0E+10)'
	OMM030 = 'Organic matter mass - Upper 30cm (kg/yr*1.0E+10)'

 /* DAYMET meterological data 1-km gridded 
      All variables below are reported as 18-year annual averages from 1980-1997 */
    d_airt = 'Annual average air temperature (deg C)'
    d_airtvar = 'Day-to-day variability in annual average air temp (deg C)'
    d_airtmx = 'Annual maximum air temperagure (deg C)'
    d_dfrost = 'Annual number of frost days (days)'
	d_dgrow = 'Annual growing degree days (degree days)'
    d_ppt1k = 'Total Annual precipitation (cm)'
    d_pptfreq = 'Mean annual precipitation frequency as proportion (# of wet days/total days)'
    d_pptsize = 'Mean annual precipitation event size (cm/day)'
    d_rad = 'Mean annual radiation of shortwave radiation (MJ/m2/day)'
    d_tq1 = '1st Quarter mean air temperature: avg of monthly means Jan-Mar (deg C)'
    d_tq2 = '2nd Quarter mean air temperature: avg of monthly means Apr-Jun (deg C)'
    d_tq3 = '3rd Quarter mean air temperature: avg of monthly means Jul-Sep (deg C)'
    d_tq4 = '4th Quarter mean air temperature: avg of monthly means Oct-Dec (deg C)'
    d_pq1 = '1st Quarter total precipitation: sum of monthly totals Jan-Mar (cm)'
    d_pq2 = '2nd Quarter total precipitation: sum of monthly totals Apr-Jun (cm)'
    d_pq3 = '3rd Quarter total precipitation: sum of of monthly totals Jul-Sep (cm)'
    d_pq4 = '4th Quarter total precipitation: sum of of monthly totals Oct-Dec (cm)'

/*  Terrain Metrics derived from HYDRO1K/GTOPO30 - DEM derived reach network */ 
	t_slod8 = 'Slope from D8 flow directions (%)'
    t_slodinf = 'Slope from Dinf flow directions (%)'
	t_ln_sca = 'ln of Specific catchment area (m): accumulation area (m2) per grid cell length (m)'
	t_ln_dist = 'ln of Mean distance along D8 flow paths to stream (m)'
	t_racc = 'Downslope accumulation along DINF flow paths to stream (m2)'
	t_ln_ratanb = 'ln of (Dinf slope / sca); reverse of standard TI'   
	
/* EPA point source load estimates */
     FLOW_ON_OBS = 'Flow discharge on RF1 (mg/d)'
     FLOW_TO_OBS = 'Flow discharge to RF1 (mg/d)'
     FLOW_NEAR_OBS = 'Flow discharge near RF1 (mg/d)'
     FLOW_ON_EST = 'Estimated flow discharge on RF1 (mg/d)'
     FLOW_TO_EST = 'Estimated flow discharge to RF1 (mg/d)'
     FLOW_NEAR_EST = 'Estimated flow discharge near RF1 (mg/d)'

     NITRO_ON_OBS = 'Nitrogen discharge on RF1 (kg/yr)'
     NITRO_TO_OBS = 'Nitrogen discharge to RF1 (kg/yr)'
     NITRO_NEAR_OBS = 'Nitrogen discharge near RF1 (kg/yr)'
     NITRO_ON_EST = 'Estimated nitrogen discharge on RF1 (kg/yr)'
     NITRO_TO_EST = 'Estimated nitrogen discharge to RF1 (kg/yr)'
     NITRO_NEAR_EST = 'Estimated nitrogen discharge near RF1 (kg/yr)'

     PHOS_ON_OBS = 'Phosphorus discharge on RF1 (kg/yr)'
     PHOS_TO_OBS = 'Phosphorus discharge to RF1 (kg/yr)'
     PHOS_NEAR_OBS = 'Phosphorus discharge near RF1 (kg/yr)'
     PHOS_ON_EST = 'Estimated phosphorus discharge on RF1 (kg/yr)'
     PHOS_TO_EST = 'Estimated phosphorus discharge to RF1 (kg/yr)'
     PHOS_NEAR_EST = 'Estimated phosphorus discharge near RF1 (kg/yr)'

     BOD5_ON_OBS = 'BOD5 discharge on RF1 (kg/yr)'
     BOD5_TO_OBS = 'BOD5 discharge to RF1 (kg/yr)'
     BOD5_NEAR_OBS = 'BOD5 discharge near RF1 (kg/yr)'
     BOD5_ON_EST = 'Estimated BOD5 discharge on RF1 (kg/yr)'
     BOD5_TO_EST = 'Estimated BOD5 discharge to RF1 (kg/yr)'
     BOD5_NEAR_EST = 'Estimated BOD5 discharge near RF1 (kg/yr)'

/* 1987 agricultural data */
  WFERT_N87 = '1987 N FERTILIZER - AREA BASED (KG)'
  WFERT_P87 = '1987 P FERTILIZER - AREA BASED (KG)'
  WNTOTAL87 = '1987 N MANURE EXCRETED - USGS (KG)'
  WPTOTAL87 = '1987 P MANURE EXCRETED - USGS (KG)'
  WNFERT87 = '1987 NFERTILIZER - BATTG USGS (KG)'

/* 1987 deposition estimates */
  NO3DEP87 = '1987 NO3 DEPOSITION (KG/YR)'

/* 1987 county-based Gianessi point sources */
  WTKN = 'NITROGEN (KG/YR) - 1987'
  WBOD5 = 'BOD 5-DAY (KG/YR) - 1987'
  WTOTP = 'TOTAL PHOSPHORUS (KG/YR) - 1987'
  WTSS = 'TOTAL SUSP SOLIDS (KG/YR) - 1987'

;

  KEEP 
     /* reach and catchment attributes */
       WATERID E2RF1 ERF1 ARCNUM ARCID FNODE TNODE RR MEANQ MEANV FRAC 
       HUC
       RCHTOT RCHLEN DEMIAREA DEMTAREA 
       HYDSEQ RCHTYPE HEADFLAG TERMFLAG PNAME CONTFLAG
       RESCODE RCHTYPE 
       PNMCD RESAREA GISAREA RESCAP QOUT PREFGIS RESLEN
	   RFRICTION RDEPTH RCHSLOPE RCHSLOPEF DRAINDEN

	   /* station attributes */
       STAID STATION_ID STATION_NAME DAREAKM2 DIFAREA

       /* nutrient criteria region IDs */
       NUTREG NUTNAME

	   /* reservoir areal water load */
	   HLOAD RHLOAD 

       /* NID reservoir storage */
       stortot storless 

	   /* foreign drainage estimates */
       FADRAIN FADRAIN_PERCENT  

	   /* 1987 station load estimates */
       NLD87 PLD87

	   /* 1987-based SPARROW TN concentration estimates (MG/L) */
       TNCONC

	   /* 1992-based load estimates */
       LOAD_A_60000 LOAD_A_66500 LOAD_A_31625 LOAD_A_00680 LOAD_A_00681
       SE_60000 SE_66500 SE_31625 SE_00680 SE_00681
	   METHOD_60000 METHOD_66500 METHOD_31625 METHOD_00680 METHOD_00681
	   WQ_START_DATE_60000 WQ_END_DATE_60000
       WQ_START_DATE_66500 WQ_END_DATE_66500
       WQ_START_DATE_31625 WQ_END_DATE_31625
       WQ_START_DATE_00680 WQ_END_DATE_00680
       WQ_START_DATE_00681 WQ_END_DATE_00681
	   PROGRAM N_PREDICT_DAYS 
       N_WQ_OBS_60000 N_WQ_OBS_66500 N_WQ_OBS_31625 N_WQ_OBS_00680 N_WQ_OBS_00681
       FLOW_STATION_AREA ALTSITE_AREA
       LAT LON

	   /* simple-average load estimates */
       ML600 ML665 MLSED MLCOLI  
       FLAGTN FLAGTP FLAGSED FLAGCOLI

	 /* Fertilizer 1992 for farm (NLCD), non-farm (NLCD), and total and average using non-NLCD disagregation */
     wfert_n wfert_p wffert_n wffert_p wnffert_n wnffert_p
     WFERTAVG_N WFERTAVG_P

	 /* Livestock wastes 1992 for USGS and ERS methods */
     wntotal wptotal wntotal_a wptotal_a wne_u wpe_u wne_c wpe_c wrn_c wrp_c
     wne_u_a wpe_u_a wne_c_a wpe_c_a wrn_c_a wrp_c_a

	 /* Ag Census 1992 estimates of row crops and pasture land */
     /*  Derived variables */
	 WAGCROPS WAGPAST WAGCROPS_NLCD WAGPAST_NLCD
     WTCRP_WO WALL_PAST_WO WTCRP_WO_NLCD WALL_PAST_WO_NLCD 

     /* Ag Census non-derived items */
     WTCRP WHARV_CRP WCRP_PASTGRZ WOTHR_CRPLND WALL_PAST WPAST_RANG WWDLND_PAST
     WTCRP_NLCD WHARV_CRP_NLCD WCRP_PASTGRZ_NLCD WOTHR_CRPLND_NLCD 
     WALL_PAST_NLCD WPAST_RANG_NLCD WWDLND_PAST_NLCD

	 /* Conservation Tillage area - 1992  */
     WCT_MT WCT_NT WCT_RT WCT_R5 WCT_RL WCT_ALL 
     WCT_MT_NLCD WCT_NT_NLCD WCT_RT_NLCD WCT_R5_NLCD WCT_RL_NLCD WCT_ALL_NLCD

     /* Tile drained land area */
     WDRAINSKM2_NLCD WDRAINSKM2

     /* stream properties */
     RUNOFF WTEMP

	 /* 1992-based NADP wet-deposition estimates */
     NO3DEP NH4DEP 

	 /* NLCD 21-class estimates land use */
     LU00 LU11 LU12 LU21 LU22 LU23 LU31 LU32 LU33 LU41 LU42 LU43 
     LU51 LU61 LU71 LU81 LU82 LU83 LU84 LU85 LU91 LU92 

     /* Topmodel variables */
     perdun perhor

	 /* STATSGO estimates of soil properties */
     permave sandave SLOPEL SLOPEH awcave omave ROCKDEPAVE BDL BDH 
     OMA OMAUP OMM OMMUP OMLUP OMHUP WTDEPL WTDEPH

    /* PSU CONUS 1-km gridded STATSGO data */
    s_hsga s_hsgb s_hsgc s_hsgd s_awc100 s_awc150 s_awc250
    s_perm030 s_perm100 s_perm150 s_poros030 s_poros100 s_poros150 
	s_ph030 s_ph100 s_ph150 s_sand030 s_sand100 s_sand150
	s_silt030 s_silt100 s_silt150 s_clay030 s_clay100 s_clay150
	s_bulkd030 s_bulkd100 s_bulkd150 s_rockf030 s_rockf100 s_rockf150
	s_kfact s_kffact s_rockdep
    OMA150 OMA100 OMA030 OMM150 OMM100 OMM030


    /* DAYMET meterological data */
    d_airt d_airtvar d_airtmx d_dfrost d_dgrow d_ppt1k d_pptfreq d_pptsize
    d_rad d_tq1 d_tq2 d_tq3 d_tq4
    d_pq1 d_pq2 d_pq3 d_pq4

    /* Terrain metrics from HYDRO1K */
	t_slod8 t_slodinf t_ln_sca t_ln_dist t_racc t_ln_ratanb 

	 /* USEPA PCS-based estimates of municipal/industrial effluent loads */
     FLOW_ON_OBS FLOW_TO_OBS FLOW_NEAR_OBS FLOW_ON_EST FLOW_TO_EST FLOW_NEAR_EST
     NITRO_ON_OBS NITRO_TO_OBS NITRO_NEAR_OBS NITRO_ON_EST NITRO_TO_EST NITRO_NEAR_EST
     PHOS_ON_OBS PHOS_TO_OBS PHOS_NEAR_OBS PHOS_ON_EST PHOS_TO_EST PHOS_NEAR_EST
     BOD5_ON_OBS BOD5_TO_OBS BOD5_NEAR_OBS BOD5_ON_EST BOD5_TO_EST BOD5_NEAR_EST

	 /* 1990-based estimates of population and sewer/septic population */
     TOTPOP PSEWER PSEPTIC POTHER PPSEWER PPSEPTIC PPOTHER

     /* 1992 Agr Census crop types */
    WWHEAT WVEG WTOBACCO WSUNFLWRS WSOYBEANS WSORGHUM WSGRBTS 
    WRICE WPOTATOES WPEANUTS WFLDGRS WBEANS_PEAS
    WCOTTON WCORN WALF WROWCROP_SUM
    WHEAT_NLCD VEG_NLCD TOBACCO_NLCD SUNFLWRS_NLCD SOYBEANS_NLCD SORGHUM_NLCD SGRBTS_NLCD 
    RICE_NLCD POTATOES_NLCD PEANUTS_NLCD FLDGRS_NLCD BEANS_PEAS_NLCD
    COTTON_NLCD CORN_NLCD ALF_NLCD ROWCROP_SUM_NLCD

     /* 1987 agricultural, deposition, and point-source data  */
     WFERT_N87 WFERT_P87 WNTOTAL87 WPTOTAL87 WNFERT87 NO3DEP87 WTKN WBOD5 WTOTP WTSS

;
RUN;

/* add calibration site indicator from 1987 model */
PROC SORT DATA=DIR.DATA1; BY STATION_ID;
DATA DIR.DATA1; MERGE DIR.DATA1 DIR1.NASQAN374; BY STATION_ID;
  IF CALIB_374 = . THEN CALIB_374 = 0;

  LABEL
  SE_NLD87 = 'TN Standard Error - 1987 based estimate (% MEAN)'
  ;

RUN;

/*  Make final sort of file    */

PROC SORT DATA = DIR.DATA1;
  BY HYDSEQ;

RUN; 

