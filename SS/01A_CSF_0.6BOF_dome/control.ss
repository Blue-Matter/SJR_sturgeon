#V3.30.14.00-safe;_2019_07_19;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.0

0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
1  #_N_Growth_Patterns (Growth Patterns, Morphs, Bio Patterns, GP are terms used interchangeably in SS)
1 #_N_platoons_Within_GrowthPattern 
#_Cond 1 #_Platoon_between/within_stdev_ratio (no read if N_platoons=1)
#_Cond  1 #vector_platoon_dist_(-1_in_first_val_gives_normal_approx)
#
4 # recr_dist_method for parameters:  2=main effects for GP, Area, Settle timing; 3=each Settle entity; 4=none (only when N_GP*Nsettle*pop==1)
1 # not yet implemented; Future usage: Spawner-Recruitment: 1=global; 2=by area
1 # number of recruitment settlement assignments 
0 # unused option
#GPattern month  area  age (for each settlement assignment)
 1 1 1 0
#
#_Cond 0 # N_movement_definitions goes here if Nareas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
0 #_Nblock_Patterns
#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
#
# AUTOGEN
1 1 1 1 1 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen all time-varying parms; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
#
#_Available timevary codes
#_Block types: 0: P_block=P_base*exp(TVP); 1: P_block=P_base+TVP; 2: P_block=TVP; 3: P_block=P_block(-1) + TVP
#_Block_trends: -1: trend bounded by base parm min-max and parms in transformed units (beware); -2: endtrend and infl_year direct values; -3: end and infl as fraction of base range
#_EnvLinks:  1: P(y)=P_base*exp(TVP*env(y));  2: P(y)=P_base+TVP*env(y);  3: null;  4: P(y)=2.0/(1.0+exp(-TVP1*env(y) - TVP2))
#_DevLinks:  1: P(y)*=exp(dev(y)*dev_se;  2: P(y)+=dev(y)*dev_se;  3: random walk;  4: zero-reverting random walk with rho;  21-24 keep last dev for rest of years
#
#_Prior_codes:  0=none; 6=normal; 1=symmetric beta; 2=CASAL's beta; 3=lognormal; 4=lognormal with biascorr; 5=gamma
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement 
#

0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
  #_no additional input for selected M option; read 1P per morph
#
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr; 5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
1 #_Age(post-settlement)_for_L1;linear growth below this
999 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0  #_placeholder for future growth feature
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
#
1 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
5 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
2 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#

## FEMALE BIOLOGY
#_growth_parms
#_ LO	HI		INIT	PRIOR 	PR_SD 	PR_type PHASE 	env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
# Sex: 1  BioPattern: 1  NatMort
0.05	0.10	0.06	0		0		0		-50		0	0	0	0	0	0	0 # NatM_p_1_Fem_GP_1

# Sex: 1  BioPattern: 1  Growth
1		45		19.7	0		0		0		-50		0	0	0	0	0	0	0 # L_at_Amin_Fem_GP_1
200		300		264		0		0		0		-50		0	0	0	0	0	0	0 # L_at_Amax_Fem_GP_1
0.01	0.2		0.04	0		0		0		-50		0	0	0	0	0	0	0 # VonBert_K_Fem_GP_1
0.01	0.2		0.1		0		0		0		-50		0	0	0	0	0	0	0 # CV_young_Fem_GP_1
0.01	0.2		0.1		0		0		0		-50		0	0	0	0	0	0	0 # CV_old_Fem_GP_1

# Sex: 1  BioPattern: 1  WtLen
0		0.1		2e-5	0		0		0		-50		0	0	0	0	0	0	0 # Wtlen_1_Fem_GP_1
2		4		2.72	0		0		0		-50		0	0	0	0	0	0	0 # Wtlen_2_Fem_GP_1

# Sex: 1  BioPattern: 1  Maturity&Fecundity
100		200		175		0		0		0		-50		0	0	0	0	0	0	0 # Mat50%_Fem_GP_1
-0.5	0.5		-0.2	0		0		0		-50		0	0	0	0	0	0	0 # Mat_slope_Fem_GP_1
-3		3		1		0		0		0		-50		0	0	0	0	0	0	0 # Eggs/kg_inter_Fem_GP_1
-3		3		0		0		0		0		-50		0	0	0	0	0	0	0 # Eggs/kg_slope_wt_Fem_GP_1

## MALE BIOLOGY
# Sex: 2  BioPattern: 1  NatMort
0		1		0.4054651	0		0		0		-50		0	0	0	0	0	0	0 # NatM_p_1_Mal_GP_1

# Sex: 2  BioPattern: 1  Growth
0		45		0.0639038		0		0		0		-50		0	0	0	0	0	0	0 # L_at_Amin_Mal_GP_1
-1		1		-0.1378698		0		0		0		-50		0	0	0	0	0	0	0 # L_at_Amax_Mal_GP_1
0.01	0.5		0.4054651	0		0		0		-50		0	0	0	0	0	0	0 # VonBert_K_Mal_GP_1
0		0.2		0		0		0		0		-50		0	0	0	0	0	0	0 # CV_young_Mal_GP_1
0		0.2		0		0		0		0		-50		0	0	0	0	0	0	0 # CV_old_Mal_GP_1

# Sex: 2  BioPattern: 1  WtLen
0		0.1		2e-5	0		0		0		-50		0	0	0	0	0	0	0 # Wtlen_1_Mal_GP_1
2		4		2.72	0		0		0		-50		0	0	0	0	0	0	0 # Wtlen_2_Mal_GP_1

# Hermaphroditism

#  Recruitment Distribution  
 #0 0 0 0 0 0 -4 0 0 0 0 0 0 0 # RecrDist_GP_1
 #0 0 0 0 0 0 -4 0 0 0 0 0 0 0 # RecrDist_Area_1
 #0 0 0 0 0 0 -4 0 0 0 0 0 0 0 # RecrDist_month_1


#  Cohort growth dev base
 0.1 10 1 1 1 0 -1 0 0 0 0 0 0 0 # CohortGrowDev

0 2 1.67 0 0 -1 -50 0 0 0 0 0 0 0 # Catch_Mult:_3_Historical_BayofFundy

#  Movement
#  Age Error from parameters
#  catch multiplier
#  fraction female, by GP
 1e-006 0.999999 0.5 0 0 -1 -50 0 0 0 0 0 0 0 # FracFemale_GP_1
#
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
3 #_Spawner-Recruitment; Options: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepherd_3Parm; 9=RickerPower_3parm
1  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn #  parm_name

#_ LO	HI		INIT	PRIOR 	PR_SD 	PR_type PHASE 	env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
0.05	5		2		0		0		0		1		0	0	0	0	0	0	0 # SR_LN(R0)
0.2		1		0.6		0		0		0		-50		0	0	0	0	0	0	0 # SR_BH_steep
0.2		1		0.3		0		0		0		-50		0	0	0	0	0	0	0 # SR_sigmaR
-1		1		0		0		0		0		-50		0	0	0	0	0	0	0 # SR_regime
-1		1		0		0		0		0		-50		0	0	0	0	0	0	0 # SR_autocorr

#_no timevary SR parameters
0 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
1971 # first year of main recr_devs; early devs can preceed this era
2015 # last year of main recr_devs; forecast devs start in following year
-1 #_recdev phase
 
0 # (0/1) to read 13 advanced options
#0 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
#-4 #_recdev_early_phase
#0 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
#1 #_lambda for Fcast_recr_like occurring before endyr+1
#1962.8 #1966.8 #1980 #_last_yr_nobias_adj_in_MPD; begin of ramp
#1983.6 #1985 #_first_yr_fullbias_adj_in_MPD; begin of plateau
#2013.5 #2014 #_last_yr_fullbias_adj_in_MPD
#2015 #2016 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS sets bias_adj to 0.0 for fcast yrs)
# 0.9 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
# 0 #_period of cycles in recruitment (N parms read below)
#-15 #min rec_dev
# 15 #max rec_dev
# 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#  1971R 1972R 1973R 1974R 1975R 1976R 1977R 1978R 1979R 1980R 1981R 1982R 1983R 1984R 1985R 1986R 1987R 1988R 1989R 1990R 1991R 1992R 1993R 1994R 1995R 1996R 1997R 1998R 1999R 2000R 2001R 2002F 2003F 2004F 2005F 2006F 2007F 2008F 2009F 2010F 2011F
#  0.152049 -0.0946753 0.110298 -0.178096 0.0435859 0.709985 -0.0130968 0.00421151 0.267748 0.165904 0.132445 -0.228699 -0.466715 -0.301659 0.402607 0.57651 0.23602 0.154297 -0.422341 0.555724 -0.653761 -0.244394 -0.987371 0.376023 -0.476265 0.408171 1.1255 -0.544831 -0.661744 0.160207 -0.307643 0 0 0 0 0 0 0 0 0 0
# implementation error by year in forecast:  0 0 0 0 0 0 0 0 0 0
#
#Fishing Mortality info 
0.2 # F ballpark
-2008 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
3 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
7  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms; count = 0
#_ LO 	HI 	INIT 	PRIOR 	PR_SD  PR_type  PHASE
#0		1	0		0		0		0		-50	# F1
#0		1	0		0		0		0		-50	# F2
#
#_Q_setup for fleets with cpue or survey data
#_1:  fleet number
#_2:  link type: (1=simple q, 1 parm; 2=mirror simple q, 1 mirrored parm; 3=q and power, 2 parm; 4=mirror with offset, 2 parm)
#_3:  extra input for link, i.e. mirror fleet# or dev index number
#_4:  0/1 to select extra sd parameter
#_5:  0/1 for biasadj or not
#_6:  0/1 to float
#_survey: 4 Depletion is a depletion fleet
#_Q_setup(f,2)=0; add 1 to phases of all parms; only R0 active in new phase 1
#_fleet link link_info  extra_se   biasadj     float  #  fleetname
 4	1	0	0	0	1  #  Survey
-9999 0 0 0 0 0	   

#_Q_parms(if_any);Qunits_are_ln(q)
#_ LO	HI		INIT	PRIOR 	PR_SD 	PR_type PHASE 	env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
0.05	5		0		0		0		0		-50		0	0	0	0	0	0	0 # LnQ_base_SURVEY(3)

#_no timevary Q parameters
#
#_size_selex_patterns
#Pattern:_0; parm=0; selex=1.0 for all sizes
#Pattern:_1; parm=2; logistic; with 95% width specification
#Pattern:_5; parm=2; mirror another size selex; PARMS pick the min-max bin to mirror
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_6; parm=2+special; non-parm len selex
#Pattern:_43; parm=2+special+2;  like 6, with 2 additional param for scaling (average over bin range)
#Pattern:_8; parm=8; New doublelogistic with smooth transitions and constant above Linf option
#Pattern:_9; parm=6; simple 4-parm double logistic with starting length; parm 5 is first length; parm 6=1 does desc as offset
#Pattern:_21; parm=2+special; non-parm len selex, read as pairs of size, then selex
#Pattern:_22; parm=4; double_normal as in CASAL
#Pattern:_23; parm=6; double_normal where final value is directly equal to sp(6) so can be >1.0
#Pattern:_24; parm=6; double_normal with sel(minL) and sel(maxL), using joiners
#Pattern:_25; parm=3; exponential-logistic in size
#Pattern:_27; parm=3+special; cubic spline 
#Pattern:_42; parm=2+special+3; // like 27, with 2 additional param for scaling (average over bin range)
#_discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead;_4=define_dome-shaped_retention
#_Pattern Discard Male Special
 24 0 3 0 # 1 F1
 15 0 0 1 # 2 F2
 24 0 0 1 # 3 F3
 0 0 0 0 # 4 S4
#
#_age_selex_patterns
#Pattern:_0; parm=0; selex=1.0 for ages 0 to maxage
#Pattern:_10; parm=0; selex=1.0 for ages 1 to maxage
#Pattern:_11; parm=2; selex=1.0  for specified min-max age
#Pattern:_12; parm=2; age logistic
#Pattern:_13; parm=8; age double logistic
#Pattern:_14; parm=nages+1; age empirical
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_16; parm=2; Coleraine - Gaussian
#Pattern:_17; parm=nages+1; empirical as random walk  N parameters to read can be overridden by setting special to non-zero
#Pattern:_41; parm=2+nages+1; // like 17, with 2 additional param for scaling (average over bin range)
#Pattern:_18; parm=8; double logistic - smooth transition
#Pattern:_19; parm=6; simple 4-parm double logistic with starting age
#Pattern:_20; parm=6; double_normal,using joiners
#Pattern:_26; parm=3; exponential-logistic in age
#Pattern:_27; parm=3+special; cubic spline in age
#Pattern:_42; parm=2+special+3; // cubic spline; with 2 additional param for scaling (average over bin range)
#_Pattern Discard Male Special
 10 0 0 0 # 1 F1
 10 0 0 0 # 2 F2
 10 0 0 0 # 3 F3
 10 0 0 0 # 4 S4
#
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
### Size Selectivity
#_LO 	HI 		INIT 	PRIOR	SD	PR_type PHASE   env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn
130		250		200		0		0	0		2		0	0	0	0	0	0	0 #  Size_DblN_peak_Fishery(1)
-15.0	15.0	-1		0		0	0		2		0	0	0	0	0	0	0 #  Size_DblN_top_logit_Fishery(1)
-5.0	10.0	5		0		0	0		2		0	0	0	0	0	0	0 #  Size_DblN_ascend_se_Fishery(1)
-5.0	20.0	5		0		0	0		2		0	0	0	0	0	0	0 #  Size_DblN_descend_se_Fishery(1)
-15.0	15.0	-15		0		0	0		-50		0	0	0	0	0	0	0 #  Size_DblN_start_logit_Fishery(1)
-1000	15.0	-1000	0		0	0		-50		0	0	0	0	0	0	0 #  Size_DblN_end_logit_Fishery(1)
                                                                                
### Male Offset                                                                 
-50		50		-20		0		0	0		2		0	0 	0	0	0	0	0 #  SzSel_Male_Peak_Fishery(1)
-15		15		0		0		0	0		2		0	0 	0	0	0	0	0 #  SzSel_Male_Ascend_Fishery(1)
-20		2		0		0		0	0		2		0	0 	0	0	0	0	0 #  SzSel_Male_Descend_Fishery(1)
-30		0		0		0		0	0		2		0	0 	0	0	0	0	0 #  SzSel_Male_Final_Fishery(1)
0		1		0.8		0		0	0		-50		0	0 	0	0	0	0	0 #  SzSel_Male_Scale_Fishery(1)

100		250		150		0		0	0		2		0	0	0	0	0	0	0 #  Size_DblN_peak_Historical_BayofFundy(3)
-15.0	15.0	-15		0		0	0		-50		0	0	0	0	0	0	0 #  Size_DblN_top_logit_Historical_BayofFundy(3)
-5.0	10.0	5		0		0	0		2		0	0	0	0	0	0	0 #  Size_DblN_ascend_se_Historical_BayofFundy(3)
-5.0	20.0	5		0		0	0		2		0	0	0	0	0	0	0 #  Size_DblN_descend_se_Historical_BayofFundy(3)
-1000	15.0	-999	0		0	0		-50		0	0	0	0	0	0	0 #  Size_DblN_start_logit_Historical_BayofFundy(3)
-1000	15.0	-999	0		0	0		-50		0	0	0	0	0	0	0 #  Size_DblN_end_logit_Historical_BayofFundy(3)

#
0   #  use 2D_AR1 selectivity(0/1):  experimental feature
#_no 2D_AR1 selex offset used
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read and autogen if tag data exist; 1=read
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# no timevary parameters
#
#
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp
#_Factor  Fleet  Value
-9999   1    0  # terminator


#
1 #_maxlambdaphase
0 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 3 changes to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark; 18=initEQregime
#like_comp fleet  phase  value 
-9999  1  1  1  1  #  terminator
#R

0 #1 # (0/1) read specs for more stddev reporting 
 #1 1 -1 5 1 5 1 -1 5 # selex_fleet, 1=len/2=age/3=both, year, N selex bins, 0 or Growth pattern, N growth ages, 0 or NatAge_area(-1 for sum), NatAge_yr, N Natages
 #5 15 25 35 43 # vector with selex std bins (-1 in first bin to self-generate)
 #1 2 14 26 40 # vector with growth std ages picks (-1 in first bin to self-generate)
 #1 2 14 26 40 # vector with NatAge std ages (-1 in first bin to self-generate)
999

