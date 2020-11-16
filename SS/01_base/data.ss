#V3.30.14.00-safe;_2019_07_19;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.0

1880 #_StartYr
2020 #_EndYr
1 #_Nseas
12 #_months/season
2 #_Nsubseasons (even number, minimum is 2)
7 #_spawn_month
2 #_Ngenders: 1, 2, -1  (use -1 for 1 sex setup with SSB multiplied by female_frac parameter)
60 #_Nages=accumulator age, first age is always age 0
1 #_Nareas
3 #_Nfleets (including surveys)
#_fleet_type: 1=catch fleet; 2=bycatch only fleet; 3=survey; 4=ignore 
#_sample_timing: -1 for fishing fleet to use season-long catch-at-age for observations, or 1 to use observation month;  (always 1 for surveys)
#_fleet_area:  area the fleet/survey operates in 
#_units of catch:  1=bio; 2=num (ignored for surveys; their units read later)
#_catch_mult: 0=no; 1=yes
#_rows are fleets
#_fleet_type fishery_timing area catch_units need_catch_mult fleetname
 1 -1 1 2 0 Fishery  # 1
 1 -1 1 1 0 Historical_pre2007  # 2
 3 1 1 2 0 Fishery_CPUE  #3

#_Catch data: yr, seas, fleet, catch, catch_se
#_catch_se:  standard error of log(catch)
#_NOTE:  catch data is ignored for survey fleets
2007	1	1	0.142	0.01
2008	1	1	0.116	0.01
2009	1	1	0.274	0.01
2010	1	1	0.173	0.01
2011	1	1	0.263	0.01
2012	1	1	0.246	0.01
2013	1	1	0.347	0.01
2014	1	1	0.344	0.01
2015	1	1	0.347	0.01
2016	1	1	0.286	0.01
2017	1	1	0.339	0.01
2018	1	1	0.351	0.01
2019	1	1	0.33	0.01
2020	1	1	0.197	0.01 # provisional see email to Daphne Nov. 16, 2020
1880	1	2	273.29	0.01
1881	1	2	205.68	0.01
1882	1	2	128.98	0.01
1883	1	2	56.83	0.01
1884	1	2	29.11	0.01
1885	1	2	11.9	0.01
1886	1	2	7.38	0.01
1897	1	2	11.34	0.01
1898	1	2	9.07	0.01
1899	1	2	5.44	0.01
1900	1	2	4.54	0.01
1901	1	2	0.91	0.01
1902	1	2	0.45	0.01
1903	1	2	2.72	0.01
1904	1	2	2.72	0.01
1905	1	2	4.38	0.01
1906	1	2	4.9	0.01
1907	1	2	4.31	0.01
1908	1	2	2.227	0.01
1909	1	2	5.58	0.01
1910	1	2	7.48	0.01
1911	1	2	2.18	0.01
1912	1	2	4.17	0.01
1913	1	2	3.18	0.01
1914	1	2	4.9	0.01
1915	1	2	3.95	0.01
1916	1	2	2.45	0.01
1917	1	2	1.41	0.01
1918	1	2	2.09	0.01
1919	1	2	3.18	0.01
1920	1	2	3.63	0.01
1921	1	2	4.49	0.01
1922	1	2	5.03	0.01
1923	1	2	4.54	0.01
1924	1	2	2.99	0.01
1925	1	2	1.91	0.01
1926	1	2	2.59	0.01
1927	1	2	1.09	0.01
1928	1	2	3.04	0.01
1929	1	2	1.32	0.01
1930	1	2	0.68	0.01
1931	1	2	0.23	0.01
1934	1	2	2.72	0.01
1935	1	2	3.63	0.01
1936	1	2	2.95	0.01
1937	1	2	2.31	0.01
1938	1	2	1.63	0.01
1939	1	2	1.22	0.01
1940	1	2	0.54	0.01
1941	1	2	0.54	0.01
1942	1	2	0.82	0.01
1943	1	2	0.82	0.01
1944	1	2	0.73	0.01
1945	1	2	1.59	0.01
1946	1	2	2.81	0.01
1947	1	2	1.81	0.01
1948	1	2	1.36	0.01
1949	1	2	1.81	0.01
1950	1	2	1.81	0.01
1951	1	2	1.36	0.01
1952	1	2	1.81	0.01
1953	1	2	3.18	0.01
1954	1	2	3.63	0.01
1955	1	2	4.08	0.01
1956	1	2	2.27	0.01
1957	1	2	2.72	0.01
1958	1	2	5.9	0.01
1959	1	2	3.63	0.01
1960	1	2	4.99	0.01
1961	1	2	1.81	0.01
1962	1	2	1.81	0.01
1963	1	2	1.81	0.01
1964	1	2	1.36	0.01
1965	1	2	2.27	0.01
1966	1	2	2.72	0.01
1967	1	2	1.81	0.01
1968	1	2	1.36	0.01
1969	1	2	8.16	0.01
1970	1	2	4.99	0.01
1971	1	2	3.63	0.01
1972	1	2	7.26	0.01
1973	1	2	4.08	0.01
1974	1	2	2.72	0.01
1975	1	2	3.18	0.01
1976	1	2	8.98	0.01
1977	1	2	1.09	0.01
1978	1	2	7.19	0.01
1979	1	2	14.57	0.01
1980	1	2	23.06	0.01
1981	1	2	11.19	0.01
1982	1	2	11.16	0.01
1983	1	2	13.88	0.01
1984	1	2	16.96	0.01
1985	1	2	16.02	0.01
1986	1	2	14.09	0.01
1987	1	2	7.08	0.01
1988	1	2	40.94	0.01
1989	1	2	40.46	0.01
1990	1	2	16.4	0.01
1991	1	2	9.46	0.01
1992	1	2	9.62	0.01
1993	1	2	4.93	0.01
1994	1	2	10.21	0.01
1995	1	2	10.57	0.01
1996	1	2	13.77	0.01
1997	1	2	11.27	0.01
1998	1	2	6.16	0.01
1999	1	2	3.23	0.01
2000	1	2	2.52	0.01
2001	1	2	10.34	0.01
2002	1	2	5.07	0.01
2003	1	2	0.13	0.01
2004	1	2	0.19	0.01
-9999 0 0 0 0
#
 #_CPUE_and_surveyabundance_observations
#_Units:  0=numbers; 1=biomass; 2=F; >=30 for special types
#_Errtype:  -1=normal; 0=lognormal; >0=T
#_SD_Report: 0=no sdreport; 1=enable sdreport
#_Fleet Units Errtype SD_Report
1 0 0 0 # F1
2 0 0 0 # F2
3 0 0 0 # F3
#_yr month fleet obs stderr
2007	6	3	1.828571429	0.776909106
2008	6	3	1.352941176	0.691898054
2009	6	3	2.911111111	0.767699669
2010	6	3	1.728395062	0.796146269
2011	6	3	0.987766831	0.489257902
2012	6	3	0.847149123	0.699084475
2013	6	3	1.301900585	0.651197524
2014	6	3	1.072474747	0.6115786
2015	6	3	1.227777778	0.501969897
2016	6	3	0.752873563	0.861185198
2017	6	3	1.184073291	0.676680181
2018	6	3	0.823700643	0.567208104
2019	6	3	0.725073314	0.579541639
2020	6	3	0.603764922	0.603434945
-9999 1 1 1 1 # terminator for survey observations 
#
0 #_N_fleets_with_discard
0 #_use meanbodysize_data (0/1)
#
# set up population length bin structure (note - irrelevant if not using size data and using empirical wtatage
2 # length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector
5 # binwidth for population size comp 
15 # minimum size in the population (lower edge of first bin and size at age 0.00) 
275 # maximum size in the population (lower edge of last bin)
1 # use length composition data (0/1)
#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.
#_addtocomp:  after accumulation of tails; this value added to all bins
#_males and females treated as combined gender below this bin number 
#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation
#_Comp_Error:  0=multinomial, 1=dirichlet
#_Comp_Error2:  parm number  for dirichlet
#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001
#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize
0 0.0001 0 0 0 0 0.001 #_fleet:1
0 0.0001 0 0 0 0 0.001 #_fleet:2
0 0.0001 0 0 0 0 0.001 #_survey:3

# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sex-length distribution
# partition codes:  (0=combined; 1=discard; 2=retained)
30 #_N_LengthBins; then enter lower edge of each length bin
130 135 140 145 150 155 160 165 170 175 180 185 190 195 200 205 210 215 220 225 230 235 240 245 250 255 260 265 270 275
#_yr month fleet sex part Nsamp datavector(female-male)
#Year	Month	Fleet	Sex	Partition	Nsamp	F_130	F_135	F_140	F_145	F_150	F_155	F_160	F_165	F_170	F_175	F_180	F_185	F_190	F_195	F_200	F_205	F_210	F_215	F_220	F_225	F_230	F_235	F_240	F_245	F_250	F_255	F_260	F_265	F_270	F_275	M_130	M_135	M_140	M_145	M_150	M_155	M_160	M_165	M_170	M_175	M_180	M_185	M_190	M_195	M_200	M_205	M_210	M_215	M_220	M_225	M_230	M_235	M_240	M_245	M_250	M_255	M_260	M_265	M_270	M_275
2007	6	1	1	2	15	0	0	0	0	0	0	0	0	0	0	5	0	0	5	1	0	4	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	1	0	0	1	0	0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
2008	6	1	1	2	84	0	0	0	0	1	0	0	1	3	0	8	11	12	12	13	4	8	5	1	2	2	0	1	0	0	0	0	0	0	0	0	0	0	0	1	0	6	13	8	9	11	7	6	0	0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0
2009	6	1	1	2	131	0	0	0	0	0	0	1	2	1	1	6	13	26	11	30	11	10	6	8	1	1	1	1	0	1	0	0	0	0	0	0	0	0	0	2	2	4	6	16	12	28	22	17	12	8	3	3	2	1	2	0	0	0	0	0	0	0	0	0	0
2010	6	1	1	2	99	0	0	0	0	0	0	0	1	1	1	4	8	12	10	15	11	7	9	6	8	1	3	1	1	0	0	0	0	0	0	0	0	0	1	1	2	5	8	6	10	14	8	8	6	1	3	2	1	0	0	1	0	0	0	0	0	0	0	0	0
2011	6	1	1	2	149	0	0	0	0	0	0	0	0	0	3	2	6	11	19	23	26	24	17	2	7	2	3	3	0	0	1	0	0	0	0	0	0	0	0	1	1	5	3	9	21	14	16	26	15	11	6	3	2	0	0	0	0	0	0	0	0	0	0	0	0
2012	6	1	1	2	128	0	0	0	0	0	0	0	0	3	1	4	8	8	11	17	19	20	11	9	5	4	2	1	3	1	1	0	0	0	0	0	0	0	1	0	1	2	0	10	18	18	21	30	14	8	2	0	0	1	0	0	0	0	0	0	0	0	0	0	0
2013	6	1	1	2	187	0	0	0	0	0	0	0	1	0	5	7	13	23	26	25	21	18	14	11	8	4	3	2	1	4	0	1	0	0	0	0	0	0	0	0	0	4	6	10	26	19	30	30	16	8	4	2	4	2	1	0	0	0	0	0	0	0	0	0	0
2014	6	1	1	2	177	0	0	0	0	0	0	0	1	1	0	5	8	19	18	26	23	18	15	10	11	7	5	3	2	3	1	1	0	0	0	0	0	0	0	1	0	5	3	10	24	28	41	22	18	12	5	1	1	0	0	0	0	0	0	0	0	0	0	0	0
2015	6	1	1	2	179	0	0	0	0	0	0	0	0	0	4	4	9	19	31	18	28	18	14	16	4	4	2	3	2	0	2	0	0	0	1	0	0	0	0	0	0	4	7	16	26	34	35	25	19	5	2	0	0	0	0	0	1	0	0	0	0	0	0	0	0
2016	6	1	1	2	172	0	0	0	0	0	0	0	1	0	3	8	6	17	21	25	21	14	13	13	12	6	3	4	2	1	0	0	1	1	0	0	0	0	3	2	2	4	6	11	20	24	16	12	9	6	5	1	0	0	1	0	0	0	0	0	0	0	0	0	0
2017	6	1	1	2	174	0	0	0	0	1	0	0	1	1	4	12	8	23	21	27	23	25	9	8	4	3	1	1	1	0	1	0	0	0	0	0	0	0	0	1	1	7	23	37	37	16	13	11	6	5	3	1	3	1	0	0	0	0	0	0	0	0	0	0	0
2018	6	1	1	2	173	0	0	0	0	0	0	0	0	5	5	9	14	20	16	18	23	15	14	14	5	8	4	3	0	0	0	0	0	0	0	1	0	0	2	2	0	15	22	24	33	25	20	10	5	6	4	1	0	1	0	0	0	0	0	0	0	0	0	0	0
2019	6	1	1	2	153	0	0	0	0	0	0	0	1	3	6	5	12	18	12	20	17	19	13	10	6	6	1	2	0	0	2	0	0	0	0	0	0	0	0	2	2	13	22	31	32	19	19	13	10	4	2	1	1	0	0	1	0	0	0	0	0	0	0	0	0
2020	6	1	1	2	90	0	0	0	0	0	0	0	2	0	1	5	7	5	11	15	10	11	8	3	4	3	2	3	0	0	0	0	0	0	0	0	0	2	0	1	5	12	14	19	20	12	9	6	5	1	0	0	1	0	0	0	0	0	0	0	0	0	0	0	0
2007	6	1	2	2	3	0	0	0	0	0	0	0	0	0	0	5	0	0	5	1	0	4	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	1	0	0	1	0	0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0
2008	6	1	2	2	62	0	0	0	0	1	0	0	1	3	0	8	11	12	12	13	4	8	5	1	2	2	0	1	0	0	0	0	0	0	0	0	0	0	0	1	0	6	13	8	9	11	7	6	0	0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0
2009	6	1	2	2	140	0	0	0	0	0	0	1	2	1	1	6	13	26	11	30	11	10	6	8	1	1	1	1	0	1	0	0	0	0	0	0	0	0	0	2	2	4	6	16	12	28	22	17	12	8	3	3	2	1	2	0	0	0	0	0	0	0	0	0	0
2010	6	1	2	2	77	0	0	0	0	0	0	0	1	1	1	4	8	12	10	15	11	7	9	6	8	1	3	1	1	0	0	0	0	0	0	0	0	0	1	1	2	5	8	6	10	14	8	8	6	1	3	2	1	0	0	1	0	0	0	0	0	0	0	0	0
2011	6	1	2	2	133	0	0	0	0	0	0	0	0	0	3	2	6	11	19	23	26	24	17	2	7	2	3	3	0	0	1	0	0	0	0	0	0	0	0	1	1	5	3	9	21	14	16	26	15	11	6	3	2	0	0	0	0	0	0	0	0	0	0	0	0
2012	6	1	2	2	126	0	0	0	0	0	0	0	0	3	1	4	8	8	11	17	19	20	11	9	5	4	2	1	3	1	1	0	0	0	0	0	0	0	1	0	1	2	0	10	18	18	21	30	14	8	2	0	0	1	0	0	0	0	0	0	0	0	0	0	0
2013	6	1	2	2	162	0	0	0	0	0	0	0	1	0	5	7	13	23	26	25	21	18	14	11	8	4	3	2	1	4	0	1	0	0	0	0	0	0	0	0	0	4	6	10	26	19	30	30	16	8	4	2	4	2	1	0	0	0	0	0	0	0	0	0	0
2014	6	1	2	2	171	0	0	0	0	0	0	0	1	1	0	5	8	19	18	26	23	18	15	10	11	7	5	3	2	3	1	1	0	0	0	0	0	0	0	1	0	5	3	10	24	28	41	22	18	12	5	1	1	0	0	0	0	0	0	0	0	0	0	0	0
2015	6	1	2	2	174	0	0	0	0	0	0	0	0	0	4	4	9	19	31	18	28	18	14	16	4	4	2	3	2	0	2	0	0	0	1	0	0	0	0	0	0	4	7	16	26	34	35	25	19	5	2	0	0	0	0	0	1	0	0	0	0	0	0	0	0
2016	6	1	2	2	122	0	0	0	0	0	0	0	1	0	3	8	6	17	21	25	21	14	13	13	12	6	3	4	2	1	0	0	1	1	0	0	0	0	3	2	2	4	6	11	20	24	16	12	9	6	5	1	0	0	1	0	0	0	0	0	0	0	0	0	0
2017	6	1	2	2	165	0	0	0	0	1	0	0	1	1	4	12	8	23	21	27	23	25	9	8	4	3	1	1	1	0	1	0	0	0	0	0	0	0	0	1	1	7	23	37	37	16	13	11	6	5	3	1	3	1	0	0	0	0	0	0	0	0	0	0	0
2018	6	1	2	2	171	0	0	0	0	0	0	0	0	5	5	9	14	20	16	18	23	15	14	14	5	8	4	3	0	0	0	0	0	0	0	1	0	0	2	2	0	15	22	24	33	25	20	10	5	6	4	1	0	1	0	0	0	0	0	0	0	0	0	0	0
2019	6	1	2	2	172	0	0	0	0	0	0	0	1	3	6	5	12	18	12	20	17	19	13	10	6	6	1	2	0	0	2	0	0	0	0	0	0	0	0	2	2	13	22	31	32	19	19	13	10	4	2	1	1	0	0	1	0	0	0	0	0	0	0	0	0
2020	6	1	2	2	107	0	0	0	0	0	0	0	2	0	1	5	7	5	11	15	10	11	8	3	4	3	2	3	0	0	0	0	0	0	0	0	0	2	0	1	5	12	14	19	20	12	9	6	5	1	0	0	1	0	0	0	0	0	0	0	0	0	0	0	0
-9999	6	1	2	2	107	0	0	0	0	0	0	0	2	0	1	5	7	5	11	15	10	11	8	3	4	3	2	3	0	0	0	0	0	0	0	0	0	2	0	1	5	12	14	19	20	12	9	6	5	1	0	0	1	0	0	0	0	0	0	0	0	0	0	0	0

#
0 #_N_age_bins

#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.
#_addtocomp:  after accumulation of tails; this value added to all bins
#_males and females treated as combined gender below this bin number 
#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation
#_Comp_Error:  0=multinomial, 1=dirichlet
#_Comp_Error2:  parm number  for dirichlet
#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001
#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize
#0 0.0001 1 0 0 0 1 #_fleet:1
#0 0.0001 1 0 0 0 1 #_fleet:2
#2 #_Lbin_method_for_Age_Data: 1=poplenbins; 2=datalenbins; 3=lengths
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
#_yr month fleet sex part ageerr Lbin_lo Lbin_hi Nsamp datavector(female-male)

#
0 #_Use_MeanSize-at-Age_obs (0/1)
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
# ageerr codes:  positive means mean length-at-age; negative means mean bodywt_at_age
#_yr month fleet sex part ageerr ignore datavector(female-male)
#                                          samplesize(female-male)
#
0 #_N_environ_variables
#Yr Variable Value
#
0 # N sizefreq methods to read 
#
0 # do tags (0/1)
#
0 #    morphcomp data(0/1) 
#  Nobs, Nmorphs, mincomp
#  yr, seas, type, partition, Nsamp, datavector_by_Nmorphs
#
0  #  Do dataread for selectivity priors(0/1)
# Yr, Seas, Fleet,  Age/Size,  Bin,  selex_prior,  prior_sd
# feature not yet implemented
#
999

ENDDATA
