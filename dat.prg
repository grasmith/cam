'PROGRAM: dat.prg  Copyright 2012,2015 Alphametrics Co.,Ltd.
'
' CAM version 6.0 FESSUD variant
'
' data preparation
'
' reads historical data from a spreadsheet (WD.XLS). The
' spreadsheet must provide data for variables referenced in
' this program and blocs defined in the settings file (set.prg)
'
' the program writes the workfile DAT.wf1
'
' updated: FC 01/04/2015
'
'==================================================================
' OPTIONS
'==================================================================
include "ztab"
include "set"call dat
'------------------------------------------------------------------
subroutine dat

%graphs = "No"
%markets = "No"
%tables = "No"
%analysis = "No"
%csv = "No"

%repstart = "1971"

'==================================================================
' PREFACE
'==================================================================
mode quiet
'--- create a workfile
%wkfile = "DAT"
wfcreate(wf={%wkfile}, page=data) a {%start} {%end}
pagecreate(page=graphs) a %start %end
pagecreate(page=tables) a %start %end
pageselect data
call pLog("DAT PROGRAM v3005")
'--- workfile settings
table t_Settings
t_Settings(1,1) = "Last actual observation"
t_Settings(1,2) = %latest
t_Settings(2,1) = "Last available observation"
t_Settings(2,2) = %latest
t_Settings(3,1) = "Series alias"
t_Settings(3,2) = ""
t_Settings(4,1) = "Title for graphs"
t_Settings(4,2) = "Historical data"
t_Settings(5,1) = "Comparison alias"
t_Settings(5,2) = ""
t_Settings(6,1) = "Comparison title"
t_Settings(6,2) = ""
t_Settings(7,1) = "Workfile"
t_Settings(7,2) = %wkfile
t_Settings(8,1) = "Scenario or shock info"
t_Settings(8,2) = ""
t_Settings(9,1) = "First observation in workfile"
t_Settings(9,2) = %start
t_Settings(10,1) = "Last observation in workfile"
t_Settings(10,2) = %end
t_Settings(11,1) = "List of exogenous variables"
t_Settings(11,2) = "DNNVF DNNVM DNNYF DNNYM NCP NOF NOM"
t_Settings(12,1) = "First-ever simulation year"
t_Settings(12,2) = ""
t_Settings(13,1) = "Current simulation start"
t_Settings(13,2) = ""

'--- bloc definitions
table t_Bloc
scalar nBloc = 0
call LoadTable(t_Bloc, nBloc, %blocs)
for !i = 1 to nBloc
  t_Bloc(!i, 3) = t_Bloc(!i, 1)
next
call ListCol(t_Bloc, 1, nBloc, 1, " ", %blocs)
pool p_Bloc {%blocs}

'--- world defined as sum of blocs
scalar nArea = nBloc
call LoadTable(t_Bloc, nArea, "W;World;" + %blocs)
scalar nWorld = nArea
'--- group definitions
call LoadTable(t_Bloc, nArea, %groups)
call ListCol(t_Bloc, nWorld + 1, nArea, 1, " ", %groups)
pool p_Group {%groups}
scalar nGroup = nArea - nWorld
'--- composite pools
pool p_BW {%blocs} W
pool p_GW {%groups} W
pool p_BGW {%blocs} {%groups} W

'--- table holding definitions of non-model series
'    including group and world totals
table t_Result
scalar nResult = 0

'==================================================================
' PROCESSING
'==================================================================

'--- load source data and prefix series names with wd_
smpl %start %end
call pLog("loading " + @str(!nSeries) + " series from WD.XLS")
pagecreate(page=WD) a %start %end
pageselect WD
read(s=Load,t,b2) WD.XLS !nSeries
pageselect data
copy WD\* wd_*
pagedelete WD

'--- source data adjustments
'--- rebase world price indexes: commodities and energy
wd_PXA = wd_PXA/@elem(wd_PXA, %base)
wd_PXE = wd_PXE/@elem(wd_PXE, %base)

'--- rebase expenditure, GDP, export and import volumes
p_Bloc.genr wd_AXD0? = wd_AXD0?*@elem(wd_AXD?, %base) _
  /@elem(wd_AXD0?, %base)
p_Bloc.genr wd_APT0? = wd_APT0?*@elem(wd_APT?, %base) _
  /@elem(wd_APT0?, %base)
p_Bloc.genr wd_AXX0? = wd_AXX0?*@elem(wd_AXX?, %base) _
  /@elem(wd_AXX0?, %base)
p_Bloc.genr wd_AXM0? = wd_AXM0?*@elem(wd_AXM?, %base) _
  /@elem(wd_AXM0?, %base)

'--- check world totals and base-year GDP identity
call BlocEval("W","wd_AXX0? wd_AXM0?","")
p_Bloc.genr wd_AXM0? = wd_AXM0?*wd_AXX0W/wd_AXM0W
p_Bloc.genr wd_APT0? = wd_AXD0? + wd_AXX0? - wd_AXM0?

'--- defining model variables
call pLog("defining model variables")

'============ 1: population, migration and employment

'--- population
p_Bloc.genr NCP_? = wd_NCT?     'child population
p_Bloc.genr NIM_? = wd_NIT?     'net migration
p_Bloc.genr NUR_? = wd_NUT?     'urban population
                          
p_Bloc.genr NOF_? = wd_NOF?     'female population 65+
p_Bloc.genr NOM_? = wd_NOM?     'male population 65+
p_Bloc.genr NYF_? = wd_NYF?     'female population 15-24
p_Bloc.genr NYM_? = wd_NYM?     'male population 15-24
p_Bloc.genr NY_? = NYF_?+NYM_?   'total population 15-24
p_Bloc.genr NVF_? = wd_NWF?+wd_NOF?-wd_NYF?
                                 'female population 25+ 
p_Bloc.genr NVM_? = wd_NWM?+wd_NOM?-wd_NYM?
                                 'male population 25+ 
p_Bloc.genr NV_? = NVF_?+NVM_?   'total population 25+
p_Bloc.genr NOP_? = NOF_?+NOM_?  'total population 65+
p_Bloc.genr NPF_? = NYF_?+NVF_?  'female population 15+
p_Bloc.genr NPM_? = NYM_?+NVM_?  'male population 15+
p_Bloc.genr NP_? = NPF_?+NPM_?   'total population 15+

'--- total population
p_Bloc.genr N_? = NCP_?+NP_?         'all ages
p_Bloc.genr NWP_? = N_?-NCP_?-NOP_?  '15-64
call BlocEval("W","N_? NP_? NV_? NOP_? NWP_?","")
call BlocEval("G","N_? NP_? NV_? NOP_? NWP_?","")

'--- group and world totals
call LoadTable(t_Result, nResult, _
  "GW;" _
  +  "NCP_? NOF_? NOM_? NPF_? NPM_? " _
  +  "NY_? NYF_? NYM_? NVF_? NVM_? " _
  +  "NIM_? NUR_? " _
)

'--- natural increase (assuming net migration split equally
'    between females and males and between young and adults)
p_Bloc.genr DNNYF_? = d(NYF_?) - NIM_?/4   'female 15-25
p_Bloc.genr DNNYM_? = d(NYM_?) - NIM_?/4   'male 15-25
p_Bloc.genr DNNVF_? = d(NVF_?) - NIM_?/4   'female 25+
p_Bloc.genr DNNVM_? = d(NVM_?) - NIM_?/4   'male 25+

'--- labour force, employment and unemployment
p_Bloc.genr NLF_? = wd_LLFF?       'female labour force
p_Bloc.genr NLM_? = wd_LLFM?       'male labour force 
p_Bloc.genr NLYF_? = wd_LLYF?      'female labour force 15–24
p_Bloc.genr NLYM_? = wd_LLYM?      'male labour force 15-24
p_Bloc.genr NLVF_? = NLF_?-NLYF_?  'female labour force 25+
p_Bloc.genr NLVM_? = NLM_?-NLYM_?  'male labour force 25+

p_Bloc.genr NUF_? = wd_LUEF?       'female unemployment
p_Bloc.genr NUM_? = wd_LUEM?       'male unemployment
p_Bloc.genr NUYF_? = wd_LUYF?      'female unemployment 15–24
p_Bloc.genr NUYM_? = wd_LUYM?      'male unemployment 15–24
p_Bloc.genr NUVF_? = NUF_?-NUYF_?  'female unemployment 25+
p_Bloc.genr NUVM_? = NUM_?-NUYM_?  'male unemployment 25+
 
p_Bloc.genr NEVF_? = NLVF_? - NUVF_?  'female employment 25+
p_Bloc.genr NEVM_? = NLVM_? - NUVM_?  'male employment 25+
p_Bloc.genr NEYF_? = NLYF_? - NUYF_?  'female employment 15-24
p_Bloc.genr NEYM_? = NLYM_? - NUYM_?  'male employment 15-24
p_Bloc.genr NEF_? = NEYF_? + NEVF_?   'total female employment
p_Bloc.genr NEM_? = NEYM_? + NEVM_?   'total male employment

'--- employment by broad sector
p_Bloc.genr NEAF_? = wd_LAGF?  'female employment in agriculture
p_Bloc.genr NEAM_? = wd_LAGM?  'male employment in agriculture
p_Bloc.genr NEIF_? = wd_LINF?  'female employment in industry
p_Bloc.genr NEIM_? = wd_LINM?  'male employment in industry
'--- non-agricultural employment
p_Bloc.genr NENF_? = NEF_?-NEAF_?     'female
p_Bloc.genr NENM_? = NEM_?-NEAM_?     'male 
'--- employment in services
p_Bloc.genr NESF_? = NENF_?-NEIF_?     'female
p_Bloc.genr NESM_? = NENM_?-NEIM_?     'male

'--- ratios by sex and age-group
p_Bloc.genr NLNYF_? = 100*NLYF_?/NYF_?  'participation
p_Bloc.genr NLNVF_? = 100*NLVF_?/NVF_?
p_Bloc.genr NLNYM_? = 100*NLYM_?/NYM_?
p_Bloc.genr NLNVM_? = 100*NLVM_?/NVM_?
p_Bloc.genr NULYF_? = 100*NUYF_?/NLYF_?  'unemployment
p_Bloc.genr NULVF_? = 100*NUVF_?/NLVF_?
p_Bloc.genr NULYM_? = 100*NUYM_?/NLYM_?
p_Bloc.genr NULVM_? = 100*NUVM_?/NLVM_?

'--- composition by sex
'--- agr as share of total
p_Bloc.genr NEAEF_? = 100*NEAF_?/NEF_?
p_Bloc.genr NEAEM_? = 100*NEAM_?/NEM_?
'--- ind as share of non-agr
p_Bloc.genr NEINF_? = 100*NEIF_?/NENF_?
p_Bloc.genr NEINM_? = 100*NEIM_?/NENM_?

'--- totals
p_Bloc.genr NL_? = NLF_?+NLM_?
p_Bloc.genr NLY_? = NLYF_?+NLYM_?
p_Bloc.genr NLV_? = NLVF_?+NLVM_?
p_Bloc.genr NU_? = NUF_?+NUM_?
p_Bloc.genr NUY_? = NUYF_?+NUYM_?
p_Bloc.genr NUV_? = NUVF_?+NUVM_?
p_Bloc.genr NE_? = NEF_?+NEM_?
p_Bloc.genr NEY_? = NEYF_?+NEYM_?
p_Bloc.genr NEV_? = NEVF_?+NEVM_?
p_Bloc.genr NEA_? = NEAF_?+NEAM_?
p_Bloc.genr NEI_? = NEIF_?+NEIM_?
p_Bloc.genr NES_? = NESF_?+NESM_?
p_Bloc.genr NEN_? = NEI_?+NES_?

'--- group and world totals
call LoadTable(t_Result, nResult, _
  "GW;" _
  +  "NL_? NLF_? NLM_? " _
  +  "NLY_? NLYF_? NLYM_? NLV_? NLVF_? NLVM_? " _
  +  "NLNVF_?=100*NLVF_?/NVF_? " _
  +  "NLNVM_?=100*NLVM_?/NVM_? " _
  +  "NLNYF_?=100*NLYF_?/NYF_? " _
  +  "NLNYM_?=100*NLYM_?/NYM_? " _
  +  "NU_? NUF_? NUM_? " _
  +  "NUY_? NUYF_? NUYM_? NUV_? NUVF_? NUVM_? " _
  +  "NULVF_?=100*NUVF_?/NLVF_? " _
  +  "NULVM_?=100*NUVM_?/NLVM_? " _
  +  "NULYF_?=100*NUYF_?/NLYF_? " _
  +  "NULYM_?=100*NUYM_?/NLYM_? " _
  +  "NE_? NEF_? NEM_? " _
  +  "NEY_? NEYF_? NEYM_? NEV_? NEVF_? NEVM_? " _
  +  "NEA_? NEAF_? NEAM_? NEI_? NEIF_? NEIM_? " _
  +  "NES_? NESF_? NESM_? NEN_? NENF_? NENM_? " _
  +  "NEAEF_?=100*NEAF_?/NEF_? " _
  +  "NEAEM_?=100*NEAM_?/NEM_? " _
  +  "NEINF_?=100*NEIF_?/NENF_? " _
  +  "NEINM_?=100*NEIM_?/NENM_? " _
)

'==== 2: price base, domestic expenditure, c/a and trade balance
'
'--- domestic expenditure and price indexes
'(blocs)
'  pp0 purchasing power parity adjustment = base-year APT0/APT1
'   where APT0 = constant price GDP and APT1 = PPP GDP
'  H   expenditure in PPP units = AXD0/pp0
'  ph  domestic expenditure deflator = AXD/H
'   where AXD0 = const price expenditure and AXD = current $ exp
'  rx  real exchange rate = ph/ph_w
'      where ph_w is a rebased world expenditure deflator
'(world)
'  H_W   world expenditure in PPP units = sum(H)
'  pp0w base-year PPP adjustment = base-year sum(ph.H)/sum(H)
'  ph_w  world expenditure deflator = [sum(ph.H)/sum(H)]/pp0_w
'(groups)
'  H   group expenditure in PPP units = sum(H)
'  ph  domestic expenditure deflator = sum(ph.H)/sum(H)
'  pp0 base-year PPP adjustment = base-year ph
'  rx  real exchange rate = ph/ph_w

'--- PPP adjustment: market price GDP/PPP GDP
p_Bloc.genr pp0_? = @elem(wd_APT0?,%base)/@elem(wd_APT1?,%base)

'--- expenditure in constant PPP units
p_Bloc.genr H_? = wd_AXD0?/pp0_?
call BlocEval("W","H_? wd_AXD?","")
'--- market price deflator: current dollars / constant PPP
p_BW.genr ph_? = wd_AXD?/H_?

'--- world price deflator rebased
scalar pp0w = @elem(ph_w, %base)
ph_w = ph_w/pp0w

'--- real exchange rate: own deflator / world deflator
p_Bloc.genr rx_? = ph_?/ph_w

'--- current account and trade balance
p_Bloc.genr X$_? = wd_AXX?/ph_w
p_Bloc.genr M$_? = wd_AXM?/ph_w
p_Bloc.genr X0_? = wd_AXX0?
p_Bloc.genr M0_? = wd_AXM0?
p_Bloc.genr TB$_? = X$_?-M$_?
p_Bloc.genr TB0_? = X0_?-M0_?
p_Bloc.genr TB_? = TB$_?/rx_?
p_Bloc.genr CA$_? = wd_BCA?/ph_w
p_Bloc.genr CA_? = CA$_?/rx_?
p_Bloc.genr tt_? = (H_?+TB_?)/(H_?+TB0_?/pp0_?)
p_Bloc.genr XIT$_? = (wd_BCI?+wd_BCT?)/ph_w
p_Bloc.genr MIT$_? = (wd_BDI?+wd_BDT?)/ph_w
p_Bloc.genr BIT$_? = CA$_?-TB$_?
p_Bloc.genr NIT$_? = @iif(XIT$_?<MIT$_?,XIT$_?,MIT$_?)

'--- explanatory variable for NITU$
call BlocEval("W", "X$_? ", "")

'--- group and world totals
' note: pp0_w rx_w and tt_w must be provided as result series
' to avoid problems when results are generated for all blocs and
' groups and the world as a whole (BGW)
call LoadTable(t_Result, nResult, _
  "W;" _
  + "pp0_?=pp0w rx_?=1 tt_w=1 " _
+ ":GW;" _
  + "X$_? M$_? X0_? M0_? TB$_? TB0_? TB_? CA$_? CA_? " _
  + "XIT$_? MIT$_? BIT$_? " _
  + "NIT$_?=@iif(XIT$_?<MIT$_?,XIT$_?,MIT$_?) " _
+ ":G;" _
  + "H_? " _
  + "ph_?(H_?) " _
  + "rx_?=ph_?/ph_w " _
  + "pp0_?=@elem(ph_?," + %base + ") " _
  + "tt_?=(H_?+TB_?)/(H_?+TB0_?/pp0_?) " _
)

'============ 3: income and GDP

p_Bloc.genr Y_? = H_? + CA$_?/rx_?
p_Bloc.genr Y$_? = Y_?*rx_?
p_Bloc.genr V0_? = wd_APT0?
p_Bloc.genr V_? = wd_APT0?/pp0_?
p_Bloc.genr VV$_? = H_?*rx_? + TB$_?
p_Bloc.genr VV_? = H_? + TB$_?/rx_?
p_Bloc.genr VVN_? = VV_?/N_?
p_Bloc.genr YN$_? = Y$_?*N_?

'--- GDP by broad sector
p_Bloc.genr VVA_? = wd_APA?/ph_?  'agriculture
p_Bloc.genr VVE_? = wd_APE?/ph_?  'extraction and utilities
p_Bloc.genr VVI_? = wd_APM?/ph_?  'industry
p_Bloc.genr VVS_? = VV_?-VVA_?-VVE_?-VVI_?  'services

'--- GDP by income type
p_Bloc.genr VVEM_? = (wd_AYW?+wd_AYM?)/ph_?
                                      'income from employment
p_Bloc.genr VVTX_? = wd_AYX?/ph_?     'indirect taxes less subsidies
p_Bloc.genr VVPR_? = VV_?-VVEM_?-VVTX_?  'profits and rent

p_Bloc.genr rtx_? = 100*VVTX_?/(VV_?-VVTX_?) 'indirect tax markup
p_Bloc.genr mu_? = 100*VVPR_?/VVEM_?         'profit markup

'--- labour share and average real earnings
p_Bloc.genr VVEMV_? = 100*VVEM_?/VV_?
p_Bloc.genr VVEME_? = VVEM_?/NE_?

'--- productive capacity
p_Bloc.genr VT_? = 1.05*@movav(V_?,6)*exp(0.3*(log(V_?/V_?(-6))))

call BlocEval("W", "Y_? V_? VV_? VV$_?", "")
call BlocEval("G", "VV_? VVN_?=VV_?/N_?", "")

p_BW.genr YN_? = Y_?/N_?
p_BW.genr VVN_? = VV_?/N_?
'--- relative income per capita
p_BW.genr YR_? = YN_? / YN_W

'--- group and world totals
call LoadTable(t_Result, nResult, _
  "G;" _
  + "Y_? V_? VV$_? " _
  + "YN_?=Y_?/N_? " _
  + "YR_?=YN_?/YN_W " _
+ ":GW;" _
  + "Y$_? V0_? VT_? " _
  + "VVA_? VVE_? VVI_? VVS_? " _
  + "VVEM_? VVPR_? VVTX_? " _
  + "VVEMV_?=100*VVEM_?/VV_? " _
  + "VVEME_?=VVEM_?/NE_? " _
  + "mu_?(VVEM_?) rtx_?(VV_?-VVTX_?) " _
  + "YN$_?=Y$_?/N_? " _
)

'======== 4: inflation, interest rates and nominal exchange rates
'NB tt = (1+(AXX-AXM)/AXD)/(1+(AXX0-AXM0)/AXD0)
'--- domestic cost and price inflation
p_Bloc.genr pvi_? = wd_FVI?  'cost inflation (GDP)
p_Bloc.genr pi_? = 100*((1+pvi_?/100)*tt_?(-1)/tt_?-1)
                           'price inflation (domestic expenditure)

p_Bloc.genr ei_? = 100*((1+pvi_?/100)/ _
 (((1+mu_?/100)/(1+mu_?(-1)/100)) _
 *((1+rtx_?/100)/(1+rtx_?(-1)/100)) _
 *(V_?(-1)*NE_?/(V_?*NE_?(-1))))-1)
                   'cost inflation (average earnings)

p_Bloc.genr spvi_? = log(-0.718 + 3.436*(1+pvi_?/100)/(2+pvi_?/100))

'--- valuation ratio for financial assets brought forward
p_Bloc.genr rpfa_? = 1/(1+spvi_?)

'--- generate group series here to allow rebasing of price indexes
call BlocEval("G", "H_? ph_?(H_?) V_? ", "")
call BlocEval("GW", "pi_?(H_?) pvi_?(V_?) ei_?(V_?) ", "")

'--- dollar price inflation
p_BGW.genr pi$_? = 100*(ph_?/ph_?(-1)-1)

'--- align US inflation estimates
smpl %start+1 %end
pi_us = pi$_us
smpl %start %end

'--- domestic cost and price indexes
p_BGW.genr pvd_? = exp(@cumsum(log(1+pvi_?/100)))
p_BGW.genr pvd_? = 100*pvd_?/@elem(pvd_?, %base)
p_BGW.genr phd_? = exp(@cumsum(log(1+pi_?/100)))
p_BGW.genr phd_? = 100*phd_?/@elem(phd_?, %base)

'--- nominal interest rates
p_Bloc.genr is_? = wd_FIS?         'short-term rate
p_Bloc.genr im_? = wd_FIM?         'bond yield

'--- real interest rates
p_Bloc.genr irs_? = 100*((1 + is_?/100)/(1 + pi_?/100) - 1)
p_Bloc.genr irm_? = 100*((1 + im_?/100)/(1 + pi_?/100) - 1)

'--- nominal exchange rate appreciation
p_BGW.genr rxna_? = 100*((1 + pi$_?/100)/(1 + pi_?/100) - 1)

'--- nominal exchange rate index
p_BGW.genr rxd_? = exp(@cumsum(log(1+rxna_?/100)))
p_BGW.genr rxd_? = 100*rxd_?/@elem(rxd_?, %base)

'--- unit cost of exports
' total costs = V.rx + M$
' sales volume = pp0*H + X0
' relative import content of exports vx = 2+(X0-M0)/V0
' import content of dom exp mh = M0/(pp0.H + vx.X0)
' import content of exports = mh.vx.X0
' import cost of exports = mh.vx.X0 M$/M0
' remaining content of exports = (1-mh.vx)X0
' domestic cost of exports = (1-mh.vx)X0 rx/pp0
' unit cost of exports = mh.vx.M$/M0+(1-mh.vx)*rx/pp0

'--- import content of domestic expenditure
p_Bloc.genr vx_? = 2+(X0_?-M0_?)/V0_?
p_Bloc.genr mh_? = M0_?/(pp0_?*H_?+vx_?*X0_?)

'--- unit cost of exports in international purchasing power units
p_Bloc.genr ucx$_? = mh_?*vx_?*M$_?/M0_? _
                    +(1-mh_?*vx_?)*rx_?/pp0_?
        
'--- group and world totals
call LoadTable(t_Result, nResult, _
  "GW;" _
  + "ei_?(V_?) " _
  + "pvi_?(V_?) pi_?(H_?) is_?(H_?) im_?(H_?) " _
  + "irs_?(H_?) irm_?(H_?) " _
  + "pvd_?=pvd_?(-1)*(1+pvi_?/100) " _
  + "spvi_?=log(-0.718+3.436*(1+pvi_?/100)/(2+pvi_?/100)) " _
  + "phd_?=phd_?(-1)*(1+pi_?/100) " _
  + "pi$_?=100*(ph_?/ph_?(-1)-1) " _
  + "rxna_?=100*((1+pi$_?/100)/(1+pi_?/100)-1) " _
  + "rxd_?=rxd_?(-1)*(1+rxna_?/100) " _
  + "ucx$_?(X0_?) " _
)

'============ 5: private expenditure and income
p_Bloc.genr C_? = wd_ACP?/ph_?              'consumer spending
p_Bloc.genr IP_? = wd_AKP?/ph_?             'fixed capital formation
p_Bloc.genr IV_? = wd_AVP?/ph_?             'change in inventories
p_Bloc.genr IPT_? = IP_? + IV_?             'total private investment 
p_Bloc.genr YP_? = wd_AYP?/ph_?               'disposable income
p_Bloc.genr SP_? = YP_? - C_?               'saving
p_Bloc.genr NLP_? = SP_? - IP_? - IV_?      'net lending

'--- world investment totals
call BlocEval("W", "IP_? IV_? ", "")

'--- group and world totals
call LoadTable(t_Result, nResult, _
  "G;" _
  + "IP_? IV_? " _
+ ":GW;" _
  + "C_? IPT_? YP_? SP_? NLP_? " _
)

'============ 6: government expenditure, income and debt
p_Bloc.genr NLG_? = CA_? - NLP_?       'net lending
p_Bloc.genr YG_? = wd_AYG?/ph_?        'net income
p_Bloc.genr G_? = YG_? - NLG_?         'expenditure on g&s
p_Bloc.genr YGD_? = YG_? - VVTX_?    'direct taxes less transfers
p_Bloc.genr LG_? = wd_FGD?/ph_?      'government debt
p_Bloc.genr LGF_? = wd_FGF?/ph_?     'government debt held by banks
p_Bloc.genr LGO_? = LG_? - LGF_?     'other government debt
p_Bloc.genr AGF_? = wd_FFG?/ph_?     'bank liabilities held by govt
p_Bloc.genr NGF_? = AGF_? - LG_?     'net identified assets
p_Bloc.genr NGI_? = @iif(AGF_?<LG_?,AGF_?,LG_?)  'covered debt

'--- flow of funds
p_Bloc.genr slgx_? = 1 - log(1+YR_?)/2   'share of fc debt
p_Bloc.genr rplgo_? = _
  slgx_?*ph_?(-1)/ph_? + (1-slgx_?)*rpfa_?   'valuation ratio for non-bank debt

p_Bloc.genr ILGO_? = LGO_?-LGO_?(-1)*rplgo_? 'debt acquisition by private sector
p_Bloc.genr ILGF_? = LGF_?-LGF_?(-1)*rpfa_?  'debt acquisition by banks
p_Bloc.genr ILG_? = ILGO_?+ILGF_?            'total acquisition of govt debt

'--- group and world totals
call LoadTable(t_Result, nResult, _
  "GW;" _
  + "G_? YG_? YGD_? NLG_? LG_? LGF_? LGO_? AGF_? NGF_? " _
  + "ILGO_? ILGF_? ILG_? NGI_?=@iif(AGF_?<LG_?,AGF_?,LG_?) " _
)

'============ 7: external position and banking system
'--- external position
p_Bloc.genr R$_? = wd_FXR?/ph_w        'reserves
p_Bloc.genr AXO$_? = wd_FXA?/ph_w      'other external assets
p_Bloc.genr LX$_? = wd_FXL?/ph_w       'external liabilities
p_Bloc.genr NX$_? = R$_?+AXO$_?-LX$_?  'external position
p_Bloc.genr NXF$_? = AXO$_?-LX$_?      'external pos excl reserves

'--- exchange reserves
p_Bloc.genr IR$_? = wd_BFI?/ph_w
p_Bloc.genr HR$_? = R$_?-IR$_?-R$_?(-1)
'--- other external assets (incl derivatives and unreported)
p_Bloc.genr IAXO$_? = _
   (wd_BDA?+wd_BPA?+wd_BOA?+wd_BFF?-wd_BEO?)/ph_w
p_Bloc.genr HAXO$_? = AXO$_?-IAXO$_?-AXO$_?(-1)
'--- external liabilities (incl net receipts on capital a/c)
p_Bloc.genr ILX$_? = IR$_?+IAXO$_?-CA$_?
p_Bloc.genr HLX$_? = LX$_?-ILX$_?-LX$_?(-1)
'--- direct investment, portfolio investment
'    and other flows
p_Bloc.genr IADI$_? = wd_BDA?/ph_w   'outward direct inv
p_Bloc.genr ILDI$_? = wd_BDL?/ph_w   'inward direct inv
p_Bloc.genr IAPI$_? = wd_BPA?/ph_w   'outward portfolio inv
p_Bloc.genr ILPI$_? = wd_BPL?/ph_w   'inward portfolio inv
p_Bloc.genr IAOI$_? = (wd_BOA?+wd_BFF?-wd_BEO?)/ph_w
                                     'other outward flows
p_Bloc.genr ILOI$_? = (wd_BOL?+wd_BKA?)/ph_w
                                     'other inward flows
'--- stocks
p_Bloc.genr ADI$_? = wd_FDA?/ph_w    'direct inv assets
p_Bloc.genr LDI$_? = wd_FDL?/ph_w    'direct inv liabilities
p_Bloc.genr API$_? = wd_FPA?/ph_w    'portfolio inv assets
p_Bloc.genr LPI$_? = wd_FPL?/ph_w    'portfolio inv liabilities
p_Bloc.genr AOI$_? = (wd_FOA?+wd_FFF?)/ph_w    'other inv assets
p_Bloc.genr LOI$_? = wd_FOL?/ph_w    'other inv liabilities

'--- holding gains on other assets and liabilities
p_Bloc.genr HAOI$_? = AOI$_?-AOI$_?(-1)-IAOI$_?
p_Bloc.genr HLOI$_? = LOI$_?-LOI$_?(-1)-ILOI$_?

'--- cash flow excl other assets and liabilities
p_Bloc.genr IOI$_? = CA$_?+ILDI$_?+ILPI$_?-IADI$_? _
  -IAPI$_?-IR$_?
'--- net position on other assets and liabilities
p_Bloc.genr NOF$_? = AOI$_?-LOI$_?
'--- covered positions
p_Bloc.genr NOI$_? = @iif(AOI$_?<LOI$_?,AOI$_?,LOI$_?)
p_Bloc.genr NXN$_? = _
            @iif(R$_?+AXO$_?<LX$_?,R$_?+AXO$_?,LX$_?)
                              
'--- bank deposits and loans
p_Bloc.genr DP_? = wd_FFP?/ph_?         'bank deposits and capital
p_Bloc.genr LN_? = (wd_FFG?+wd_FFP?-wd_FXR?-wd_FGF?)/ph_?
                                        'bank loans
p_Bloc.genr NFF_? = DP_? - LN_?                  'liquidity
p_Bloc.genr NFI_? = @iif(DP_?<LN_?,DP_?,LN_?)   'covered lending

'--- group and world totals
call LoadTable(t_Result, nResult, _
  "GW;" _
  + "R$_? HR$_? AXO$_? HAXO$_? LX$_? HLX$_? NX$_? NXF$_? " _
  + "NXN$_? IR$_? IAXO$_? ILX$_? " _
  + "IADI$_? ILDI$_? IAPI$_? ILPI$_? IAOI$_? ILOI$_? " _
  + "ADI$_? LDI$_? API$_? LPI$_? AOI$_? LOI$_? IOI$_? " _
  + "NOI$_?=@iif(AOI$_?<LOI$_?,AOI$_?,LOI$_?) " _
  + "DP_? LN_? NFF_? " _
  + "NFI_?=@iif(DP_?<LN_?,DP_?,LN_?)" _
)

'============ 8: write-offs, holding gains and domestic cash flows

'--- abnormal write-offs of bank loans
'    assumed zero in the historical period since data
'    are not available
smpl %start %end
p_Bloc.genr wln_? = 0
'--- proportion of loan write-offs absorbed by govt
p_Bloc.genr lnbail_? = 0.7

'--- holding gain on govt investment in banks
'    after deducting cost of loan write-offs

' NB: HAGF = AGF-AGF(-1)-IAGF       (accounting def.)
'     AGF = R$/rx + LN + LGF - DP   (bank sector balance sheet)
' implying  HAGF = R$/rx + LN + LGF - DP - AGF(-1) - IAGF
' Now express this in terms of changes in the rhs variables:
'     R$/rx = R$(-1)/rx(-1) + HR$/rx + IR$/rx
'     LGF = rpfa LGF(-1) + ILGF
'     LN = (1-wln)rpfa LN(-1) + ILN
'     DP = rpfa DP(-1) - (1-lnbail)wln rpfa LN(-1) + IDP
' The latter equation specifies the proportion of losses arising
' from abnormal write-offs that is taken by non-government holdings
' of banking sector equity.
' Note also that
'     IAGF = IR$/rx + ILGF + ILN - IDP
' These definitions allow us to derive the holding gain on
' government investment in banks as a function of valuation changes
' and abnormal write-offs:  
'     HAGF =  R$(-1)/rx(-1) + HR$/rx + rpfa*(LGF(-1)+LN(-1)-DP(-1))
'             - lnbail*wln*rpfa*LN(-1) - AGF(-1)

p_Bloc.genr HAGF_? = R$_?(-1)/rx_? + HR$_?/rx_? _
   + rpfa_?*(LN_?(-1)+LGF_?(-1)-DP_?(-1)) _
   - lnbail_?*wln_?*rpfa_?*LN_?(-1) - AGF_?(-1)

'--- cash transactions related to government assets
p_Bloc.genr IAGF_? = AGF_?-AGF_?(-1)-HAGF_? 'investment in banks
p_Bloc.genr IAGO_? = NLG_?-IAGF_?+ILG_?     'other investment
p_Bloc.genr IAG_? = IAGO_?+IAGF_?           'total investment
  
'--- loans and deposits
'--- weighted average write-off (impact on outstanding debt)
'    assumes 2 year average lag
smpl %start %start
p_Bloc.genr WLNA_? = LN_?*wln_?
smpl %start+1 %end
for !i = 1 to nBloc
  %b = t_Bloc(1, !i)
p_Bloc.genr WLNA_? = 0.8*LN_?(-1)*wln_?*rpfa_? + 0.2*WLNA_?(-1)
next

smpl %start %end
p_Bloc.genr ILN_? = LN_? - LN_?(-1)*rpfa_?*(1-wln_?)
p_Bloc.genr IDP_? = ILN_?-ILGO_?+IAGO_? - (IAXO$_?-ILX$_?)/rx_? + NLP_?

'--- holding gains and write-offs
p_Bloc.genr HDP_? = DP_? - DP_?(-1) - IDP_?
p_Bloc.genr HLN_? = LN_? - LN_?(-1) - ILN_?
p_Bloc.genr HLGO_? = LGO_? - LGO_?(-1) - ILGO_?
p_Bloc.genr HAXO_? = AXO$_?/rx_? - AXO$_?(-1)/rx_?(-1) - IAXO$_?/rx_?
p_Bloc.genr HLX_? = LX$_?/rx_? - LX$_?(-1)/rx_?(-1) - ILX$_?/rx_?

'--- group and world totals
call LoadTable(t_Result, nResult, _
  "GW;" _
  + "HAGF_? IAGF_? IAGO_? IAG_? WLNA_? ILN_? IDP_? " _
  + "HDP_? HLN_? HLGO_? HAXO_? HLX_? " _
)

'================ 9: capital stock and wealth

'--- produced capital stock
scalar rdp = 0.05                           'depreciation rate
smpl %start %start
p_Bloc.genr KI_? = 2.5*V_?
p_Bloc.genr KID_? = rdp*KI_?
smpl %start+1 %end
p_Bloc.genr KI_? = (1-rdp)*KI_?(-1) + IP_? + IV_?
p_Bloc.genr KID_? = rdp*KI_?(-1)
smpl %start %end

'--- private income net of capital consumption
p_Bloc.genr YPN_? = YP_? - KID_?

'--- default wealth estimate
'    constant capital/GDP valuation + 50% of
'    net private financial assets
p_Bloc.genr WP_? = 4*VV_?+(LGO_?+NFF_?+NXF$_?/rx_?)/2

'--- holding gains and write-offs
p_Bloc.genr HWP_? = WP_? - WP_?(-1) - SP_? + KID_? - IAGO_?

'--- value of capital and real asset price
p_Bloc.genr KP_? = WP_? - (LGO_? + NFF_? + NXF$_?/rx_?)
p_Bloc.genr pkp_? = KP_?/KI_?
p_Bloc.genr rpkp_? = pkp_?/pkp_?(-1)
p_Bloc.genr HKP_? = KP_? - KP_?(-1) - KI_? + KI_?(-1)

'--- group and world totals
call LoadTable(t_Result, nResult, _
  "GW;" _
  + "KI_? KID_? YPN_? WP_? HWP_? KP_? HKP_? " _
  + "pkp_?=KP_?/KI_? " _
  + "rpkp_?=pkp_?/pkp_?(-1) " _
)

'============ 10: trade by commodity group

'--- commodity prices relative to world exp deflator
series pa_w = wd_PXA/ph_w

'--- lagged terms of trade for primary commodities
smpl %start %start
p_Bloc.genr lpa_? = log(pa_w/rx_?)
smpl %start+1 %end
p_Bloc.genr lpa_? = 0.3*log(pa_w/rx_?) + 0.7*lpa_?(-1)
smpl %start %end

p_Bloc.genr XA$_? = wd_TXA?/ph_w
p_Bloc.genr MA$_? = wd_TMA?/ph_w
p_Bloc.genr XE$_? = wd_TXE?/ph_w
call BlocMinVal("B", "XE$ 0.001")
p_Bloc.genr ME$_? = wd_TME?/ph_w
p_Bloc.genr XM$_? = wd_TXM?/ph_w
p_Bloc.genr MM$_? = wd_TMM?/ph_w
p_Bloc.genr XS$_? = wd_BXS?/ph_w
p_Bloc.genr MS$_? = wd_BMS?/ph_w

'--- demand for imported manufactures
p_Bloc.genr MMH_? = C_?+0.4*G_?+2*(IP_?+IV_?) _
  +(X$_?+2*XM$_?)/rx_?

'--- constant price commodity breakdown
'--- initial values
p_Bloc.genr XA0U_? = wd_TXA?/wd_PXA
p_Bloc.genr MA0_? = wd_TMA?/wd_PXA
p_Bloc.genr XE0U_? = wd_TXE?/wd_PXE
p_Bloc.genr ME0_? = wd_TME?/wd_PXE
p_Bloc.genr XM0U_? = XM$_?
p_Bloc.genr MM0_? = MM$_?
p_Bloc.genr XS0U_? = XS$_?
p_Bloc.genr MS0_? = MS$_?

'--- RAS iteration to match bloc and world totals
'--- bloc scale factor for imports
'    initial world scale factor for exports
'    initial adjusted values
p_Bloc.genr rsfM0_? = M0_?/(MA0_?+ME0_?+MM0_?+MS0_?)
for %a A E M S
  p_Bloc.genr M{%a}0_? = M{%a}0_?*rsfM0_?
  call BlocEval("W","M"+%a+"0_?","")
  series rsfX{%a}0_W = 1
  p_Bloc.genr X{%a}0_? = X{%a}0U_?
next
'--- loop to adjust exports
for !i = 1 to 10
  p_Bloc.genr rsfX0_? = X0_?/(XA0_?+ XE0_?+XM0_?+XS0_?)
  for %a A E M S
    p_Bloc.genr X{%a}0_? = X{%a}0_?*rsfX0_?
    call BlocEval("W","X"+%a+"0_?","")
    rsfX{%a}0_W = M{%a}0_W/X{%a}0_W
    %g = @trim(@str(!i))
    series rsfX{%a}0_W{%g} = rsfX{%a}0_W
    p_Bloc.genr X{%a}0_? = X{%a}0_?*rsfX{%a}0_W
    '--- final adjustment factors
    if !i = 10 then
      p_Bloc.genr rsfX{%a}0_? = X{%a}0_?/X{%a}0U_?
    endif  
  next
next

delete rsfM0_* rsfX?0_* X?0U_*

'--- group and world totals
call LoadTable(t_Result, nResult, _
  "GW;" _
  + "XA$_? MA$_? XE$_? ME$_? XM$_? MM$_? XS$_? MS$_? " _
  + "XA0_? MA0_? XE0_? ME0_? XM0_? MM0_? XS0_? MS0_? " _
)

'--- manufactures: market shares and intra trade
for !i = 1 to nBloc
  %b = t_Bloc(!i, 1)
  %s = "wd_TPM" + %b + %b
  '--- bloc market shares
  for !j = 1 to nBloc
    %p = t_Bloc(!j, 1)
    %s = "wd_TPM" + %b + %p
    if @isobject(%s) then
      series sxm_{%b}_{%p} = @nan({%s},10)/(ph_w*MM$_{%p})
    endif
  next
next

'--- weighted average supply price for imports of manufactures
' compute as inverse weighted average of inverse supplier prices
'  pmm0 = 1/sum(sxm.xm0/xm$)
p_Bloc.genr pmm0_? = 0
for !i = 1 to nBloc
  %b = t_Bloc(!i, 1)
  for !j = 1 to nBloc
    %p = t_Bloc(!j, 1)
    %s = "sxm_" + %p + "_" + %b
    if @isobject(%s) then
      pmm0_{%b} = pmm0_{%b} + {%s}*XM0_{%p}/XM$_{%p}
    endif
  next
  pmm0_{%b} = 1/pmm0_{%b}
next

'============ 11: energy production and trade

'--- sum over 4 fuel types: primary electricity, gas, liquids, solids
p_Bloc.genr EP_? = 0
p_Bloc.genr EX_? = 0
p_Bloc.genr EM_? = 0

!c = 2                  'higher weight for primary electricity
'--- non-carbon production (primary electricity)
p_Bloc.genr EPN_? = !c*wd_EPE?
'--- primary electricity exports
p_Bloc.genr EPNX_? = !c*wd_EXE?
p_Bloc.genr EPNX_? = @iif(EPNX_? > EPN_?, EPN_?, EPNX_?)
'--- domestic non-carbon energy
p_Bloc.genr EPNN_? = EPN_?-EPNX_?
for !j = 1 to 4
  %f = @mid("EGLS", !j, 1)
  p_Bloc.genr EP_? = EP_? + !c*wd_EP{%f}?
  p_Bloc.genr EX_? = EX_? + !c*wd_EX{%f}?
  p_Bloc.genr EM_? = EM_? + !c*wd_EM{%f}?
  !c = 1
next
call BlocMinVal("B", "EP 0.1, EPN 0.001, EX 0.001")

'--- carbon energy production
p_Bloc.genr EPC_? = EP_? - EPN_?

'--- CO2 emissions with group and world totals as policy target
p_Bloc.genr CO2_? = wd_IHCE?
call BlocEval("GW", "CO2_? ", "")

'--- adjust imports for world balance
call BlocEval("W", "EP_? EX_? EM_? ED_?=EP_?", "")
p_Bloc.genr EM_? = EM_?*EX_W/EM_W
p_Bloc.genr ED_? = EP_?+EM_?-EX_?

'--- group and world totals
call LoadTable(t_Result, nResult, _
  "GW;ED_? EP_? EPC_? EPN_? EPNX_? EPNN_? EX_? EM_? ")

'--- oil price relative to world exp deflator
call BlocEval("W","pe_?=wd_PXE/ph_?","")
'--- domestic energy prices
p_Bloc.genr pep_? = 400*pe_w/rx_?  'producer price per toe
p_Bloc.genr ttco2_? = 0            'tax on CO2 emissions per ton
p_Bloc.genr ped_? = 800*rx_?*EPNN_?/ED_? _
  + pep_?*(1-EPNN_?/ED_?) + ttco2_?*CO2_?/ED_?
                                   'user price per toe
p_Bloc.genr pepc_? = pep_?+ttco2_?*CO2_?/(ED_?-EPNN_?)
                                   'cost of domestic carbon energy
'--- oil price ceiling
scalar pepmax = 1600
'--- lagged energy prices and carbon tax
smpl %start %start
p_Bloc.genr lpep_? = log(pep_?*rx_?/(pepmax-400*pe_w))
p_Bloc.genr lped_? = log(ped_?*rx_?/(pepmax-400*pe_w))
p_Bloc.genr lpepc_? = log(pepc_?*rx_?/(pepmax-400*pe_w))
p_Bloc.genr lttco2_? = log(1+ttco2_?/pepmax)
smpl %start+1 %end
p_Bloc.genr lpep_? = 0.15*log(pep_?*rx_?/(pepmax-400*pe_w)) _
 + 0.85*lpep_?(-1)
p_Bloc.genr lped_? = 0.3*log(ped_?*rx_?/(pepmax-400*pe_w)) _
 + 0.7*lped_?(-1)
p_Bloc.genr lpepc_? = 0.05*log(pepc_?*rx_?/(pepmax-400*pe_w)) _
 + 0.95*lpepc_?(-1)
p_Bloc.genr lttco2_? = 0.05*log(1+ttco2_?/pepmax) _
 + 0.95*lttco2_?(-1)
smpl %start %end

'============ 12: well-being and other indicators
p_Bloc.genr JIM_? = wd_IHIM?  'infant mortality rate per 1000
p_Bloc.genr JLX_? = wd_IHLX?  'life expectancy at birth (years)
p_Bloc.genr JGN_? = wd_IYGN?  'internal Gini index
p_bloc.genr JLA_? = wd_IALA?  'land area (sq km)

call LoadTable(t_Result, nResult, _
  "GW;JIM_?(NCP_?) JLX_?(NCP_?) JGN_?(N_?) " _
  + "JLA_? " _
)

'============ delete source series

delete wd_*

'============ model extension
call pLog("extended model series")
table t_ModelX
scalar nModelX = 0
call LoadTable(t_ModelX, nModelX, _
  "W;pa$_?=pa_?*ph_? " _
    + "pe$_?=pe_?*ph_? " _
+ ":B;_Y_?=Y$_?*ph_w " _
  + "_BIT_?=BIT$_?*ph_w " _
  + "_VV_?=_Y_?-_BIT_? " _
  + "_CA_?=CA$_?*ph_w " _
  + "_M_?=M$_?*ph_w " _
  + "BA0_?=XA0_?-MA0_? " _
  + "BA$_?=XA$_?-MA$_? " _
  + "BE0_?=XE0_?-ME0_? " _
  + "BE$_?=XE$_?-ME$_? " _
  + "BM0_?=XM0_?-MM0_? " _
  + "BM$_?=XM$_?-MM$_? " _
  + "BS0_?=XS0_?-MS0_? " _
  + "BS$_?=XS$_?-MS$_? " _
  + "EB_?=EX_?-EM_? " _
  + "HNX$_?=NX$_?-CA$_?-NX$_?(-1) " _
  + "HNX_?=(NX$_?-CA$_?)/rx_?-NX$_?(-1)/rx_?(-1) " _
)

'--- historical values
for !i = 1 to nModelX
  call BlocEval(t_ModelX(!i, 1), t_ModelX(!i, 2), "")
next

'--- group and world totals
call LoadTable(t_Result, nResult, _
  "GW;" _
  + "BA$_? BE$_? BM$_? BS$_? BA0_? BE0_? BM0_? BS0_? " _
  + "EB_? HNX$_? HNX_? " _
)

call pLog("defining additional result series")

'============ post-solution analysis

'====== NB MODEL VARIABLES NOT CALCULATED FOR GROUPS
' rpfa vx mh slgx rplgo
' wln lnbail lpa lped lpep lpepc sxm pmm0

'--- Theil indexes: list of variables
table t_TVar
scalar nTVar = 0

call LoadTable(t_TVar, nTVar, _
  "Y;income:" _
  + "G;govt expenditure:" _
  + "ED;energy use:" _
  + "EP;energy production:" _
  + "CO2;CO2 emissions:" _
  + "XM$;manufactured exports:" _
  + "XS$;service exports" _
  )

'--- world prices
call LoadTable(t_Result, nResult, _
  "W;" _
  + "pm_?=M$_?/M0_? " _
  + "pm$_?=pm_?*ph_? " _
  + "pmm_?=MM$_?/MM0_? " _
  + "pmm$_?=pmm_?*ph_? " _
  + "pms_?=MS$_?/MS0_? " _
  + "pms$_?=pms_?*ph_? " _
  + "pkp$_?=pkp_?*ph_? " _
  + "pikp_?=@pc(pkp_?) " _
  + "pikp$_?=@pc(pkp$_?) " _
  )

'--- ratios to GDP
call LoadTable(t_Result, nResult, _
  "BGW;" _
  + "AGFV_?=100*AGF_?/VV_? " _
  + "AX$_?=AXO$_?+R$_? " _
  + "AXV$_?=100*AX$_?/VV$_? " _
  + "BAV$_?=100*BA$_?/VV$_? " _
  + "BEV$_?=100*BE$_?/VV$_? " _
  + "BITV$_?=100*BIT$_?/VV$_? " _
  + "BMV$_?=100*BM$_?/VV$_? " _
  + "BSV$_?=100*BS$_?/VV$_? " _
  + "CAV$_?=100*CA$_?/VV$_? " _
  + "CO2V_?=1000*CO2_?/VV_? " _
  + "CV_?=100*C_?/VV_? " _
  + "DPV_?=100*DP_?/VV_? " _
  + "EBV_?=1000*EB_?/VV_? " _
  + "EDV_?=1000*ED_?/VV_? " _
  + "EPV_?=1000*EP_?/VV_? " _
  + "EPND_?=100*EPN_?/ED_? " _
  + "EPNP_?=100*EPN_?/EP_? " _
  + "GV_?=100*G_?/VV_?" _
)
call LoadTable(t_Result, nResult, _
  "BGW;" _
  + "HAGFV_?=100*HAGF_?/VV_? " _
  + "HAXOV$_?=100*HAXO$_?/VV$_? " _
  + "HDPV_?=100*HDP_?/VV_? " _
  + "HKPV_?=100*HKP_?/VV_? " _
  + "HLXV$_?=100*HLX$_?/VV$_? " _
  + "HNXV_?=100*HNX_?/VV_? " _
  + "HV_?=100*H_?/VV_? " _
  + "HWPV_?=100*HWP_?/VV_? " _
  + "IAGOV_?=100*IAGO_?/VV_? " _
  + "IAGOCV_?=100*@cumsum(IAGO_?)/VV_? " _
  + "IPTV_?=100*IPT_?/VV_? " _
  + "IPV_?=100*IP_?/VV_? " _
  + "IVV_?=100*IV_?/VV_? " _
  + "KPV_?=100*KP_?/VV_? " _
  + "KIV_?=100*KI_?/VV_? " _
  + "LGOV_?=100*LGO_?/VV_? " _
  + "LGV_?=100*LG_?/VV_? " _
  + "LNV_?=100*LN_?/VV_? " _
  + "LXV$_?=100*LX$_?/VV$_? " _
  + "LYR_?=@nan(log(YR_?),0) " _
  + "MAV$_?=100*MA$_?/VV$_? " _
  + "MEV$_?=100*ME$_?/VV$_? " _
  + "MITV$_?=100*MIT$_?/VV$_? " _
  + "MMV$_?=100*MM$_?/VV$_? " _
  + "MSV$_?=100*MS$_?/VV$_? " _
  + "MV$_?=100*M$_?/VV$_?" _
  )
call LoadTable(t_Result, nResult, _
  "BGW;" _
  + "NFAV_?=100*(LG_?+NX$_?/rx_?)/VV_? " _
  + "NLGV_?=100*NLG_?/VV_? " _
  + "NLPV_?=100*NLP_?/VV_? " _
  + "NXV$_?=100*NX$_?/VV$_? " _
  + "pmm_?=MM$_?/MM0_? " _
  + "pxm_?=XM$_?/XM0_? " _
  + "RALX$_?=100*AX$_?/LX$_? " _
  + "RMX$_?=100*R$_?/M$_? " _
  + "SPV_?=100*SP_?/VV_? " _
  + "SXMM_?=100*XM$_?/MM$_W " _
  + "TBV$_?=100*TB$_?/VV$_?" _
  )
call LoadTable(t_Result, nResult, _
  "BGW;" _
  + "ulc$_?=rx_?*VVEM_?*pp0_?/v0_? " _
  + "uva$_?=rx_?*(VVEM_?+VVPR_?)*pp0_?/v0_? " _
  + "VVT_?=100*(V_?/VT_?-1) " _
  + "WPV_?=100*WP_?/VV_? " _
  + "XAV$_?=100*XA$_?/VV$_? " _
  + "XEV$_?=100*XE$_?/VV$_? " _
  + "XITV$_?=100*XIT$_?/VV$_? " _
  + "XMV$_?=100*XM$_?/VV$_? " _
  + "XSV$_?=100*XS$_?/VV$_? " _
  + "XV$_?=100*X$_?/VV$_? " _
  + "YGV_?=100*YG_?/VV_? " _
  + "YGDV_?=100*YGD_?/VV_? " _
  )
call LoadTable(t_Result, nResult, _
  "BGW;" _
  + "ADIV$_?=100*ADI$_?/VV$_? " _
  + "APIV$_?=100*API$_?/VV$_? " _
  + "AOIV$_?=100*AOI$_?/VV$_? " _
  + "LDIV$_?=100*LDI$_?/VV$_? " _
  + "LPIV$_?=100*LPI$_?/VV$_? " _
  + "LOIV$_?=100*LOI$_?/VV$_? " _
)

'--- other policy indicators
'    JLAN land area per person (hectares)
'    VGA  agriculture productivity gap as % of GDP
'    NNE  economic dependency ratio (% of employed population)
'    GSS  government service standard
'--- external assets and liabilities as % of domestic deposits
'    rlx  liabilities
'    rr   official reserves
'    rax  assets 
'    raxo other assets (excl official reserves)
call LoadTable(t_Result, nResult, _
  "BGW;" _
  + "JLAN_?=0.0001*JLA_?/n_? " _
  + "VGA_?=100*(NEA_?/NE_?-VVA_?/VV_?)/(1-NEA_?/NE_?) " _
  + "NNE_?=100*(N_?/NE_?-1) " _
  + "GSS_?=100*(G_?/(N_?-0.5*NV_?))/VVN_? " _
  + "rlx_?=100*LX$_?/(rx_?*DP_?) " _
  + "rr_?=100*R$_?/(rx_?*DP_?) " _
  + "rax_?=100*AX$_?/(rx_?*DP_?) " _
  + "raxo_?=100*AXO$_?/(rx_?*DP_?) " _
  )

'--- per capita results
call LoadTable(t_Result, nResult, _
  "BGW;" _
  + "CO2N_?=CO2_?/N_? " _
  + "VN_?=V_?/N_? " _
  + "VNE_?=V_?/NE_? " _
  + "VVAE_?=VVA_?/NEA_? " _
  + "VVIE_?=VVI_?/NEI_? " _
  + "VVSE_?=VVS_?/NES_? " _
  + "VN0_?=V0_?/N_? " _
  + "VVN$_?=VV$_?/N_? " _
  + "YN$_?=Y$_?/N_? " _
  )

'--- growth rates
call LoadTable(t_Result, nResult, _
  "BGW;" _
  + "DEP_?=@pc(EP_?) " _
  + "DEPN_?=@pc(EPN_?) " _
  + "DIPT_?=@pc(IPT_?) " _
  + "DV0_?=@pc(V0_?) " _
  + "DVNE_?=@pc(VNE_?) " _
  + "DVV_?=@pc(VV_?) " _
  + "DVV5_?=100*(exp(log(VV_?/VV_?(-5))/5)-1) " _
  + "DVVN_?=@pc(VVN_?) " _
  + "DX$_?=@pc(X$_?) " _
  + "DXM$_?=@pc(XM$_?) " _
  + "DY_?=@pc(Y_?) " _
  + "DYN_?=@pc(YN_?) " _
 + ":W;DCO2_?=@pc(CO2_?) " _
  )

'--- income and exports per person employed
call LoadTable(t_Result, nResult, _
  "BGW;" _
  + "VVNE_?=VV_?/NE_? " _
  + "YNE_?=Y_?/NE_? " _
  + "XNE_?=X$_?/(rx_?*NE_?) " _
  + "YXNE_?=YNE_?-XNE_? " _
  + "YX_?=YNE_?/XNE_? " _
  + "XAX_?=100*XA$_?/X$_? " _
  + "XEX_?=100*XE$_?/X$_? " _
  + "XAEX_?=XAX_?+XEX_? " _
  + "XMX_?=100*XM$_?/X$_? " _
  + "XSX_?=100*XS$_?/X$_? " _
  + "XNR_?=100*X$_?*N_W/(X$_W*N_?) " _
  )

'--- household income, investment and profits,
'    dependency, rural population
call LoadTable(t_Result, nResult, _
  "BGW;" _
  + "CN_?=C_?/N_? " _
  + "CNR_?=100*CN_?*N_W/C_W " _
  + "NEDR_?=100*N_?/NE_? " _
  + "IIY_?=100*(IP_?+IV_?)/VV_? " _
  + "NDEP_?=100*(1-(NVM_?+NVF_?)/N_?) " _
  + "NRUN_?=100*(1-NUR_?/N_?) " _
  )

'--- population structure and employment
call LoadTable(t_Result, nResult, _
  "BGW;" _
  + "NCPN_?=100*NCP_?/N_? " _
  + "NEAE_?=100*NEA_?/NE_? " _
  + "NEFE_?=100*NEF_?/NE_? " _
  + "NEIE_?=100*NEI_?/NE_? " _
  + "NER_?=100*NE_?/NP_? " _
  + "NESE_?=100*NES_?/NE_? " _
  + "NERVF_?=100*NEVF_?/NVF_? " _
  + "NERVM_?=100*NEVM_?/NVM_? " _
  + "NERYF_?=100*NEYF_?/NYF_? " _
  + "NERYM_?=100*NEYM_?/NYM_? " _
  + "NIME_?=100*NIM_?/NE_? " _
  + "NLN_?=100*NL_?/NP_? " _
  + "NOPN_?=100*NOP_?/N_? " _
  + "NUL_?=100*NU_?/NL_? " _
  + "NULF_?=100*NUF_?/NLF_? " _
  + "NULM_?=100*NUM_?/NLM_? " _
  + "NURN_?=100*NUR_?/N_? " _
  + "NWPN_?=100*NWP_?/N_? " _
  + "NYN_?=100*(NYF_?+NYM_?)/N_? " _
)

'--- share of GDP by broad sector and income type
call LoadTable(t_Result, nResult, _
  "BGW;" _
  + "VVAV_?=100*VVA_?/VV_? " _
  + "VVEV_?=100*VVE_?/VV_? " _
  + "VVIV_?=100*VVI_?/VV_? " _
  + "VVSV_?=100*VVS_?/VV_? " _
  + "VVPRV_?=100*VVPR_?/VV_? " _
  + "VVTXV_?=100*VVTX_?/VV_? " _
  )


'--- current dollar domestic income and spending
%tlv = "Y;VV;H;YG;G;NLG;AGF;LG;LGO;LN;DP;YP;SP;C;IP;IV;IPT;NLP"
%t = "BGW;"
while %tlv <> ""
  call Token(%tlv, ";", %v)
  %t = %t + "_" + %v + "_?=ph_?*" + %v +"_? "
wend
call LoadTable(t_Result, nResult, %t)

'--- current dollar international transactions
%tlv = "R;CA;BIT;XIT;MIT;TB;X;M;BA;XA;MA;BE;XE;ME;BM;XM;MM;BS;XS;MS"
%t = "BGW;"
while %tlv <> ""
  call Token(%tlv, ";", %v)
  %t = %t + "_" + %v + "_?=ph_w*" + %v +"$_? "
wend
call LoadTable(t_Result, nResult, %t)

'--- current dollar GDP
call LoadTable(t_Result, nResult, "BGW;_V_?=_Y_?-_BIT_?")

'--- compute Gini, Theil and result series
call DerivedSeries(%start, %end, "")

'============ special tables of calculated values
call pLog("tables of parameters inferred from historical data")
smpl %start %latest

'--- capital stock as ratio to income
table(2,4) t
setcell(t,2,1, _
  "Estimated capital stock relative to GDP " _
  + %start + "-" + %latest,"c")
t.setmerge(r2c1:r2c4)
t.setwidth(1) 24
t.setwidth(2:4) 10
setcell(t,4,1,"Bloc","c")
setcell(t,4,2,"Minimum","c")
setcell(t,4,3,"Mean","c")
setcell(t,4,4,"Maximum","c")
!irow = 5
!icol = 1
for !i = 1 to nBloc
  !irow = !irow + 1
  %b = t_Bloc(!i,1)
  setcell(t,!irow,!icol,t_Bloc(!i,2),"l")
  setcell(t,!irow,!icol+1,0.01*@min(KIV_{%b}),"c",1)
  setcell(t,!irow,!icol+2,0.01*@mean(KIV_{%b}),"c",1)
  setcell(t,!irow,!icol+3,0.01*@max(KIV_{%b}),"c",1)
next
!n = 5 + nBloc
t.setindent(5,1,!n,1) 10
copy t tables\t_capital_stock
delete t

'--- base-year purchasing power parity adjustments
table(2,4) t
setcell(t,2,1, _
  "Year " + %base + " purchasing power parity adjustments","c")
t.setmerge(r2c1:r2c4)
t.setwidth(1) 24
t.setwidth(3) 24
setcell(t,4,1,"Bloc","c")
setcell(t,4,2,"Ratio","c")
setcell(t,4,3,"Bloc","c")
setcell(t,4,4,"Ratio","c")
'--- write the data in two columns
!n = @floor((nBloc+1)/2)
!irow = 5
!icol = 1
for !i = 1 to nBloc
  !irow = !irow + 1
  %b = t_Bloc(!i, 1)
  setcell(t,!irow,!icol,t_Bloc(!i, 2),"l")
  setcell(t,!irow,!icol+1,@elem(pp0_{%b},%base),"c",3)
  if !i = !n then
    !irow = 5
    !icol = 3
  endif
next
!n = 5 + !n
t.setindent(5,1,!n,1) 10
t.setindent(5,3,!n,3) 10
copy t tables\t_ppp_adjustment
delete t

'============ standard output of results

call pLog("defining standard output of results")

'--- world graphs and tables
table t_WRep
scalar nWRep = 0
call LoadTable(t_WRep, nWRep, _
  "GR;World growth rates;" _
  + "DV0_W,DVV_W,DYN_W,DVVN_W,DVNE_W,DIPT_W," _
  + "DX$_W,DXM$_W,DEP_W,DEPN_W,DCO2_W,pi$_w,pi_w,VVT_W;" _
  + "GDP at market rates,GDP at ppp rates," _
  + "Income per capita,GDP per capita at ppp rates," _
  + "GDP per person employed at ppp rates," _
  + "Private investment,Exports of goods and services," _
  + "Exports of manufactures,Energy production," _
  + "Non-carbon energy production," _
  + "CO2 emissions,Dollar inflation," _
  + "Domestic ccy inflation, Capacity utilisation;" _
  + "GT;% per year;-10,15" _
)

'--- relative prices
call LoadTable(t_WRep, nWRep, _
  "PX;Relative price indexes;" _
  + "pa_w,pe_w,pmm_w,pms_w,pm_w,pkp_w;" _
  + "Primary commodities,Oil,Exports of manufactures," _
  + "Exports of services,Exports of goods & services," _
  + "Asset prices;GT;" _
  + "base " + %base + " = 1; auto" _
)

'--- dollar prices of traded goods and services
call LoadTable(t_WRep, nWRep, _
  "PX$;Dollar price indexes;" _
  + "ph_w,pa$_w,pe$_w,pmm$_w,pms$_w,pm$_w,pkp$_w;" _
  + "Domestic expenditure,Primary commodities," _
  + "Oil,Exports of manufactures," _
  + "Exports of services,Exports of goods & services," _
  + "Asset prices;GT;" _
  +  "base " + %base + " = 1;auto" _
)
    
'--- components of world demand
call LoadTable(t_WRep, nWRep, _
  "dem;Components of final demand;" _
  + "CV_W,IPV_W,IVV_W,GV_W,XV$_W;" _
  + "Consumption,Private fixed capital formation," _
  + "Inventory accumulation," _
  + "Government spending," _
  + "Exports of goods & services;GT;" _
  +"per cent of GDP;" _
)

'--- world population and employment
call LoadTable(t_WRep, nWRep, _
  "dne;Population and employment;" _
  + "NWPN_W,NOPN_W,NCPN_W,NURN_W,NER_W;" _
  + "Working age population," _
  + "Old age population," _
  + "Child population," _
  + "Urban population," _
  + "Employed population," _
  + "Employment rate;GT;" _
  + "per cent;" _
)

'--- world energy and emissions
call LoadTable(t_WRep, nWRep, _
 "ene;Energy and emissions;" _
  + "EP_W,EPC_W,EPN_W,EX_W,CO2_W;" _
  + "Total energy production," _
  + "Carbon energy production," _
  + "Non-carbon energy production," _
  + "Exports of energy," _
  + "CO2 emissions;GT;" _
  + "million tons;" _
)

'--- Gini / Theil inequality 
'--- graph and table
%t = "T;Inequality measures;GY"
%tn = ";Gini income"
for !j = 1 to nTVar
  %t = %t + ",TH_" + t_TVar(!j,1)
  %tn = %tn  + ",Theil " + t_TVar(!j,2)
next
%t = %t + ",rylow_w"
%tn = %tn + ",Low income ratio"  
%t = %t + %tn + ";GT;index;auto"
call LoadTable(t_WRep, nWRep, %t)

'--- world variables (dump as CSV)
table t_WVar
scalar nWVar = 0
call LoadTable(t_WVar, nWVar, _
  "C_W;Consumption;$m ppp;0:" _
  + "IP_W;Private fixed capital formation;$m ppp;0:" _
  + "IV_W;Inventory accumulation;$m ppp;0:" _
  + "G_W;Government spending;$m ppp;0:" _
  + "V_W;GDP at ppp rates;$m ppp;0:" _
  + "V0_W;GDP at market rates;$m " + %base + ";0:" _
  + "VVT_W;Capacity utilisation;%;1:" _
  + "VV$_W;GDP purchasing power at world prices;$m wpp;0:" _
  + "VV_W;income from GDP at ppp rates;$m ppp;0:" _
  + "VVA_W;GDP in agriculture;$m ppp;0:" _
  + "VVE_W;GDP in extraction and utilities;$m ppp;0:" _
  + "VVI_W;GDP in industry;$m ppp;0:" _
  + "VVS_W;GDP in services;$m ppp;0:" _
  + "VVEM_W;Labour and mixed income;$m ppp;0:" _
  + "VVPR_W;Profits and rent;$m ppp;0:" _
  + "VVTX_W;Indirect taxes less subsidies;$m ppp;0:" _
  + "Y_W;Income at ppp rates;$m ppp;0:" _
  + "Y$_W;Income at world ppp;$m wpp;0:" _
  + "N_W;Population;m;0:" _
  + "YN_W;Income per capita;$ ppp;0:" _
  + "VN0_W;GDP per capita at market rates $;$ " + %base + ";0:" _
  + "X$_W;Exports of goods & services;$m wpp;0:" _
  + "XA$_W;Exports of primary commodities;$m wpp;0:" _
  + "XE$_W;Exports of energy;$m wpp;0:" _
  + "XM$_W;Exports of manufactures;$m wpp;0:" _
  + "XS$_W;Exports of services;$m wpp;0:" _
  + "EP_W;Primary energy production;mtoe;0:" _
  + "EPC_W;Carbon primary energy production;mtoe;0:" _
  + "EPN_W;Non-carbon primary energy production;mtoe;0:" _
  + "CO2_W;CO2 emissions;mt;0:" _
  + "pi_w;Domestic ccy inflation;% p.a.;3:" _
  + "pi$_w;Dollar inflation;% p.a.;3:" _
  + "ph_w;Domestic expenditure deflator;index;2:" _
  + "pa$_w;Dollar price of primary commodities;index;2:" _
  + "pe$_w;Dollar price of oil;index;2:" _
  + "pmm$_w;Dollar price of manuf exports;index;2:" _
  + "pms$_w;Dollar price of service exports;index;2:" _
  + "pm$_w;Dollar price of exports of goods & services;index;2:" _
  + "pa_w;Real price of primary commodities;index;2:" _
  + "pe_w;Real price of oil;index;2:" _
  + "pmm_w;Real price of manuf exports;index;2:" _
  + "pms_w;Real price of service exports;index;2:" _
  + "pm_w;Real price of exports of goods & services;index;2:" _
  )

'--- world variables (dump as CSV)
call LoadTable(t_WVar, nWVar, _
  "NYF_W;Female population 15-24;m;0:" _
  + "NYM_W;Male population 15-24;m;0:" _
  + "NVF_W;Female population 25+;m;0:" _
  + "NVM_W;Male population 25+;m;0:" _
  + "NWP_W;Working age population;m;0:" _
  + "NOP_W;Old age population;m;0:" _
  + "NCP_W;Child population;m;0:" _
  + "NUR_W;Urban population;m;0:" _
  + "NE_W;Employment;m;0:" _
  + "NEYF_W;Female employment 15-24 ;m;0:" _
  + "NEYM_W;Male employment 15-24;m;0:" _
  + "NEVF_W;Female employment 25+;m;0:" _
  + "NEVM_W;Male employment 25+;m;0:" _
  )

'--- world variables (dump as CSV)
call LoadTable(t_WVar, nWVar, _
  "NL_W;Labor force;m;0:" _
  + "NLYF_W;Female labor force 15-24;m;0:" _
  + "NLYM_W;Male labor force 15-24;m;0:" _
  + "NLVF_W;Female labor force 25+;m;0:" _
  + "NLVM_W;Male labor force 25+;m;0:" _
  + "NU_W;Unemployment;m;0:" _
  + "NUYF_W;Female unemployment 15-24t;m;0:" _
  + "NUYM_W;Male unemployment 15-24;m;0:" _
  + "NUVF_W;Female unemployment 25+;m;0:" _
  + "NUVM_W;Male unemployment 25+;m;0:" _
  + "NEAF_W;Female employment in agriculture;m;0:" _
  + "NEAM_W;Male employment in agriculture;m;0:" _
  + "NEIF_W;Female employment in industry;m;0:" _
  + "NEIM_W;Male employment in industry;m;0:" _
  + "NESF_W;Female employment in services;m;0:" _
  + "NESM_W;Male employment in services;m;0:" _
  )
'--- Gini / Theil inequality 
'--- dump as CSV 
%t = "GY;Gini income;%;2:"
for !j = 1 to nTVar
  %t = %t + "TH_" + t_TVar(!j,1) + ";" + _
    "Theil " + t_TVar(!j,2) + ";%;2:"
next
  
call LoadTable(t_WVar, nWVar, %t)

'--- bloc results
' Note: if an item includes two variables, the descriptor is parsed as
'      prefix] var1 and var2 [suffix

'--- bloc graphs and tables
table t_BRep
scalar nBRep = 0
call LoadTable(t_BRep, nBRep, _
  "JLAN_?;Land area;GT;hectares per person;auto:" _  
  + "VGA_?;Productivity gap in agriculture;GT;% of GDP;auto:" _
  + "NNE_?;Economic dependency ratio;GT;%;auto:" _  
  + "GSS_?;Government service standard;" _
    + "GT;index;auto:" _  
  + "rlx_?;External liabilities as % dom deposits;GT;%;auto:" _  
  + "rr_?;Reserves as % dom deposits ;GT;%;auto:" _  
  + "rax_?;External assets as % dom deposits;GT;%;auto:" _  
  + "raxo_?;Non-reserve assets as % dom deposits;GT;%;auto:" _
  + "ADIV$_? LDIV$_?;" _
    + "Direct investment] assets and liabilities" _
    + " [as % of GDP;GT;%;auto:" _
  + "APIV$_? LPIV$_?;" _
    + "Portfolio] assets and liabilities" _
    + " [as % of GDP;GT;%;auto:" _
  + "AOIV$_? LOIV$_?;" _
    + "Other] assets and liabilities" _
    + " [as % of GDP;GT;%;auto:" _
  )  

call LoadTable(t_BRep, nBRep, _
  "AGFV_?;Govt investment in banks as % of GDP;GT;%;auto:" _
  + "AXV$_? LXV$_?;External] assets and liabilities" _
    + " [as % of GDP;GT;%;auto:" _
  + "BA0_?;Volume balance in primary commodities;GT; million $" _
    + %base + " prices;auto:" _
  + "BAV$_?;Trade balance in primary commodities" _
    + " as % of GDP;GT;%;auto:" _
  + "BE0_?;Volume balance in fuels;GT; million $" _
    + %base + " prices;auto:" _
  + "BEV$_?;Trade balance in fuels as % of GDP;GT;%;auto:" _
  + "BITV$_?;Balance on income and transfers" _
    + " as % of GDP;GT;%;auto:" _
  + "BM0_?;Volume balance in manufactures;GT; million $" _
    + %base + " prices;auto:" _
  + "BMV$_?;Trade balance in manufactures as % of GDP;GT;%;auto:" _
  + "BS0_?;Volume balance in services;GT; million $" _
    + %base + " prices;auto:" _
  + "BSV$_?;Trade balance in services as % of GDP;GT;%;auto:" _
  + "CA$_?;Current account;GT;million $ " _
    + %base + " prices;auto:" _
  + "CAV$_?;Current account as % of GDP;GT;%;auto:" _
  + "CO2_?;Annual CO2 emissions;GT;m tons;auto:" _
  + "CO2N_?;Annual CO2 emissions per capita;GT;tons;auto:" _
  + "CO2V_?;CO2 emissions per $ of GDP;GT;kg per $;0,2.5:" _
  + "CV_?;Consumers expenditure as % of GDP;GT;%;auto:" _
  )
call LoadTable(t_BRep, nBRep, _
  + "DPV_? LNV_?;Bank] deposits and lending" _
    + " [as % of GDP;GT;%;0,300:" _
  + "DV0_?;Growth rate of GDP;G;% per year;-15,15:" _
  + "DVNE_?;Growth rate of output per person employed;" _
    + "G;% per year;-15,15:" _
  + "DVV5_?;5-year avg GDP growth rate;G;% per year;-10,15:" _
  + "DX$_?;Growth of exports;GT;% per year;-30,30:" _
  + "DY_?;Growth rate of real income;GT;% per year;-15,15:" _
  + "DYN_?;Growth rate of per capita income;GT;" _
    + "% per year;-15,15:" _
  + "ED_? EP_?;Energy] demand and production;GT;" _
    + "million tons of oil equivalent;auto:" _
  + "EDV_? EPV_?;Energy] demand and production [relative to GDP" _
    + ";GT;kg of oil equivalent per $;0,2:" _
  + "ei_?;Earnings inflation;GT;%;-5,30:" _
  + "ei_? pi_?;Earnings and price [inflation;G;%;-5,30:" _
  + "EPC_? EPN_?;Carbon and non-carbon [energy production;GT;" _
    + "million tons of oil equivalent;auto:" _
  + "EPND_? EPNP_?;" _
    + "Non-carbon energy as a share of] demand and production" _
    + ";GT;%;0,100:" _
  + "GV_?;Government expenditure as % of GDP;GT;%;auto:" _
  )

call LoadTable(t_BRep, nBRep, _
  "HAGFV_? HDPV_?;Holding gains on] govt and priv investment" _
    + " [in banks as % of GDP;GT;%;auto:" _
  + "HAXOV$_? HLXV$_?;Holding gains on] external assets" _
    + " and liabilities [as % of GDP;GT;%;auto:" _
  + "HNXV_? HKPV_?;Holding gains on] external position and" _
    + " domestic capital [as % of GDP;GT;%;-50,40:" _
  + "HWPV_?;Holding gains on private wealth" _
    + " as % of GDP;GT;%;auto:" _
  + "irs_? irm_?;Real] short rate and bond rate;GT;%;-5,20:" _
  + "is_? im_?;Nominal] short rate and bond rate;GT;%;-5,30:" _
  + "IAGOV_?;Government asset transactions as % of GDP;" _
    + "GT;%;auto:" _
  + "IPV_?;Private investment as % of GDP;G;%;0,50:" _
  + "IVV_?;Inventory accumulation as % of GDP;GT;%;-5,10:" _
  + "JIM_?;Infant mortality rate;GT;per 1000;auto:" _
  + "JLX_?;Life expectancy at birth;GT;(years);auto:" _
  + "JGN_?;Internal Gini index;GT;%;auto;0,100:" _
  + "KIV_?;Capital output ratio;GT;%;auto:" _
  + "KPV_? WPV_?;Value of capital and wealth [as % of GDP;" _
    + "GT;%;auto:" _
  + "LGV_?;Govt debt as % of GDP;GT;%;auto:" _
  + "LGOV_?;Non-bank government debt as % of GDP;GT;%;0,200:" _
  + "LYR_?;Relative income per capita (ppp);GT;log scale;auto:" _
  + "MMV$_?;Imports of manufactures as % of GDP;G;%;auto:" _
  + "mu_?;Profit and rent markup;GT;%;auto:" _
)

call LoadTable(t_BRep, nBRep, _
  + "NCPN_?;Child population;GT;%;auto:" _
  + "NE_?;Employment;GT;millions;:" _
  + "NEAEF_? NEAEM_?;" _
    + "Female and male [employment in agriculture;GT;%;auto:" _
  + "NEFE_?;Female share of employment;GT;%;auto:" _
  + "NEINF_? NEINM_?;" _
    + "Female and male [employment in industry" _
    + " as % of non-agricultural employment;GT;%;auto:" _
  + "NER_?;Employment rate;GT;%;auto:" _
  + "NERYF_? NERYM_?;" _
    + "Female and male [youth employment rate;GT;%;10,90:" _
  + "NERVF_? NERVM_?;" _
    + "Female and male [adult employment rate;GT;%;10,100:" _
  )
  
call LoadTable(t_BRep, nBRep, _
  "NFAV_?;Net financial assets as % of GDP;G;%;auto:" _
  + "NIM_?;Net migration;GT;millions;auto:" _
  + "NIME_?;Net migration as % of employment;GT;%;auto:" _
  + "NLGV_?;Government sector net lending as % of GDP;" _
    + "G;%;-15,15:" _
  + "NLGV_? NLPV_?;Government and private [net lending" _
    + " as % of GDP;GT;%;-15,15:" _
  + "NLNVF_? NLNVM_?;" _
    + "Female and male " _
    + "[adult labour force as % of population;" _
    + "GT;%;0,100:" _
  + "NLNYF_? NLNYM_?;" _
    + "Female and male " _
    + "[youth labour force as % of population;" _
    + "GT;%;0,100:" _
  + "NLPV_?;Private sector net lending as % of GDP;G;%;-15,15:" _
  + "NOPN_?;Old age population;GT;%;auto:" _
  + "NUL_?;Unemployment as % of labour force;GT;%;auto:" _
  + "NULVF_? NULVM_?;" _
    + "Female and male [adult unemployment rate;" _
    + "GT;%;auto:" _
  + "NULYF_? NULYM_?;" _
    + "Female and male [youth unemployment rate;" _
    + "GT;%;auto:" _
  + "NURN_?;Urban population;GT;%;auto:" _
  + "NWPN_?;Working age population;GT;%;auto:" _
  + "NXV$_?;Net external assets as % of GDP;G;%;auto:" _
  )

call LoadTable(t_BRep, nBRep, _
  + "pi_?;Price inflation;GT;%;-5,30:" _
  + "pvi_?;Cost inflation;GT;%;-5,30:" _
  + "pvi_? pi_?;Cost and price [inflation;G;%;-5,30:" _
  + "pkp_?;Valuation of capital stock;GT;%;auto:" _
  + "pxm_? pmm_?;Relative prices of] exports and imports" _
    +  " [of manufactures;GT;index;0,2:" _
  + "RALX$_?;External asset/liability ratio;GT;%;0,300:" _
  + "RMX$_?;Reserves as % of annual imports;GT;%;auto:" _
  + "rtx_?;Indirect taxes less subsidies;GT;%;auto:" _
  + "rx_?;Real exchange rate;GT;index;0,3:" _
  + "rxna_?;Nominal exchange rate revaluation /" _
    + " devaluation;GT;%;auto:" _
  )

call LoadTable(t_BRep, nBRep, _
  "SPV_?;Private saving as % of GDP;G;%;0,50:" _
  + "SPV_? IPV_?;Private] saving and investment [as % of GDP;GT;" _
    + "%;0,50:" _
  + "SXMM_?;Exports of manufactures as % of world total;" _
    + "GT;%;auto:" _
  + "TBV$_?;Trade balance as % of GDP;GT;%;-15,15:" _
  + "tt_?;Terms of trade impact on income;GT;index;0.7,1.3:" _
  + "ucx$_?;Unit cost of exports;GT;index;0,3:" _
  + "ulc$_?;Unit labour cost;GT;world pp;auto:" _
  + "uva$_?;Unit factor cost;GT;world pp;auto:" _
  + "VN0_? VVN_?;GDP per capita at] market rates and ppp rates;" _
    + "GT;$ " + %base + ";auto:" _
  + "VNE_?;GDP per person employed at ppp rates;GT;$ " _
    + %base + ";auto:" _
  )
  
call LoadTable(t_BRep, nBRep, _
  "VVAV_?;GDP in agriculture as % of GDP;GT;%;auto:" _
  + "VVEV_?;GDP in extraction and utilities as % of GDP;" _
    + "GT;%;auto:" _
  + "VVIV_?;GDP in industry as % of GDP;GT;%;auto:" _
  + "VVSV_?;GDP in services as % of GDP;GT;%;auto:" _
  + "VVIE_? VVAE_?;GDP in] industry and agriculture" _
    + " [per person employed;" _
    + "GT;$ ppp " + %base + ";0,100000:" _
  + "VVN_?;Per capita GDP;GT;$ ppp;auto:" _
  + "VVSE_? VVIE_?;GDP in] services and industry" _
    + " [per person employed;" _
    + "GT;$ ppp " + %base + ";auto:" _
  + "VVEME_?;Average annual earnings;" _
    + "GT;$ ppp;auto:" _
  + "VVEMV_?;Income from employment as % of GDP;" _
    + "GT;%;auto:" _
  + "VVPRV_?;Profits and rent as % of GDP;GT;%;auto:" _
  + "VVT_?;Capacity utilization;GT;%;-10,10:" _
  + "VVTXV_?;Indirect taxes less subsidies as % of GDP;GT;%;auto:" _
  )

call LoadTable(t_BRep, nBRep, _
  "WPV_?;Private wealth as % of GDP;G;%;auto:" _
  + "XMV$_?;Exports of manufactures " _
    + "as % of GDP;G;%;auto:" _
  + "XSV$_? MSV$_?;Exports and imports [of services" _
    + " as % of GDP;GT;%;auto:" _
  + "YGV_?;Government income as % of GDP;GT;%;auto:" _
  + "YGV_? GV_?;Government] income and expenditure" _
    + " [as % of GDP;G;%;auto:" _
  + "YGDV_?;Direct taxes less transfers as % of GDP;GT;%;-10,30:" _
  + "YN_?;Income per capita at ppp rates;GT;$ ppp;auto:" _
  + "YN$_?;Income per capita at market rates;GT;$ wpp;auto" _
)
  
'--- bloc variables (dump as CSV)
table t_BVar
scalar nBVar = 0
call LoadTable(t_BVar, nBVar, _
  "H;Domestic spending;$m ppp;0:" _
  + "V;GDP at base-year pp rates;$m ppp;0:" _
  + "V0;GDP at base-year market rates;$m " + %base + ";0:" _
  + "VVT;Capacity utilization;%;2:" _
  + "tt;Terms of trade impact on income;index;3:" _
  + "VV;Income from GDP;$m ppp;0:" _
  + "VV$;GDP purchasing power at world prices;$m wpp;0:" _
  + "VVA;GDP in agriculture;$m ppp;0:" _
  + "VVE;GDP in extraction and utilities;$m ppp;0:" _
  + "VVI;GDP in industry;$m ppp;0:" _
  + "VVS;GDP in services;$m ppp;0:" _
  + "VVEM;Labour and mixed income;$m ppp;0:" _
  + "VVPR;Profits and rent;$m ppp;0:" _
  + "VVTX;Indirect taxes less subsidies;$m ppp;0:" _
  + "Y;Income at ppp rates;$m ppp;0:" _
  + "Y$;Income at market rates;$m wpp;0:" _
  + "N;Population;m;0:" _
  + "VVN;GDP per capita;$ ppp;0:" _
  + "VN0;GDP per capita at base-year market rates;$ " _
    + %base + ";0:" _
  + "YN;Income per capita;$ ppp;0:" _
  + "YN$;Income per capita at market rates;$ wpp;0:" _
  + "YG;Government income;$m ppp;0:" _
  + "YGD;Direct taxes less transfers;$m ppp;0:" _
  + "G;Government expenditure on goods & services;$m ppp;0:" _
  + "NLG;Government net lending;$m ppp;0:" _
  + "IAG;Government net acquisition of assets;$m ppp;0:" _
  + "LG;Government debt;$m ppp;0:" _
  + "LGV;Govt debt as % of GDP;%;2:" _
  + "C;Consumption;$m ppp;0:" _
  + "SP;Private savings;$m ppp;0:" _
  + "IP;Private fixed investment;$m ppp;0:" _
  + "IV;Inventory accumulation;$m ppp;0:" _
  + "NLP;Private net lending;$m ppp;0:" _
  + "KP;Capital stock;$m ppp;0:" _
  + "WP;Wealth;$m ppp;0:" _
  + "HNX$;Holding gains on external positions;$m wpp;0:" _
  + "HWP;Holding gains on wealth;$m ppp;0:" _
  + "NCP;Population 0-14;m;1:" _
  + "NWP;Population 15-64;m;1:" _
  + "NOP;Population 65+;m;1:" _
  + "NYF;Female population 15-24 ;m;1:" _
  + "NYM;Male population 15-24;m;1:" _
  + "NVF;Female population 25+;m;1:" _
  + "NVM;Male population 25+;m;1:" _
  + "NUR;Urban population;m;1:" _
  + "NIM;Net migration;m;1:" _
)
  
call LoadTable(t_BVar, nBVar, _
  "NE;Employment;m;1:" _
  + "NEYF;Female employment 15-24 ;m;1:" _
  + "NEYM;Male employment 15-24;m;1:" _
  + "NEVF;Female employment 25+;m;1:" _
  + "NEVM;Male employment 25+;m;1:" _
  + "NER;Employment rate;%;2:" _
  + "NL;Labor force;m;1:" _
  + "NLYF;Female labor force 15-24;m;1:" _
  + "NLYM;Male labor force 15-24;m;1:" _
  + "NLVF;Female labor force 25+;m;1:" _
  + "NLVM;Male labor force 25+;m;1:" _
  + "NU;Unemployment;m;1:" _
  + "NUYF;Female unemployment 15-24t;m;1:" _
  + "NUYM;Male unemployment 15-24;m;1:" _
  + "NUVF;Female unemployment 25+;m;1:" _
  + "NUVM;Male unemployment 25+;m;1:" _
  + "NEAF;Female employment in agriculture;m;1:" _
  + "NEAM;Male employment in agriculture;m;1:" _
  + "NEIF;Female employment in industry;m;1:" _
  + "NEIM;Male employment in industry;m;1:" _
  + "NESF;Female employment in services;m;1:" _
  + "NESM;Male employment in services;m;1:" _
)

call LoadTable(t_BVar, nBVar, _
  "irs;Real short rate;% p.a.;2:" _
  + "irm;Real bond rate;% p.a.;2:" _
  + "is;Nominal short rate;% p.a.;2:" _
  + "im;Nominal bond rate;% p.a.;2:" _
  + "pi;Price inflation;% p.a.;1:" _
  + "pvi;Cost inflation;% p.a.;1:" _
  + "ei;Earnings inflation;% p.a.;1:" _
  + "mu;Profit and rent markup;%;2:" _
  + "rtx;Indirect taxes less subsidies;%;2:" _
  + "pmm;Real price of imports of manufactures;index;4:" _
  + "pxm;Real price of exports of manufactures;index;4:" _
  + "rx;Real exchange rate;index;4:" _
  + "rxna;Nominal exchange rate change;% p.a.;3:" _
  + "ucx$;Real unit cost of exports;index;4:" _
  + "EP;Primary energy production;mtoe;0:" _
  + "EPC;Primary carbon energy production;mtoe;0:" _
  + "EPN;Primary non-carbon energy production;mtoe;0:" _
  + "ED;Primary energy use;mtoe;0:" _
  + "BA$;Balance in primary commodities;$m wpp;0:" _
  + "BE$;Balance in fuels;$m wpp;0:" _
  + "BM$;Balance in manufactures;$m wpp;0:" _
  + "BS$;Balance in services;$m wpp;0:" _
  + "XA$;Exports of primary commodities;$m wpp;0:" _
  + "MA$;Imports of primary commodities;$m wpp;0:" _
  + "XE$;Exports of fuels;$m wpp;0:" _
  + "ME$;Imports of fuels;$m wpp;0:" _
  )
  
call LoadTable(t_BVar, nBVar, _
  "XM$;Exports of manufactures;$m wpp;0:" _
  + "MM$;Imports of manufactures;$m wpp;0:" _
  + "XS$;Exports of services;$m wpp;0:" _
  + "MS$;Imports of services;$m wpp;0:" _
  + "TB$;Trade balance;$m wpp;0:" _
  + "BIT$;Balance in income and transfers;$m wpp;0:" _
  + "XIT$;Income and transfer credits;$m wpp;0:" _
  + "MIT$;Income and transfer debits;$m wpp;0:" _
  + "CA$;Current account;$m wpp;0:" _
  + "AX$;External assets;$m wpp;0:" _
  + "LX$;External liabilities;$m wpp;0:" _
  + "NX$;Net external assets;$m wpp;0:" _
  + "R$;Exchange reserves;$m wpp;0:" _
  )

call LoadTable(t_BVar, nBVar, _
  + "_VV;GDP in current $;$m;0:" _
  + "_Y;National income in current $;$m;0:" _
  + "_CA;Current account in current $;$m;0:" _
  + "_BIT;Balance of income & transfers in current $;$m;0:" _
  + "_TB;Trade balance in current $;$m;0:" _
  + "_X;Exports of goods and services in current $;$m;0:" _
  + "_M;Imports of goods and services in current $;$m;0:" _
  + "_H;Domestic spending in current $;$m;0:" _
  + "_C;Private consumption in current $;$m;0:" _
  + "_IPT;Private investment in current $;$m;0:" _
  + "_G;Government spending in current $;$m;0:" _
  + "_NLP;Private net lending in current $;$m;0:" _
  + "_NLG;Government net lending in current $;$m;0:" _
  + "_LG;Government debt in current $;$m;0:" _
  + "_LGO;Government debt to non-financial agents in current $;$m;0:" _
  + "_LN;Bank lending to private sector in current $;$m;0:" _
  + "_DP;Bank liabilities to private sector in current $;$m;0" _
  )
 
'--- standard analysis tables
table t_TDef
scalar nTDef = 0
call pTabDef(t_TDef, nTDef)

'--- generate standard outputs

%tlopt = ""
if @upper(@left(%graphs,1)) = "Y" then %tlopt = "G" endif
if @upper(@left(%subgraphs,1)) = "Y" then %tlopt = %tlopt + "S" endif
if @upper(@left(%tables,1)) = "Y" then %tlopt = %tlopt + "T" endif
if @upper(@left(%analysis,1)) = "Y" then %tlopt = %tlopt + "A" endif
if @upper(@left(%csv,1)) = "Y" then %tlopt = %tlopt + "C" endif
if @upper(@left(%markets,1)) = "Y" then %tlopt = %tlopt + "M" endif
if @upper(@left(%grtab,1)) = "Y" then %tlopt = %tlopt + "D" endif
if %tlopt <> "" then call pReport(%tlopt, 0) endif

call pEnd

endsub
