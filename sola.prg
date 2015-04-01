'PROGRAM: sola.prg       Copyright (C) 2012,2014 Alphametrics Co. Ltd.
'
' CAM Version 5.2  ASIA variant
'
' Align the model to match values loaded from ALIGN.XLS
'
' Series must be provided from a given start year
'      %loadfrom (in OPTIONS below)  <=  %latest  (defined in set.prg)
' up to at least the alignment end year
'      %align  (also defined in set.prg)
'
' Aligns the model with estimates for the world oil price,
' bloc GDP in current and base-year dollars,
' current account and nominal exchange rate changes,
' labour force and unemployment
'
' Run this program after est.prg (equation estimation)
' and before sol0p.prg (plain baseline projection)
'
' The program reads EST.wf1 and writes SOLA.wf1
'
' Results are recorded as actuals
'
' updated: NK 17/12/2014
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call sola
'------------------------------------------------------------------
subroutine sola

'--- alignment horizon
%align = "2014"

'--- decay rate for add factors beyond the alignment horizon
!vdecay = 0.6

%graphs = "Yes"
%markets = "Yes"
%tables = "No"
%analysis = "No"
%csv = "No"

'==================================================================
' PREFACE
'==================================================================
mode quiet
%wkfile = "SOLA"
call CreateModelFile("EST", %wkfile) 

'--- update settings
call pLog("SOLA PROGRAM v3005")
t_Settings(3,2) = ""
t_Settings(4,2) = "actuals"

'--- preprocessing
table t_Rule
scalar nRule
wfsave {%wkfile}
%s = @str(@val(%latest)+1)
if @val(%latest)+1 < @val(%align) then
  %s = %s + "-" + %align
endif  
call pLog("computing estimates for " + %s)
call pLog("building the model")

'--- build the core model (zdef.prg)
smpl %start %end
call pDefineModel
'--- create the alignment model
%gm = "m_wma"
rename m_ds {%gm}
{%gm}.merge m_wm
{%gm}.scenario "actuals"
%alias = ""

'==================================================================
' DEFINITIONS
'==================================================================

'    EViews bug: if add factors are not defined at the bottom level
'                they are lost when the workfile is saved and reloaded
'    Solution: define add factors at the bottom level for all variables
m_wmi.addassign @stochastic
m_wmo.addassign @stochastic

smpl %latest+1 %align

'--- reduced trend growth of carbon energy supply in Europe and China
EPC_DE_ins = -0.01
EPC_EUC_ins = -0.01
EPC_UK_ins = -0.01
EPC_OEU_ins = -0.01
EPC_CN_ins = -0.02

'--- nuclear energy supply
'    reduced trends in main countries
EPN_US_ins = -0.02
EPN_FR_ins = -0.03
EPN_CN_ins = -0.02
EPN_CIS_ins = -0.03
EPN_UK_ins = -0.02
'    Japan: 2012 closedown and gradual, partial restart
EPN_OEH_ins = 0

smpl %latest+1 %end

'==================================================================
' LOAD TARGETS
'==================================================================
call pLog("reading targets from align.xls")
'--- oil price
smpl 2008 %align
read(t=xls,s=UNCTAD,t,f6) align.xls 1
'--- WEO bloc data
smpl 2008 %align
!n = 28*nBloc
read(t=xls,s=WEO bloc data,t,n4) align.xls !n
delete ser*
'--- EMPL bloc data
smpl 2008 %align
!n = 21*nBloc
read(t=xls,s=EMPL bloc data,t,m4) align.xls !n
delete ser*

'--- create alignment targets
smpl %latest+1 %align
'--- world oil price
series pew$_tar = PEW$_E*@elem(pe$_w,%latest)/@elem(PEW$_E,%latest)
'--- GDP at constant prices
p_Bloc.genr V0_? = APT0_E_? _
    * @elem(V0_?,%latest)/@elem(APT0_E_?,%latest)
p_Bloc.genr V0_tar_? = @pc(V0_?)
'--- GDP and current account in current dollars
p_Bloc.genr _VV_? = APT_E_? _
    * @elem(_VV_?,%latest)/@elem(APT_E_?,%latest)
p_Bloc.genr _VV_tar_? = @pc(_VV_?)
p_Bloc.genr _CA_tar_? = 1000*BCA_E_? _
    + @elem(_CA_?,%latest) - 1000*@elem(BCA_E_?,%latest)
'--- nominal exchange rate
' 1+rxna/100 = (APT.APT0(-1)/APT(-1))/sum[APT2.APT0(-1)/APT2(-1)]
p_Bloc.genr rxna_tar_? = 100*(APT_E_?*APT0_E_?(-1) _
                              /(APT_E_?(-1)*WNGR_E_?)-1)
'--- labour force and unemployment
p_Bloc.genr NL_? = NL_I_?*@elem(NL_?,%latest)/@elem(NL_I_?,%latest)
p_Bloc.genr NL_tar_? = @pc(NL_?)
p_Bloc.genr NUL_tar_? = (NU_I_?/NL_I_?)*@elem(NUL_?,%latest) _
  /(@elem(NU_I_?,%latest)/@elem(NL_I_?,%latest))

delete PEW$_E NL_I_* NU_I_* APT0_I_* 
delete APT0_E_* APT_E_* BCA_E_* WNGR_E_*

'==================================================================
' DEFINE ADJUSTMENT RULES
'==================================================================
call pLog("defining adustment rules")

'--- dollar price of oil (index)
'    align by supply adjustment in main export blocs
call Target("EPC_NWA","pe$_w","pew$_tar",-5,100)
call Link("EPC_CIS","EPC_NWA",1)
call Link("EPC_ACX","EPC_NWA",0.5)
call Link("EPC_AFL","EPC_NWA",0.5)

'--- GDP, current account and nominal exchange rate
for !i = 1 to nBloc
  %b = t_Bloc(!i,1)
  '--- adjust consumption and investment to get real GDP
  call Target("IP_"+%b,"@pc(V0_"+%b+")","V0_tar_"+%b,1000,100)
  if %b = "cn" then
    call Link("pkp_"+%b,"IP_"+%b,0.3)
    call Link("SP_"+%b,"IP_"+%b,-0.3)
  else
    call Link("pkp_"+%b,"IP_"+%b,0.8)
    call Link("SP_"+%b,"IP_"+%b,-0.8)
  endif
  '--- adjust US domestic inflation to fix dollar GDP
  if %b = "us" then
    call Target("ei_us","@pc(_VV_us)","_VV_tar_us",100,100)
  else
    '--- fix dollar GDP by adjusting real exchange rates [300]
    call Target("rxu_"+%b, "@pc(_VV_"+%b+")", _
      "_VV_tar_"+%b,300,100)
    '--- adjust domestic inflation to fix nominal xrate changes [-100]
    call Target("ei_"+%b,"rxna_"+%b,"rxna_tar_"+%b,-100,100)
  endif
  '--- adjust current accounts with CN as the residual
  if %b <> "cn" then
    %s = "sxmu_" + %b
    call Target(%s,"100*_CA_"+%b+"/_VV_"+%b+"(-1)", _
      "100*_CA_tar_"+%b+"/_VV_"+%b+"(-1)",100,100)
    '--- link imports of manufactures (double weight)
    %s1 = "MM$_" + %b
    call Link(%s1,%s,-2.0)
    '--- link net exports of services
    %s1 = "BS$U_" + %b
    call Link(%s1,%s,1.5)
    '--- link net exports of food and raw materials
    %s1 = "BA0U_" + %b
    call Link(%s1,%s,1.5)
    '--- link exports of energy
    %s1 = "XE$U_" + %b
    call Link(%s1,%s,1)
    '--- link imports of energy
    %s1 = "ME$_" + %b
    call Link(%s1,%s,-1)
    '--- link net income from abroad
    %s1 = "BIT$U_" + %b
    call Link(%s1,%s,1)
  endif
next

'--- labour force and unemployment
for !i = 1 to nBloc
  %b = t_Bloc(!i,1)
'--- labour force
  call Target("NLNVM_"+%b,"@pc(NL_"+%b+")", _
    "NL_tar_"+%b,100,100)
  call Link("NLNVF_"+%b,"NLNVM_"+%b,1)
'--- unemployment
  call Target("NULVM_"+%b,"100*NU_"+%b+"/NL_"+%b, _
    "NUL_tar_"+%b,1,100)
  call Link("NULVF_"+%b,"NULVM_"+%b,1)
  call Link("NULYM_"+%b,"NULVM_"+%b,1)
  call Link("NULYF_"+%b,"NULVM_"+%b,1)
next

'==================================================================
' PROCESSING
'==================================================================

'--- note: computed observations recorded as actuals
%graphcomp = "No"

call pLog("align with series loaded from ALIGN.XLS")
call pCheckSolveReport({%gm}, @str(@val(%latest)-3), %latest, _
  %align, "o=g,m=10000", 5)

'--- convert instrument values to add factors
call InsToAddFactors(m_wm, t_Rule, nRule, t_Bloc, nBloc)

'--- record alignment horizon
t_Settings(1,2) = %align
t_Settings(4,2) = "WEO alignment"

'--- decay add factors from %align + 1 to %end
smpl %align+1 %end
call ExtendAdd(!vdecay)
smpl %start %align

'--- clean up
delete *_tar*
call pEnd

endsub
