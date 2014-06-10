'PROGRAM: sola.prg       Copyright (C) 2012,2013 Alphametrics Co. Ltd.
'
' CAM version 5.0      WEO alignment
'
' Align the model to match values loaded from ALIGN.XLS
'
' Series must be provided from a given start year
'      %loadfrom (in OPTIONS below)  <=  %latest  (defined in set.prg)
' up to at least the alignment end year
'      %align  (also defined in set.prg)
'
' Aligns the model with estimates for the world oil price, bloc GDP
' in current and base-year dollars, current accounts and nominal
' exchange rate changes
'
' Run this program after est.prg (equation estimation)
' and before sol0p.prg (plain baseline projection)
'
' The program reads EST.wf1 and writes SOLA.wf1
'
' Results are recorded as actuals
'
' updated: FC 08/05/2013
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

%graphs = "No"
%markets = "No"
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
call pLog("SOLA PROGRAM v0805")
t_Settings(3,2) = ""
t_Settings(4,2) = "WEO alignment"

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
EPC_PL_ins = -0.01
EPC_EUW_ins = -0.01
EPC_UK_ins = -0.01
EPC_EUN_ins = -0.01
EPC_CN_ins = -0.02

'--- nuclear energy supply
'    reduced trends in US, CA, CN, CIS and FR
EPN_US_ins = -0.02
EPN_OD_ins = -0.03
EPN_CN_ins = -0.02
EPN_CI_ins = -0.03
EPN_FR_ins = -0.02
'    Japan: 2012 closedown and gradual, partial restart
EPN_JA_ins = 0
EPN_JA_ins.fill(s) -2.3

smpl %latest+1 %end

'==================================================================
' LOAD TARGETS
'==================================================================
call pLog("reading targets from align.xls")
'--- oil price
smpl 2008 %align
read(t=xls,s=UNCTAD,t,f6) align.xls 1
'--- bloc data
smpl 2008 %align
!n = 28*nBloc
read(t=xls,s=WEO bloc data,t,n4) align.xls !n
delete ser*

'--- create target series
smpl %start %latest
series pew$_tar = pe$_w
p_Bloc.genr V0_tar_? = V0_?
p_Bloc.genr _V_tar_? = _V_?
p_Bloc.genr _CA_tar_? = _CA_?
p_Bloc.genr rxna_tar_? = rxna_?

smpl %latest+1 %align
pew$_tar = pew$_e*@elem(pe$_w,%latest)/@elem(pew$_e,%latest)
delete pew$_e

for !i = 1 to nBloc
  %b = t_Bloc(!i, 1)
  '--- link the estimates to latest data
  V0_{%b} = APT0_e_{%b} _
    * @elem(V0_{%b},%latest)/@elem(APT0_e_{%b},%latest)
  _V_{%b} = APT_e_{%b} _
    * @elem(_V_{%b},%latest)/@elem(APT_e_{%b},%latest)
  '--- convert GDP targets to % growth for uniform scaling
  _V_tar_{%b} = @pc(_V_{%b})
  V0_tar_{%b} = @pc(V0_{%b})
  '--- set other targets directly
  _CA_tar_{%b} = 1000*BCA_e_{%b} _
    + @elem(_CA_{%b},%latest) - 1000*@elem(BCA_e_{%b},%latest)
  rxna_tar_{%b} = 100*(APT_E_{%b}*APT0_E_{%b}(-1) _
                     /(APT_E_{%b}(-1)*WNGR_E_{%b})-1)
next
delete APT0_e_* APT_e_* BCA_e_* WNGR_e_*

'==================================================================
' DEFINE ADJUSTMENT RULES
'==================================================================

call pLog("defining adustment rules")
'--- dollar price of oil (index)
'    align by supply adjustment in main export blocs
call Target("EPC_WA","pe$_w","pew$_tar",-5,100)
call Link("EPC_CI","EPC_WA",1)
call Link("EPC_ACX","EPC_WA",0.5)
call Link("EPC_AFS","EPC_WA",0.5)

'--- GDP, current account and nominal exchange rate
for !i = 1 to nBloc
  %b = t_Bloc(!i, 1)
  '--- adjust consumption and investment to get real GDP [1000]
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
    call Target("ei_us","@pc(_V_us)","_V_tar_us",100,100)
  else
    '--- fix dollar GDP by adjusting real exchange rates [300]
    call Target("rxu_"+%b,+ "@pc(_V_"+%b+")","_V_tar_"+%b,300,100)
    '--- adjust domestic inflation to fix nominal xrate changes [-100]
    call Target("ei_"+%b,"rxna_"+%b,"rxna_tar_"+%b,-100,100)
  endif
  '--- adjust current accounts with CN as the residual
  if %b <> "cn" then
    %s = "sxmu_" + %b
    call Target(%s,"100*_CA_"+%b+"/_V_"+%b+"(-1)", _
      "100*_CA_tar_"+%b+"/_V_"+%b+"(-1)",100,100)
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

'==================================================================
' PROCESSING
'==================================================================

'--- note: computed observations recorded as actuals
%graphcomp = "No"

call pLog("align with series loaded from ALIGN.XLS")
call pCheckSolveReport({%gm}, @str(@val(%latest)-3), %latest, _
  %align, "m=10000", 5)

'--- convert instrument values to add factors
call InsToAddFactors(m_wm, t_Rule, nRule, t_Bloc, nBloc)

'--- record alignment horizon
t_Settings(1,2) = %align

'--- decay add factors from %align + 1 to %end
smpl %align+1 %end
call ExtendAdd(!vdecay)
smpl %start %align

'--- clean up
delete *_tar*
call pEnd

endsub
