'PROGRAM: sola.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6      WEO alignment
'
' Align the model to match values loaded from ALIGN.XLS
'
' Series must be provided from a given start year
'      %loadfrom (in OPTIONS below)  <=  %latest  (defined in set.prg)
' up to at least the alignment end year
'      %align  (also defined in set.prg)
' The first observation for the world oil price sheet is at cell G15
' and for the bloc data sheet at cell O4.
'
' Aligns the model with estimates for the world oil price, bloc GDP
' in current and base-year dollars, current accounts and nominal
' exchange rate changes
'
' Run this program after est.prg (equation estimation)
' and before sol0.prg (baseline projection)
'
' The program reads EST.wf1 and writes SOLA.wf1
'
' Results are recorded as actuals
'
' updated: FC 15/04/2012
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call sola
'------------------------------------------------------------------
subroutine sola

'--- first year of alignment data in ALIGN.XLS
%loadfrom = "2007"

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
'--- open the estimation workfile
open EST
pageselect tables
delete *
pageselect graphs
delete *
pageselect data
delete sp_log*

'--- update settings
call pLog("SOLA PROGRAM v0415")
%wkfile = "SOLA"
t_Settings(3,2) = ""
t_Settings(4,2) = "WEO alignment"
t_Settings(7,2) = %wkfile
%gm = "m_wma"

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
model m_wm
m_wm.merge m_wmi
m_wm.merge m_wmo

'--- create a scenario model
model {%gm}
{%gm}.merge m_wm
{%gm}.scenario "actuals"
%alias = ""

'==================================================================
' DEFINITIONS
'==================================================================

smpl %latest+1 %align

'--- reduced trend growth of carbon energy supply in US, Europe and China
EPC_US_ins =  -0.005
EPC_EUW_ins = -0.01
EPC_EUE_ins = -0.01
EPC_UK_ins = -0.01
EPC_EUN_ins = -0.03
EPC_CN_ins = -0.02

smpl %latest+1 %end

'==================================================================
' LOAD TARGETS
'==================================================================
%s = %loadfrom + "-" + %align
call pLog("reading targets from align.xls for " + %s)
smpl %loadfrom %align
'--- oil price
read(t=xls,s=World oil price,t,g15) align.xls 1
'--- bloc data
!n = 28*nBloc
read(t=xls,s=Bloc data,t,o4) align.xls !n
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
  V0_{%b} = V0_e_{%b} _
    * @elem(V0_{%b},%latest)/@elem(V0_e_{%b},%latest)
  _V_{%b} = _V_e_{%b} _
    * @elem(_V_{%b},%latest)/@elem(_V_e_{%b},%latest)
  '--- convert GDP targets to % growth for uniform scaling
  _V_tar_{%b} = @pc(_V_{%b})
  V0_tar_{%b} = @pc(V0_{%b})
  '--- set other targets directly
  _CA_tar_{%b} = 1000*_CA_e_{%b} _
    + @elem(_CA_{%b},%latest) - 1000*@elem(_CA_e_{%b},%latest)
  rxna_tar_{%b} = rxna_e_{%b}
next
delete V0_e_* _V_e_* _CA_e_* rxna_e_*

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
  call Target("IP_"+%b,"@pc(V0_"+%b+")","V0_tar_"+%b,50,100)
  if %b = "cn" then
    call Link("pkp_"+%b,"IP_"+%b,0.3)
    call Link("SP_"+%b,"IP_"+%b,-0.3)
  else
    call Link("pkp_"+%b,"IP_"+%b,0.8)
    call Link("SP_"+%b,"IP_"+%b,-0.8)
  endif

  '--- adjust US domestic inflation to fix dollar GDP [100]
  if %b = "us" then
    call Target("pvi_us","@pc(_V_us)","_V_tar_us",100,100)
  else
    '--- fix dollar GDP by adjusting real exchange rates [300]
    call Target("rxu_"+%b,+ "@pc(_V_"+%b+")","_V_tar_"+%b,100,100)
    '--- adjust domestic inflation to fix nominal xrate changes [-100]
    call Target("pvi_"+%b,"rxna_"+%b,"rxna_tar_"+%b,-100,100)
  endif
  '--- adjust current accounts with EUW as the residual [100]
  if %b <> "euw" then
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

'--- record the computed observations as actuals
%graphcomp = "No"

call pLog("align with series loaded from ALIGN.XLS")

call pCheckSolveReport({%gm}, @str(@val(%latest)-3), %align, _
  "m=20000", 5)

t_Settings(1,2) = %align

'--- convert instrument values to add factors
'    EViews bug: if add factors are not defined at the bottom level
'                they are lost when the workfile is saved and reloaded
'    Solution: define add factors at the bottom level for all variables
m_wmi.addassign(c,i) @stochastic
m_wmo.addassign(c,i) @stochastic
call InsToAddFactors(m_wm, t_Rule, nRule, t_Bloc, nBloc)

'--- decay add factors from %align + 1 to %end
smpl %align+1 %end
call ExtendAdd(!vdecay)
smpl %start %align

'--- clean up
delete *_tar*
call pEnd

endsub
