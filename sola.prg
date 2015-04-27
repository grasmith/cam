'PROGRAM: sola.prg       Copyright (C) 2012,2015 Alphametrics Co. Ltd.
'
' CAM Version 6.0  FESSUD variant
'
' Aligns the model with estimates for
' - the world oil price (UNCTAD)
' - GDP, current account and exchange rates (WEO)
' - employment and unemployment (ILO)
'
' Run this program after est.prg (equation estimation)
' and before sol0p.prg (plain baseline projection)
'
' See the LOAD TARGETS section below for details of loading
' from spreadsheets in ALIGN.XLS
'
' Estimates must be provided for a sequence of years that
' at least covers the period from
'    %latest  last year for historical data, up to 
'    %align   the alignment end year (defined below)
'
' The program reads EST.wf1 and writes SOLA.wf1
'
' Results are recorded as actuals
'
' updated: FC 24/04/2015
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call sola
'------------------------------------------------------------------
subroutine sola

'--- alignment horizon
%align = "2015"

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
call pLog("SOLA PROGRAM v2404")
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

'--- Note: if add factors are not defined at the bottom level
' they are lost when the workfile is saved and reloaded
m_wm.addassign @stochastic
'--- set historical add factors
smpl %start %latest
m_wm.addinit(v=n) @stochastic

'==================================================================
' DEFINITIONS
'==================================================================

smpl %latest+1 %end

'--- reduced trend growth of carbon energy supply in Europe and China
EPC_DE_a = -0.01
EPC_EUC_a = -0.01
EPC_UK_a = -0.01
EPC_OEU_a = -0.01
EPC_CN_a = -0.02

'--- nuclear energy supply
'    reduced trends in main countries
EPN_US_a = -0.02
EPN_FR_a = -0.03
EPN_CN_a = -0.02
EPN_RU_a = -0.03
EPN_UK_a = -0.02
'    Japan: 2012 closedown and gradual, partial restart
EPN_OEH_a = 0

smpl %latest+1 %end

'==================================================================
' LOAD TARGETS
'==================================================================
call pLog("reading targets from align.xls")
'--- oil price
smpl 2009 %align
read(t=xls,s=UNCTAD,t,f6) align.xls 1
'--- WEO bloc data
smpl 2009 %align
!n = 28*nBloc
read(t=xls,s=WEO bloc data,t,n4) align.xls !n
delete ser*
'--- ILO bloc data
smpl 2009 %align
!n = 16*nBloc
read(t=xls,s=ILO bloc data,t,l4) align.xls !n
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
p_Bloc.genr rxna_tar_? = 100*(APT_E_?*APT0_E_?(-1) _
                              /(APT_E_?(-1)*WNGR_E_?)-1)
'--- employment and unemployment
p_Bloc.genr NU_? = U_I_?*@elem(NU_?,%latest) _
                        /@elem(U_I_?,%latest)
p_Bloc.genr NL_? = NU_? + EMP_I_?*@elem(NE_?,%latest) _
                          /@elem(EMP_I_?,%latest)
p_Bloc.genr NUL_tar_? = 100*NU_?/NL_?
p_Bloc.genr NL_tar_? = @pc(NL_?)

delete PEW$_E EMP_I_* U_I_*
delete APT0_E_* APT_E_* BCA_E_* WNGR_E_*

'==================================================================
' DEFINE ADJUSTMENT RULES
'==================================================================
call pLog("defining adjustment rules")

'--- dollar price of oil (index)
'    supply adjustments
call Target("EPC_NWA","pe$_w","pew$_tar",-10,100)
call Link("EPC_RU","EPC_NWA",1)
call Link("EPC_US","EPC_NWA",1)
call Link("EPC_CAN","EPC_NWA",1)
call Link("EPC_OCA","EPC_NWA",1)
call Link("EPC_ACX","EPC_NWA",0.5)
call Link("EPC_BR","EPC_NWA",0.5)
call Link("EPC_OAF","EPC_NWA",0.5)

'--- GDP, current account and nominal exchange rate
for !i = 1 to nBloc
  %b = t_Bloc(!i,1)
  '--- adjust consumption and investment to get real GDP
  call Target("IP_"+%b,"@pc(V0_"+%b+")","V0_tar_"+%b,1000,100)
  call Link("mu_"+%b,"IP_"+%b,-1)
  call Link("SP_"+%b,"IP_"+%b,-0.8)
  call Link("G_"+%b,"IP_"+%b,0.25)
  call Link("YGD_"+%b,"IP_"+%b,-0.1)
  call Link("pkp_"+%b,"IP_"+%b,0.5)
  '--- adjust US domestic inflation to fix dollar GDP
  if %b = "us" then
    call Target("pvi_us","@pc(_VV_us)","_VV_tar_us",100,100)
  else
    '--- fix dollar GDP by adjusting real exchange rates
    call Target("rxu_"+%b, "@pc(_VV_"+%b+")", _
      "_VV_tar_"+%b,300,100)
    '--- adjust domestic inflation to fix nominal xrate changes
    call Target("pvi_"+%b,"rxna_"+%b,"rxna_tar_"+%b,-100,100)
  endif
  '--- adjust current accounts with CN as the residual
  if %b <> "cn" then
    '--- shares of mf export markets
    call Target("sxmu_" + %b,"100*_CA_"+%b+"/_VV_"+%b+"(-1)", _
      "100*_CA_tar_"+%b+"/_VV_"+%b+"(-1)",100,100)
    '--- linked adjustments
    '    imports of manufactures 
    '    net exports of services
    '    net exports of food and raw materials
    '    net income from abroad
    '    energy exports and imports
    call Link("MM$_" + %b,"sxmu_" + %b,-1)
    call Link("BS$U_" + %b,"sxmu_" + %b,1)
    call Link("BA0U_" + %b,"sxmu_" + %b,1)
    call Link("BIT$U_" + %b,"sxmu_" + %b,1)
    call Link("XE$U_" + %b,"sxmu_" + %b,1)
    call Link("ME$_" + %b,"sxmu_" + %b,-1)
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
  %align, "", 5)

'--- project adjustments with decay to %end
smpl %align+1 %end
call ExtendIns(!vdecay)
'--- add ins values to add factors and zero ins
smpl %latest+1 %end
call InsToAddFactors(m_wm, t_Rule, nRule, t_Bloc, nBloc)

'--- record alignment horizon
t_Settings(1,2) = %align
t_Settings(4,2) = "alignment"

smpl %start %align

'--- clean up
delete *_tar*
call pEnd

endsub
