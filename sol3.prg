'PROGRAM: sol3.prg    Copyright (C) 2013,2014 Alphametrics Co. Ltd.
'
' CAM Version 6.1   FESSUD variant
'
' Sustainable development in Europe
'
' The program reads SOL0.wf1 and creates SOL3.wf1
'
' Note: many policies but no new fiscal redistribution
' mechanism. Convergence is aided by a German industry
' relocation policy. Everything is financed via the
' banking system with restrictions on cross-border
' portfolios and other external positions to maintain
' stability. Open market issue of government debt
' is restricted and banks have to hold the remainder as
' a condition for operating in each country.
'
' updated: FC 01/05/2015
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call sol3
'------------------------------------------------------------------
subroutine sol3

%actual = "2015"

%graphs = "Yes"
%graphcomp = "Yes"
%markets = "No"
%tables = "No"
%analysis = "No"
%csv = "No"

'==================================================================
' PREFACE
'==================================================================
mode quiet
%wkfile = "SOL3"
call CreateModelFile("SOL0", %wkfile) 
delete m_wm0 *_0p

'--- update settings
call pLog("SOL3 PROGRAM v0105")
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "3"
t_Settings(4,2) = "Sustainable development"

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end
'--- reasonable growth for Germany
call Target("SP_DE", "@pc(VV_DE/NE_DE)","2",-100,100)
call Link("IP_DE", "SP_DE", -0.2)
call Limit (99, "ALL")
'--- relocation (mainly to rest of Europe)
sxmu_DE_ins = -0.02
'--- productivity standard for rest of Europe
for %b fr euc eup oeu uk
  call Floor("IP_" + %b, _
    "100*VV_"+%b+"*NE_DE/(VV_DE*"+"NE_"+%b+")", _
    "100",20,10)
next
'--- Europe-wide policies
for %b de fr euc eup oeu uk
  '--- job creation
  call Ceiling("NULVM_" + %b, _
    "100*NU_" + %b + "/NL_" + %b,"6",10,10)
  call Link("NULVF_" + %b, "NULVM_" + %b, 1)
  call Link("NULYM_" + %b, "NULVM_" + %b, 1)
  call Link("NULYF_" + %b, "NULVM_" + %b, 1)
  '--- fiscal equalisation
  call Target("YGR_" + %b, _
    "100*YGR_" + %b + "/VV_" + %b,"50",50,20)
  '--- government service and transfer standard
  call Target("G_" + %b, _
    "100*(G_" + %b + "+YGTI_" + %b + ")" _
    + "/((1-0.5*NV_" + %b + "/N_" + %b + ")" _
    + "*VV_" + %b + ")","80",50,10)
  call Link("YGTI_" + %b, "G_" + %b, 0.3)
  '--- cross-border direct investment limits
  call Ceiling("ADI$_" + %b, _
    "100*ADI$_" + %b + "/VV$_" + %b,"100",100,20)
  call Ceiling("LDI$U_" + %b, _
    "100*LDI$_" + %b + "/VV$_" + %b,"50",100,10)
  '--- domestic finance
  '    banks finance govt debt over the ceiling
  call Ceiling("LGO_" + %b, _
    "100*LGO_" + %b + "/VV_" + %b,"40",20,50)
  '--- cross-border financial risk limits
  call Ceiling("API$_" + %b, _
    "100*API$_" + %b + "/VV$_" + %b,"200",100,10)
  call Ceiling("LPI$_" + %b, _
    "100*LPI$_" + %b + "/VV$_" + %b,"100",100,10)
  call Ceiling("NOI$_" + %b, _
    "100*AOI$_" + %b + "/VV$_" + %b,"200",100,30)
next

'--- carbon emission limit
call Target("ED_DE", _
  "@pc(CO2_DE+CO2_FR+CO2_UK+CO2_EUP" _
    + "+CO2_OEU)","-1",100,20)
call Link("EPN_DE", "ED_DE",-1)
for %b fr euc eup oeu uk
  call Link("ED_" + %b, "ED_DE", 1)
  call Link("EPN_" + %b, "ED_DE", -1)
next
  
call Limit (95, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub
