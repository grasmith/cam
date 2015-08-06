'PROGRAM: sol4.prg    Copyright (C) 2013,2015 Alphametrics Co. Ltd.
'
' CAM Version 6.1   FESSUD variant
'
' India: sustainable GDP growth with FDI
'
' The program reads SOL0.wf1 and creates SOL4.wf1
'
' updated: FC 05/06/2015
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call sol4
'------------------------------------------------------------------
subroutine sol4

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
%wkfile = "sol4"
call CreateModelFile("SOL0", %wkfile) 
delete m_wm0

'--- update settings
call pLog("sol4 PROGRAM v0506")
t_Settings(5,2) = t_Settings(3,2)
t_Settings(4,2) = t_Settings(4,2)
t_Settings(3,2) = "4"
t_Settings(4,2) = "India: FDI and growth"

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end

' 	Objective: achieve high growth by increasing demand
'   with limited government deficit and c/a deficit

'	Demand is adjusted by incentives for private investment
' and savings (IP, SP). To mitigate inflation pressures
' GDP growth should not exceed 8% pa.

' The external current a/c deficit is limited to 2% of GDP
' through real exchange rate devaluation, promotion of 
' non-carbon energy production and inward and outward FDI.

' The government deficit is limited to a maximum 4% of GDP
' by adjustment of government spending.

' UNRESOLVED PROBLEM: low availability of resources for
' government spending. It may be necessary to increase
' national and local taxation and other revenue sources.

'--- 8% GDP growth (investment and savings incentives)
'    cool off if GDP growth is going to exceed 8% p.a.
	call Target ("IP_IN", "@pc(VV_IN)","8",50,50) 
	call Link("SP_IN", "IP_IN", -1)
	
'--- max 2% c/a deficit
	call Floor("sxmu_in", "100*CA$_IN/VV$_IN","-2",5,30)
'--- inward and outward direct investment to support
'    the export push
	call Link("LDI$_IN", "sxmu_in", 5)
	call Link("ADI$_IN", "sxmu_in", 5)
'-- real exchange rate devaluation
	call Link("rxu_in", "sxmu_in", -5)

'--- boost domestic non-carbon energy supply to reach 10%
'    of total energy demand by 2030
	call Target("EPN_in","100*EPN_in/ED_in","10",5,10)

'--- 4% govt deficit (adjust govt spending)
  call Target("G_in","(-100*NLG_in)/VV_in","4",1,50)
  
  call Limit (90, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g,mit=5000", 8)
call pEnd

endsub

