'PROGRAM: sol2.prg    Copyright (C) 2013,2014 Alphametrics Co. Ltd.
'
' CAM Version 5.2   FESSUD variant
'
' EU structural policies
'
' The program reads SOL1.wf1 and creates SOL2.wf1
'
' updated: FC 05/06/2014
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call sol2
'------------------------------------------------------------------
subroutine sol2

%actual = "2014"

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
%wkfile = "SOL2"
call CreateModelFile("SOL1", %wkfile) 
delete m_wm1

'--- update settings
call pLog("SOL2 PROGRAM v0506")
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "2"
t_Settings(4,2) = "Structural policies"

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end

'--- except de oeu uk
for %b fr euc eup
  call DropRules("G_" + %b + " IP_" + %b)
  '--- current a/c target (floor)
  series CAV_{%b}_TAR = -3.5+(@trend-45)*0.14
  '--- growth target
  call Floor("IP_" + %b, _
    "@pc(VVN_" + %b + ")","3",100,20)
  call Link("G_" + %b, "IP_" + %b, 1)
  '--- trade target: mf export mkt share
  call Floor("sxmu_" + %b, _
    "100*CA$_" + %b + "/VV$_" + %b, _
    "CAV_" + %b + "_TAR",50,20)
  '--- and energy saving
  call Link("ED_" + %b, "sxmu_" + %b, -1) 
next

call Limit (90, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub