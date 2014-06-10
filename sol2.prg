'PROGRAM: sol2.prg    Copyright (C) 2013,2014 Alphametrics Co. Ltd.
'
' CAM Version 5.2   EUR variant
'
' growth and trade boost
'
' The program reads SOL0.wf1 and creates SOL2.wf1
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
%graphcomp = "No"
%markets = "Yes"
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


for %b EUP FR ENE
  call DropRules("G_" + %b + " IP_" + %b)
  '--- current a/c target (floor)
  series CAV_{%b}_TAR = -3.5+(@trend-45)*0.14
  '--- growth target
  call Floor("G_" + %b, _
    "@pc(VVN_" + %b + ")","3",100,100)
  call Link("IP_" + %b, "G_" + %b, 1)
  '--- trade target
  call Floor("sxmu_" + %b, _
    "100*CA$_" + %b + "/VV$_" + %b, _
    "CAV_" + %b + "_TAR",50,20)
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
