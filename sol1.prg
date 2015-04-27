'PROGRAM: sol1.prg    Copyright (C) 2013,2014 Alphametrics Co. Ltd.
'
' CAM Version 6.0   FESSUD variant
'
' EU investment boost
'
' The program reads SOL0.wf1 and creates SOL1.wf1
'
' updated: FC 09/04/2015
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call sol1
'------------------------------------------------------------------
subroutine sol1

%actual = "2015"

%graphs = "No"
%graphcomp = "No"
%markets = "No"
%tables = "No"
%analysis = "No"
%csv = "No"

'==================================================================
' PREFACE
'==================================================================
mode quiet
%wkfile = "SOL1"
call CreateModelFile("SOL0", %wkfile) 
delete m_wm0 *_0p

'--- update settings
call pLog("SOL1 PROGRAM v0904")
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "1"
t_Settings(4,2) = "EU investment boost"

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end
'--- except de oeu uk
for %b fr euc eup
  '--- investment boost aimed at establishing a minimum
  '    3% per capita GDP growth rate over a period of time
  '    with some supporting government services & investmt
  call Floor("IP_" + %b, _
    "@pc(VVN_" + %b + ")","3",100,20)
  call Link("G_" + %b, "IP_" + %b, 0.5)
next

call Limit (95, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub
