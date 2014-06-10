'PROGRAM: sol1.prg    Copyright (C) 2013,2014 Alphametrics Co. Ltd.
'
' CAM Version 5.2   EUR variant
'
' investment boost
'
' The program reads SOL0.wf1 and creates SOL1.wf1
'
' updated: FC 05/06/2014
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call sol1
'------------------------------------------------------------------
subroutine sol1

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
%wkfile = "SOL1"
call CreateModelFile("SOL0", %wkfile) 
delete m_wm0 *_0p

'--- update settings
call pLog("SOL1 PROGRAM v0506")
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "1"
t_Settings(4,2) = "High investment"

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end
for %b EUP FR ENE
  '--- investment boost
  call Floor("IP_" + %b, _
    "@pc(VVN_" + %b + ")","3",100,100)
  call Link("G_" + %b, "IP_" + %b, 0.5)
  '--- limit monetary inflows to 1% of GDP
  '    by attracting sufficient direct
  '    investment and some portfolio investment
  call Ceiling("ILDI$_" + %b, "100*ILOI$_" _
    + %b + "/VV$_" + %b,"2",-100,50)
  call Link("LPI$_" + %b, "ILDI$_" + %b, 0.5)
next

call Limit (90, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub
