'PROGRAM: sol0b.prg    Copyright (C) 2013,2014 Alphametrics Co. Ltd.
'
' CAM Version 6.1a   FESSUD variant
'
' Alternative baseline with China recession
' differences start from 2016
'
' The program reads SOL0.wf1 and creates SOL0B.wf1
'
' updated: FC 27/01/2016
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call sol0b
'------------------------------------------------------------------
subroutine sol0b

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
%wkfile = "SOL1"
call CreateModelFile("SOL0", %wkfile) 
delete m_wm0 *_0p

'--- update settings
call pLog("SOL0B PROGRAM v2701")
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "0b"
t_Settings(4,2) = "China recession"

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end

'--- hard money policy
rxu_cn_a = rxu_cn_a + 0.02
ip_cn_a = ip_cn_a - 0.02

call Limit (95, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub
