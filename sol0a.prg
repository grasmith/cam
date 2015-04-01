'PROGRAM: sol0a.prg    Copyright (C) 2013,2014 Alphametrics Co. Ltd.
'
' CAM Version 5.2   CIS(Russia) plunge variant
'
' The program reads SOL0.wf1 and creates SOL0a.wf1
'
' updated: NK 18/12/2014
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call sol0a
'------------------------------------------------------------------
subroutine sol0a

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
%wkfile = "SOL0a"
call CreateModelFile("SOL0", %wkfile) 
delete m_wm0 *_0p

'--- update settings
call pLog("SOL0a PROGRAM v1812")
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "0a"
t_Settings(4,2) = "Russia plunge"

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end

smpl %actual+1 %actual+2
'--- Russia rouble and investment plunge
rxu_CIS_ins.fill(s) -0.5 -0.05 
IP_CIS_ins.fill(s) -0.2 -0.15 -0.10 -0.05
SP_CIS_ins = 0.1

'--- Russia sanctions
BIT$U_CIS_ins = -0.01

smpl %actual+1 %end

call Limit (90, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub

