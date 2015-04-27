'PROGRAM: sol0p.prg          Copyright (C) 2013 Alphametrics Co. Ltd.
'
' GPM version 6.0
'
' baseline projection (plain version)
'
' Run this program after sola.prg (alignment).
' It gives a plain projection of the model that should be examined
' before defining the baseline in sol0.prg
'
' The program reads SOLA.wf1 and creates SOL0P.wf1
'
' updated: FC 22/03/2013
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call sol0p
'------------------------------------------------------------------
subroutine sol0p

%graphs = "No"
%graphcomp = "No"
%markets = "No"
%tables = "No"
%analysis = "No"
%csv = "No"

%repstart = "1980"
'==================================================================
' PREFACE
'==================================================================
mode quiet
%wkfile = "SOL0P"
call CreateModelFile("SOLA", %wkfile) 

delete m_wma t_Rule* nRule*

'--- update settings
call pLog("SOL0P PROGRAM v2203")
t_Settings(3,2) = "0p"
t_Settings(4,2) = "plain baseline"

call pCreateScenario(%gm, %alias)

'==================================================================
' DEFINITIONS
'==================================================================

'--- last available from alignment
'    plain baseline starts after this year
%latest = t_Settings(2,2)

smpl %latest+1 %end

'==================================================================
' PROCESSING
'==================================================================
call pCheckSolveReport({%gm}, %latest, %latest, %predict, _
  "m=1000", 8)
call pEnd

endsub
