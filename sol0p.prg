'PROGRAM: sol0p.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6 AUGUR variant
'
' Baseline projection (main version)
'
' Run this program after sola.prg (alignment).
' It gives a plain projection of the model that should be examined
' before defining the baseline in sol0.prg
'
' The program reads SOLA.wf1 and creates SOL0P.wf1
'
' updated: FC 15/04/2012
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call sol0p
'------------------------------------------------------------------
subroutine sol0p

%graphs = "Yes"
%markets = "No"
%tables = "No"
%analysis = "No"
%csv = "No"

%repstart = "1980"

'==================================================================
' PREFACE
'==================================================================
mode quiet

'--- open the SOLA workfile (plain baseline)
open SOLA
pageselect graphs
delete *
pageselect tables
delete *
pageselect data
delete sp_log* m_wma t_Rule* nRule*

'--- update settings
call pLog("SOL0P PROGRAM v0415")
%wkfile = "SOL0P"
t_Settings(3,2) = "0p"
t_Settings(4,2) = "plain baseline"
t_Settings(7,2) = %wkfile
call pCreateScenario(%gm, %alias)

'==================================================================
' DEFINITIONS
'==================================================================

'--- last actual (new solution starts after this year)
%actual = t_Settings(1,2)

smpl %actual+1 %end

'==================================================================
' PROCESSING
'==================================================================

%graphcomp = "No"
call pCheckSolveReport({%gm}, %actual, %predict, "m=10000", 8)
call pEnd

endsub
