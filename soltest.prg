'PROGRAM: soltest.prg          Copyright (C) 2013 Alphametrics Co. Ltd.
'
' CAM Version 6.0 utility
'
' Quick baseline (optional)
'
' Run this program after est.prg to check long-run system
' implications of estimated equations
'
' The program reads EST.wf1 and creates SOL0.wf1
' Simulation results have alias _0
' 
' updated: FC 08/03/2013
' 
'==================================================================
' OPTIONS
'==================================================================
include "set"
call soltest
'------------------------------------------------------------------
subroutine soltest

%graphs = "No"
%markets = "No"
%tables = "No"
%analysis = "No"
%csv = "No"

%repstart = "1980"
'==================================================================
' PREFACE
'==================================================================
mode quiet
%wkfile = "SOL0"
call CreateModelFile("EST", %wkfile) 

'--- update settings
call pLog("SOLTEST PROGRAM v0803")
t_Settings(3,2) = "0"
t_Settings(4,2) = "Baseline"

'--- preprocessing
table t_Rule
scalar nRule
wfsave {%wkfile}

'--- build the core model (zdef.prg)
smpl %start %end
call pDefineModel
'--- create the baseline model
call pCreateScenario(%gm, %alias)
{%gm}.scenario "Baseline"

'==================================================================
' DEFINITIONS
'==================================================================

'--- last actual (new solution starts after this year)
%actual = t_Settings(1,2)
smpl %actual+1 %end

call Target("rxu_fr", "rxna_fr-rxna_de", "0", 100, 100)
call Target("rxu_euc", "rxna_euc-rxna_de", "0", 100, 100)
call Target("rxu_eup", "rxna_eup-rxna_de", "0", 100, 100)
call Limit (100, "ALL")

'==================================================================
' PROCESSING
'==================================================================

%graphcomp = "No"

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "m=10000", 8)
call pEnd

endsub
