'PROGRAM: sol0.prg       Copyright (C) 2012,2015 Alphametrics Co. Ltd.
'
' CAM Version 6.0 FESSUD variant
'
' Baseline projection (main version)
'
' Assumes alignment up to 2015
'
' Run this program after sol0p.prg (alignment) and before running
' other simulations
'
' The program reads SOL0p.wf1 and creates SOL0.wf1
'
' updated: FC 30/03/2015
'
' Note: adjustments are made using add factors (_a).
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call sol0
'------------------------------------------------------------------
subroutine sol0

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
%wkfile = "SOL0"
call CreateModelFile("SOL0P", %wkfile) 
delete m_wm0p t_Rule* nRule*

'--- update settings
call pLog("SOL0 PROGRAM v3003")
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "0"
t_Settings(4,2) = "baseline"

call pCreateScenario(%gm, %alias)

'==================================================================
' DEFINITIONS
'==================================================================

call pLog("baseline definitions")
%actual = t_Settings(1,2)

'--- Eurozone: nominal exchange rates exclude Other Europe(oeu)
call Target("rxu_eup", "rxna_eup-rxna_DE", "rxna_tar_eup", 100,100)
call Target("rxu_euc", "rxna_euc-rxna_DE", "rxna_tar_euc", 100,100)
call Target("rxu_fr", "rxna_fr-rxna_DE", "rxna_tar_fr", 100,100)
call Limit(100, "ALL")

'--- targets up to the alignment horizon
if @val(%latest) < @val(%actual) then
  smpl %latest+1 %actual
  '--- eurozone exchange rates exclude other Europe
  series rxna_tar_eup = rxna_eup-rxna_de
  series rxna_tar_euc = rxna_euc-rxna_de
  series rxna_tar_fr = rxna_fr-rxna_de
endif
'--- targets and adjustments beyond the alignment horizon
smpl %actual+1 %end
'--- eurozone exchange rates
rxna_tar_eup = 0
rxna_tar_euc = 0
rxna_tar_fr = 0

'--- increased oil and gas supply in US and Canada
EPC_US_a = 0.03
EPC_CAN_a = 0.02

call Limit(95, "ALL")
'==================================================================
' PROCESSING
'==================================================================

smpl %actual+1 %end
call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "", 8)
call pEnd

endsub

