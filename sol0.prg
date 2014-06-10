'PROGRAM: sol0.prg       Copyright (C) 2012,2013,2014 Alphametrics Co. Ltd.
'
' CAM version 5.1 EUR variant
'
' Baseline projection (main version)
'
' Assumes alignment up to 2014
'
' Run this program after sol0p.prg (alignment) and before running
' other simulations
'
' The program reads SOL0p.wf1 and creates SOL0.wf1
'
' updated: FC 11/01/2014
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

%graphs = "Yes"
%graphcomp = "Yes"
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
call pLog("SOL0 PROGRAM v1101")
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

'--- Eurozone: nominal exchange rates
call Target("rxu_FR", "rxna_FR-rxna_DE", "rxna_tar_FR", 100,100)
call Target("rxu_IT", "rxna_IT-rxna_DE", "rxna_tar_IT", 100,100)
call Target("rxu_ES", "rxna_ES-rxna_DE", "rxna_tar_ES", 100,100)
call Target("rxu_PL", "rxna_PL-rxna_DE", "rxna_tar_PL", 100,100)
call Target("rxu_EUW", "rxna_EUW-rxna_DE", "rxna_tar_EUW", 100,100)
call Target("rxu_EUS", "rxna_EUS-rxna_DE", "rxna_tar_EUS", 100,100)
call Target("rxu_EUE", "rxna_EUE-rxna_DE", "rxna_tar_EUE", 100 ,100)
call Limit(100, "ALL")

'--- targets up to the alignment horizon
if @val(%latest) < @val(%actual) then
  smpl %latest+1 %actual
  '--- eurozone exchange rates
  series rxna_tar_fr = rxna_fr-rxna_de
  series rxna_tar_it = rxna_it-rxna_de
  series rxna_tar_es = rxna_es-rxna_de
  series rxna_tar_pl = rxna_pl-rxna_de
  series rxna_tar_euw = rxna_euw-rxna_de
  series rxna_tar_eus = rxna_eus-rxna_de
  series rxna_tar_eue = rxna_eue-rxna_de
endif
'--- targets and adjustments beyond the alignment horizon
smpl %actual+1 %end
'--- eurozone exchange rates
rxna_tar_fr = 0
rxna_tar_it = 0
rxna_tar_es = 0
rxna_tar_pl = 0
rxna_tar_euw = 0
rxna_tar_eus = 0
rxna_tar_eue = 0

'--- trend adjustments
'--- reduced growth of carbon energy supply
EPC_PL_ins = -0.01
EPC_EUW_ins = -0.01
EPC_UK_ins = -0.01
EPC_EUN_ins = -0.01
EPC_CN_ins = -0.02

'--- nuclear energy supply
'    reduced trends
EPN_US_a = -0.02
EPN_OD_a = -0.03
EPN_CN_a = -0.02
EPN_CI_a = -0.03
EPN_FR_a = -0.02
'    Japan gradual restart after 2012 closedown
EPN_JA_a = 0.0

'--- reduced saving
SP_US_a = -0.01
SP_CN_a = -0.01
SP_UK_a = -0.01
SP_IT_a = -0.01
SP_OD_a = -0.005

'--- reduced losses on liabilities
rplx$_us_a = -0.005
rplx$_uk_a = -0.005

'--- avoid deflation
ei_JA_a = 0.1
ei_EAH_a = 0.1
ei_CN_a = 0.2
ei_IN_a = 0.1
ei_CI_a = 0.1
ei_DE_a = 0.05
ei_PL_a = 0.05

call Limit(95, "ALL")
'==================================================================
' PROCESSING
'==================================================================

smpl %actual+1 %end
call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "", 8)
call pEnd

endsub

