'PROGRAM: sol0.prg       Copyright (C) 2012,2013,2014 Alphametrics Co. Ltd.
'
' CAM Version 5.2 EUR variant
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
' updated: FC 05/06/2014
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
%graphcomp = "No"
%markets = "Yes"
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
call pLog("SOL0 PROGRAM v0506")
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
call Target("rxu_fr", "rxna_fr-rxna_EUC", "rxna_tar_fr", 100,100)
call Target("rxu_eup", "rxna_eup-rxna_EUC", "rxna_tar_eup", 100,100)
call Limit(100, "ALL")

'--- targets up to the alignment horizon
if @val(%latest) < @val(%actual) then
  smpl %latest+1 %actual
  '--- eurozone exchange rates
  series rxna_tar_fr = rxna_fr-rxna_euc
  series rxna_tar_eup = rxna_eup-rxna_euc
endif
'--- targets and adjustments beyond the alignment horizon
smpl %actual+1 %end
'--- eurozone exchange rates
rxna_tar_fr = 0
rxna_tar_eup = 0

'--- trend adjustments
'--- reduced trend growth of carbon energy supply in Europe and China
EPC_EUC_ins = -0.01
EPC_EUP_ins = -0.01
EPC_UK_ins = -0.01
EPC_ENE_ins = -0.01
EPC_CN_ins = -0.02

'--- nuclear energy supply
'    reduced trends in main countries
EPN_US_ins = -0.02
EPN_CAN_ins = -0.03
EPN_CN_ins = -0.02
EPN_CIS_ins = -0.03
EPN_FR_ins = -0.02
'    Japan: gradual restart
EPN_JA_ins = -0.1

'--- saving adjustments
SP_US_a = 0.005
SP_UK_a = -0.01
SP_BR_a = -0.01

'--- investment slowdown
IP_CN_a = -0.02

'--- reduced losses on liabilities
rplx$_us_a = -0.005
rplx$_uk_a = -0.005

'--- avoid deflation
ei_JA_a = 0.1
ei_OEH_a = 0.1
ei_CN_a = 0.2
ei_IN_a = 0.1
ei_CIS_a = 0.1
ei_EUC_a = 0.05

call Limit(95, "ALL")
'==================================================================
' PROCESSING
'==================================================================

smpl %actual+1 %end
call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "", 8)
call pEnd

endsub

