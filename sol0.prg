'PROGRAM: sol0.prg       Copyright (C) 2012,2013,2014 Alphametrics Co. Ltd.
'
' CAM Version 5.2 ASIA variant
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
' updated: NK 17/12/2014
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
call pLog("SOL0 PROGRAM v1712")
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
'--- decline in real exchange rate (prevents
'    negative growth in long run)
rxu_DE_a = -0.03
call Target("rxu_eup", "rxna_eup-rxna_DE", "rxna_tar_eup", 100,100)
call Target("rxu_euc", "rxna_euc-rxna_DE", "rxna_tar_euc", 100,100)
call Target("rxu_fr", "rxna_fr-rxna_DE", "rxna_tar_fr", 100,100)
call Limit(100, "ALL")
'--- UK real exchange rate too
rxu_UK_a = -0.03

'--- targets up to the alignment horizon
if @val(%latest) < @val(%actual) then
  smpl %latest+1 %actual
  '--- eurozone exchange rates exclude ther Europe
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

'--- trend adjustments
'--- reduced growth of carbon energy supply
EPC_DE_a = -0.01
EPC_EUC_a = -0.01
EPC_UK_a = -0.01
EPC_OEU_a = -0.01
EPC_CN_a = -0.02
'--- increased growth of carbon energy supply
EPC_US_ins = 0.04
EPC_NWA_ins = 0.02

'--- nuclear energy supply
'    reduced trends in main countries
EPN_US_a = -0.02
EPN_FR_a = -0.03
EPN_CN_a = -0.02
EPN_CIS_a = -0.03
EPN_UK_a = -0.02
'    Japan: 2012 closedown and gradual, partial restart
EPN_OEH_a = 0

'--- saving adjustments
SP_US_a = 0.005
SP_BR_a = -0.01

'--- investment slowdown
IP_CN_a = -0.02

'--- reduced losses on liabilities
rplx$_us_a = -0.005
rplx$_uk_a = -0.005

'--- avoid deflation
ei_OEH_a = 0.1
ei_CN_a = 0.15
ei_IN_a = 0.1
ei_CIS_a = 0.05
ei_DE_a = 0.08
ei_DE_a.fill(s) 0.01,0.02,0.03,0.04,0.05,0.06,0.07
'--- cost pressure on other eurozone members
ei_FR_a = -0.03
ei_EUC_a = -0.04
ei_EUP_a = -0.02
ei_UK_a = -0.02

call Limit(95, "ALL")
'==================================================================
' PROCESSING
'==================================================================

smpl %actual+1 %end
call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "", 8)
call pEnd

endsub

