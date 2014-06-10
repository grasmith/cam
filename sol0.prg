'PROGRAM: sol0.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6 AUGUR variant
'
' Baseline projection (main version)
'
' Assumes alignment up to 2012
'
' Run this program after sol0p.prg (alignment) and before running
' other simulations
'
' The program reads SOL0p.wf1 and creates SOL0.wf1
'
' updated: FC 17/05/2012
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
%markets = "Yes"
%tables = "No"
%analysis = "No"
%csv = "No"

%repstart = "1980"

'==================================================================
' PREFACE
'==================================================================
mode quiet

'--- open the SOL0p workfile (plain baseline)
open {%alignmethod}SOL0p
pageselect graphs
delete *
pageselect tables
delete *
pageselect data
delete sp_log* m_wm0p t_Rule* nRule*

'--- update settings
call pLog("SOL0 PROGRAM v0517")
%wkfile = "SOL0"
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "0"
t_Settings(4,2) = "baseline"
t_Settings(7,2) = %wkfile
call pCreateScenario(%gm, %alias)
{%gm}.scenario "Baseline"

'==================================================================
' DEFINITIONS
'==================================================================

call pLog("baseline definitions")

'--- projection period
%actual = t_Settings(1,2)
smpl %actual+1 %end

'--- Eurozone: nominal exchange rates
call Target("rxu_EUS", "rxna_EUS-rxna_EUW", "0", 100,100)
call Target("rxu_EUE", "rxna_EUE-rxna_EUW", "2 2 1 *0", 100 ,100)
call Limit(100, "ALL")
'--- East Europe: inflation differential
call Target("pvi_EUE", "pvi_EUE-pvi_EUW", "1", 100,100)

'--- trend adjustments
'--- reduced growth of carbon energy supply in US, EU and CN
EPC_US_a =  -0.005
EPC_EUW_a = -0.01
EPC_EUE_a = -0.01
EPC_UK_a = -0.01
EPC_EUN_a = -0.03
EPC_CN_a = -0.02

'--- Japan: real exchange rate
rxu_JA_a = -0.05

'--- China: government income
YG_CN_a = -0.05
'--- China: net imports of commodities
BA0U_CN_a = -0.001

'--- Other East Asia: net transfer revenue
BIT$U_EAO_a = -0.0025

'--- India: government income
YG_IN_a = -0.03

'--- CIS: government income
YG_CI_a = -0.02

call Limit(95, "ALL")
'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %predict, "m=10000", 8)
call pEnd

endsub
