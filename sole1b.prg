'PROGRAM: solE1b.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' SOAS Baseline Scenario
' Intended as a projection of ongoing austerity and 
' business as usual policies.
'
' The program reads SOL0.wf1 and creates SOLE1b.wf1
'
' updated: JM 25/01/2014
'
' differences from sol0
'   IAGO issues neutralised by fixing at zero
'   East Europe inflation assumption dropped
'   government budget cuts in US, OD and EU (net revenue limits)
'   debt ceilings in Europe (cuts in government spending)
'   negative impact of EU crisis on investment in EUW and EUN
'   debt ceilings in the US, OD and JA (cuts in govt spending)
'   more relocation of industry to India and S America
'   more raw material exports from Africa and S America
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call solE1b
'------------------------------------------------------------------
subroutine solE1b

'%actual = "2013"

%graphs = "Yes"
%graphcomp = "Yes"
%markets = "No"
%tables = "No"
%analysis = "No"
%csv = "No"

'================================================================
' PREFACE
'==================================================================
mode quiet
'--- open the SOL0 workfile
open SOL0
pageselect graphs
delete *
pageselect tables
delete *
pageselect data
delete sp_log* m_wm0
'--- update settings
call pLog("SOLE1b PROGRAM v0305")
%wkfile = "SOLE1b"
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "E1b"
t_Settings(4,2) = "Struggling on"
t_Settings(7,2) = %wkfile
call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

%actual = t_Settings(1,2)
smpl %actual+1 %end

'---- FIX IAGO
call Fix("IAGO_EUS", "level", "0")
call Fix("IAGO_EUN", "level", "0")
call Fix("IAGO_EUW", "level", "0")
call Fix("IAGO_EUE", "level", "0")
call Fix("IAGO_UK", "level", "0")

'----- Drop East Europe inflation assumption
call DropRules("pvi_EUE")

'---  cuts in government budgets (US, OD, EU)
'call Target("YG_US","YG_US/VV_US", ".15", 1, 30)
'call Target("YG_OD","YG_OD/VV_OD", ".20", 1, 30)
call Target("YG_EUW","YG_EUW/VV_EUW", ".20", 1, 30)
call Target("YG_EUE","YG_EUE/VV_EUE", ".20", 1, 30)
call Target("YG_EUS","YG_EUS/VV_EUS", ".20", 1, 30)
call Target("YG_UK","YG_UK/VV_UK", ".20", 1, 30)

'--- slow implementation and higher target for EUN
call Target("YG_EUN","YG_EUN/VV_EUN", ".30", 1, 20)

'--- Europe: ceiling on debt ratios
call Ceiling("G_EUS", "G_EUS/VV_EUS", "0.19", 0, 30)
call Floor("G_EUE", "G_EUE/VV_EUE", "0.20", 0, 30)
call Ceiling("G_UK", "G_UK/VV_UK", "0.21", 0, 30)
call Ceiling("G_EUW", "G_EUW/VV_EUW", "0.23", 0, 30)
call Ceiling("G_EUN", "G_EUN/VV_EUN", "0.31", 0, 30)

'--- Europe: ceiling on debt ratios
'call Ceiling("G_EUS", "LG_EUS/VV_EUS", "0.6", 0.1, 30)
'call Ceiling("G_EUE", "LG_EUE/VV_EUE", "0.6", 0.1, 30)
'call Ceiling("G_UK", "LG_UK/VV_UK", "0.6", 0.1, 30)
'call Ceiling("G_EUW", "LG_EUW/VV_EUW", "0.6", 0.1, 30)
'call Ceiling("G_EUN", "LG_EUN/VV_EUN", "0.6", 0.1, 30)

'--- negative investment impact of European crisis
IP_EUW_ins.fill(s) -0.02, -0.02, -0.01 
IP_EUN_ins = 0.5*IP_EUW_ins


' Interest rate ceilings
call Ceiling("im_UK", "irm_UK", "1.5", 0, 50)
call Ceiling("im_EUW", "irm_EUW", "1.5", 0, 50)
call Ceiling("im_EUS", "irm_EUS", "3", 0, 50)
call Ceiling("im_EUN", "irm_EUN", "1.5", 0, 50)
call Ceiling("im_EUE", "irm_EUE", "3.5", 0, 50)

'--- US, OD and JA debt ceilings
'call Ceiling("G_US","100*LG_US/VV_US(-1)","50",100,10)
'call Link("IAGO_US","G_US", 0.5)
'call Ceiling("G_OD","100*LG_OD/VV_OD(-1)","60",100,10)
'call Link("IAGO_OD","G_OD", 0.5)
'call Ceiling("G_JA","100*LG_JA/VV_JA(-1)","150",100,10)
'call Link("IAGO_JA","G_JA", 0.5)

'--- relocation of industries to IN and AMS
'sxmu_AMS_ins = 0.05
'sxmu_IN_ins = 0.05

'--- raw material exports from low income areas
'BA0U_AFS_ins = 0.003
'BA0U_AFN_ins = 0.5*BA0U_AFS_ins
'BA0U_AMS_ins = 0.5*BA0U_AFS_ins

'call Limit (95, "ALL")
'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %predict, "m=10000", 8)
call pEnd

endsub
