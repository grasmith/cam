'PROGRAM: sol1.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' Reduced government
'
' The program reads SOL0.wf1 and creates SOL1.wf1
'
' updated: FC 15/04/2012
'
' differences from sol0
'   government budget cuts in US, OD and EU (net revenue limits)
'   debt ceilings in Europe (cuts in government spending)
'   consumption restriction in UK to limit current account deficit
'   negative impact of EU problems on investment in EUW and EUN
'   net immigration limits in EU regions
'   debt ceilings in the US, OD and JA (cuts in govt spending)
'   more relocation of industry to India and S America
'   more raw material exports from Africa and S America
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call sol1
'------------------------------------------------------------------
subroutine sol1

%actual = "2012"

%graphs = "Yes"
%graphcomp = "Yes"
%markets = "Yes"
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
call pLog("SOL1 PROGRAM v1504")
%wkfile = "SOL1"
t_Settings(1,2) = %actual
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "1"
t_Settings(4,2) = "Reduced government"
t_Settings(7,2) = %wkfile
call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end

'---  cuts in government budgets
call Target("YG_US","YG_US/VV_US", ".15", 1, 30)
call Target("YG_OD","YG_OD/VV_OD", ".20", 1, 30)
call Target("YG_EUN","YG_EUN/VV_EUN", ".25", 1, 30)
call Target("YG_EUW","YG_EUW/VV_EUW", ".20", 1, 30)
call Target("YG_EUE","YG_EUE/VV_EUE", ".20", 1, 30)
call Target("YG_EUS","YG_EUS/VV_EUS", ".20", 1, 30)
call Target("YG_UK","YG_UK/VV_UK", ".20", 1, 30)

'--- Europe: ceiling on debt ratios
call Ceiling("G_EUS", "LG_EUS/VV_EUS", "0.6", 0.1, 30)
call Link("IAGO_EUS","G_EUS", 0.5)
call Ceiling("G_EUE", "LG_EUE/VV_EUE", "0.6", 0.1, 30)
call Link("IAGO_EUE","G_EUE", 0.5)
call Ceiling("G_UK", "LG_UK/VV_UK", "0.6", 0.1, 30)
call Link("IAGO_UK","G_UK", 0.5)
call Ceiling("G_EUW", "LG_EUW/VV_EUW", "0.6", 0.1, 30)
call Link("IAGO_EUW","G_EUW", 0.5)
call Ceiling("G_EUN", "LG_EUN/VV_EUN", "0.6", 0.1, 30)
call Link("IAGO_EUN","G_EUN", 0.5)

'--- Europe: limit external deficits of UK
call Floor("SP_UK", "CA$_UK/Y$_UK", "-0.05", 0.3, 20)

'--- negative investment impact of European crisis
IP_EUW_ins.fill(s) -0.04, -0.06, -0.06, -0.04, -0.04, -0.02 
IP_EUN_ins = 0.5*IP_EUW_ins

'--- blocks on intra-Europe migration
call Ceiling("NIMU_EUN","100*(NIMU_EUN/NE_EUN)","0.5",100,20)
call Ceiling("NIMU_EUW","100*(NIMU_EUW/NE_EUW)","0.0",100,20)
call Ceiling("NIMU_EUE","100*(NIMU_EUE/NE_EUE)","0.0",100,20)
call Ceiling("NIMU_EUS","100*(NIMU_EUS/NE_EUS)","0.5",100,20)
call Ceiling("NIMU_UK","100*(NIMU_UK/NE_UK)","0.2",100,20)

'--- US, OD and JA debt ceilings
call Ceiling("G_US","100*LG_US/VV_US(-1)","60",100,10)
call Link("IAGO_US","G_US", 0.5)
call Ceiling("G_OD","100*LG_OD/VV_OD(-1)","60",100,10)
call Link("IAGO_OD","G_OD", 0.5)
call Ceiling("G_JA","100*LG_JA/VV_JA(-1)","150",100,10)
call Link("IAGO_JA","G_JA", 0.5)

'--- appreciation of CN real exchange rate relative to US
call Target("rxu_cn", "@pc(rx_cn/rx_us)", "1", 0, 20)
call Limit (100, "rxu_cn")

'--- relocation of industries to IN and AMS
sxmu_AMS_ins = 0.05
sxmu_IN_ins = 0.05

'--- raw material exports from low income areas
BA0U_AFS_ins = 0.003
BA0U_AFN_ins = 0.5*BA0U_AFS_ins
BA0U_AMS_ins = 0.5*BA0U_AFS_ins

call Limit (95, "ALL")
'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %predict, "m=10000", 8)
call pEnd

endsub
