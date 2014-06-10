'PROGRAM: solE1.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' Struggling on with reduced government
'
' The program reads SOL0.wf1 and creates SOLE1.wf1
'
' updated: FC 17/05/2012
'
' differences from sol0
'   government budget cuts in US, OD and EU (net revenue limits)
'   debt ceilings in Europe (cuts in government spending)
'   negative impact of EU crisis on investment in EUW and EUN
'   debt ceilings in the US, OD and JA (cuts in govt spending)
'   China: balanced budget and real appreciation vs US
'   CIS ceiling on budget surplus
'   more relocation of industry to India and S America
'   more raw material exports from Africa and S America
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call solE1
'------------------------------------------------------------------
subroutine solE1

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
call pLog("SOLE1 PROGRAM v0517")
%wkfile = "SOLE1"
t_Settings(1,2) = %actual
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "E1"
t_Settings(4,2) = "Struggling on"
t_Settings(7,2) = %wkfile
call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end

'---  cuts in government budgets (US, OD, EU)
call Target("YG_US","YG_US/VV_US", ".15", 1, 30)
call Target("YG_OD","YG_OD/VV_OD", ".20", 1, 30)
call Target("YG_EUW","YG_EUW/VV_EUW", ".20", 1, 30)
call Target("YG_EUE","YG_EUE/VV_EUE", ".20", 1, 30)
call Target("YG_EUS","YG_EUS/VV_EUS", ".20", 1, 30)
call Target("YG_UK","YG_UK/VV_UK", ".20", 1, 30)
'--- slow implementation and higher target for EUN
call Target("YG_EUN","YG_EUN/VV_EUN", ".25", 1, 10)

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

'--- negative investment impact of European crisis
IP_EUW_ins.fill(s) -0.04, -0.06, -0.06, -0.04, -0.04, -0.02 
IP_EUN_ins = 0.5*IP_EUW_ins

'--- US, OD and JA debt ceilings
call Ceiling("G_US","100*LG_US/VV_US(-1)","50",100,10)
call Link("IAGO_US","G_US", 0.5)
call Ceiling("G_OD","100*LG_OD/VV_OD(-1)","60",100,10)
call Link("IAGO_OD","G_OD", 0.5)
call Ceiling("G_JA","100*LG_JA/VV_JA(-1)","150",100,10)
call Link("IAGO_JA","G_JA", 0.5)

'--- real appreciation of CN exchange rate relative to US
call Target("rxu_cn", "@pc(rx_cn/rx_us)", "1", 100, 20)
call Limit (100, "rxu_cn")
'--- CN budget balance
call Ceiling("G_CN", "NLG_CN/VV_CN", "0", -0.1, 15)

'--- CIS restrict budget surplus
call Ceiling("G_CI", "NLG_CI/VV_CI", "0.05", -0.1, 15)
call Link("YG_CI", "G_CI", -1)

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
