'PROGRAM: solE3.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' Multi-speed Europe with global regionalisation
'
' The program reads SOLE2.wf1 and creates SOLE3.wf1
'
' updated: FC 20/09/2012
'
' differences from SOLE2
'
' Europe
'  crawling peg exchange rate adjustment
'  restrictions on national budgets are removed
'  government spending and labour market policies to create jobs
'  investment incentives for S and E Europe
'  investment incentives and trade preferences for neighbours
'
' N America and E Asia: regional exchange rate management, trade
' preferences and investment stimulus
' Rest of world: regional exchange rate management, trade preferences
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call solE3
'------------------------------------------------------------------
subroutine solE3

%actual = "2012"

%graphs = "Yes"
%graphcomp = "Yes"
%markets = "No"
%tables = "No"
%analysis = "No"
%csv = "No"

'==================================================================
' PREFACE
'==================================================================
mode quiet
'--- open the SOLE2 workfile
open SOLE2
pageselect graphs
delete *
pageselect tables
delete *
pageselect data
delete sp_log* m_wme2
'--- update settings
call pLog("SOLE3 PROGRAM v0920")
%wkfile = "SOLE3"
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "E3"
t_Settings(4,2) = "Multi-speed Europe"
t_Settings(7,2) = %wkfile
%first = @str(@val(t_Settings(1,2))+1)
call CopyAliasValues("_" + t_Settings(5,2), "", %first, %actual)
t_Settings(1,2) = %actual

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end

'--- growth-orientated real exchange rates
call DropRules("rxu_EUN rxu_EUW rxu_EUE rxu_EUS rxu_UK")
rxu_EUN_ins = 0
rxu_EUW_ins = 0
rxu_EUE_ins = 0
rxu_EUS_ins = 0
rxu_UK_ins = 0
call Target("rxu_EUE", "rx_EUE/rx_EUW", "0.55", 1, 10)
call Target("rxu_EUN", "rx_EUN/rx_EUW", "1.20", 1, 10)
call Target("rxu_EUS", "rx_EUS/rx_EUW", "0.65", 1, 10)
call Target("rxu_UK", "rx_UK/rx_EUW", "0.80", 1, 10)

'--- European carbon taxes
series ttco2_EUW_ins = @iif(@trend()<43,0,25*(@trend()-43))
for %b EUN UK EUS EUE
  series ttco2_{%b}_ins = ttco2_EUW_ins
next

'--- budget rules (EUE, EUS and UK)
call DropRules("YG_EUN YG_EUW YG_EUE YG_EUS YG_UK")
call Target("YG_EUE","YG_EUE/VV_EUE", ".25", 1, 30)
call Target("YG_EUS","YG_EUS/VV_EUS", ".25", 1, 30)
call Target("YG_UK","YG_UK/VV_UK", ".25", 1, 30)

call DropRules("G_EUN G_EUW G_UK G_EUS G_EUE")
call DropRules("IAGO_EUN IAGO_EUW IAGO_UK IAGO_EUS IAGO_EUE")

'--- government spending and labour market policies to create jobs
call Floor("G_EUN", "NE_EUN/(NWP_EUN+NOP_EUN)", "0.60", 0.1, 10)
call Floor("G_EUW", "NE_EUW/(NWP_EUW+NOP_EUW)", "0.60", 0.1, 10)
call Floor("G_UK", "NE_UK/(NWP_UK+NOP_UK)", "0.58", 0.1, 10)
call Floor("G_EUS", "NE_EUS/(NWP_EUS+NOP_EUS)", "0.52", 0.1, 10)
call Floor("G_EUE", "NE_EUE/(NWP_EUE+NOP_EUE)", "0.54", 0.1, 10)
for %b EUN EUW UK EUS EUE
  call Link("NEAM_" + %b, "G_" + %b, 2)
  call Link("NEAF_" + %b, "G_" + %b, 2)
next

'--- Europe: investment and trade impact (financial instability)
IP_EUS_ins = 0
IP_EUS_ins.fill(s) -0.02, -0.05, -0.03, -0.01
IV_EUS_ins = 0.02*IP_EUS_ins
MM$_EUS_ins = 0
MM$_EUS_ins.fill(s) -0.02, -0.04, -0.02
for %b EUN EUW UK EUE
  IP_{%b}_ins = IP_EUS_ins
  IV_{%b}_ins = IV_EUS_ins
  MM$_{%b}_ins = MM$_EUS_ins
next

'--- longer-term investment stimulus in S and E Europe
IP_EUS_ins = 0.01
IP_EUS_ins.fill(s) -0.02, -0.05, -0.03, -0.01, 0
IP_EUE_ins = IP_EUS_ins

'--- drop impact of European collapse in US
IP_US_ins = 0
IV_US_ins = 0

'--- investment stimulus in neighbouring regions
IP_CI_ins = 0.02
IP_WA_ins = IP_CI_ins
IP_AFN_ins = IP_CI_ins

'--- trade preferences
sxmu_EUW_CI_ins = 0.03
sxmu_EUE_CI_ins = sxmu_EUW_CI_ins
sxmu_EUN_CI_ins = sxmu_EUW_CI_ins
sxmu_UK_CI_ins = sxmu_EUW_CI_ins
sxmu_EUS_CI_ins = sxmu_EUW_CI_ins
sxmu_CI_EUW_ins = 0.05
sxmu_CI_EUN_ins = sxmu_CI_EUW_ins
sxmu_CI_EUE_ins = sxmu_CI_EUW_ins
sxmu_CI_EUS_ins = sxmu_CI_EUW_ins
sxmu_CI_UK_ins = sxmu_CI_EUW_ins
sxmu_CI_AFN_ins = sxmu_CI_EUW_ins
sxmu_CI_WA_ins = sxmu_CI_EUW_ins
sxmu_EUW_WA_ins = 0.03
sxmu_EUE_WA_ins = sxmu_EUW_WA_ins
sxmu_EUN_WA_ins = sxmu_EUW_WA_ins
sxmu_UK_WA_ins = sxmu_EUW_WA_ins
sxmu_EUS_WA_ins = sxmu_EUW_WA_ins
sxmu_WA_EUW_ins = 0.05
sxmu_WA_EUN_ins = sxmu_EUW_WA_ins
sxmu_WA_EUE_ins = sxmu_EUW_WA_ins
sxmu_WA_EUS_ins = sxmu_EUW_WA_ins
sxmu_WA_UK_ins = sxmu_EUW_WA_ins
sxmu_WA_CI_ins = sxmu_EUW_WA_ins
sxmu_WA_WA_ins = sxmu_EUW_WA_ins
sxmu_WA_AFN_ins = sxmu_EUW_WA_ins
sxmu_EUW_AFN_ins = 0.03
sxmu_EUE_AFN_ins = sxmu_EUW_AFN_ins
sxmu_EUN_AFN_ins = sxmu_EUW_AFN_ins
sxmu_UK_AFN_ins = sxmu_EUW_AFN_ins
sxmu_EUS_AFN_ins = sxmu_EUW_AFN_ins
sxmu_AFN_EUW_ins = 0.05
sxmu_AFN_EUN_ins = sxmu_EUW_AFN_ins
sxmu_AFN_EUE_ins = sxmu_EUW_AFN_ins
sxmu_AFN_EUS_ins = sxmu_EUW_AFN_ins
sxmu_AFN_UK_ins = sxmu_EUW_AFN_ins
sxmu_AFN_CI_ins = sxmu_EUW_AFN_ins
sxmu_AFN_WA_ins = sxmu_EUW_AFN_ins
sxmu_AFN_AFN_ins = sxmu_EUW_AFN_ins

'--- N America (US, OD, ACX)
call DropRules("YG_US YG_OD")
'--- exchange rates
call Target("rxu_OD", "rx_OD/rx_US", "1.2", 1, 20)
call Target("rxu_ACX", "rx_ACX/rx_US", "0.7", 1, 20)
'--- investment stimulus
IP_US_ins = 0.01
IP_OD_ins = IP_US_ins + 0.01
IP_ACX_ins = IP_US_ins + 0.02
'--- trade preferences
sxmu_OD_US_ins = 0.07
sxmu_ACX_US_ins = sxmu_OD_US_ins
sxmu_US_OD_ins = sxmu_OD_US_ins
sxmu_OD_OD_ins = sxmu_OD_US_ins
sxmu_ACX_OD_ins = sxmu_OD_US_ins
sxmu_US_ACX_ins = sxmu_OD_US_ins
sxmu_OD_ACX_ins = sxmu_OD_US_ins
sxmu_ACX_ACX_ins = sxmu_OD_US_ins
sxmu_AMS_ACX_ins = sxmu_OD_US_ins

'--- E Asia (CN, JA, EAH, EAO)
'--- real exchange rates
call Target("rxu_JA", "rx_JA/rx_CN", "2.2", 1, 10)
call Target("rxu_EAH", "rx_EAH/rx_CN", "1.8", 1, 10)
call Target("rxu_EAO", "rx_EAO/rx_CN", "0.8", 1, 10)

'--- budget ceiling in EAO
call Ceiling("G_EAO", "NLG_EAO/VV_EAO", "0.02", -0.1, 15)

'--- investment stimulus
IP_JA_ins = IP_CN_ins + 0.01
IP_EAH_ins = IP_CN_ins + 0.01
IP_EAO_ins = IP_CN_ins + 0.02
'--- trade preferences
sxmu_EAH_CN_ins = 0.02
sxmu_EAH_EAH_ins = sxmu_EAH_CN_ins
sxmu_EAH_JA_ins = sxmu_EAH_CN_ins
sxmu_EAH_EAO_ins = sxmu_EAH_CN_ins
sxmu_EAO_CN_ins = sxmu_EAH_CN_ins
sxmu_EAO_EAH_ins = sxmu_EAH_CN_ins
sxmu_EAO_JA_ins = sxmu_EAH_CN_ins
sxmu_EAO_EAO_ins = sxmu_EAH_CN_ins
sxmu_JA_CN_ins = sxmu_EAH_CN_ins
sxmu_JA_EAH_ins = sxmu_EAH_CN_ins
sxmu_JA_EAO_ins = sxmu_EAH_CN_ins
sxmu_CN_EAH_ins = sxmu_EAH_CN_ins
sxmu_CN_JA_ins = sxmu_EAH_CN_ins
sxmu_CN_EAO_ins = sxmu_EAH_CN_ins

'--- Other Asia (CI, IN, ASO, WA)
'--- exchange rate management
call Target("rxu_IN", "rx_IN/rx_US", "0.4", 1, 20)
call Target("rxu_ASO", "rx_ASO/rx_IN", "0.9", 1, 20)
call Target("rxu_CI", "rx_CI/rx_EUW", "0.4", 1, 20)
call Target("rxu_WA", "rx_WA/rx_EUW", "0.5", 1, 20)
'--- trade preferences
sxmu_IN_ASO_ins = 0.03
sxmu_ASO_ASO_ins = 0.03
sxmu_CI_ASO_ins = 0.03
sxmu_WA_ASO_ins = 0.03
sxmu_ASO_IN_ins = 0.03
sxmu_CI_IN_ins = 0.03
sxmu_WA_IN_ins = 0.03
sxmu_IN_CI_ins = 0.05
sxmu_ASO_CI_ins = 0.05
sxmu_WA_CI_ins = 0.05
sxmu_IN_WA_ins = 0.08
sxmu_ASO_WA_ins = 0.08
sxmu_CI_WA_ins = 0.08
sxmu_WA_WA_ins = 0.08

'--- Rest of world (Africa, S America)
'--- exchange rate management
call Target("rxu_AFN", "rx_AFN/rx_EUS", "0.5", 1, 20)
call Target("rxu_AFS", "rx_AFS/rx_EUS", "0.5", 1, 20)
call Target("rxu_AMS", "rx_AMS/rx_US", "0.6", 1, 20)

'--- trade preferences
sxmu_AFN_AFN_ins = 0.05
sxmu_AFS_AFN_ins = 0.05
sxmu_AFN_AFS_ins = 0.05
sxmu_AFS_AFS_ins = 0.05
sxmu_AMS_AMS_ins = 0.08

'--- raw material exports from low income areas (cancel)
BA0U_AFS_ins = 0.0
BA0U_AFN_ins = 0.5*BA0U_AFS_ins
BA0U_AMS_ins = 0.5*BA0U_AFS_ins

call Limit (95, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %predict, "m=30000", 8)
call pEnd

endsub
