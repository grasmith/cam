'PROGRAM: solE2F.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' Federal Europe with fiscal stimulus
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
include "zlibsoas"
include "mb"
call solE2F
'------------------------------------------------------------------
subroutine solE2F

%actual = "2013"

'%graphs = "Yes"
'%graphcomp = "Yes"
'%markets = "Yes"
'%tables = "Yes"

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
'--- open the SOLE1b workfile
open SOLE1b
pageselect graphs
delete *
pageselect tables
delete *
pageselect data
delete sp_log* m_wme1b
'--- update settings
call pLog("SOLE2F PROGRAM v0305")
%wkfile = "SOLE2Fr1"
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "E2F"
t_Settings(4,2) = "Employment Stimulus within Federal Europe"
t_Settings(7,2) = %wkfile
%first = @str(@val(t_Settings(1,2))+1)
call CopyAliasValues("_" + t_Settings(5,2), "", %first, %actual)
t_Settings(1,2) = %actual
call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================
smpl %actual+1 %end

' Clear baseline austerity rules
call DropRules("G_EUS G_EUE G_UK G_EUW G_US G_EUN G_CI")
call DropRules("YG_US YG_EUS YG_EUE YG_EUW YG_UK YG_EUN YG_OD YG_CI")
call DropRules("IP_EUW IP_EUN IP_EUS IP_US")
call DropRules("NIMU_EUN NIMU_EUW NIMU_EUE NIMU_EUS NIMU_UK")
call DropRules("G_OD G_JA")
call DropRules("sxmu_AMS_ins sxmu_IN_ins")
call DropRules("BA0U_AFS_ins BA0U_AFN_ins BA0U_AMS_ins")
call DropRules("G_CI YG_CI")

' Lose EUE from Eurozone
call DropRules("rxu_EUE pvi_EUE")

' Fix up IAGO
call DropRules("IAGO_EUS IAGO_EUE IAGO_UK IAGO_EUW IAGO_EUN")
call Fix("IAGO_EUS", "level", "0")
call Fix("IAGO_EUN", "level", "0")
call Fix("IAGO_EUW", "level", "0")
call Fix("IAGO_EUE", "level", "0")
call Fix("IAGO_UK", "level", "0")

'call Limit (100, "ALL")

'--- European budget
table t_MB
scalar nMB = 0


'--- Fiscal mechanism
call MBDef(t_MB, nMB, "EU", "EUW EUS UK EUN EUE", _
  "Private Wealth:WP_?; Income per capita:Y_?/N_?", _
  "Per capita:N_?;Employment support:NE_?(-1):0.85*NWP_?(-1):50000;", _
  "1.8 0.6 1.0 0.6 2.0", "1 1 1 1 1", "2" _
)

call MBBuild(m_wme2f, t_MB, nMB, "EU","1 1.33 1.66 2 2.5 3", "60 40", "40 60")

'--- Debt transfers
call MBTransfer(m_wme2f, t_MB, nMB, "EU", "0 0.55 0 0 0", "2015", %actual)

IP_EUE_ins=-0.01
IP_EUN_ins = 0.00 

' Mild wage rises, with a lag in EUW
SP_EUW_ins=-0.002
SP_EUW_ins.fill(s) 0, 0, 0, 0, 0, 0 , 0, -0.002, -0.002, -0.002

' Saving increase in EUE to keep a lid on expansion
SP_EUE_ins=0.02
SP_EUE_ins.fill(s) 0.03, 0.05, 0.05, 0.03, 0.02

' Lagged and mild investment stimulus in North Eurozone.
IP_EUW_ins=0.02
IP_EUW_ins.fill(s) 0, 0, 0, 0, 0.02

' UK Exchange rate devaluation
' Reduced from 0.75

call Target("rxu_UK", "rx_UK/rx_US", "0.88", 1, 10)

' EUE Exchange rate foor
call Floor("rxu_EUE", "rx_EUE/rx_US", "0.6 0.55 *0.55", 1, 20)
'call Floor("rxu_EUE", "rx_EUE/rx_US", "0.6 0.55 0.55 0.45 0.4 0.35 *0.3", 1, 20)

' EUN Exchange rate ceiling
call Ceiling("rxu_EUN", "rx_EUN/rx_US", "1.7", 1, 30)

'Previous targets
'call Floor("IP_UK", "IP_UK/VV_UK", "0.15", 0, 10)
'call Floor("IP_EUW", "IP_EUW/VV_EUW", "0.18", 0, 30)
'call Floor("IP_EUS", "IP_EUS/VV_EUS", "0.18", 0, 30)
'call Floor("IP_EUN", "IP_EUN/VV_EUN", "0.18", 0, 10)
'call Target("IP_EUE", "IP_EUE/VV_EUE", "0.18", 0, 10)

' New Targets
call Floor("IP_UK", "IP_UK/VV_UK", "0.18", 0, 10)
'call Floor("IP_EUW", "IP_EUW/VV_EUW", "0.18", 0, 5)
call Floor("IP_EUS", "IP_EUS/VV_EUS", "0.21", 0, 15)
call Floor("IP_EUN", "IP_EUN/VV_EUN", "0.19", 0, 10)
call Target("IP_EUE", "IP_EUE/VV_EUE", "0.175", 0, 10)

call Floor("G_UK", "G_UK/VV_UK", "0.24", 0, 10)
call Floor("G_EUW", "G_EUW/VV_EUW", "0.23", 0, 10)
call Ceiling("G_EUS", "G_EUS/VV_EUS", "0.26", 0, 10)
call Ceiling("G_EUN", "G_EUN/VV_EUN", "0.32", 0, 70)
call Floor("G_EUE", "G_EUE/VV_EUE", "0.22", 0, 3)

'--- Revenue Increase US and Europe
call Target("YG_EUS","(YG_EUS-YGADJ_EUS)/VV_EUS", ".20 .21 *.22", 0, 20)
call Target("YG_EUW","(YG_EUW-YGADJ_EUW)/VV_EUW", ".21 .22 .23 *.24", 0, 30)
call Target("YG_UK","(YG_UK-YGADJ_UK)/VV_UK", ".21 .22 .23 *.24", 0, 30)
call Target("YG_EUN","(YG_EUN-YGADJ_EUN)/VV_EUN", ".30 .31 .32 *.33", 0, 30)
'call Target("YG_EUE","(YG_EUE-YGADJ_EUE)/VV_EUE", ".20 .21 .22 .23 *.235", 0, 10) 
call Target("YG_EUE","(YG_EUE-YGADJ_EUE)/VV_EUE", ".20 .21 *.22", 0, 10) 

' Interest rate ceilings
'call Ceiling("im_UK", "irm_UK", "1.5", 0, 50)
'call Ceiling("im_EUW", "irm_EUW", "1.5", 0, 50)
'call Ceiling("im_EUS", "irm_EUS", "3", 0, 50)
'call Ceiling("im_EUN", "irm_EUN", "1.5", 0, 50)
'call Ceiling("im_EUE", "irm_EUE", "3.5", 0, 50)

'call Limit (95, "ALL")
'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %predict, "m=30000", 8)
call MBRep(t_MB, nMB, "EU", "e2f Baseline Federal Europe", %actual, %predict, 13)
call mk_soas_graphs("e1b; Austerity: e2f; Federal")
call pEnd

endsub
