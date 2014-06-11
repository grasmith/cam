'PROGRAM: solE2F.prg          Copyright (C) 2014 Alphametrics Co. Ltd.
'
' CAM version 5.2 FEPS EUR variant
'
' FEPS Federal Europe scenario
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
include "mb"
'include "zlibsoas"  ' additional presentation code
call solEf
'------------------------------------------------------------------
subroutine solEf

%actual = "2014"

%graphs = "Yes"
%graphcomp = "Yes"
%markets = "No"
%tables = "Yes"
%analysis = "No"
%csv = "No"

'==================================================================
' PREFACE
'==================================================================
mode quiet
%wkfile = "SOLEF"
call CreateModelFile("SOLEB", %wkfile) 
delete sp_log* m_wmeb *_0

'--- update settings
call pLog("SOLEf")
%wkfile = "SOLEf"
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "E2F"
t_Settings(4,2) = "Employment Stimulus"

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

'--- European budget
table t_MB
scalar nMB = 0

'--- Debt transfers
call MBTransfer(m_wme2f, "EU", "EUW EUS", "2015", %actual, "0.55", "1.0 0.0")


'--- Fiscal mechanism
call MBDef(t_MB, nMB, "EU", "EUW EUS", _
  "Private Wealth:WP_?; Income per capita:Y_?/N_?", _
  "Per capita:N_?;Dependents:NCP_?+NOP_?;" _
+ "Employment support:NE_?(-1):0.85*NWP_?(-1):50000;" _
+ "Deficit finance:NLG_?:0:1.65;", _
  "1 1", "1 1" _
)


call MBBuild(m_wme2f, t_MB, nMB, "EU","1 1.33 1.66 2 2.5 3 3.5 4 4.5 5", _
  "60 40", "20 20 40 20", "2")


' Government and investment stimulus

IP_EUE_ins=-0.01

SP_EUW_ins=-0.007
SP_EUW_ins.fill(s) 0, 0, 0, 0, -0.003, -0.004 '-0.007, -0.010, '-0.013

' UK Exchange rate devaluation
call Target("rxu_UK", "rx_UK/rx_US", "0.75", 1, 10)

' EUE Exchange rate foor
call Floor("rxu_EUE", "rx_EUE/rx_US", "0.6 0.6 0.6 0.55 0.55 0.55 0.50 0.50 0.50 0.48 0.48 0.45 0.45 *0.4", 1, 20)

' EUN Exchange rate ceiling
call Ceiling("rxu_EUN", "rx_EUN/rx_US", "1.7", 1, 30)

call Floor("IP_UK", "IP_UK/VV_UK", "0.15", 0, 10)
call Floor("IP_EUW", "IP_EUW/VV_EUW", "0.18", 0, 30)
call Floor("IP_EUS", "IP_EUS/VV_EUS", "0.18", 0, 30)
call Floor("IP_EUN", "IP_EUN/VV_EUN", "0.18", 0, 10)

call Floor("G_UK", "G_UK/VV_UK", "0.24", 0, 10)
call Floor("G_EUW", "G_EUW/VV_EUW", "0.245", 0, 10)
call Ceiling("G_EUS", "G_EUS/VV_EUS", "0.26", 0, 10)
call Ceiling("G_EUN", "G_EUN/VV_EUN", "0.32", 0, 70)
call Floor("G_EUE", "G_EUE/VV_EUE", "0.25", 0, 10)

'--- Revenue Increase US and Europe
call Target("YG_EUS","(YG_EUS-YGADJ_EUS)/VV_EUS", ".20 .21 *.22", 0, 20)
call Target("YG_EUW","(YG_EUW-YGADJ_EUW)/VV_EUW", ".21 .22 *.23", 0, 30)
call Target("YG_UK","YG_UK/VV_UK", ".21 .22 .23 *.24", 0, 30)
call Target("YG_EUN","YG_EUN/VV_EUN", ".29 .30 *.31", 0, 30)
call Target("YG_EUE","YG_EUE/VV_EUE", ".20 .21 .22 .23 *.24", 0, 10) 

call Limit (95, "ALL")
'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call MBRep(t_MB, nMB, "EU", "Ef Federal Europe", %actual, %predict, 8)
'call mk_soas_graphs("e1b; Austerity: e2f; Employment-focused")
call pEnd

endsub
