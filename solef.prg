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
include "zlibj"  ' additional library and presentation routines
call solEf
'------------------------------------------------------------------
subroutine solEf

%actual = "2014"

%graphs = "No"
%graphcomp = "No"
%markets = "No"
%tables = "No"
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
t_Settings(3,2) = "Ef"
t_Settings(4,2) = "Employment Stimulus"

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================
smpl %actual+1 %end

' Clear baseline austerity rules
call DropRules("G_EUC G_FR G_EUP G_ENC G_UK")

IP_EUC_ins = 0
IP_FR_ins = 0
IP_ENC_ins = 0
IP_EUP_ins = 0

'--- European budget
table t_MB
scalar nMB = 0

'--- Fiscal mechanism

call MBDef(t_MB, nMB, "EU", "EUC FR EUP", _
  "Private Wealth:WP_?; Income per capita:Y_?/N_?", _
  "Per capita:N_?;Employment support:NE_?(-1):0.85*NWP_?(-1):50000;", _
  "1.8 1.0 0.6", "1 1 1", "2" _
)

call MBBuild(m_wmef, t_MB, nMB, "EU","1 1.33 1.66 2 2.5 3", "60 40", "40 60")


'--- Debt transfers
call MBTrans(m_wmef, t_MB, nMB, "EU", "0 0 0.55", "2015", %actual)

' IP_EUE_ins=-0.01
' IP_EUN_ins = 0.00 
' 
' ' Mild wage rises, with a lag in EUW
 SP_EUC_ins=-0.002
 SP_EUC_ins.fill(s) 0, 0, 0, 0, 0, 0 , 0, -0.002, -0.002, -0.002
' 
' ' Saving increase in EUE to keep a lid on expansion
' SP_EUE_ins=0.02
' SP_EUE_ins.fill(s) 0.03, 0.05, 0.05, 0.03, 0.02
' 
' ' Lagged and mild investment stimulus in Core Eurozone
IP_EUC_ins=0.02
IP_EUC_ins.fill(s) 0, 0, 0, 0, 0.02
' 
' ' UK Exchange rate devaluation
' 
 call Target("rxu_UK", "rx_UK/rx_US", "0.88", 1, 10)
' 
' ' EUE Exchange rate foor
' call Floor("rxu_EUE", "rx_EUE/rx_US", "0.6 0.55 *0.55", 1, 20)
' 
' ' EUN Exchange rate ceiling
' call Ceiling("rxu_EUN", "rx_EUN/rx_US", "1.7", 1, 30)
' 
'
 call Floor("IP_UK", "IP_UK/VV_UK", "0.18", 0, 10)
 call Floor("IP_EUP", "IP_EUP/VV_EUP", "0.21", 0, 15)
 call Floor("IP_FR", "IP_FR/VV_FR", "0.21", 0, 15)
' call Floor("IP_EUN", "IP_EUN/VV_EUN", "0.19", 0, 10)
' call Target("IP_EUE", "IP_EUE/VV_EUE", "0.175", 0, 10)
' 
' call Floor("G_UK", "G_UK/VV_UK", "0.24", 0, 10)
call Floor("G_EUC", "G_EUC/VV_EUC", "0.23", 0, 10)
call Floor("G_EUP", "G_EUP/VV_EUP", "0.24", 0, 10)
' call Ceiling("G_EUN", "G_EUN/VV_EUN", "0.32", 0, 70)
' call Floor("G_EUE", "G_EUE/VV_EUE", "0.22", 0, 3)
' 
' '--- Revenue Increase US and Europe
call Target("YGD_EUC","(YG_EUC-YGADJ_EUC)/VV_EUC", ".20 .21 .22 .23 *.24", 0, 20)
' call Target("YG_EUS","(YG_EUS-YGADJ_EUS)/VV_EUS", ".20 .21 *.22", 0, 20)
' call Target("YG_EUW","(YG_EUW-YGADJ_EUW)/VV_EUW", ".21 .22 .23 *.24", 0, 30)
' call Target("YG_UK","(YG_UK-YGADJ_UK)/VV_UK", ".21 .22 .23 *.24", 0, 30)
' call Target("YG_EUN","(YG_EUN-YGADJ_EUN)/VV_EUN", ".30 .31 .32 *.33", 0, 30)
' call Target("YG_EUE","(YG_EUE-YGADJ_EUE)/VV_EUE", ".20 .21 *.22", 0, 10) 
' 
' Interest rate ceilings are in SOLEB and don't need to be restated

call Limit (95, "ALL")
'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call MBRep(t_MB, nMB, "EU", "Ef Federal Europe", %actual, %predict, 8)
call mk_extra_graphs("eb; Austerity: ef; Federal")
call pEnd

endsub
