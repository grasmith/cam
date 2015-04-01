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
call DropRules("IP_EUC IP_FR IP_EUP IP_ENC IP_UK")

IP_EUC_ins = 0
IP_FR_ins = 0
IP_ENC_ins = 0
IP_EUP_ins = 0

' ---- Reduction in government income
YGD_FR_ins=-0.00
YGD_EUP_ins=-0.00
YGD_EUC_ins=-0.00
YGD_UK_ins=-0.00
YGD_ENE_ins=-0.00


'--- European budget
table t_MB
scalar nMB = 0

'--- Fiscal mechanism

call MBDef(t_MB, nMB, "EU", "EUC FR EUP", _
  "Private Wealth:WP_?; Income per capita:Y_?/N_?", _
  "Per capita:N_?;Employment support:NE_?(-1):0.85*NWP_?(-1):50000;", _
  "1.8 1.0 0.6", "1 1 1", "1.5" _
)
call MBBuild(m_wmef, t_MB, nMB, "EU","1 1.33 1.66 2 2.5 3", "60 40", "40 60")

'--- Debt transfers
call MBTrans(m_wmef, t_MB, nMB, "EU", "0 0 0.55", "2015", %actual)
' 
' ' Mild wage rises, with a lag in EUW
 SP_EUC_ins=-0.002
 SP_EUC_ins.fill(s) 0, 0, 0, 0, 0, 0 , 0, -0.002, -0.002, -0.002
' 
' SP_EUP_ins=-0.01
' SP_EUP_ins.fill(s) 0, 0, 0, 0, 0, 0 , 0, -0.002, -0.005, -0.01

' SP_FR_ins=-0.01
' SP_FR_ins.fill(s) 0, 0, 0, 0, 0, 0 , 0, -0.002, -0.002, -0.002
' 
' ' Lagged and mild investment stimulus in Core Eurozone
IP_EUC_ins=0.02
IP_EUC_ins.fill(s) 0, 0, 0, 0, 0.02
' 
IP_UK_ins=0.06
IP_UK_ins.fill(s) 0, 0, 0, 0, 0.01, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06

IP_EUP_ins=0.04
IP_EUP_ins.fill(s) 0, 0, 0, 0.01, 0.01, 0.02, 0.03, 0.04, 0.04, 0.04

IP_FR_ins=0.06
IP_FR_ins.fill(s) 0, 0, 0, 0.01, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06

' ' UK Exchange rate devaluation
 call Target("rxu_UK", "rx_UK/rx_US", "0.80", 1, 10)

' '--- Revenue Increase US and Europe
call Target("YGD_EUC","(YG_EUC-YGADJ_EUC)/VV_EUC", ".20 .21 .22 .23 *.24", 0, 20)
call Target("YGD_FR","(YG_FR-YGADJ_FR)/VV_FR", ".20 .21 .22 .23 .24 *.25", 0, 20)
call Target("YGD_UK","(YG_UK-YGADJ_UK)/VV_UK", ".21 .22 .23 *.23", 0, 30)

'call DropRules("im_UK im_EUC im_FR im_EUP im_ENC im_ENE")
call Ceiling("ei_UK", "irm_UK", "1.7", 0, 90)
call Ceiling("ei_EUC", "irm_EUC", "1.0", 0, 90)
call Ceiling("ei_FR", "irm_FR", "1.5", 0, 90)
call Ceiling("ei_EUP", "irm_EUP", "2.0", 0, 90)
call Ceiling("ei_ENC", "irm_ENC", "1.5", 0, 90)
call Ceiling("ei_ENE", "irm_ENE", "3.0", 0, 90)





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
