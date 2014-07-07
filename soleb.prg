'PROGRAM: solEb.prg          Copyright (C) 2014 Alphametrics Co. Ltd.
'
' CAM version 5.2 FEPS EUR variant
'
' FEPS European Baseline scenario
'
' The program reads SOL0.wf1 and creates SOLEB.wf1
'
' differences from sol0
'   debt ceilings in Europe (cuts in government spending)

'==================================================================
' OPTIONS
'==================================================================
include "set"
call solEb
'------------------------------------------------------------------
subroutine solEb

%actual = "2014"

%graphs = "Yes"
%graphcomp = "Yes"
%markets = "No"
%tables = "Yes"
%analysis = "No"
%csv = "No"

'================================================================
' PREFACE
'==================================================================
mode quiet
%wkfile = "SOLEB"
call CreateModelFile("SOL0", %wkfile) 
delete m_wm0 *_0p

 '--- update settings
call pLog("SOLEb")
%wkfile = "SOLEb"
t_Settings(1,2) = %actual
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "Eb"
t_Settings(4,2) = "Austerity"

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end

'----- Drop East Europe inflation assumption
call DropRules("pvi_EUE")

'---- FIX IAGO
call Fix("IAGO_EUC", "level", "0")
call Fix("IAGO_FR", "level", "0")
call Fix("IAGO_EUP", "level", "0")
call Fix("IAGO_ENC", "level", "0")
call Fix("IAGO_UK", "level", "0")
call Fix("IAGO_ENE", "level", "0")

'---  cuts in government budgets (US, OD, EU)
'call Target("YGD_EUC","YGD_EUC/VV_EUC", ".15", 1, 30)
'call Target("YGD_FR", "YGD_FR/VV_FR", ".15", 1, 30)
'call Target("YGD_EUP","YGD_EUP/VV_EUP", ".12", 1, 30)
'call Target("YGD_ENC","YGD_ENC/VV_ENC", ".20", 1, 10)
'call Target("YGD_UK", "YGD_UK/VV_UK", ".12", 1, 30)
'call Target("YGD_ENE","YGD_ENE/VV_ENE", ".20", 1, 30)


'--- Europe: attempt at ceiling on debt ratios
call Target("G_EUC","LG_EUC/VV_EUC", ".5", 1, 5)
call Target("G_FR", "LG_FR/VV_FR",   ".6", 1, 30)
call Target("G_EUP","LG_EUP/VV_EUP", ".6", 1, 30)
call Target("G_ENC","LG_ENC/VV_ENC", ".6", 1, 30)
call Target("G_UK", "LG_UK/VV_UK",   ".6", 1, 30)
' Nothing for Eastern Europe?

'--- negative investment impact of European crisis
IP_EUC_ins.fill(s) -0.04, -0.06, -0.06, -0.04, -0.04, -0.02 
IP_FR_ins = 0.5*IP_EUC_ins
IP_ENC_ins = 0.5*IP_EUC_ins
IP_EUP_ins = 0.5*IP_EUC_ins

' Interest rate ceilings
call Ceiling("im_UK", "irm_UK", "1.5", 0, 90)
call Fix("im_EUC", "level", "1.5")
call Ceiling("im_FR", "irm_FR", "2.0", 0, 90)
call Ceiling("im_EUP", "irm_EUP", "3", 0, 90)
call Ceiling("im_ENC", "irm_ENC", "1.5", 0, 90)
call Ceiling("im_ENE", "irm_ENE", "3.5", 0, 90)

call Limit (99, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub
