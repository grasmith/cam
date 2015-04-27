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
include "zlibj"  ' additional library and presentation routines
call solEb
'------------------------------------------------------------------
subroutine solEb

%actual = "2014"

%graphs = "Yes"
%graphcomp = "No"
%markets = "Yes"
%tables = "Yes"
%analysis = "Yes"
%csv = "Yes"

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
t_Settings(4,2) = "Secular Stagnation"

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end


'---- FIX IAGO
call Fix("IAGO_EUC", "level", "0")
call Fix("IAGO_FR", "level", "0")
call Fix("IAGO_EUP", "level", "0")
call Fix("IAGO_ENC", "level", "0")
call Fix("IAGO_UK", "level", "0")
call Fix("IAGO_ENE", "level", "0")

call DropRules("G_EUC G_FR G_EUP G_ENC G_UK")
call DropRules("IP_CN_a")

'--- Europe: attempt at cutting government expenditure
call Ceiling("G_EUC","G_EUC/VV_EUC", "0.23 0.225 *0.22", 0, 30)
call Ceiling("G_FR", "G_FR/VV_FR",   "0.27 0.265 *0.26", 0, 30)
call Ceiling("G_EUP","G_EUP/VV_EUP", "0.22 0.215 *0.21", 0, 30)
call Ceiling("G_ENC","G_ENC/VV_ENC", "0.24", 0, 30)
call Ceiling("G_UK", "G_UK/VV_UK", "0.23 0.225 0.22 *0.215", 0, 30)

' --- Adjustments in trends of private investment
call Floor("IP_EUP","IP_EUP/VV_EUP", "0.17 0.175 0.18 0.185 0.185 0.18 0.175 *0.17", 0, 20)
call Floor("IP_FR","IP_FR/VV_FR", "0.175 0.18 0.185 0.185 0.18 0.175 *0.17", 0, 30)
call Floor("IP_EUC","IP_EUC/VV_EUC", " 0.17 0.175 0.18 0.18 0.18 0.175 *0.17", 0, 30)
call Floor("IP_UK","IP_UK/VV_UK", "0.15 0.155 0.16 0.165 *0.16", 0, 30)

' Interest rate ceilings
call Ceiling("im_UK", "irm_UK", "1.7", 0, 90)
call Ceiling("im_EUC", "irm_EUC", "0.8", 0, 90)
call Ceiling("im_FR", "irm_FR", "1.3", 0, 90)
call Ceiling("im_EUP", "irm_EUP", "2.1", 0, 90)
call Ceiling("im_ENC", "irm_ENC", "1.1", 0, 90)
call Ceiling("im_ENE", "irm_ENE", "2.9", 0, 90)
'call Fix("im_EUC", "level", "1.5") 

' ---- Reduction in government income
YGD_FR_ins=-0.02
YGD_EUP_ins=-0.03
YGD_EUC_ins=-0.01
YGD_UK_ins=-0.02
YGD_ENE_ins=-0.02

'Slowdown in Energy demand China
ED_CN_ins=-0.05

' Euro depreciation (due to QE?)

'rxu_EUC_ins = -0.1
'call Target("rxu_EUC", "rx_EUC/rx_US", "1.10", 1, 90)

call Limit (99, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
'call mk_extra_graphs("eb; Austerity: ef; Federal")
call pEnd

endsub
