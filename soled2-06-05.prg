'PROGRAM: solE2D.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6 - Francis Updated Version 16 May 2012
'
' Increased US and European Spending 
' Employment target
' RER Adjuetment
' Government Income increases
'
' The program reads SOLE2b.wf1 and creates solED2.wf1
'
' updated: GC HB TM 30/05/2013
'
'  differences from solE1b
'   Growth of G of US and European blocks linked to Employment Rate
'   Sustained real devaluation in US, EUS, EUE, EUW
'  Sustained real appreciation in EUC and EUN
'   NFI growth - Investment Stimulus
' Ceiling of Government Debt in South Europe

'==================================================================
' OPTIONS
'==================================================================
'include "set"
include "zlibsoas"
'call solE2D
'------------------------------------------------------------------
subroutine solE2D

%actual = "2012"

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
'--- open the SOLE1b workfile
open SOLE1b
pageselect graphs
delete *
pageselect tables
delete *
pageselect data
delete sp_log* m_wme1b
'--- update settings
call pLog("SOLE2D PROGRAM v0305")
%wkfile = "SOLE2D"
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "E2D"
t_Settings(4,2) = "Employment Stimulus within the Eurozone"
t_Settings(7,2) = %wkfile
%first = @str(@val(t_Settings(1,2))+1)
call CopyAliasValues("_" + t_Settings(5,2), "", %first, %actual)
t_Settings(1,2) = %actual
call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================
smpl %actual+1 %end

call DropRules("G_EUS G_EUE G_UK G_EUW G_US G_EUN G_CI")
'call DropRules("SP_EUE IP_EUE SP_UK IP_UK")
call DropRules("IP_EUW IP_EUN")
call DropRules("IP_EUS")
'call DropRules("rxu_EUS rxu_EUE") 
'call DropRules("pvi_EUE")
call DropRules("IAGO_US IAGO_EUS IAGO_EUE IAGO_UK IAGO_EUW IAGO_EUN")
call DropRules("YG_US YG_EUS YG_EUE YG_EUW YG_UK YG_EUN YG_OD YG_CI")
call DropRules("NIMU_EUN NIMU_EUW NIMU_EUE NIMU_EUS NIMU_UK")
call DropRules("G_OD G_JA")
call DropRules("sxmu_AMS_ins sxmu_IN_ins")
call DropRules("BA0U_AFS_ins BA0U_AFN_ins BA0U_AMS_ins")
call DropRules("G_CI YG_CI")

'---- FIX IAGO
call Fix("IAGO_EUS", "level", "0")
call Fix("IAGO_EUN", "level", "0")
call Fix("IAGO_EUW", "level", "0")
call Fix("IAGO_EUE", "level", "0")
call Fix("IAGO_UK", "level", "0")


'--- global carbon tax
series ttco2_w = @iif(@trend()<43,0,25*(@trend()-43))

'--- Debt transfers
call MBTransfer(m_wm0f, "EU", "EUW EUN EUS", "2015", %actual, "60", "0.3 0.5 0.8 1.0")

'--- Fiscal mechanism
call MBDef(t_MB, nMB, "EU", "EUW EUN EUS", _
  "Income Tax:YP_?;Carbon tax:CO2_?*ttco2_w::0.5", _
  "Employment support:NE_?(-1):0.8*NWP_?(-1):40000;" _
  + "Deficit finance:NLG_?:0:1" _
  )
call MBBuild(m_wm0f, t_MB, nMB, "EU","1 2 3 4 5 6 7 8 9 10", _
  "70 30", "50 100")



'--- Employment target US and Europe
call Floor("G_US","(NE_US/NWP_US)", "0.74",0,20)
call Link("IP_US","G_US",1)
call Floor("G_EUS","(NE_EUS/NWP_EUS)", "0.56 0.57 0.58 0.59 0.60 *0.61",0, 20)
'call Link("IP_EUS","G_EUS",2)
IP_EUS_ins=0.02
IP_EUS_ins.fil(s)=0.01,0.01,0.01,0.02,0.02 
call Floor("G_EUW","(NE_EUW/NWP_EUW)", "0.67 0.675 0.68 0.685 0.69 0.695 0.70 0.705 *0.71",0,30)
call Link("IP_EUW","G_EUW",1)
'IP_EUW_ins=0.01
call Floor("G_UK","(NE_UK/NWP_UK)", "0.68 0.685 0.69 0.695 0.70 0.705 *0.71",0,10)
'call Link("IP_UK","G_UK",1.5)
IP_UK_ins=0.02
IP_UK_ins.fil(s)=0.01,0.01,0.01,0.02,0.02
call Floor("G_EUN","(NE_EUN/NWP_EUN)", "0.71 0.72 0.73 0.74 0.75 0.76 *0.77",0,30)
'call Link("IP_EUN","G_EUN",1)
IP_EUN_ins=0.01
call Target("G_EUE","(NE_EUE/NWP_EUE)", "0.58 0.59 0.60 0.61 0.62 0.63 *0.64",0,20)
IP_EUE_ins=0.02
'call Link("IP_EUE","G_EUE",0.3)
'G_EUE_ins=0.01


'---Ceiling on South Europe Debt
'Call Target("IAGO_EUS","(LG_EUS/VV_EUS)","0.70",0,30)

'--- NFI bank lending stimulus
'NFI_US_a = 0.3
'NFI_EUS_a = 0.3
'NFI_EUW_a = 0.3
'NFI_UK_a = 0.3
'NFI_EUE_a = 0.1
'NFI_EUN_a = 0.3

'--- Revenue Increase US and Europe
Call Target("YG_US","YG_US/VV_US", ".18", 0, 20)
call Target("YG_EUS","YG_EUS/VV_EUS", ".20 .21 *.22", 0, 20)
call Target("YG_EUW","YG_EUW/VV_EUW", ".21 .22 *.23", 0, 30)
call Target("YG_UK","YG_UK/VV_UK", ".21 .22 *.23", 0, 20)
call Target("YG_EUN","YG_EUN/VV_EUN", ".29 .30 *.31", 0, 30)
call Target("YG_EUE","YG_EUE/VV_EUE", ".17 .18 .19 .20 .21 *.22", 0, 10)

'--- RER Adjustment US and Europe
'call Ceiling("rxu_US","rx_US","1.0", 0, 20)
'call Target("rxu_EUS","rx_EUS","0.85", 0, 20)
'call Target("rxu_UK","rx_UK","0.9", 0, 20)
'call Target("rxu_EUW","rx_EUW","1.5", 0, 20)
'call Target("rxu_EUN","rx_EUN","1.75", 0, 20)
'call Target("rxu_EUE","rx_EUE","0.6", 0, 20)

call Limit (95, "ALL")
'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %predict, "m=30000", 8)
call MBRep(t_MB, nMB, "EU", "0f Baseline Federal Europe", %actual, %predict, 8)
'call mk_soas_graphs("e1b; soas baseline: e2d; soas expansion in ez")
call pEnd

endsub
