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
' updated: GC 10/07/2012
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
include "set"
call solE2D
'------------------------------------------------------------------
subroutine solE2D

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

' Debt writeoffs
series debt_writedown=0
smpl 2017 2017
debt_writedown = 0.3

smpl %actual+1 %end
call zMBAppend(m_wm0p, "IAGOADJ_EUS", "-(debt_writedown*LG_EUS)")
call zMBAppend(m_wm0p, "IAGOADJ_EUW", "debt_writedown*LG_EUS")



call DropRules("G_EUS G_EUE G_UK G_EUW G_US G_EUN G_CI")
'call DropRules("SP_EUE IP_EUE SP_UK IP_UK")
call DropRules("IP_EUW IP_EUN")
'call DropRules("rxu_EUS rxu_EUE") 
'call DropRules("pvi_EUE")
call DropRules("G_EUS_a")
call DropRules("IAGO_EUN_a")
call DropRules("IAGO_US IAGO_EUS IAGO_EUE IAGO_UK IAGO_EUW IAGO_EUN")
'call DropRules("YG_US YG_EUS YG_EUE YG_EUW YG_UK YG_EUN YG_OD YG_CI")
call DropRules("NIMU_EUN NIMU_EUW NIMU_EUE NIMU_EUS NIMU_UK")
call DropRules("G_OD G_JA")
call DropRules("sxmu_AMS_ins sxmu_IN_ins")
call DropRules("BA0U_AFS_ins BA0U_AFN_ins BA0U_AMS_ins")
call DropRules("G_CI YG_CI")

'--- Employment target US and Europe
call Floor("G_US","(NE_US/NWP_US)", "0.74",0,10)
call Link("IP_US","G_US",0.5)
call Target("IP_EUS","(NE_EUS/NWP_EUS)", "0.68",0,10)
call Link("G_EUS","IP_EUS",0.5)
call Floor("G_EUW","(NE_EUW/NWP_EUW)", "0.73",0,20)
call Link("IP_EUW","G_EUW",0.4)
call Floor("G_UK","(NE_UK/NWP_UK)", "0.77",0,10)
call Link("IP_UK","G_UK",0.5)
call Target("G_EUN","(NE_EUN/NWP_EUN)", "0.76",0,20)
call Link("IP_EUN","G_EUN",0.2)
'call Target("G_EUE","(NE_EUE/NWP_EUE)", "0.70",0,20)
'call Link("IP_EUE","G_EUE",0.3)

'G_EUE_a=0.02

'---Ceiling on South Europe Debt
Call Target("IAGO_EUS","(LG_EUS/VV_EUS)","0.70",0,30)

'--- NFI bank lending stimulus
NFI_US_a = 0.1
NFI_EUS_a = 0.1
NFI_EUW_a = 0.1
NFI_UK_a = 0.1
'NFI_EUE_a = 0.1
NFI_EUN_a = 0.1

'--- Revenue Increase US and Europe
'Call Target("YG_US","YG_US/VV_US", ".18", 0, 20)
'call Target("YG_EUS","YG_EUS/VV_EUS", ".24", 0, 20)
'call Target("YG_EUW","YG_EUW/VV_EUW", ".24", 0, 20)
'call Target("YG_UK","YG_UK/VV_UK", ".22", 0, 20)
'call Target("YG_EUN","YG_EUN/VV_EUN", ".29", 0, 20)
'call Target("YG_EUE","YG_EUE/VV_EUE", ".21", 0, 20)

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
call pEnd

endsub
