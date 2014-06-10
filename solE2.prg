'PROGRAM: solE2.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' Reduced government with EU breakup from 2014
'
' The program reads SOLE1.wf1 and creates SOLE2.wf1
'
' updated: FC 20/09/2012
'
' differences from solE1
'   EU breakup: all EU currencies fall
'   tighter debt ceilings
'   relax constraints on government income
'   investment and trade collapse in Europe
'   investment impact in US
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call solE2
'------------------------------------------------------------------
subroutine solE2

%actual = "2013"

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
'--- open the SOLE1 workfile
open SOLE1
pageselect graphs
delete *
pageselect tables
delete *
pageselect data
delete sp_log* m_wme1
'--- update settings
call pLog("SOLE2 PROGRAM v0920")
%wkfile = "SOLE2"
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "E2"
t_Settings(4,2) = "EU breakup"
t_Settings(7,2) = %wkfile
%first = @str(@val(t_Settings(1,2))+1)
call CopyAliasValues("_" + t_Settings(5,2), "", %first, %actual)
t_Settings(1,2) = %actual

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

'--- scenario assumptions
smpl %actual+1 %end

'--- EUR breakup
call DropRules("rxu_EUS rxu_EUE pvi_EUE")
'--- all EU currencies fall
rxu_EUW_ins = 0
rxu_EUW_ins.fill(s) -0.10,-0.04
rxu_EUN_ins = 0.3*rxu_EUW_ins
rxu_EUS_ins = 1.5*rxu_EUW_ins
rxu_UK_ins = rxu_EUW_ins
'--- EUE keeps cost alignment with EUW
call Target("rxu_EUE", "@pc(rx_EUE/rx_EUW)", "0", 100, 30)

'--- Europe: tighter budget ceilings
call DropRules("G_EUN G_EUW G_EUE G_EUS G_UK")
call DropRules("IAGO_EUN IAGO_EUW IAGO_EUE IAGO_EUS IAGO_UK")
call Ceiling("G_EUS", "LG_EUS/VV_EUS", "0.4", 0.1, 30)
call Link("IAGO_EUS","G_EUS", 0.5)
call Ceiling("G_EUE", "LG_EUE/VV_EUE", "0.4", 0.1, 30)
call Link("IAGO_EUE","G_EUE", 0.5)
call Ceiling("G_UK", "LG_UK/VV_UK", "0.5", 0.1, 30)
call Link("IAGO_UK","G_UK", 0.5)
call Ceiling("G_EUW", "LG_EUW/VV_EUW", "0.5", 0.1, 30)
call Link("IAGO_EUW","G_EUW", 0.5)
'--- exception: limit spending as share of GDP
call Ceiling("G_EUN", "G_EUN/VV_EUN(-1)", "0.3", 0.5, 30)

'--- Europe: drop limits on government income (except EUE)
call DropRules("YG_EUN YG_EUW YG_EUS YG_UK")

'--- Europe: investment and trade collapse
IP_EUS_ins = 0
IP_EUS_ins.fill(s) -0.4, -0.3, -0.2, -0.1, -0.05
IV_EUS_ins = 0.02*IP_EUS_ins
MM$_EUS_ins = 0
MM$_EUS_ins.fill(s) -0.1, -0.05
for %b EUN EUW UK EUE
  IP_{%b}_ins = IP_EUS_ins
  IV_{%b}_ins = IV_EUS_ins
  MM$_{%b}_ins = MM$_EUS_ins
next

'--- US: impact of European collapse
IP_US_ins = 0.2*IP_EUS_ins
IV_US_ins = 0.2*IV_EUS_ins

smpl %actual+1 %end

call Limit (95, "ALL")
'==================================================================
' PROCESSING
'==================================================================

'--- don't set pre-solution values (excluded) from actuals
call pCheckSolveReport({%gm}, %actual, %predict, "m=10000", 8)
call pEnd

endsub
