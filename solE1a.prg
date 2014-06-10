'PROGRAM: solE1a.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' Struggling on with US-China hegemony
'
' The program reads SOLE1.wf1 and creates SOLE1a.wf1
'
' updated: FC 20/09/2012
'
' differences from SOLE1
'   China: growth slowdown, c/a balance, energy saving
'   US: devaluation, looser budget targets, energy saving
'   cancel additional relocation of industry to AMS and IN
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call solE1a
'------------------------------------------------------------------
subroutine solE1a

%actual = "2013"

%graphs = "Yes"
%graphcomp = "Yes"
%markets = "Yes"
%tables = "No"
%analysis = "No"
%csv = "No"

'==================================================================
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
delete sp_log* m_wmE1
'--- update settings
call pLog("SOLE1a PROGRAM v0920")
%wkfile = "SOLE1a"
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "E1a"
t_Settings(4,2) = "Struggling on with US-China hegemony"
t_Settings(7,2) = %wkfile
%first = @str(@val(t_Settings(1,2))+1)
call CopyAliasValues("_" + t_Settings(5,2), "", %first, %actual)
t_Settings(1,2) = %actual
call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================
smpl %actual+1 %end

'--- CN growth adjustment
call Target("SP_CN", "@pc(V_CN)", "8 7.5 7 7 7 7 6.5 6.5 6.5 6.5 *6", -500, 20)
call Link("IP_CN","SP_CN",-1)
'--- CN c/a balance
call DropRules("rxu_CN")
call Target("rxu_CN", "CA$_CN/Y$_CN", "0", -0.1, 20)
'--- CN energy saving
call Target("ED_CN", "@pc(EPC_CN)", "1", 100, 30)
call Link("EPN_CN", "ED_CN", -1)

'--- US devaluation
call Target("rxu_US", "@pc(rx_US)", "-2", 100, 100)
'--- US debt and revenue targets relaxed a bit
call DropRules("G_US IAGO_US YG_US")
call Ceiling("G_US","100*LG_US/VV_US(-1)","70",100,10)
call Link("IAGO_US","G_US", 1)
call Target("YG_US","YG_US/VV_US", ".18", 1, 30)
'--- US energy saving
ED_US_ins = -0.005

'--- relocation of industries to IN and AMS (cancel)
sxmu_AMS_ins = 0
sxmu_IN_ins = 0

call Limit (95, "ALL")
'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %predict, "m=30000", 8)
call pEnd

endsub

