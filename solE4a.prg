'PROGRAM: solE4a.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' Towards Federal Europe with multi-polar world
'
' The program reads SOLE4.wf1 and creates SOLE4a.wf1
'
' updated: FC 19/07/2012
'
' differences from SOLE4
'   global carbon tax
'   c/a ceilings for EUN EUW JA EAH
'   growth targets in low income G20
'   assistance for reserves in low income areas
'   fiscal targets for low income regions
'   investment in raw material exports from Africa
'==================================================================
' OPTIONS
'==================================================================
include "mb.prg"
include "set"
call solE4a
'------------------------------------------------------------------
subroutine solE4a

%actual = "2012"

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
'--- open the SOLE4 workfile
open SOLE4
pageselect graphs
delete *
pageselect tables
delete *
pageselect data
delete sp_log* m_wmE4
'--- update settings
call pLog("SOLE4a PROGRAM v0719")
%wkfile = "SOLE4a"
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "E4a"
t_Settings(4,2) = "Federal Europe with multi-polar world"
t_Settings(7,2) = %wkfile
%first = @str(@val(t_Settings(1,2))+1)
call CopyAliasValues("_" + t_Settings(5,2), "", %first, %actual)
t_Settings(1,2) = %actual

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end

'--- global carbon tax
series ttco2_w = @iif(@trend()<43,0,25*(@trend()-43))
p_Bloc.genr ttco2_?_ins = ttco2_w

'--- European budget
call MBBuild(m_wmE4a, t_MB, nMB, "EU","0.5 1 1.5 2 2.5 3 3.5 4 4.5 5", _
  "80 20", "30 50 20 20 30")

'--- c/a ceilings for EUN EUW JA EAH
call Ceiling("SP_EUN","100*CA$_EUN/Y$_EUN","2",100,10)
call Link("MM$_EUN","SP_EUN",-1)
call Link("BS$U_EUN","SP_EUN",1)
call Ceiling("SP_EUW","100*CA$_EUW/Y$_EUW","2",100,10)
call Link("MM$_EUW","SP_EUW",-1)
call Link("BS$U_EUW","SP_EUW",1)
call Ceiling("SP_JA","100*CA$_JA/Y$_JA","2",100,10)
call Link("MM$_JA","SP_JA",-1)
call Link("BS$U_JA","SP_JA",1)
call Ceiling("SP_EAH","100*CA$_EAH/Y$_EAH","2",100,10)
call Link("MM$_EAH","SP_EAH",-0.5)
call Link("BS$U_EAH","SP_EAH",0.5)
 
'--- growth targets in low income G20
call Target("IP_IN", "@pc(H_IN)", "8", 25, 50)
call Target("IP_EAO", "@pc(H_EAO)", "6", 25, 50)
call Target("IP_AMS", "@pc(H_AMS)", "6", 25, 50)
call Floor("G_IN","@pc(G_IN)", "9", 100, 100)
call Floor("G_AMS","@pc(G_AMS)", "6", 100, 100)

'--- assistance for reserves in low income regions
call Floor("R$_AFS","R$_AFS/(RX_AFS*VV_AFS)","0.2",0.1,30)
call Floor("R$_ASO","R$_ASO/(RX_ASO*VV_ASO)","0.2",0.1,30)

'--- fiscal targets for low income regions
call Floor("YG_AFN","YG_AFN/VV_AFN", ".15", 1, 30)
call Floor("G_AFN","@pc(G_AFN)", "7", 100, 100)
call Floor("YG_AFS","YG_AFS/VV_AFS", ".15", 1, 30)
call Floor("G_AFS","@pc(G_AFS)", "7", 100, 100)
call Floor("YG_ASO","YG_ASO/VV_ASO", ".15", 1, 30)
call Floor("G_ASO","@pc(G_ASO)", "7", 100, 100)

'--- raw material exports from Africa (restore)
BA0U_AFS_ins = 0.003

call Limit (95, "ALL")

'==================================================================
' PROCESSING
'================e==================================================

call pCheckSolveReport({%gm}, %actual, %predict, "m=30000", 8)
call MBRep(t_MB, nMB, "EU", "E4a Federal Europe", %actual, %predict, 8)
call pEnd

endsub

