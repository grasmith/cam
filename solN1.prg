'PROGRAM: solN1.prg          Copyright (C) 2013 Alphametrics Co. Ltd.
'
' CAM version 5.0
'
' Struggling on with reduced government
'
' The program reads SOL0.wf1 and creates SOLN1.wf1
'
' updated: FC 30/07/2013
'
' differences from sol0
'  US net revenue target, debt ceiling, devaluation,
'     energy saving
'  CN growth rate, budget balance, c/a balance, energy saving
'  OD and EU net revenue targets
'  OD, JA and EU debt ceilings
'  negative investment impact of EU crisis
'  more relocation of industry to India and S America
'  more raw material exports from Africa and S America
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call solN1
'------------------------------------------------------------------
subroutine solN1

'--- projection period
%actual = "2012"

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
%wkfile = "SOLN1"
call CreateModelFile("SOL0", %wkfile) 
delete m_wm0 *_0p

'--- update settings
call pLog("SOLN1 PROGRAM v3007")
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "N1"
t_Settings(4,2) = "Struggling on"
call pCreateScenario(%gm, %alias)

'==================================================================
' SCENARIO DEFINITIONS
'==================================================================

smpl %actual+1 %end

'--- US net revenue target
call Target("YGD_US","YG_US/VV_US", ".17", 1, 10)
'--- US debt ceiling
call Ceiling("G_US","LG_US/VV_US(-1)","0.7",0.1,10)
call Link("IAGO_US","G_US", 1)
'--- US devaluation
call Target("rxu_US", "@pc(rx_US)", "-2", 100, 100)
'--- US energy saving
ED_US_ins = -0.005

'--- CN growth adjustment
call Target("SP_CN", "@pc(V_CN)", "8 7.5 7 7 7 7 6.5 6.5 6.5 6.5 *6", -500, 20)
call Link("IP_CN","SP_CN",-1)
'--- CN budget balance
call Ceiling("G_CN", "NLG_CN/VV_CN", "0", -0.1, 15)
'--- CN c/a balance
call Target("rxu_CN", "CA$_CN/Y$_CN", "0", -0.1, 20)
'--- CN energy saving
call Target("ED_CN", "@pc(EPC_CN)", "1", 100, 30)
call Link("EPN_CN", "ED_CN", -1)

'---  OD and EU net revenue targets
for %b OD DE ES FR IT PL UK EUE EUS EUW
  call Target("YGD_"+%b,"YG_"+%b+"/VV_"+%b, ".20", 0.2, 10)
next
call Target("YGD_EUN","YG_EUN/VV_EUN", ".25", 0.2, 10)

'--- OD, JA and EU debt ceilings
call Ceiling("G_JA","LG_JA/VV_JA", "1.5", 0.1, 10)
call Link("IAGO_JA","G_JA", 0.5)
for %b OD DE ES FR IT PL UK EUE EUS EUW EUN
  call Ceiling("G_"+%b, "LG_"+%b+"/VV_"+%b, "0.6", 0.1, 20)
  call Link("IAGO_"+%b, "G_"+%b, 0.5)
next

'--- negative investment impact of European crisis
IP_EUW_ins = 0
IP_EUW_ins.fill(s) -0.04, -0.03, -0.02, -0.01 
for %b DE ES FR IT PL UK EUE EUS EUN
  IP_{%b}_ins = IP_EUW_ins
next

'--- relocation of industries to IN and AMS
sxmu_AMS_ins = 0.05
sxmu_IN_ins = 0.05

'--- raw material exports from low income areas
BA0U_AFS_ins = 0.003
BA0U_AFN_ins = 0.5*BA0U_AFS_ins
BA0U_AMS_ins = 0.5*BA0U_AFS_ins

call Limit (95, "ALL")
'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub
