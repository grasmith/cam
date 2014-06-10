'PROGRAM: solN3c.prg          Copyright (C) 2013 Alphametrics Co. Ltd.
'
' CAM version 5.0
'
' Reflation with industrial location policies
'
' The program reads SOLN2.wf1 and creates SOLN3c.wf1
'
' updated: FC 30/07/2013
'
' differences from SOLN2
'
' Europe
'  industrial location policies
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call solN3c
'------------------------------------------------------------------
subroutine solN3c

'--- projection period
%actual = "2013"

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
%wkfile = "SOLN3c"
call CreateModelFile("SOLN2", %wkfile) 
delete m_wmN2

'--- update settings
call pLog("SOLN3c PROGRAM v0920")
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "N3c"
t_Settings(4,2) = "Industrial location policies"

call pCreateScenario(%gm, %alias)

'==================================================================
' SCENARIO DEFINITIONS
'==================================================================

smpl %actual+1 %end

'--- weaker euro
rxu_de_ins = -0.02

'--- industrial location policies
'    growth targets for industrial exports
call Ceiling("sxmu_de", "@pc(xm$_de)", "3", 100, 50)
call Ceiling("sxmu_eun", "@pc(xm$_eun)", "3", 100, 50)
call Floor("sxmu_euw", "@pc(xm$_euw)", "3", 100, 50)
call Floor("sxmu_fr", "@pc(xm$_fr)", "6", 100, 50)
call Floor("sxmu_it", "@pc(xm$_it)", "6", 100, 50)
call Floor("sxmu_uk", "@pc(xm$_uk)", "6", 100, 50)
call Floor("sxmu_pl", "@pc(xm$_pl)", "6", 100, 50)
call Floor("sxmu_eue", "@pc(xm$_eue)", "6", 100, 50)
call Floor("sxmu_eus", "@pc(xm$_eus)", "6.5", 100, 50)
call Floor("sxmu_es", "@pc(xm$_es)", "7", 100, 50)

call Limit (95, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub
