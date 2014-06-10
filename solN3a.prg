'PROGRAM: solN3a.prg          Copyright (C) 2013 Alphametrics Co. Ltd.
'
' CAM version 5.0
'
' Reflation with super-euro
'
' The program reads SOLN2.wf1 and creates SOLN3a.wf1
'
' updated: FC 30/07/2013
'
' differences from SOLN2
'
' Europe
'  super-euro
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call solN3a
'------------------------------------------------------------------
subroutine solN3a

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
%wkfile = "SOLN3a"
call CreateModelFile("SOLN2", %wkfile) 
delete m_wmN2

'--- update settings
call pLog("SOLN3a PROGRAM v0920")
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "N3a"
t_Settings(4,2) = "Super-euro"

call pCreateScenario(%gm, %alias)

'==================================================================
' SCENARIO DEFINITIONS
'==================================================================

smpl %actual+1 %end

'--- cancel existing euro zone exchange rate system
for %b FR IT ES PL EUW EUS EUE
  call DropRules("rxu_" + %b)
next
'--- new system: PL fix exchange rates vs Germany
call Target("rxu_PL", "rxna_PL-rxna_DE", "rxna_tar_PL", 100,100)
'--- change trend for Germany
rxu_DE_ins = 0.01
rxu_DE_ins.fill(s) 0.05, 0.02
'--- new system: others fix exchange rates vs France
for %b IT ES EUW EUS EUE
  call Target("rxu_" + %b, "rxna_" + %b + "-rxna_FR", "rxna_tar_" + %b, 100,100)
next
'--- change trend for France
rxu_FR_ins = -rxu_DE_ins
call Limit(100, "ALL")

'--- new eurozone exchange rate targets
rxna_tar_pl = 0
rxna_tar_it = 0
rxna_tar_es = 0
rxna_tar_euw = 0
rxna_tar_eus = 0
rxna_tar_eue = 0

call Limit (95, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub
