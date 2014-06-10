'PROGRAM: sol0f.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6 AUGUR variant
'
' Baseline projection (main version)
'
' Run this program after sola.prg (alignment).
' It gives a plain projection of the model that should be examined
' before defining the baseline in sol0.prg
'
' The program reads SOL.wf1 and creates SOL0F.wf1
'
' updated: FC 15/04/2012
'
'==================================================================
' OPTIONS
'==================================================================
'include "set"
'include "zlibsoas"
'include "zlibp"
'include "mb"
'call sol0f
'------------------------------------------------------------------
subroutine sol0f

%graphs = "Yes"
%graphcomp = "Yes"
%markets = "No"
%tables = "No"
%analysis = "No"
%csv = "No"

%repstart = "1980"

'==================================================================
' PREFACE
'==================================================================
mode quiet

'--- open the SOLA workfile (plain baseline)
open SOL0
pageselect graphs
delete *
pageselect tables
delete *
pageselect data
delete sp_log* m_wm0 t_Rule* nRule*

'--- update settings
call pLog("SOL0F PROGRAM v0415")
%wkfile = "SOL0F"
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "0f"
t_Settings(4,2) = "baseline federal europe"
t_Settings(7,2) = %wkfile
call pCreateScenario(%gm, %alias)



'==================================================================
' DEFINITIONS
'==================================================================

'--- last actual (new solution starts after this year)
%actual = t_Settings(1,2)

smpl %actual+1 %end

'--- European budget
table t_MB
scalar nMB = 0

'--- global carbon tax
series ttco2_w = @iif(@trend()<43,0,25*(@trend()-43))

'--- Debt transfers
call MBTransfer(m_wm0f, "EU", "EUW EUN EUS", "2015", %actual, "60", "0.5 1.0")

'--- Fiscal mechanism
call MBDef(t_MB, nMB, "EU", "EUW EUN EUS", _
  "Income Tax:YP_?;Carbon tax:CO2_?*ttco2_w::0.5", _
  "Employment support:NE_?(-1):0.8*NWP_?(-1):40000;" _
  + "Deficit finance:NLG_?:0:1" _
  )
call MBBuild(m_wm0f, t_MB, nMB, "EU","1 2 3 4 5 6 7 8 9 10", _
  "70 30", "50 100")

call Fix("IAGO_EUS", "level", "0")
call Fix("IAGO_EUW", "level", "0")
call Fix("IAGO_EUN", "level", "0")
call Fix("IAGO_EUE", "level", "0")

'series debt_writedown=0
'smpl 2015 2020
''%shite = "0.1,0.1,0.1,0.1,0.1"
''
'debt_writedown.fill(s) 0.1,0.1,0.1,0.1,0.1
''debt_writedown.fill(s) {%shite}
''
'smpl %actual+1 %end
'call zMBAppend(m_wm0pj, "LGADJ_EUS", "-(debt_writedown*LG_EUS)")
'call zMBAppend(m_wm0pj, "LGADJ_EUW", "debt_writedown*LG_EUS")
''call zMBAppend(m_wm0p, "BITADJ$_EUS", "(debt_writedown*LG_EUS)")
''call zMBAppend(m_wm0p, "BITADJ$_EUW", "-(debt_writedown*LG_EUS)")


'==================================================================
' PROCESSING
'==================================================================

'%graphcomp = "No"
call pCheckSolveReport({%gm}, %actual, %predict, "m=10000", 8)
call MBRep(t_MB, nMB, "EU", "0f Baseline Federal Europe", %actual, %predict, 8)
'call mk_soas_graphs("e0p; plain baseline: e0pj; modified plain baseline")
call pEnd

endsub
