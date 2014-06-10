'PROGRAM: sol0p.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6 AUGUR variant
'
' Baseline projection (main version)
'
' Run this program after sola.prg (alignment).
' It gives a plain projection of the model that should be examined
' before defining the baseline in sol0.prg
'
' The program reads SOLA.wf1 and creates SOL0P.wf1
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
'call sol0pj
'------------------------------------------------------------------
subroutine sol0pj

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
open SOL0P
pageselect graphs
delete *
pageselect tables
delete *
pageselect data
delete sp_log* m_wm0p t_Rule* nRule*

'--- update settings
call pLog("SOL0PJ PROGRAM v0415")
%wkfile = "SOL0PJ"
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "0pj"
t_Settings(4,2) = "plain baseline test"
t_Settings(7,2) = %wkfile
call pCreateScenario(%gm, %alias)



'==================================================================
' DEFINITIONS
'==================================================================

'--- last actual (new solution starts after this year)
%actual = t_Settings(1,2)

smpl %actual+1 %end


'
'
''--- European budget
'table t_MB
'scalar nMB = 0
'
'
''--- Debt transfers
'call MBTransfer(m_wm0pj, "2015", "2020", %actual, "0.3,0.3,0.3,0.3,0.3", "EUS", "EUW")
'
''--- Fiscal mechanism
'call MBDef(t_MB, nMB, "EU", "EUW EUN EUS EUE", _
'  "VAT:C_?;Carbon tax:CO2_?*ttco2_?_ins::0.5", _
'  "Per capita:N_?;Dependents:NCP_?+NOP_?;" _
'  + "Service standards:G_?:0.5*(N_?+NCP_?+2*NOP_?)*YN_?/3:0.5;" _
'  + "Employment support:NE_?(-1):0.7*NWP_?(-1):40000;" _
'  + "Deficit finance:NLG_?:0:1;" _
'  )
'call MBBuild(m_wme4, t_MB, nMB, "EU","0.5 1 1.5 2 2.5 3 3.5 4 4.5 5", _
'  "80 20", "30 50 20 20 30")
'
'
'call Fix("IAGO_EUS", "level", "0")
'
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
'call mk_soas_graphs("e0p; plain baseline: e0pj; modified plain baseline")
call pEnd

endsub
