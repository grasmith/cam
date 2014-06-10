'PROGRAM: solE4.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' Federal Europe with global regionalisation
'
' The program reads SOLE3.wf1 and creates SOLE4.wf1
'
' updated: FC 25/06/2012
'
' differences from SOLE3
'  Europe excluding UK
'    extended eurozone with EUE as full member and EUN aligning
'    federal budget rising to 5% of GDP
'      revenue: carbon tax, other tax
'      expenditure: deficit finance, social objectives
'    investment stimulus in member states and neighbouring regions
'  UK no investment stimulus
'
'==================================================================
' OPTIONS
'==================================================================
include "mb.prg"
include "set"
call sole4
'------------------------------------------------------------------
subroutine sole4

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
'--- open the SOLE3 workfile
open SOLE3
pageselect graphs
delete *
pageselect tables
delete *
pageselect data
delete sp_log* m_wme3
'--- update settings
call pLog("SOLE4 PROGRAM v0625")
%wkfile = "SOLE4"
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "e4"
t_Settings(4,2) = "Towards Federal Europe"
t_Settings(7,2) = %wkfile
%first = @str(@val(t_Settings(1,2))+1)
call CopyAliasValues("_" + t_Settings(5,2), "", %first, %actual)
t_Settings(1,2) = %actual

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end

'--- Eurozone: nominal exchange rates
call DropRules("rxu_EUS rxu_EUE rxu_UK rxu_EUN")
call Target("rxu_EUS", "rxna_EUS-rxna_EUW", "0", 100,100)
call Target("rxu_EUE", "rxna_EUE-rxna_EUW", "0", 100,100)
call Target("rxu_EUN", "rxna_EUN-rxna_EUW", "0", 100,100)
call Limit(100, "ALL")
'--- North and East Europe: inflation differentials
call Target("pvi_EUE", "pvi_EUE-pvi_EUW", "0.5", 100,100)
call Target("pvi_EUN", "pvi_EUN-pvi_EUW", "0.5", 100,100)

'--- European budget
table t_MB
scalar nMB = 0
call MBDef(t_MB, nMB, "EU", "EUW EUN EUS EUE", _
  "VAT:C_?;Carbon tax:CO2_?*ttco2_?_ins::0.5", _
  "Per capita:N_?;Dependents:NCP_?+NOP_?;" _
  + "Service standards:G_?:0.5*(N_?+NCP_?+2*NOP_?)*YN_?/3:0.5;" _
  + "Employment support:NE_?(-1):0.7*NWP_?(-1):40000;" _
  + "Deficit finance:NLG_?:0:1;" _
  )
call MBBuild(m_wme4, t_MB, nMB, "EU","0.5 1 1.5 2 2.5 3 3.5 4 4.5 5", _
  "80 20", "30 50 20 20 30")

'--- investment stimulus in Europe
IP_EUW_ins = 0.01
IP_EUN_ins = IP_EUW_ins
IP_EUS_ins = IP_EUW_ins + 0.01
IP_EUE_ins = IP_EUW_ins + 0.01
IP_UK_ins = 0

'--- investment stimulus in neighbouring regions
IP_CI_ins = IP_EUW_ins
IP_WA_ins = IP_EUW_ins + 0.01
IP_AFN_ins = IP_EUW_ins

call Limit (95, "ALL")

'==================================================================
' PROCESSING
'================e==================================================

call pCheckSolveReport({%gm}, %actual, %predict, "m=30000", 8)
call MBRep(t_MB, nMB, "EU", "e4 Federal Europe", %actual, %predict, 8)
call pEnd

endsub