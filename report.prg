'PROGRAM: report.prg      Copyright (C) 2010,2015 Alphametrics Co. Ltd.
'
' CAM Version 6.0 utility
'
' program to create reports
'
' you can run this program to create reports with any CAM workfile
'
' the workfile must be open and up to date before you run
' the program by typing   run(c) report
'
' select options by adjusting code below before running the program
'
' updated: FC 23/03/2015
'
'==================================================================
' OPTIONS
'==================================================================
mode quiet
tic
include "set"
include "ztab"
'------------------------------------------------------------------

'--- coverage of graphs
%repgeo = "FGO"                  'blocs and bloc groups
%grtab = "No"

'--- range of years
%repstart = "1980"
%replast = ""

'--- load series from another workfile and display comparison graphs
'    if another workfile is specified as the source for comparisons
'    the alias and title for copied data may not be blank and must
'    differ from the alias and title of main data in the target
'%wfComp = "SOLA"                     'source workfile or blank
'%scCompare = "c"                     'alias in target workfile
'%scCompTitle = "Alignment"           'title in target workfile

'--- output options
%graphs = "Yes"
'--- comparison graphs (if not Y, graph main data only)
%graphcomp = "Yes"
%markets = "Yes"
%tables = "No"
%analysis = "No"
%csv = "No"

'==================================================================
pageselect data
output(s) sp_log
show sp_log
call pLog("REPORT PROGRAM v2303")

if %wfComp <> "" then
  call pLoadComparison(%wfComp, %scCompare, %scCompTitle)
endif

%tlopt = ""
if @upper(@left(%graphs,1)) = "Y" then %tlopt = "G" endif
if @upper(@left(%subgraphs,1)) = "Y" then %tlopt = %tlopt + "S" endif
if @upper(@left(%tables,1)) = "Y" then %tlopt = %tlopt + "T" endif
if @upper(@left(%analysis,1)) = "Y" then
  delete t_TDef
  table t_TDef
  nTDef = 0
  call pTabDef(t_TDef, nTDef)
  %tlopt = %tlopt + "A"
endif
if @upper(@left(%csv,1)) = "Y" then %tlopt = %tlopt + "C" endif
if @upper(@left(%markets,1)) = "Y" then %tlopt = %tlopt + "M" endif
if @upper(@left(%grtab,1)) = "Y" then %tlopt = %tlopt + "D" endif
!qgraphcomp = @upper(@left(%graphcomp,1)) = "Y"

call pReport(%tlopt,!qgraphcomp)
 
call pEnd

subroutine pLoadComparison(string %p_wf, string %p_alias, _
  string %p_title)
'==================================================
'load comparison series from another workfile
'
' Call: %p_wf     name of the comparison workfile
'       %p_alias  alias name for series in target workfile
'       %p_title  title for comparison series in target workfile
'
' Ret:
'
'---------------------------------------------------------------

%wkfile = t_Settings(7,2)

call pLog("loading series from " + %p_wf)

'--- open the source workfile and determine the source alias (if any)
open {%p_wf}
pageselect data
%s = t_Settings(3,2)
'--- select and copy series individually if the source alias is blank
if %s = "" then
  smpl %start %start
  group gp_all *
  freeze(t_all) gp_all
  copy t_all {%wkfile}::data\t_all
  delete gp_all t_all
  smpl %start %end
  wfselect {%wkfile}
  !i = 1
  while 1
    !i = !i + 1
    %v = t_all(1, !i)
    if %v = "" then exitloop endif
    if not (@right(%v,2) = "_a" or @right(%v,4) = "_ins") then
      copy {%p_wf}::data\{%v} {%v}_{%p_alias}
    endif
  wend
'--- bulk copy identifying by the source suffix
else
  wfselect {%wkfile}
  smpl %start %end
  copy {%p_wf}::data\*_{%s} *_{%p_alias}
endif
close {%p_wf}

'--- reset comparison information
t_Settings(5,2) = %p_alias
t_Settings(6,2) = %p_title
endsub
