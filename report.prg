'PROGRAM: report.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' program to create reports
'
' you can run this program to create reports after running
' any data-generating program (dat.prg or solx.prg)
'
' the workfile must be open and up to date before you run
' the program
'
' select options before running the program
'
' execute the program by typing   run report
'
' updated: FC 11/01/2010
'
' an option has been added to compare series from another workfile
'
'==================================================================
' OPTIONS
'==================================================================

'--- range of years
%repstart = "1980"
%replast = ""

'--- load series from another workfile and display comparison graphs
'    if another workfile is specified as the source for comparisons
'    the alias and title for copied data may not be blank and must
'    differ from the alias and title of main data in the target
'%wfComp = "A1SOLA"                     'source workfile or blank
'%scCompare = "c"                       'alias in target workfile
'%scCompTitle = "WD alignment"          'title in target workfile

'--- output options
%graphs = "No"
'--- comparison graphs (if not Y, graph main data only)
%graphcomp = "No"
%markets = "No"
%tables = "No"
%analysis = "No"
%csv = "Yes"

'==================================================================

mode quiet
tic
include "set"
pageselect data
output(s) sp_log
show sp_log
call pLog("REPORT PROGRAM v2910")

if %wfComp <> "" then
  call pLoadComparison(%wfComp, %scCompare, %scCompTitle)
endif

%tlopt = ""
if @upper(@left(%graphs,1)) = "Y" then %tlopt = "G" endif
if @upper(@left(%tables,1)) = "Y" then %tlopt = %tlopt + "T" endif
if @upper(@left(%analysis,1)) = "Y" then %tlopt = %tlopt + "A" endif
if @upper(@left(%csv,1)) = "Y" then %tlopt = %tlopt + "C" endif
if @upper(@left(%markets,1)) = "Y" then %tlopt = %tlopt + "M" endif
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
