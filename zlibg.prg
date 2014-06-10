'LIBRARY: zlibg.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' library routines for graphs
'
' Note: routines whose names begin with z are internal. Names
' beginning with lib_ are reserved and should not be used
' elsewhere.
'
' updated: FC 25/04/2011
'
'---------------------------------------------------------------
' BlocGraph(%Name, %Title, %List, %Series, %Description, _
'  %Units, %Scale, nCol, %First, %Last, %EndFit)
' BlocStack(%Name, %Var, %Blocs, %Title, %First, %Last)
' MultiGraph(%Name, %Title, %Legend, %Series, %Headers, _
'  %Units, %Scale, nCol, %First, %Last, %Endfit)
' Range(group gp, vmin, vmax)
' Rename(%Series, %Alias1, %Alias2, %Result)
'---------------------------------------------------------------

subroutine BlocGraph(string %Name, string %Title, _
  string %List, string %Series, string %Description, _
  string %Units, string %Scale, scalar nCol, _
  string %First, string %Last, string %EndFit)
'==================================================
'Create a composite graph with a separate display for each bloc.
'The sub-graph for each bloc has the graph name prefixed by s.
'
' Example:    graph grb_EDEP
'             subgraphs sgr_EDEP_aa, sgr_EDEP_bb ..
'
' Call: %Name    graph name
'       %Title   title for the multi-graph
'       %List   list of blocs to include
'       %Series  list of series with ? placeholders
'       %Description  series descriptions separated by commas
'       %Units   description of units of measurement
'       %Scale   common scale for the left-hand axis - may be
'                'auto' or a pair of numbers separated by commas
'                if this argument is blank each sub-graph will
'                have its own scale
'       nCol     number of columns in each row or 0 to let
'                the routine select automatically
'       %First   first observation
'       %Last    last observation
'       %Endfit  latest historical or estimated observation
'
' Note: line colors are selected in reverse order as follows
'       dark blue, red, green, grey, light blue, violet
'---------------------------------------------------------------

'--- font size for individual graph titles
%font = "24"
'if nCol > 0 then %font = "36" endif

'--- set the sample to be displayed
smpl %First %Last
!lib_nser = 0
!lib_nsub = 0

'--- loop to make a sub-graph for each bloc
%lib_gp = "gp_" + %Name
%lib_tl = ""
%lib_tlBloc = %List

call Token(%lib_tlBloc, " ", %lib_b)
while %lib_b <> ""
  call BlocName(%lib_b, %lib_t)
  
'--- bloc code, title and sub-graph name
  %lib_g = "s" + %Name + "_" + %lib_b

'--- insert the bloc code in the list of series
  %lib_s = @replace(%Series, "?", %lib_b)

'--- create the sub-graph
  !lib_nsub = !lib_nsub + 1
  graph {%lib_g}.line {%lib_s}
  {%lib_g}.addtext(t, font={%font}) %lib_t

'--- add the sub-graph to a list for inclusion in the main graph
  %lib_tl = %lib_tl + %lib_g + " "

'--- create or extend the EViews group containing the data
  if !lib_nsub = 1 then
    group {%lib_gp} {%lib_s}
  else
    {%lib_gp}.add {%lib_s}
  endif

  '--- enforce standard colouring of series
  call CountTokens(%lib_s, " ", !lib_nser)
  call zColor(%lib_g, !lib_nser)

  '--- shade the forecast period
  if @val(%Last) > @val(%Endfit) then
    %lib_s = @str(@val(%Endfit)+1) + " " + %Last
    {%lib_g}.draw(shade,b) {%lib_s}
  endif

  call Token(%lib_tlBloc, " ", %lib_b)
wend

'--- create the main graph as a composite of the sub-graphs
%lib_g = %Name
'--- set up the title for the composite graph
%lib_hdr = %Title +"\n\n" + %Description
if %Units <> "" then
  %lib_hdr = %lib_hdr + "    Units: " + %Units
endif

'--- if there is only one sub-graph, copy and delete
if !lib_nsub = 1 then
  copy {%lib_tl} {%lib_g}
  delete {%lib_tl}
  '--- add the bloc name to the title
  %lib_hdr = %lib_hdr + "\n\n" + %lib_t

'--- otherwise create a composite graph
else if !lib_nsub > 1 then
  graph {%lib_g}.merge {%lib_tl}
else
  return
endif
endif

'--- delete legends
{%lib_g}.legend -display
'--- display the title and description at the top
{%lib_g}.addtext(t, just(c), font=30) %lib_hdr

'--- fix properties of data sub-graphs

'--- check the scale argument
'--- determine the range of data values over data series in the group
if %Scale = "auto" then
  !lib_vmin = 0
  !lib_vmax = 0
  call Range({%lib_gp}, !lib_vmin, !lib_vmax)
else
  %lib_t = @trim(%Scale)
  call Token(%lib_t, ",", %lib_s)
  if %lib_t <> "" then
    !lib_vmin = @val(%lib_s)
    !lib_vmax = @val(%lib_t)
  else
    !lib_vmin = -999999
  endif
endif
delete {%lib_gp}

if !lib_vmin <> -999999 then
  '--- impose the left-hand scale range
  {%lib_g}.scale(l) range(!lib_vmin,!lib_vmax) grid

  '--- add a zero line if the scale has negative values
  if !lib_vmin < 0 then {%lib_g}.scale zeroline endif
endif

{%lib_g}.axis(left) font({%font})
{%lib_g}.axis(bottom) font({%font})

'--- display in grid with 2" and 1.5" separation
if nCol = 0 then
  !lib_n = @floor(@sqrt(!lib_nsub-1))+1
  !lib_n1 = @floor((!lib_nsub-1)/!lib_n)+1
  if !lib_n = !lib_n1 then
    !lib_n = !lib_n+1
  endif
else
  !lib_n = nCol
endif    
{%lib_g}.align(!lib_n,2,1.5)

endsub

subroutine zColor(string %Name, scalar n)
'==================================================
'Color lines on a multi-graph
'
' Call: %Name    name of graph
'       n        number of series displayed
'
' Note: colors selected are, in reverse order
'       dark blue, red, green, grey, light blue, violet
'
'---------------------------------------------------------------
for !llib_i = 1 to n
  !llib_j = n - !llib_i + 1
  if !llib_j = 1 then {%Name}.setelem(!llib_i) lcolor(blue) endif
  if !llib_j = 2 then {%Name}.setelem(!llib_i) lcolor(red) endif
  if !llib_j = 3 then {%Name}.setelem(!llib_i) lcolor(0,255,0) endif
  if !llib_j = 4 then {%Name}.setelem(!llib_i) lcolor(127,127,127) endif
  if !llib_j = 5 then {%Name}.setelem(!llib_i) lcolor(0,127,255) endif
  if !llib_j = 6 then {%Name}.setelem(!llib_i) lcolor(127,0,255) endif
next
endsub

subroutine BlocStack(string %Name, string %Var, _
  string %Blocs, string %Title, string %First, string %Last)
'==============================================================
'create a stacked area chart for bloc series
'
' Call: %Name   graph name
'       %Var    bloc variable
'       %Blocs  list of blocs for which the variable is available
'       %Title  title for the chart
'       %First  first observation
'       %Last   last observation
'
' Ret:
'
'---------------------------------------------------------------

smpl %First %Last

'--- create a pool object, then a group and finally, the chart
pool lib_p {%Blocs}
lib_p.makegroup(lib_g) {%Var}
freeze({%Name}) lib_g.area(s,o=reverse)
{%Name}.options -outlinearea

'--- change the legend names
!lib_j = 1
for !lib_i = 1 to nBloc
  %lib_s = @upper(t_Bloc(!lib_i, 1))
  if %lib_s = lib_p.@idname(!lib_j) then
    %lib_s = t_Bloc(!lib_i, 2)
    {%Name}.name(!lib_j) {%lib_s}
    !lib_j = !lib_j + 1
  endif
next
{%Name}.legend position(r) columns(1)

'--- fixup the scale
{%Name}.scale(left) range(0,1)

'--- add the title at the top
{%Name}.addtext(t) %Title

'--- drop the pool and group
delete lib_p
delete lib_g

endsub

subroutine MultiGraph(string %Name, string %Title, _
  string %Legend, string %Series, _
  string %Headers, string %Units, string %Scale, _
  scalar nCol, string %First, string %Last, _
  string %Endfit)
'==================================================
'Create a composite graph containing a number of sub-graphs.
'
' Example:    graph gr_AE
'             subgraphs sgr_AE_aaa, sgr_AE_bbb ..
'
' Each sub-graph should display data for a particular variable
' or entity. A sub-graph may show variants such as Baseline
' and scenario values. The variants should be the same for data
' sub-graphs and may be described in the Legend text displayed
' at the top of the composite graph.
'
' Call: %Name     name of the composite graph
'       %Title    title for the composite graph
'       %Legend   text describing series variants displayed
'                 in data sub-graphs
'       %Series   list of series to be displayed in each
'                 sub-graph, separated by commas
'       %Headers  list of sub-graph titles separated by commas
'       %Units    description of units of measurement
'       %Scale    common scale for the left-hand axis - may be
'                 'auto' or a pair of numbers separated by commas
'                 if this argument is blank each sub-graph will
'                 have its own scale
'       nCol      number of columns in each row or 0 to let
'                 the routine select automatically
'       %First    first observation
'       %Last     last observation
'       %Endfit   latest historical or estimated observation
'
' Uses: t_Bloc    bloc codes and names
'       nBloc     number of blocs
'
'Note: line colors are selected in reverse order as follows
'      dark blue, red, green, grey, light blue, violet
'---------------------------------------------------------------

'--- set the range to be displayed
smpl %First %Last

!lib_n = 0
'--- loop to create the sub-graphs
%lib_gp = "gp_" + %Name
%lib_tl = ""
%lib_tls = %Series
%lib_tlh = %Headers
!lib_nSer = 0
while 1=1
  call Token(%lib_tls, ",", %lib_s)
  if %lib_s = "" then exitloop endif
  call Token(%lib_tlh, ",", %lib_t)
  !lib_nSer = !lib_nSer + 1
'--- create the sub-graph
  %lib_g = %lib_s
  call Token(%lib_g, " ",%lib_s1)
  %lib_g = "s" + %Name + "_" + %lib_s1
  if @isobject(%lib_g) then delete {%lib_g} endif
  graph {%lib_g}.line {%lib_s}
  '--- select colors based on the number of series
  call CountTokens(%lib_s, " ", !lib_n)
  call zColor(%lib_g, !lib_n)

 '--- add a title and append to the list of sub-graphs
  {%lib_g}.addtext(t, font=20) %lib_t
  %lib_tl = %lib_tl + %lib_g + " "

'--- create or extend the group of series
  if !lib_nSer = 1 then
    group {%lib_gp} {%lib_s}
  else
    {%lib_gp}.add {%lib_s}
  endif
wend

'--- name the result graph
%lib_g = %Name
%lib_s = "                    "

'--- if there is only one sub-graph copy and delete
if !lib_nSer = 1 then
  copy {%lib_tl} {%lib_g}
  delete {%lib_tl}
  {%lib_g}.legend position(l)
  %lib_hdr = %Legend
  if %Units <> "" then
    %lib_hdr = %lib_hdr + %lib_s + "Units: " + %Units
  endif
  {%lib_g}.addtext(b, font=20) %lib_hdr

'--- otherwise create a composite graph
else if !lib_nSer > 1 then
  graph {%lib_g}.merge {%lib_tl}
  '--- delete legends
  {%lib_g}.legend -display
  '--- set up the title for the composite graph
  %lib_hdr = %Title + %lib_s + %Legend
  if %Units <> "" then
    %lib_hdr = %lib_hdr + %lib_s + "Units: " + %Units
  endif
  {%lib_g}.addtext(t, font=20) %lib_hdr
endif
endif

'--- set common properties for sub-graphs

'--- shade the forecast period
if @val(%Last) > @val(%Endfit) then
  %lib_s = @str(@val(%Endfit)+1) + " " + %Last
  {%lib_g}.draw(shade,b) {%lib_s}
endif

'--- check the scale argument
'--- determine the range of data values over data series in the group
if %Scale = "auto" then
  !lib_vmin = 0
  !lib_vmax = 0
  call Range({%lib_gp}, !lib_vmin, !lib_vmax)
else
  %lib_t = @trim(%Scale)
  call Token(%lib_t, ",", %lib_s)
  if %lib_t <> "" then
    !lib_vmin = @val(%lib_s)
    !lib_vmax = @val(%lib_t)
  else
    !lib_vmin = -999999
  endif
endif
delete {%lib_gp}

if !lib_vmin <> -999999 then
  '--- impose the left-hand scale range
  {%lib_g}.scale(l) range(!lib_vmin,!lib_vmax) grid

  '--- add a zero line if the scale has negative values
  if !lib_vmin < 0 then {%lib_g}.scale zeroline endif
endif

{%lib_g}.axis(left) font(14)
{%lib_g}.axis(bottom) font(14)

'--- display in grid with 1.5" separation
'--- display in grid with 1.5" separation
if nCol = 0 then
  !lib_n = @floor(@sqrt(!lib_nSer-1))+1
else
  !lib_n = nCol
endif    
{%lib_g}.align(!lib_n,1.5,1.5)
endsub

subroutine local Range(group gp, scalar vmin, scalar vmax)
'==================================================
'Return minimum and maximum values of series in a group
'
' Call: gp  name of the group
'
' Ret:  vmin  minimum value
'       vmax  maximum value
'
'---------------------------------------------------------------
series ss = gp(1)
vmin = @min(ss)
vmax = @max(ss)
for !i = 2 to gp.@count
  ss = gp(!i)
  !v = @min(ss)
  if !v < vmin then vmin = !v endif
  !v = @max(ss)
  if !v > vmax then vmax = !v endif
next
endsub

subroutine local Rename(string %Series, string %Alias1, _
  string %Alias2, string %Result)
'===============================================================
'Converts a list of series names to include baseline and scenario
'variants
'
' Call: %Series   list of series names separated by commas
'       %Alias1   first scenario alias
'       %Alias2   second scenario alias
'
' Ret:  %Result   adjusted list
'
' Note: if the aliases are different, the series names are
' returned in pairs separated by blank
'
'---------------------------------------------------------------
%t = %Series
%Result = ""
%r = ""
while 1
  call Token(%t, ",", %r)
  if %r = "" then exitloop endif
  if %Result <> "" then %Result = %Result + "," endif
  %Result = %Result + %r + %Alias1
  if %Alias1 <> %Alias2 then
    %Result = %Result + " " + %r + %Alias2
  endif
wend
endsub
