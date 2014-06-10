'PROGRAM: zlibp.prg     Copyright (C) 2012,2013 Alphametrics Co. Ltd.
'
' CAM Version 5.1
'
' library functions for presentations
'
' updated: FC 25/04/2013
'
'   SPGraph     make a graph tableau
'
' Graph tableaus may be put on different pages of the ' workfile.
'
' A graph tableau has a title and one or more graphs
' arranged in a grid. Graphs may cover different
' topics and may be repeated for specified geo codes
' (blocs, groups or world). The tableau may specify a
' list of scenarios for comparison in each graph.
' Series in one graph may cover different variables,
' geo codes and/or scenarios.
'
' colours for up to 14 series (allocated in sequence)
'  red blue gray green purple cyan orange black pink
'  brown olive teal mint plum
'
' the following may be used for the graph backround
%blyellow = "255,255,190"
%blmauve = "255,190,255"
%blsea = "190,255,255" 
%blpink = "255,222,222"
%blgray = "222,222,255"
%blblue = "222,255,222"
%blorange = "255,222,190"
%blgreen = "222,255,190"
%bmblue = "190,222,255"
%bwhite = "250,250,250"

'---------------------------------------------------------------
' SPGraph(%Page, %Name, %Title, %Color, %Font, %tlBloc, %tlAlias, 
'   %tlYear, nCol, %tlContent)
'   where content = title;scale;options;series list
'---------------------------------------------------------------
  
subroutine local SPGraph(string %p_Page, string %p_Name, _
  string %p_Title, string %p_Color, string %p_Font, _
  string %p_tlBloc, string %p_tlScenario, string %p_tlYear, _
  scalar p_nCol, string %p_tlContent)
'=====================================================================
'Make graph tableau
'
' Call: %p_Page       page on which the graph is stored
'       %p_Name       graph name (will be prefixed by gr_)
'       %p_Title      title for the tableau
'       %p_Color      background colour or blank for black and white
'       %p_Font       font size (e.g. 20, 24 or 28)
'       %p_tlBloc     list of blocs
'                       prefix with 'single' to show all blocs in the
'                       same subgraph
'       %p_tlScenario list of aliases to display in each subgraph
'                       alias;title
'       %p_tlYear     start year, end year and first shaded year
'       p_nCol        number of subgraphs in each row
'       %p_tlContent  content definition - list separated by colon
'                       title;scale;options;series list
'                       options = 0, bar, line, area, scat, log
'
' Note: the series list is separated by |. Each series definition
' is a variable name or formula optionally followed by a name (legend)
' Occurrences of ? in the name or formula are substituted with the
' relevant bloc code.
'
' Legends are displayed at the right-hand side of the last sub-graph.
' Series names are not shown in the legend if multiple content definitions
' are supplied or if there is only one series in the content definition.
'
' A data table is created for each graph unless the global variable
' %DataTables is set to "No".
'
'-----------------------------------------------------
statusline {%p_Name}

!nScenario = 0
call CountTokens(%p_tlScenario, ":", !nScenario)

'--- check whether blocs will be shown together (single)
'    or split to separate subgraphs and check the number of blocs
%tlBloc = %p_tlBloc
!qsingle = @left(%tlBloc,7) = "single "
if !qsingle then %tlBloc = @mid(%tlBloc, 8) endif
!qsingle = !qsingle and !nScenario < 2
!qsingle = !qsingle or %tlBloc = ""
!nBloc = 0
call CountTokens(%tlBloc, ":", !nBloc)

'--- unpack dates
%yrsh = %p_tlYear
call Token(%yrsh, " ", %yrs)
call Token(%yrsh, " ", %yre)
if @instr(%p_tlContent, "scat") > 0 then %yrsh = "" endif
if %yrsh <> "" then %yrsh = %yrsh + " " + %yre endif
smpl %yrs %yre

'--- create data table and write col headings in first row
!nTData = 0
%tName = "t" + %p_Name
call zCreateDataTable(%tName, %yrs, %yre, !nTData)

'--- loop to generate subgraphs
%tlgr = ""                     'list of subgraphs
!n = 0                         'subgraph count
'--- loop over content
%tl = %p_tlContent
!qfirst = 1
while 1 = 1
  call Token(%tl, ":", %content)
  if %content = "" then exitloop endif
  '--- blocs shown together
  if !qsingle then
    !n = !n + 1
    %grs = %p_Name + "_" + @str(!n)
    %tlgr = %tlgr + %grs + " "
    call zSubGraph(%grs, %tlBloc, %p_tlScenario, %content, _
                   %yrs, %yre, %yrsh, %p_Font, %tName, !nTData)
  else 
  '--- loop to create a subgraph for each bloc
    %tlb = %tlBloc
    while 1 = 1
      call Token(%tlb, " ", %b)
      if %b = "" then exitloop endif
      %t = @replace(%content, "?", %b)
      '--- if the number of blocs and columns differ
      '    or if this is the first content definition
      '    display the bloc name as the first line of the
      '    subgraph title
      if !qfirst or !nBloc <> p_nCol then
        call BlocName(%b, %bn)
        if @left(%t,1) <> ";" then
          %t = "\n" + %t
        endif
        %t = %bn + %t
      endif
      !n = !n + 1
      %grs = %p_Name + "_" + @trim(@str(!n))
      %tlgr = %tlgr + %grs + " "
      call zSubGraph(%grs, "", %p_tlScenario, %t, _
                     %yrs, %yre, %yrsh, %p_Font, %tName, !nTData)
    wend
    !qfirst = 0
  endif
wend  

'--- set up legend texts
if not !qsingle then %tlBloc = "" endif
!nLegend = 0
!nSeries = 0
call zBuildLegends(%tlBloc, %p_tlContent, %p_tlScenario, _
  %tlLegend, !nLegend, !nSeries)
  
'--- expand the title to include the first scenario name
%t1 = ""
if !nScenario = 1 then
  %t = %p_tlScenario
  call Token(%t, ":", %t1)
  call Token(%t1, ";", %t2)
  %t1 = @upper(@left(%t1,1)) + @mid(%t1,2)
endif
if %t1 <> "" then
  %title = %t1 + ": " + %p_Title
else
  %title = @upper(@left(%p_Title,1)) + @mid(%p_Title,2)
endif

'--- merge subgraphs into composite
call zMergeGraph(%p_Page, %p_Name, %title, %yrsh, _
  p_nCol, %p_Color, %p_Font, %tlgr, %tlLegend, !nLegend, !nSeries, _
  !nTData)

endsub

subroutine zCreateDataTable(string %p_TName, string %p_yrs, _
  string %p_yre, scalar p_nrow)
'=====================================================================
'Create data table to store graphed series
'
' Call: %p_TName   table name
'       %p_yrs     start year
'       %p_yre     end year
'
' Ret:  p_nrow     1 if table created
'
'---------------------------------------------------------------------
if @upper(@left(%DataTables, 1)) = "N" then return endif
table {%p_TName}
setcell({%p_TName}, 1, 1, "Graph")
setcell({%p_TName}, 1, 2, "Series")
!iys = @val(%p_yrs)
!iye = @val(%p_yre)
for !iy = !iys to !iye
  setcell({%p_TName}, 1, 3+!iy-!iys, @str(!iy), "c")
next  
p_nrow = 1
endsub

subroutine local zSubGraph(string %p_Name, string %p_tlBloc, _
  string %p_tlScenario, string %p_Content, _
  string %p_yrs, string %p_yre, string %p_yrsh, _
  string %p_Font, string %p_TName, scalar p_nrow)
'=====================================================================
'Presentation sub-graph
'
' Call: %p_Name        graph name (will be prefixed by gr_)
'       %p_tlBloc      list of blocs
'       %p_tlScenario  list of scenarios
'       %p_Content     specifications separated by semi-colon
'                        title;scale;options;series lists
'                        scale is a min max pair or blank
'                        options may include 0, line, bar or area,
'                                            scat, log, legend
'                        series list is a comma-separated list of expressions
'                        without aliases; each expression may be followed by
'                        a blank and a title
'
'       %p_yrs         first year to graph
'       %p_yre         last year to graph
'       %p_yrsh        range for predicted values (after historical period)
'       %p_Font        font size
'       %p_TName       data table name
'       p_nrow         number of rows already entered or 0
'                      to skip writing data series
'
' Ret:  p_nrow         updated number of rows in the data table
'
'---------------------------------------------------------------------
%tl = %p_Content
call Token(%tl, ";", %title)
call Token(%tl, ";", %scale)
call Token(%tl, ";", %tloption)

'--- put series and constants in separate lists
%tlvar = ""
%tlfixed = ""
while %tl <> ""
  '--- next expression (remove title that follows)
  call Token(%tl, "|", %v2)
  call Token(%v2, " ", %v)
  '--- expression that doesn't contain series
  if @instr(%v, "_") = 0 then
    %tlfixed = %tlfixed + %v + " "
  '--- expression involving one or more series
  else
    %tlvar = %tlvar + %v + " "
  endif
wend

'--- expand the variable expression list for aliases
!nscen = 0
if %p_tlScenario <> "" then
  call CountTokens(%p_tlScenario, ":", !nscen)
  %tl = ""
  while %tlvar <> ""
    call Token(%tlvar, " ", %v)
    %tla = %p_tlScenario
    '--- colon-separated scenario list
    for !i = 1 to !nscen
      call Token(%tla, ":", %a1)
      call Token(%a1, ";", %a)
      call AliasExpr(%v, "_" + %a, %v1)
      %tl = %tl + %v1 + " "
    next
  wend
  %tlvar = %tl
endif

'--- suppress area option if there are multiple scenarios
if !nscen > 1 then
  %tloption = @replace(%tloption, "area", "")
endif

'--- expand the variable expression list for blocs
if %p_tlBloc <> "" then
  %tl = ""
  %tlb = %p_tlBloc
  while %tlb <> ""
    call Token(%tlb, " ", %b)
    %tl = %tl + @replace(%tlvar, "?", %b)
  wend
  %tlvar = %tl
endif

'--- series descriptions
%tldesc = @replace(%tlvar, " ", ";")

'--- if multiple scenarios will be graphed,
'    avoid overlap of historical values by showing the
'    complete series for the first scenario only
'    limit series for additional scenarios to projected values
!nshort = 0
if !nscen > 1 and (%p_yrsh = "" or %p_yrs < %p_yrsh) then
  !qshort = 0
  '--- build a list of follow-on scenario suffixes
  %tla = %p_tlScenario
  %tls = "_"
  for !i = 1 to !nscen
    call Token(%tla, ":", %a1)
    call Token(%a1, ";", %a)
    if !i > 1 then
      %tls = %tls + %a + "_"
    endif
  next
  '--- make a new series list creating short series to avoid overlap
  smpl {%p_yrsh}
  %tl = ""
  %tlv = %tlvar
  while %tlv <> ""
    call Token(%tlv, " ", %v)
    '--- if the suffix matches, create a new short series
    call zCheckShort(%v, %tls, !qshort)
    if !qshort then
      !nshort = !nshort + 1
      %v1 = "libp_ss_" + @trim(@str(!nshort))
      %ss = %v1 + "=" + %v
      call CreateSeries(%ss)
      %tl = %tl + %v1 + " "
    else
      %tl = %tl + %v + " "
    endif
  wend  
  %tlvar = %tl
  smpl %p_yrs %p_yre
endif

'--- combine the lists
%tlser = %tlvar + %tlfixed

'--- create subgraph
call zCreateGraph(%p_Name, %title, %scale, %p_Font, _
  %tloption, %tlser, %p_yrs, %p_yre, %p_TName, p_nrow, _
  %tldesc, %tlvar)

'--- delete short series
if !nshort > 0 then call DeleteObjects("libp_ss_*") endif

endsub

subroutine local zCheckShort(string %p_Expr, string %p_Suffixes, _
  scalar p_q)
'=====================================================================
'Check expression for scenario suffixes
'
' Call: %p_Expr        expression to be graphed
'       %p_tlSuffixes  list of suffixes prefixed with _
'                      and ended with _
'
' Ret:  p_q  true if the expression contains series with one or more
'            of the given suffixes
'
'---------------------------------------------------------------------
'--- parse the expression for series names
%s = %p_Expr
p_q = 1
while %s <> ""
  call Parse(%s, %g)
  '-- series names have more than 1 char, don't begin
  '   with @ and include an underscore after the var name
  if @len(%g) > 1 and @left(%g,1) <> "@" _
      and @instr(@mid(%g,2), "_") > 0 then
    '--- find the scenario suffix
    !i = @instr(%g, "_", 2)
    if !i > 0 then
      %a = @mid(%g, !i) + "_"
      !j = @instr(%p_Suffixes, %a)
      if !j > 0 then return endif
    endif
  endif
wend  
p_q = 0
endsub

subroutine zCreateGraph(string %p_Name, string %p_Title, _
  string %p_Scale, string %p_Font, string %p_tlOpt, _
  string %p_tlSer, string %p_yrs, string %p_yre, _
  string %p_TName, scalar p_nrow, string %p_tlDesc, _
  string %p_tlVar)
'=====================================================================
'Create a graph
'
' Call: %p_Name     graph name
'       %p_Title    title
'       %p_Scale    min,max or blank
'       %p_Font     font size
'       %p_tlOpt    list of options
'                     0 zeroline
'                     area, line or bar display type
'       %p_tlSer    list of series
'       %p_yrs      start year
'       %p_yre      end year
'       %p_TName    data table name
'       p_nrow      number of rows already entered or 0
'                   to skip writing data series
'       %p_tlDesc   series descriptors (semi-colon separators)
'       %p_tlVar    series to write to data table
'
' Ret:  p_nrow      updated number of rows
'
'---------------------------------------------------------------------
%opt = "line"
%ctype = "l"
%width = "4"
if @instr(%p_tlOpt, "scat") > 0 then
  %opt = "scatpair"
else if @instr(%p_tlOpt, "area") > 0 then
  %opt = "area(s)"
  %ctype = "f"
else if @instr(%p_tlOpt, "bar") > 0 then
  %opt = "bar"
  %ctype = "f"
endif
endif
endif
if @instr(%p_tlopt, "narrow") > 0 then
  %width = "2.4"
else
  if @instr(%p_tlopt, "square") > 0 then
    %width = "3"
  else
    if @instr(%p_tlopt, "wide") > 0 then
      %width = "6"
    endif
  endif
endif

'--- create the graph
graph {%p_Name}.{%opt} {%p_tlSer}
{%p_Name}.legend -display
{%p_Name}.options size({%width},3)
'--- title
if %p_Title <> "" then
  {%p_Name}.addtext(t, just(c), font={%p_Font}) %p_Title
endif
'--- open-ended scale with zero
if %p_Scale = "0" then
  {%p_Name}.axis(left) linearzero
'--- other options
else
  '--- explicit scale
  if %p_Scale <> "" then
    %vmax = %p_Scale
    call Token(%vmax, ",",%vmin)
    !vmax = @val(%vmax)
    !vmin = @val(%vmin)
    {%p_Name}.axis(left) range(!vmin,!vmax) grid    
  endif
  '--- log scaling
  if @instr(%p_tlOpt, "log") > 0 then
    {%p_Name}.axis(left) log
  endif
endif
if @instr(%p_tlOpt, "%") > 0 then
  {%p_Name}.axis(left) units(p)
endif

'--- standard colouring of series
!nser = 0
call CountTokens(%p_tlSer, " ", !nser)
call zColor(%p_Name, %ctype, !nser)
'--- zeroline
if @instr(%p_tlOpt, "0") > 0 then {%p_Name}.scale zeroline endif

'--- write series to data table
if p_nrow > 0 then
  call zDataTable(%p_Name, %p_tlDesc, %p_tlVar, %p_yrs, %p_yre, _
    %p_TName, p_nrow)
endif
endsub

subroutine local zDataTable(string %p_Name, string %p_tlDesc, _
  string %p_tlSer, string %p_yrs, string %p_yre, string %p_Tname, _
  scalar p_nrow) 
'=====================================================================
'Add series for a subgraph to a data table
'
' Call: %p_Name     subgraph name
'       %p_tlDesc   series descriptions (semi-colon separators)
'       %p_tlSer    series expressions (blank separators)
'       %p_yrs      start year
'       %p_yre      end year
'       %p_TName    data table name
'       p_nrow      number of rows already entered
'
' Ret:  p_nrow      updated number of rows
'
'---------------------------------------------------------------------
!iys = @val(%p_yrs)
!iye = @val(%p_yre)
series s
%tl = %p_tlSer
%tld = %p_tlDesc
while %tl <> ""
  call Token(%tl, " ", %v)
  call Token(%tld, ";", %t)
  call EvalS(%v, %p_yrs, %p_yre, s)
  p_nrow = p_nrow + 1
  call WriteCell(%p_TName, p_nrow, 1, %p_Name, "l")
  call WriteCell(%p_TName, p_nrow, 2, %t, "l")
  for !j = !iys to !iye
    !v = @elem(s, @str(!j))
    call XFormat(!v, 4, %s)
    call WriteCell(%p_TName, p_nrow, 3+!j-!iys, %s, "r")
  next
wend
endsub

subroutine zMergeGraph(string %p_Page, string %p_Name, _
  string %p_Title, string %p_Yrsh, scalar p_nCol, _
  string %p_Color, string %p_Font, string %p_tlSub, _
  string %p_tlLegend, scalar p_nLegend, scalar p_nSeries, _
  scalar p_nrow)
'=====================================================================
'Merge graphs into a composite display on the presentation page
'of the workfile
'
' Call: %p_Page     page on which graph is stored
'       %p_Name     graph name
'       %p_Title    title
'       %p_Yrsh     first and last year to shade or blank
'       p_nCol      number of columns in the composite display
'       %p_Color    background colour or blank for black and white
'       %p_Font     font size
'       %p_tlSub    list of graphs to merge
'       %p_tlLegend list of legends to display beside the last graph
'                     or blank if legends are not displayed
'       p_nLegend   no of legend texts
'       p_nSeries   no of series
'       p_nrow      no of rows in data table or 0 if none
'
'---------------------------------------------------------------------
!qNTData = p_nrow > 0
if @isobject(%p_Name) then delete {%p_Name} endif
!lib_n = 0
call CountTokens(%p_tlSub, " ", !lib_n)
if !lib_n = 0 then return endif
'--- if required, display legends on the last graph
if %p_tlLegend <> "" then
  %lib_tl = %p_tlSub
  call RToken(%lib_tl, " ", %lib_gr)
  call zLegend(%lib_gr, p_nSeries, p_nLegend, %p_tlLegend, _
    %p_Font)
endif

'--- copy or merge the graphs into the tableau
if !lib_n = 1 then
  copy {%p_tlSub} {%p_Name}
else
  graph {%p_Name}.merge {%p_tlSub}
endif  

'--- decorate the tableau
if %p_Title <> "" then
  {%p_Name}.addtext(t, just(c), font={%p_Font}) %p_Title
endif
'--- fonts and shade on all graphs
{%p_Name}.axis(left) font({%p_Font})
{%p_Name}.axis(bottom) font({%p_Font})
'--- shade
if %p_Yrsh <> "" then
  {%p_Name}.draw(shade,b,color(192,192,192)) {%p_Yrsh}
endif
'--- layout and tableau background colour
'    EViews BUG: .align doesn't work unless we transfer p_nCol to !lib_n
!lib_n = p_nCol
{%p_Name}.align(!lib_n,2,2)
if %p_Color = "" then
  {%p_Name}.options -color gridl
else
  {%p_Name}.options backcolor({%p_Color}) gridl
endif
{%p_Name}.options background

'--- clean up and move the tableau to the target page
delete {%p_tlSub}
pageselect {%p_Page}
if @isobject(%p_Name) then delete {%p_Name} endif
copy data\{%p_Name}
%lib_s = "t" + %p_Name
if @isobject(%lib_s) then delete {%lib_s} endif
if !qNTData then
  copy data\{%lib_s}
endif
pageselect data
delete {%p_Name}
if !qNTData then
  delete {%lib_s}
endif
endsub

subroutine zColor(string %Name, string %Type, scalar nser)
'======================================================
'Color a multi-graph
'
' Call: %Name    name of graph
'       %Type    line or fill
'       nser     number of series displayed
'
'---------------------------------------------------------------

if nser > 14 then return endif

if nser = 1 then
  {%Name}.setelem(1) linecolor(blue)
else
  '--- color list: 0,127,255 is cyan  205,16,118 is pink
  '                139,35,35 is brown, 128,128,0 is olive 
  '                0,128,128 is teal, 46,139,87 is mint
  '                139,102,139 is plum
  ' red blue gray green purple cyan orange black pink brown
  ' olive teal mint plum"  
  if %Type = "l" then
  %llib_tl = "blue red black green purple 0,127,255 orange gray " _
    + "205,16,118 139,35,35 128,128,0 0,128,128 46,139,87 139,102,139"
  else  
    %llib_tl = "78,255,255 255,128,32 255,255,78 78,255,78 255,128,255 " _
    + "127,127,255 223,128,0 78,78,78 " _
    + "237,16,118 139,35,35 128,128,0 0,128,128 46,139,87 139,102,139"
  endif  
  for !llib_i = 1 to nser
    call Token(%llib_tl, " ", %llib_g)
    {%Name}.setelem(!llib_i) {%Type}color({%llib_g})
  next
endif
endsub

subroutine zLegend(string %Name, scalar nser, _
  scalar nLegend, string %tlLegend, string %p_Font)
'======================================================
'Display legends at right-hand side of graph
'
' Call: %Name       name of graph
'       nser        number of series
'       nLegend     number of legend texts
'       %tlLegend   comma-separated list of legend texts
'       %p_Font     font size
'
'---------------------------------------------------------------
{%Name}.legend display position(right) font({%p_Font}) -inbox
%llib_tl = %tlLegend
for !llib_i = 1 to nser
  if !llib_i <= nLegend then
    call Token(%llib_tl, ",", %llib_t)
  else
    %llib_t = ""
  endif
  {%Name}.setelem(!llib_i) legend({%llib_t})
next
endsub

subroutine local zBuildLegends(string %tlBloc, string %tlContent, _
  string %tlScenario, string %tlLegend, scalar nLegend, _
  scalar nSeries)
'======================================================
'Generate a list of legend texts
'
' Call: %tlBloc      list of blocs
'       %tlContent   content definitions
'       %tlScenario  scenario list
'
' Ret:  %tlLegend    comma-separated legend texts
'       nLegend      number of legend texts
'       nSeries      number of series
'
'---------------------------------------------------------------

nSeries = 0
'--- generate a list of series legend texts
%tlSeries = ""
%t = %tlContent
call Token(%t, ":", %t1)
'--- no series legends if the tableau has multiple content definitions
if %t = "" then
  call RToken(%t1, ";", %t2)
  !n = 0
  while 1=1
    call Token(%t2, "|", %ts)
    if %ts = "" then exitloop endif
    call Token(%ts, " ", %t3)
    '--- ignore constants
    if @instr(%t3, "_") = 0 then
      nSeries = nSeries + 1
    else
      if %ts = "" then %ts = %t3 endif
      %tlSeries = %tlSeries + %ts + ","
      !n = !n + 1
    endif
  wend
  '--- no series legend if there is only one series
  if !n = 1 then %tlSeries = "" endif
endif

'--- generate a list of scenario legend texts
%tlAlias = ""
%t = %tlScenario
call Token(%t, ":", %ta)
'--- no scenario legend if the tableau has only one scenario
if %t <> "" then
  while 1=1
    call Token(%ta, ";", %t1)
    if %ta = "" then %ta = %t1 endif
    %tlAlias = %tlAlias + %ta + ","
    call Token(%t, ":", %ta)
    if %ta = "" then exitloop endif
  wend
endif
'--- no bloc legend if subgraphs show not more than one bloc
%tlb = %tlBloc
!n = 0
call CountTokens(%tlBloc, " ", !n)
if !n < 2 then %tlb = "" endif

'--- loop over blocs, series and aliases to build the legend list
nLegend = 0
%tlLegend = ""
call Token(%tlb, " ", %b)
while 1 = 1
  call BlocName(%b, %bn)
  %tls = %tlSeries
  call Token(%tls, ",", %s)
  while 1 = 1
    %tt = @trim(%bn + " " + %s)
    %tla = %tlAlias
    call Token(%tla, ",", %a)
    while 1 = 1
      %t = %tt
      if %a <> "" then
        if %t = "" then
          %t = %a
        else
          %t = %t + " (" + %a + ")"
        endif
      endif
      if %t <> "" then
        %tlLegend = %tlLegend + %t + ","
        nLegend = nLegend + 1
      endif
      call Token(%tla, ",", %a)
      if %a = "" then exitloop endif
    wend
    call Token(%tls, ",", %s)
    if %s = "" then exitloop endif
  wend
  call Token(%tlb, " ", %b)
  if %b = "" then exitloop endif
wend
nSeries = nSeries + nLegend
endsub
