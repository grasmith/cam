'PROGRAM: zrep.prg     Copyright (C) 2012,2014 Alphametrics Co. Ltd.
'
' CAM Version 5.2
'
' subroutine to create standard outputs
'
' updated: FC 18/09/2014
'
'================================================

subroutine pReport(string %p_tlOpt, scalar p_qComp)
'==================================================
'create standard graphs
'
' Call: %p_tlOpt  list of selected options
'                A analysis tables
'                G world and geo graphs
'                S world and geo subgraphs
'                M market graphs
'                D data tables for graphs
'                T report tables
'                C csv files
'       p_qComp   comparison option - 1 yes, 0 no
'
' Ret:
'
' Notes:
'
' (i) additional options  %repstart  first year for reports
'                         %replast   last year for reports
'                         %repgeo    B,F,G,O,W geo selection
'                         %grtab     data tables for graphs
'
' (ii) series with the alias defined in t_Settings
'
'---------------------------------------------------------------

%tl = ""
if @instr(%p_tlOpt, "C")>0 then %tl = %tl + " csv files," endif
if @instr(%p_tlOpt, "T")>0 then %tl = %tl + " report tables," endif
if @instr(%p_tlOpt, "A")>0 then %tl = %tl + " analysis tables," endif
if @instr(%p_tlOpt, "G")>0 then %tl = %tl + " std graphs," endif
if @instr(%p_tlOpt, "M")>0 then %tl = %tl + " mkt graphs," endif
if @len(%tl) > 0 then
  %tl = @left(%tl, @len(%tl)-1)
endif

call pLog("generating" + %tl)

!qgrtab = @instr(%p_tlOpt, "D") > 0

'--- list of areas and names
call ListCol(t_Bloc, 1, nBloc, 1, " ", %areas)
call ListCol(t_Bloc, 1, nBloc, 2, ",", %names)

'--- fixup years to report (first, actual, last)
%first = t_Settings(9,2)
if %repstart <> "" then
  if %first < %repstart then %first = %repstart endif
endif
%actual = t_Settings(1,2)
%last = t_Settings(2,2)
if %replast <> "" then
  if %last > %replast then %last = %replast endif
endif

'--- fixup aliases and titles
%scAlias = t_Settings(3,2)
%scTitle = t_Settings(4,2)
if p_qComp then
  %scCompare = t_Settings(5,2)
  %scCompTitle = t_Settings(6,2)
else
  %scCompare = %scAlias               'indicates no comparison to be shown
  %scCompTitle = ""
endif
if %scAlias <> "" then
  %scAlias = "_" + %scAlias
endif
if %scCompare <> "" then
  %scCompare = "_" + %scCompare
endif

'--- CSV dump files
if @instr(%p_tlOpt, "C")> 0 then
  %s = t_Settings(3,2)
  statusline WCSV
  call WCSV(%s, t_WVar, nWVar, %first, %last)
  statusline BCSV
  call BCSV(%s, t_BVar, nBVar, t_Bloc, nArea, _
    %first, %last)
endif

'--- fixup list of years for Table output
if @instr(%p_tlOpt, "T") + @instr(%p_tlOpt, "A") > 0 then
  if %scAlias <> "" then
    %tlyr = %yrsol
  else
    if %actual = %align then
      %tlyr = %yrext
    else
      %tlyr = %yrdat
    endif  
  endif
  '--- remove years for which data are not available
  call zRestrict(%tlyr, %first, %last)
endif

'--- cleanup
delete gr* sgrb_* ta_* td_* tdb_*

'--- balance checks
group gp_Balance_check BA$_W{%scAlias} BE$_W{%scAlias} _
  BM$_W{%scAlias} BS$_W{%scAlias} BIT$_W{%scAlias} _
  CA$_W{%scAlias} EB_W{%scAlias} BA0_W{%scAlias} _
  BE0_W{%scAlias} BM0_W{%scAlias} BS0_W{%scAlias}
freeze(tdb_balance_check) gp_balance_check

'--- create bloc analysis tables
if @instr(%p_tlOpt, "A") >0 then
  statusline Analysis tables
  '--- cleanup
  delete tables\ta_*
  '--- create tables and transfer them to the tables page
  call ATable("ta", %scTitle, %scAlias, %areas, _
    %names, %tlyr)
  delete ta*
endif

'--- create world and bloc graphs and tables
if @instr(%p_tlOpt, "G")>0 or _
   @instr(%p_tlOpt, "T")>0 then
  '--- clear graph data tables 
  delete tables\gr*
  
  call pLog("world results")
  for !i = 1 to nWRep
    %gtc = t_WRep(!i, 5)
    '--- graph
    if @instr(%p_tlOpt, "G")> 0 and @instr(%gtc, "G") > 0 then 
      call Rename(t_WRep(!i, 3), %scAlias, %scCompare, %tlv)
      %g = "gr_" + t_WRep(!i, 1)
      statusline Graph {%g}
      if %scAlias = %scCompare or %scCompTitle = "" then
        %tt = %scTitle
      else
        %tt = %scCompTitle + " (blue), " + %scTitle + " (red)"
      endif
      call MultiGraph(%g, t_WRep(!i, 2), %tt, %tlv, _
        t_WRep(!i, 4), t_WRep(!i, 6), t_WRep(!i, 7), _
        0, %first, %last, t_Settings(13,2), !qgrtab)
    endif
    '--- tables
    if @instr(%p_tlOpt, "T")> 0 and @instr(%gtc, "T") > 0 then 
      %tab = "td_" + t_WRep(!i, 1)
      statusline Table {%tab}
      '--- period average or annual values
      if t_WRep(!i, 1) = "GR" then
        %a = "P"
      else
        %a = "A"
      endif
      %tt = %scTitle + ": " + @lower(t_WRep(!i, 2))
      call Rename(t_WRep(!i, 3), %scAlias, "", %tlv)
      table {%tab}
      call WTable({%tab}, %a, %tt, %tlv, _
        t_WRep(!i, 4), t_WRep(!i, 6), %tlyr)
    endif
  next
  
  call pLog("bloc results")
  for !i = 1 to nBRep
    %gtc = t_BRep(!i, 3)
    %tlv = t_BRep(!i, 1)        'list of variables
    '--- graph
    if @instr(%p_tlOpt, "G")> 0 and @instr(%gtc, "G") > 0 then 
      '--- identify the data to be graphed
      if %scAlias = %scCompare or %scCompTitle = "" then
        %tt = %scTitle
      else if @instr(%tlv, " ") = 0 then
        %tt = %scCompTitle + " (blue), " + %scTitle + " (red)"
      else
        %tt = %scCompTitle + " (green,blue), " + %scTitle + " (black,red)"
      endif
      endif
      %t = @replace(t_BRep(!i, 2), "]", "")
      %t = @replace(%t, "[", "")
      %s = @replace(%tlv, " ", ",")
      call Rename(%s, %scAlias, %scCompare, %tlv1)
      %tlv1 = @replace(%tlv1, ",", " ")
      for !igeo = 1 to @len(%repgeo)
        %geo = @mid(%repgeo, !igeo, 1)
        '--- graph name
        %g = "gr" + @lower(%geo) + "_" + _
          @replace(@replace(%tlv, "_?", "")," ","_")
        statusline Graph {%g}
        call ListGeo(%geo, 1, %tlgeo)
        call BlocGraph(%g, %t, %tlgeo, %tlv1, %tt, _
          t_BRep(!i, 4), t_BRep(!i, 5), _
          0, %first, %last, t_Settings(13,2), !qgrtab)
      next
    endif
    '--- tables
    if @instr(%p_tlOpt, "T")> 0 and @instr(%gtc, "T") > 0 then
      call zSplitDesc(%tlv, t_BRep(!i, 2), %tldesc)
      %s = @replace(%tlv, " ", ",")
      %t = "tdb_" + @replace(%tlv,"_?","")
      statusline Table {%t}
      call Rename(%s, %scAlias, "", %tlvv)
      %tlvv = @replace(%tlvv, ",", " ")
      call BTable("tdb", %scTitle, "Bloc", %areas, %names, _
        %tlvv, %tldesc, t_BRep(!i, 4), %tlyr)
    endif
  next
endif

'--- market share graphs
if @instr(%p_tlOpt, "M") then
  call pLog("market shares")
  delete graphs\grm_* graphs\grs_* graphs\grx_*
  '--- graphs of market shares by supplier
  for !j = 1 to nBloc
    %p = t_Bloc(!j, 1)
    %tlb = ""
    for !i = 1 to nBloc
      %b = t_Bloc(!i, 1)
      %s = "sxm_" + %b + "_"+ %p
      if @isobject(%s) then
        %tlb = %tlb + " " + %b
      endif  
    next
    %g = "grm_" + %p
    statusline Graph {%g}
    %v = "sxm_?_" + %p + %scAlias
    %tt = "Shares of " + t_Bloc(!j, 2) + _
      "'s market for imported manufactures"
    call BlocStack(%g , %v, %tlb, %tt, %first, %last, !qgrtab)
  next
  
  '--- graphs of change in competitiveness
  '    and relative importance of export destinations
  for !j = 1 to nBloc
    %b = t_Bloc(!j, 1)
    %tlp = ""
  
    '--- calculate required data
    %s3 = "xm$_" + %b + %scAlias     'total exports of mfs
    %ss = "ssm_" + %b + %scAlias     'predicted export growth
    series {%ss} = 0
    smpl %first %last
    for !i = 1 to nBloc              'loop over partner blocs
      %p = t_Bloc(!i, 1)
      %s = "smx_" + %b + "_" + %p + %scAlias  'exports to partner
      %s1 = "sxm_" + %b + "_" + %p + %scAlias 'mkt share
      %s2 = "mm$_" + %p + %scAlias            'imports by partner
      if @isobject(%s1) then
        %tlp = %tlp + " " + %p
        series {%s} = {%s1}*{%s2}/{%s3}    'share of dest in total
        {%ss} = {%ss} + {%s1}(-1)*{%s2}(-1)*@pc({%s2})
                                           'predicted increase
      endif  
    next
    
    '--- real exchange rate and change in competitiveness 
    {%ss} = {%ss}/{%s3}
    %s1 = "rx_" + %b + %scAlias
    statusline Graph grs_{%b}
    graph grs_{%b}.area(d,l) @pc({%s3})-{%ss} {%s1}
    grs_{%b}.name(1) % change in competitiveness (lh scale)
    grs_{%b}.name(2) real exchange rate (rh scale)
    %tt = t_Bloc(!j, 2) + "'s exports of manufactures"
    grs_{%b}.addtext(t) {%tt}
  
    '--- share of destinations in total exports of mfs
    %g = "grx_" + %b
    statusline Graph {%g}
    %v = "smx_" + %b + "_?" + %scAlias
    %tt = "Share of " + t_Bloc(!j, 2) + _
      "'s exports of manufactures by destination"
    call BlocStack(%g , %v, %tlp, %tt, %first, %last, !qgrtab)
  next
endif

'--- move outputs to graphs or tables page
if @instr(%p_tlOpt, "G") > 0 or @instr(%p_tlopt, "M") > 0 then
  copy gr* graphs\gr*
  if @instr(%p_tlOpt, "S") > 0 then
    copy sgr* graphs\sgr*
  endif
  delete gr* sgr*
endif
if @instr(%p_tlOpt, "T") > 0 then
  copy td_* tables\td_*
  copy tdb_* tables\tdb_*
  delete td_* tdb_*
endif  

endsub

subroutine local WCSV(string %p_Alias, table p_tVar, _
  scalar p_nVar, string %p_First, string %p_Last)
'==============================================================
'Dump world series to CSV
'
' Call: %p_Alias    scenario alias or blank
'       p_tVar      table of variables to dump
'       p_nVar      no of variables
'       %p_First    start year
'       %p_Last     end year
'
' Ret:  
'
'---------------------------------------------------------------
table trep 

!yr0 = @val(%p_First)
!yr1 = @val(%p_Last)
!ncol = !yr1 - !yr0 + 4

setcell(trep,1,1, "Variable","l") 
setcell(trep,1,2, "Units","c")
setcell(trep,1,3, "Code","l")
trep.setwidth(1) 40
trep.setwidth(2) 10
trep.setwidth(3) 10

'--- write year headings
for !i = 4 to !ncol
  setcell(trep,1,!i, _
    @str(!yr0 + !i - 4), "r")
next
trep.setwidth(4:!ncol) 8

'--- write data rows
!v = 0
!irow = 1
series ser
smpl %p_First %p_Last
for !i = 1 to p_nVar
  %v = p_tVar(!i, 1)
  %td = p_tVar(!i, 2)
  %tu = p_tVar(!i, 3)
  %fmt = "f." + p_tVar(!i, 4)
  !irow = !irow + 1
  setcell(trep,!irow,1, %td, "l")
  setcell(trep,!irow,2, %tu, "l")
  setcell(trep,!irow,3, %v, "c")
  if %p_Alias <> "" then
    %v  = %v + "_" + %p_Alias
  endif
  call EvalS(%v, %p_First, %p_Last, ser)
  stomna(ser,mat)
  mat.setformat {%fmt}
  freeze(t) mat
  for !j = 4 to !ncol
    setcell(trep,!irow,!j, t(!j,2), "r")
  next
  delete t
next
trep.save(t = csv) {%p_Alias}WVar

endsub

subroutine local BCSV(string %p_Alias, table p_tVar, _
  scalar p_nVar, table p_tMem, scalar p_nMem,  _
  string %p_First, string %p_Last)
'==============================================================
'Dump bloc series to CSV
'
' Call: %p_Alias    scenario alias or blank
'       p_tVar      table of variables to dump
'       p_nVar      no of variables
'       p_tMem      grouping table with codes in col 1
'                     and names in col 2
'       p_nMem      number of blocs or 0 for dump of world vars
'       %p_First    start year
'       %p_Last     end year
'
' Ret:  
'
'---------------------------------------------------------------

table trep

!yr0 = @val(%p_First)
!yr1 = @val(%p_Last)
!ncol = !yr1 - !yr0 + 5

setcell(trep,1,1, "Variable","l")
setcell(trep,1,2, "Units","l")
setcell(trep,1,3, "Bloc","l")
setcell(trep,1,4, "Code","c")
trep.setwidth(1) 40
trep.setwidth(2) 10
trep.setwidth(3) 20
trep.setwidth(4) 10

'--- write year headings
for !i = 5 to !ncol
  setcell(trep,1,!i, _
    @str(!yr0 + !i - 5), "r")
next
trep.setwidth(5:!ncol) 8

'--- create pool
%tl = ""
for !i = 1 to p_nMem
  %tl = %tl + p_tMem(!i, 1) + " "
next
pool p {%tl}

!v = 0
!irow = 1
matrix mat
'--- write data for each variable
for !i = 1 to p_nVar
  %v = p_tVar(!i, 1)
  %v1 = %v + "_?"
  %td = p_tVar(!i, 2)
  %tu = p_tVar(!i, 3)
  %fmt = "f." + p_tVar(!i, 4)
  if %p_Alias <> "" then
    %v1  = %v1 + "_" + %p_Alias
  endif
  call EvalP(p, %v1, %p_First, %p_Last, mat)
  mat.setformat {%fmt}
  freeze(t) mat
  '--- write one row for each pool element
  for !k = 1 to p_nMem
    !irow = !irow + 1
    %s = p_tMem(!k, 1)
    %t = p_tMem(!k, 2)
    setcell(trep,!irow,1, %td, "l")
    setcell(trep,!irow,2, %tu, "l")
    setcell(trep,!irow,3, %t, "l")
    %v1 = %v + "_" + %s
    setcell(trep,!irow,4, %v1, "c")
    for !j = 5 to !ncol
      setcell(trep,!irow,!j, t(!j-1, !k+1) , "r")
    next
  next
  delete t
next

trep.save(t = csv) {%p_Alias}BVar

endsub

subroutine local WTable(table p_t, string %p_Mode, _
  string %p_Title, string %p_tlVar, string %p_tlDesc, _
  string %p_Units, string %p_tlyr)
'==============================================================
'Create tables for a list of world series
'
' Call: p_t         table
'       %p_Mode     display mode - Annual or Period averages
'       %p_Title    table title
'       %p_tlVar    list of variables
'       %p_tlDesc   list of descriptors
'       %p_Units    units description
'       %p_tlyr     list of years for which data will be reported
'
' Ret:
'
'---------------------------------------------------------------

!nrow = 0
!nyr = 0
call CountTokens(%p_tlyr, " ", !nyr)
vector(!nyr) vyr
call zTHeader(p_t, %p_Title, %p_Mode, "Variable", %p_Units, _
  %p_tlyr, vyr, !nyr, !nrow)

'--- check no of columns

!ncol = !nyr + 1
if %p_Mode = "A" then
  !ncol = !ncol + 1
endif

!v = 0
%tlv = %p_tlVar
%tld = %p_tlDesc
while %tlv <> ""
  call Token(%tlv, ",", %v)
  !nrow = !nrow + 1
  call Token(%tld, ",", %td)
  call zUCase(%td)
  setcell(p_t,!nrow,1, %td, "l")
  setcell(p_t,!nrow,2, %v, "l")
  for !i = 3 to !ncol
    if %p_Mode = "A" then
      call Eval(%v, @str(vyr(!i-2)), !v)
    else
      call zAvg(%v, @str(vyr(!i-2)+1), _
         @str(vyr(!i-1)), !v) 
    endif
    call XFormat(!v, 3, %s)
    setcell(p_t,!nrow,!i, %s, "r")
  next
wend
p_t.setindent(3,1,!nrow,2) 10

endsub

subroutine BTable(string %p_Tab, string %p_Title, _
  string %p_Mode, string %p_tlArea, string %p_tlName, _
  string %p_tlVar, string %p_tlDesc, string %p_Units, _
  string %p_tlyr)
'==============================================================
'Create tables for a list of grouped series
'
' Call: %p_Tab      table prefix
'       %p_Title    table title
'       %p_Mode     display mode
'       %p_tlArea   list of areas
'       %p_tlName   list of names
'       %p_tlVar    list of variables
'       %p_tlDesc   list of descriptors
'       %p_Units    units description
'       %p_tlyr     list of years
'
' Ret:
'
'---------------------------------------------------------------
%lib_tlv = %p_tlVar
%lib_tld = %p_tlDesc
while %lib_tlv <> ""
  call Token(%lib_tlv, " ", %lib_v)
  call Token(%lib_tld, ",", %lib_td)
  call zLCase(%lib_td)
  %lib_t = "tdb_" + @replace(%lib_v, "_?", "")
  table {%lib_t}
  call zBTable({%lib_t}, %p_Title + ": " + %lib_td , _
    %p_Mode, %p_tlArea, %p_tlName, %lib_v, %p_Units, %p_tlyr)
wend
endsub

subroutine local zBTable(table p_Tab, string %p_Title, _
  string %p_Mode, string %p_tlArea, string %p_tlName, _
  string %p_Var, string %p_Units, string %p_tlyr)
'==============================================================
'Create table for a group of series
'
' Call: p_Tab       table
'       %p_Title    table title
'       %p_Mode     display mode
'       %p_tlArea   list of areas
'       %p_tlName   list of names
'       %p_Var      variable with ?
'       %p_Units    units description
'       %p_tlyr     list of years
'
' Ret:
'
'---------------------------------------------------------------
!nrow = 0
!nyr = 0
call CountTokens(%p_tlyr, " ", !nyr)
vector(!nyr) vyr

'--- period average or annual values
if @instr("DP", @upper(@left(%p_Var, 1))) >0 then
  %a = "P"
else
  %a = "A"
endif

'--- write header rows
call zTHeader(p_Tab, %p_Title, %a, %p_Mode, %p_Units, _
  %p_tlyr, vyr, !nyr, !nrow)

'--- check no of columns
!ncol = !nyr + 1
if %a = "A" then
  !ncol = !ncol + 1
endif

!v = 0
%tlb = %p_tlArea
%tlnb = %p_tlName
while %tlb <> ""
  call Token(%tlb, " ", %b)
  !nrow = !nrow + 1
  call Token(%tlnb, ",", %nb)
  call zUCase(%nb)
  setcell(p_Tab,!nrow,1, %nb, "l")
  setcell(p_Tab,!nrow,2, %b, "l") 
  %v = @replace(%p_Var, "?", %b)
  for !i = 3 to !ncol
    if %a = "A" then
      call Eval(%v, @str(vyr(!i-2)), !v)
    else
      call zAvg(%v, @str(vyr(!i-2)+1), _
         @str(vyr(!i-1)), !v) 
    endif
    call XFormat(!v, 3, %s)
    setcell(p_Tab,!nrow,!i, %s, "r")
  next
wend
p_Tab.setindent(3,1,!nrow,2) 10

endsub

subroutine local zTHeader(table p_Tab, string %p_Title, _
  string %p_Mode, string %p_tRDesc, string %p_Units, _
  string %p_tlyr, vector p_vyr, scalar p_nyr, scalar p_nrow)
'==============================================================
'Create header for a listing of bloc series
'
' Call: p_Tab       table
'       %p_Title    table title
'       %p_Mode     display mode - Annual or Period averages
'       %p_tRDesc   row description
'       %p_Units    units description
'       %p_tlyr     list of years
'       p_vyr       vector of years
'       p_nyr       no of years in the list
'
' Ret:  p_nrow      no of rows
'
'---------------------------------------------------------------

'--- write title lines
setcell(p_Tab,1,1, %p_Title, "c")
setcell(p_Tab,2,1, "Units: " + %p_Units , "r")
setcell(p_Tab,4,1, %p_tRDesc,"l") 
setcell(p_Tab,4,2, "Code","l")
p_Tab.setwidth(1) 24
p_Tab.setwidth(2) 8

'--- check no of columns
!ncol = p_nyr + 1
if %p_Mode = "A" then
  !ncol = !ncol + 1
endif

'--- set up vector of years
%tl = %p_tlyr
for !i = 1 to p_nyr
  call Token(%tl, " ", %yr)
  p_vyr(!i) = @val(%yr)
next

'--- write year or period headings
!n = 2
call zColHeading(p_Tab, 4, %p_Mode, p_vyr, p_nyr, !n)

'--- merge top rows across columns
!n = p_nRow + 1
p_Tab.setmerge(!n,1,!n,!ncol)
!n = !n + 1
p_Tab.setmerge(!n,1,!n,!ncol)

p_nrow = 5

endsub

subroutine local zLCase(string %p_t)
'==============================================================
'lower case the first character in a string
'
' Call: %p_t   string
'
' Ret:  %p_t   adjusted string
'
' Note: unless the string begins with 2 upper case characters
'
'---------------------------------------------------------------
%g = @left(%p_t, 2)
if %g < "AA" or %g > "AZ" then
  %p_t = @lower(@left(%p_t, 1)) + @mid(%p_t, 2)
endif  
endsub

subroutine local zColHeading(table p_Tab, scalar p_iRow, _
  string %p_Mode, vector p_vYear, scalar p_nYear, _
  scalar p_nCol)
'==============================================================
'add column headings to a report
'
' Call: p_Tab     table
'       p_iRow    row to display col headings
'       p_Mode    A for annual, P for periods
'       p_vYear   list of years
'       p_nYear   no of years
'       p_nCol    no of columns
'
' Ret:  p_Tab     updated table
'       p_nCol    incremented no of columns
'
'---------------------------------------------------------------
!n = p_nYear
if %p_Mode = "P" then !n = !n - 1 endif
'--- write year or period headings
for !i = 1 to !n
  p_nCol = p_nCol + 1
  %s = @str(p_vYear(!i))
  if %p_Mode = "P" then
    %s = %s + "-" + @right(@str(p_vYear(!i+1)),2)
  endif
  setcell(p_Tab, p_iRow, p_nCol, %s, "r")
  !nc = p_nCol
  if %p_Mode = "A" then
    p_Tab.setwidth(!nc) 8
  else
    p_Tab.setwidth(!nc) 9
  endif
next
endsub

subroutine local zUCase(string %p_t)
'==============================================================
'upper case the first character in a string
'
' Call: %p_t   string
'
' Ret:  %p_t   adjusted string
'
'---------------------------------------------------------------
%p_t = @upper(@left(%p_t, 1)) + @mid(%p_t, 2)
endsub

subroutine zAvg(string %p_Expr, string %p_First, _
  string %p_Last, scalar p_Val)
'==============================================================
'Return average value
'
' Call: %p_Expr     expression to be evaluated
'       %p_First    first year
'       %p_Last     last year
'
' Ret:  p_Val       average value of the expression
'
'---------------------------------------------------------------
smpl %p_First %p_Last
series lib_evs = {%p_Expr}
p_Val = @mean(lib_evs)
delete lib_evs
endsub

subroutine local zRestrict(string %p_tlyr, string %p_First, string %p_Last)
'===============================================================
'remove years not required in the report
'
' Call: %p_tlyr  list of years
'       %p_First first year
'       %p_Last  last year
'
' Ret:  %p_tlyr  adjusted list
'
' Note: if the list is truncated, make sure the first or last available
'       year is included as appropriate
'
'---------------------------------------------------------------
%tl = %p_tlyr
%tl1 = ""
call Token(%tl, " ", %s)
!qfirst = 0
!qlast = 0
!ncut = 0
while %s <> ""
  if %s <= %p_Last and %s >= %p_First then
    %tl1 = %tl1 + %s + " "
    if %s = %p_First then !qfirst = 1 endif
    if %s = %p_Last then !qlast = 1 endif
  else
    !ncut = !ncut + 1
  endif
  call Token(%tl, " ", %s)
wend
if !ncut >= 1 and !qlast = 0 then
  %tl1 = %tl1 + %p_Last
  !ncut = !ncut - 1
endif
if !ncut >= 1 and !qfirst = 0 then
  %tl1 = %p_First + " " + %tl1
endif
%p_tlyr = %tl1
endsub

subroutine local zSplitDesc(string %p_tlv, string %p_Title, _
  string %p_tlDesc)
'==============================================================
'Split descriptor for Table and CSV output
'
' Call: %p_tlv      list of variables from t_BRep
'       %p_Title    combined title from t_BRep
'
' Ret:  %p_tlDesc   split descriptor list
'
' Note: if there is more than one variable in %p_tlv the
'       combined title is parsed as
'         prefix] desc1 and desc2 [suffix
'
'---------------------------------------------------------------

'--- only one item in the list
if @instr(%p_tlv, " ") = 0 then
  %p_tlDesc = %p_Title
  return
endif

%t = @lower(@left(%p_Title,1)) + @mid(%p_Title,2)
%tpre = ""
if @instr(%t, "]") > 0 then
  call Token(%t, "]", %tpre)
endif
%tsuf = ""
if @instr(%t, "[") > 0 then
  %tsuf = %t
  call Token(%tsuf, "[", %t)
endif
!i = @instr(%t, " and ")
%tv1 = @left(%t, !i-1)
%tv2 = @mid(%t, !i+5)
if %tpre <> "" then
  %tv1 = %tpre + " " + %tv1
  %tv2 = %tpre + " " + %tv2
endif
if %tsuf <> "" then
  %tv1 = %tv1 + " " + %tsuf
  %tv2 = %tv2 + " " + %tsuf
endif
call zUCase(%tv1)
call zUCase(%tv2)
%p_tlDesc = %tv1 + "," + %tv2

endsub

subroutine ATable(string %p_tPrefix, string %p_Title, _
  string %p_Alias, string %p_tlArea, _
  string %p_tlName, string %p_tlyr)
'==============================================================
'Create analysis tables
'
' Call: %p_tPrefix  table prefix
'       %p_Title    scenario title
'       %p_Alias    scenario alias
'       %p_tlArea   list of areas
'       %p_tlName   list of names
'       %p_tlyr     list of years
'
' Ret:
'
'---------------------------------------------------------------
for !i = 1 to nTDef
  if t_TDef(!i, 1) = "T" then
    %g = t_TDef(!i, 2)
    call SPBuildTable("tables", %p_tPrefix, %g, _
      %p_Title, %p_Alias, _
      %p_tlArea, %p_tlName, %p_tlyr, t_TDef, nTDef)
  endif
next
endsub

subroutine SPBuildTable(string %p_Page, string %p_tPrefix, _
  string %p_tTDef, string %p_Title, string %p_Alias, _
  string %p_tlArea, string %p_tlName, string %p_tlyr, _
  table p_tDef, scalar p_nDef)
'==============================================================
' Build presentation table
'
' Call: %p_Page     destination page
'       %p_tPrefix  table prefix
'       %p_tTDef    table definition
'       %p_Title    scenario title
'       %p_Alias    scenario alias
'       %p_tlArea   list of areas
'       %p_tlName   list of names
'       %p_tlyr     list of years
'       p_tDef      table of definitions
'       p_nDef      no of rows in the table of definitions
'
' Ret:
'
'---------------------------------------------------------------
statusline table %p_tTDef
pageselect data
for !lib_i = 1 to p_nDef
  if p_tDef(!lib_i, 1) = "T" then
    if p_tDef(!lib_i, 2) = %p_tTDef then
      %lib_tab = %p_tPrefix + "_" + p_tDef(!lib_i,2)
      %lib_tt = p_tDef(!lib_i,3) + "     "
      if %p_Alias <> "" then
        %lib_tt = %lib_tt + "S" _
          + @replace(%p_Alias, "_", "") + " "
      endif
      %lib_tt = %lib_tt + %p_Title
      table {%lib_tab}
      call zSPBuildTable(%p_Page, {%lib_tab}, %lib_tab, _
        %lib_tt, %p_Alias, _
        %p_tlArea, %p_tlName, %p_tlyr, p_tDef, p_nDef, !lib_i)
      delete {%lib_tab}
      return
    endif
  endif   
next
call pLog("definition for table " + %p_tTDef + " not found")
endsub

subroutine local zSPBuildTable(string %p_Page, table p_Tab, _
  string %p_TName, string %p_Title, string %p_Alias, _
  string %p_tlArea, string %p_tlName, string %p_tlyr, _
  table p_tDef, scalar p_nDef, scalar p_iDef)
'==============================================================
' Build presentation table
'
' Call: %p_Page     page to store the result table
'       p_Tab       work table
'       %p_TName    result table name
'       %p_Title    table title
'       %p_Alias    scenario alias
'       %p_tlArea   list of areas
'       %p_tlName   list of names
'       %p_tlyr     list of years
'       p_tDef      table of definitions
'       p_nDef      no of rows in the table of definitions
'       p_iDef      header row
'
' Ret:
'
'---------------------------------------------------------------

'--- locate subtable definitions
for !i = p_iDef + 1 to p_nDef
  if p_tDef(!i,1) = "S" then !nSub = !i endif
  if p_tDef(!i,1) = "R" then !nDef = !i endif
  if p_tDef(!i,1) = "T" then exitloop endif
next

!nrow = 0
'--- set up vector of years
!nyr = 0
call CountTokens(%p_tlyr, " ", !nyr)
vector(!nyr) vyr
%tl = %p_tlyr
for !i = 1 to !nyr
  call Token(%tl, " ", %yr)
  vyr(!i) = @val(%yr)
next
%first = @str(vyr(1))
%last = @str(vyr(!nyr))

'--- header
setcell(p_Tab, 1, 1, %p_Title, "l")
p_Tab.setwidth(1) 30
p_Tab.setindent(1,1) 5

'--- write row and column headings
!ncol = 1 + !nyr
!nrow = 3
p_Tab.setlines(!nrow,1,!nrow,!ncol) +b
!nrow0 = !nrow + 4

for !isub = p_iDef + 1 to !nSub
  %a = p_tDef(!isub,2)            'A or P
  %s = p_tDef(!isub,4)            'subtitle
  call zSPHeader(p_Tab, !nrow, p_tDef, !nDef, !nSub+1, _
  %a, vyr, !nyr, %s)
next

'--- fill the table for each bloc
%tlb = %p_tlArea
%tlbn = %p_tlName
!ninc = !nDef - !nSub + 5
!icentre = @floor((!ncol+1)/2)
while %tlb <> ""
  call Token(%tlb," ",%b)
  call Token(%tlbn,",",%bn)
  setcell(p_Tab, 2, !icentre, %bn, "c")
  %ba = %b + %p_Alias

  '--- put data into the table
  !nrow = !nrow0
  series s
  for !i = !nSub + 1 to !nDef
    !nrow = !nrow + 1
    !icol = 1
    '--- get row values
    %v = p_tDef(!i, 2)
    if %v <> "" then
      %fmt = p_tDef(!i, 4)
      if @left(%fmt,1) = "U" then %fmt = @mid(%fmt, 2) endif
      %v = @replace(%v, "~", %p_Alias)
      %v = @replace(@replace(%v, "??", %b), "?", %ba)
      call EvalS(%v , %first, %last, s)
      '--- write values in subtables
      !n1 = !nrow
      %opt = p_tDef(!i, 5)
'      if %opt = "" then %opt = "N" endif
      for !isub = p_iDef + 1 to !nSub
        %a = p_tDef(!isub,2)            'A or P
        %g = p_tDef(!isub,3)            'formula
        if @instr("%/", @left(%g,1)) >0 then
          if not @isobject("s1") then
            series s1
          endif
          %v1 = @mid(%g,2)
          %v1 = @replace(%v1, "?", %ba)
          call EvalS(%v1 , %first, %last, s1)
          %g = @left(%g,1)
        endif
        if %a = "A" then
          !icol0 = 1
        else
          !icol0 = 2
        endif
        !icol = !icol0
        for !j = !icol0 to !nyr
          !icol = !icol + 1
          %r = ""
          !v = NA
          '--- select formula and generate cell entry
          if %g = "Value" then
            !v = @elem(s, @str(vyr(!j)))
            call SPFormat(!v, %fmt, %r)
          else if %g = "%" then
            if @instr(%opt, "NRP") = 0 then
              !v = 100*@elem(s, @str(vyr(!j))) _
                   / @elem(s1, @str(vyr(!j)))
            endif
            call SPFormat(!v, "f.1", %r)
          else if %g = "/" then
            if @instr(%opt, "NRP") = 0 then
              !v = @elem(s, @str(vyr(!j))) _
                   / @elem(s1, @str(vyr(!j)))
            endif
            call SPFormat(!v, "ft.0", %r)
          else if %g = "Change" then
            !v = (@elem(s, @str(vyr(!j))) _
                  - @elem(s, @str(vyr(!j-1))))
            call SPFormat(!v, %fmt, %r)
          else if %g = "GR" then
            if @instr(%opt, "NGR") = 0 then
              !v1 = @elem(s, @str(vyr(!j)))
              !v0 = @elem(s, @str(vyr(!j-1)))
              !n = vyr(!j) - vyr(!j-1)
              call GrowthRate(!v1, !v0, !n, !v)
            endif
            call SPFormat(!v, "f.1", %r)
          endif
          endif
          endif
          endif
          endif
          '--- write cell entry
          setcell(p_Tab, !n1, !icol, %r, "r")
        next
        !n1 = !n1 + !ninc
      next  
    endif
  next
  '--- write divider lines in data section
  !nrow = !nrow0
  for !i = !nSub + 1 to !nDef
    !nrow = !nrow + 1
    !icol = 1
    '--- get row values
    %v = p_tDef(!i, 2)
    if %v <> "" then
      %s = p_tDef(!i, 4)
      if @left(%s,1) = "U" then
        !n1 = !nrow
        for !isub = p_iDef + 1 to !nSub
          p_Tab.setlines(!n1,2,!n1,!ncol) +b +l
          !n1 = !n1 + !ninc
        next
      endif
    endif
  next
  '--- move the table to the target page  
  %t = %p_TName + "_" + %b
  call zSPCopyTable(p_Tab, %p_Page, %t)
wend
p_Tab.deletecol(1) !ncol
endsub

subroutine zSPCopyTable(table p_t, string %p_Page, _
  string %p_Name)
'==============================================================
' copy table to a named page
'
' Call:  p_t        table on data page
'        %p_Page    destination page
'        %p_Name   destination table name
'
' Ret:
'
'---------------------------------------------------------------
pageselect {%p_Page}
copy data\p_t {%p_Name}
pageselect data
endsub

subroutine local SPFormat(scalar p_v, string %p_fmt, _
  string %p_res)
'==============================================================
'format a number
'
' Call: p_v     table to write
'       %p_fmt  format code
'
' Ret:  %p_res  formatted result
'
'---------------------------------------------------------------
if @isna(p_v) then
  %p_res = "..."
else if %p_fmt = "" then
  call XFormat(p_v, 4, %p_res)
else if @abs(p_v)>= 1000000 then
  %p_res = "***"
else
  %p_res = @str(p_v, %p_fmt)
endif
endif
endif
endsub

subroutine local zSPHeader(table p_Tab, scalar p_nRow, _
  table p_tDef, scalar p_nDef, scalar p_iRow, string %p_Mode, _
  vector p_vyr, scalar p_nyr, string %p_Title)
'==============================================================
'write row and column headers for a sub-table
'
' Call: p_Tab     table to write
'       p_nRow    last used row
'       p_tDef    table definitions
'       p_nDef    no of definitions
'       p_iRow    first row definition 
'       p_Mode    A for annual figures, P for periods
'       p_vyr     vector with list of years
'       p_nyr     number of years
'       %p_Title  title to display under col headings
'
' Ret:  p_Tab     updated table
'       p_nRow    last used row
'
'---------------------------------------------------------------
!nrow = p_nRow + 1
!ncol = 1
if %p_Mode = "P" then !ncol = 2 endif
!nrow = !nrow + 1
call zColHeading(p_Tab, !nrow, %p_Mode, p_vyr, p_nyr, !ncol)
!icol = @floor((!ncol+1)/2)
!nrow = !nrow + 2
setcell(p_Tab, !nrow, !icol, %p_Title, "c")
p_Tab.setlines(!nrow,1,!nrow,!ncol) +t
!nrow1 = !nrow
for !i = p_iRow to p_nDef
  !nrow = !nrow + 1
  setcell(p_Tab, !nrow, 1, p_tDef(!i, 3), "l")
next
!nrow = !nrow + 1
p_Tab.setlines(!nrow1,1,!nrow,1) +r
p_Tab.setindent(!nrow1,1,!nrow,1) 5
p_Tab.setlines(!nrow,1,!nrow,!ncol) +b
p_nRow = !nrow
endsub

subroutine local GrowthRate(scalar p_v1, scalar p_v0, _
  scalar p_ny, scalar p_vgr)
'==============================================================
'return a compound growth rate or n.a.
'
' Call: p_v1    end value
'       p_v0    startig value
'       p_ny    number of years
'
' Ret:  p_vgr   compound growth rate (% p.a.) or na
'
'---------------------------------------------------------------
p_vgr = NA
if p_v0 = 0 then return endif
!v = p_v1/p_v0
if !v <=0 then return endif
p_vgr = 100*(exp(log(!v)/p_ny)-1)
endsub

subroutine ListGeo(string %p_geo, scalar p_ncol, _
  string %p_tl)
'==============================================================
'return a list of area codes or names
'
' Call: p_geo  B blocs, F first blocs, O other blocs
'              G groups, W world
'       p_ncol 1 codes, 2 names
'
' Ret:  p_tl   list of codes or names
'
'---------------------------------------------------------------
if %p_geo = "B" then
  !lib_is = 1
  !lib_ie = nBloc
else if %p_geo = "F" then
  !lib_is = 1
  !lib_ie = !nFirstBlocs
else if %p_geo = "O" then
  !lib_is = !nFirstBlocs + 1
  !lib_ie = nBloc
else if %p_geo = "G" then
  !lib_is = nWorld + 1
  !lib_ie = nArea
else if %p_geo = "W" then
  !lib_is = nWorld
  !lib_ie = !lib_is
endif
endif
endif
endif
endif  
call ListCol(t_Bloc, !lib_is, !lib_ie, p_ncol, " ", %p_tl)
endsub
