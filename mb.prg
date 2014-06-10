'PROGRAM: mb.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' Functions to define and execute a multi-bloc budget
'
' updated: FC 26/10/2011
'
'---------------------------------------------------------------
' MBDef(t_Def, nDef, %group, %tlmem, %tlrev, %tlexp)
' MBBuild(m, t_Def, nDef, %group, %size, %tlrev, %tlexp)
' MBRep(tDef, nDef, %group, %scenario, %actual, %predict, max)
'---------------------------------------------------------------

'mode quiet
'tic
'include "set"
'pageselect data
'output(s) sp_log
'show sp_log
'%actual = "2012"
'%predict = "2030"
'call MBRep(t_MB, nMB, "EU", "4 Multipolar collaboration", %actual, %predict, 8)
'stop

subroutine MBDef(table p_tDef, scalar p_nDef, string %p_group, _
  string %p_tlmem, string %p_tlrev, string %p_tlexp)
'==================================================================
' define a multi-bloc budget system
'
' Call: p_tDef    definition table
'       p_nDef    number of rows
'       %p_group  group identifier
'       %p_tlmem  list of participating blocs separated by blanks
'                 the first bloc absorbs any c/a imbalance
'       %p_tlrev  list of revenue items separated by ;
'       %p_tlexp  list of expenditure items separated by ;
'
' Ret:  p_tDef    updated definition table
'       p_nDef    updated number of rows
'
' Note: each revenue or expenditure item is defined as follows
'         name:basis  or name:basis:standard:maxrate
' where
'  name is a short text
'  basis is an expression for the base on which the budget
'    amount will be assessed with ? as a placeholder for the
'    member bloc code
'  standard (optional) is an expression for the normal value
'    of the basis: if a standard is provided, the budget
'    amount is assessed on the gap (shortfall) between the
'    actual value of the basis and the standard. The assessment
'    is zero if the actual value exceeds the standard.
'  maxrate (optional) specifies the maximum rate at which revenue 
'    or expenditure will be assessed as a fraction of the gap.
'
'---------------------------------------------------------------
!i = 0
for !j = 1 to p_nDef
  if p_tDef(!j, 1) = %p_group then
    !i = !j
    exitloop
  endif
next
if !i = 0 then
  p_nDef = p_nDef + 1
  !i = p_nDef
endif  
p_tDef(!i, 1) = %p_group
p_tDef(!i, 2) = %p_tlmem
p_tDef(!i, 3) = %p_tlrev
p_tDef(!i, 4) = %p_tlexp
endsub

subroutine MBBuild(model p_m, table p_tDef, scalar p_nDef, _
  string %p_group, string %p_size, _
  string %p_tlrev, string %p_tlexp)
'==================================================================
' append variables and equations for a multi-bloc budget
' system to a model
'
' Call: p_m  model to which equations are appended
'       p_tDef  table of definitions
'       p_nDef  number of rows
'       %p_group  group identifier
'       %p_size  size of the budget as % of group GDP
'            may be a single value or list of values
'            the last value is repeated as necessary
'            up to the end date
'       %p_tlrev  shares for revenue items separated by blanks
'       %p_tlexp  shares for expenditure items separated by blanks
' 
' Ret:
'
' Note: shares are specified as percentages and should normally
' sum to 100. If the total of revenue and expenditure shares
' is different (e.g. expenditure shares exceed revenue) the budget
' is unbalanced. The budget surplus or deficit is matched by a
' corresponding credit or debit on the current account of the
' first member bloc (effectively the budget authority is treated
' as a resident of this bloc). 
'
' Discretionary items limited by a maximum rate may not use the
' full amount (share x budget size x GDP). This is another
' reason why the budget may show a surplus or deficit. 
'
'---------------------------------------------------------------
smpl %start %end

!igroup = 0
for !j = 1 to p_nDef
  if p_tDef(!j, 1) = %p_group then
    !igroup = !j
    exitloop
  endif
next
if !igroup = 0 then
  call pLog("No budget definition for " + %p_group)
  return
endif  
%tlmem = p_tDef(!igroup, 2)
%gp = %p_group + "_"

'--- group GDP
%y = %gp + "y$"                   'series name
call BlocList("Y$_?", "+", %tlmem, %s)    'formula
call zMBAppend(p_m, %y, %s)                'add series to model

'--- budget as % of GDP (exogenous)
%pcyg = %gp + "pcyg"                'series name
series {%pcyg} = 0                         'create series
%tl = %p_size
call RToken(%tl, " ", %s)                   'long-term value
smpl %actual+1 %end
{%pcyg} = {%s}
if %tl <> "" then                           'immediate values
  %s = @replace(%tl," ",",")
  {%pcyg}.fill(s) {%s}
endif
smpl %start %end

'--- budget size
%yg = %gp + "yg$"
call zMBAppend(p_m, %yg, %pcyg + "*" + %y + "/100")

'--- loop for revenue and expenditure
%tla = %p_tlrev
!iside = 1
for %arx m x
  %tldef = p_tDef(!igroup, 2+!iside)
  '--- item loop
  !item = 0
  %tlitem = ""
  %tld = %tldef
  while %tld <> ""
    !item = !item + 1
    %a = @trim(@str(!item))
    %tlitem = %tlitem + %a + " "
    %ai$ = %arx + %a + "$"

    '--- item definition
    call Token(%tld, ";", %basis)
    call Token(%basis, ":", %s)
    '--- basis with or without ceiling
    if @instr(%basis, ":") > 0 then
      %maxrate = %basis
      call Token(%maxrate, ":", %basis)
      call Token(%maxrate, ":", %std)
      if %std <> "" then
        %s = %std + "-(" + %basis + ")"
        %basis = "@iif(" + %s + ">0," + %s + ",0)"
      endif  
    else
      %maxrate = ""
    endif
    %basis = "(" + %basis + ")*rx_?"

    '--- budget share
    call Token(%tla, " ", %share)
    if %share = "" then %share = "0" endif
    
    '--- total basis and rate
    %ygbai = %gp + "ygb" + %ai$
    %ygrai = %gp + "ygr" + %ai$
    '--- bloc basis and allocation
    %ygbaib = %ygbai + "_?"
    %ygaib = %gp + "yg" + %ai$ + "_?"
    '--- member bloc loop
    %tlmem = p_tDef(!igroup, 2)
    while %tlmem <> ""
      call Token(%tlmem, " ", %b)
      '--- bloc basis
      call zMBAppend(p_m, @replace(%ygbaib, "?", %b), _
                   @replace(%basis, "?", %b))
      '--- bloc allocation
      %s = %ygrai + "*" + %ygbaib
      call zMBAppend(p_m, @replace(%ygaib, "?", %b), _
                   @replace(%s, "?", %b))
    wend
    '--- total basis
    %tlmem = p_tDef(!igroup, 2)
    call BlocList(%ygbaib, "+", %tlmem, %s)
    call zMBAppend(p_m, %ygbai, %s)
    '--- rate
    %s = "@nan(" + %yg + "*0.01*" + %share + "/" + %ygbai _
         + ",0)"
    if %maxrate <> "" then
      %s = "@iif(" + %s + ">" + %maxrate + "," _
                   + %maxrate + "," + %s + ")"
    endif
    call zMBAppend(p_m, %ygrai, %s)
  wend
  '--- total bloc receipt or payment
  %yga = %gp + "yg" + %arx
  %tlmem = p_tDef(!igroup, 2)
  while %tlmem <> ""
    call Token(%tlmem, " ", %b)
    %gb = "$_" + %b
    call BlocList(%yga + "?" + %gb, "+", %tlitem, %s)
    call zMBAppend(p_m, %yga + %gb, %s)
  wend
  '--- total budget revenue or expenditure
  %yga = %yga + "$"
  %tlmem = p_tDef(!igroup, 2)
  call BlocList(%yga + "_?", "+", %tlmem, %s)
  call zMBAppend(p_m, %yga, %s)
  '--- switch to expenditure side
  %tla = %p_tlexp
  !iside = !iside + 1
next
'--- budget surplus or deficit
%ygb = %gp + "ygm$-" + %gp + "ygx$"

'--- loop for impact on each bloc
%tlmem = p_tDef(!igroup, 2)
!qfirst = 1
while %tlmem <> ""
  call Token(%tlmem, " ", %b)
  %g = "$_" + %b
  '--- net transfer to bloc
  %ygbb = "(" + %gp + "ygx" + %g _
              + "-" + %gp + "ygm" + %g + ")"

  '--- c/a transfer
  %s1 = "BITADJ$_" + %b
  %s = %ygbb
  '--- in case of first bloc, add the surplus or deficit
  if !qfirst then
    %s = "(" + %s + "+" + %ygb + ")"
  endif
  call zMBAppend(p_m, %s1, %s)

  '--- govt revenue impact
  %s1 = "YGADJ_" + %b
  %s = %ygbb + "/rx_" + %b
  call zMBAppend(p_m, %s1, %s)
  !qfirst = 0
wend
smpl %actual+1 %end
endsub

subroutine zMBAppend(model p_m, string %p_series, _
  string %p_expr)
'==================================================================
' create series and add identity to model
'
' Call: p_m         model
'       %p_series   series name
'       %p_expr     RHS expression
'
' Ret:  
'
'---------------------------------------------------------------
series {%p_series} = 0
p_m.append @identity {%p_series} = {%p_expr}
endsub

subroutine local MBRep(table p_tDef, scalar p_nDef, _
  string %p_group, string %p_scenario, _
  string %p_actual, string %p_predict, scalar p_max)
'=======================================================
' generate multi-bloc budget reports
'
' Call: p_tDef       definition table
'       p_nDef       number of rows
'       %p_group     budget group
'       %p_scenario  scenario alias and description
'       %p_actual    last actual
'       %p_predict   last predicted
'       p_max        max years to display
'
' Ret:  group and bloc tables on tables page
'
'---------------------------------------------------------------

'--- locate the budget definition
!igroup = 0
for !j = 1 to p_nDef
  if p_tDef(!j, 1) = %p_group then
    !igroup = !j
    exitloop
  endif
next
if !igroup = 0 then
  call pLog("No budget definition for " + %p_group)
  return
endif  

call pLog(%p_group + " budget reports")

'--- get list of member blocs and their names
%tlmem = p_tDef(!igroup, 2)
%tl = %tlmem
while %tl <> ""
  call Token(%tl, " ", %b)
  call BlocName(%b, %s)
  %tlname = %tlname + "," + %s
wend
%tlname = @mid(%tlname, 2)
%gp = %p_group + "_"

'--- build lists of revenue and expenditure items to report 
'--- loop for revenue and expenditure
!iside = 1
for %arx m x
  %tld = p_tDef(!igroup, 2+!iside)
  '--- item loop
  !item = 0
  while %tld <> ""
    !item = !item + 1
    %a = @trim(@str(!item))
    %ai$ = %arx + %a + "$"

    '--- item definition
    call Token(%tld, ";", %basis)
    call Token(%basis, ":", %desc)

    '--- total basis and rate
    %ygbai = %gp + "ygb" + %ai$
    %ygrai = %gp + "ygr" + %ai$
    '--- bloc allocation
    %ygaib = %gp + "yg" + %ai$ + "_?"
    '--- total revenue or expenditure by item
    %tlygm = %tlygm + "R;" + %ygbai + "~*" _
      + %ygrai + "~/1000;" + %desc + ";"
    if %tld = "" and !iside = 1 then %tlygm = %tlygm + "U" endif
    %tlygm = %tlygm + "ft.0:"
    '--- percentage rate by item
    %tlygr = %tlygr + "R;" + %ygrai + "~;" + %desc + ";"
    if %tld = "" and !iside = 1 then %tlygr = %tlygr + "U" endif
    %tlygr = %tlygr + ":"
    '--- bloc impact
    %tlygb = %tlygb + "R;" + %ygaib + "/(1000*rx_?);" + %desc + ";"
    if %tld = "" and !iside = 1 then %tlygb = %tlygb + "U" endif
    %tlygb = %tlygb + "ft.1:"
  wend
  '--- switch to expenditure side
  !iside = !iside + 1
next

'--- budget tables
table t_Def
scalar nDef = 0

'--- summary table
call LoadTable(t_Def, nDef, _
  "T;ygm;" + %p_group + " BUDGET:" _
  + "S;A;Value;{$ billion, 2005 pp):" _
  + "S;P;GR;{growth rates, % p.a.):" _
  + "R;"+%gp+"y$~/1000;Total GDP;ft.0:" _
  + "R;"+%gp+"ygm$~/1000;Revenue;ft.0:" _
  + "R;100*"+%gp+"ygm$~/" + %gp + "y$~;" _
    + "Revenue as % of GDP;ft.1:" _
  + "R;"+%gp+"ygx$~/1000;Expenditure;ft.0:" _
  + "R;("+%gp+"ygm$~-"+%gp+"ygx$~)/1000;" _
    + "Budget balance;ft.0;NGR:" _
  + "R;100*("+%gp+"ygm$~-"+%gp+"ygx$~)/"+%gp+"y$~;" _
    + "Balance as % of GDP;Uft.1;NGR:" _
  + %tlygm _
)

'--- rate table
call LoadTable(t_Def, nDef, _
  "T;ygmr;" + %p_group + " BUDGET RATES:" _
  + "S;A;Value;:" _
  + %tlygr _
)

'--- bloc table  
call LoadTable(t_Def, nDef, _
  "T;ygmb;" + %p_group + " BUDGET:" _
  + "S;A;Value;{$ billion, 2005 pp):" _
  + "S;P;GR;{growth rates, % p.a.):" _
  + "R;y_?/1000;Total GDP;ft.1:" _
  + "R;yg_?/1000;Government revenue;ft.1:" _
  + "R;100*yg_?/y_?;Government revenue as % of GDP;ft.1:" _
  + "R;"+%gp+"ygm$_?/(1000*rx_?);EU budget contributions;ft.1:" _
  + "R;"+%gp+"ygx$_?/(1000*rx_?);EU budget receipts;ft.1:" _
  + "R;YGADJ_?/1000;Net receipts;ft.1;NGR:" _
  + "R;100*YGADJ_?/Y_?;Net receipts as % of GDP;ft.1;NGR:" _
  + "R;(yg_?-g_?)/1000;Budget balance;ft.1;NGR:" _
  + "R;(yg_?-g_?-YGADJ_?)/1000;" _
    + "Balance excl EU transfers;Uft.1;NGR:" _
  + %tlygb _
)

'--- select years to list
vector vyr
!nyr = 1
%first = @str(@val(%p_actual)+1)
call YearList(%first, %p_predict, p_max, vyr, !nyr)
%tlyr = @str(vyr(1))
for !i = 2 to !nyr
  %tlyr = %tlyr + " " + @str(vyr(!i))
next

%desc = %p_scenario
call Token(%desc, " ", %alias)
%alias = "_" + %alias

call SPBuildTable("tables", %p_group + "0", "ygm", _
      %desc, %alias, _
      %p_group, "", %tlyr, t_Def, nDef)

call SPBuildTable("tables", %p_group + "0", "ygmr", _
      %desc, %alias, _
      %p_group, "", %tlyr, t_Def, nDef)

call SPBuildTable("tables", %p_group + "1", "ygmb", _
      %desc, %alias, _
      %tlmem, %tlname, %tlyr, t_Def, nDef)
endsub
