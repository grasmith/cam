'PROGRAM: mb.prg          Copyright (C) 2014 Alphametrics Co. Ltd.
'
' CAM version 5.2
'
' Functions to define and execute a multi-bloc budget
'
'---------------------------------------------------------------
' MBDef(t_Def, nDef, %group, %tlmem, %tlrev, %tlexp)
' MBBuild(m, t_Def, nDef, %group, %size, %tlrev, %tlexp)
' MBTrans(m, %p_group, %p_bloc_list, %p_start, %p_actual, %p_debtpc, %p_scale)
' MBRep(tDef, nDef, %group, %scenario, %actual, %predict, max)
'---------------------------------------------------------------

'subroutine MBDef(table p_tDef, scalar p_nDef, string %p_group, _
'  string %p_tlmem, %p_tlmem_sc, string %p_tlrev, string %p_tlexp)
subroutine MBDef(table p_tDef, scalar p_nDef, string %p_group, _
  string %p_tlmem, string %p_tlrev, string %p_tlexp, _
  string %p_rev_wt, string %p_exp_wt)
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
'       %p_rev_wt list of per-bloc weights for revenue
'       %p_exp_wt list of per-bloc weights for expenditure
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

' Find row that matches group identifier
!i = 0
for !j = 1 to p_nDef
  if p_tDef(!j, 1) = %p_group then
    !i = !j
    exitloop
  endif
next

' If no row found, then it's a new entry
if !i = 0 then
  p_nDef = p_nDef + 1
  !i = p_nDef
endif

' Add the information to the row
p_tDef(!i, 1) = %p_group
p_tDef(!i, 2) = %p_tlmem
p_tDef(!i, 3) = %p_tlrev
p_tDef(!i, 4) = %p_tlexp
p_tDef(!i, 5) = %p_rev_wt
p_tDef(!i, 6) = %p_exp_wt
endsub

subroutine MBBuild(model p_m, table p_tDef, scalar p_nDef, _
  string %p_group, string %p_size, _
  string %p_tlrev, string %p_tlexp, string %p_rev_lag)
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
'       %p_rev_lag lag revenues by this many years
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

' locate row of defition table based on group identifier
!igroup = 0
for !j = 1 to p_nDef
  if p_tDef(!j, 1) = %p_group then
    !igroup = !j
    exitloop
  endif
next

' if not found, then abandon
if !igroup = 0 then
  call pLog("No budget definition for " + %p_group)
  return
endif  

' extract list of group members
%tlmem = p_tDef(!igroup, 2)
' count em
!num_mem = @wcount(%tlmem)

' extract revenue weights
%m_wt = p_tDef(!igroup, 5)
%x_wt = p_tDef(!igroup, 6)

' calculate weighting totals (long winded)
%m_wt_tmp = %m_wt
%x_wt_tmp = %x_wt

!m_wt_tot = 0
!x_wt_tot = 0

while %m_wt_tmp <> ""
	call Token(%m_wt_tmp, " ", %w)
	!m_wt_tot = !m_wt_tot + @val(%w)
wend

while %x_wt_tmp <> ""
	call Token(%x_wt_tmp, " ", %w)
	!x_wt_tot = !x_wt_tot + @val(%w)
wend

' create group suffix for variables
%gp = %p_group + "_"

'--- calculate group GDP as a sum of member GDP figures
%y = %gp + "y$"                   'series name

' replace ? by member names and concatenate with +
call BlocList("Y$_?", "+", %tlmem, %s)    'formula
call zMBAppend(p_m, %y, %s)                'add series to model

'--- budget as % of GDP (exogenous)

'--- create series containing GDP shares
%pcyg = %gp + "pcyg"                'series name
series {%pcyg} = 0                         'create series
' "pop" last element of list of GDP shares
%tl = %p_size
call RToken(%tl, " ", %s)                   'long-term value
smpl %actual+1 %end
{%pcyg} = {%s}
' fill initial values if required
if %tl <> "" then                           'immediate values
  %s = @replace(%tl," ",",")
  {%pcyg}.fill(s) {%s}
endif


smpl %start %end

'--- use group GDP and % share series to calculate budget size
%yg  = %gp + "yg$"
%ygxd = %gp + "ygxd$"
call zMBAppend(p_m, %yg, %pcyg + "(-"+%p_rev_lag+")*" + %y + "/100")
call zMBAppend(p_m, %ygxd, %pcyg + "*" + %y + "/100")

'--- loop for revenue and expenditure

' -- get list of revenue *shares*
%tla = %p_tlrev

!iside = 1
' repeat for revenue and expenditure
for %arx m x
  ' extract list of items from definition table
  %tldef = p_tDef(!igroup, 2+!iside)

  '--- loop over items of revenue or expenditure 
  !item = 0 ' item counter
  %tlitem = ""
  %tld = %tldef
  while %tld <> ""
    ' construct suffix for series names
    !item = !item + 1
    %a = @trim(@str(!item))
    %tlitem = %tlitem + %a + " "
    %ai$ = %arx + %a + "$"
    '--- "pop" item definition
    call Token(%tld, ";", %basis)
	'--- "pop" basis english description string
    call Token(%basis, ":", %s)

    if @instr(%basis, ":") > 0 then
	  '--- basis has additional rate ceiling and/or standard
      %maxrate = %basis
      call Token(%maxrate, ":", %basis)
      call Token(%maxrate, ":", %std)
      '--- %maxrate now contains the ceiling value. 
	  '--- %std either contains standard or is empty
      if %std <> "" then
		'--- adjust the basis definition to be the difference
		'--- between the basis value and the standard
        %s = %std + "-(" + %basis + ")"
        %basis = "@iif(" + %s + ">0," + %s + ",0)"
      endif  
    else
      ' --- no basis ceiling or standard
      %maxrate = ""
    endif
	
	' --- express basis in PPP dollars
    %basis = "(" + %basis + ")*rx_?"

    '--- get budget share for this item
    call Token(%tla, " ", %share)
    if %share = "" then %share = "0" endif
    
    '--- total group basis and rate series names
    %ygbai = %gp + "ygb" + %ai$ ' eg. EU_ygbm1$
    %ygrai = %gp + "ygr" + %ai$ ' eg. EU_ygrm1$

    '--- bloc basis and allocation series names
    %ygbaib = %ygbai + "_?"           ' eg. EU_ygbm1$_?
    %ygaib = %gp + "yg" + %ai$ + "_?" ' eg. EU_ygm1$_?

    '--- member bloc loop
    %tlmem = p_tDef(!igroup, 2)
    while %tlmem <> ""
	  '--- pop member from list
      call Token(%tlmem, " ", %b)

      call zMBAppend(p_m, @replace(%ygbaib, "?", %b), _
                   @replace(%basis, "?", %b))

      '--- bloc allocation equals basis multiplied by rate
      %s = %ygrai + "*" + %ygbaib ' eg. EU_ygrm1$*EU_ygbm1$
      call zMBAppend(p_m, @replace(%ygaib, "?", %b), _
                   @replace(%s, "?", %b))
    wend

    '--- add up to obtain total basis
    %tlmem = p_tDef(!igroup, 2)
    call BlocList(%ygbaib, "+", %tlmem, %s)
	call zMBAppend(p_m, %ygbai, %s)

    '--- calculate rate required to achieve desired GDP share
	if %arx = "x" then
		'--- scale expenditures.
		%s = "@nan(" + %ygxd + "*0.01*" + %share + "/" + %ygbai + ",0)"
	else
		%s = "@nan(" + %yg + "*0.01*" + %share + "/" + %ygbai + ",0)"		  
	endif

    if %maxrate <> "" then
      %s = "@iif(" + %s + ">" + %maxrate + "," _
                   + %maxrate + "," + %s + ")"
    endif
    call zMBAppend(p_m, %ygrai, %s)
  wend

  '--- now we have the rate, loop over blocs and use
  '--- it to calculate allocation for each bloc
  %yga = %gp + "yg" + %arx
  %tlmem = p_tDef(!igroup, 2)
  while %tlmem <> ""
	'--- pop member from list
    call Token(%tlmem, " ", %b)
    %gb = "$_" + %b
    '--- pop member weight from list
    %wt = "%"+%arx+"_wt"
    %wt_tmp = %wt
    call Token({%wt_tmp}, " ", %w)
    '--- set member weight to one if empty string
    if @isna(@val(%w)) then
      %w = "1"
    endif
    ' -- calculate scale factor from weights
    %wt_tot = "!"+%arx+"_wt_tot"
    '!scale = ({%w})/{%wt_tot})'*!num_mem
    !scale = ({%w}*!num_mem)/{%wt_tot}
    call pLog(%arx+" scale for "+%b+"=" + @str(!scale))

    call BlocList(%yga + "?" + %gb, "+", %tlitem, %s)
    call zMBAppend(p_m, %yga + %gb, @str(!scale)+"*("+%s+")")
  wend
  '--- sum over bloc allocations to obtain
  '--- total budget revenue or expenditure
  %yga = %yga + "$"
  %tlmem = p_tDef(!igroup, 2)
  call BlocList(%yga + "_?", "+", %tlmem, %s)
  call zMBAppend(p_m, %yga, %s)

  '--- switch to expenditure side and repeat
  %tla = %p_tlexp
  !iside = !iside + 1
next

'--- budget surplus or deficit
%ygb = %gp + "ygm$-" + %gp + "ygx$"

'--- loop for impact on each bloc
'--- sum contributions and recipets for each bloc 
'--- to obtain total effect on revenues.
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

	'--- construct a series which keeps track of the 
	'--- federal net position as a cumulation of 
	'--- budget surplus or deficits plus debt transfers

	%gpdebt = %gp + "LG"
	'call zMBAppend(p_m, %gpdebt, %gpdebt+"(-1)*rpfa_"+%b+"+LGADJ_"+%b+"-"+%ygb+"/rx_"+%b)
	call zMBAppend(p_m, %gpdebt, %gpdebt+"(-1)*rpfa_"+%b+"+LGADJ_"+%b)
	%gpdebt2 = %gp + "LG2"
	call zMBAppend(p_m, %gpdebt2, %gpdebt2+"(-1)*rpfa_"+%b+"-("+%ygb+")/rx_"+%b)
	{%gpdebt}.fill 0
	{%gpdebt2}.fill 0

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
'call pLog(%p_series + "=" + %p_expr) ' uncomment for debugging
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

subroutine MBTrans(model p_m, string %p_group, _
	string %p_bloc_list, string %p_start, _
	string %p_actual, string %p_debtpc, string %p_scale)
'=====================================================
' Set up transfers of sovereign debt from national balance sheets
' onto a 'federal' budget
'
' Call: p_m  model to which equations are appended
'       %p_group  group identifier
'       %p_bloc_list list of blocs involved in the transfers
'       %p_actual the last 'actual' data
'       %p_debtpc the debt-to-GDP ratio above which debt is to be transferred
'       %p_scale  how much of the excess debt to remove in each period
' 
' Ret:
'
' NB. 'federal' budget is actually the balance sheet of the first 
' bloc.


	call pLog("Initialising debt transfers for "+%p_group)
	'--- create series containing debt proportions to shift
	%pctfr = %p_group + "_pctfr"                'series name
	series {%pctfr} = 0                         'create series
	' "pop" last element of list of GDP shares
	%tl = %p_scale
	call RToken(%tl, " ", %s)                   'long-term value
	smpl %actual+1 %end
	{%pctfr} = {%s}
	' fill initial values if required
	if %tl <> "" then                           'immediate values
		%s = @replace(%tl," ",",")
		{%pctfr}.fill(s) {%s}
	endif
	call pLog("Debt percentage series name" + %pctfr)
	smpl %start %end
	' debt is actually transferred to the balance sheet of the first bloc
	call First(%p_bloc_list, " ", %debt_home)

	%total_tfr = ""

	for %b {%p_bloc_list}
		%pctfrb = %p_group+"_tfr_"+%b+"_"+%debt_home

		call zMBAppend(p_m, %pctfrb, _
		  "@iif((LG_"+%b+"(-1)/V_"+%b+")>"+%p_debtpc+"," + _
		  "(LG_"+%b+"(-1)-("+%p_debtpc+"*V_"+%b+"))*"+%pctfr+", 0)")

		if %b<>%debt_home then
			call zMBAppend(p_m, "LGADJ_"+%b, "-"+%pctfrb)
			if %total_tfr="" then
				%total_tfr=%pctfrb
			else
				%total_tfr=%total_tfr + "+" + %pctfrb
			endif
		endif
	next
	call zMBAppend(p_m, "LGADJ_"+%debt_home, %total_tfr)
endsub
