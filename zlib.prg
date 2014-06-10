'LIBRARY: zlib.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM Version 5.2
'
' library routines
'
' Note: routines whose names begin with z are internal. Names
' beginning with lib_ are reserved and should not be used
' elsewhere.
'
' updated: FC 11/04/2013
'
'---------------------------------------------------------------
' Adjusted(%Var, %Name)
' AliasExpr(%Expr, %Alias, %Result)
' AppendIdent(%tl)
' ArraySort(y, x, n)
' AssignSeries(%Var, %Val, %First, %Last)
' BlocCreateSeries(%Scope, %tlSpec, %Alias)
' BlocEval(%Scope, %tlSpec, %Alias)
' BlocList(%Expr, %Sep, %tlBloc, %Result)
' BlocMem(%Group, %tlBloc)
' BlocMinVal(%Scope, %tlVar)
' BlocName(%Bloc, %Name)
' CopyAliasValues(%Source, %Dest, %First, %Last)
' CountTokens(%t, %a, n)
' CreateModelFile(%Source, %File)
' CreateSeries(%Var)
' DeleteObjects(%List)
' DerivedSeries(%First, %Last, %Alias)
' Eval(%Expr, %Year, v)
' EvalP(p, %Expr, %First, %Last, mat)
' EvalS(%Expr,%First, %Last, ser)
' Exists(%Obj, q)
' FillSeries(%Var, %Val, %First, %Last)
' FindEq(%Var, iEq)
' Gini(%Source, %Pop, %Gini, %RLow, %Alias, %Start, %End)
' ListCol(table t, nrow, iCol, %Sep, %Result)
' LoadTable(table t, nrow, %tlrows)
' ModelAppend(%Model, %Identity)
' NFormat(Val, %Res)
' OpenSpool(%Name)
' pAbort(%tMsg)
' Parse(%tExpr, %Next)
' pEnd()
' pLog(%tMsg)
' PoolId(pool p, %Mem, iMem)
' ReadTable(%Name, iRow, iCol, %Cell)
' ROWGen(%Var, %Bloc, %tlSc, %RVar)
' RToken(%t, %a, %r)
' ScenarioExpr(%tExpr, %Alias, %tRes)
' SolveModel(m, %tlOption, %Actual, %Predict)
' SortTable(%Page, table tSource, !nRow, !nCol, %Dir, %OrderBy, %tResult)
' Theil(%vb, %vw, %s)
' Token(%t, %a, %r)
' Unadjusted(%Var, %Name)
' VarParts(%Var, %Root, %Mem)
' WriteCell(%Tab, iRow, iCol, %Text, %Align)
' XFormat(Val, DP, %Res)
'---------------------------------------------------------------

subroutine local Adjusted(string %Var, string %Name)
'==================================================
'return name of adjusted variable
'
' Call: %Var  name of variable
'
' Ret:  %Name name of adjusted version with U removed
'
'---------------------------------------------------------------
%v = %Var
call Token(%v, "_", %v1)
if @upper(@right(%v1,1)) = "U" then
  %v1 = @left(%v1, @len(%v1)-1)
endif
%Name = %v1
if %v <> "" then %Name = %Name + "_" + %v endif
endsub

subroutine local AliasExpr(string %p_Expr, string %p_Alias, _
  string %p_Result)
'==================================================
'Insert alias at the end of series names in an expression
'
' Call: %p_Expr    expression with series names and no aliases
'       %p_Alias   scenario alias or null
'
' Ret:  %p_Result  adjusted expression
'
'---------------------------------------------------------------
if %p_Alias = "" then
  %p_Result = %p_Expr
else
  %s = %p_Expr
  %t = ""
  while %s <> ""
    call Parse(%s, %g)
    '-- series names have more than 1 char, don't begin
    '   with @ and include an underscore after the var name
    if @len(%g) > 1 and @left(%g,1) <> "@" _
    and @instr(@mid(%g,2), "_") > 0 then
      %g = %g + %p_Alias
    endif
    %t = %t + %g
  wend
  %p_Result = %t
endif
endsub

subroutine AppendIdent(string %p_tl)
'==================================================
'Append sets of identities to the model
'
' Call: %p_tl  list of specifications
'
' Ret:
'
' Note: specifications are separated by : (colon)
'   each specification has three fields separated by ;
'     scope - B BW W WG G or BWG  (bloc, world and/or group)
'     model - i (inner) or o (outer)
'     list of identities separated by blank (space)
'   an identity may be specified as follows
'     var         summation over blocs
'     var1(var2)  weighted summation over blocs
'     var=expr    formula
'   var and expr may contain ? to denote a geo or bloc code
'
' Example identities:
'   H_?             H = sum(H) total over blocs
'   ph_?(H_?)       ph = sum(ph*H)/sum(H) total over blocs
'   pewsav=pe_w     world formula (no substitution)
'   rx_?=ph_?/ph_w  group or bloc formula with substitution
'
' Note: BG will be interpreted as BGW
'
'---------------------------------------------------------------
%lib_tl = %p_tl
'--- loop for multiple specifications
while 1=1
  call Token(%lib_tl, ":", %lib_spec)
  if %lib_spec = "" then exitloop endif
  '--- extract scope and model
  call Token(%lib_spec, ";", %lib_scope)
  call Token(%lib_spec, ";", %lib_model)
  %lib_model = "m_wm" + %lib_model
  '--- fix range of areas to process
  !lib_is = 1
  !lib_ie = 1
  call zScope(%lib_Scope, !lib_is, !lib_ie)
  '--- area loop
  for !lib_i = !lib_is to !lib_ie
    call zAppendIdent(t_Bloc(!lib_i, 1), t_Bloc(!lib_i, 3), _
        %lib_model, %lib_spec)
  next
wend
endsub  

subroutine zScope(string %p_Scope, scalar p_is, _
  scalar p_ie)
'==================================================
'Select region of bloc table to match a given scope
'
' Call: %p_Scope   B G W BW GW BGW or similar
'
' Ret:  p_is  first row of t_Bloc
'       p_ie  last row of t_Bloc
'
' Note: BG is interpreted as BWG (includes world totals)
'
'---------------------------------------------------------------
if @instr(%p_Scope,"B") > 0 then
  p_is = 1
else
  if @instr(%p_Scope,"W") > 0  then
    p_is = nWorld
  else
    p_is = nWorld + 1
  endif
endif
if @instr(%p_Scope,"G") > 0  then
  p_ie = nArea
else
  if @instr(%p_Scope,"W") > 0  then
    p_ie = nWorld
  else
    p_ie = nBloc
  endif
endif
endsub

subroutine local zAppendIdent(string %p_lhb, string %p_rhb, _
  string %p_model, string %p_tlident)
'==================================================
'Append identies for one geo to the model
'
' Call: %p_lhb     lhs geo (bloc, group or world)
'       %p_rhb     list of members separated by blank
'       %p_model   name of model object
'       %p_tlident list of identities separated by blank
'
' Ret:
'
' Note: see AppendIdent for methods of specifying an identity
'
'---------------------------------------------------------------

%tl = %p_tlident
'--- loop for multiple identities
while 1=1
  call Token(%tl, " ", %ident)
  if %ident = "" then exitloop endif
  '--- check for formula
  if @instr(%ident, "=") > 0 then
    %ident = @replace(%ident, "?", %p_lhb)
  '--- if not a formula, expand as a simple or weighted sum
  else
    '--- variable to be summed
    call Token(%ident, "(", %var1)
    '--- weighted sum
    if %ident <> "" then
      '--- weight
      %var3 = @left(%ident, @len(%ident)-1)
      '--- variable x weight
      %var2 = %var1 + "*(" + %var3 + ")"
      '--- sum of weights (denominator)
      call BlocList(%var3, "+", %p_rhb, %var3)
    '--- simple sum
    else
      %var2 = %var1
      %var3 = ""        
    endif
    '--- expand the numerator
    call BlocList(%var2, "+", %p_rhb, %var2)
    '--- text of the identity
    %ident = @replace(%var1, "?", %p_lhb) + "=(" + %var2 + ")"
    if %var3 <> "" then
      %ident = %ident + "/(" + %var3 + ")"
    endif
  endif
  call ModelAppend(%p_model, %ident)
wend
endsub

subroutine local ArraySort(vector y, vector x, scalar n)
'==================================================
'Create a sorted index for a vector.
'
' Call: y  vector with values to sort
'       x  vector in which the index is returned
'       n  number of elements to sort
'
' Ret:  x  list of elements of y in ascending order
'
'---------------------------------------------------------------

'--- locate lowest values in turn
for !i1 = 1 to n-1
  !i = x(!i1)
  '--- assume the current value is the lowest
  !v = y(!i)
  !k1 = !i1
  '--- check remaining values
  for !j1 = !i1+1 to n
    !j = x(!j1)
    '--- note if lowest
    if y(!j) < !v then
      !v = y(!j)
      !k1 = !j1
    endif
  next
  '--- if the current value was not the lowest
  '    replace the current pointer by the pointer
  '    to the lowest value and move the current
  '    pointer to the empty slot formerly occupied
  '    by the pointer to the lowest value
  if !k1 > !i1 then
    !j = x(!k1)
    x(!k1) = x(!i1)
    x(!i1) = !j
  endif
next
endsub

subroutine AssignSeries(string %Var, string %Val, _
  string %First, string %Last)
'==============================================================
'assign values to a series
'
' Call: %Var    series name
'       %Val    expression
'       %First  from or null to use existing smpl
'       %Last   to or null
'
' Ret:  
'
'---------------------------------------------------------------
if %First <> "" then smpl %First %Last endif
if @isobject(%Var) then
  {%Var} = {%Val}
else
  series {%Var} = {%Val}
endif
endsub

subroutine BlocCreateSeries(string %p_Scope, string %p_tlSpec, _
  string %p_Alias)
'==================================================
'Creates alias series for lhs actuals
'
' Call: %p_Scope   B BW BG GW or BGW  (bloc, group and/or world)
'       %p_tlSpec  list of assignment statements separated by blank
'       %p_Alias   scenario alias with _ prefix or null for actuals
'
' Ret:
'
' Note: assignments are as defined for BlocEval
'
'---------------------------------------------------------------
!lib_is = 1
!lib_ie = 1
call zScope(%p_Scope, !lib_is, !lib_ie)
for !lib_i = !lib_is to !lib_ie
  call zBlocCreateSeries(t_Bloc(!lib_i, 1), %p_Alias, %p_tlSpec)
next
endsub

subroutine local zBlocCreateSeries(string %p_lhb, _
  string %p_alias, string %p_tlassign)
'==================================================
'Creates alias series from assignments for one geo
'
' Call: %p_lhb      lhs geo (bloc, group or world)
'       %p_alias    scenario alias prefixed by _
'       %p_tlassign list of assignities separated by blank
'
' Ret:
'
' Note: see BlocEval for methods of specifying an assignment
'
'---------------------------------------------------------------
%tl = %p_tlassign
'--- loop for multiple assigns
while 1=1
  call Token(%tl, " ", %assign)
  if %assign = "" then exitloop endif
  '--- check for formula
  !i = @instr(%assign, "=")
  if !i > 0 then
    %var = @replace(@left(%assign, !i-1), "?", %p_lhb)
  '--- otherwise simple or weighted sum
  else
    call Token(%assign, "(", %var)
    %var = @replace(%var, "?", %p_lhb)
  endif
  call CreateSeries(%var + %p_alias + "=" + %var)
wend
endsub

subroutine BlocEval(string %p_Scope, string %p_tlSpec, _
  string %p_Alias)
'==================================================
'Executes sets of series assignments
'
' Call: %p_Scope   B BW BG GW or BGW  (bloc, group and/or world)
'       %p_tlSpec  list of assignment statements separated by blank
'       %p_Alias   scenario alias with _ prefix or null for actuals
'
' Ret:
'
' Note: assignments are specified as follows
'     var         summation over blocs
'     var1(var2)  weighted summation over blocs
'     var=expr    formula
'   var and expr may contain ? to denote a geo or bloc code
'
' Example assignments:
'   H_?             H = sum(H) total over blocs
'   ph_?(H_?)       ph = sum(ph*H)/sum(H) total over blocs
'   pewsav=pe_w     world formula (no substitution)
'   rx_?=ph_?/ph_w  group or bloc formula with substitution
'
'---------------------------------------------------------------

'--- insert alias
call AliasExpr(%p_tlSpec, %p_Alias, %lib_tl)

'--- fix range of areas to process
!lib_is = 1
!lib_ie = 1
call zScope(%p_Scope, !lib_is, !lib_ie)
for !lib_i = !lib_is to !lib_ie
  call zBlocEval(t_Bloc(!lib_i, 1), t_Bloc(!lib_i, 3), %lib_tl)
next
endsub

subroutine local zBlocEval(string %p_lhb, string %p_rhb, _
  string %p_tlassign)
'==================================================
'Execute series assignments for one geo
'
' Call: %p_lhb     lhs geo + alias (bloc, group or world)
'       %p_rhb     list of members + alias separated by blank
'       %p_tlassign list of assigns separated by blank
'
' Ret:
'
' Note: see BlocEval for methods of specifying an assignment
'
'---------------------------------------------------------------
%tl = %p_tlassign
'--- loop for multiple assigns
while 1=1
  call Token(%tl, " ", %assign)
  if %assign = "" then exitloop endif
  '--- check for formula
  !i = @instr(%assign, "=")
  if !i > 0 then
    %lhs = @replace(@left(%assign, !i-1), "?", %p_lhb)
    %rhs = @replace(@mid(%assign, !i+1), "?", %p_lhb)
  '--- if not a formula, expand as a simple or weighted sum
  else
    '--- variable to be summed
    call Token(%assign, "(", %var1)
    '--- weighted sum
    if %assign <> "" then
      '--- weight
      %var3 = @left(%assign, @len(%assign)-1)
      '--- variable x weight
      %var2 = %var1 + "*(" + %var3 + ")"
      '--- sum of weights (denominator)
      %var3 = @replace(%var3, "?", %p_lhb)
    '--- simple sum
    else
      %var2 = %var1
      %var3 = ""        
    endif
    '--- expand the numerator
    call BlocList(%var2, "+", %p_rhb, %var2)
    '--- text of the assign
    %lhs = @replace(%var1, "?", %p_lhb)
    %rhs = "(" + %var2 + ")"
    if %var3 <> "" then
      %rhs = %rhs + "/(" + %var3 + ")"
    endif
  endif
  call AssignSeries(%lhs, %rhs, "", "")
wend
endsub

subroutine local BlocList(string %p_Expr, string %p_Sep, _
  string %p_tlBloc, string %p_Result)
'==================================================
'Create a list of expressions with ? replaced by geo codes
'
' Call: %p_Expr    expression with ? placeholders
'       %p_Sep     separator (eg blank, comma or +)
'       %p_tlBloc  list of geo codes separated by blank (space)
'
' Ret:  %p_Result  concatenated list
'
'---------------------------------------------------------------
%tl = %p_tlBloc
%t = ""
while 1=1
  call Token(%tl, " ", %b)
  if %b = "" then exitloop endif
  if %t <> "" then %t = %t + %p_Sep endif
  %t = %t + @replace(%p_Expr, "?", %b)
wend
%p_Result = %t
endsub

subroutine BlocMem(string %p_Group, string %p_tlBloc)
'==================================================
'List blocs that are members of a group
'
' Call: %p_Group   group name
'
' Ret:  %p_tlBloc  member blocs or blank
'
'---------------------------------------------------------------
for !lib_i = 1 to nArea
  if t_Bloc(!lib_i, 1) = %p_Group then
    %p_tlBloc = t_Bloc(!lib_i, 3)
    return
  endif  
next
%p_tlBloc = ""
endsub

subroutine BlocMinVal(string %p_Scope, string %p_tlVar)
'==================================================
'Set minimum values for a series
'
' Call: %p_Scope   B BW BG GW or BGW  (bloc, group and/or world)
'       %p_tlVar   list of var minval pairs separated by comma
'
' Ret:
'
'---------------------------------------------------------------
!lib_is = 1
!lib_ie = 1
call zScope(%p_Scope, !lib_is, !lib_ie)
for !lib_i = !lib_is to !lib_ie
  call zBlocMinVal(t_Bloc(!lib_i, 1), %p_tlVar)
next
endsub

subroutine local zBlocMinVal(string %p_geo, string %p_tlvar)
'==================================================
'Set minimum values for a list of geo variables
'
' Call: %p_geo     geo (bloc, group or world)
'       %p_tlvar   list of var minval pairs separated by comma
'
' Ret:
'
'---------------------------------------------------------------
%tl = %p_tlvar
'--- loop for multiple assigns
while 1=1
  call Token(%tl, ",", %val)
  if %val = "" then exitloop endif
  call Token(%val, " ", %var)
  %var = %var + "_" + %p_geo
  %minval = "@recode(" + %var + "<" + %val + "," _
    + %val + "," + %var + ")"
  call AssignSeries(%var, %minval, "", "")
wend
endsub

subroutine BlocName(string %Area, string %Name)
'==================================================
'Look up the name of a bloc, bloc group or world as whole
'
' Call: %Area  bloc, group or world code
'
' Ret:  %Name  name of the area
'
' Uses: t_Bloc  table of area definitions
'       nArea   number of areas
'
' Note: an area may be a bloc, group of blocs or the world
'
'---------------------------------------------------------------
%Name = @upper(%Area)          ' default if not found
for !lib_i = 1 to nArea
  if @lower(t_Bloc(!lib_i, 1)) = @lower(%Area) then
    %Name = t_Bloc(!lib_i, 2)
    exitloop
  endif      
next
endsub

subroutine CopyAliasValues(string %Source, string %Dest, _
  string %First, string %Last)
'==================================================
'copy values for all series for one alias to another
'
' Call: %Source  source alias (must not be blank)
'                with preceding _
'       %Dest    destination alias with preceding _
'                or blank for actuals
'       %First   first observation to copy
'       %Last   last observation to copy
'
' Ret:  x  list of elements of y in ascending order
'
' Note: does not copy aliased _ins series
'
'---------------------------------------------------------------
if %Source = "" or %First > %Last then return endif
if %Dest = "" then
  %lib_s = "actuals"
else
  %lib_s = %Dest
endif
call pLog("copying " + %lib_s + " from " + %First _
  + " - " + %Last + " from " + %Source)
smpl %First %Last
!lib_n2 = @strlen(%Source)
group lib_g *{%Source}
!lib_n = lib_g.@count
for !lib_i = 1 to !lib_n
  %lib_s = @upper(lib_g.@seriesname(!lib_i))
  if @instr(%lib_s, "_INS") = 0 then
    !lib_n1 = @len(%lib_s)
    %lib_s1 = @left(%lib_s, !lib_n1 - !lib_n2) + %Dest
    '--- if the target already exists update values for the specified years
    if @isobject(%lib_s1) then
      {%lib_s1} = {%lib_s}
    '--- otherwise create a new series with values for the specified years
    else
      series {%lib_s1} = {%lib_s}
    endif
  endif
next
delete lib_g
endsub

subroutine local CountTokens(string %p_t, string %p_a, _
  scalar p_n)
'==================================================
'Count the number of tokens in a string
'
' Call: %p_t  source string
'       %p_a  separator character
'
' Ret:  p_n   number of tokens in the string
'
'---------------------------------------------------------------
p_n = 0
%tl = @trim(%p_t)
while %tl <> "" 
  call Token(%tl, %p_a, %r)
  p_n = p_n + 1
wend
endsub

subroutine CreateModelFile(string %p_Source, string %p_File)
'==================================================
'create workfile with model and data from source
'
' Call: %p_Source  workfile with source data
'       %p_File    name of new workfile
'
' Ret:  
'
' Note: EViews 6 problem
' i) the default for deterministic simulation is to carry forward AR
' residuals from the last actual value. There is a bug in
' the implementation as it picks up any add factor value and bundles
' this with the residual carried forward. The unintended outcome is
' that model solutions starting at different years generate different
' results even if all conditions are identical.
' ii) this bug can be avoided by specifying the option Structural
' (ignore ARMA) in a check box on the Solve dialog. When this option
' is selected prior residuals or add factors are not carried forward
' into the solution period.
' iii) we have to create a model manually to specify the Structural
' option as no command is available to set the option programatically.
' iv) if the model object is copied it loses the property. Therefore
' it is necessary to use the manually-created model and the workfile
' that contains it every time, copying data from GPM source workfiles
' into the new workfile.
'
'---------------------------------------------------------------
open {%p_Source}
shell copy MDS.wf1 {%p_File}.wf1
open {%p_File}.wf1
copy {%p_Source}::data\*
close {%p_Source}
delete sp_log*
t_Settings(7,2) = %p_File
endsub

subroutine CreateSeries(string %p_Var)
'==============================================================
'create a series
'
' Call: %p_Var    series name or assignment expression
'
' Ret:
'
'---------------------------------------------------------------
series {%p_Var}
endsub

subroutine DeleteObjects(string %p_List)
'==============================================================
'delete listed objects
'
' Call: %p_List    list of objects (may include ? and *)
'
' Ret:
'
'---------------------------------------------------------------
delete {%p_List}
endsub

subroutine DerivedSeries(string %p_First, _
  string %p_Last, string %p_Alias)
'==================================================
'create or extend series derived from model variables
'
' Call: %p_First    first year of new data
'       %p_Last     last year of new data
'       %p_Alias    blank or Scenario alias with _
'
' Ret: 
'
' Note: creates Gini and variables listed in TVar and t_Result
'       with the given alias. If %Alias is not blank copies
'       values before %First from actual values
'
'---------------------------------------------------------------
call pLog("calculating derived series")
%lib_start = t_Settings(9,2)
call zDerivedSeries(%lib_start, %p_First, %p_Last, %p_Alias, _
  t_TVar, nTVar, t_Result, nResult)
endsub

subroutine local zDerivedSeries(string %p_Start, _
  string %p_First, string %p_Last, string %p_Alias, _
  table p_tT, scalar p_nT, table p_tR, scalar p_nR)
'==================================================
'create or extend series derived from model variables
'
' Call: %p_Start     first year of workfile
'       %p_First     first year of new data
'       %p_Last      last year of new data
'       %p_Alias     blank or Scenario alias with _
'       p_tT         table of Theil specifications
'       p_nT         no of specifications
'       p_tR         table of result specifications
'       p_nR         no of result specifications
'
' Ret:
'
'---------------------------------------------------------------

'--- copy historical data for Scenario variant from actuals
if %p_Alias <> "" and %p_First > %p_Start then
  smpl %p_Start %p_First-1
  '--- result specifications
  for !j = 1 to p_nR
    call BlocCreateSeries(p_tR(!j, 1), p_tR(!j, 2), %p_Alias)
  next
  '--- Gini coefficient for income
  call CreateSeries("GY" + %p_Alias + " = GY")
  call CreateSeries("rylow_w" + %p_Alias + " = rylow_w")
  '--- Theil measures
  for !j = 1 to p_nT
    %v = p_tT(!j, 1)
    call CreateSeries("TH_" + %v + %p_Alias + " = TH_" + %v) 
  next
endif

'--- Gini coefficient for income
call Gini("Y", "N", "GY", "rylow_w", %p_Alias, _
          %p_First, %p_Last)

'--- generate new values for the Scenario period
smpl %p_First %p_Last

'--- result specifications
for !j = 1 to p_nR
  call BlocEval(p_tR(!j, 1), p_tR(!j, 2), %p_Alias)
next

'--- Theil measures
for !j = 1 to p_nT
  %vb = p_tT(!j, 1)
  call Theil(%vb, %vb + "_W", %s)
  call Token(%s, "=", %s1)
  call ScenarioExpr(%s, %p_Alias, %s1) 
  call AssignSeries("TH_" + %vb + %p_Alias, %s1, _
    %p_First, %p_Last)
next

endsub

subroutine Eval(string %p_Expr, string %p_Year, scalar p_v)
'==============================================================
'return the value of an expression in a given year
'
' Call: %p_Expr    the expression
'       %p_Year    the year for which it is to be evaluated
'
' Ret:  p_v        the value
'
'---------------------------------------------------------------
smpl %p_Year %p_Year
series lib_evs = {%p_Expr}
p_v = @elem(lib_evs, %p_Year)
delete lib_evs
endsub

subroutine EvalP(pool p_p, string %p_Expr, string %p_First, _
  string %p_Last, matrix p_mat)
'==============================================================
'evaluate a pool expression
'
' Call: p_p         a pool
'       %p_Expr     the expression
'       %p_First    start year
'       %p_Last     end year
'       p_mat       matrix to store results
'
' Ret:  p_mat       updated matrix
'
'---------------------------------------------------------------
smpl %p_First %p_Last
p_p.genr lib_evs_? = {%p_Expr}
p_p.makegroup(lib_evgr) lib_evs_?
stomna(lib_evgr,p_mat)
endsub

subroutine EvalS(string %p_Expr, string %p_First, _
  string %p_Last, series p_ser)
'==============================================================
'evaluate an expression and return a series
'
' Call: %p_Expr     the expression
'       %p_First    start year
'       %p_Last     end year
'       p_ser       series to store results
'
' Ret:  p_ser       updated series
'
'---------------------------------------------------------------
smpl %p_First %p_Last
p_ser = {%p_Expr}
endsub

subroutine Exists(string %p_Obj, scalar p_q)
'==============================================================
'return 1 if an object exists, 0 otherwise
'
' Call: %p_Obj     object name
'
' Ret:  p_q        1 exists
'                  0 does not exist
'
'---------------------------------------------------------------
p_q = 0
if @isobject(%p_Obj) then p_q = 1 endif
endsub

subroutine FillSeries(string %Var, string %Val, _
  string %First, string %Last)
'==============================================================
'fill a series with a list of values
'
' Call: %Var    series name
'       %Val    list of values
'       %First  from
'       %Last   to
'
' Ret:  
'
'---------------------------------------------------------------
smpl %First %Last
{%Var}.fill(s) {%Val}
endsub

subroutine FindEq(string %Var, scalar iEq)
'==================================================
'return the number of the behavioural equation determining a var
'
' Call: %Var  name of the variable (with or without _ suffix)
'
' Ret:  iEq   equation number or zero if not found
'
'---------------------------------------------------------------
%lib_lib_s = %Var
call Token(%lib_lib_s, "_", %lib_lib_v)
for !lib_lib_i = 1 to nEq
  if @upper(t_Eq(!lib_lib_i,2)) = @upper(%lib_lib_v) then
    iEq = !lib_lib_i
    return
  endif
next
iEq = 0
endsub

subroutine Gini(string %Source, string %Pop, string %Gini, _
  string %RLow, string %Alias, string %Start, string %End)
'==================================================
'Compute series of Gini index values
'
' Call: %Source  name of series for which Gini will be calculated
'       %Pop     name of population series
'       %Gini    name of series to store the result
'       %RLow    name of series to store the low ratio
'       %Alias   suffix for series names with _
'       %Start   start year
'       %End     end year
'
' Ret:  Gini index values in the result series
'
' Uses: t_Bloc  list of bloc codes
'       nBloc   number of blocs
'
' Note: the "low ratio" is the ratio of the per capita average
' in the bottom quartile to the ratio of the per capita average
' for the whole population.
'
'---------------------------------------------------------------

series {%Gini}{%Alias}
series {%RLow}{%Alias}

vector(nBloc) lib_vy
vector(nBloc) lib_vn
vector(nBloc) lib_vyn
vector(nBloc) lib_x

'--- initialize the index vector
for !lib_i = 1 to nBloc
  lib_x(!lib_i) = !lib_i
next

!lib_iStart = @val(%Start)
!lib_iEnd = @val(%End)

'--- loop over years
for !lib_ky = !lib_iStart to !lib_iEnd
  %lib_year = @str(!lib_ky)
  !lib_sy = 0
  !lib_sn = 0
  for !lib_i = 1 to nBloc
    %b = t_Bloc(!lib_i, 1)
    '--- resource
    lib_vy(!lib_i) = @elem({%Source}_{%b}{%Alias}, %lib_year)
    '--- population
    lib_vn(!lib_i) = @elem({%Pop}_{%b}{%Alias}, %lib_year)
    '--- per capita resource
    lib_vyn(!lib_i) = lib_vy(!lib_i) / lib_vn(!lib_i)
    '--- world resource
    !lib_sy = !lib_sy + lib_vy(!lib_i)
    '--- world population
    !lib_sn = !lib_sn + lib_vn(!lib_i)
  next
  '--- sort by per capita value
  Call ArraySort(lib_vyn, lib_x, nBloc)

  '--- start from the bloc with lowest per capita value
  !lib_k = lib_x(1)
  '--- area = resource x population
  !lib_v = lib_vn(!lib_k) * lib_vy(!lib_k)
  '--- resource
  !lib_v1 = lib_vy(!lib_k)
  '--- population
  !lib_n1 = lib_vn(!lib_k)
  !lib_vlow = 0
  for !lib_i = 2 to nBloc
    !lib_k = lib_x(!lib_i)
    '--- cumulate area
    !lib_v = !lib_v + lib_vn(!lib_k) * _
             (2*!lib_v1 + lib_vy(!lib_k))
    '--- cumulate the resource
    !lib_v1 = !lib_v1 + lib_vy(!lib_k)
    '--- cumulate population
    !lib_n1 = !lib_n1 + lib_vn(!lib_k)
    '--- set the low ratio if cumulated popn reaches 25%
    if !lib_vlow = 0 then
      if !lib_n1 >= 0.25*!lib_sn then
        '--- excess population
        !lib_n1 = !lib_n1 - 0.25*!lib_sn
        '--- adjusted cumulative resource
        !lib_vlow = !lib_v1 - lib_vy(!lib_k)*!lib_n1 _
                                 /lib_vn(!lib_k)     
        '--- low ratio
        !lib_vlow = 400*!lib_vlow/!lib_sy
      endif
    endif    
  next

  '--- the Gini index is 100*(1 - computed area)
  '    normalized by dividing by total value and total population
  !lib_v = 100*(1 - !lib_v / (!lib_sy * !lib_sn))
  call FillSeries(%Gini+%Alias,@str(!lib_v),%lib_year,%lib_year)
  call FillSeries(%RLow+%Alias,@str(!lib_vlow),%lib_year,%lib_year)
next

delete lib_*
endsub

subroutine local ListCol(table p_t, scalar p_srow, scalar p_nrow, _
  scalar p_iCol, string %p_Sep, string %p_Result)
'==============================================================
'List values from a table column
'
' Call: p_t        table
'       p_srow     starting row
'       p_nrow     number of rows
'       p_iCol     column to return
'       %p_Sep     separator
'
' Ret:  %p_Result  string with column values delimited by %p_Sep
'
'---------------------------------------------------------------
%t = ""
for !i = p_srow to p_nrow
  if %t <> "" then %t = %t + %p_Sep endif
  %t = %t + p_t(!i, p_iCol) 
next
%p_Result = %t
endsub

subroutine local LoadTable(table t, scalar nrow, string %tlrows)
'==============================================================
'Load rows into an EViews table
'
' Call: t        table to load
'       nrow     number of rows
'       %tlrows  string with rows of data
'                 delimited by ; (field) and : (row)
'
' Ret:  nrow     no of rows after loading additional data
'
'---------------------------------------------------------------
%tl = %tlrows
call Token(%tl, ":", %row)
while %row <> ""
  nrow = nrow + 1
  !icol = 0
  call Token(%row, ";", %cell)
  while %cell <> ""
    !icol = !icol + 1
    t(nrow, !icol) = %cell
    call Token(%row, ";", %cell)
  wend
  call Token(%tl, ":", %row)
wend
endsub

subroutine ModelAppend(string %p_Model, string %p_Ident)
'==============================================================
'Append an identity to a model
'
' Call: %p_Model   name of the model
'       %p_Ident   text of the identity
'
' Ret:
'
'---------------------------------------------------------------
{%p_Model}.append @identity {%p_Ident}
endsub

subroutine local NFormat(scalar Val, string %Res)
'==============================================================
'format a number
'
' Call: Val    number
'
' Ret:  %Res   number formatted as a string
'
' Note:
' if the number is small (abs value < 1) it is shown with 2 dp's
' if it is large but not very large it is shown with 0 dp's
' otherwise it is formatted with 6 significant figs
'
'---------------------------------------------------------------
'--- show small numbers with 2 dp
!v = @abs(Val)
if !v < 1 then
  %Res = @str(Val, "f.5")
else if !v <1e10 and !v >= 1e6 then    
  %Res = @str(Val, "ft.0")
else
  %Res = @str(Val, "gt.6")
endif
endif
endsub

subroutine OpenSpool(string %Name)
'==============================================================
'create a spool if it doesn't exist and route output to the spool
'
' Call: %Name  name for the spool
'
' Ret:
'
'---------------------------------------------------------------
if not @isobject(%Name) then
  spool {%Name}
  {%Name}.leftmargin 0.1
  {%Name}.vertspacing 0.1
  {%Name}.graphmode(type=variablelimit) 6
  {%Name}.tablemode(type=variablelimit) 6
  {%Name}.options -tree
endif
output(s) {%Name}
show {%Name}
endsub

subroutine pAbort(string %tMsg)
'==============================================================
'abort the program
'
' Call: %tMsg  message text
'
' Ret:
'
'---------------------------------------------------------------
call pLog(%tMsg)
call pLog("end")
toc
stop
endsub

subroutine local Parse(string %tExpr, string %Next)
'==============================================================
'parse an expression for the next token
'
' Call: %tExpr  the expression
'
' Ret:  %Next   next token
'       %tExpr  remainder of the expression
'
'---------------------------------------------------------------
%t = %tExpr
%s = ""
!n = @len(%t)
if !n > 0 then
  for !i = 1 to !n
    %a = @mid(%t,!i, 1)
    if @instr(" +-*/()^<>=,:;", %a) > 0 then
      if !i = 1 then
        %s = %a
      endif
      exitloop
    endif
    %s = %s + %a
  next
endif
%Next = %s
%tExpr = @mid(%t,@len(%s) + 1)
endsub

subroutine pEnd()
'==============================================================
'halt program
'
' Call: 
'
' Ret:
'
'---------------------------------------------------------------
call pLog("end")
%wkf = t_Settings(7,2)
wfsave {%wkf}
toc
endsub

subroutine pLog(string %tMsg)
'==============================================================
'log program event
'
' Call: %tMsg  message text
'
' Ret:
'
'---------------------------------------------------------------
if not @isobject("sp_log") then
  tic
  call OpenSpool("sp_log")
  table(1,2) t_log
  t_log.setwidth(1) 20
  t_log.setwidth(2) 60
  setcell(t_log,1,1,@strnow("YYYY-MM-DD HH:MI:SS"),"l")
  setcell(t_log,1,2,%sysTitle + " (" + %model + ")","l")
  print t_log
endif
setcell(t_log,1,1,@strnow("YYYY-MM-DD HH:MI:SS"),"l")
setcell(t_log,1,2,%tMsg,"l")
print t_log
endsub

subroutine local PoolId(pool p, string %Mem, scalar iMem)
'==============================================================
'returns the pool member id for a given suffix
'
' Call: p      the pool
'       %Mem   suffix
'
' Ret:  iMem   pool member id
'
'---------------------------------------------------------------
for !i = 1 to p.@ncrossest
  %v = p.@idnameest(!i)
  if @upper(%v) = @upper(%Mem) then
    iMem = !i
    return
  endif
next
iMem = 0
endsub

subroutine ReadTable(string %Name, scalar iRow, _
  scalar iCol, string %Cell)
'==============================================================
'return the contents of a cell of a table
'
' Call: %Name   the table name
'       iRow    the row
'       iCol    the col
'
' Ret:  %Cell   contents of the given cell
'
'---------------------------------------------------------------
%Cell = {%Name}(iRow, iCol)
endsub

subroutine ROWGen(string %Var, string %Bloc, _
  string %tlSc, string %RVar)
'===========================================================
'Create rest of world equivalents to a bloc variable
'
' Call: %Var  name of bloc variable without _%b  suffix
'       %Bloc bloc for which ROW value is to be calculated
'       %tlSc list of Scenario chars without _ prefixes
'
' Ret:  %RVar  name of created variable with bloc suffix
'
'---------------------------------------------------------------

'--- name of the ROW variable
%RVar = "ROW" + %var + "_" + %Bloc

'--- name of the related world total
%lib_w = %Var + "_W"

'--- generate the ROW variable for listed Scenarios
%lib_tl = %tlSc
for !lib_i = 1 to @len(%tlSc)
  %lib_a = @mid(%tlSc,!lib_i,1)
  series {%RVar}_{%lib_a} = _
    {%lib_w}_{%lib_a} - {%Var}_{%Bloc}_{%lib_a}
next  

endsub

subroutine local RToken(string %t, string %a, string %r)
'===========================================================
' return the last item from a list and truncate the list
'
' Call: %t  source string
'       %a  separator character for list items
'
' Ret:  %t  string remainder
'       %r  token
'
'---------------------------------------------------------------
%t = @trim(%t)
!i = @instr(%t, %a)
if !i = 0 then
  %r = %t
  %t = ""
else
  !n = 1
  while !i > 0
    !j = !i
    !n = !n + 1
    !i = @instr(%t, %a, !n)
  wend
  %r = @trim(@mid(%t, !j+1))
  %t = @left(%t, !j-1)
endif
%r = @trim(%r)
endsub

subroutine local ScenarioExpr(string %tExpr, string %Alias, _
  string %tRes)
'==============================================================
'alias an expression for a Scenario
'
' Call: %tExpr  the expression
'       %Alias  scenario alias with _ if relevant
'
' Ret:  %tRes   aliased expression
'
' Warning: all variable names in the expression are aliased
' 
' Note: alias is added to the end of all tokens that contain _
'
'---------------------------------------------------------------
%ts = @trim(%tExpr)
if %Alias = "" then
  %tRes = %ts
  return
endif

'--- loop to check elements of the expression
%t = ""
!q = 0
while %ts <> ""
  call Parse(%ts, %g)
  '-- series names have more than 1 char, don't begin
  '   with @ and include an underscore
  if @len(%g) > 1 and @left(%g,1) <> "@" _
  and @instr(@mid(%g,2), "_") > 0 then
    %g1 = %g + %Alias
    call Exists(%g1, !q)
    if !q then %g = %g1 endif
  endif
  %t = %t + %g
wend
%tRes = %t
endsub

subroutine SolveModel(model m, string %tlOption, _
  string %Actual, string %Predict)
'==================================================
'solve the model and compute post-solution analysis
'
' Call: m          model to solve
'       %tlOption  EViews solution options
'       %Actual    last actual year
'       %Predict   last solution year
'
' Ret:
'
' Note: solves with the alias defined in t_Settings
'
'---------------------------------------------------------------

statusline "solving the model"
%lib_first = @str(@val(%Actual)+1)

'--- solve for future years
smpl %lib_first %Predict
mode verbose
m.solve({%tlOption})
mode quiet
t_Settings(2,2) = %Predict

statusline "post-solution analysis"

%lib_alias = t_Settings(3,2)
if %lib_alias <> "" then
  %lib_alias = "_" + %lib_alias
endif
call DerivedSeries(%lib_first, %Predict, %lib_alias)
endsub

subroutine SortTable(string %Page, table tSource, _
  scalar !nRow, scalar !nCol, string %Dir, _
  string %OrderBy, string %tResult)
'===============================================================
'Create a sorted table.
'
' Call: %Page     name of source page
'       tSource   source table
'       !nrow     no of rows
'       !ncol     no of columns
'       %Dir      direction - a ascending or d descending
'       %OrderBy  list of columns on which to sort
'       %tResult  name of result table
'
' Ret:  sorted table in the source workfile.
'
' Note: the first row of tSource must contain column names that
' can be used as series names (no embedded blanks). Other rows
' must contain numeric values only in data columns. The table
' from row 2 onwards is sorted in ascending or descending order
' by values of columns listed in the OrderBy argument.
'
' The routine creates a temporary workfile to do the sort and
' copies the result back as a new table in the source workfile.
'
'---------------------------------------------------------------


'--- create a temp page with one observation for each
'    row of data in the source table
!lib_n = !nRow - 1
pagecreate(page=lib_tmp) u 1 !lib_n

'--- copy the source table to the temp page
copy {%Page}\tSource t

'--- create one series for each column of the table
for !lib_j = 1 to !nCol
  %s = t(1, !lib_j)
  series {%s}
  for !lib_i = 1 to !lib_n
    {%s}(!lib_i) = t(!lib_i + 1, !lib_j)
  next
next

'--- sort the page by values in the named columns
sort({%Dir}) {%OrderBy}

'--- copy sorted data back into columns of the table
for !lib_j = 1 to !nCol
  %s = t(1, !lib_j)
  for !lib_i = 1 to !lib_n
  t(!lib_i+1, !lib_j) = {%s}(!lib_i)
  next
next

'--- copy the sorted table back to the source page
pageselect {%Page}
copy lib_tmp\{%tResult}

pagedelete lib_tmp
endsub

subroutine Theil(string %vb, string %vw, string %s)
'==================================================
'Return formula for Theil inequality
'
' Call: %vb   bloc variable
'       %vw   corresponding world variable
'
' Ret:  %s    Theil formula
'
' Uses: t_Bloc   bloc codes and names
'       nBloc    number of blocs
'
'--- the Theil inequality formula is
'
'   T = 100*(1 - exp( -sum( s(i) ln(x(i)/x) ) ) )
'
' where s(i) is the share of each bloc in the world total for the
' variable of interest, x(i) is the per capita figure for the variable
' of interest in each bloc, and x is the world average (per capita
' figure for the world as a whole).
'
' The minimum value is 0 when the per capita figure is exactly the
' same in each bloc. The maximum value is 100.
'---------------------------------------------------------------
%s = "TH_" + %vb + " = @nan(100*(1-exp(-("
for !lib_i = 1 to nBloc
  %lib_b = t_Bloc(!lib_i, 1)
  %lib_v = %vb + "_" + %lib_b
'--- adjust in case the bloc value is zero or negative
  %lib_v = "@iif(" + %lib_v + ">0.0001," + %lib_v + ",.0001)"
  %s = %s + "+" + %lib_v + "*log(" + %lib_v _
       + "*N_W/(" + %vw + "*N_" + %lib_b + "))"
next
%s = %s + ")/" + %vw + ")),0)"
endsub

subroutine local Token(string %t, string %a, string %r)
'=================================================='
'Return the first item from a list and truncate the list
'
' Call: %t  source string
'       %a  separator character for list items
'
' Ret:  %t  string remainder
'       %r  token
'
'---------------------------------------------------------------
!i = @instr(%t, %a)
if !i > 0 then
  %r = @left(%t, !i-1)
  %t = @trim(@mid(%t, !i+1))
else
  %r = %t
  %t = ""
endif
%r = @trim(%r)
endsub

subroutine local Unadjusted(string %Var, string %Name)
'==================================================
'return name of unadjusted variable =
'
' Call: %Var  root name of variable
'
' Ret:  %Name name of unadjusted variable with inserted U or u
'
'---------------------------------------------------------------
%a = "u"
%v = %Var
if @upper(%v) = %v then %a = "U" endif
call Token(%v, "_", %v1)
%Name = %v1 + %a
if %v <> "" then %Name = %Name + "_" + %v endif
endsub

subroutine local VarParts(string %Var, string %Root, _
  string %Mem)
'==============================================================
'return root and suffix parts of a variable name
'
' Call: %Var   variable name
'
' Ret:  %Root  root name
'       %Mem   suffix
'
'---------------------------------------------------------------
%Mem = %Var
call Token(%Mem, "_", %Root)
endsub

subroutine WriteCell(string %p_Tab, scalar p_iRow, _
  scalar p_iCol, string %p_Text, string %p_Align)
'==============================================================
'write a table cell
'
' Call: %p_Tab    table name
'       p_irow    row to write
'       p_icol    column to write
'       %p_Text   cell text
'       %p_Align  alignment - l, c or r
'
' Ret:  
'
'---------------------------------------------------------------
setcell({%p_Tab}, p_iRow, p_iCol, %p_Text, %p_Align)
endsub

subroutine local XFormat(scalar p_Val, scalar p_DP, _
  string %p_Res)
'==============================================================
'format a number
'
' Call: p_Val    number
'       p_DP     max decimal places
'
' Ret:  %p_Res   number formatted as a string
'
' Note: the number of decimal places reduces as the number
'       gets larger
'
'---------------------------------------------------------------
!v = @abs(p_Val)
!idp = 0
!vmin = 10^p_DP
while 1
  %fmt = "f." + @str(!idp)
  %s = @str(!v, %fmt)
  if @val(%s) >= !vmin or !idp = p_DP then
    %p_Res = @str(p_Val, %fmt)
    exitloop
  endif
  !idp = !idp + 1
  !vmin = !vmin / 10
wend
endsub
