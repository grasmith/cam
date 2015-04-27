'PROGRAM: zlibr.prg  Copyright (C) 2012,2015 Alphametrics Co. Ltd.
'
' CAM Version 6.0
'
' library routines for scenario rules
'
' Note: routines whose names begin with z are internal. Names
' beginning with lib_ are reserved and should not be used
' elsewhere.
'
' updated: FC 23/03/2015
'
'---------------------------------------------------------------
'
' AddAssign(m, %Opt, %Var)
' AddRulesToModel(tRule, nRule, m, %Alias, %Actual, %Predict)
' CalcProb(%Var, vShock, vPr)
' CalcShock(%Var, %Pr, vShock)
' Ceiling(%Ins, %Expr, %Value, vSens, vPCT)
' CheckTransform(tEq, nEq, %Var, %Trsf)
' ClearShockGroup(tRule, nRule, iRule, %Year)
' DropRules(%tlIns)
' ExtendAdd(vDecay)
' ExtendIns(vDecay)
' Floor(%Ins, %Expr, %Value, vSens, vPCT)
' Fix(%Ins, %Mode, %Value)
' InsToAddFactors(m, tRule, nRule, tBloc, nBloc)
' Limit(vPr, %tlVar)
' Link(%Var, %ToVar, %Value)
' ListAPR(tRep, %Title, %First, %Last, Max)
' ListGroup(g, tRep, %Title, %First, %Last, Max)
' ListExog(tRep, %Title, m, %First, %Last, Max)
' ModifyIns(%Var, %tlVal)
' Override(m, %Var)
' pRuleCheck(m, tRule, nRule, %Actual)
' ResetIns(%Key)
' RuleHeader(tRule, nRule, iRule, tRes, nRes)
' RuleTable(%Tab, %Alias, %First, %Last, Max)
' ScenarioTable(tRule, nRule, tRes, nRes, %Alias, %First, %Last)
' SetShockGroup(tRule, nRule, iRule, %Year, %Pr)
' Shock(%Var, %Mode, %Value)
' ShockTable(tRule, nRule, iRule, tRes, nRes, %Year, %Pr)
' Target(%Ins, %Expr, %Value, vSens, vPCT)
' Try(%Ins, %Expr, %Value, vSens, vPCT)
' YearList(%First, %Last, Max, vyr, nyr)
'==============================================================

subroutine AddAssign(model m, string %Opt, string %Var)
'==============================================================
'assign an add factor
'
' Call: m      model
'       %Opt   options
'       %Var   dependent variable
'
' Ret:
'
'---------------------------------------------------------------
m.AddAssign(c,{%Opt}) {%Var}
endsub

subroutine local AddRulesToModel(table tRule, scalar nRule, _
  model m, string %Alias, string %Actual, string %Predict)
'==============================================================
'incorporate stored rules in a model
'
' Call: tRule    rules table
'       nRule    no of rules
'       m        the model
'       %Alias   scenario alias with _ if relevant
'       %Actual  last actual year
'       %Predict last solution year
'
' Ret:
'
'---------------------------------------------------------------

%first = @str(@val(%Actual)+1)

for !irule = 1 to nRule
  %var = tRule(!irule,1)
  %gtyp = tRule(!irule,2)
  %obj = tRule(!irule,3)
  %val = tRule(!irule,4)
  if @instr("CeilingFloorTargetTry", %gtyp) > 0 then
    %grad = @str(1/@val(tRule(!irule,5)))
  endif  
  %pct = @str(0.01*@val(tRule(!irule,6)))
  %pr = tRule(!irule,7)
  '--- adjust the probability limit for a two-sided range
  !v = @val(%pr)
  if !v > 0 and !v < 100 then
    %pr = @str((100 + !v)/2)
  endif

  '--- if the target is a list of values load them into
  '    an exogenous variable named after the instrument
  '    with a _val suffix
  '    otherwise the target is a single number or variable name
  if @instr(%val, " ") > 0 then
    '--- initialise values to be 0
    %v = %var + "_val"
    call CreateSeries(%v)
    '--- if the value list contains *
    '    divide the list at the *
    '    count the tokens in both parts
    '    add the rh portion of the list until there are enough
    '    values to cover the solution period
    !nyr = @val(%Predict)-@val(%Actual)
    if @instr(%val, "*") > 0 then
      %v1 = %val
      !n = 0
      !n1 = 0
      call Token(%v1, "*", %val)
      call CountTokens(%v1, " ", !n1)
      call CountTokens(%val, " ", !n)
      if %val = "" then
        !n = !n1
        %val = %v1
      endif
      while !n < !nyr
        %val = %val + " " + %v1
        !n = !n + !n1
      wend
    endif
    '--- build the value list for the sample
    call Token(%val, " ", %v1)
    for !i = 2 to !nyr
      call Token(%val, " ", %s)
      if %s = "" then exitloop endif
      %v1 = %v1 + "," + %s
    next
    %v = %var + "_val"
    call FillSeries(%v, %v1, %first, %Predict)
    '--- change the value token to be a reference to the _val variable
    %val = %v
  endif
  
  '--- shocks
  if %gtyp = "Shock" then
    '--- additive shock
    if %obj = "A" then
      '--- create an additive add factor
      '    and set its values
      call AddAssign(m, "v", %var)
      %v = %var + "_a"
      call AssignSeries(%v, %val, %first, %Predict)
    '--- proportionate shock
    ' check the transform in the equation for the variable
    ' adjust the model accordingly
    else
      call CheckTransform(%var, %trsf)
      %v = %var + "_ins"
      if %trsf = "none" then
        m.append @identity {%v} = {%val}*{%var}
      else
        if %trsf = "log" then
          call AssignSeries(%v, %val, %first, %Predict)
        '--- ratio
        else
          m.append @identity {%v} = {%val}*{%var}/{%trsf}
        endif
      endif
    endif
  endif

  '--- fixed values
  if %gtyp = "Fix" then

    '--- exogenous variable
    if %obj = "E" then
      call Override(m, %var)
      %v = %var + %Alias
      call CreateSeries(%v)
      call AssignSeries(%v, %val, %first, %Predict)
    else
      '--- determine the gradient
      call CheckTransform(%var, %trsf)
      if %trsf = "none" then
        %grad = "1"
      else
        if %trsf = "log" then
          %grad = "(1/" + %var + ")"
        else
          %grad = "(1/" + %trsf + ")"
        endif
      endif
      '--- level
      if %obj = "L" then
        %s = %var + "_sav+0.1*" + %grad + _
          "*(" + %val + "-" + %var + ")"
      '--- growth rate
      '    multiply the gradient by 0.01*v(-1)
      else
        %grad = "(" + %grad + "*0.01*" + %var + "(-1))"
        %s = %var + "_sav+0.1*" + %grad + _
          "*(" + %val + "-@pc(" + %var + "))"
      endif
      %v = %var + "_sav"
      call CreateSeries(%v)
      m.append @identity {%v} = {%var}_ins
    endif
  endif

  '--- link
  if %gtyp = "Link" then
    !v = 1
    !v1 = 1
    call CalcShock(%obj, "95", !v)
    call CalcShock(%var, "95", !v1)
    if !v <> na and !v1<> na then
      %val = @str(@val(%val)*!v1/!v)
      %s = %val + "*" + %obj + "_ins"
    else
      %s = %val + "*" + %obj
    endif
  endif

  '--- lagged adjustment
  if %gtyp = "Try" then
    '--- new instrument value
    %s = "(" + %obj + "-d(" + %obj + "))"
    %s = %pct+ "*(" + %grad + ")*(" + %val + "-" + %s + ")"
  endif

  '--- target-instrument or constraint
  if @instr("CeilingFloorTarget", %gtyp) > 0 then
    %v = %var + "_sav"
    call CreateSeries(%v)
    m.append @identity {%v} = {%var}_ins
    '--- in case of partial adjustment
    if @val(%pct) < 1 then
      '--- prior-year value
      %prior = "(" + %obj + "-d(" + %obj + "))"
      '--- weighted average of the target and prior value
      %partial = %pct + "*" + %val _
        + "+(1-" + %pct + ")*" + %prior
    '--- full current-year adjustment
    else
      %partial = %val
    endif
    '--- new instrument value
    if %gtyp = "Target" then
      %s = %var + "_sav+0.1*" + %grad+ "*(" _
        + %partial + "-(" + %obj + "))"
    else if %gtyp = "Ceiling" then
      if @val(%pct) < 1 then
        %s = "@iif("+%val+">"+%prior+","+%val+","+%partial+")"
      else
        %s = %val
      endif  
      %gap = "("+%s+"-("+%obj+"))"
      %over = "@iif("+%gap+"<0,-"+%gap+",0)"
      %under = "@iif("+%gap+">0,"+%gap+",0)"
      %s = "@iif(" + %under + ">0 and " _
         + %grad + "*" + %var + "_sav >= 0, 0," _
         + %var + "_sav + 0.1*" + %grad + "*(-" _
         + %over + "+0.5*" + %under + "))"
    '--- Floor
    else
      if @val(%pct) < 1 then
        %s = "@iif("+%val+"<"+%prior+","+%val+","+%partial+")"
      else
        %s = %val
      endif
      %gap = "("+%s+"-("+%obj+"))"
      %over = "@iif("+%gap+"<0,-"+%gap+",0)"
      %under = "@iif("+%gap+">0,"+%gap+",0)"
      %s = "@iif(" + %over + ">0 and " _
         + %grad + "*" + %var + "_sav <= 0, 0, " _
         + %var + "_sav + 0.1*" + %grad + "*(" _
         + %under + "-0.5*" + %over + "))"
    endif
    endif
  endif

  '--- check in case there is a range constraint
  if not (%gtyp = "Shock" or (%gtyp = "Fix" and %obj = "E")) then
    if @val(%pr) > 0 and @val(%pr) < 100 then
      %v = %var + "_insu"
      call CreateSeries(%v)
      m.append @identity {%v} = {%s}
      !v = 0
      call CalcShock(%var, %pr, !v)
      if !v <> na then
        %lm = @str(!v)
        m.append @identity {%var}_ins = @iif({%v}>{%lm}, _
          {%lm}, @iif({%v}<-{%lm}, -{%lm}, {%v}))
      else
        m.append @identity {%var}_ins = {%s}
      endif
    else
      m.append @identity {%var}_ins = {%s}
    endif
  endif
next
endsub

subroutine CalcProb(string %Var, scalar vShock, scalar vPr)
'==============================================================
'return the pvalue of a shock of given size
'
' Call: %Var    variable name
'       vShock  size of shock
'
' Ret:  vPr     pvalue for the shock relative to the
'               historical distribution of residuals for the
'               variable or na if the standard error
'               is not available
'
' Note: uses the pool or EV equation object for the variable
'       to get the standard error of residuals
'
'---------------------------------------------------------------
call CalcSE(%Var, !lib_v)
!lib_v = @cnorm(vShock / !lib_v)
vPr = 2 * @abs(!lib_v - 0.5)
endsub

subroutine CalcSE(string %Var, scalar vSE)
'==============================================================
'return the standard error of shocks for a variable
'
' Call: %Var   variable name
'
' Ret:  vSE    standard error
'              or na if no EV pool or equation is available
'
' Note: uses the pool or EV equation object for the variable
'       to get the standard error of residuals
'
'---------------------------------------------------------------
call VarParts(%Var, %lib_root, %lib_mem)
vSE = 1
if %lib_mem = "" then
  %lib_s = "eq_" + %lib_root
  if @isobject(%lib_s) then
    vSE = eq_{%lib_root}.@se
  endif
else
  if %lib_root = "sxmu" then
    vSE = p_{%Var}.@se
  else
    %lib_s = "p_" + %lib_root
    if @isobject(%lib_s) then
      vSE = p_{%lib_root}.@se
      !lib_i = 0
      call PoolId(p_{%lib_root}, %lib_mem, !lib_i)
      if !lib_i > 0 then
        !lib_vse = _
          @sqrt(p_{%lib_root}.@residcov(!lib_i,!lib_i))
        '--- check for very small std error at bloc level
        if !lib_vse > 0.3*vSE then
          vSE = !lib_vse
        else
          vSE = 0.3*vSE
        endif          
      endif
    endif
  endif  
endif
endsub

subroutine CalcShock(string %Var, string %Pr, scalar vShock)
'==============================================================
'compute size of shock at a given probability level
'
' Call: %Var   variable name
'       %Pr    probability level for the shock
'
' Ret:  vShock size of shock
'              or na if the standard error is not available
'
' Note: uses the pool or EV equation object for the variable
'       to get the standard error of residuals
'
'---------------------------------------------------------------
call CalcSE(%Var, vShock)
vShock = @qnorm(@val(%Pr)/100)*vShock
endsub

subroutine Ceiling(string %Ins, string %Expr, _
  string %Value, scalar vSens, scalar vPCT)
'==============================================================
'set up a lagged adjustment rule
'
' Call: %Ins    instrument variable
'       %Expr   objective function
'       %Value  single value or list
'       vSens   sensitivity of the objective function to the
'                instrument add factor
'               or 0 to have the sensitivity estimated
'       vPCT    percentage of excess to close each year
'
' Ret:
'
' Note: sensitivity is estimated by making a scenario with
' a one-off shock to the instrument add factor
'
'---------------------------------------------------------------
call zAddRule(t_Rule,nRule,%Ins,"Ceiling",%Expr,%Value,vSens,vPCT)
endsub

subroutine local CheckTransform(string %Var, string %Trsf)
'==============================================================
'return the type of transform applied to the dependent variable
'in a behavioural equation
'
' Call: %Var    behavioural variable root name
'
' Ret:  %Trsf   "none" if the variable appears directly or
'                 as a difference
'               "log" if it appears as a log or log difference
'               variable name if it appears as a ratio to another
'                 variable
'
'---------------------------------------------------------------

%Trsf = "none"

'--- get the root name and bloc name
call VarParts(%Var, %v, %b)

'--- find the equation that determines the variable
!iEq = 0
call FindEq(%v, !iEq)
if !iEq = 0 then
  call pLog(%v + " is not determined by a behavioural equation")
  stop
endif

'--- lhs of the equation
call ReadTable("t_Eq", !iEq, 6, %eq)
call Token(%eq, "=", %lhs)

'--- leading characters on lhs before the var name
%s = @left(%lhs, 4)
if %s = "log(" or %s = "dlog" then
  %Trsf = "log"
else

  '--- if the var name is followed by /
  !i1 = @instr(%lhs, "/")
  !i = @instr(%lhs, %v)
  if !i1 > !i then
    %s = @mid(%lhs, !i1+1)
    call Parse(%s, %v)
    %Trsf = @replace(%v,"_?","_" + %b)
  endif
endif
endsub

subroutine local ClearShockGroup(table tRule, scalar nRule, _
  scalar iRule, string %Year)
'==============================================================
'clear one-off shock to a listed variable and any linked
'variables
'
' Call: tRule  rules table
'       nRule  no of rules
'       iRule  rule for variable to be shocked
'       %Year  year at which the shock is to be applied
'
' Ret:
'
'---------------------------------------------------------------
'--- reset main variable
%var = tRule(iRule,1)
call FillSeries(%var + "_ins", "0", %Year, %Year)

'--- reset linked variables
for !i = 1 to nRule
  if tRule(!i, 3) = %var then
    %v = tRule(!i, 1)
    call FillSeries(%v + "_ins", "0", %Year, %Year)
  endif
next
endsub

subroutine DropRules(string %p_tlIns)
'==============================================================
'Drop rules for listed instruments
'
' Call: %p_tlIns   list of instruments separated by blanks
'
' Ret:
'
'---------------------------------------------------------------
call zDropRules(t_Rule, nRule, %p_tlIns)
endsub

subroutine local zDropRules(table p_tRule, scalar p_nRule, _
  string %p_tlIns)
'==============================================================
'Drop rules for listed instruments
'
' Call: p_tRule  rules table
'       p_nRule  no of rules
'       %p_tlIns list of instruments separated by blanks
'
' Ret:  p_tRule  modified table
'       p_nRule  no of remaining rules
'
'---------------------------------------------------------------
%t = @upper(%p_tlIns)
while %t <> ""
  call Token(%t, " ", %s)
  for !i = 1 to p_nRule
    if @upper(p_tRule(!i, 1)) = %s then
      for !j = !i+1 to p_nRule
        for !k = 1 to 7
          p_tRule(!j-1, !k) = p_tRule(!j, !k)
        next
      next
      p_nRule = p_nRule - 1
      exitloop
    endif
  next
wend
endsub

subroutine ExtendAdd(scalar p_vDecay)
'==============================================================
'Extend alignment add factors beyond alignment horizon
'
' Call: p_vDecay   decay rate
'
' Ret:
'
'---------------------------------------------------------------

if p_vDecay = 0 then return endif

'--- make a list of all add factors
smpl %align %align
group lib_gp *_a
freeze(lib_t) lib_gp
delete lib_gp

'--- extrapolation period
smpl %align+1 %end

'--- for each add factor
!lib_i = 1
while 1
  !lib_i = !lib_i + 1
  %lib_s = lib_t(1, !lib_i)
  if %lib_s = "" then exitloop endif
  !lib_v = @elem({%lib_s}, %align)
  if !lib_v <> 0 then
    {%lib_s} = {%lib_s}(-1)*p_vdecay
  endif
wend

delete lib_t
endsub

subroutine ExtendIns(scalar p_vDecay)
'==============================================================
'Extend alignment adjustments beyond alignment horizon
'
' Call: p_vDecay   decay rate
'
' Ret:
'
'---------------------------------------------------------------

if p_vDecay = 0 then return endif

'--- make a list of all ins variables
smpl %align %align
group lib_gp *_ins
freeze(lib_t) lib_gp
delete lib_gp

'--- extrapolation period
smpl %align+1 %end  

'--- for each ins variable
!lib_i = 1
while 1
  !lib_i = !lib_i + 1
  %lib_s = lib_t(1, !lib_i)
  if %lib_s = "" then exitloop endif
  !lib_v = @elem({%lib_s}, %align)
  if !lib_v <> 0 then
    {%lib_s} = {%lib_s}(-1)*p_vdecay
  endif
wend

delete lib_t
endsub

subroutine Floor(string %Ins, string %Expr, _
  string %Value, scalar vSens, scalar vPCT)
'==============================================================
'set up a lagged adjustment rule
'
' Call: %Ins    instrument variable
'       %Expr   objective function
'       %Value  single value or list
'       vSens   sensitivity of the objective function to the
'                instrument add factor
'               or 0 to have the sensitivity estimated
'       vPCT    percentage of shortfall to close each year
'
' Ret:
'
' Note: sensitivity is estimated by making a scenario with
' a one-off shock to the instrument add factor
'
'---------------------------------------------------------------
call zAddRule(t_Rule,nRule,%Ins,"Floor",%Expr,%Value,vSens,vPCT)
endsub

subroutine Fix(string %Ins, string %Mode, string %Value)
'==============================================================
'fix the value of an instrument
'
' Call: %Ins    instrument variable
'       %Mode   "level", "growth" or "exog"
'       %Value  single value or list
'               prefix the last value by * to repeat indefinitely
'
' Ret:
'
' Note: the fix is implemented by modifying the _ins add factor
' in the behavioural equation for the specified instrument
'
'---------------------------------------------------------------
call zAddRule(t_Rule,nRule,%Ins,"Fix",@upper(@left(%Mode,1)), _
  %Value,0,100)
endsub

subroutine local zAddRule(table tRule, scalar nRule, _
  string %Var, string %Action, string %Expression, _
  string %Value, scalar vSens, scalar vPCT)
'==============================================================
'add a rule to the rule table
'
' Call: tRule   rule table
'       nRule   number of rows in the table
'       %Var    behavioural variable modified by the rule
'       %Action type of rule
'                 Shock, Fix, Try, Target, Ceiling, Floor
'                 or Link
'       %Expr   objective function or other reference
'       %Value  target values or multiplier
'       vSens   sensitivity of the objective function to the
'                instrument add factor
'               or 0 to have the sensitivity estimated
'       vPCT    percentage of target-actual gap to close each year
'
' Ret:  tRule   updated table
'       nRule   updated no of rows
'
'---------------------------------------------------------------
!i = nRule + 1
tRule(!i,1) = @upper(%Var)
tRule(!i,2) = %Action
tRule(!i,3) = @upper(%Expression)
tRule(!i,4) = @upper(%Value)
tRule(!i,5) = @str(vSens)
tRule(!i,6) = @str(vPCT)            'how close
tRule(!i,7) = ""                    'limit
nRule = !i
endsub

subroutine local InsToAddFactors(model m, table tRule, _
  scalar nRule, table tBloc, scalar nBloc)
'==============================================================
'convert instrument values to add factors
'
' Call: m       model
'       tRule   table name
'       nRule   number of rows in the table
'       tBloc   bloc list
'       nBloc   number of blocs
'
' Ret:
'
' Note: adds ins to existing add factors if any
'       and zeroes ins
'
'---------------------------------------------------------------
scalar q = 0
call pLog("converting ins to add factors")
for !i = 1 to nRule
  %v = tRule(!i,1)
  '--- special processing for sxmu bloc-level adjuster
  if @left(%v,5) = "sxmu_" and @len(%v) < 10 then
    '--- one add factor for each partner bloc
    for !j = 1 to nBloc
      %b = tBloc(!j, 1)
      %s = %v + "_" + %b
      call Exists(%s, q)
      if q then
        %s = %s + "_a"
        call CreateSeries(%s + "=@nan("+%s+",0)+" + %v + "_ins")
      endif
    next
  '--- other instrument adjustments match a single variable
  else
    %s = %v + "_a"
    call CreateSeries(%s + "=@nan("+%s+",0)+" + %v + "_ins")
  endif
  call CreateSeries(%v + "_ins = 0")
next
endsub

subroutine Limit(scalar vPr, string %tlVar)
'==============================================================
'restrict one or more rules to a probability range
'
' Call: vPr     probability in range 0 to 100
'       %tlVar  list of variables to be restricted
'
' Ret:
'
'---------------------------------------------------------------
call zLimit(t_Rule, nRule, %tlVar, vPr)
endsub

subroutine local zLimit(table tRule, scalar nRule, _
  string %tlVar, scalar vPr)
'==============================================================
'restrict one or more rules to a probability range
'
' Call: tRule   rules table
'       nRule   number of rules
'       %tlVar  list of variables to be restricted
'       vPr     probability in range 0 to 100
'
' Ret:  tRule   updated rules tables
'
'---------------------------------------------------------------
%t = @trim(%tlVar)
if @upper(%t) = "ALL" then
  for !i = 1 to nRule
    if tRule(!i,7) = "" _
      and not (tRule(!i,2) = "Shock" _
        or (tRule(!i,2) = "Fix" and tRule(!i,3) = "E")) then
      tRule(!i,7) = @str(vPr)
    endif
  next
else
  call Token(%t, " ", %s)
  while %s <> ""
    for !i = 1 to nRule
      if tRule(!i,1) = %s then
        if not (tRule(!i,2) = "Shock" _
            or (tRule(!i,2) = "Fix" and tRule(!i,3) = "E")) then
          tRule(!i,7) = @str(vPr)
        endif
      endif
    next
    call Token(%t, " ", %s)
  wend
endif
endsub

subroutine Link(string %Var, string %ToVar, scalar vMult)
'==============================================================
'link a behavioural variable to a shocked variable or policy variable
'
' Call: %Var    behavioural variable
'       %ToVar  variable to which the given variable is linked
'       vMult   multiplier (value of the add factor for %Var
'               as a proportion of the add factor for %ToVar
'
' Ret:
'
'---------------------------------------------------------------
call zAddRule(t_Rule,nRule,%Var,"Link",%ToVar,@str(vMult),0,100)
endsub

subroutine ListAPR(table p_tRep, string %p_Title, _
  string %p_First, string %p_Last, scalar p_Max)
'==============================================================
'write table of add-factor probabilities
'
' Call: p_tRep     result table
'       %p_Title   scenario title
'       %p_First   first year to list
'       %p_Last    last year to list
'       p_Max      max years to display
'
' Ret:  p_tRep     updated result table
'
'---------------------------------------------------------------

'--- get a list of add factors
group llib_g *_a
freeze(t) llib_g

'--- construct a probability series for each add factor
!llib_i = 2
!llib_vse = 0
while 1
  %llib_v = t(1, !llib_i)
  if %llib_v = "" then exitloop endif
  %llib_v1 = @left(%llib_v, @len(%llib_v)-2)
  call CalcSE(%llib_v1, !llib_vse)
  series {%llib_v1}_apr = 2*(@cnorm({%llib_v}/!llib_vse)-0.5)
  series {%llib_v1}_apr = _
    @iif(@abs({%llib_v1}_apr)<0.9, NA, {%llib_v1}_apr)
  !llib_i = !llib_i+1
wend
delete llib_g

'--- tabulate the probabilities
group llib_g *_apr
call ListGroup(llib_g, p_tRep, %p_Title, _
  %p_First, %p_Last, p_Max, 3)
'--- clean up
delete llib_g *_apr
endsub

subroutine local ListGroup(group p_g, table p_tRep, _
  string %p_Title, string %p_First, string %p_Last, _
  scalar p_Max, scalar p_DP)
'==============================================================
'tabulate values of a group of series
'
' Call: p_g        group of series
'       p_tRep     result table
'       %p_Title   title
'       %p_First   first year to list
'       %p_Last    last year to list
'       p_Max      max years to display
'       p_DP       max decimal places
'
' Ret:  p_tRep     updated result table
'
'---------------------------------------------------------------

'--- select years to list
vector vyr
!nyr = 1
call YearList(%p_First, %p_Last, p_Max, vyr, !nyr)

'--- headings
setcell(p_tRep,1,1, %p_Title,"l")
setcell(p_tRep,3,1, "Variable","l")
p_tRep.setwidth(1) 10
!nc0 = 1

'--- write year headings
!ncol = !nyr + !nc0
!nc1 = !nc0 + 1
for !i = !nc1 to !ncol
  setcell(p_tRep,3,!i, @str(vyr(!i-!nc0)), "r")
  p_tRep.setwidth(!i) 9
next
!nrow = 3

freeze(t) p_g
!i = 2
series ser
while 1
  %v = t(1, !i)
  if %v = "" then exitloop endif
  if @instr(%v, "_TAR") = 0 then
    call EvalS(%v, %p_First, %p_Last, ser)
    if @mean(@abs(ser))> 0 then
      !nrow = !nrow + 1
      setcell(p_tRep,!nrow,1, %v, "l")
      for !j = !nc0+1 to !ncol
        !v = @elem(ser, @str(vyr(!j-!nc0)))
        if @isna(!v) then
          %s = ""
        else
          call XFormat(!v, p_DP, %s)
        endif
        setcell(p_tRep,!nrow,!j, %s, "r")
      next
    endif
  endif
  !i = !i + 1
wend

endsub

subroutine local ListExog(table p_tRep, string %p_Title, _
  model p_m, string %p_First, string %p_Last, scalar p_Max)
'==============================================================
'write table of values of non-zero exogenous variables
'
' Call: p_tRep     result table
'       %p_Title   scenario title
'       p_m        model
'       %p_First   first year to list
'       %p_Last    last year to list
'       p_Max      max years to display
'
' Ret:  p_tRep     updated result table
'
'---------------------------------------------------------------

'--- list all instruments
p_m.makegroup(a,n) g_exog @exog

call ListGroup(g_exog, p_tRep, _
 %p_Title + ": exogenous variables", %p_First, %p_Last, p_Max, 4)

endsub

subroutine ModifyIns(string %Var, string %tlVal)
'==============================================================
'Modify instrument values
'
' Call: %Var   variable to modify
'       %tlVal values list
'
' Ret:
'
' Note: applies value list to current sample
'
'---------------------------------------------------------------
series lib_afm
lib_afm.fill {%tlVal}
{%Var} = {%Var} + lib_afm
delete lib_afm
endsub

subroutine Override(model m, string %Var)
'==============================================================
'overrides an exogenous variable and makes a baseline copy
'
' Call: m      model
'       %Var   series to override
'
' Ret:
'
'---------------------------------------------------------------
'--- make a copy in the baseline scenario for use in comparisons
copy {%Var} {%Var}_0
m.override {%Var}
endsub

subroutine pRuleCheck(model m, table tRule, scalar nRule, _
  string %Actual)
'==============================================================
'check scenario rules, print headers and perform shock
'analysis if required
'
' Call: m         model
'       tRule     rules tables
'       nRule     no of entries
'       %Actual   last actual year (base for shocks)
'
' Ret:
'
'---------------------------------------------------------------

%pr = "95"

call pLog("checking rule definitions")

'--- baseline suffix
%compare = t_Settings(5,2)
if %compare <> "" then %compare = "_" + %compare endif

'--- years for shock scenarios
'    backtrack if the comparison is with actuals
%first = @str(@val(%Actual)+1)
if %compare = "" then %first = @str(@val(%first)-3) endif
%last = @str(@val(%first)+2)

'--- check the rules
for !ip = 1 to nRule
  '--- for all rules except Link and Limit
  if @instr("LinkLimit", tRule(!ip,2)) = 0 then
    %v = tRule(!ip,1)
    '--- print a header section
    table t
    scalar n = 0
    call RuleHeader(tRule, nRule, !ip, t, n)
    print t
    delete t n
    '--- if sensitivity is 0, run shock scenario
    if @instr("CeilingFloorTargetTry", tRule(!ip,2)) > 0 _
        and tRule(!ip,5) = "0" then
      '--- create shock scenario
      call pLog("Modelling shock for " + %v)
      m.scenario(n,a=s) Shock
      call SetShockGroup(tRule, nRule, !ip, %first, %pr)
      smpl %first %last
      mode verbose
      m.solve(m=1000)
      m.scenario(d) Shock
      t_Settings(8,2) = "RULECHECK: " + %v + "::" + %pr
      mode quiet
      table t
      scalar n = 0
      call ShockTable(tRule, nRule, !ip, t, n, %first, _
        %compare, %pr)
      print t
      delete t n
      call ClearShockGroup(tRule, nRule, !ip, %First)
      delete *_s
    endif
  endif
next
endsub

subroutine ResetIns(string %Key)
'==============================================================
'Reset instrument values
'
' Call: %Key   key for instruments to reset
'
' Ret:
'
' Note: zeros values in current sample
'
'---------------------------------------------------------------
if @instr("_?_?", %Key) > 0 then
  for !lib_i  = 1 to nBloc
    %lib_b  = t_Bloc(!lib_i, 1)
    for !lib_j = 1 to nBloc
      %lib_p = t_Bloc(!lib_j, 1)
      %lib_s = "_" + %lib_b + "_" + %lib_p
      %lib_v = @replace(%Key, "_?_?", %lib_s)
      if @isobject(%lib_v) then
        {%lib_v} = 0
      endif
    next
  next
else if @instr("_?", %Key) > 0 then
  for !lib_i  = 1 to nBloc
    %lib_b = t_Bloc(!lib_i, 1)
    %lib_s = "_" + %lib_b
    %lib_v = @replace(%Key, "_?", %lib_s)
    if @isobject(%lib_v) then
      {%lib_v} = 0
    endif
  next
else
  if @isobject(%Key) then
    {%Key} = 0
  endif
endif
endif
endsub

subroutine local RuleHeader(table tRule, scalar nRule, _
  scalar iRule, table tRes, scalar nRes)
'==============================================================
'table describing one policy including linked instruments
'
' Call: tRule   rule definitions
'       nRule   no of definitions
'       iRule   main rule in the package
'       tRes    table to contain package definitions
'       nRes    no of rows in the table
'
' Ret:  tRes    description of the package
'       nRes   updated no of rows in the table
'
'---------------------------------------------------------------
%var = tRule(iRule,1)
%gtyp = tRule(iRule,2)
%obj = tRule(iRule,3)
%val = tRule(iRule,4)

'--- list of linked variables
%tlvar = ";" + %var + ";"

'--- get a description from the equation definition for the instrument
!iEq = 0
call FindEq(%var, !iEq)
%s = ""
if !iEq > 0 then
  call ReadTable("t_Eq", !iEq, 3, %s)
endif

'--- write a header line
setcell(tRes,1,1,"Instrument: " + %var + "  " + %s,"l")

%s = tRule(iRule,2)
%s1 = tRule(iRule,7)
if %s1 <> "" and @val(%s1) <> 100 and !iEq > 0 _
    and not (%s = "Shock" _
         or (%s = "Fix" and tRule(iRule,3) = "E")) then
  setcell(tRes,1,2, _
     "probability limit: " + tRule(iRule,7) + "%", "l")
else
  setcell(tRes,1,2," ")
endif
tRes.setwidth(1) 56
tRes.setwidth(2) 24

'--- write the call statement
%s = "   call " + %gtyp + "(" + %var + ", " _
  + %obj + ", " + %val
if @instr("CeilingFloorTargetTry", %gtyp) then
  %s = %s + ", " + tRule(iRule,5)
endif
if tRule(iRule, 6) <> "100" then
  %s = %s + ", " + tRule(iRule, 6) + "%"
endif
%s = %s + ")"
setcell(tRes,3,1,%s, "l")
!irow = 3
!irow0 = !irow

'--- list linked instruments (if any)
for !i = 1 to nRule
  '--- check whether the object is already included
  '    in the package
  %obj = tRule(!i,3)
  if @instr(%tlvar, ";"+%obj+";") > 0 then
    if !irow = !irow0 then
      !irow = !irow + 2
      setcell(tRes,!irow,1,"Linked instruments","l")
      !irow = !irow + 1
    endif
    !irow = !irow + 1
    %v1 = tRule(!i, 1)
    '--- add to list of linked variables
    %tlvar = %tlvar + %v1 + ";"
    call FindEq(%v1, !iEq)
    %s1 = ""
    if !iEq > 0 then
      call ReadTable("t_Eq", !iEq, 3, %s1)
      %s1 = %s1 + " "
    endif
    %s1 = %s1 + "to " + %obj + " (" + tRule(!i, 4) + ")"
    setcell(tRes,!irow,1,"   "+%v1 + " " + %s1,"l")
    %s1 = tRule(!i, 7)
    if %s1 <> "" and @val(%s1) <> 100 _
      and not (tRule(!i,2) = "Shock" _
           or (tRule(!i,2) = "Fix" and tRule(!i,3) = "E")) then
      setcell(tRes,!irow,2, _
        "probability limit: " + tRule(!i,7) + "%", "l")
    endif
  endif
next
nRes = !irow
endsub

subroutine RuleTable(string %p_Tab, string %p_Alias, _
  string %p_First, string %p_Last, scalar p_Max)
'==============================================================
'write tables describing outcome of scenario rules
'
' Call: %p_Tab      table prefix
'       %p_Alias    scenario alias with _ if relevant
'       %p_First    first solution year
'       %p_Last     last solution year
'       p_Max       max data columns
'
' Ret:
'
'---------------------------------------------------------------
if nRule = 0 then return endif
for !liblib_i = 1 to nBloc
  %liblib_b = t_Bloc(!liblib_i, 1)
  table {%p_Tab}_{%liblib_b}
  call zRuleTable(%liblib_b, t_Bloc(!liblib_i, 2), _
    t_Rule, nRule, {%p_Tab}_{%liblib_b}, %p_Alias, _
    %p_First, %p_Last, p_Max)
next

'=== call RuleTableCSV(t_Rule, nRule, _
'           %p_Alias, %p_First, %p_Last, p_Max)

endsub

subroutine RuleTableCSV(table p_tRule, scalar p_nRule, _
  string %p_Alias, string %p_First, string %p_Last, _
  scalar p_Max)
'==============================================================
'write a table describing outcome of scenario rules
'
' Call: %p_Alias    scenario alias with _ if relevant
'       %p_First    first solution year
'       %p_Last     last solution year
'       p_Max       max data columns
'
' Ret:  
'
'---------------------------------------------------------------
table lib_tRes

'--- header lines
!nc0 = 1
!ir =1
for !liblib_i = 1 to nBloc
  %p_Bloc = t_Bloc(!liblib_i, 1)
  %p_BlocName = t_Bloc(!liblib_i, 2)

%b = "_" + @upper(%p_Bloc)

'--- select years to list
vector vyr
!nyr = 1
call YearList(%p_First, %p_Last, p_Max, vyr, !nyr)
!ncol = !nyr + !nc0

'--- write year headers
for !j = !nc0+1 to !ncol
  setcell(lib_tRes,2,!j, @str(vyr(!j-!nc0)), "r")
next
!n = !nc0 + 1
lib_tRes.setwidth(!n:!ncol) 9
'!ir = 1

'--- loop through rules
!ick = 1
!v = 0
!vpr = 0
vector(3) vr
for !i = 1 to p_nRule
  '--- instrument for this bloc
  %vins = p_tRule(!i,1)
  
  if @instr(@upper(%vins), %b) > 0 and !ick = 1 then
  '--- header
    setcell(lib_tRes,!ir,1,"Rules for " + %p_BlocName,"l")
    lib_tRes.setwidth(1) 20
    !ick = 2
  endif
  if @instr(@upper(%vins), %b) > 0 then
    %var = p_tRule(!i,1)
    %gtyp = p_tRule(!i,2)
    %obj = p_tRule(!i,3)
    %val = @trim(p_tRule(!i,4))
    if %gtyp = "Shock" then
      %obj = %var
    endif
    '--- Link to instrument in other bloc
    if %gtyp = "Link" then
      if @instr(@upper(%obj), %b) = 0 then
        !ir = !ir + 2
        setcell(lib_tRes,!ir,1,"Instrument linked to " + %obj,"l")
      endif
    '--- rules with targets
    else
      !ir =!ir + 2
      %s = %gtyp + " " + %obj
      setcell(lib_tRes,!ir,1, %s,"l")
      setcell(lib_tRes,!ir+1,1,"  Target values","l")
      setcell(lib_tRes,!ir+2,1,"  Actual values","l")
      setcell(lib_tRes,!ir+3,1,"  Difference ","l")
      setcell(lib_tRes,!ir+5,1,"Instruments","l")

      '--- get an expression for the target value
      if @instr(%val," ") > 0 then
        %val = %var + "_val"
      endif
      for !j = 1 to !nyr
        '--- target value
        call Eval(%val, @str(vyr(!j)), !v)
        vr(1) = !v
        '--- actual value
        if %gtyp = "Fix" then
          call ScenarioExpr(%var, %p_Alias, %v)
        else
          call ScenarioExpr(%obj, %p_Alias, %v)
        endif
        call Eval(%v, @str(vyr(!j)), !v)
        vr(2) = !v
        '--- difference
        vr(3) = vr(1) - vr(2)
        for !k = 1 to 3
          call XFormat(vr(!k), 4, %s)
          setcell(lib_tRes,!ir+!k,!j+1, %s, "r")
        next
      next
      !ir = !ir + 5
    endif
    '--- instrument value and probability
    !ir = !ir + 1
    setcell(lib_tRes,!ir,1,"  " + %vins + "_ins","l")
    setcell(lib_tRes,!ir+1,1,"     p-value","l")

    if %gtyp = "Shock" then
      if p_tRule(!i,3) = "A" then
        %var = %vins + "_a"
        setcell(lib_tRes,!ir,1,"  " + %vins + "_a","l")
      else
        %var = %vins + "_ins"
      endif
    else
      %var = %vins + "_ins" + %p_Alias
    endif
    for !j = 1 to !nyr
      call Eval(%var, @str(vyr(!j)), !v)
      call XFormat(!v, 4, %s)
      setcell(lib_tRes,!ir, !j+1, %s, "r")
      call CalcProb(%vins, !v, !vpr)
      %s = "("+@str(!vpr, "f.3")+")"
      setcell(lib_tRes,!ir+1, !j+1, %s, "r")
    next
    !ir = !ir + 1
  endif
next
 !ir = !ir + 2
next

lib_tRes.save(t = csv) RuleTable{%p_Alias}
endsub

subroutine local zRuleTable(string %p_Bloc, string %p_BlocName, _
  table p_tRule, scalar p_nRule, table p_tRes, string %p_Alias, _
  string %p_First, string %p_Last, scalar p_Max)
'==============================================================
'write a table describing outcome of scenario rules for one bloc
'
' Call: %p_Bloc     bloc code
'       %p_BlocName bloc name
'       p_tRule     rule definitions
'       p_nRule     no of definitions
'       p_tRes      result table
'       %p_Alias    scenario alias with _ if relevant
'       %p_First    first solution year
'       %p_Last     last solution year
'       p_Max       max data columns
'
' Ret:  p_tRes      updated table
'
'---------------------------------------------------------------

if p_nRule = 0 then return endif
%b = "_" + @upper(%p_Bloc)

'--- header lines
setcell(p_tRes,1,1,"Rules for " + %p_BlocName,"l")
p_tRes.setwidth(1) 20
!nc0 = 1

'--- select years to list
vector vyr
!nyr = 1
call YearList(%p_First, %p_Last, p_Max, vyr, !nyr)
!ncol = !nyr + !nc0

'--- write year headers
for !j = !nc0+1 to !ncol
  setcell(p_tRes,2,!j, @str(vyr(!j-!nc0)), "r")
next
!n = !nc0 + 1
p_tRes.setwidth(!n:!ncol) 9
!ir = 1

'--- loop through rules
!v = 0
!vpr = 0
vector(3) vr
for !i = 1 to p_nRule
  '--- instrument for this bloc
  %vins = p_tRule(!i,1)
  if @instr(@upper(%vins), %b) > 0 then
    %var = p_tRule(!i,1)
    %gtyp = p_tRule(!i,2)
    %obj = p_tRule(!i,3)
    %val = @trim(p_tRule(!i,4))
    if %gtyp = "Shock" then
      %obj = %var
    endif
    '--- Link to instrument in other bloc
    if %gtyp = "Link" then
      if @instr(@upper(%obj), %b) = 0 then
        !ir = !ir + 2
        setcell(p_tRes,!ir,1,"Instrument linked to " + %obj,"l")
      endif
    '--- rules with targets
    else
      !ir =!ir + 2
      %s = %gtyp + " " + %obj
      setcell(p_tRes,!ir,1, %s,"l")
      setcell(p_tRes,!ir+1,1,"  Target values","l")
      setcell(p_tRes,!ir+2,1,"  Actual values","l")
      setcell(p_tRes,!ir+3,1,"  Difference ","l")
      setcell(p_tRes,!ir+5,1,"Instruments","l")

      '--- get an expression for the target value
      if @instr(%val," ") > 0 then
        %val = %var + "_val"
      endif
      for !j = 1 to !nyr
        '--- target value
        call Eval(%val, @str(vyr(!j)), !v)
        vr(1) = !v
        '--- actual value
        if %gtyp = "Fix" then
          call ScenarioExpr(%var, %p_Alias, %v)
        else
          call ScenarioExpr(%obj, %p_Alias, %v)
        endif
        call Eval(%v, @str(vyr(!j)), !v)
        vr(2) = !v
        '--- difference
        vr(3) = vr(1) - vr(2)
        for !k = 1 to 3
          call XFormat(vr(!k), 4, %s)
          setcell(p_tRes,!ir+!k,!j+1, %s, "r")
        next
      next
      !ir = !ir + 5
    endif
    '--- instrument value and probability
    !ir = !ir + 1
    setcell(p_tRes,!ir,1,"  " + %vins + "_ins","l")
    setcell(p_tRes,!ir+1,1,"     p-value","l")

    if %gtyp = "Shock" then
      if p_tRule(!i,3) = "A" then
        %var = %vins + "_a"
        setcell(p_tRes,!ir,1,"  " + %vins + "_a","l")
      else
        %var = %vins + "_ins"
      endif
    else
      %var = %vins + "_ins" + %p_Alias
    endif
    for !j = 1 to !nyr
      call Eval(%var, @str(vyr(!j)), !v)
      call XFormat(!v, 4, %s)
      setcell(p_tRes,!ir, !j+1, %s, "r")
      call CalcProb(%vins, !v, !vpr)
      %s = "("+@str(!vpr, "f.3")+")"
      setcell(p_tRes,!ir+1, !j+1, %s, "r")
    next
    !ir = !ir + 1
  endif
next
if !ir = 1 then delete p_tRes endif
endsub

subroutine local ScenarioTable(table tRule, scalar nRule, _
  table tRes, scalar nRes, string %Alias, string %Compare, _
  string %Actual, string %Predict)
'==============================================================
'write table describing scenario results
'
' Call: tRule    rule definitions
'       nRule    no of definitions
'       tRes     table to contain scenario results
'       nRes     no of rows in the table
'       %Alias   scenario alias with _ if relevant
'       %Compare baseline alias with _ if relevant
'       %Actual  last actual year
'       %Predict last solution year
'
' Ret:  tRes    description of scenario results
'       nRes    updated no of rows in the table
'
'---------------------------------------------------------------

if nRule = 0 then return endif

'--- header lines
setcell(tRes,2,1,"  Scenario results","l")
tRes.setwidth(1) 20

'--- display values for 1, 2, 3 or 4 years
vector(4) yr
yr(1) = @val(%Actual) + 1
yr(2) = yr(1) + 1
yr(4) = @val(%Predict)
if yr(4) < yr(2) then
  !nyr = 1
  tRes.setwidth(2:3) 20
else
  if yr(4) = yr(2) then
    !nyr = 2
    tRes.setwidth(2:3) 30
  else
    if yr(4) = yr(2) + 1 then
      !nyr = 3
      yr(3) = yr(4)
      tRes.setwidth(2:4) 20
    else
      !n = @floor((yr(4)-yr(2))/2)
      yr(3) = yr(2) + !n
      !nyr = 4
      tRes.setwidth(2:5) 15
    endif
  endif
endif
for !j = 1 to !nyr
  setcell(tRes,2,!j+1,yr(!j),"r",0)
next
!ir = 4

'--- objectives
!ir0 = !ir
!v = 0
vector(5) vr
for !i = 1 to nRule
  %gtyp = tRule(!i,2)
  if @instr("CeilingFloorTargetTry", %gtyp) > 0 then
    if !ir = !ir0 then
      setcell(tRes,!ir,1,"  Targets","l")
      !ir = !ir + 2
    endif
    %var = tRule(!i,1)
    %gtyp = tRule(!i,2)
    %obj = tRule(!i,3)
    %val = @trim(tRule(!i,4))
    setcell(tRes,!ir,1,"  " + %obj,"l")
    setcell(tRes,!ir+1,1,"    target values","l")
    setcell(tRes,!ir+2,1,"    scenario","l")
    setcell(tRes,!ir+3,1,"    gap","l")
    !nr = 3
    if %Compare <> %Alias then
      setcell(tRes,!ir+4,1,"    baseline","l")
      setcell(tRes,!ir+5,1,"    difference","l")
      !nr = 5
    endif
    '--- get an expression for the target value
    if @instr(%val," ") > 0 then
      %val = %var + "_val"
    endif
    for !j = 1 to !nyr
      '--- get the target value
      call Eval(%val,@str(yr(!j)),!v)
      vr(1) = !v
      call ScenarioExpr(%obj, %Alias, %v)
      call Eval(%v,@str(yr(!j)),!v)
      vr(2) = !v
      vr(3) = vr(1) - vr(2)
      if !nr = 5 then
        call ScenarioExpr(%obj, %Compare, %v)
        call Eval(%v,@str(yr(!j)),!v)
        vr(4) = !v
        vr(5) = vr(2)-vr(4)
      endif
      for !i1 = 1 to !nr
        call NFormat(vr(!i1), %s)
        setcell(tRes,!ir+!i1,!j+1,%s,"r")
      next
    next
    !ir = !ir + !nr + 2
  endif
next

'--- instrument values
'    scenario and baseline values, diff and %diff
!ir0 = !ir
for !i = 1 to nRule
  if !ir = !ir0 then
    setcell(tRes,!ir,1,"  Instrument settings","l")
    !ir = !ir + 2
  endif
  %var = tRule(!i,1)
  '--- check whether there is an _ins component and
  '    whether it is scenario-specific
  %vins = %var + "_ins" + %Alias
  call Exists(%vins, !v)
  if !v = 0 then
    %vins = %var + "_ins"
    call Exists(%vins, !v)
    if !v = 0 then
      %vins = ""
    endif
  endif
  setcell(tRes,!ir,1,"  " + %var,"l")
  if %vins <> "" then
    !ir = !ir + 1
    setcell(tRes,!ir,1,"    add factor", "l")
  endif
  setcell(tRes,!ir+1,1,"    scenario value","l")
  !nr = 1
  if %Alias <> %Compare then
    setcell(tRes,!ir+2,1,"    baseline value","l")
    setcell(tRes,!ir+3,1,"    difference","l")
    setcell(tRes,!ir+4,1,"    % difference","l")
    !nr = 4
  endif
  for !j = 1 to !nyr
    if %vins <> "" then
      call Eval(%vins,@str(yr(!j)),!v)
      vr(1) = !v
    endif
    %v = %var+%Alias
    !v = na
    !v1 = 0
    call Exists(%v, !v1)
    if !v1 <> 0 then
      call Eval(%v,@str(yr(!j)),!v)
    endif
    vr(2) = !v
    if !nr = 4 then
      if !v1 = 0 then
        vr(3) = na
        vr(4) = na
        vr(5) = na
      else
        call Eval(%var+%Compare,@str(yr(!j)),!v)
        vr(3) = !v
        vr(4) = vr(2) - vr(3)
        if @abs(vr(3)) > 1e-6 then
          vr(5) = 100*vr(4)/@abs(vr(3))
        endif
      endif
    endif
    !i0 = 1
    if %vins = "" then !i0 = 2 endif
    for !i1 = !i0 to !nr + 1
      call NFormat(vr(!i1), %s)
      setcell(tRes,!ir+!i1-1,!j+1,%s,"r")
    next
  next
  !ir = !ir + !nr + 2
next
nRes = !ir-1
endsub

subroutine local SetShockGroup(table tRule, scalar nRule, _
  scalar iRule, string %Year, string %Pr)
'==============================================================
'set up a one-off shock to a listed variable and any linked
'variables
'
' Call: tRule  rules table
'       nRule  no of rules
'       iRule  rule for variable to be shocked
'       %Year  year at which the shock is to be applied
'       %Pr    probability level for the shock (between 0 and 100)
'
' Ret:
'
'---------------------------------------------------------------
!v = 0
'--- shock the main variable
%var = tRule(iRule,1)
call CalcShock(%var, %Pr, !v)
%s = @str(!v)
call FillSeries(%var + "_ins", %s, %Year, %Year)

'--- find linked variables and shock them
for !i = 1 to nRule
  if tRule(!i, 3) = %var then
    %v = tRule(!i, 1)
    call CalcShock(%v, %Pr, !v)
    %s = @str(!v*@val(tRule(!i,4)))
    call FillSeries(%v + "_ins", %s, %Year, %Year)
  endif
next
endsub

subroutine Shock(string %Var, string %Mode, string %Value)
'==============================================================
'setup a shock to a behavioural variable
'
' Call: %Var    behavioural variable
'       %Mode   "add" or "mult"
'       %Value  single value or list
'               insert * to repeat remaining value(s) indefinitely
'
' Ret:
'
' Note: the shock is implemented by modifying the _ins add factor
' in the behavioural equation for the specified variable
'
'---------------------------------------------------------------
call zAddRule(t_Rule,nRule,%Var,"Shock",@upper(@left(%Mode,1)), _
  %Value,0,100)
endsub

subroutine local ShockTable(table tRule, scalar nRule, _
  scalar iRule, table tRes, scalar nRes, string %Year, _
  string %Compare, string %Pr)
'==============================================================
'write table describing effects of a shock in years 1 to 3
'
' Call: tRule    rule definitions
'       nRule    no of definitions
'       iRule    main rule in the package
'       tRes     table to contain shock effects
'       nRes     no of rows in the table
'       %Year    year in which the shock is applied
'       %Compare baseline alias with _ if relevant
'       %Pr      probability of the shock
'
' Ret:  tRes    description of shock effects
'       nRes    updated no of rows in the table
'
'---------------------------------------------------------------
%var = tRule(iRule,1)
%atyp = tRule(iRule,2)
%obj = tRule(iRule,3)
%val = tRule(iRule,4)

'--- list of linked variables
%tlvar = ";" + %var + ";"

setcell(tRes,2,1,"  Impact on " + %obj + " of " _
  + %Pr + "% shock to " + %var + " in " + %Year,"l")

%Y2 = @str(@val(%Year)+1)
%Y3 = @str(@val(%Year)+2)
setcell(tRes,4,2,%Year,"r")
setcell(tRes,4,3,%Y2,"r")
setcell(tRes,4,4,%Y3,"r")
!ihdr = 4

tRes.setwidth(1:4) 20

'--- set row headings
setcell(tRes,5,1,"  " + %var + "_ins","l")  'shocked series
!vins = 0
call Eval(%var + "_ins", tRes(!ihdr,2), !vins)   'size of shock
call NFormat(!vins,%s)
setcell(tRes,5,2,%s,"r")
setcell(tRes,5,3,"0","r")
setcell(tRes,5,4,"0","r")

setcell(tRes,6,1,"  " + %obj,"l")     'objective expression
!irow0 = 6
setcell(tRes,7,1,"  sensitivity","l") 'sensitivity
setcell(tRes,8,1,"  " + %var,"l")     'instrument variable
!irow = 8                             'linked variables if any
for !i = 1 to nRule
  '--- check whether the object is already included
  '    in the package
  %obj = tRule(!i,3)
  if @instr(%tlvar, ";"+%obj+";") > 0 then
    !irow = !irow + 1
    setcell(tRes,!irow,1,"  " + tRule(!i, 1),"l")
    '--- add to list of linked variables
    %tlvar = %tlvar + %v1 + ";"
  endif
next
!nrow = !irow

'--- fill data columns
!v = 0
!vs = 0
for !irow = !irow0 to !nrow
  '--- sensitivity row: impact/shock size
  if !irow = !irow0 + 1 then
    for !j = 2 to 4
      !v = @val(tRes(!irow0,!j))/!vins
      call NFormat(!v,%s)
      setcell(tRes,!irow,!j,%s,"r")
    next
    '--- if unset in the rule definition, write in
    if @val(tRule(iRule,5)) = 0 then
      setcell(tRule, iRule, 5, tRes(!irow,2), "l")
    endif
  '--- other rows: get scenario value minus baseline value
  else
    call ScenarioExpr(tRes(!irow,1), "_s", %vs)
    call ScenarioExpr(tRes(!irow,1), %Compare, %v)
    '--- check whether variable exists
    !v1 = 1
    if !irow  > !irow0 + 1 then
      call Exists(%vs, !v1)
    endif
    for !j = 2 to 4
       if !v1 <> 0 then
        call Eval(%vs, tRes(!ihdr,!j), !vs)
        call Eval(%v, tRes(!ihdr,!j), !v)
        call NFormat(!vs-!v,%s)
      else
        %s = "NA"
      endif
      setcell(tRes,!irow,!j,%s,"r")
    next
  endif
next

'--- write advice about the sensitivity parameter
!v = @val(tRes(!irow0+1,2))
if @abs(!v) < 0.01 then
  %t = "NB: the instrument is not effective for this target"
else
  %t = "  NB: the sensitivity parameter" _
  + " in the rule definition should be set to approx. " + _
  + @str(!v,"g.4")
endif
setcell(tRes,!nrow + 2,1,%t,"l")
!nrow = !nrow + 3
setcell(tRes,!nrow,1," ")
tRes.setlines(!nrow,1,!nrow,4) +b
nRes = !nrow
endsub

subroutine Target(string %Ins, string %Expr, string %Value, _
  scalar vSens, scalar vPCT)
'==============================================================
'set up a target-instrument rule
'
' Call: %Ins    instrument variable
'       %Expr   objective function
'       %Value  single value or list
'               prefix the last value by * to repeat indefinitely
'       vSens   sensitivity of the objective function to the
'                instrument add factor
'               or 0 to have the sensitivity estimated
'       vPCT    percentage of target-actual gap to close each year
'
' Ret:
'
' Note: sensitivity is estimated by making a scenario with
' a one-off shock to the instrument's add factor
'
'---------------------------------------------------------------
call zAddRule(t_Rule,nRule,%Ins,"Target",%Expr,%Value,vSens,vPCT)
endsub

subroutine Try(string %Ins, string %Expr, string %Value, _
  scalar vSens, scalar vPCT)
'==============================================================
'set up a lagged adjustment rule
'
' Call: %Ins    instrument variable
'       %Expr   objective function
'       %Value  single value or list
'       vSens   sensitivity of the objective function to the
'                instrument add factor
'               or 0 to have the sensitivity estimated
'       vPCT    percentage of target-actual gap to close each year
'
' Ret:
'
' Note: sensitivity is estimated by making a scenario with
' a one-off shock to the instrument add factor
'
'---------------------------------------------------------------
call zAddRule(t_Rule,nRule,%Ins,"Try",%Expr,%Value,vSens,vPCT)
endsub

subroutine local YearList(string %p_First, string %p_Last, _
  scalar p_Max, vector p_vyr, scalar p_nyr)
'==============================================================
'write a list of years into a vector
'
' Call: %p_First   first year to list
'       %p_Last    last year to list
'       p_Max      max length of list
'       p_vyr      empty vector
'
' Ret:  p_vyr      updated vector
'       p_nyr      no of years in the list
'
' Note: if p_Max is sufficient, the list contains all years
'       from First to Last; otherwise, it contains a selection
'       beginning with First and ending with Last
'
'---------------------------------------------------------------

!yr0 = @val(%p_First)
!yr1 = @val(%p_Last)
!nyr = !yr1 - !yr0 + 1
!nmax = p_Max
if !nmax > !nyr then !nmax = !nyr endif

!nc1 = 0
!nc2 = 0
!ncol = !nyr

'--- spacing required
if !ncol > !nmax then
  !ncol = !nmax
  '--- consecutive years at the start
  !nc1 = 4
  if !nmax < 5 then
    !nc1 = !ncol - 1
  endif
  '--- remaining no of years and no of cols
  !nyrem = !nyr - !nc1 + 1
  !ncrem = !ncol - !nc1 + 1
  '--- increment for early years
  !inc = @floor(!nyrem/!ncrem)
  '--- column at which to switch to larger increment
  !nc2 = !nc1 + !ncrem*(1 + !inc) - !nyrem
endif

'--- build the vector
vector(!ncol) p_vyr
p_vyr(1) = !yr0
!yr = !yr0
!k = 1
for !i = 2 to !ncol
  if !i = !nc1 then !k = !inc endif
  if !i = !nc2 then !k = !k + 1 endif
  p_vyr(!i) = p_vyr(!i-1) + !k
next
p_nyr = !ncol
endsub
