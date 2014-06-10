'PROGRAM: zimpact.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 5.0
'
' subroutine to generate a dynamic multiplier report
'
' Note: routines whose names begin with z_ are internal. Names
' beginning with lib_ are reserved and should not be used
' elsewhere.
'
' updated: NK 15/09/2010
'
'---------------------------------------------------------------

'==============================================================
'variable type definitions
'   A  aggregate: diff = ys-y0, %diff = 100*(ys/y0-1)
'   B  balance: diff = ys-y0, %diff = 100*(ys-y0)/GDP
'   G  growth or inflation rate: %diff = ys-y0
'   I  index: %diff = 100*(ys/y0-1)
'   R  ratio: %diff = 100*(ys-y0) or %diff = 100*(ys-y0)/ref
'
'variables not listed are assumed to be type A
'
'---------------------------------------------------------------
%tvar = "(B) BA$,BE$,BM$,BS$,BIT$,TB$,CA$,IV," _
    + "BA0U,NLG,NFF$,NGF$,NLP,NLG,NX$,NXF$,SP;" _
  + "(G) is,im,irs,irm,NUL,NULVF,NULVM,NULYF,NULYM,VVPRV," _
    + "NLNVF,NLNVM,NLNYF,NLNYM,ei,mu,rtx,pvi,pi,pi$,rxna;" _
  + "(I) CO2,ED,EP,NER,NE,NEF,NEM,NEVF,NEVM,NEYF,NEYM,NL,mu," _
    + "NEA,NEAF,NEAM,VGA,NNE,GCY,VVAE,VNE," _
    + "VV,VVA,VVE,VVI,VVS,VVEM,VVEME,VVPR,VVTX," _
    + "pa,pa$,pe,pe$,ph,pkp,rtx,rxu,rx,rxd,tt;" _
  + "(R) NIMU(N population)," _
      + "NIM(NE employment),sxm,wln"

subroutine pShockImpact(string %tlImpact, scalar qFull, _
  scalar qGraph)
'==============================================================
'report shock impacts
'
' Call: %tlImpact  list of variables impacted by the shock
'       qFull      true to list detailed resullts
'       qGraph     true to include graphs
'
' Ret: 
'
' Note: creates table tmult_shock_var and graphs grsh_var_effect
'
'---------------------------------------------------------------

'--- load variable type definitions
if not @isobject("t_VType") then
  table t_VType
  scalar nVType = 0
  call zLoadVarTypes(%tvar, t_VType, nVType)
endif

'--- shocked var and probability level
%lbp_pr = t_Settings(8,2)
call Token(%lbp_pr, ":", %lbp_s)
call Token(%lbp_pr, ":", %lbp_s)
call Token(%lbp_pr, ":", %lbp_vn)
call Token(%lbp_s, "(", %lbp_shockvar)
if %lbp_s <> "" then
  call Token(%lbp_s,")", %lbp_shockref)
else
  %lbp_shockref = %lbp_shockvar
endif

'--- shocked bloc code
%lbp_b = %lbp_shockvar
call Token(%lbp_b, "_", %lbp_s)
if %lbp_b = "" then %lbp_b = "world" endif

'--- evaluation period
%lbp_actual = t_Settings(1,2)
%lbp_first = @str(@val(%lbp_actual) + 1)
%lbp_predict = @str(@val(%lbp_actual) + 6)
if @val(%lbp_predict) > t_Settings(2,2) then
  %lbp_predict = t_Settings(2,2)
endif

'--- shocked
%lbp_alias = t_Settings(3,2)
if %lbp_alias <> "" then
  %lbp_alias = "_" + %lbp_alias
endif  

'--- baseline
%lbp_compare = t_Settings(5,2)
if %lbp_compare <> "" then
  %lbp_compare = "_" + %lbp_compare
endif  

'--- generate the title
if qFull then
  %lbp_t = "Impact of " + %lbp_pr _
    + "% shock to " + %lbp_shockvar + " in " _
    + %lbp_first
else
  call BlocName(%lbp_b, %lbp_bn)
  %lbp_t = "Impact of shock to " + %lbp_bn + " " + _
    %lbp_vn + " in " + %lbp_first
endif

call pLog(%lbp_t)

'--- create a tabular report
table lbp_t
scalar lbp_n
if qFull then
  call zFullImpact(%lbp_shockvar, %lbp_shockref, %tlImpact, _
    %lbp_t, %lbp_pr, lbp_t, lbp_n, _
    %lbp_alias, %lbp_compare, %lbp_actual, %lbp_predict)
  pageselect tables
  '--- check whether the report already exists
  '    if it does, add "_more" to the name for the new report
  %lib_s = "tsh_" + %lbp_shockvar
  if @isobject(%lib_s) then
    %lib_s = %lib_s + "_more"
    if @isobject(%lib_s) then delete {%lib_s} endif
  endif  
  copy data\lbp_t {%lib_s}
else
  call zSummaryImpact(%lbp_shockvar, %lbp_shockref, _
    %lbp_vn, %tlImpact, _
    %lbp_t, %lbp_pr, lbp_t, lbp_n, %lbp_alias, _
    %lbp_compare, %lbp_actual, %lbp_predict, _
    t_Bloc, nBloc)
  pageselect tables
  copy data\lbp_t tmult_{%lbp_shockvar}
endif
'--- transfer to the tables page
pageselect data
delete lbp_t lbp_n

'--- generate graphs (also printed to the spool)
if qGraph then
  call zImpactGraphs(%lbp_shockvar, %Impacton, _
    %lbp_t, %lbp_alias, %lbp_compare, %lbp_first, %lbp_predict)
endif
endsub

subroutine local zFullImpact(string %Shockvar, _
  string %Shockref, string %tlImpact, string %Title, _
  string %Pr, table tRes, scalar nRes, string %Alias, _
  string %Compare, string %Actual, string %Predict)
'==============================================================
'write table describing scenario results
'
' Call: %Shockvar  shocked variable
'       %Shockref  reference for size of shock
'       %tlImpact  list of impact expressions
'       %Title     title for the table
'       %Pr        probability level
'       tRes       table to contain the results
'       nRes       no of rows in the table
'       %Alias     scenario alias with _ if relevant
'       %Compare   baseline alias with _ if relevant
'       %Actual    last actual year
'       %Predict   last solution year
'
' Ret:  tRes    description of shock and results
'       nRes    updated no of rows in the table
'
'---------------------------------------------------------------

'--- header lines
setcell(tRes, 2, 1, %Title, "l")
tRes.setwidth(1) 20
for !j = 2 to 5
  setcell(tRes, 1, !j, " ")
next  

'--- display values for 1, 2, 3 or 4 years
vector(4) yr
!nyr = 0
!width = 0
call zYears(%Actual, %Predict, yr, !nyr, !width)
for !j = 1 to !nyr
  setcell(tRes,4, !j+1, yr(!j), "r", 0)
next
!n = !nyr + 1
tRes.setwidth(2:!n) !width
!ir = 4

'--- size of shock
!vins = 0
call CalcShock(%Shockref, %Pr, !vins)
!ir = !ir + 1
setcell(tRes,!ir, 1, "Shock", "l")
call NFormat(!vins, %s)
setcell(tRes, !ir, 2, %s, "r")
for !j = 2 to !nyr
  setcell(tRes, !ir, !j+1, "0", "r")
next

'--- shocked variable
call zSVarType(%Shockvar, _
  %stype, %spc, %sref, %seq, %svar, %sb, %slhvar)

%sb = @upper(%sb)
call BlocName(%sb, %sbn)

'--- size of shock
!vins = 0
call CalcShock(%Shockref, %Pr, !vins)

'---- get ex ante impact
!vexa = na
if %seq <> "" then
  call ExAnteValue(%slhvar, %Shockvar + "_ins", %seq, _
    @str(yr(1)), !vins, !vexa)
endif

'--- impacts
!v = 0
vector(4) vr
%tl = %tlImpact
%vn = %Shockvar
!qfirst = 1
while %vn <> ""
  call Token(%vn, ":", %var)
  if !qfirst or %Shockvar <> %var then
    '--- check the variable type
    call zVarType(%var, %type, %pc, %vref)
    '--- determine the bloc ref
    !i = @instr(%var, "_")
    if !i > 0 then
      %bref = "_" + @mid(%var, !i+1)
    else
      %bref = ""
    endif
    if %vref <> "" then %vref = %vref + %bref endif
    
    '--- year 1 differences for shocked variable
    '    will be used to calculate multipliers and elasticities
    if !qfirst then 
      '--- ex post difference is ok for exogenous variable
      if !vexa = na then
        call zDiff(%Shockvar, %stype, %Alias, %Compare, %sref, _
          !vins, @str(yr(1)), vr)
      '--- ex ante difference is used for endogenous variable
      else    
        call zSDiff(%Shockvar, %stype, %Compare, %sref, _
          !vexa, @str(yr(1)), vr)
      endif
      !vdif = vr(3)
      !vpct = vr(4)
      !ir = !ir + 1
      setcell(tRes,!ir, 1, "Ex ante effect on " + %Shockvar, "l")
      call NFormat(!vdif, %s)
      setcell(tRes, !ir, 2, %s, "r")
      for !j = 2 to !nyr
        setcell(tRes, !ir, !j+1, "0", "r")
      next
    endif
    '--- multiplier or elasticity
    !qmult = 0
    %sens = "elasticity"
    if @instr("AB", %stype) > 0 and @instr("AB", %type) > 0 then
      !qmult = 1
      %sens = "multiplier"
    endif
    '--- write row headers
    !ir = !ir + 2
    setcell(tRes, !ir,1, %var, "l")
    !ir1 = !ir
    !ir = !ir + 1
    setcell(tRes, !ir, 1, "  values with shock", "l")
    !ir = !ir + 1
    setcell(tRes, !ir, 1, "  baseline", "l")
    if !qmult then
      %s = "difference"
    else 
      %s = %pc
    endif
    !ir = !ir + 1
    setcell(tRes, !ir, 1, "  " + %s, "l")
    !ir = !ir + 1
    setcell(tRes, !ir, 1,"  " + %sens, "l")
    '--- fetch the data for each column
    for !j = 1 to !nyr
      '--- differences for shocked variable if exogenous
      if !j = 1 then !v = !vins else !v = 0 endif
      call zDiff(%var, %type, %Alias, %Compare, %vref, _
        !v, @str(yr(!j)), vr)
      !ir = !ir1 + 1
      call NFormat(vr(1), %s)
      setcell(tRes, !ir, !j+1, %s, "r")
      !ir = !ir + 1
      call NFormat(vr(2), %s)
      setcell(tRes, !ir, !j+1, %s,"r")
      '--- difference or % difference
      !ir = !ir + 1
      if !qmult then
        call NFormat(vr(3), %s)
      else
        call NFormat(vr(4), %s)
      endif
      setcell(tRes, !ir, !j+1, %s, "r")
      '--- multiplier or elasticity
      if !qmult then
        call zRatio(vr(3), !vdif, na, !v)
      else    
        call zRatio(vr(4), !vpct, na, !v)
      endif
      !ir = !ir + 1
      call NFormat(!v, %s)
      setcell(tRes, !ir, !j+1, %s, "r")
    next
  endif
  call Token(%tl, " ", %vn)
  !qfirst = 0
wend
nRes = !ir
endsub

subroutine local zSummaryImpact(string %Shockvar, _
  string %Shockref, string %Shocknam, string %tlImpact, _
  string %Title, string %Pr, table tRes, scalar nRes, _
  string %Alias, string %Compare, string %Actual, _
  string %Predict, table tBloc , scalar nBloc)
'==============================================================
'write table describing scenario results
'
' Call: %Shockvar  shocked variable
'       %Shockref  reference for size of shock
'       %Shocknam  description of shocked variable
'       %tlImpact  list of impact expressions
'       %Title     title for the table
'       %Pr        probability level
'       tRes       table to contain the results
'       nRes       no of rows in the table
'       %Alias     scenario alias with _ if relevant
'       %Compare   baseline alias with _ if relevant
'       %Actual    last actual year
'       %Predict   last solution year
'       tBloc      bloc table
'       nBloc      number of blocs
'
' Ret:  tRes    description of shock and results
'       nRes    updated no of rows in the table
'
'---------------------------------------------------------------

'--- header lines
setcell(tRes, 1, 1, %Title, "c")
tRes.setwidth(1) 16
for !j = 2 to 6
  setcell(tRes, 1, !j, " ")
next  
tRes.setwidth(2) 28
tRes.setmerge(1,1,1,6)

'--- display values for 1, 2, 3 or 4 years
vector(4) yr
!nyr = 0
!width = 0
call zYears(%Actual, %Predict, yr, !nyr, !width)
for !j = 1 to !nyr
  setcell(tRes, 3, !j+2, yr(!j), "r", 0)
next
!n = !nyr + 2
!width = !width-5
tRes.setwidth(3:!n) !width
!ir = 3

'--- shocked variable
call zSVarType(%Shockvar, _
  %stype, %spc, %sref, %seq, %svar, %sb, %slhvar)

%sb = @upper(%sb)
call BlocName(%sb, %sbn)

'--- size of shock
!vins = 0
call CalcShock(%Shockref, %Pr, !vins)

'---- get ex ante impact
!vexa = na
if %seq <> "" then
  call ExAnteValue(%slhvar, %Shockvar + "_ins", %seq, _
    @str(yr(1)), !vins, !vexa)
endif

'--- multipliers and elasticities
if @instr("AB", %stype) > 0 then
  !isec0 = 1
else
  !isec0 = 2
endif

!v = 0
vector(4) vr
for !isec = !isec0 to 2
  if !isec = 1 then
    %s = "$m change in each variable " _
      + "after an initial increase in " _
      + %sbn + " " + %Shocknam + " by $1b"
  else
    !ir = !ir + 1
    %s = "per cent change in each variable " _
      + "after an initial increase in " _
      + %sbn + " " + %Shocknam + " by 1 " + %spc 
  endif
  !ir = !ir + 1
  setcell(tRes, !ir, 1, %s, "l")
  tRes.setmerge(!ir,1,!ir,6)
  
  !qfirst = 1
  '--- year 1 diff for shocked variable
  '--- ex post difference is ok for exogenous variable
  if !vexa = na then
    call zDiff(%Shockvar, %stype, %Alias, %Compare, %sref, _
      !vins, @str(yr(1)), vr)
  '--- ex ante difference is used for endogenous variable
  else
    call zSDiff(%slhvar, %stype, %Compare, %sref, _
      !vexa, @str(yr(1)), vr)
  endif
  '--- multipliers use absolute difference
  if !isec = 1 then
    !vsdiff = vr(3)
  '--- elasticities use % difference
  else
    !vsdiff = vr(4)
  endif
  '--- impact lines
  %tl = @trim(%tlImpact)
  %b0 = "-"
  %var0 = "-"
  !qofwhich = 0
  while %tl <> ""
    call Token(%tl, ";", %varn)
    call Token(%varn, ":", %ivar)
    call zVarType(%ivar, %type, %pc, %vref)
    '--- multiplier or elasticity is available
    if (!isec = 1 and @instr("AB", %type) > 0) _
        or (!isec = 2 and !isec0 = 2) _
        or (!isec = 2 and @instr("AB", %type) = 0) then
      !ir = !ir + 1
      '--- determine the variable and bloc ref
      %b = @upper(%ivar)
      call Token(%b, "_", %var)
      %bref = "_" + %b
      call BlocName(%b, %bn)
      '--- world impacts
      if %b = "W" then
        if %b <> %b0 then
          !ir = !ir + 1
          setcell(tRes, !ir, 1, "  world", "l")
        endif
        %s = %varn
        if !isec = 2 then
          %s = %s + " (" + %pc + ")"
        endif
        setcell(tRes, !ir, 2, %s, "l")
      '--- local impacts
      else if %b = %sb then
        if %b <> %b0 then
          !ir = !ir + 1
          setcell(tRes, !ir, 1, "  local", "l")
        endif
        %s = %varn
        if !isec = 2 then
          %s = %s + " (" + %pc + ")"
        endif
        setcell(tRes, !ir, 2, %s, "l")
      else                  
        '--- spillover header for multipliers
        if !qfirst then
          if !isec = 1 then
            setcell(tRes, !ir, 1, "  spillovers", "l")
            tRes.setmerge(!ir,1,!ir,6)
          endif  
          !qfirst = 0
          %var0 = "-"
        endif
        '--- variable header
        if %var <> %var0 then
          !ir = !ir + 1
          '--- elasticity heading for variable
          if !isec = 2 then
            %s = %varn + " spillover (" + %pc + ")"
            setcell(tRes, !ir, 1, "  " + %s, "l")
            tRes.setmerge(!ir, 1, !ir, 6)
            !ir = !ir + 1
          '--- other blocs multiplier
          else
            setcell(tRes, !ir, 1, "    " + %varn, "l")
            setcell(tRes, !ir, 2, "total", "l")
            for !j = 1 to !nyr
              !v = 0
              for !i = 1 to nBloc
                %bb = @upper(tBloc(!i, 1))
                if %bb <> %sb then
                  %v = %var + "_" + %bb
                  %vrf = %vref
                  if %vrf <> "" then %vrf = %vref + "_" + %bb endif
                  call zDiff(%v, %type, %Alias, %Compare, _
                    %vrf, !vins, @str(yr(!j)), vr)
                  !v = !v + vr(3)
                endif
              next  
              call zRatio(1000*!v, !vsdiff, na, !v)
              %s = @str(!v, "ft.0")
              setcell(tRes, !ir, !j+2, %s, "r")
            next
            !qofwhich = 1
            !ir = !ir + 1
          endif
        endif
        '--- bloc multiplier or elasticity  
        if !qofwhich then
          setcell(tRes, !ir, 1, "        of which", "l")
          !qofwhich = 0
        endif  
        setcell(tRes, !ir, 2, %bn, "l")
      endif
      endif
      %b0 = %b
      %var0 = %var
      '--- write multipliers or elasticities         
      for !j = 1 to !nyr
        %vrf = %vref
        if %vrf <> "" then %vrf = %vref + %bref endif
        call zDiff(%ivar, %type, %Alias, %Compare, %vrf, _
          !vins, @str(yr(!j)), vr)
        if !isec = 1 then
          call zRatio(1000*vr(3),!vsdiff, na, !v)
          %s = @str(!v, "ft.0")
        else  
          call zRatio(vr(4), !vsdiff, na, !v)
          %s = @str(!v, "f.3")
        endif  
        setcell(tRes, !ir, !j+2, %s, "r")
      next
    endif
  wend
next
nRes = !ir
endsub

subroutine zImpactGraphs(string %Shockvar, _
  string %tlImpact, string %Title, _
  string %Alias, string %Compare, _
  string %First, string %Predict)
'==============================================================
'create graphs describing scenario results
'
' Call: %Shockvar  shocked variable
'       %tlImpact  list of impact expressions
'       %Title     title for the table
'       %Alias     scenario alias with _ if relevant
'       %Compare   baseline alias with _ if relevant
'       %First     first year of shock
'       %Predict   last solution year
'
' Ret:  
'
'---------------------------------------------------------------

'--- display 10 prior years
%lib_s = @str(@val(%First)-10)
smpl %lib_s %Predict

%lib_tl = %Shockvar + " " + %tlImpact
call Token(%lib_tl, " ", %lib_obj)
!lib_nobj = 0
while %lib_obj <> ""
  call ScenarioExpr(%lib_obj, %Alias, %lib_s)
  if @isobject(%lib_s) then
    !lib_nobj = !lib_nobj+1
    call ScenarioExpr(%lib_obj, %Compare, %lib_s1)
    graph lib_gr.line {%lib_s} {%lib_s1}
    %lib_t = %Title + "\n\n" + " on " + %lib_obj _
      + ": baseline (red),  shocked (blue)"
    lib_gr.addtext(t, font=10) {%lib_t}
    lib_gr.legend -display
    pageselect graphs
    %lib_g = "grsh_" + %Shockvar + "_" + %lib_obj
    copy data\lib_gr {%lib_g}
    pageselect data
    delete lib_gr
  endif  
  call Token(%lib_tl, " ", %lib_obj)
wend
delete {%lib_gr}_*

endsub

subroutine local zRatio(scalar v1, scalar v2, scalar valt, _
  scalar vres)
'==================================================
'return ratio or alternate value
'
' Call: v1    numerator
'       v2    divisor
'       valt  alternate value
'
' Ret:  vres  ratio or alternate value
'
'---------------------------------------------------------------
if v2 = NA or @abs(v2) < 1e-10 then
  vres = valt
else
  vres = v1/v2
endif  
endsub

subroutine zVarType(string %Var, string %Type, string %PerCent, _
  string %Ref)
'==================================================
'Return variable type and reference
'
' Call: %Var  variable to look up
'
' Ret:  %Type     variable type (default to A)
'       %PerCent  percentage reference text
'       %Ref      reference variable for scaling (blank if none)
'
' Note: see LoadVarTypes for descriptions of var types
'
'---------------------------------------------------------------
%Type = "A"
%lib_r = ""
%PerCent = "%"
%Ref = ""
!lib_i = @instr(%Var, "_")
if !lib_i > 0 then
  %lib_v = @left(%Var, !lib_i-1)
  for !lib_i = 1 to nVType
    if t_VType(!lib_i, 1) = @upper(%lib_v) then
      %Type = t_VType(!lib_i, 2)
      %lib_r = t_VType(!lib_i, 3)
      exitloop
    endif  
  next
endif
if %type = "B" then
  %PerCent = "% of GDP"
  if @instr(%Var, "0") > 0 then
    %Ref = "V0"
  else
    %Ref = "V"
  endif  
else if %type = "G" then
  %PerCent = "per cent"
else if %type = "R" and %lib_r <> "" then
  call Token(%lib_r, " ", %Ref)
  %PerCent = "% of " + %lib_r
else if %type = "R" then
  %PerCent = "% difference"
endif
endif
endif
endif

endsub

subroutine local zLoadVarTypes(string %List, table t, scalar n)
'==================================================
'Load var type list into a table
'
' Call: %List  list of variables by type
'       t      table to load
'       n      number of rows in the table
'
' Ret:  t  table with one row for each variable
'       n  number of rows
'
' Note: the table has columns
'        1 var
'        2 type code
'        3 reference (for type code R)
'
' The following type codes are defined for use in presenting
' results of shock simulations
'   A  aggregate: diff = ys-y0, %diff = 100*(ys/y0-1)
'   B  balance: diff = ys-y0, %diff = 100*(ys-y0)/GDP
'   G  growth or inflation rate: %diff = ys-y0
'   I  index: %diff = 100*(ys/y0-1)
'   R  ratio: %diff = 100*(ys-y0), %diff = 100*(ys-y0)/ref
'
' Var type lists are declared in the following format:
'
' (type) var,var,...;(type) var,var,...;(R) var(ref name), var(ref name)...
'
'---------------------------------------------------------------
%tl = %List
call Token(%tl, ";", %trow)
while %trow <> ""
  call Token(%trow, "(", %a)
  call Token(%trow, ")", %a)
  call Token(%trow, ",", %s)
  while %s <> ""
    if %a = "R" then
      call Token(%s, "(", %v)
      call Token(%s, ")", %ref)
    else
      %v = %s
      %ref = ""  
    endif
    n = n + 1
    t(n, 1) = @upper(%v)
    t(n, 2) = %a
    t(n, 3) = %ref
    call Token(%trow, ",", %s)
  wend 
  call Token(%tl, ";", %trow)
wend
endsub

subroutine Add2Series(string %Var, string %Val, _
  string %First, string %Last)
'==============================================================
'add a scalar to a series
'
' Call: %Var    series name
'       %Val    scalar as string
'       %First  from
'       %Last   to
'
' Ret:  
'
'---------------------------------------------------------------
smpl %First %Last
{%Var} = {%Var} + ({%Val})
endsub

subroutine local zYears(string %Act, string %Pred, _
  vector yr, scalar ny, scalar width)
'==============================================================
'select years for impact reports
'
' Call: %Act    last actual year
'       %Pred   last predicted year
'
' Ret:  yr      vector with years
'       ny      no of years
'       width   width of data columns
'
'---------------------------------------------------------------
yr(1) = @val(%Act) + 1
yr(2) = yr(1) + 1
yr(4) = @val(%Pred)
if yr(4) < yr(2) then
  ny = 1
  width = 20
else if yr(4) = yr(2) then
  ny = 2
  width = 30
else
  if yr(4) = yr(2) + 1 then
    ny = 3
    yr(3) = yr(4)
    width = 20
  else  
    !n = @round((yr(4)-yr(2))/2)
    yr(3) = yr(2) + !n
    ny = 4
    width = 15
  endif
endif
endif
endsub

subroutine local zDiff(string %Var, string %Type, string %Alias, _
  string %Comp, string %Ref, scalar vShock, string %Year, _
  vector vr)
'==============================================================
'return differences vector
'
' Call: %Var    variable
'       %Type   variable type
'       %Alias  alias for shocked values
'       %Comp   alias for baseline values
'       %Ref    reference variable for scaling
'       vShock  size of shock
'       %Year   year to evaluate
'
' Ret:  vr      differences vector
'                 column 1 shocked value
'                 column 2 baseline value
'                 column 3 difference
'                 column 4 % difference
'
'---------------------------------------------------------------
!qfnd = 0
!v = 0
'--- name of shocked variable
call ScenarioExpr(%Var, %Alias, %v)
'--- if the variable exists with shock alias get its shocked
'    value vr(1) and baseline value vr(2)
call Exists(%v, !qfnd)
if !qfnd then
  call Eval(%v, %Year, !v)
  vr(1) = !v
  call ScenarioExpr(%Var, %Comp, %v)
  call Eval(%v, %Year, !v)    
  vr(2) = !v
'--- if the shocked variable does not exist with alias
'    get the actual value of the variable or related _ins
'    actual in vr(1) and actual minus shock in vr(2)
else
  '--- check use of suffix ins
  %v = %Var + "_ins"
  call Exists(%v, !qfnd)
  if not !qfnd then
    %v = %Var
  endif
  '--- get values
  call Eval(%v, %Year, !v)
  vr(1) = !v
  vr(2) = !v - vShock
endif   
'--- differences
call zDDiff(%Type, %Comp, %Ref, %Year, vr)
endsub

subroutine local zSDiff(string %Var, string %Type, _
  string %Comp, string %Ref, scalar vExa, string %Year, _
  vector vr)
'==============================================================
'return ex ante differences vector for shocked variable
'
' Call: %Var    variable
'       %Type   variable type
'       %Comp   alias for reference
'       %Ref    reference variable for type B
'                this will be bloc or world GDP
'       vExa    size of ex ante shock or na
'       %Year   year to evaluate
'
' Ret:  vr      differences vector
'                 column 1 shocked value
'                 column 2 actual value
'                 column 3 difference
'                 column 4 % difference
'
'---------------------------------------------------------------
!v = 0
call Eval(%Var, %Year, !v)
vr(2) = !v
vr(1) = vExa
'--- differences
call zDDiff(%Type, %Comp, %Ref, %Year, vr)
endsub

subroutine local zDDiff(string %Type, string %Comp, _
  string %Ref, string %Year, vector vr)
'==============================================================
'return differences vector for shocked variable
'
' Call: %Type   variable type
'       %Comp   alias for reference
'       %Ref    reference variable for scaling
'       %Year   observation for which diffs are calculated
'       vr      differences vector
'                 column 1 shocked value
'                 column 2 baseline value
'
' Ret:  vr      differences vector
'                 column 3 difference
'                 column 4 % difference
'
'---------------------------------------------------------------
!v = 0
!v1 = 0
'--- difference (scenario - baseline)
vr(3) = vr(1) - vr(2)
'--- % differences
if %Ref <> "" then
  call ScenarioExpr(%Ref, %Comp, %v)
  call Eval(%v, %Year, !v1)
  call zRatio(vr(3), !v1, 0, !v)
  vr(4) = 100*!v
else if %Type = "A" then
  call zRatio(vr(1), vr(2), 1, !v)
  vr(4) = 100*(!v-1)
else if %Type = "G" then
  vr(4) = vr(3)
else if %Type = "I" then
  call zRatio(vr(1), vr(2), 1, !v)
  vr(4) = 100*(!v-1)
else
  vr(4) = 100*vr(3)
endif
endif
endif
endif
endsub

subroutine local zSVarType(string %Shockvar, string %Type, _
  string %PC, string %Ref, string %Eq, string %Var, string %Bloc, _
  string %LHVar)
'==============================================================
'return settings for shocked variable
'
' Call: %Shockvar    shocked variable
'
' Ret:  %Type   variable type
'       %PC     percent change label
'       %Ref    reference variable for scaling
'       %Eq     equation if endogenous or blank otherwise
'       %Var    root name of the variable
'       %Bloc   bloc name or blank
'       %LHVar  name of dependent variable in equation
'               (same as %Shockvar)
'
'---------------------------------------------------------------
call zVarType(%Shockvar, %Type, %PC, %Ref)

'--- default values
%Eq = ""
%LHVar = %Shockvar

!i = @instr(%Shockvar, "_")
'--- bloc variable
if !i > 0 then
  %Bloc = @mid(%Shockvar, !i+1)
  %Var = @left(%Shockvar, !i-1)
  if %Ref <> "" then %Ref = %Ref + "_" + %Bloc endif
  '--- check whether the variable is endogenous
  if %Var <> "sxm" then
    !ieq = 0
    call FindEq(%Shockvar, !ieq)
    if !ieq >0 then
      %LHVar = %Shockvar
      !qendog = 1
      %Eq = "p_" + %Var
    endif
  endif  
'--- world variable
else
  %Bloc = ""
  %Var = %Shockvar
  if %Ref <> "" then %Ref = %Ref + "W" endif
  '--- check whether the variable is endogenous
  %Eq = "eq_" + %Shockvar
  !q = 0
  call Exists(%Eq, !q)
  if not !q then %Eq = "" endif
endif
endsub

subroutine ExAnteValue(string %Var, string %Ins, string %Eq, _
  string %Year, scalar vShock, scalar vExa)
'==============================================================
'return the value of a variable when shocked with no feedbacks
'
' Call: %Var    dependent variable
'       %Ins    instrument to which shock is applied
'       %Eq     the equation
'       %Year   solution year
'       vShock  size of shock
'
' Ret:  vExa    value of dependent variable when its actual
'               value is shocked
'
'---------------------------------------------------------------
smpl %Year %Year
model m_ea
m_ea.append :{%Eq}
m_ea.scenario(n,a=ea) ex ante
m_ea.addassign {%Var}
m_ea.override {%Var}_a
m_ea.addinit(v=n,s=o) {%Var}
{%Ins} = {%Ins} + vShock
m_ea.solve
{%Ins} = {%Ins} - vShock
vExa = @elem({%Var}_ea, %Year)
endsub
