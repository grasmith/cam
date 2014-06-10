'LIBRARY: zlibs.prg     Copyright (C) 2012,2013 Alphametrics Co. Ltd.
'
' CAM Version 5.1
'
' model solution routines
'
' updated: FC 25/04/2013
'
'---------------------------------------------------------------
' pCheckSolveReport(p_m, %p_CheckStart, %p_Prior, %p_Horizon,
'   %p_Solopt, p_Max)
' pCreateScenario (%ModelName, %Alias)
'---------------------------------------------------------------

subroutine pCheckSolveReport(model p_m, string %p_CheckStart, _
  string %p_Prior, string %p_Horizon, string %p_Solopt, _
  scalar p_Max)
'==================================================
'Check rules, solve and report results
'
' Call: p_m            model
'       %p_CheckStart  first year for rule check simulations
'       %p_Prior       last year before start of solution period
'       %p_Horizon     horizon for solution period
'       %p_Solopt      solution options
'                       c convergence criterion (default 1e-8)
'                       m max iterations (default 5000)
'                       v verbose messages (default f false)
'       p_Max          max years to display in rule table
'
' Ret:  
'
'---------------------------------------------------------------

'--- current simulation base year
t_Settings(13,2) = %p_Prior
'--- if not solving for alignment or plain baseline
if not (t_Settings(3,2) = "" or t_Settings(3,2) = "0p") then
'--- first-ever simulation base year (baselines)
  if t_Settings(12,2) = "" then
    t_Settings(12,2) = %p_Prior
  else
  '--- reset actuals from source solution to provide same
  '    initial conditions  
    if t_Settings(12,2) < %p_Prior then
      %first = @str(@val(t_Settings(12,2))+1)
      call CopyAliasValues("_" + t_Settings(5,2), "", %first, _
          %p_Prior)
  '--- or update the first-ever simulation base year
    else
      if %p_Prior < t_Settings(12,2) then
        t_Settings(12,2) = %p_Prior
      endif  
    endif
  endif
endif

'--- check rules and perform shock analysis if required
call pRuleCheck(p_m, t_Rule, nRule, %p_CheckStart)

'--- generate the scenario
%s = t_Settings(4,2)
p_m.scenario() %s
%first = @str(@val(%p_Prior)+1)
call pLog("solving for " + %first + "-" + %p_Horizon)
call AddRulesToModel(t_Rule, nRule, p_m, "", %p_Prior, %p_Horizon)

'--- standard solution options
' g rounding of results: n no rounding (default 7 digits)
' i initialise values: p prior period values (default a actuals)
' o solution method: g gauss-seidel (default b broyden)
' v diagnostics: t trace (default trace)
' z zero small results: n don't zero (default zero if abs<1e-7)
%s = "g=n,i=p,o=b,v=t,z=n"
if %p_Solopt <> "" then %s = %s + "," + %p_Solopt endif
call SolveModel(p_m, %s, %p_Prior, %p_Horizon)

'--- reset actuals from baseline
if t_Settings(12,2) <> "" and t_Settings(12,2) < %p_Prior then
  %first = @str(@val(t_Settings(12,2))+1)
  call CopyAliasValues("_0", "", %first, %p_Prior)
endif

call pLog("listing exogenous variables")
%s = t_Settings(4,2)
t_Settings(8,2) = %s
table t_Exog
call ListExog(t_Exog, %s, p_m, %first, %p_Horizon, p_Max)

call pLog("listing rule outcomes")
call RuleTable("tr", %alias, %first, %p_Horizon, p_Max)

'--- move tables to tables page
pageselect tables
copy data\t_Exog
copy data\tr_*

'--- clean up
pageselect data
delete t_Exog tr_* sgrf_*
smpl %start %end

'--- generate standard outputs

%tlopt = ""
if @upper(@left(%graphs,1)) = "Y" then %tlopt = "G" endif
if @upper(@left(%subgraphs,1)) = "Y" then %tlopt = %tlopt + "S" endif
if @upper(@left(%tables,1)) = "Y" then %tlopt = %tlopt + "T" endif
if @upper(@left(%analysis,1)) = "Y" then %tlopt = %tlopt + "A" endif
if @upper(@left(%csv,1)) = "Y" then %tlopt = %tlopt + "C" endif
if @upper(@left(%markets,1)) = "Y" then %tlopt = %tlopt + "M" endif
!qgraphcomp = @upper(@left(%graphcomp,1)) = "Y"
if %tlopt <> "" then call pReport(%tlopt,!qgraphcomp) endif

endsub

subroutine pCreateScenario (string %ModelName, string %Alias)
'==============================================================
'create a scenario and setup exogenous variables
'
' Call: 
'
' Ret:  %ModelName    model name
'       %Alias        alias with prefix _ added
'
' Note: the model is created by renaming the template m_ds
' loaded by opening the workfile as a copy of MDS.wf1. See
' CreateModelFile in zlib.prg for further details.
'
'---------------------------------------------------------------

%Alias = t_Settings(3,2)
%t = t_Settings(4,2)
call pLog("creating scenario " + %Alias + ": " + %t)

'--- define the rules table
table t_Rule
scalar nRule

'--- preprocessing
%s = t_Settings(7,2)
wfsave {%s}
call pLog("computing " + %t)
call pLog("building the model")
%ModelName = "m_wm" + %Alias
rename m_ds {%ModelName}
{%ModelName}.merge m_wm

'--- create scenario if not baseline
!q0 = %Alias = "0"
if not !q0 then {%ModelName}.scenario(n,a={%Alias}) %t endif
%Alias = "_" + %Alias

'--- overrides for exogenous variables
smpl %start %end
%tl = t_Settings(11,2)                    'list of exogenous variables
while %tl <> ""
  call Token(%tl, " ", %v)
  p_Bloc.genr {%v}_?{%Alias} = {%v}_?
  if not !q0 then
    for !i = 1 to nBloc
      %b = t_Bloc(!i, 1)
      {%ModelName}.override(m) {%v}_{%b}
    next
  endif
wend
endsub
