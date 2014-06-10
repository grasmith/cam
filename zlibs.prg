'LIBRARY: zlibs.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' model solution routines
'
' updated: FC 25/04/2011
'
'---------------------------------------------------------------
' pCheckSolveReport(p_m, %p_CheckStart, %p_Horizon, %p_Solopt, p_Max)
' pCreateScenario (%ModelName, %Alias, %Name, %tlExog)
'---------------------------------------------------------------

subroutine pCheckSolveReport(model p_m, string %p_CheckStart, _
  string %p_Horizon, string %p_Solopt, scalar p_Max)
'==================================================
'Check rules, solve and report results
'
' Call: p_m            model
'       %p_CheckStart  first year for rule check simulations
'       %p_Horizon     horizon for solutions
'       %p_Solopt      solution options
'                       c convergence criterion (default 1e-8)
'                       m max iterations (default 5000)
'                       v verbose messages (default f false)
'       p_Max          max years to display in rule table
'
' Ret:  
'
'---------------------------------------------------------------

'--- check rules and perform shock analysis if required
call pRuleCheck(m_wm, t_Rule, nRule, %p_CheckStart)

'--- generate the scenario
%prior = t_Settings(1,2)
%first = @str(@val(%prior)+1)
call pLog("solving for " + %first + "-" + %p_Horizon)
call AddRulesToModel(t_Rule, nRule, p_m, "", %prior, %p_Horizon)
'--- standard solution options
' g rounding of results: n no rounding (default 7 digits)
' i initialise values: p prior period values (default a actuals)
' o solution method: g gauss-seidel (default b broyden)
%s = "g=n,i=p,o=g"
if %p_Solopt <> "" then %s = %s + "," + %p_Solopt endif
call pLog("options " + %s)
call SolveModel(p_m, %s, %prior, %p_Horizon)

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
model {%ModelName}
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
