'PROGRAM: shock.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6
'
' multiplier analysis (free-style with detail)
'
' use this program to investigate multiplier effects and
' generate a detailed report
'
' use solm.prg to generate summary reports
' 
' run these programs after creating the baseline with sol0.prg
'
' this program generates a one-time shock for one variable (shockvar)
' and reports the impact on other variables (impacton).
'
' Note: you can use impact.prg to display results for additional
' variables without recalculating the shock scenario
'
' the program reads SOL0.wf1 and writes SHOCK.wf1
'
' tables and charts of shock impacts are stored in sp_log
'
' updated: NK 08/05/2011
'
' NB: the shockvar must be a behavioural variable of the model
' or an exogenous variable. If it is an exogenous variable you
' must provide the name of a behavioural variable in brackets
' immediately after the name of the variable to be shocked.
' The behavioural variable will be used to determine the size
' of shock.
'
' e.g.  %shockvar = "wln_EU(NFI_EU)"
'
' where wln_EU represents the rate of abnormal loan write-off
' and NFI represents the covered loan position of banks.
'
'==================================================================
' OPTIONS
'==================================================================

%shockvar = "G_US"
%impacton = "YP_US C_US IP_US IV_US V_US Y_US" _
  + " X$_US M$_US TB$_US CA$_US rx_US NE_US NIM_US pkp_US" _
  + " pvi_US pi_US is_US im_US irs_US irm_US"
%graphs = "Yes"

'==================================================================
' PREFACE
'==================================================================
mode quiet
'--- constants and library
include "set.prg"
'--- open the baseline and clean up
open SOL0
pageselect tables
delete *
pageselect graphs
delete *
pageselect data
delete sp_log* t_Rule* nRule* *_0p
'--- update settings
call pLog("SHOCK PROGRAM v0110")
%wkfile = "SOLS"
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "S"
t_Settings(4,2) = "shock analysis"
t_Settings(7,2) = %wkfile
call pCreateScenario(%gm, %alias)

'==================================================================
' DEFINITIONS
'==================================================================

'--- shock size relative to historical equation residuals
'    (percentage point on normal distribution)
%probability = "95"

'==================================================================
' PROCESSING
'==================================================================

!qgraph = @upper(@left(%graphs,1)) = "Y"

'--- evaluation period
%actual = t_Settings(1,2)
%first = @str(@val(%actual) + 1)
%predict = @str(@val(%actual) + 6)
if @val(%predict) > @val(t_Settings(2,2)) then
  %predict = t_Settings(2,2)
endif

'--- copy baseline values for the first year to actuals
'    these will be used for calculation of ex ante
'    shock values
call CopyAliasValues("_0", "", %first, %first)
smpl %start %end

'--- define the shocked scenario
{%gm}.scenario(n,a=_S) shocked

'--- check for a reference variable in brackets
call Token(%shockvar, "(", %v)
if %shockvar <> "" then
  call Token(%shockvar, ")", %shockref)
else
  %shockref = %v
endif
%shockvar = %v
%shockins = %shockvar + "_ins"
'--- check whether an _ins series exists
if not @isobject(%shockins) then
  %shockins = %shockvar
endif
'--- determine the shock size
!iEq = 0
call FindEQ(%shockref, !iEq)
if !iEQ = 0 then
  call pLog( %shockref + _
   + " is not defined as a behavioural variable")
else 
'--- write a header
  call pLog("imposing a " + %probability + "% shock" _
    + " on " + %shockvar + " in " + %first)
'--- impose the shock
  !vshock = 0
  call CalcShock(%shockref, %probability, !vshock)
  call Add2Series(%shockins, @str(!vshock), %first, %first)
'--- solve the model   
  call pLog("solving for " + %first + "-" + %predict)
  call SolveModel({%gm}, "o=g,g=n,m=10000",%actual, %predict)
  %s = %shockvar
  if %shockref <> %shockvar then
    %s = %s + "(" + %shockref + ")"
  endif
  t_Settings(8,2) = "SHOCK: " + %s + "::" + %probability
'--- report the shock size and effects
  call pShockImpact(%impacton, 1, !qgraph)
endif
smpl %start %end
call pEnd

