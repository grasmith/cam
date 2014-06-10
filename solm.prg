'PROGRAM: solm.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6                            
'
' multiplier analysis summary reports
'
' use shock.prg to run free-style detailed analyses
'
' run these programs after creating the baseline with sol0.prg
'
' this program generates shocks for blocs and variables listed
' below and reports the local and global impact and spillovers
' to selected blocs. Each shock is applied and reported separately.
'
' the program reads SOL0.wf1 and writes SOLM.wf1
'
' results are stored on the table page of SOLM.wf1. Each table
' is named after the shocked var and bloc
'
' WARNING: execution time is around 10 minutes per shocked bloc.
'
' updated: FC 01/10/2010
'
'==================================================================
' OPTIONS
'
' see DEFINITIONS below for lists of variables to be shocked and
' results to be reported

%shockblocs = "US CN EUW AM"

'==================================================================
' PREFACE
'==================================================================
mode quiet
'--- constants and library
include "set.prg"
'--- open the baseline and clean up
open SOL0
pageselect graphs
delete *
pageselect tables
delete *
pageselect data
delete sp_log* *_0p
'--- update settings
call pLog("SOLM PROGRAM v0110")
%wkfile = "SOLM"
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "M"
t_Settings(4,2) = "multipliers"
t_Settings(7,2) = %wkfile
call pCreateScenario(%gm, %alias)

'==================================================================
' DEFINITIONS
'==================================================================

'--- shock size relative to historical equation residuals
'    (percentage point on normal distribution)
%probability = "95"

%shockvars = "G:govt exp on goods & services;" _
  + "YG:govt revenue (net of grants & subs);" _
  + "is:short-term interest rate;" _
  + "rxu:real exchange rate;" _
  + "MM$:imports of manufactures;" _
  + "ED:energy use;" _
  + "NIMU:net migration;" _   
  + "pvi:inflation;" _
  + "pkp:real asset prices;" _
  + "im:bond rates;" _
  + "wln(NFI):abnormal loan write-offs;" _
  + "IP:private investment"

'NB: if abnormal loan write-offs, wln ,are included in the list of
'variables to be shocked, the reference (NFI) must be provided to
'specify the source for size of shocks.

%ownimpact = "C:consumption;G:government spending;IP:investment;" _
  + "IV:inventories;V:GDP;YG:govt net revenue;LG:government debt;" _
  + "CA$:current account;" _
  + "pvi:cost inflation;pi:price inflation;pkp:real asset prices;" _
  + "rx:real exchange rate;rxna:nominal change in $ rate;" _
  + "is:interest rates;im:bond rates;irs:real interest rates;" _
  + "NER:employment rate;NIM:net migration;NE:employment;" _
  + "irm:real bond rates;"

%otherimpact = "V:GDP;CA$:current account;pi:inflation"
%blocimpact = "EUN EUW EUE EUS UK US JA CN WA AFS"
%worldimpact = "VV_W:GDP;X$_W:exports;pe_w:oil price;NE_W:employment;" _
  + "pi$_w:dollar inflation;pi_w:domestic inflation"

'==================================================================
' PROCESSING
'==================================================================

'--- evaluation period
%actual = t_Settings(1,2)
%first = @str(@val(%actual) + 1)
%predict = @str(@val(%actual) + 6)
if @val(%predict) > @val(t_Settings(2,2)) then
  %predict = t_Settings(2,2)
endif

'--- include baseline rules (if any) in the model
call AddRulesToModel(t_Rule, nRule, {%gm}, "", %actual, %predict)

'--- copy baseline values for the first year to actuals
'    these will be used for calculation of ex ante
'    shock values
call CopyAliasValues("_0", "", %first, %first)
smpl %start %end

'--- shocks for one bloc at a time
call Token(%shockblocs, " ", %shockb)
while %shockb <> ""
  '--- impact list
  %tlImpact = ""
  %tl = %ownimpact
  call Token(%tl, ";", %vn)
  while %vn <> ""
    call zAddImpact(%tlImpact, %vn, "_" + %shockb)
    call Token(%tl, ";", %vn)
  wend
  %tl = %worldimpact
  call Token(%tl, ";", %vn)
  while %vn <> ""
    call zAddImpact(%tlImpact, %vn, "")
    call Token(%tl, ";", %vn)
  wend
  %tl = %otherimpact
  call Token(%tl, ";", %vn)
  while %vn <> ""
    %tlb = %blocimpact
    call Token(%tlb, " ", %b)
    while %b <> ""
      if %b <> %shockb then
        call zAddImpact(%tlImpact, %vn, "_" + %b)
      endif
      call Token(%tlb, " ", %b)
    wend
    call Token(%tl, ";", %vn)
  wend
  '--- shock one variable at a time
  %tlshockv = %shockvars
  call Token(%tlshockv, ";", %shockvn)
  while %shockvn <> ""
    call Token(%shockvn, ":", %shockv)
    call Token(%shockv, "(", %v)
    if %shockv <> "" then
      call Token(%shockv, ")", %shockref)
    else
      %shockref = %v
    endif
    %shockref = %shockref + "_" + %shockb
    %shockvar = %v + "_" + %shockb
    %shockins = %shockvar + "_ins"
    if not @isobject(%shockins) then
      %shockins = %shockvar
    endif
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
      t_Settings(8,2) = "SHOCK: " + %s + ":" + %shockvn _
        + ":" + %probability
  '--- report the shock size and effects
      call pShockImpact(%tlImpact, 0, 0)
  '--- remove the shock
      call Add2Series(%shockins, @str(-!vshock), %first, %first)
    endif
    call Token(%tlshockv, ";", %shockvn)
  wend
  call Token(%shockblocs, " ", %shockb)
wend
smpl %start %end
call pEnd

subroutine local zAddImpact(string %tl, string %vn, string %bref)
'==============================================================
'add an impact variable to a list
'
' Call: %tl     list
'       %vn     variable + name
'       %bref   bloc with _ or blank for world variable
'
' Ret:  %tl     extended list
'
'---------------------------------------------------------------
%vvn = %vn
call Token(%vvn, ":", %v)
%tl = %tl + %v + %bref + ":" + %vvn + ";"
endsub