'PROGRAM: solm.prg    Copyright 2012,2015 Alphametrics Co.,Ltd.         
'
' CAM version 6.0 utility
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
' WARNING: execution time may be 1 minute or more per shocked bloc.
'
' updated: FC 16/04/2015
'
'==================================================================
' OPTIONS
include "set"
call solm
'------------------------------------------------------------------
subroutine solm
'
' see DEFINITIONS below for lists of variables to be shocked and
' results to be reported

%shockblocs = "US DE BR CN"

'==================================================================
' PREFACE
'==================================================================
mode quiet
%wkfile = "SOLM"
call CreateModelFile("SOL0", %wkfile) 
delete m_wm0 *_0p
'--- update settings
call pLog("SOLM PROGRAM v1604")
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "M"
t_Settings(4,2) = "multipliers"
call pCreateScenario(%gm, %alias)

'==================================================================
' DEFINITIONS
'==================================================================

'--- shock size relative to historical equation residuals
'    (percentage point on normal distribution)
%probability = "95"
  
%shockvars = "G:govt exp on goods & services;" _
  + "YGD:direct government revenue net;" _
  + "IP:private investment;" _
  + "rxu:exchange rate;" _
  + "pvi:inflation;" _
  + "mu:profit markup;" _
  + "NLNVM:male adult labour force participation;" _
  + "NULVM:male adult unemployment;" _
  + "ed:energy demand"

'NB: if abnormal loan write-offs, wln ,are included in the list of
'variables to be shocked, the reference (NFI) must be provided to
'specify the source for size of shocks.

%ownimpact = "G:govt exp on goods & services;" _
  + "YGD:direct government revenue net;" _
  + "IP:private investment;" _
  + "rx:real exchange rate;" _
  + "ei:wage inflation;" _
  + "mu:profit markup;" _
  + "NLNVM:male adult labour force participation;" _
  + "NULVM:male adult unemployment;" _
  + "ed:energy demand;" _
  + "C:consumption;" _
  + "V:GDP;" _
  + "NLG:government net lending;" _
  + "LG:government debt;" _
  + "CA$:current account;" _
  + "rxd:US$ exchange rate;" _
  + "pi:price inflation;" _
  + "VVEME:average real earnings;" _
  + "VVPRV:profit share;" _
  + "NIM:net migration;" _
  + "NE:total employment;" _
  + "NUL:unemployment rate;" _
  + "CO2:CO2 emissions"
%otherimpact = "V:GDP;NE:employment;CA$:current account"
%blocimpact = "US DE BR CN EUC EUP RU IN OEA"
%worldimpact = "V_W:GDP;X$_W:exports;" _
  + "pe_w:oil price"

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
while %shockblocs <> ""
  call Token(%shockblocs, " ", %shockb)
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
  while %tlshockv <> ""
    call Token(%tlshockv, ";", %shockvn)
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
      call SolveModel({%gm}, "o=b,g=n,i=p,m=10000,c=1e-8,v=t",%actual, %predict)
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
  wend
wend
smpl %start %end
call pEnd
endsub

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