'PROGRAM: solN1.prg    Copyright (C) 2013,2014 Alphametrics Co. Ltd.
'
' CAM version 5.1   EUR variant
'
' growth and employment targets
'
' The program reads SOL0.wf1 and creates SOLN1.wf1
'
' updated: FC 11/01/2014
'
'==================================================================
' OPTIONS
'==================================================================
'include "set"
'call solN1
'------------------------------------------------------------------
subroutine solN1

%actual = "2014"

%graphs = "Yes"
%graphcomp = "No"
%markets = "No"
%tables = "No"
%analysis = "No"
%csv = "No"

'==================================================================
' PREFACE
'==================================================================
mode quiet
%wkfile = "SOLN1"
call CreateModelFile("SOL0", %wkfile) 
delete m_wm0 *_0p

'--- update settings
call pLog("SOLN1 PROGRAM v1101")
t_Settings(3,2) = "N1"
t_Settings(4,2) = "Growth and employment targets"
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end
'--- participation targets
for %b EUN DE EUW UK FR IT ES EUS PL EUE
  call Floor("NLNVM_"+%b, "NLVM_"+%b+"/(NVM_"+%b _
    +"+0.2*NOM_"+%b+")","0.64",1,10)
  call Floor("NLNVF_"+%b, "NLVF_"+%b+"/(NVF_"+%b _
    +"+0.2*NOF_"+%b+")","0.50",1,10)
  call Floor("NLNYM_"+%b, _
    "NLYM_"+%b+"/NYM_"+%b,"0.55",1,10)
  call Floor("NLNYF_"+%b, _
    "NLYF_"+%b+"/NYF_"+%b,"0.55",1,10)
next

'--- unemployment target: within 1% of lowest rate in 
'    the last 20 years, but not exceeding 8%
for %b EUN DE EUW UK FR IT ES EUS PL EUE
  series NUL_{%b}_TAR = @min(NUL_{%b}, "1993 2012")+1
  NUL_{%b}_TAR = @iif(NUL_{%b}_TAR > 8, 8, NUL_{%b}_TAR)
  call Ceiling("NULVM_"+%b,"100*NU_"+%b+"/NL_"+%b, _
    "NUL_"+%b+"_TAR",10,20)
  call Link("NULVF_" + %b, "NULVM_" + %b, 1)
  call Link("NULYM_" + %b, "NULVM_" + %b, 1)
  call Link("NULYF_" + %b, "NULVM_" + %b, 1)
next

'--- GDP growth targets 2030 productivity
'    measured relative to labour supply
'    German productivity grows by 1.5% pa
'    other countries need to attain the same level
'    maximum growth 4% pa
!v = (1.015^16)*@elem(VV_de,"2014")/@elem(NL_de,"2014")
for %b EUN DE EUW UK FR IT ES EUS PL EUE
  !v1 = @elem(VV_{%b},"2014")/@elem(NL_{%b},"2014")
  !v1 = 100*(exp(log(!v/!v1)/16)-1)
  if !v1 > 4 then !v1 = 4 endif
  %s = @str(!v1)
  call Target("G_"+%b, "@pc(VV_"+%b+"/NL_"+%b+")", %s, 50, 100) 
next
 
call Limit (100, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub
