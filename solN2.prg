'PROGRAM: solN2.prg          Copyright (C) 2013,2014 Alphametrics Co. Ltd.
'
' CAM version 5.1   EUR variant
'
' Long-term impact of the SGP, ESM and TSCG
'
' The program reads SOL1.wf1 and creates SOLN2.wf1
'
' updated: FC 11/01/2014
'
'==================================================================
' OPTIONS
'==================================================================
'include "set"
'call solN2
'------------------------------------------------------------------
subroutine solN2

'--- projection period
%actual = "2014"

%graphs = "Yes"
%graphcomp = "No"
%markets = "No"
%tables = "No"
%analysis = "No"
%csv = "No"

'================================================================
' PREFACE
'==================================================================
mode quiet
%wkfile = "SOLN2"
call CreateModelFile("SOLN1", %wkfile) 
delete m_wmn1

'--- update settings
call pLog("SOLN2 PROGRAM v1101")
t_Settings(5,2) = "0"
t_Settings(6,2) = "Baseline"
t_Settings(3,2) = "N2"
t_Settings(4,2) = "SGP and TSCG"
call pCreateScenario(%gm, %alias)

'==================================================================
' SCENARIO DEFINITIONS
'==================================================================

smpl %actual+1 %end

'--- EU debt ceilings
for %b EUN DE EUW UK FR IT ES EUS PL EUE
  call DropRules("NULVM_"+%b+" NULVF_"+%b+" NULYM_"+%b _
    + " NULYF_"+%b+" G_"+%b)
  call DropRules("NLNVM_"+%b+" NLNVF_"+%b+" NLNYM_"+%b _
    + " NLNYF_"+%b)
  call Ceiling("G_"+%b, "LG_"+%b+"/VV_"+%b, "0.6", 0.1, 10)
  call Link("IAGO_"+%b, "G_"+%b, 0.25)
next

call Limit (95, "ALL")
'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub
