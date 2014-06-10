'PROGRAM: solN2.prg    Copyright (C) 2012,2013 Alphametrics Co. Ltd.
'
' CAM version 5.0
'
' Reflation in Europe with reduced government
'
' The program reads SOLN1.wf1 and creates SOLN2.wf1
'
' updated: FC 30/07/2013
'
' differences from SOLN1
'
' Europe
'  restrictions on national budgets are removed
'  government spending and labour market policies to create jobs
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call solN2
'------------------------------------------------------------------
subroutine solN2

%actual = "2013"

%graphs = "Yes"
%graphcomp = "Yes"
%markets = "No"
%tables = "No"
%analysis = "No"
%csv = "No"

'==================================================================
' PREFACE
'==================================================================
mode quiet
%wkfile = "SOLN2"
call CreateModelFile("SOLN1", %wkfile) 
delete m_wmN1

'--- update settings
call pLog("SOLN2 PROGRAM v3007")
t_Settings(5,2) = "N1"
t_Settings(6,2) = "Struggling on with US China hegemony"
t_Settings(3,2) = "N2"
t_Settings(4,2) = "Reflation in Europe"

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end

'--- budget rules
for %b DE ES FR IT PL UK EUE EUS EUW
'--- unemployment target: within 1% of lowest rate in 
'    the last 20 years, but not exceeding 8%
  series NUL_{%b}_TAR = @min(NUL_{%b}, "1993 2012")+1
  NUL_{%b}_TAR = @iif(NUL_{%b}_TAR > 8, 8, NUL_{%b}_TAR)
  call DropRules("YG_"+%b+" G_"+%b+" IAGO_"+%b)
  call Ceiling("NULVM_"+%b,"100*NU_"+%b+"/NL_"+%b, _
    "NUL_"+%b+"_TAR",10,20)
  call Link("NULVF_" + %b, "NULVM_" + %b, 1)
  call Link("NULYM_" + %b, "NULVM_" + %b, 1)
  call Link("NULYF_" + %b, "NULVM_" + %b, 1)
  call Link("G_" + %b, "NULVM_" + %b, -1)
'--- cancel negative investment impact of austerity policies
  IP_{%b}_ins = 0
next

call Limit (95, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub
