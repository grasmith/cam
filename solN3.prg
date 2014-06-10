'PROGRAM: solN3.prg    Copyright (C) 2013,2014 Alphametrics Co. Ltd.
'
' CAM version 5.1  EUR variant
'
' EU investment program
'
' The program reads SOLN2.wf1 and creates SOLN3.wf1
'
' updated: FC 11/01/2014
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call solN3
'------------------------------------------------------------------
subroutine solN3

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
%wkfile = "SOLN3"
call CreateModelFile("SOLN2", %wkfile) 
delete m_wmN2 *_0

'--- update settings
call pLog("SOLN3 PROGRAM v1101")
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "N3"
t_Settings(4,2) = "EU investment program"

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end

'---phasing-in
series eudum = 1
smpl %actual+1 %actual+3
eudum.fill(s) 0.2, 0.5, 0.8
smpl %actual+1 %end

'--- European investment stimulus
m_wmn3.append @identity IP_GAP_eu = _
  -0.6*log(VVN_eu(-1)/VVN_eu_n1(-1))
m_wmn3.append @identity IP_eu_ins = _
  + @iif(IP_GAP_eu>0,IP_GAP_eu,0)

for %b EUN DE EUW UK FR IT ES EUS PL EUE
'--- participation targets
  call Floor("NLNVM_"+%b, "NLVM_"+%b+"/(NVM_"+%b _
    +"+0.2*NOM_"+%b+")","0.64",1,10)
  call Floor("NLNVF_"+%b, "NLVF_"+%b+"/(NVF_"+%b _
    +"+0.2*NOF_"+%b+")","0.50",1,10)
  call Floor("NLNYM_"+%b, _
    "NLYM_"+%b+"/NYM_"+%b,"0.55",1,10)
  call Floor("NLNYF_"+%b, _
    "NLYF_"+%b+"/NYF_"+%b,"0.55",1,10)
'--- investment target:
  %s = "IP_GAP_"+%b+" =" _
    + "-0.6*log(VVN_"+%b+"(-1)/VVN_"+%b+"_n1(-1))"
  m_wmn3.append @identity {%s}
  %s = "IP_"+%b+"_ins = eudum*(IP_eu_ins + " _
    + "@iif(IP_GAP_"+%b+">0,IP_GAP_"+%b+",0))"
  m_wmn3.append @identity {%s}
'--- location policy
  %s = "sxmu_"+%b+"_ins = 0.7*sxmu_"+%b+"_ins(-1)" _
    + "+0.3*0.5*(IP_"+%b+"_ins-IP_eu_ins)"
  m_wmn3.append @identity {%s}
next

call Limit (95, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub
