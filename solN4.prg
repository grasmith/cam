'PROGRAM: solN4.prg    Copyright (C) 2013,2014 Alphametrics Co. Ltd.
'
' CAM Version 5.1   EUR variant
'
' EU recovery and convergence programme
'
' The program reads SOLN3.wf1 and creates SOLN4.wf1
'
' updated: FC 11/01/2014
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call solN4
'------------------------------------------------------------------
subroutine solN4

%actual = "2014"

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
%wkfile = "SOLN4"
call CreateModelFile("SOLN3", %wkfile) 
delete m_wmN3

'--- update settings
call pLog("SOLN4 PROGRAM v1101")
t_Settings(5,2) = "N2"
t_Settings(6,2) = "Struggling on"
t_Settings(3,2) = "N4"
t_Settings(4,2) = "EU convergence"

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
m_wmn4.append @identity IP_GAP_eu = _
  -0.6*log(VVN_eu(-1)/VVN_eu_n1(-1))
m_wmn4.append @identity IP_eu_ins = _
  + @iif(IP_GAP_eu>0,IP_GAP_eu,0)

for %b EUN DE EUW UK FR IT ES EUS PL EUE
'--- govt transfer equal to 50% of needs/resource gap
'    in prior year
'  target on EU standard resource standard
'    g_?/ndep_? = 0.38 x VVN_eu
'    where ndep = N - 0.5 x NV
'  needs: ndep_? x 0.38 x VVN_eu
'  total needs: ndep_eu/n_eu x 0.38 x VV_eu
'  total cost shared per GDP
'  own resources: ndep_eu/n_eu x 0.38 x VV_?
'  gap: 0.38 ndep_eu(ndep_?/ndep_eu - VV_?/VV_eu)VVN_eu
  %s = "(N_eu(-1)-0.5*NV_eu(-1))"
  %s = "G_GAP_"+%b+" = 0.38*"+%s _
    +"*((N_"+%b+"(-1)-0.5*NV_"+%b _
    +"(-1))/"+%s+"-VV_"+%b+"(-1)/VV_eu(-1))*VVN_eu(-1)"
  m_wmn4.append @identity {%s}
'  EU pays 50% of the difference
  %s = "NLGADJ_"+%b+" = " _
    + "eudum*0.5*@iif(G_GAP_"+%b+">0,G_GAP_"+%b+",0)"
  m_wmn4.append @identity {%s}
'--- investment target:
  %s = "IP_GAP_"+%b+" =" _
    + "-0.6*log(VVN_"+%b+"(-1)/VVN_"+%b+"_n1(-1))"
  m_wmn4.append @identity {%s}
  %s = "IP_"+%b+"_ins = eudum*(IP_eu_ins + " _
    + "@iif(IP_GAP_"+%b+">0,IP_GAP_"+%b+",0))"
  m_wmn4.append @identity {%s}
'--- location policy
  %s = "sxmu_"+%b+"_ins = 0.7*sxmu_"+%b+"_ins(-1)" _
    + "+0.3*0.5*(IP_"+%b+"_ins-IP_eu_ins)"
  m_wmn4.append @identity {%s}
'--- 40% target for non-bank govt debt
'    additional debt to be financed by banks
  call Ceiling("LGO_"+%b,"LGO_"+%b+"/VV_"+%b,"0.4",0.1,20)
'--- unemployment target: within 1% of lowest rate in 
'    the last 20 years, but not exceeding 8%
  series NUL_{%b}_TAR = @min(NUL_{%b}, "1994 2013")+1
  NUL_{%b}_TAR = @iif(NUL_{%b}_TAR > 8, 8, NUL_{%b}_TAR)
  call Ceiling("NULVM_"+%b,"100*NU_"+%b+"/NL_"+%b, _
    "NUL_"+%b+"_TAR",10,20)
  call Link("NULVF_" + %b, "NULVM_" + %b, 1)
  call Link("NULYM_" + %b, "NULVM_" + %b, 1)
  call Link("NULYF_" + %b, "NULVM_" + %b, 1)
next
  
call Limit (95, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub
