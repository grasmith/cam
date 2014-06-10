'PROGRAM: solN3b.prg          Copyright (C) 2013 Alphametrics Co. Ltd.
'
' CAM version 5.0
'
' Reflation with incomes policy
'
' The program reads SOLN2.wf1 and creates SOLN3b.wf1
'
' updated: FC 30/07/2013
'
' differences from SOLN2
'
' Europe
'  incomes policy
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call solN3b
'------------------------------------------------------------------
subroutine solN3b

'--- projection period
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
%wkfile = "SOLN3b"
call CreateModelFile("SOLN2", %wkfile) 
delete m_wmN2

'--- update settings
call pLog("SOLN3b PROGRAM v0920")
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "N3b"
t_Settings(4,2) = "Incomes policy"

call pCreateScenario(%gm, %alias)

'==================================================================
' SCENARIO DEFINITIONS
'==================================================================

smpl %actual+1 %end

'--- incomes policy
'    Germany increases money wages to maintain inflation at 2%
'    targets for other euro zone member as below
'    in order to adjust real exchange rates vs Germany
call Target("ei_de", "pi_de", "2", 15, 50)
call Ceiling("ei_pl", "pi_pl-pi_de", "0.5", 15, 50)
call Ceiling("ei_euw", "pi_euw-pi_de", "0", 15, 50)
call Ceiling("ei_fr", "pi_fr-pi_de", "-0.5", 15, 50)
call Ceiling("ei_it", "pi_it-pi_de", "-0.5", 15, 50)
call Ceiling("ei_es", "pi_es-pi_de", "-0.5", 15, 50)
call Ceiling("ei_eus", "pi_eus-pi_de", "0", 15, 50)
call Ceiling("ei_eue", "pi_eue-pi_de", "0", 15, 50)
'    NB corporate income is also limited to 60%
for %b de pl euw fr it es eus eue
  call Floor("mu_"+%b, "100*VVEM_"+%b+"/VV_"+%b, "60", -50, 5)
next

call Limit (95, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g", 8)
call pEnd

endsub
