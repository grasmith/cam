'PROGRAM: sol5.prg    Copyright (C) 2013,2015 Alphametrics Co. Ltd.
'
' CAM Version 6.1   FESSUD variant
'
' China as a global financial centre
'  increase domestic consumption
'  strong Yuan
'  fast growth of China's external assets
'  reduced growth of China's exchange reserves
'
' Note: even with rapid growth, China's share of global
' financial markets remains modest (less than the US,
' Europe and Other East Asia).
'
' External investment might help to protect China's external
' business (industrial exports and energy and commodity
' imports). But the model cannot quantify such effects.
'
' The program reads SOL0.wf1 and creates SOL5.wf1
'
' updated: FC 04/06/2015
'
'==================================================================
' OPTIONS
'==================================================================
include "set"
call sol5
'------------------------------------------------------------------
subroutine sol5

%actual = "2015"

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
%wkfile = "sol5"
call CreateModelFile("SOL0", %wkfile) 
delete m_wm0

'--- update settings
call pLog("sol5 PROGRAM v0406")
t_Settings(5,2) = t_Settings(3,2)
t_Settings(6,2) = t_Settings(4,2)
t_Settings(3,2) = "5"
t_Settings(4,2) = "China financial"

call pCreateScenario(%gm, %alias)

'==================================================================
' RULE DEFINITIONS
'==================================================================

smpl %actual+1 %end

  '--- China: increase consumer spending to 50% of GDP
  call Target("SP_cn","C_cn/VV_cn","0.5",-1,10)
  call Link ("mu_cn","SP_cn", 0.5)

  '--- strong Yuan
  call Target("rxu_cn", "rx_cn", "0.8",1,10)

  '--- very fast growth of China's external investments
  '    reaching 70% of the US level by 2030
  call Target("IADI$_cn","100*AXO$_cn/AXO$_us","70",500,10)
  call Link("IAPI$_cn", "IADI$_cn", 1)
  call Link("NOI$_cn", "IADI$_cn", 0.5)
  '--- reduced growth of China's official reserves
  call Link("R$_cn", "IADI$_cn", -1)

call Limit (95, "ALL")

'==================================================================
' PROCESSING
'==================================================================

call pCheckSolveReport({%gm}, %actual, %actual, %predict, _
  "o=g,mit=5000", 8)
call pEnd

endsub

