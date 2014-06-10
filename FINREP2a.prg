'PROGRAM: FINREP2a.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.61  AUGUR
'
' presentation graphs for final report
' data from scenarios E2 and E2a
'
' updated: FC 20/09/2012
'
' requires SOLE2a
' uses library routines in zlibp
'
'---------------------------------------------------------------
'
'==================================================================
' INITIALISATION
'==================================================================
mode quiet
tic
include "set"
include "zlibp"
pageselect data
output(s) sp_log
show sp_log

'==================================================================
' PROCESSING
'==================================================================

'--- load data for scenario e1a
'open sole1a
'workfile sole2a
'copy sole1a::data\*_e1a
'close sole1a
  
'--- generate the graphs

'pagecreate(page=E22a) a %start %end
call pGraphE2a("E22a", "2000 2030 2012", _
  "E2;EU break-up:E2a;US-China hegemony:", %blyellow)

'pagecreate(page=E1a2a) a %start %end
call pGraphE2a("E1a2a", "2000 2030 2012", _
  "E1a;reduced government:E2a;EU break-up:", %blyellow)

stop

subroutine pGraphE2a(string %p_Page, string %p_tlYear, _
  string %p_tlScenario, string %p_color)
'======================================================

%page = %p_Page
pageselect {%page}
delete *
pageselect data

%t = %p_tlScenario
call Token(%t, ";", %compare)

%font = "24"
%color = %p_color
if @left(@upper(%p_color),1) = "B" then %color = "" endif

pageselect data
smpl 1970 2030
series tmp_eus_E2a = @cumprod((1+rxna_eus_E2a/100)/ _
 (1+rxna_euw_E2a/100),"2012 2030")
series tmp_eus_e2 = @cumprod((1+rxna_EUS_e2/100)/ _
 (1+rxna_euw_e2/100),"2012 2030")

call SPGraph(%p_Page,"EU_RXNA", _
  "Exchange rate index - South Europe vs West Europe", _
  %color, %font,"EUS",%p_tlScenario,"2012 2030 2012",3, _
  ";0.8,1.0;;tmp_?:" _
  )

call SPGraph(%p_Page,"EU_RX", _
  "Real exchange rates vs W Europe", _
  %color, %font,"EUS EUE UK",%p_tlScenario,"2010 2030 2012",3, _
  ";0.5,0.95;;rx_?/rx_EUW:" _
  )

call SPGraph(%p_Page,"EU_SHOCKS", _
  "Europe: trade and investment ", _
  %color, %font,"",%p_tlScenario,"2007 2030 2012",3, _
  "private investment (% of GDP);0;;IPV_EU:" _
  + "total imports ($2005 billion);0;;M$_EU/1000:" _
  + "intra-trade in manufactures ($2005 billion)" _
  + ";0;;(MM$_EU-MMEX$_EU)/1000:" _
  )

call SPGraph(%p_Page,"EU_IMPACT", _
  "Europe: GDP, income and employment ", _
  %color, %font,"",%p_tlScenario,"2007 2030 2012",3, _
  "GDP growth (% p.a.);;0;@pc(V_EU):" _
  + "income per capita ($2005 pp);25000,35000;;YN_EU:" _
  + "employment rate (% of 15+ population)" _
  + ";44,56;;NER_EU:" _
  )

call SPGraph(%p_Page,"EU_BLOCS", _
  "GDP, income and employment", _
  %color, %font,"EUW EUS EUE UK",%p_tlScenario, _
  "2007 2030 2012",4, _
  "GDP growth (% p.a.);-5,6;0;@pc(V_?):" _
  + "income per capita ($2005 pp);12000,34000;;YN_?:" _
  + "employment rate (% of 15+ population)" _
  + ";40,60;;NER_?:" _
  )
  
call SPGraph(%p_Page,"EU_LGV", _
  "government debt (% of GDP)", _
  %color, %font,"EUS UK EUW EUN EUE", _
  %p_tlScenario,%p_tlyear,3, _
  ";0,;0;100*LG_?/VV_?|60:" _
  )

call SPGraph(%p_Page,"EU_G", _
  "government spending on goods and services(% of GDP)", _
  %color, %font,"EUN EUW EUE UK EUS", _
  %p_tlScenario,%p_tlyear,3, _
  ";15,35;0;100*G_?/VV_?:" _
  )

call SPGraph(%p_Page,"EU_YN", _
  "income per capita ($2005 pp)", _
  %color, %font,"EUN EUW UK EUS EUE", _
  %p_tlScenario,%p_tlyear,3, _
  ";0,50000;;YN_?:" _
  )
  
pageselect data
smpl 1970 2030
series tmp_EUW_E2a = 100*YN_EUW_E2a/YN_EUW_{%compare}
series tmp_EUS_E2a = 100*YN_EUS_E2a/YN_EUS_{%compare}
series tmp_EUE_E2a = 100*YN_EUE_E2a/YN_EUE_{%compare}
series tmp_EUN_E2a = 100*YN_EUN_E2a/YN_EUN_{%compare}
series tmp_UK_E2a = 100*YN_UK_E2a/YN_UK_{%compare}

call SPGraph(%p_Page,"EU_YNHIT", _
  "Income per capita (scenario " + @upper(%compare) + " = 100)", _
  %color, %font,"single EUS EUW UK EUE EUN","E2a","2012 2030 2012",3, _
  ";;100;tmp_?:" _
  )

call SPGraph(%p_Page,"USCN_CAV$", _
  "Current accounts (% GDP)", _
  %color, %font,"EU US CN",%p_tlScenario,"2007 2030 2012",2, _
  ";-2,6;0;CAV$_?:" _
  )

call SPGraph(%p_Page,"USCN_DV", _
  "GDP growth (% p.a.)", _
  %color, %font,"EU US CN",%p_tlScenario,"2007 2030 2012",2, _
  ";-5,15;0;@pc(V_?):" _
  )

call SPGraph(%p_Page,"USCN_NER", _
  "Employment rate (%)", _
  %color, %font,"EU US CN",%p_tlScenario,"2007 2030 2012",2, _
  ";40,80;0;NER_?:" _
  )

call SPGraph(%p_Page,"USCN_RX", _
  "Real exchange rates: West Europe, the US and China", _
  %color, %font,"EUW US CN",%p_tlScenario,"2007 2030 2012",2, _
  ";0.4,1.8;0;rx_?:" _
  )

call SPGraph(%p_Page,"REG_DV", _
  "Exports and GDP growth by world region (% p.a.)", _
  %color, "36","W EU AF OA EA AM", _
  %p_tlScenario,%p_tlyear,3, _
  "exports;-10,20;0;@pc(X$_?):" _
  + "GDP;-5,10;0;@pc(V_?):" _
  )

call SPGraph(%p_Page,"REG_DXV1", _
  "20-year average growth by world region (% p.a.)", _
  %color, %font,"W EU AF OA EA AM", _
  %p_tlScenario,%p_tlyear,3, _
  "GDP;0,6;;100*(exp(log(V_?/V_?(-20))/20)-1):" _
  + "exports;0,10;;100*(exp(log(X$_?/X$_?(-20))/20)-1):" _
  )

call SPGraph(%p_Page,"W_PAPE", _
  "World commodity prices (index)", _
  %color, %font,"",%p_tlScenario,"2007 2030 2012",2, _
  ";;;pa_w food and raw materials:" _
  + ";;;pe_w energy:" _
  )

pageselect data
smpl 1970 2030
series tmp_AFS_E2a = 100*YN_AFS_E2a/YN_AFS_{%compare}
series tmp_ASO_E2a = 100*YN_ASO_E2a/YN_ASO_{%compare}
series tmp_AMS_E2a = 100*YN_AMS_E2a/YN_AMS_{%compare}
series tmp_AFN_E2a = 100*YN_AFN_E2a/YN_AFN_{%compare}
series tmp_WA_E2a = 100*YN_WA_E2a/YN_WA_{%compare}
series tmp_CI_E2a = 100*YN_CI_E2a/YN_CI_{%compare}
series tmp_IN_E2a = 100*YN_IN_E2a/YN_IN_{%compare}
  
call SPGraph(%p_Page,"W_YNLOMI", _
  "Income per capita (scenario " + @upper(%compare) + " = 100)", _
  %color, %font,"single AFS AFN ASO WA CI AMS IN","E2a","2012 2030 2012",3, _
  ";90,110;0;tmp_?:" _
  )

endsub

