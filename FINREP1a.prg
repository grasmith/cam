'PROGRAM: FINREP1a.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6  AUGUR
' data from scenarios E1 and E1a
'
' presentation graphs for final report
'
' updated: FC 20/09/2012
'
' requires SOLE1a
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
  
'--- generate the graphs

pagecreate(page=E1a) a %start %end
call pGraphE1a("E1a", "2000 2030 2012", _
  "E1;reduced government:E1a;US-China accommodation:", %blyellow)

stop

subroutine pGraphE1a(string %p_Page, string %p_tlYear, _
  string %p_tlScenario, string %p_color)
'======================================================

%page = %p_Page
pageselect {%page}
delete *
pageselect data

%font = "24"
%color = %p_color
if @left(@upper(%p_color),1) = "B" then %color = "" endif

pageselect data
smpl 1970 2030
series tmp_eus_e1a = @cumprod((1+rxna_eus_e1a/100)/ _
 (1+rxna_euw_e1a/100),"2012 2030")
series tmp_eus_e1 = @cumprod((1+rxna_EUS_e1/100)/ _
 (1+rxna_euw_e1/100),"2012 2030")

call SPGraph(%p_Page,"EU_RXNA", _
  "Exchange rate index - South Europe vs West Europe", _
  %color, %font,"EUS",%p_tlScenario,"2012 2030 2012",3, _
  ";0.9,1.1;;tmp_?:" _
  )

call SPGraph(%p_Page,"EU_RX", _
  "Real exchange rates vs W Europe", _
  %color, %font,"EUS EUE UK",%p_tlScenario,"2010 2030 2012",3, _
  ";0.5,0.95;;rx_?/rx_EUW:" _
  )

call SPGraph(%p_Page,"EU_SHOCKS", _
  "Europe: trade and investment with US-China accommodation ", _
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
  "GDP growth (% p.a.);;0;@pc(V_?):" _
  + "income per capita ($2005 pp);;;YN_?:" _
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
series tmp_EUW_e1a = 100*YN_EUW_e1a/YN_EUW_e1
series tmp_EUS_e1a = 100*YN_EUS_e1a/YN_EUS_e1
series tmp_EUE_e1a = 100*YN_EUE_e1a/YN_EUE_e1
series tmp_EUN_e1a = 100*YN_EUN_e1a/YN_EUN_e1
series tmp_UK_e1a = 100*YN_UK_e1a/YN_UK_e1
  
call SPGraph(%p_Page,"EU_YNHIT", _
  "Income per capita (scenario 1 = 100)", _
  %color, %font,"single EUS EUW UK EUE EUN","e1a","2012 2030 2012",3, _
  ";90,110;0;tmp_?:" _
  )

call SPGraph(%p_Page,"USCN_CAV$", _
  "Current accounts (% GDP)", _
  %color, %font,"EU US CN",%p_tlScenario,"2007 2030 2012",2, _
  ";-5,10;0;CAV$_?:" _
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
  "Real exchange rates", _
  %color, %font,"EUW US CN",%p_tlScenario,"2007 2030 2012",2, _
  ";0.4,1.8;0;rx_?:" _
  )

call SPGraph(%p_Page,"USCN_DXV", _
  "Exports and GDP growth (% p.a.)", _
  %color, %font,"CN JA EAH EAO US OD ACX AMS W AF OA EA AM EU", _
  %p_tlScenario,%p_tlyear,3, _
  "GDP;0,6;;100*(exp(log(V_?/V_?(-20))/20)-1):" _
  + "exports;0,10;;100*(exp(log(X$_?/X$_?(-20))/20)-1):" _
  )

call SPGraph(%p_Page,"W_DXV", _
  "Exports and GDP growth by world region (% p.a.)", _
  %color, %font,"W AF OA EA AM EU", _
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
series tmp_AFS_e1a = 100*YN_AFS_e1a/YN_AFS_e1
series tmp_ASO_e1a = 100*YN_ASO_e1a/YN_ASO_e1
series tmp_AMS_e1a = 100*YN_AMS_e1a/YN_AMS_e1
series tmp_AFN_e1a = 100*YN_AFN_e1a/YN_AFN_e1
series tmp_WA_e1a = 100*YN_WA_e1a/YN_WA_e1
series tmp_CI_e1a = 100*YN_CI_e1a/YN_CI_e1
series tmp_IN_e1a = 100*YN_IN_e1a/YN_IN_e1
  
call SPGraph(%p_Page,"W_YNLOMI", _
  "Income per capita (scenario 1 = 100)", _
  %color, %font,"single AFS AFN ASO WA CI AMS IN","e1a","2012 2030 2012",3, _
  ";90,110;0;tmp_?:" _
  )

endsub

