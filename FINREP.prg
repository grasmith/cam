'PROGRAM: FINREP.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 4.6  AUGUR
'
' presentation graphs for final report
'
' updated: FC 25/07/2012
'
' requires SOLE3
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

pagecreate(page=E1) a %start %end
pagecreate(page=E2) a %start %end
pagecreate(page=E3) a %start %end
pagecreate(page=E4) a %start %end
call pGraphE1("E1", "1980 2030 2012", "E1", %blyellow)
call pGraphE2("E2", "2000 2030 2012", _
  "E1;struggling on:E2;EU break up:", %blyellow)
call pGraphE3("E3", "2000 2030 2012", _
 "E1;struggling on:E3;towards federal Europe:", _
 %blyellow)
call pGraphE4("E4", "1990 2030 2012", _
 "E1;struggling on:E3;towards federal Europe:" _
 + "E4;multi-speed Europe:", _
 %blyellow)

stop

subroutine pGraphE1(string %p_Page, string %p_tlYear, _
  string %p_tlScenario, string %p_color)
'======================================================
%page = %p_Page
pageselect {%page}
delete *
pageselect data

%font = "24"
%color = %p_color
if @left(@upper(%p_color),1) = "B" then %color = "" endif

call SPGraph(%page, "W_PW", _
  "Relative price indexes (2005 = 1)", _
  %color, %font,"","E1",%p_tlyear,2, _
  "primary commodities;0.2,1.8;;pa_W:" _
    + "oil;0.2,2.5;;pe_W:" _
  )

call SPGraph(%page,"W_CO2", _
  "carbon emissions (billion tons p.a.)", _
  %color, %font,"","E1",%p_tlyear,3, _
  ";;;CO2_W/1000:" _
  )

call SPGraph(%page,"W_CO2GR", _
  "growth of GDP, energy use and carbon emissions (% p.a.)", _
  %color, %font,"","E1",%p_tlyear,3, _
  ";;0;@pc(Y_W) GDP|@pc(ED_W) energy use" _
  + "|@pc(CO2_W) CO2 emissions:" _
  )

call SPGraph(%p_Page,"W_CANXAX", _
  "", _
  %color, %font,"single US EU EA OA",%p_tlScenario,%p_tlyear,3, _
  "current account (% GDP);-10,10;0;100*CA$_?/(rx_?*VV_?):" _
  + "net external position (% GDP);" _
    + "-100,100;0;100*NX$_?/(rx_?*VV_?):" _
  + "external assets (% GDP);" _
    + "0,600;0;100*AX$_?/(rx_?*VV_?):" _
  + "net external position (% assets);" _
   + ";0;100*NX$_?/AX$_?:" _
  )

call SPGraph(%p_Page,"W_ER", _
  "employment rates in other large economies (%)", _
  %color, %font,"single CN US AMS EU IN", _
  %p_tlScenario,%p_tlyear,3, _
  ";40,80;;NER_?:" _
  )

call SPGraph(%p_Page,"W_YN", _
  "per capita income by world region ($2005 ppp)", _
  %color, %font,"single EU AM EA OA AF", _
  %p_tlScenario,%p_tlyear,3, _
  ";;;YN_?:" _
  )

call SPGraph(%p_Page,"W_DXV", _
  "Exports and GDP growth by world region (% p.a.)", _
  %color, %font,"single W AF OA EA AM EU", _
  %p_tlScenario,%p_tlyear,3, _
  "GDP;;;100*(exp(log(V_?/V_?(-20))/20)-1):" _
  + "exports;;;100*(exp(log(X$_?/X$_?(-20))/20)-1):" _
  )

call SPGraph(%p_Page,"W_YNLI", _
  "low income regions relative to world average (%)", _
  %color, %font,"single AFN EAO IN AFS ASO", _
  %p_tlScenario,%p_tlyear,3, _
  ";0,60;;100*YN_?/YN_W:" _
  )

call SPGraph(%p_Page,"REG_DV", _
  "GDP growth in world regions (% p.a.)", _
  %color, %font,"single AF OA AM EA EU", _
  %p_tlScenario,"2000 2030 2012",3, _
  ";-5,10;0;@pc(V_?):" _
  )
  
call SPGraph(%p_Page,"EU_LGV", _
  "government debt (% of GDP)", _
  %color, %font,"single EUS UK EUW EUN EUE", _
  %p_tlScenario,%p_tlyear,3, _
  ";0,;0;100*LG_?/VV_?|60:" _
  )

call SPGraph(%p_Page,"EU_NLG", _
  "reduction in government debt (% of GDP)", _
  %color, %font,"UK EUS", _
  %p_tlScenario,%p_tlyear,3, _
  ";-30,10;0;100*NLG_?/VV_? budget balance" _
  + "|-100*d(LG_?)/VV_? debt reduction:" _
  )

call SPGraph(%p_Page,"EU_G", _
  "government spending on goods and services(% of GDP)", _
  %color, %font,"EUN EUW EUE UK EUS", _
  %p_tlScenario,%p_tlyear,3, _
  ";15,35;0;100*G_?/VV_?:" _
  )
  
call SPGraph(%p_Page,"EU_ER", _
  "employment rates in Europe (%)", _
  %color, %font,"single EUN EUW UK EUS EUE", _
  %p_tlScenario,%p_tlyear,3, _
  ";40,70;;NER_?:" _
  )

call SPGraph(%p_Page,"CMEA_ER", _
  "employment rates in neighbouring regions (%)", _
  %color, %font,"single CI EU AFS AFN WA", _
  %p_tlScenario,%p_tlyear,3, _
  ";40,70;;NER_?:" _
  )

call SPGraph(%p_Page,"EU_OP", _
  "government spending on goods and services" _
  + "\n\rrelative to the number of elderly people ($pp per person)", _
  %color, %font,"single EUN EUW UK EUS EUE", _
  %p_tlScenario,%p_tlyear,3, _
  ";0;;G_?/NOP_?:" _
  )

call SPGraph(%p_Page,"EU_GSS", _
  "government service standard (index)", _
  %color, %font,"single EUN EUE EUW UK EUS", _
  %p_tlScenario,%p_tlyear,3, _
  ";0,25;;GSS_?:" _
  )
  
call SPGraph(%p_Page,"EU_NIM", _
  "net migration (millions)", _
  %color, %font,"single EUN EUW UK EUS EUE", _
  %p_tlScenario,%p_tlyear,3, _
  ";-1.5,1.5;0;NIM_?:" _
  )

call SPGraph(%p_Page,"EU_YN", _
  "income per capita ($2005 pp)", _
  %color, %font,"single EUN EUW UK EUS EUE", _
  %p_tlScenario,%p_tlyear,3, _
  ";0,50000;;YN_?:" _
  )

call SPGraph(%p_Page,"EU_pirx", _
  "inflation and competitiveness vs W Europe (% p.a.)", _
  %color, %font,"EUS UK",%p_tlScenario,"2000 2030 2012",2, _
  ";-15,10;0;pi_?-pi_euw inflation" _
  + "|@pc(rx_?)-@pc(rx_euw) change in real exchange rate:" _
  )

call SPGraph(%p_Page,"EU_YNFE", _
  "Income per capita ($2005 pp)", _
  %color, %font,"EUN EUW UK EUS EUE", _
  %p_tlScenario,"1990 2030 2012",3, _
  ";0,50000;;YN_?*(1+(0.6-NER_?/100))^2 full employment" _
  + "|YN_?*(1+CA$_?/M$_?) warranted" _
  + "|YN_? actual:" _
  )
  
endsub

subroutine pGraphE2(string %p_Page, string %p_tlYear, _
  string %p_tlScenario, string %p_color)
'======================================================

%page = %p_Page
pageselect {%page}
delete *
pageselect data

%font = "24"
%color = %p_color
if @left(@upper(%p_color),1) = "B" then %color = "" endif

call SPGraph(%p_Page,"W_DXV", _
  "Exports and GDP growth by world region (% p.a.)", _
  %color, %font,"single W AF OA EA AM EU", _
  "E2;EU break up:",%p_tlyear,3, _
  "GDP;;;100*(exp(log(V_?/V_?(-20))/20)-1):" _
  + "exports;;;100*(exp(log(X$_?/X$_?(-20))/20)-1):" _
  )

pageselect data
smpl 1970 2030
series tmp_eus_e2 = @cumprod((1+rxna_eus_e2/100)/ _
 (1+rxna_euw_e2/100),"2012 2030")
series tmp_eus_e1 = @cumprod((1+rxna_EUS_e1/100)/ _
 (1+rxna_euw_e1/100),"2012 2030")

call SPGraph(%p_Page,"EU_RXNA", _
  "Exchange rate index - South Europe vs West Europe", _
  %color, %font,"EUS",%p_tlScenario,"2012 2030 2012",3, _
  ";;;tmp_?:" _
  )

call SPGraph(%p_Page,"EU_RX", _
  "Real exchange rates vs W Europe", _
  %color, %font,"EUS EUE UK",%p_tlScenario,"2010 2030 2012",3, _
  ";0.5,0.95;;rx_?/rx_EUW:" _
  )

call SPGraph(%p_Page,"EU_SHOCKS", _
  "Europe: shocks following EU break up ", _
  %color, %font,"",%p_tlScenario,"2007 2030 2012",3, _
  "private investment (% of GDP);0;;IPV_EU:" _
  + "total imports ($2005 billion);0;;M$_EU/1000:" _
  + "intra-trade in manufactures ($2005 billion)" _
  + ";0;;(MM$_EU-MMEX$_EU)/1000:" _
  )

call SPGraph(%p_Page,"EU_IMPACT", _
  "Europe: GDP, income and employment after EU break up ", _
  %color, %font,"",%p_tlScenario,"2007 2030 2012",3, _
  "GDP growth (% p.a.);;0;@pc(V_EU):" _
  + "income per capita ($2005 pp);25000,35000;;YN_EU:" _
  + "employment rate (% of 15+ population)" _
  + ";44,56;;NER_EU:" _
  )

call SPGraph(%p_Page,"EU_BLOCS", _
  "GDP, income and employment after EU break up", _
  %color, %font,"single EUW EUS EUE UK","E2","2007 2030 2012",2, _
  "GDP growth (% p.a.);;0;@pc(V_?):" _
  + "income per capita ($2005 pp);;;YN_?:" _
  + "employment rate (% of 15+ population)" _
  + ";40,60;;NER_?:" _
  )
  
call SPGraph(%p_Page,"EU_LGV", _
  "government debt (% of GDP)", _
  %color, %font,"single EUS UK EUW EUN EUE", _
  "E2",%p_tlyear,3, _
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
  %color, %font,"single EUN EUW UK EUS EUE", _
  "E2",%p_tlyear,3, _
  ";0,50000;;YN_?:" _
  )
  
pageselect data
smpl 1970 2030
series tmp_EUW_e2 = 100*YN_EUW_e2/YN_EUW_e1
series tmp_EUS_e2 = 100*YN_EUS_e2/YN_EUS_e1
series tmp_EUE_e2 = 100*YN_EUE_e2/YN_EUE_e1
series tmp_EUN_e2 = 100*YN_EUN_e2/YN_EUN_e1
series tmp_UK_e2 = 100*YN_UK_e2/YN_UK_e1
  
call SPGraph(%p_Page,"EU_YNHIT", _
  "Income per capita (scenario 1 = 100)", _
  %color, %font,"single EUS EUW UK EUE EUN","E2","2012 2030 2012",3, _
  ";;0;tmp_?:" _
  )

call SPGraph(%p_Page,"USCN_DV", _
  "GDP growth after EU break up (% p.a.)", _
  %color, %font,"US CN",%p_tlScenario,"2007 2030 2012",3, _
  ";-5,15;0;@pc(V_?):" _
  )

call SPGraph(%p_Page,"USCN_CAV$", _
  "Current accounts after EU break up (% GDP)", _
  %color, %font,"EU US CN",%p_tlScenario,"2007 2030 2012",3, _
  ";-5,10;0;CAV$_?:" _
  )

call SPGraph(%p_Page,"USCN_RX", _
  "Real exchange rates: West Europe, the US and China", _
  %color, %font,"EUW US CN",%p_tlScenario,"2007 2030 2012",3, _
  ";0.4,1.8;0;rx_?:" _
  )

call SPGraph(%p_Page,"W_PA", _
  "World commodity prices after EU break up (index)", _
  %color, %font,"",%p_tlScenario,"2007 2030 2012",2, _
  ";;0;pa_w:" _
  )

pageselect data
smpl 1970 2030
series tmp_AFS_e2 = 100*YN_AFS_e2/YN_AFS_e1
series tmp_ASO_e2 = 100*YN_ASO_e2/YN_ASO_e1
series tmp_AMS_e2 = 100*YN_AMS_e2/YN_AMS_e1
series tmp_AFN_e2 = 100*YN_AFN_e2/YN_AFN_e1
series tmp_WA_e2 = 100*YN_WA_e2/YN_WA_e1
series tmp_CI_e2 = 100*YN_CI_e2/YN_CI_e1
series tmp_IN_e2 = 100*YN_IN_e2/YN_IN_e1
  
call SPGraph(%p_Page,"W_YNLOMI", _
  "Income per capita (scenario 1 = 100)", _
  %color, %font,"single AFS AFN ASO WA CI AMS IN","E2","2012 2030 2012",3, _
  ";92,100;0;tmp_?:" _
  )

endsub

subroutine pGraphE3(string %p_Page, string %p_tlYear, _
  string %p_tlScenario, string %p_color)
'======================================================

%page = %p_Page
pageselect {%page}
delete *
pageselect data

%font = "24"
%color = %p_color
if @left(@upper(%p_color),1) = "B" then %color = "" endif

call SPGraph(%p_Page,"W_DXV", _
  "Exports and GDP growth by world region (% p.a.)", _
  %color, %font,"single W AF OA EA AM EU", _
  "E3;towards federal Europe:",%p_tlyear,3, _
  "GDP;;;100*(exp(log(V_?/V_?(-20))/20)-1):" _
  + "exports;;;100*(exp(log(X$_?/X$_?(-20))/20)-1):" _
  )

smpl 1970 2030
series tmp_N_E3 = N_EUW_E3+N_EUS_E3+N_EUE_E3+N_EUN_E3
for %a 1 2 3 4 5
series tmp_{%a}_E3 = (EU_YGX{%a}$_EUS_E3+EU_YGX{%a}$_EUE_E3+EU_YGX{%a}$_EUW_E3+EU_YGX{%a}$_EUN_E3)/1000
next

smpl 2013 2030

call SPGraph(%p_Page,"FED_YGXM", _
  "European federal budget", _
  %color, %font,"","E3","2013 2030 2012",3, _
  "$2005 per person;;;" _
  + "eu_ygx$/tmp_N spending|eu_ygm$/tmp_N revenue:" _
  + "% GDP;;;" _
  + "100*eu_ygx$/eu_y$ spending|" _
  + "100*eu_ygm$/eu_y$ revenue:" _
  )

call SPGraph(%p_Page,"FED_YGXN_BLOC", _
  "European federal budget: expenditure ($2005 per person)", _
  %color, %font,"single EUS EUE EUW EUN","E3","2013 2030 2012",3, _
  ";;;eu_ygx$_?/N_?" _
  )

call SPGraph(%p_Page,"FED_YGXMY_BLOC", _
  "European federal budget: net receipts by country group (% GDP)", _
  %color, %font,"EUS EUE EUW EUN","E3","2013 2030 2012",3, _
  ";;;100*eu_ygx$_?/y$_? receipts" _
  + "|100*eu_ygm$_?/y$_? contributions:" _
  )

call SPGraph(%p_Page,"FED_YGX", _
  "European federal budget: spending by objective ($2005 billion)", _
  %color, %font,"","E3","2013 2030 2012",3, _
  ";0,1800;area;tmp_1 general per capita" _
  + "|tmp_2 children and elderly" _
  + "|tmp_3 service standards" _
  + "|tmp_4 employment creation" _
  + "|tmp_5 national government deficits:" _
  )

call SPGraph(%p_Page,"EU_RX", _
  "Real exchange rates vs W Europe", _
  %color, %font,"EUS EUE UK",%p_tlScenario,"2010 2030 2012",3, _
  ";0.55,0.95;;rx_?/rx_EUW:" _
  )

call SPGraph(%p_Page,"EU_SHOCKS", _
  "Europe: trade and investment", _
  %color, %font,"",%p_tlScenario,"2007 2030 2012",3, _
  "private investment (% of GDP);0;;IPV_EU:" _
  + "total imports ($2005 billion);0;;M$_EU/1000:" _
  + "intra-trade in manufactures ($2005 billion)" _
  + ";0;;(MM$_EU-MMEX$_EU)/1000:" _
 )

call SPGraph(%p_Page,"EU_IMPACT", _
  "Europe: GDP growth and income per capita", _
  %color, %font,"",%p_tlScenario,"2007 2030 2012",3, _
  "GDP growth (% p.a.);;0;@pc(V_EU):" _
  + "income per capita ($2005 pp);25000,45000;;YN_EU:" _
  )

call SPGraph(%p_Page,"EU_IMPACT1", _
  "Europe: GDP, income and employment", _
  %color, %font,"",%p_tlScenario,"2007 2030 2012",3, _
  "GDP growth (% p.a.);;0;@pc(V_EU):" _
  + "income per capita ($2005 pp);;;YN_EU:" _
  + "employment rate (% of 15+ population)" _
  + ";40,60;;NER_EU:" _
  )

call SPGraph(%p_Page,"EU_IMPACT2", _
  "Towards Federal Europe: GDP, income and employment by region", _
  %color, %font,"single EUW EUS EUE UK","E3","2007 2030 2012",3, _
  "GDP growth (% p.a.);;0;@pc(V_?):" _
  + "income per capita ($2005 pp);;;YN_?:" _
  + "employment rate (% of 15+ population)" _
  + ";40,60;;NER_?:" _
  )
  
call SPGraph(%p_Page,"EU_LGV", _
  "government debt (% of GDP)", _
  %color, %font,"single EUS UK EUW EUN EUE", _
  "E3",%p_tlyear,3, _
  ";0,;0;100*LG_?/VV_?|60:" _
  )

call SPGraph(%p_Page,"EU_G", _
  "government spending on goods and services (% of GDP)", _
  %color, %font,"EUN EUW EUE EUS", _
  %p_tlScenario,%p_tlyear,3, _
  ";15,35;0;100*G_?/VV_?:" _
  )

call SPGraph(%p_Page,"EU_ER", _
  "employment rates (% of 15+ population)", _
  %color, %font,"single EUN EUW EUS EUE", _
  "E3",%p_tlyear,2, _
  ";;;NER_?:" _
  )

call SPGraph(%p_Page,"EU_OP", _
  "government spending on goods and services" _
  + "\n\rrelative to the number of elderly people ($pp per person)", _
  %color, %font,"single EUN EUW UK EUS EUE", _
  %p_tlScenario,%p_tlyear,3, _
  ";0;;G_?/NOP_?:" _
  )

call SPGraph(%p_Page,"EU_YN", _
  "income per capita ($2005 pp)", _
  %color, %font,"single EUN EUW UK EUS EUE", _
  "E3",%p_tlyear,3, _
  ";0,;;YN_?:" _
  )

pageselect data
smpl 1970 2030
series tmp_EUW_e3 = 100*YN_EUW_e3/YN_EUW_e1
series tmp_EUS_e3 = 100*YN_EUS_e3/YN_EUS_e1
series tmp_EUE_e3 = 100*YN_EUE_e3/YN_EUE_e1
series tmp_EUN_e3 = 100*YN_EUN_e3/YN_EUN_e1
series tmp_UK_e3 = 100*YN_UK_e3/YN_UK_e1
  
call SPGraph(%p_Page,"EU_YNGAIN", _
  "Income per capita (scenario 1 = 100)", _
  %color, %font,"single EUS EUE UK EUW EUN","E3","2012 2030 2012",3, _
  ";;0;tmp_?:" _
  )

call SPGraph(%page, "W_PW", _
  "Relative price indexes (2005 = 1)", _
  %color, %font,"",%p_tlScenario,%p_tlyear,2, _
  "primary commodities;0.2,2.5;;pa_W:" _
    + "oil;0.2,2.5;;pe_W:" _
  )

call SPGraph(%page,"W_CO2", _
  "carbon emissions (billion tons p.a.)", _
  %color, %font,"",%p_tlScenario,%p_tlyear,3, _
  ";;;CO2_W/1000:" _
  )

call SPGraph(%p_Page,"REG_DV", _
  "GDP growth by world region (% p.a.)", _
  %color, %font,"AM EU EA OA AF",%p_tlScenario,"2007 2030 2012",3, _
  ";-5,10;0;@pc(V_?):" _
  )

call SPGraph(%p_Page,"W_DXV", _
  "Exports and GDP growth by world region (% p.a.)", _
  %color, %font,"single W AF OA EA AM EU", _
  "E3;towards federal Europe:",%p_tlyear,3, _
  "GDP;;;100*(exp(log(V_?/V_?(-20))/20)-1):" _
  + "exports;;;100*(exp(log(X$_?/X$_?(-20))/20)-1):" _
  )

call SPGraph(%p_Page,"REG_CAV$", _
  "Current accounts by world region (% GDP)", _
  %color, %font,"AM EU EA OA AF",%p_tlScenario,"2007 2030 2012",3, _
  ";-8,8;0;CAV$_?:" _
  )

call SPGraph(%p_Page,"USCN_RX", _
  "Real exchange rates: West Europe, the US and China", _
  %color, %font,"EUW US CN",%p_tlScenario,"2007 2030 2012",3, _
  ";0.4,1.8;0;rx_?:" _
  )

pageselect data
smpl 1970 2030
series tmp_AFS_e3 = 100*YN_AFS_e3/YN_AFS_e1
series tmp_ASO_e3 = 100*YN_ASO_e3/YN_ASO_e1
series tmp_AMS_e3 = 100*YN_AMS_e3/YN_AMS_e1
series tmp_AFN_e3 = 100*YN_AFN_e3/YN_AFN_e1
series tmp_WA_e3 = 100*YN_WA_e3/YN_WA_e1
series tmp_CI_e3 = 100*YN_CI_e3/YN_CI_e1
series tmp_IN_e3 = 100*YN_IN_e3/YN_IN_e1
  
call SPGraph(%p_Page,"W_YNLOMI", _
  "Income per capita (scenario 1 = 100)", _
  %color, %font,"single IN WA AFN CI AFS AMS ASO","E3","2012 2030 2012",3, _
  ";;;tmp_?:" _
  )

endsub

subroutine pGraphE4(string %p_Page, string %p_tlYear, _
  string %p_tlScenario, string %p_color)
'======================================================

%page = %p_Page
pageselect {%page}
delete *
pageselect data

%font = "24"
%color = %p_color
if @left(@upper(%p_color),1) = "B" then %color = "" endif

call SPGraph(%p_Page,"W_DXV", _
  "Exports and GDP growth by world region (% p.a.)", _
  %color, %font,"single W AF OA EA AM EU", _
  "E4;multi-speed Europe:",%p_tlyear,3, _
  "GDP;;;100*(exp(log(V_?/V_?(-20))/20)-1):" _
  + "exports;;;100*(exp(log(X$_?/X$_?(-20))/20)-1):" _
  )

pageselect data
smpl 1970 2030
series tmp_eus_e4 = @cumprod((1+rxna_eus_e4/100)/ _
 (1+rxna_euw_e4/100),"2012 2030")
series tmp_eus_e2 = @cumprod((1+rxna_eus_e2/100)/ _
 (1+rxna_euw_e2/100),"2012 2030")
series tmp_eus_e3 = @cumprod((1+rxna_EUS_e3/100)/ _
 (1+rxna_euw_e3/100),"2012 2030")

call SPGraph(%p_Page,"EU_RXNA", _
  "Exchange rate index - South Europe vs West Europe", _
  %color, %font,"EUS","E1;struggling on:E2;EU break up:" _
 + "E4;multi-speed Europe:","2012 2030 2012",3, _
  ";0.4,1.1;;tmp_?:" _
  )

call SPGraph(%p_Page,"EU_RX", _
  "Real exchange rates vs W Europe", _
  %color, %font,"EUS EUE UK","E1;struggling on:E2;EU break up:" _
 + "E3;towards federal Europe:E4;multi-speed Europe:", _
  "2010 2030 2012",3, _
  ";0.35,0.95;;rx_?/rx_EUW:" _
  )

call SPGraph(%p_Page,"EU_SHOCKS", _
  "Multi-speed Europe: trade and investment", _
  %color, %font,"",%p_tlScenario,"2007 2030 2012",3, _
  "private investment (% of GDP);0;;IPV_EU:" _
  + "total imports ($2005 billion);0;;M$_EU/1000:" _
  + "intra-trade in manufactures ($2005 billion)" _
  + ";0;;(MM$_EU-MMEX$_EU)/1000:" _
  )

call SPGraph(%p_Page,"EU_IMPACT", _
  "Multi-speed Europe: GDP growth and income per capita", _
  %color, %font,"",%p_tlScenario,"2007 2030 2012",3, _
  "GDP growth (% p.a.);;0;@pc(V_EU):" _
  + "income per capita ($2005 pp);25000,45000;;YN_EU:" _
  )

call SPGraph(%p_Page,"EU_IMPACT1", _
  "Multi-speed Europe: GDP, income and employment", _
  %color, %font,"",%p_tlScenario,"2007 2030 2012",3, _
  "GDP growth (% p.a.);;0;@pc(V_EU):" _
  + "income per capita ($2005 pp);25000,45000;;YN_EU:" _
  + "employment rate (% of 15+ population)" _
  + ";46,60;;NER_EU:" _
  )

call SPGraph(%p_Page,"EU_BLOCS", _
  "Multi-speed Europe: GDP, income and employment by region", _
  %color, %font,"single EUW EUS EUE UK","e4","2007 2030 2012",2, _
  "GDP growth (% p.a.);;0;@pc(V_?):" _
  + "income per capita ($2005 pp);;;YN_?:" _
  + "employment rate (% of 15+ population)" _
  + ";40,60;;NER_?:" _
  )
  
call SPGraph(%p_Page,"EU_LGV", _
  "government debt (% of GDP)", _
  %color, %font,"single EUS UK EUW EUN EUE", _
  "E4",%p_tlyear,3, _
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
  %color, %font,"single EUN EUW UK EUS EUE", _
  "E4",%p_tlyear,3, _
  ";0,60000;;YN_?:" _
  )

call SPGraph(%p_Page,"EU_ER", _
  "employment rates (% of 15+ population)", _
  %color, %font,"single EUN EUW UK EUS EUE", _
  "E3",%p_tlyear,3, _
  ";;;NER_?:" _
  )

pageselect data
smpl 1970 2030
series tmp_EUW_e4 = 100*YN_EUW_e4/YN_EUW_e1
series tmp_EUS_e4 = 100*YN_EUS_e4/YN_EUS_e1
series tmp_EUE_e4 = 100*YN_EUE_e4/YN_EUE_e1
series tmp_EUN_e4 = 100*YN_EUN_e4/YN_EUN_e1
series tmp_UK_e4 = 100*YN_UK_e4/YN_UK_e1
  
call SPGraph(%p_Page,"EU_YNGAIN", _
  "Income per capita (scenario 1 = 100)", _
  %color, %font,"single EUE EUS UK EUW EUN","e4","2012 2030 2012",3, _
  ";100,180;;tmp_?:" _
  )

call SPGraph(%page, "W_PW", _
  "Relative price indexes (2005 = 1)", _
  %color, %font,"",%p_tlScenario,%p_tlyear,2, _
  "primary commodities;0.2,2.5;;pa_W:" _
    + "oil;0.2,2.5;;pe_W:" _
  )

call SPGraph(%page,"W_CO2", _
  "carbon emissions (billion tons p.a.)", _
  %color, %font,"",%p_tlScenario,%p_tlyear,3, _
  ";;;CO2_W/1000:" _
  )

call SPGraph(%p_Page,"REG_DV", _
  "GDP growth by world region (% p.a.)", _
  %color, %font,"AM EU EA OA AF",%p_tlScenario,"2007 2030 2012",3, _
  ";-5,15;0;@pc(V_?):" _
  )

call SPGraph(%p_Page,"REG_CAV$", _
  "Current accounts by world region (% GDP)", _
  %color, %font,"AM EU EA OA AF",%p_tlScenario,"2007 2030 2012",3, _
  ";-8,8;0;CAV$_?:" _
  )

call SPGraph(%p_Page,"REG_NXV$", _
  "Net external positions by world region (% GDP)", _
  %color, %font,"single EA EU OA AM AF","E4","2007 2030 2012",3, _
  ";;0;NXV$_?:" _
  )

call SPGraph(%p_Page,"REG_NX$", _
  "Net external positions by world region (% world GDP)", _
  %color, %font,"single EA EU OA AF AM","E4","2007 2030 2012",3, _
  ";;0;100*NX$_?/Y$_W:" _
  )

call SPGraph(%p_Page,"USCN_RX", _
  "Real exchange rates: West Europe, the US and China", _
  %color, %font,"EUW US CN",%p_tlScenario,"2007 2030 2012",3, _
  ";0.4,1.8;0;rx_?:" _
  )

pageselect data
smpl 1970 2030
series tmp_AFS_e4 = 100*YN_AFS_e4/YN_AFS_e1
series tmp_ASO_e4 = 100*YN_ASO_e4/YN_ASO_e1
series tmp_AMS_e4 = 100*YN_AMS_e4/YN_AMS_e1
series tmp_AFN_e4 = 100*YN_AFN_e4/YN_AFN_e1
series tmp_WA_e4 = 100*YN_WA_e4/YN_WA_e1
series tmp_CI_e4 = 100*YN_CI_e4/YN_CI_e1
series tmp_IN_e4 = 100*YN_IN_e4/YN_IN_e1
  
call SPGraph(%p_Page,"W_YNLOMI", _
  "Income per capita (scenario 1 = 100)", _
  %color, %font,"single CI WA AFN IN ASO AMS AFS","e4","2012 2030 2012",3, _
  ";;;tmp_?:" _
  )

endsub
