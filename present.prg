'PROGRAM: present.prg          Copyright (C) 2012, 2013, 2014 Alphametrics Co. Ltd.
'
' CAM Version 5.1  EUR variant
'
' presentation graphs - examples
'
' updated: FC 11/01/2014
'
' requires SOLN2
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

'--- create a new page in the workfile to store the graphs
pagecreate(page=present) a %start %end

'--- generate the graphs on the new page
'    historical data for 1990-2013, predictions 2014-2030
'    comparisons of scenarios 2 and 4
'    graph panels have yellow background

call pGraph("present", "1990 2030 2014", _
  "N2;struggling on:N4;European recovery", %blyellow)

stop

'==================================================================
' GRAPH DEFINITIONS
'==================================================================

subroutine pGraph(string %p_Page, string %p_tlYear, _
  string %p_tlScenario, string %p_color)
'======================================================
%page = %p_Page
pageselect {%page}
delete *
pageselect data

%font = "28"
%color = %p_color
if @left(@upper(%p_color),1) = "B" then %color = "" endif

'--- single line graph: income for the world as a whole
'    and by world region
'    0 in the last line forces the scale to start from zero
'    the scenario is selected explicitly in the fourth line
'    and the passed argument %p_tlScenario is overridden
call SPGraph(%page, "G01_WYN", _
  "income per capita ($2005 pp)", _
  %color, %font,"single W EU NAM LAM EA OA AF", _
  "N2;struggling on",%p_tlyear,3, _
  ";0;;YN_?:" _
  )

'--- the same comparing two scenarios
'    50000 in the last line forces the graphs to use a 
'    common scale 0 to 50000 on the left-hand axis
'    the series cannot be shown conveniently on a 
'    single graph so the "single" instruction in the
'    third line is ignored and the graph becomes a tableau
'    the 3 at the end of the fourth line specifies
'    the number of graphs to be shown in each row of the
'    tableau.
call SPGraph(%page, "G02_WYN", _
  "income per capita ($2005 pp)", _
  %color, %font,"single W EU NAM LAM EA OA AF", _
  %p_tlScenario,%p_tlyear,3, _
  ";0,50000;;YN_?:" _
  )

'--- another comparison of two scenarios
call SPGraph(%page, "G02a_GV", _
  "Government services (% of GDP)", _
  %color, %font,"single EUN DE EUW UK FR IT ES EUS PL EUE", _
  %p_tlScenario,%p_tlyear,3, _
  ";17,30;;GV_?:" _
  )
  
'--- single area graph: distribution of consumers
'    expenditure at bloc level in Europe
'    'area' in the last line sets the graph type
'    bloc areas are stacked on top of each other
call SPGraph(%page, "G03_EUC", _
  "consumer spending ($2005 pp billion)", _
  %color, %font,"single DE UK FR IT ES PL EUW EUN EUE EUS", _
  "N2;struggling on",%p_tlyear,3, _
  ";0;area;C_?/1000:" _
  )

'--- scatter diagram: world energy use and CO2 emissions
'    'scat' in the second last line sets the graph type
'    series to be shown on each axis are listed with
'    a vertical bar | as the separator
call SPGraph(%page, "G04_WEDCO2", _
  "world energy use and CO2 emissions (b tons p.a.)", _
  %color, %font,"", _
  "N2;struggling on",%p_tlyear,3, _
  ";;scat;ED_W/1000 energy use" _
    + "|CO2_W/1000 CO2 emissions:" _
  )

'--- bloc comparison: energy use and non-carbon supply
'    this is a line chart with two series in each graph
'    again the series are listed with a vertical bar as
'    the separator |
call SPGraph(%page, "G05_EUEDEPN", _
  "energy use and non-carbon supply (kg per $ GDP)", _
  %color, %font,"DE UK FR IT ES PL EUW EUN EUE EUS", _
  "N2;struggling on",%p_tlyear,3, _
  ";0,0.4;;1000*ED_?/V_? energy use" _
    + "|1000*EPN_?/V_? non-carbon supply:" _
  )

'--- same: comparing scenarios
'    the only modification necessary is to specify the
'    list of scenarios to be displayed, here defined by
'    the argument %p_tlScenario from the beginning of the
'    program
call SPGraph(%page, "G06_EUEDEPN", _
  "energy use and non-carbon supply (kg per $ GDP)", _
  %color, %font,"DE UK FR IT ES PL EUW EUN EUE EUS", _
  %p_tlScenario,%p_tlyear,3, _
  ";0,0.4;;1000*ED_?/V_? energy use" _
    + "|1000*EPN_?/V_? non-carbon supply:" _
  )
  
'--- tableau for one area
'    displays three series in separate graphs,
'    comparing results for scenarios listed in %p_tlScenario
call SPGraph(%page, "G07_USGNLGLG", _
  "US government spending and debt ($2005 billion)", _
  %color, %font,"US", _
  %p_tlScenario,%p_tlyear,3, _
  "government spending;;;G_?/1000:" _
  + "government surplus/deficit;;0;NLG_?/1000:" _
  + "government debt;;;LG_?/1000:" _
  )

'--- similar for several blocs
'    the graphs for each bloc are displayed in successive
'    columns
'    to facilitate comparison the series are shown as
'    percentages of GDP with common scales on the left-hand
'    axis
call SPGraph(%page, "G08_HIGNLGLG", _
  "government spending and debt (% GDP)", _
  %color, %font,"US EU JA", _
  %p_tlScenario,%p_tlyear,3, _
  "government spending;;15,30;100*G_?/V_?:" _
  + "government surplus/deficit;-12,10;0;100*NLG_?/V_?:" _
  + "government debt;40,200;;100*LG_?/V_?:" _
  )

endsub

