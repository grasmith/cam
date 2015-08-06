'resINCN.prg  Presentation charts for India and China
'
' CAM Version 6.1  FESSUD variant
'
' Charts (and tables) presenting results
'
' run this program from sol0.wf1 by typing
'
'   run(c) resINCN
'
' creates charts and tables with prefix gre_
'
' options 
'   create charts without titles and legends
'   create tables (on the tables page) 
'
'------------------------------------------------------
' each chart/table is made by a call to the routine GREV
' at the end of this file. Before calling GREV you should
' i) set the time frame for the chart
'  e.g. smpl 2000 2030
' ii) generate any special variables e.g. smoothed trends
' (see examples below)
' The arguments passed to GREV are
' - a 2-digit identifier to distinguish the chart/table
' - the chart type e.g. "line" or "area(s)"
' - (optional) a title for the chart
' - a list of variables or formulae to be displayed
' - (optional) legends (one for each variable or formula)
' - (optional) the range for the left-hand axis
'
' optional arguments must be passed as blank strings ""
'
' See examples below for variations such as plotting
' variables with different scales (left axis and right
' axis) or including a zero line or a horizontal line
' representing a target level

'=====================================
include "zlib.prg"
pageselect data

'======= OPTIONS =====================
'--- plain charts (no titles or legends)
'%display = "plain"
'--- tables with data from graphs
%grtab = "Yes"
'----------------------------------------------------

'--- clear any previous presentation graphs and tables
delete gre* gpe* tre* graphs\gre* tables\gre* tables\tx*

'--- global financialisation
'--- note: this chart has one variable and no legend
smpl 1970 2030
call GREV("01", "line", _
  "Acquisition of external assets (% of world GDP)", _
  "ilx$_w_0*100/vv$_w_0", _
  "",  _
  "range(0,25)")

'--- note: this chart has two variables with legends
'    the second variable is mapped to the 
'    right-hand axis with a different range
smpl 1970 2030
call GREV("02", "line", _
  "Financialisation and world trade (% of world GDP)", _
  "x$_w_0*100/vv$_w_0 lx$_w_0*100/vv$_w_0", _
  "exports;R:external assets", _
  "range(10,40);range(50,200)")

'--- note: this chart has several variables on the 
'    same axis with a line across the chart at zero
smpl 1990 2015
call GREV("03", "line", _
  "Financialisation and the real economy (% of world GDP)", _
  "ilx$_w_0*100/vv$_w_0 100*d(x$_w_0)/vv$_w_0 " _
    + "100*d(ip_w_0+iv_w_0)/vv$_w_0 100*d(g_w_0)/vv$_w_0 @pc(vv_w_0)", _
  "acquisition of external assets;change in exports;change in investment;change in government spending; GDP growth", _
  "zeroline")

'--- note: this chart shows shares displayed as areas
'    adding up to 100
smpl 1990 2030
call GREV("04", "area(s)", _
  "Financial hegemony: external assets (% of world total)", _
  "ax$_in_0*100/ax$_w_0 ax$_cn_0*100/ax$_w_0 ax$_oee_0*100/ax$_w_0 ax$_oex_0*100/ax$_w_0 ax$_row_0*100/ax$_w_0 ax$_eur_0*100/ax$_w_0 ax$_us_0*100/ax$_w_0", _
  "India;China;Other East Asia;Oil exporters;Rest of world;Europe;US", _
  "range(0,100)")

smpl 1970 2030
call GREV("05", "area(s)", _
  "Trade hegemony: exports of goods and services (% of world total)", _
  "x$_in_0*100/x$_w_0 x$_cn_0*100/x$_w_0 x$_oee_0*100/x$_w_0 x$_oex_0*100/x$_w_0 x$_row_0*100/x$_w_0 x$_eur_0*100/x$_w_0 x$_us_0*100/x$_w_0", _
  "India;China;Other East Asia;Oil exporters;Rest of world;Europe;US", _
  "range(0,100)")

smpl 1970 2030
call GREV("06", "area(s)", _
  "Exchange reserves (% of world total)", _
  "r$_in_0*100/r$_w_0 r$_cn_0*100/r$_w_0 r$_oee_0*100/r$_w_0 r$_oex_0*100/r$_w_0 r$_row_0*100/r$_w_0 r$_eur_0*100/r$_w_0 r$_us_0*100/r$_w_0", _
  "India;China;Other East Asia;Oil exporters;Rest of world;Europe;US", _
  "range(0,100)")

'--- generate an additional variable
'--- acquisition of external assets (total)
smpl 1970 2030
p_BGW.genr iax$_?_0 = iadi$_?_0+iapi$_?_0+iaoi$_?_0+ir$_?_0  
'--- chart using the new variable
smpl 1990 2015
call GREV("07", "area(s)", _
  "Contribution to the bubble: acquisition of external assets (% of world GDP)", _
  "iax$_in_0*100/vv$_w_0 iax$_cn_0*100/vv$_w_0 iax$_oee_0*100/vv$_w_0 iax$_oex_0*100/vv$_w_0 iax$_row_0*100/vv$_w_0 iax$_eur_0*100/vv$_w_0 iax$_us_0*100/vv$_w_0", _
  "India;China;Other East Asia;Oil exporters;Rest of world;Europe;US", _
  "")

smpl 1990 2015
call GREV("08", "area(s)", _
  "Contribution to the bubble: incurrence of external liabilities (% of world GDP)", _
  "ilx$_in_0*100/vv$_w_0 ilx$_cn_0*100/vv$_w_0 ilx$_oee_0*100/vv$_w_0 ilx$_oex_0*100/vv$_w_0 ilx$_row_0*100/vv$_w_0 ilx$_eur_0*100/vv$_w_0 ilx$_us_0*100/vv$_w_0", _
  "India;China;Other East Asia;Oil exporters;Rest of world;Europe;US", _
  "")

'--- another new variable
'--- bank assets (total, ipp)
smpl 1970 2030
p_BGW.genr adb$_?_0 = (lgf_?_0+ln_?_0)*rx_?_0+r$_?_0  
'--- chart displaying the variable
smpl 1990 2015
call GREV("09", "line", _
  "External assets as % of assets of the banking system", _
  "ax$_in_0*100/adb$_in_0 ax$_cn_0*100/adb$_cn_0 ax$_oee_0*100/adb$_oee_0 ax$_oex_0*100/adb$_oex_0 ax$_row_0*100/adb$_row_0 ax$_eur_0*100/adb$_eur_0 ax$_us_0*100/adb$_us_0", _
  "India;China;Other East Asia;Oil exporters;Rest of world;Europe;US", _
  "")

smpl 1990 2015
call GREV("10", "line", _
  "India: domestic debt and external liabilities as % of GDP", _
  "lgv_in_0 lnv_in_0 lxv$_in_0", _
  "government debt;bank advances;external liabilities", _
  "")

smpl 1990 2015
call GREV("11", "line", _
  "China: domestic debt and external liabilities as % of GDP", _
  "lgv_cn_0 lnv_cn_0 lxv$_cn_0", _
  "government debt;bank advances;external liabilities", _
  "")

smpl 1990 2015
call GREV("12", "line", _
  "Europe: domestic debt and external liabilities as % of GDP", _
  "lgv_eur_0 lnv_eur_0 lxv$_eur_0", _
  "government debt;bank advances;external liabilities", _
  "")

smpl 1990 2015
call GREV("13", "line", _
  "US: domestic debt and external liabilities as % of GDP", _
  "lgv_us_0 lnv_us_0 lxv$_us_0", _
  "government debt;bank advances;external liabilities", _
  "")
 
'--- composition of external assets and liabilities 
smpl 1990 2015
call GREV("20", "line", _
  "Portfolio liabilities as % of total external liabilities", _
  "lpi$_in_0*100/lx$_in_0 lpi$_cn_0*100/lx$_cn_0 lpi$_eur_0*100/lx$_eur_0 lpi$_us_0*100/lx$_us_0", _
  "India;China;Europe;US", _
  "")

smpl 1990 2015
call GREV("21", "line", _
  "External portfolio liabilities as % of annual GDP", _
  "lpi$_in_0*100/vv$_in_0 lpi$_cn_0*100/vv$_cn_0 lpi$_eur_0*100/vv$_eur_0 lpi$_us_0*100/vv$_us_0", _
  "India;China;Europe;US", _
  "")

'--- net positions
smpl 1970 2015
call GREV("22", "area(s)", _
  "Net external assets: blocs with net asset positions ($2005 trillion)", _
  "nx$_cn_0/1000000 nx$_ru_0/1000000 nx$_oeh_0/1000000 nx$_nwa_0/1000000 nx$_de_0/1000000 nx$_uk_0/1000000 nx$_euc_0/1000000 nx$_oeu_0/1000000", _
  "China;Russia;Other E Asia High Income;North Africa & W Asia;Germany;UK;Core eurozone;Other Europe", _
  "range(0,12)")

smpl 1970 2015
call GREV("23", "area(s)", _
  "Net external liabilities: blocs with net liability positions ($2005 trillion)", _
  "nx$_us_0/1000000 nx$_in_0/1000000 nx$_br_0/1000000 nx$_id_0/1000000 nx$_za_0/1000000 nx$_tr_0/1000000 nx$_can_0/1000000 nx$_acx_0/1000000 nx$_oam_0/1000000 nx$_oca_0/1000000 nx$_oea_0/1000000 nx$_osa_0/1000000 nx$_oaf_0/1000000 nx$_fr_0/1000000 nx$_eup_0/1000000", _
  "US;India;Brazil;Indonesia;S Africa;Turkey;Canada, Australia & New Zealand;Central America and Caribbean;Other America;Other Central Asia;Other East Asia;Other South Asia;Other Africa;France;Eurozone periphery", _
  "range(-12,0)")

'--- government finance  
smpl 1990 2015
call GREV("30", "line", _
  "Government debt (% of GDP)", _
  "lgv_in_0 lgv_cn_0 lgv_oee_0 lgv_oex_0 lgv_row_0 lgv_eur_0 lgv_us_0", _
  "India;China;Other East Asia;Oil exporters;Rest of world;Europe;US", _
  "range(0,150)")

'--- government deficit
smpl 1990 2015
call GREV("31", "line", _
  "Government revenue (% of GDP)", _
  "ygrv_in_0 ygrv_cn_0 ygrv_oee_0 ygrv_oex_0 ygrv_row_0 ygrv_eur_0 ygrv_us_0", _
  "India;China;Other East Asia;Oil exporters;Rest of world;Europe;US", _
  "range(0,50)")

smpl 1990 2015
call GREV("32", "line", _
  "Government surplus or deficit (% of GDP)", _
  "nlgv_in_0 nlgv_cn_0 nlgv_oee_0 nlgv_oex_0 nlgv_row_0 nlgv_eur_0 nlgv_us_0", _
  "India;China;Other East Asia;Oil exporters;Rest of world;Europe;US", _
  "zeroline")

'--- energy dependence and sustainability
smpl 1970 2030
call GREV("40", "area(s)", _
  "Energy imports (% of world total)", _
  "em_in_0*100/em_w_0 em_cn_0*100/em_w_0 em_oee_0*100/em_w_0 em_oex_0*100/em_w_0 em_row_0*100/em_w_0 em_eur_0*100/em_w_0 em_us_0*100/em_w_0", _
  "India;China;Other East Asia;Oil exporters;Rest of world;Europe;US", _
  "range(0,100)")

smpl 1970 2030
call GREV("41", "area(s)", _
  "CO2 emissions (% of world total)", _
  "co2_in_0*100/co2_w_0 co2_cn_0*100/co2_w_0 co2_oee_0*100/co2_w_0 co2_oex_0*100/co2_w_0 co2_row_0*100/co2_w_0 co2_eur_0*100/co2_w_0 co2_us_0*100/co2_w_0", _
  "India;China;Other East Asia;Oil exporters;Rest of world;Europe;US", _
  "range(0,100)")

smpl 1970 2030
call GREV("42", "area(s)", _
  "Exports of manufactures (% of world total)", _
  "xm$_in_0*100/xm$_w_0 xm$_cn_0*100/xm$_w_0 xm$_oee_0*100/xm$_w_0 xm$_oex_0*100/xm$_w_0 xm$_row_0*100/xm$_w_0 xm$_eur_0*100/xm$_w_0 xm$_us_0*100/xm$_w_0", _
  "India;China;Other East Asia;Oil exporters;Rest of world;Europe;US", _
  "range(0,100)")

smpl 1970 2030
call GREV("43", "area(s)", _
  "China: energy sources (% of domestic absorption)", _
  "epc_cn_0*100/ed_cn_0 epn_cn_0*100/ed_cn_0 (-eb_cn_0)*100/ed_cn_0", _
  "coal, gas and oil production;primary electricity;net imports", _
  "range(0,100)")

smpl 1970 2030
call GREV("44", "area(s)", _
  "India: energy sources (% of domestic absorption)", _
  "epc_in_0*100/ed_in_0 epn_in_0*100/ed_in_0 (-eb_in_0)*100/ed_in_0", _
  "coal, gas and oil production;primary electricity;net imports", _
  "range(0,100)")

smpl 1970 2030
call GREV("45", "area(s)", _
  "Europe: energy sources (% of domestic absorption)", _
  "epc_eur_0*100/ed_eur_0 epn_eur_0*100/ed_eur_0 (-eb_eur_0)*100/ed_eur_0", _
  "coal, gas and oil production;primary electricity;net imports", _
  "range(0,100)")

smpl 1970 2030
call GREV("46", "area(s)", _
  "US: energy sources (% of domestic absorption)", _
  "epc_us_0*100/ed_us_0 epn_us_0*100/ed_us_0 (-eb_us_0)*100/ed_us_0", _
  "coal, gas and oil production;primary electricity;net imports", _
  "range(0,100)")

'--- per capita GDP  
'--- smooth growth rates using Hodrick-Prescott filter
'    you can create a 'trend' version of any variable
'    this way.
smpl 1970 2030
dvvn_in_0.hpf hdvvn_in_0
dvvn_cn_0.hpf hdvvn_cn_0
dvvn_us_0.hpf hdvvn_us_0
dvvn_eur_0.hpf hdvvn_eur_0
smpl 1970 2030
call GREV("50", "line", _
  "Trend per capita GDP growth (% pa)", _
  "hdvvn_in_0 hdvvn_cn_0 hdvvn_us_0 hdvvn_eur_0", _
  "India;China;US;Europe", _
  "range(0,10)")

smpl 1970 2030
call GREV("51", "line", _
  "Annual per capita GDP growth (% pa)", _
  "dvvn_in_0 dvvn_cn_0 dvvn_us_0 dvvn_eur_0", _
  "India;China;US;Europe", _
  "zeroline")

smpl 1970 2030
call GREV("52","line", _
  "India: trend per capita GDP growth (% pa)", _
  "hdvvn_in_0", _
  "", _
  "range(0,10);;5")

smpl 1970 2030
call GREV("53","line", _
  "China: trend per capita GDP growth (% pa)", _
  "hdvvn_cn_0", _
  "", _
  "range(0,10);;5")

'==================================================
subroutine GREV(string %p_gre, string %p_atyp, _
  string %p_t, string %p_tlvar, string %p_tldesc, _
  string %p_scale)
'==================================================
'graph for multiple variables
'
' Call: %p_gre    graph id (2-digit)
'       %p_atyp   graph type - e.g. line, area(s)
'       %p_t      title
'       %p_tlvar  list of variables
'       %p_tldesc list of legends
'       %p_scale  scale setting
'
' Ret: 
'
'---------------------------------------------------------------
%tlv = %p_tlvar
graph gre.{%p_atyp} {%p_tlvar}
%tlv = %p_tldesc
if @instr(%tlv, ";R:") > 0 then
  %gaxis = " (left axis)"
else
  %gaxis = ""
endif
if %tlv = "" then gre.legend -display endif  
!i = 0
while %tlv <> ""
  !i = !i + 1
  call Token(%tlv, ";", %desc)
  if @left(%desc,2) = "R:" then
    %desc = @mid(%desc,3) + " (right axis)"
    gre.setelem(!i) axis(r) legend(%desc) lwidth(1.3)
  else
    %desc = %desc + %gaxis
    gre.setelem(!i) legend(%desc) lwidth(1.3)
  endif
  gre.setelem(!i) symsize(L) symbol(!i)
wend
call zGRE(%p_gre, %p_tlvar, %p_t, %p_scale)
endsub

subroutine zGRE(string %p_gre, string %p_tlvar, _
  string %p_t, string %p_scale)
'==================================================
'graph labels and styling
'
' Call: %p_gre    graph id (2-digit)
'       %p_tlvar  list of variables
'       %p_t      title
'       %p_scale  scale setting
'
' Ret: 
'
'---------------------------------------------------------------
gre.legend font(16)
gre.options size(8,3)
'--- draw vertical line at 2015
delete maxy*
series maxy = @max(@trend())
if @elem(maxy,"2015") > @dtoo("2015") then
  gre.draw(line,b, width(1.5), pattern(dash6)) 2015
endif
'--- plain display
if %display = "plain" then
  gre.legend -display
  gre.addtext(t,just(c),font(20)) ""
else
'--- display with title and legends
gre.addtext(t,just(c),font(20)) %p_t
endif
'--- scale and other options
%s3 = %p_scale
%s2 = ""
call Token(%s3,";",%s1)
if %s3 <> "" then call Token(%s3,";",%s2) endif
'--- left-hand axis
if %s1 <> "" then
  gre.axis(l) {%s1} font(20) format(leadzero, dec=0)
else
  gre.axis(l) font(20) format(leadzero, dec=0)
endif
'--- right-hand axis
if %s2 <> "" then
  gre.axis(r) {%s2} font(20) format(leadzero, dec=0)
else
  gre.axis(r) font(20) format(leadzero, dec=0)
endif
'--- horizontal line across the chart
if %s3 <> "" then
  gre.draw(line,left,color(black), width(1)) {%s3}
endif
'--- axis font and date labels
gre.axis(b) font(20)
gre.datelabel interval(obs,10,1900)
gre.options backcolor(255,255,196) -background
'--- graph name
%tlv = %p_tlvar
call Token(%tlv, " ", %v)
call Token(%v, "/", %tlv)
call Token(%tlv, "*", %v)
%g = @replace("gre_" + %p_gre + "_" + %v,"-","")
%g = @left(%g, 17)
copy gre graphs\{%g}
delete gre 
'--- data table
if @upper(@left(%grtab,1)) = "Y" then
  group gpe {%p_tlvar}
  freeze(tre) gpe.sheet
  copy tre tables\{%g}
  delete gpe tre
endif
endsub
