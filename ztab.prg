'PROGRAM: ztab.prg     Copyright (C) 2012,2013 Alphametrics Co. Ltd.
'
' CAM Version 5.2
'
' analysis table definitions
'
' updated: FC 15/04/2013
'
'================================================

subroutine pTabDef(table t_Def, scalar nDef)
'==================================================
'load definitions of standard analysis tables
'
' Call: t_TDef  table of table definitions
'       nDef    no of rows 
'
' Ret:  t_TDef  updated table definitions
'       nDef    updated no of rows
'
' Note: the layout of a table definition is as follows
'         header row
'         1 or many section definitions
'         1 or many row definitions
'
'       header row syntax
'         T;tabname;title
'
'       section definition syntax
'         S;mode;transform;units
'           where mode is Annual or Period average
'           and transform is GR, Change, Value
'            or /expr (ratio) or %expr (per cent)
'
'       row definition syntax
'         R;expr;title;format;options
'         where expr may include placeholders
'          ? area + alias
'          ?? area only
'          ~ alias only
'         format is
'          ft.k to display k decimal places
'          optionally prefixed by U to draw a line
'          underneath the row of data 
'         options
'          NRP  no ratios or percents
'          NGR  no growth rates
'
'---------------------------------------------------------------

call LoadTable(t_TDef, nTDef, _
  "T;gdp;INCOME, EXPENDITURE AND GDP GROWTH:" _
  + "S;A;Value;{$ billion, 2005 pp):" _
  + "S;A;%VV_?/1000;{per cent of GDP):" _
  + "S;P;Change;{changes, $ billion, 2005 pp):" _
  + "S;P;GR;{growth rates, % p.a.):" _
  + "R;C_?/1000;Consumers expenditure;ft.1:" _
  + "R;G_?/1000;Government expenditure on g & s;ft.1:" _
  + "R;IP_?/1000;Non-government fixed investment;ft.1:" _
  + "R;IV_?/1000;Inventory changes;Uft.1;NGR:" _
  + "R;H_?/1000;Total domestic demand;ft.1:" _
  + "R;(X$_?/rx_?)/1000;Exports of goods and services;Uft.1:" _
  + "R;(H_?+X$_?/rx_?)/1000;Total final demand;ft.1:" _
  + "R;(M$_?/rx_?)/1000;Imports of goods and services;ft.1:" _
  + "R;VV_?/1000;GDP at market prices;Uft.1:" _
  + "R;VVEM_?/1000;Labour and mixed income;ft.1:" _
  + "R;VVPR_?/1000;Corporate income;ft.1:" _
  + "R;VVTX_?/1000;Indirect taxes less subsidies;ft.1:" _
  + "R;(BIT$_?/rx_?)/1000;External income and transfers;ft.1;NGR:" _
  + "R;Y_?/1000;National income;Uft.1:" _
  + "R;YG_?/1000;Government income;ft.1:" _
  + "R;NLG_?/1000;Government net lending;ft.1;NGR:" _
  + "R;YP_?/1000;Private and corporate income;ft.1:" _
  + "R;NLP_?/1000;Private and corporate net lending;ft.1;NGR:" _
  + "R;(CA$_?/rx_?)/1000;Current account;Uft.1;NGR:" _
  + "R;N_?;Population (millions);ft.1;NRP:" _
  + "R;YN_?;National income per capita ($ ppp);ft.0;NRP:" _
  + "R;C_?/N_?;Consumers exp per capita ($ ppp);ft.0;NRP:" _
  + "R;M$_?/N_?;Imports per capita ($);ft.0;NRP" _
)

call LoadTable(t_TDef, nTDef, _
  + "T;lab;LABOUR FORCE:" _
  + "S;A;Value;{millions):" _
  + "S;P;GR;{growth rates, % p.a.):" _
  + "R;N_?;Total population;ft.1:" _
  + "R;NL_?;Labour force;ft.1:" _
  + "R;NE_?;Employment;ft.1:" _
  + "R;NU_?;Unemployment;Uft.1;NGR:" _
  + "R;NEA_?;Employment in agriculture;ft.1:" _
  + "R;NEI_?;Employment in industry;ft.1:" _
  + "R;NES_?;Employment in services;Uft.1:" _
  + "R;NUVF_?;Female unemployed 25+;ft.1:" _
  + "R;NUVM_?;Male unemployed 25+;ft.1:" _
  + "R;NUYF_?;Female unemployed 15-24;ft.1:" _
  + "R;NUYM_?;Male unemployed 15-24;Uft.1:" _
  + "R;NLNVF_?;Female participation rate 25+ (%);ft.0:" _
  + "R;NLNVM_?;Male participation rate 25+ (%);ft.0:" _
  + "R;NLNYF_?;Female participation rate 15-24 (%);ft.0:" _
  + "R;NLNYM_?;Male participation rate 15-24 (%);Uft.0:" _
  + "R;100*NEVF_?/NVF_?;Female employment rate 25+ (%);ft.0:" _
  + "R;100*NEVM_?/NVM_?;Male employment rate 25+ (%);ft.0:" _
  + "R;100*NEYF_?/NYF_?;Female employment rate 15-24 (%);ft.0:" _
  + "R;100*NEYM_?/NYM_?;Male employment rate 15-24 (%);Uft.0:" _
  + "R;NULVF_?;Female unemployment rate 25+ (%);ft.1:" _
  + "R;NULVM_?;Male unemployment rate 25+ (%);ft.1:" _
  + "R;NULYF_?;Female unemployment rate 15-24 (%);ft.1:" _
  + "R;NULYM_?;Male unemployment rate 15-24 (%);ft.1:" _
)

call LoadTable(t_TDef, nTDef, _
  "T;exim;EXPORT AND IMPORTS:" _
  + "S;A;Value;($ billion, 2005 pp):" _
  + "S;A;/(N_?/1000);($ per capita):" _
  + "S;A;%(VV_?/1000);{per $100 of ppp GDP):" _
  + "R;XA$_?/1000;Primary commodity exports;ft.1:" _
  + "R;MA$_?/1000;Primary commodity imports;ft.1:" _
  + "R;BA$_?/1000;Primary commodity balance;ft.1:" _
  + "R;XE$_?/1000;Energy exports;ft.1:" _
  + "R;ME$_?/1000;Energy imports;ft.1:" _
  + "R;BE$_?/1000;Energy balance;ft.1:" _
  + "R;XM$_?/1000;Manufactures exports;ft.1:" _
  + "R;MM$_?/1000;Manufactures imports;ft.1:" _
  + "R;BM$_?/1000;Manufactures balance;ft.1:" _
  + "R;XS$_?/1000;Service exports;ft.1:" _
  + "R;MS$_?/1000;Service imports;ft.1:" _
  + "R;BS$_?/1000;Service balance;Uft.1:" _
  + "R;X$_?/1000;Goods and services exports;ft.1:" _
  + "R;M$_?/1000;Goods and services imports;ft.1:" _
  + "R;TB$_?/1000;Goods and services balance;ft.1:" _
  + "R;XIT$_?/1000;Income and transfer receipts;ft.1:" _
  + "R;MIT$_?/1000;Income and transfer payments;ft.1:" _
  + "R;BIT$_?/1000;Income and transfer balance;ft.1:" _
  + "R;CA$_?/1000;Current account balance;ft.1:" _
)

call LoadTable(t_TDef, nTDef, _
  "T;bop;BALANCE OF PAYMENTS:" _
  + "S;A;Value;($ billion, 2005 pp):" _
  + "S;A;%(VV$_?/1000);(% of GDP):" _
  + "R;BA$_?/1000;Primary commodities;ft.1:" _
  + "R;BE$_?/1000;Energy;ft.1:" _
  + "R;BM$_?/1000;Manufactures;ft.1:" _
  + "R;BS$_?/1000;Services;Uft.1:" _
  + "R;(BA$_?+BE$_?+BM$_?+BS$_?)/1000;Total goods and services;ft.1:" _
  + "R;BIT$_?/1000;Income and transfers;Uft.1:" _
  + "R;(BA$_?+BE$_?+BM$_?+BS$_?+BIT$_?)/1000;Current account;Uft.1:" _
  + "R;AXO$_?/1000;External assets (excl reserves);ft.1:" _
  + "R;LX$_?/1000;External liabilities;ft.1:" _
  + "R;(AXO$_?-LX$_?)/1000;Net external assets (excl reserves);ft.1:" _
  + "R;R$_?/1000;Reserves;ft.1:" _
)

call LoadTable(t_TDef, nTDef, _
  "T;lg;GOVERNMENT BUDGET AND DEBT:" _
  + "S;A;Value;($ billion, 2005 pp):" _
  + "S;A;%(VV_?/1000);(% of GDP):" _
  + "S;P;GR;(growth rates, % p.a.):" _
  + "R;VVTX_?/1000;Indirect taxes less subsidies;ft.1:" _
  + "R;YGD_?/1000;Other revenue less interest and transfers;ft.1:" _
  + "R;YG_?/1000;Disposable income (net);ft.1:" _
  + "R;G_?/1000;Expenditure on goods and services;ft.1:" _
  + "R;(YG_?-G_?)/1000;" _
    + "Financial surplus (+) or deficit (-);Uft.1;NGR:" _
  + "R;IAG_?/1000;Net acquisition of financial assets;ft.1;NGR:" _
  + "R;ILG_?/1000;" _
    + "Net incurrence of financial liabilities;Uft.1;NGR:" _
  + "R;LG_?/1000;Outstanding debt;ft.1:" _
  + "R;VV_?/1000;GDP;ft.1:" _
) 

call LoadTable(t_TDef, nTDef, _
  "T;pixr;INFLATION AND THE EXCHANGE RATE:" _
  + "S;A;Value;(index 2005 = 100):" _
  + "S;P;GR;(average change, % p.a.):" _
  + "R;phd_?*vveme_?/@elem(vveme_?,2005);" _
    + "1. Average nominal earnings;f.0:" _
  + "R;100*vveme_?/@elem(vveme_?,2005);" _
    + "2. Average real earnings;f.0:" _
  + "R;phd_?*(vvem_?/v_?)/@elem(vvem_?/v_?,2005);" _
    + "3. Unit labour cost (domestic ccy);f.0:" _
  + "R;pvd_?;4. Unit cost of GDP (domestic ccy);f.0:" _
  + "R;phd_?;5. Price of domestic expenditure;Uf.0:" _
  + "R;rxd_?;6. Exchange rate ($ per ccy unit);f.0:" _
  + "R;phd_?*rxd_?/100;7. Domestic prices in USD;f.0:" _
  + "R;100*ph_w~*ucx$_?;8. Unit cost of exports in USD;f.0:" _
  + "R;100*ph_w~;9. World prices in USD;Uf.0:" _
  + "R;100*rx_?/pp0_??;10. Real exchange rate;f.0:" _
  + "R;100*ucx$_?;11. Relative unit cost of exports;f.0:" _
)

call LoadTable(t_TDef, nTDef, _
  "T;eped;ENERGY SUPPLY AND USE:" _
  + "S;A;Value;(kg of oil equivalent):" _
  + "R;1000*EPC_?/N_?;Carbon energy supply per person;ft.0:" _
  + "R;1000*EPN_?/N_?;Non-carbon energy supply per person;ft.0:" _
  + "R;1000*EP_?/N_?;Total energy supply per person;ft.0:" _
  + "R;1000*ED_?/N_?;Energy use per person;Uft.0:" _
  + "R;1000*(EX_?-EM_?)/N_?;Energy balance per person;Uft.0:" _
  + "R;1000*ED_?/V_?;Energy use in kg per $ of ppp GDP;ft.2:" _
  + "R;1000*CO2_?/V_?;CO2 emissions in kg per $ of ppp GDP;ft.2:" _
)

endsub
