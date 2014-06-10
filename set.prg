'PROGRAM: set.prg          Copyright (C) 2012 Alphametrics Co. Ltd.
'
' CAM version 5.0 FEPS variant
'
' settings
'
' this program fragment is included in all executable programs
' it initialises the system and provides common settings
'
' updated: FC 10/05/2013
'
'=======================================================

'--- set the current directory to this area and load libraries
%s = @runpath
cd {%s}
include "zlib"
include "zlibg"
include "zlibr"
include "zlibs"
include "zdef"
include "zrep"
include "zimpact"

'--- title for reports
%sysTitle = "CAM 5.0 with FEPS blocs"

'--- data source (historical series)
!nSeries = 3066

'--- default simulation horizon
%predict = "2030"
'--- start year for simulation charts and reports
%repstart = "1980"
'--- columns in simulation tables
%yrsol = "2000 2008 2010 2011 2012 2015 2020 2030"

'--- historical data and alignment periods
%start = "1970"               ' first year of historical data
%base = "2005"                ' base year
%latest = "2011"              ' latest historical data
%align = "2012"               ' default alignment horizon
%end    = "2030"              ' end of workfile

'--- columns in history and alignment tables
%yrdat = "1980 1990 2000 2005 2008 2009 2010 2011"
%yrext = "1990 2000 2007 2008 2009 2010 2011 2012"

'--- graph coverage
'    B blocs, C countries only, O other blocs only,
'    G groups, W world
%repgeo = "BG"
%subgraphs = "No"

'--- blocs
!nCountry = 11
%blocs = _
       + "us;USA:" _
       + "uk;UK:" _
       + "de;Germany:" _
       + "fr;France:" _
       + "it;Italy:" _
       + "es;Spain:" _
       + "pl;Poland:" _
       + "tr;Turkey:" _
       + "ja;Japan:" _
       + "cn;China:" _
       + "in;India:" _
       + "eun;North Europe:" _
       + "euw;Other West Europe:" _
       + "eus;Other South Europe:" _
       + "eue;Other East Europe:" _
       + "od;Other Developed:" _
       + "eah;East Asia High Income:" _
       + "ci;CIS:" _
       + "wa;West Asia:" _
       + "ams;South America:" _
       + "acx;Central America:" _
       + "eao;Other East Asia:" _
       + "aso;Other South Asia:" _
       + "afn;North Africa:" _
       + "afs;Other Africa"

'--- bloc aggregates
%groups = "eu;Europe;uk de fr it es pl eun euw eus eue:" _
        + "am;America;us od acx ams:" _
        + "af;Africa;afn afs:" _
        + "oa;Other Asia;ci tr wa in aso:" _
        + "ea;East Asia;ja cn eah eao:" _
        + "cwa;European neighbours;ci tr wa afn:" _
        + "nam;North America;us od acx:"

'=======================================================
