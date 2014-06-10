'PROGRAM: set.prg          Copyright (C) 2014 Alphametrics Co. Ltd.
'
' CAM Version 5.2 EUR variant
'
' settings
'
' this program fragment is included in all executable programs
' it initialises the system and provides common settings
'
' updated: FC 05/06/2013
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
%sysTitle = "CAM 5.2 with ASIA blocs"

'--- data source (historical series)
!nSeries = 3008

'--- default simulation horizon
%predict = "2030"
'--- start year for simulation charts and reports
%repstart = "1980"
'--- columns in simulation tables
%yrsol = "2000 2008 2010 2011 2012 2015 2020 2030"

'--- historical data and alignment periods
%start = "1970"               ' first year of historical data
%base = "2005"                ' base year
%latest = "2012"              ' latest historical data
%align = "2014"               ' default alignment horizon
%end    = "2030"              ' end of workfile

'--- columns in history and alignment tables
%yrdat = "1980 1990 2000 2008 2009 2010 2011 2012"
%yrext = "1990 2000 2008 2009 2010 2011 2012 2014"

'--- graph coverage
'    B blocs, F first blocs, O other blocs,
'    G groups, W world
%repgeo = "FO"
%subgraphs = "No"

'--- blocs
!nFirstBlocs = 6
%blocs = "" _
  + "euc;Core eurozone:" _
  + "fr;France:" _
  + "eup;Eurozone periphery:" _
  + "enc;North Europe non-euro:" _
  + "uk;United Kingdom:" _
  + "ene;East Europe non-euro:" _
  + "tr;Turkey:" _
  + "cis;CIS and other:" _
  + "nwa;North Africa and West Asia:" _
  + "afl;Africa Low Income:" _
  + "za;South Africa:" _
  + "in;India:" _
  + "osa;Other South Asia:" _
  + "cn;China:" _
  + "ja;Japan:" _
  + "oeh;Other East Asia High Income:" _
  + "oea;Other East Asia:" _
  + "can;Canada,Australia,New Zealand:" _
  + "us;USA:" _
  + "acx;Central America and Caribbean:" _
  + "br;Brazil:" _
  + "oam;Other South America:" _
  + ""

'--- bloc aggregates
%groups = "" _
 + "EU;Europe;euc fr eup enc uk ene:" _
 + "CMEA;CIS, Middle East and Africa;" _
    + "cis tr nwa afl za:" _
 + "ROA;Rest of Asia;" _
    + "ja cn oeh oea in osa:" _
 + "RAM;America;" _
    + "can us acx br oam:" _
 + ""
'=======================================================
