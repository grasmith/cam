 'BATCH EXECUTION
'
'This program may be used to execute CAM programs sequentially without
'stopping.
'
'To select programs for execution, remove comments before call statements.
'
'NB the first two statements in each program file listed in the include
'section below must be commented out. If they are not commented out, EViews
'attempts to include the "set" file twice and stops with an error message.
'
'        e.g. in the file dat.prg comment the lines
'                                   include "set"
'                                   call dat
'=========================================================================

'--- include configs and libraries
include "set"
'--- multi-bloc budget (special)
include "mb"
'--- program files
include "dat"
include "est"
include "sola"
include "sol0p"
include "sol0"
include "solE1"
include "solE1a"
include "solE2"
include "solE2a"
include "solE3"
include "solE4"
include "solE4a"

'--- execute programs
call dat
call est
call sola
call sol0p
call sol0
call solE1
call solE1a
call solE2
call solE2a
call solE3
call solE4
call solE4a
