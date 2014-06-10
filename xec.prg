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
include "sol0pj"
include "sol0"
include "solE1"
include "solE1a"
include "solE2"
include "solE2a"
include "solE3"
include "solE3a"
include "solE4"
include "solE4a"

'--- execute programs
'call dat
'call est
'close dat
'call sola
'close est
call sol0p
call sol0pj
'close sola
'call sol0
'close sol0p
'call solE1
'close sol0
'call solE1a
'close sole1
'call solE2
'close sole1a
'call solE2a
'close sole2
'call solE3
'close sole2a
'call solE3a
'close sole3
'call solE4
'close sole3a
'call solE4a
'close sole4
