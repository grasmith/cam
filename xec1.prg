'BATCH EXECUTION
'
'This program may be used to execute CAM programs sequentially
' without stopping.
'
'The following statements in each program file listed here must be
'commented out:
'
' include "set"
' call <xxx>             where <xxx> is the program name

' If these statements are not commented out, EViews will stop
' with an error message.
'
'===================================================================

'--- include configs and libraries
include "set"
'--- include program files
include "sol0"
include "solN1"
include "solN2"

'--- execute programs
call sol0
call solN1
call solN2
