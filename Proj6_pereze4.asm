TITLE Temp List Reverser     (Proj6_pereze4.asm)

; Author: Eduardo Perez
; Last Modified: March 16, 2025
; OSU email address: pereze4@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6               Due Date: March 16, 2025
; Description: This program loads a text file. The file contains Temperature values, each separated by a delimiter.
; Each Temperature values are extracted, converted to it's integer value format, and then stored in an array.
; The values are then displayed in reverse order as they are stored.

INCLUDE Irvine32.inc

mGetString MACRO str_Message, buffer, bufferSize, fileByteSize
    ; Display prompt message
    MOV EDX, OFFSET str_Message
    CALL WriteString  
    
    ; Get user input
    MOV EDX, OFFSET buffer
    MOV ECX, bufferSize
    CALL ReadString  

    ; Store number of bytes read
    MOV fileByteSize, EAX
ENDM

mDisplayString MACRO str_Message
    MOV EDX, OFFSET str_Message  ; Load string address into EDX
    CALL WriteString             ; Display the string
ENDM


mDisplayChar MACRO charValue
    PUSH EAX
    
    MOV AL, charValue
    CALL WriteChar

    POP EAX
ENDM

TEMPS_PER_DAY = 24
DELIMITER   EQU ','


.data

str_FileName                    BYTE    100 DUP(0)                  ; Memory buffer file name
str_MsgPromptFileName           BYTE    "Enter a string: ", 0
                  
int_BufferSize                  DWORD   99
int_inputFileByteSize           DWORD   ?                           ; Stores the number of bytes read

str_TestChar                    BYTE    "@",0




.code
main PROC

mGetString str_MsgPromptFileName, str_FileName, int_BufferSize, int_inputFileByteSize

mDisplayString str_FileName







	Invoke ExitProcess,0	; exit to operating system
main ENDP

; (insert additional procedures here)

END main