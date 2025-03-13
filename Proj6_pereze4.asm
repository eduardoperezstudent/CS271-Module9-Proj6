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
    PUSH    EAX
    PUSH    ECX
    PUSH    EDX
    ; Display prompt message
    MOV EDX, OFFSET str_Message
    CALL WriteString  
    
    ; Get user input
    MOV EDX, OFFSET buffer
    MOV ECX, bufferSize
    CALL ReadString  

    ; Store number of bytes read
    MOV fileByteSize, EAX

    POP     EDX
    POP     ECX
    POP     EAX
ENDM

mDisplayString MACRO str_Message
    PUSH    EDX

    MOV     EDX, OFFSET str_Message  ; Load string address into EDX
    CALL    WriteString             ; Display the string

    POP     EDX
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

str_MsgPromptFileName           BYTE    "Give the name of file containing the Temperature Readings: ", 0


str_NameOfFile                  BYTE    100 DUP(0)                  ; Memory buffer for file name
str_TemperatureFile             BYTE    1000 DUP(255)               ; Memory buffer for file containing the temperature readings
                  
int_BufferSizeFileName          DWORD   99
int_LenNameOfFile               DWORD   ?                           ; Stores the number of bytes read
int_BufferSizeTemperatureFile   DWORD   999




.code
main PROC

    ; Get File name
    mGetString str_MsgPromptFileName, str_NameOfFile, int_BufferSizeFileName, int_LenNameOfFile
    mDisplayString str_NameOfFile

    ; Open File
    MOV     EDX, OFFSET str_NameOfFile
    CALL    OpenInputFile



    MOV     ECX, int_BufferSizeTemperatureFile
    MOV     EDX, OFFSET str_TemperatureFile
    CALL    ReadFromFile

    MOV     EDX, OFFSET str_TemperatureFile
    CALL    CrLf
    CALL    WriteString




	Invoke ExitProcess,0	; exit to operating system
main ENDP

; (insert additional procedures here)

END main