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



;=================================
; Macros

;=================================================================================
; Description:
; Parameters:
; Local Variables:
; Registers used:
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

    MOV     EDX, OFFSET str_Message
    CALL    WriteString

    POP     EDX
ENDM


mDisplayChar MACRO charValue
    PUSH EAX
    
    MOV AL, charValue
    CALL WriteChar

    POP EAX
ENDM



;=================================
; Global Variables
TEMPS_PER_DAY = 24
DELIMITER   EQU ','


.data

str_MsgPromptFileName           BYTE    "Give the name of file containing the Temperature Readings: ", 0


str_NameOfFile                  BYTE    100 DUP(0)                  ; Memory buffer for file name
file_TempReadings               BYTE    1000 DUP(255)               ; Memory buffer for file containing the temperature readings
                  
int_BufferSizeFileName          DWORD   99
int_LenNameOfFile               DWORD   ?                           ; Stores the number of bytes read
int_BufferSizeTemperatureFile   DWORD   999

;arr_TempMatrix                  DWORD   300 DUP(1), 0FFFFFFFFh


.code
main PROC

    ;=================================
    ; Get File name
    mGetString str_MsgPromptFileName, str_NameOfFile, int_BufferSizeFileName, int_LenNameOfFile
    mDisplayString str_NameOfFile


    ;=================================
    ; Open File
    MOV     EDX, OFFSET str_NameOfFile
    CALL    OpenInputFile

    MOV     ECX, int_BufferSizeTemperatureFile
    MOV     EDX, OFFSET file_TempReadings
    CALL    ReadFromFile

    ;=================================
    PUSH    OFFSET file_TempReadings
    CALL    ParseTempsFromString


; ==========================================================================================================================
; Extracts temperature readings, in string delimited format, from memory. Then saves the readings in an arrary as integers
; receives: Address of the Temperature array
; returns:
; preconditions: passed address references of array
; postconditions: values saved in array
; registers changed: none
; ==========================================================================================================================


	Invoke ExitProcess,0	; exit to operating system
main ENDP


ParseTempsFromString PROC
    LOCAL   arr_TempMatrix[200]:DWORD, str_CurntTemp[5]:BYTE, int_Len_Str_CrntTemp:DWORD, int_Sign:DWORD, int_RowIndex:DWORD, int_ColIndex:DWORD, \
            int_CrntTemp:DWORD, int_PrevDlmterPos:DWORD, offset_File_TempReadings:DWORD


    PUSH	EAX
    PUSH	EBX
    PUSH	ECX
    PUSH	EDX
    PUSH	ESI
    PUSH	EDI


    ; Stack Layout:
    ; [EBP + 8] = OFFSET file_TempReadings
    ; [EBP + 4] = return address
    ; [EBP] = old ebp

    ; move parameter to local variable
    MOV     EAX, [EBP + 8]
    MOV     offset_File_TempReadings, EAX

    ; Initialize array with value 1
    MOV ECX, LENGTHOF arr_TempMatrix                                  ; Loop counter (300 elements)
    LEA EDI, arr_TempMatrix                         ; Load address of the array into EDI
    MOV EAX, 1                                      ; Value to initialize (1)

_InitLoop:
    MOV DWORD PTR [EDI], EAX                        ; Store 1 at current position
    ADD EDI, 4                                      ; Move to the next DWORD (4 bytes)
    LOOP _InitLoop                                   ; Decrement ECX, loop if not zero


        MOV ECX, LENGTHOF arr_TempMatrix  ; Reset loop counter
    LEA ESI, arr_TempMatrix           ; Load base address of array

_PrintLoop:
    MOV EAX, DWORD PTR [ESI]          ; Load current array value
    CALL WriteDec                      ; Print number
    MOV AL, ' '
    CALL WriteChar

    ADD ESI, TYPE arr_TempMatrix       ; Move to next DWORD
    LOOP _PrintLoop                    ; Repeat until ECX = 0
    



    MOV     EDX, offset_File_TempReadings
    CALL    CrLf
    CALL    WriteString




    POP	    EDI
    POP 	ESI
    POP	    EDX
    POP	    ECX
    POP 	EBX
    POP	    EAX
    RET     4

ParseTempsFromString ENDP


; (insert additional procedures here)

END main