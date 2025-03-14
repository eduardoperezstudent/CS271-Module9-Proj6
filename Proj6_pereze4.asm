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




	Invoke ExitProcess,0	; exit to operating system
main ENDP



; ==========================================================================================================================
; Extracts temperature readings, in string delimited format, from memory. Then saves the readings in an arrary as integers
; receives: Address of the Temperature array
; returns:
; preconditions: passed address references of array
; postconditions: values saved in array
; registers changed: none
; ==========================================================================================================================
ParseTempsFromString PROC
    LOCAL   arr_TempMatrix[200]:DWORD, str_CurntTemp[5]:BYTE, int_Len_Str_CrntTemp:DWORD, int_Sign:DWORD, int_RowIndex:DWORD, int_ColIndex:DWORD, int_CrntTemp:DWORD, int_PrevDlmterPos:DWORD, offset_File_TempReadings:DWORD, int_LenMatrix:DWORD, int_WidthMatrix:DWORD

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

    MOV     EDX, offset_File_TempReadings
    CALL    CrLf
    CALL    WriteString


    ;==================================================================
    ; Initialize Temp Matrix with value 1
    MOV ECX, LENGTHOF arr_TempMatrix                                  ; Loop counter (300 elements)
    LEA EDI, arr_TempMatrix                         ; Load address of the array into EDI
    MOV EAX, 1                                      ; Value to initialize (1)

    ; Loop to initialize Temp Matrix
    _InitLoop:
        MOV DWORD PTR [EDI], EAX                        ; Store 1 at current position
        ADD EDI, 4                                      ; Move to the next DWORD (4 bytes)
        LOOP _InitLoop                                   ; Decrement ECX, loop if not zero


        MOV ECX, LENGTHOF arr_TempMatrix  ; Reset loop counter
    LEA ESI, arr_TempMatrix           ; Load base address of array

    ; Print array
    ;_PrintLoop:
    ;    MOV EAX, DWORD PTR [ESI]          ; Load current array value
    ;    CALL WriteDec                      ; Print number
    ;    MOV AL, ' '
    ;    CALL WriteChar

    ;    ADD ESI, TYPE arr_TempMatrix       ; Move to next DWORD
    ;    LOOP _PrintLoop                    ; Repeat until ECX = 0


    ;==================================================================
    ; Get Matrix Size
    LEA     EAX, int_WidthMatrix
    PUSH    EAX
    LEA     EAX, int_LenMatrix
    PUSH    EAX
    PUSH    offset_File_TempReadings
    CALL    get_MatrixSize


    ;MOV     EAX, int_LenMatrix
    ;CALL    CrLf
    ;CALL    WriteDec

    ;MOV     EAX, int_WidthMatrix
    ;CALL    CrLf
    ;CALL    WriteDec
    

    POP	    EDI
    POP 	ESI
    POP	    EDX
    POP	    ECX
    POP 	EBX
    POP	    EAX
    RET     4

ParseTempsFromString ENDP



; ==========================================================================================================================
; Gathers the Length and Width og the matrix
; receives: Address of the buffered file, offset of the matrix Length and Width placeholder
; returns:
; preconditions: passed address offsets
; postconditions: values saved in memory
; registers changed: none
; ==========================================================================================================================

get_MatrixSize PROC

    LOCAL int_NumRows:DWORD, int_NumCols:DWORD, offset_File_TempReadings:DWORD

    PUSH    EAX
    PUSH    EBX
    PUSH    ECX
    PUSH    EDX
    PUSH    ESI
    PUSH    EDI

    ; Load file address into ESI
    MOV     EAX, [EBP + 8]
    MOV     offset_File_TempReadings, EAX
    MOV     ESI, offset_File_TempReadings

    ; Initialize counters
    MOV     int_NumRows, 0
    MOV     int_NumCols, 0    ; Column count will be (commas + 1)

    ; ----------------------------------------------------------------
    ; Count columns in the first row
_CountCols:
    LODSB                   ; Load next byte from [ESI] into AL and increment ESI
    MOV     BL, AL        ; Save the original byte in BL for comparison

    CMP     BL, 0         ; Check for end-of-file (null terminator)
    JE      _Done

    CMP     BL, 0Dh       ; Check for CR (Carriage Return)
    JE      _CountCols    ; Skip CR by reading the next byte

    CMP     BL, 0Ah       ; Check for LF (Line Feed)
    JE      _NextRow      ; End of first row reached

    CMP     BL, ','       ; Check for a comma (column separator)
    JNE     _ContinueCols
    INC     int_NumCols   ; Increment column count when comma is found

_ContinueCols:
    ; Optionally display the character (or debug output)
    MOV     AL, BL        ; Restore AL with the original byte
    CALL    CrLf
    CALL    WriteChar

    JMP     _CountCols    ; Continue scanning the first row

    ; ----------------------------------------------------------------
    ; Count rows (after the first row)
_NextRow:
    INC     int_NumRows   ; New row detected

_CountRows:
    LODSB                   ; Load next byte
    CMP     AL, 0         ; End-of-file?
    JE      _Done
    CMP     AL, 0Dh       ; Skip CR if found
    JE      _CountRows
    CMP     AL, 0Ah       ; Check for LF to count additional rows
    JE      _NextRow
    JMP     _CountRows

    ; ----------------------------------------------------------------
    ; Store row and column counts in output variables
_Done:
    MOV     EAX, int_NumRows
    MOV     EDI, [EBP + 12]
    MOV     DWORD PTR [EDI], EAX

    CALL    CrLf
    CALL    WriteDec

    MOV     EAX, int_NumCols
    MOV     EDI, [EBP + 16]
    MOV     DWORD PTR [EDI], EAX

    CALL    CrLf
    CALL    WriteDec

    ; Restore registers and return
    POP     EDI
    POP     ESI
    POP     EDX
    POP     ECX
    POP     EBX
    POP     EAX
    RET 12

get_MatrixSize ENDP



END main