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
DELIMITER   EQU 3Bh


.data

str_MsgPromptFileName           BYTE    "Give the name of file containing the Temperature Readings: ", 0


str_NameOfFile                  BYTE    100 DUP(0)                  ; Memory buffer for file name
file_TempReadings               BYTE    1000 DUP(255)               ; Memory buffer for file containing the temperature readings
                  
int_BufferSizeFileName          DWORD   99
int_LenNameOfFile               DWORD   ?                           ; Stores the number of bytes read
int_BufferSizeTemperatureFile   DWORD   999

str_MsgNumberofRows             BYTE    "The number of rows: ", 0
str_MsgNumberofColumns          BYTE    "The number of columns: ", 0

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

    CALL    CrLf
    MOV     EDX, OFFSET str_MsgNumberofRows
    CALL    WriteString
    MOV     EAX, int_LenMatrix
    CALL    WriteDec

    CALL    CrLf
    MOV     EDX, OFFSET str_MsgNumberofColumns
    CALL    WriteString
    MOV     EAX, int_WidthMatrix
    CALL    WriteDec



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

    LOCAL int_NumRows:DWORD
    LOCAL int_NumCols:DWORD
    LOCAL offset_File_TempReadings:DWORD
    LOCAL rowHasData:BYTE

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

    ; Initialize counters and flag
    MOV     int_NumRows, 0
    MOV     int_NumCols, 0        ; Column count is based on delimiters (+ 1 if non-empty)
    MOV     rowHasData, 0

    ; ----------------------------------------------------------------
    ; Process the first row: count columns and check for non-empty line
    _countCols:
        LODSB                               ; Load next byte from [ESI] into AL and increment ESI
        MOV     BL, AL                       ; Save the character in BL

        CMP     BL, 0                       ; End-of-file?
        JE      _end_Get_MatrixSize

        CMP     BL, 0Dh                     ; Skip CR (Carriage Return)
        JE      _countCols

        CMP     BL, 0Ah                     ; LF indicates end-of-line
        JE      _finishFirstRow

        ; Non CR/LF character: mark the row as non-empty
        MOV     rowHasData, 1

        CMP     BL, DELIMITER                     ; Check for delimiter
        JNE     _continueCols
        INC     int_NumCols

    ; Jump here if encountered delimiter
    _continueCols:
        MOV     AL, BL
        JMP     _countCols

    ; At the end of the first row, count it if it contains data.
    _finishFirstRow:

    CMP     rowHasData, 1
    JNE     _skipFirstRow
    INC     int_NumRows

    ; Assume file has at least one row
    _skipFirstRow:
        MOV     rowHasData, 0               ; Reset flag for next row


    ; Process subsequent rows
    _countRows:
        LODSB                               ; Load next byte
        CMP     AL, 0                       ; End-of-file?
        JE      _end_Get_MatrixSize
        CMP     AL, 0Dh                     ; Skip CR (Carriage Return)
        JE      _countRows
        CMP     AL, 0Ah                     ; LF indicates end-of-line
        JE      _endOfRow
        ; Non CR/LF character: mark the row as non-empty
        MOV     rowHasData, 1
        JMP     _countRows

    ; At the end of a row, count it if it contains data.
    _endOfRow:
    CMP     rowHasData, 1
    JNE     _resetRow
    INC     int_NumRows

    ; Reset flag for the next row
    _resetRow:
        MOV     rowHasData, 0       
        JMP     _countRows

     
     ; Calculations done. Store matrix size info
    _end_Get_MatrixSize:
    CMP     rowHasData, 1
    JE      _addFinalRow

    ; Store the row and column counts in the provided output addresses.
    _storeCounts:

    MOV     EAX, int_NumRows
    MOV     EDI, [EBP + 12]
    MOV     DWORD PTR [EDI], EAX

    MOV     EAX, int_NumCols
    MOV     EDI, [EBP + 16]
    MOV     DWORD PTR [EDI], EAX

    POP     EDI
    POP     ESI
    POP     EDX
    POP     ECX
    POP     EBX
    POP     EAX
    RET     12

    ; add final row to the count. 
    _addFinalRow:
        INC     int_NumRows
        JMP     _storeCounts

get_MatrixSize ENDP


END main