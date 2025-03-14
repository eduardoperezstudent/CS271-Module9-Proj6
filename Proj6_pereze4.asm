TITLE Temp List Reverser     (Proj6_pereze4.asm)

; Author: Eduardo Perez
; Last Modified: March 16, 2025
; OSU email address: pereze4@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6               Due Date: March 16, 2025
; Description: This program loads a text file. The file contains Temperature values, each separated by a delimiter.
; Each Temperature values are extracted, converted to it's integer value format, and then stored in an array.
; The values are then displayed in reverse order as they are stored.
; Implementation note 1: LODSB is used in detecting delimiter positions and presence of Cr Lf. 

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

;=================================================================================
; Description:
; Parameters:
; Local Variables:
; Registers used:

mDisplayString MACRO str_Message
    PUSH    EDX

    MOV     EDX, OFFSET str_Message
    CALL    WriteString

    POP     EDX
ENDM


;=================================================================================
; Description:
; Parameters:
; Local Variables:
; Registers used:

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

str_MsgNumberofRows             BYTE    "The number of rows: ", 0
str_MsgNumberofColumns          BYTE    "The number of columns: ", 0
str_MsgPrevDlmtrPos             BYTE    "The previous demimiter position: ", 0
str_MsgCrntDlmtrPos             BYTE    "The current demimiter position: ", 0
str_MsgLoadedFile               BYTE    "The Loaded file: ", 0

;arr_TempMatrix                  DWORD   300 DUP(1), 0FFFFFFFFh


.code
main PROC

    ;=================================
    ; Get File name
    mGetString str_MsgPromptFileName, str_NameOfFile, int_BufferSizeFileName, int_LenNameOfFile



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
    LOCAL   arr_TempMatrix[200]:DWORD, str_CurntTemp[5]:BYTE, int_Len_Str_CrntTemp:DWORD, int_Sign:DWORD, int_RowIndex:DWORD, int_ColIndex:DWORD, int_CrntTemp:DWORD, int_PrevDlmterPos:DWORD, int_CrntDlmterPos:DWORD, offset_File_TempReadings:DWORD, int_LenMatrix:DWORD, int_WidthMatrix:DWORD

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

    ;MOV     EDX, offset_File_TempReadings
    ;CALL    CrLf
    ;CALL    WriteString


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

    ;==================================================================
    ; Get current delimiter position
    
    PUSH    offset_File_TempReadings
    LEA     EAX, int_CrntDlmterPos
    PUSH    EAX
    MOV     EAX, 0
    MOV     int_PrevDlmterPos, EAX
    PUSH    int_PrevDlmterPos
    CALL    get_NextDlmtrPos
    ;!!!!! After current line is done, in the loop, need to add 2 to int_PrevDlmterPos to compensate for CrLf

   ;==================================================================
    

    POP	    EDI
    POP 	ESI
    POP	    EDX
    POP	    ECX
    POP 	EBX
    POP	    EAX
    RET     4

ParseTempsFromString ENDP





; ==========================================================================================================================
; Searches for the next delimiter in the file buffer
; receives: Address of the buffered file, offset of the matrix Length and Width placeholder
; returns:
; preconditions: passed address offsets
; postconditions: values saved in memory
; registers changed: none
; ==========================================================================================================================
get_NextDlmtrPos PROC
    LOCAL crntDlmtrPos:DWORD
    LOCAL prevDlmterPos:DWORD
    LOCAL offset_File_TempReadingsLoc:DWORD

    PUSH    EAX
    PUSH    EBX
    PUSH    ECX
    PUSH    EDX
    PUSH    ESI
    PUSH    EDI

    ; Parameters:
    ;   [EBP+8]  : int_PrevDlmterPos
    ;              (DWORD) The index of the previous delimiter in the current row.
    ;   [EBP+12] : offset_Int_CrntDlmtrPos
    ;              (DWORD) The address in memory where the result (the current delimiter index) will be saved.
    ;   [EBP+16] : offset_File_TempReadings
    ;              (DWORD) The address of the file saved in memory buffer (Temperature readings).

    ; Store passed parameters in local variables.
    MOV     EAX, [EBP+8]  
    MOV     prevDlmterPos, EAX                ; Save previous delimiter position.

    MOV     EDX, OFFSET str_MsgPrevDlmtrPos
    CALL    CrLf
    CALL    WriteString
    MOV     EAX, prevDlmterPos
    CALL    WriteDec

    MOV     EAX, [EBP+16]
    MOV     offset_File_TempReadingsLoc, EAX  ; Save file buffer base address.

    MOV     EDX, OFFSET str_MsgLoadedFile
    CALL    CrLF
    CALL    WriteString
    MOV     EDX, offset_File_TempReadingsLoc
    CALL    CrLF
    CALL    WriteString

    ; Compute starting address for search:
    ; Start searching at (prevDlmterPos + 1) relative to the file buffer.
    MOV     EAX, prevDlmterPos
    ADD     EAX, 1                                      ; Next search index.
    MOV     EBX, offset_File_TempReadingsLoc            ; Load base address.
    ADD     EBX, EAX                                    ; EBX now points to the search start position.
    MOV     ESI, EBX                                    ; Set ESI to the starting search pointer.

    ; Search for next delimiter value in the file
    _searchLoop2:
        LODSB                                           ; Load byte at [ESI] into AL; ESI increments automatically.
        CMP     AL, DELIMITER                           ; Compare byte with the delimiter
        JE      _found_Dlmtr
        JMP     _searchLoop2                            ; Continue scanning

    ; Jump here once the next delimiter is found
    _found_Dlmtr:
        ; ESI now points one byte past the found delimiter.
        MOV     EAX, ESI
        DEC     EAX                                     ; Adjust: EAX now points to the delimiter itself.
        MOV     crntDlmtrPos, EAX                       ; Store current delimiter absolute position.

        ; Calculate the delimiter's index relative to the file buffer.
        MOV     ECX, offset_File_TempReadingsLoc        ; Base address.
        SUB     crntDlmtrPos, ECX                       ; crntDlmtrPos now holds the index.


        ; Check for CRLF between previous delimiter and current delimiter.
        ; Scan from (prevDlmterPos + 1) up to the found delimiter index.
        MOV     EAX, prevDlmterPos
        ADD     EAX, 1                                  ; Starting index for scan.
        MOV     EBX, crntDlmtrPos                       ; EBX holds current delimiter index.
        MOV     EDI, EAX                                ; EDI is our scanning index.

    _adjustLoop:
        CMP     EDI, EBX
        JGE     _doneAdjust                             ; If scanning index >= current delimiter index, finish.
        MOV     AL, BYTE PTR [offset_File_TempReadingsLoc + EDI]
        CMP     AL, 0Dh
        JE      _checkLF
        INC     EDI
        JMP     _adjustLoop

    _checkLF:
        CMP     BYTE PTR [offset_File_TempReadingsLoc + EDI + 1], 0Ah
        JE      _foundCRLF
        INC     EDI
        JMP     _adjustLoop

    _foundCRLF:
        ADD     crntDlmtrPos, 2            ; Adjust current delimiter index by adding 2.


    _doneAdjust:
        ; Save the result in the memory location pointed to by offset_Int_CrntDlmtrPos.
        MOV     EAX, crntDlmtrPos
        MOV     EDX, [EBP+12]
        MOV     [EDX], EAX

        ; Debug printouts:
        MOV     EDX, OFFSET str_MsgCrntDlmtrPos
        CALL    CrLf
        CALL    WriteString
        CALL    WriteDec


        POP     EDI
        POP     ESI
        POP     EDX
        POP     ECX
        POP     EBX
        POP     EAX
        RET     12
get_NextDlmtrPos ENDP




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


    ; Parameters
    ; [EBP + 16] int_WidthMatrix
    ; [EBP + 12] int_LenMatrix
    ; [EBP + 8]  offset_File_TempReadings

    ; Load file address into ESI
    MOV     EAX, [EBP + 8]
    MOV     offset_File_TempReadings, EAX
    MOV     ESI, offset_File_TempReadings       ; Had to abandon use of local variable, use ESI instead to make use of LODSB

    ; Initialize counters and flag
    MOV     int_NumRows, 0
    MOV     int_NumCols, 0                      ; Column count is based on delimiters (+ 1 if non-empty)
    MOV     rowHasData, 0

    ; ----------------------------------------------------------------
    ; Process the first row: count columns and check for non-empty line
    _countCols:
        LODSB                                   ; Load next byte from [ESI] into AL and increment ESI
        MOV     BL, AL                          ; Save the character in BL

        CMP     BL, 0                           ; End-of-file?
        JE      _end_Get_MatrixSize

        CMP     BL, 0Dh                          ; Skip CR (Carriage Return)
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

     
    ; check if final row has data
    _end_Get_MatrixSize:
    CMP     rowHasData, 1
    JE      _addFinalRow
    JMP    _storeCounts 


    ; Jump here if final Row has data to count it
    _addFinalRow:
        INC     int_NumRows
        JMP     _storeCounts

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



get_MatrixSize ENDP





END main