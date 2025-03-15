TITLE Temp List Reverser     (Proj6_pereze4.asm)

; Author: Eduardo Perez
; Last Modified: March 16, 2025
; OSU email address: pereze4@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6               Due Date: March 16, 2025
; Description: This program loads a text file. The file contains Temperature values, each separated by a delimiter.
; Each Temperature values are extracted ONE AT A TIME; converted to it's integer value format, and then stored in an array.
; The values are then displayed in reverse order as they are stored.
; Implementation note 1:    LODSB is used in detecting delimiter positions and presence of Cr Lf. 
;                           MOVSB is utilized in extracting current Temp reading iteration from the file.
;                           LODSB is utlized to convert a Temp reading from str to int format
; Implementation note 2:    Can accept Temp readings with prefix '+' or '-', and/or a leading zero

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
str_MsgPrevDlmtrPos             BYTE    "The previous delimiter position: ", 0
str_MsgCrntDlmtrPos             BYTE    "The current delimiter position: ", 0
str_MsgLoadedFile               BYTE    "The Loaded file: ", 0
str_MsgCurrentTempIteration     BYTE    "The extracted current Temperature reading: ", 0
str_MsgSign                     BYTE    "The sign bit of the extracted current Temperature reading: ", 0
str_MsgSignRemoved              BYTE    "The extracted current Temperature reading, sign removed if any: ", 0
str_MsgConvertedInt             BYTE    "The converted integer value: ", 0
str_MsgAfterSignCheck           BYTE    "The integer value after the sign check: ", 0


;arr_TempMatrix                  DWORD   300 DUP(1), 0FFFFFFFFh


; For debugging PROC GetSign
STR_MSGPOINTER         BYTE "POINTER VALUE: ",0
STR_MSGPASSEDSTRING    BYTE "PASSED STRING: ",0
STR_MSGFIRSTCHAR       BYTE "FIRST CHAR: ",0
STR_MSGASCII           BYTE " ASCII: ",0
STR_MSGSHIFTING        BYTE "SHIFTING STRING...",0
STR_MSGSHIFTEDSTRING   BYTE "SHIFTED STRING: ",0
STR_MSGINTSIGN         BYTE "INT SIGN: ",0

; For debugging PROC ConvertStrToInteger
STR_MSGLENGTH        BYTE "LENGTH: ",0
STR_MSGCONVERTING    BYTE "CONVERTING STRING:",0
STR_MSGCONVERTED     BYTE "CONVERTED INTEGER: ",0






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



    ; Initialize temp matrix
    LEA     EAX, arr_TempMatrix
    PUSH    EAX
    MOV     EAX, LENGTHOF arr_TempMatrix
    PUSH    EAX
    CALL    Init_TempMatrix


    ;==================================================================
    ; Get File Size
    LEA     EAX, int_WidthMatrix
    PUSH    EAX
    LEA     EAX, int_LenMatrix
    PUSH    EAX
    PUSH    offset_File_TempReadings
    CALL    Get_MatrixSize

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
    MOV     EAX, 2
    MOV     int_PrevDlmterPos, EAX
    PUSH    int_PrevDlmterPos
    CALL    Get_NextDlmtrPos
    ;!!!!! After current line is done, in the loop, need to add 2 to int_PrevDlmterPos to compensate for CrLf

   
   ;==================================================================
   ; Get an iteration of Temp reading, save as string
   MOV      EAX, offset_File_TempReadings
   PUSH     EAX
   LEA      EAX, str_CurntTemp
   PUSH     EAX
   MOV      EAX, int_PrevDlmterPos
   PUSH     EAX
   MOV      EAX, int_CrntDlmterPos
   PUSH     EAX
   CALL     Extract_StrCrntTemp
   MOV      EDX, OFFSET str_MsgCurrentTempIteration
   CALL     CrLf
   CALL     WriteString
   LEA      EDX, str_CurntTemp
   CALL     WriteString


   ;==================================================================
   ; Detect the sign, store this info then removes the sign from the string
    LEA      EAX, int_Sign
    PUSH     EAX
    LEA      EAX, str_CurntTemp
    PUSH     EAX
    CALL     GetSign
    ; Debug Print
    CALL    CrLf
    MOV     EDX, OFFSET str_MsgSign
    CALL    WriteString
    MOV     EAX, int_Sign
    CALL    WriteDec
    CALL    CrLf     
    MOV     EDX, OFFSET str_MsgSignRemoved
    CALL    WriteString
    LEA     EDX, str_CurntTemp
    CALL    WriteString
    CALL    CrLf


    
   ;==================================================================
   ; Convert current Temp Reading to Integer

    LEA     EAX, int_CrntTemp
    PUSH    EAX
    LEA     EAX, str_CurntTemp
    PUSH    EAX
    CALL    ConvertStringToInteger
    ; Debugging printouts
    CALL    CrLf
    MOV     EDX, OFFSET str_MsgConvertedInt
    CALL    WriteString
    MOV     EAX, int_CrntTemp
    CALL    WriteInt


   ;==================================================================
   ; Check if reference sign bit is set. 
   ; If yes, negate the  stored integer Temp reading
    
    MOV     EAX, int_Sign
    CMP     EAX, 1
    JNE     _Save_ToTempMatrix
    LEA     EAX, int_CrntTemp
    PUSH    EAX
    CALL    Negate_CurntTemp


    _Save_ToTempMatrix:
    ; Debugging printouts
    CALL    CrLf
    MOV     EDX, OFFSET str_MsgAfterSignCheck
    CALL    WriteString
    MOV     EAX, int_CrntTemp
    CALL    WriteInt


    ;   Cleanup then Finish Proc
    POP	    EDI
    POP 	ESI
    POP	    EDX
    POP	    ECX
    POP 	EBX
    POP	    EAX
    RET     4

ParseTempsFromString ENDP


; ==========================================================================================================================
; Negates an unsigned Integer
; receives: Address of the current Temperature reading, an integer
; returns:
; preconditions: passed address references of array
; postconditions: values saved in array
; registers changed: none
; ==========================================================================================================================
Negate_CurntTemp PROC
    LOCAL originalValue:DWORD, negatedValue:DWORD

    PUSH    EAX
    PUSH    EBX

    ;------------------------------------------------------------
    ; PARAMETER HANDLING:
    ; [EBP+8] : offset_Int_CurntTemp - pointer to the integer value to be negated.
    ;------------------------------------------------------------
    MOV     EAX, [EBP+8]                ; EAX = pointer to the integer.
    MOV     EBX, [EAX]                  ; Load the original integer value.
    MOV     originalValue, EBX


    ;------------------------------------------------------------
    ; DEBUG PRINT: Print original integer value (optional).
    ; MOV     EDX, OFFSET str_MsgOriginal  ; "ORIGINAL VALUE: "
    ; CALL    CrLf
    ; CALL    WriteString
    ; MOV     EAX, originalValue
    ; CALL    WriteInt
    ; CALL    CrLf

    ;------------------------------------------------------------
    ; NEGATE THE INTEGER:
    MOV     EBX, originalValue          ; Copy original value to EBX.
    NEG     EBX                         ; EBX = -originalValue.
    MOV     negatedValue, EBX           ; Store the negated value in local variable.

    ;------------------------------------------------------------
    ; STORE THE NEGATED VALUE:
    MOV     EAX, [EBP + 8]
    MOV     [EAX], EBX        ; Write negated value back to memory.

    ;------------------------------------------------------------
    ; DEBUG PRINT: Print negated integer value (optional).
    ; MOV     EDX, OFFSET str_MsgNegated   ; "NEGATED VALUE: "
    ; CALL    CrLf
    ; CALL    WriteString
    ; MOV     EAX, negatedValue
    ; CALL    WriteInt
    ; CALL    CrLf


    POP     EBX
    POP     EAX
    RET     4
Negate_CurntTemp ENDP




; ==========================================================================================================================
; Converts the current string Temperature reading to Integer
; receives: Address of the current Temperature reading
; returns:
; preconditions: passed address references of array
; postconditions: values saved in array
; registers changed: none
; ==========================================================================================================================
ConvertStringToInteger PROC
    LOCAL offset_Str_CurntTempLoc:DWORD
    LOCAL len_Str_CurntTemp:DWORD
    LOCAL numInt:DWORD
    LOCAL loopCount:DWORD

    PUSH    EAX
    PUSH    EBX
    PUSH    ECX
    PUSH    EDX
    PUSH    ESI
    PUSH    EDI

    ;------------------------------------------------------------
    ; PARAMETERS:
    ; [EBP+12] : offset_Int_CurntTemp  - pointer where the converted integer will be stored.
    ; [EBP+8]  : offset_Str_CurntTemp - pointer to the ASCII string, null Terminated
    ;
    ; Store the string pointer in a local variable.
    MOV     EAX, [EBP + 8]
    MOV     offset_Str_CurntTempLoc, EAX

    ;------------------------------------------------------------
    ; GET STRING LENGTH USING IRVINE STR_LENGTH:
    MOV     EAX, offset_Str_CurntTempLoc    ; Load pointer into EAX.
    CALL    Str_Length                      ; Returns length in EAX.
    MOV     len_Str_CurntTemp, EAX

    ;------------------------------------------------------------
    ; DEBUG PRINT: Print pointer value.
    ;MOV     EDX, OFFSET STR_MSGPOINTER    ; "POINTER VALUE: "
    ;CALL    CrLf
    ;CALL    WriteString
    ;MOV     EAX, offset_Str_CurntTempLoc
    ;CALL    WriteDec
    ;CALL    CrLf

    ; DEBUG PRINT: Print passed string.
    ;MOV     EDX, OFFSET STR_MSGPASSEDSTRING   ; "PASSED STRING: "
    ;CALL    CrLf
    ;CALL    WriteString
    ;MOV     EDX, offset_Str_CurntTempLoc
    ;CALL    WriteString
    ;CALL    CrLf

    ; DEBUG PRINT: Print string length.
    ;MOV     EDX, OFFSET STR_MSGLENGTH   ; "LENGTH: "
    ;CALL    WriteString
    ;MOV     EAX, len_Str_CurntTemp
    ;CALL    WriteDec
    ;CALL    CrLf

    ;------------------------------------------------------------
    ; INITIALIZE THE RESULT INTEGER.
    MOV     numInt, 0

    ;------------------------------------------------------------
    ; SET UP POINTERS FOR CONVERSION:
    ; Use ESI to point to the start of the string.
    MOV     ESI, offset_Str_CurntTempLoc
    ; Use ECX as a counter, set to the length of the string.
    MOV     ECX, len_Str_CurntTemp


    ;------------------------------------------------------------
    ; CONVERSION LOOP:
    ; For each character in the string, if it is between '0' and '9',
    ; update numInt = 10 * numInt + (char - '0').
    _convert_loop:
        LODSB                       ; Load byte from [ESI] into AL, ESI++, ECX--
        CMP     AL, 0
        JE      _end_convert_loop    ; If null terminator, end loop.
        CMP     AL, '0'
        JB      _end_convert_loop    ; If char < '0', break.
        CMP     AL, '9'
        JA      _end_convert_loop    ; If char > '9', break.
        ; Convert character to digit.
        MOVZX   EAX, AL             ; Zero-extend AL into EAX.
        SUB     EAX, '0'            ; EAX = digit value.
        ; Multiply current numInt by 10.
        MOV     EBX, numInt
        IMUL    EBX, 10
        ADD     EBX, EAX            ; Add digit value.
        MOV     numInt, EBX         ; Update numInt.
        LOOP    _convert_loop

    ; end loop and store the converted integer value
    _end_convert_loop:
        ;------------------------------------------------------------
        ; DEBUG PRINT: Print the converted integer.
        ; MOV     EDX, OFFSET STR_MSGCONVERTED   ; "CONVERTED INTEGER: "
        ; CALL    CrLf
        ; CALL    WriteString
        ; MOV     EAX, numInt
        ; CALL    WriteDec
        ; CALL    CrLf

        ;------------------------------------------------------------
        ; STORE THE RESULT:
        ; Save the converted integer at the memory location pointed to by offset_Int_CurntTemp.
        MOV     EBX, [EBP+12]
        MOV     EAX, numInt
        MOV     DWORD PTR [EBX], EAX

    ;------------------------------------------------------------
    ; RESTORE REGISTERS AND RETURN.
    POP     EDI
    POP     ESI
    POP     EDX
    POP     ECX
    POP     EBX
    POP     EAX
    RET     8
ConvertStringToInteger ENDP




; ==========================================================================================================================
; Detects sign of the current temp reading saved as string. Removes the sign
; receives: Address of the Temperature reading
; returns:
; preconditions: passed address references of array
; postconditions: values saved in array
; registers changed: none
; ==========================================================================================================================

GetSign PROC
    PUSH    EBP
    MOV     EBP, ESP
    SUB     ESP, 4                ; Allocate 4 bytes for local variable (offset_Str_CurntTempLoc)

    PUSH    EDI
    PUSH    ESI
    PUSH    EDX
    PUSH    ECX
    PUSH    EBX
    PUSH    EAX

    ;------------------------------------------------------------
    ; Parameter Handling:
    ; [EBP+8]  : offset_Str_CurntTemp - pointer to the string (e.g. "-15",0)
    ; [EBP+12] : offset_Int_Sign      - pointer to the INT byte.
    ;
    ; Load the string pointer into EBX and store it in our local variable.
    MOV     EBX, [EBP+8]          ; EBX = pointer to passed string.
    MOV     DWORD PTR [EBP-4], EBX  ; Store this pointer in local variable.

    ;------------------------------------------------------------
    ; Debug Print: Print pointer value.
    ; MOV     EDX, OFFSET str_MsgPointer  ; "POINTER VALUE: "
    ; CALL    CrLf
    ; CALL    WriteString
    ; MOV     EAX, EBX            ; Use EBX directly.
    ; CALL    WriteDec
    ; CALL    CrLf

    ;------------------------------------------------------------
    ; Debug Print: Print passed string.
    ;MOV     EDX, OFFSET str_MsgPassedString  ; "PASSED STRING: "
    ;CALL    CrLf
    ;CALL    WriteString
    ;MOV     EDX, EBX
    ;CALL    WriteString

    ;------------------------------------------------------------
    ; Read the first character from the string.
    MOV     EAX, [EBP-4]         ; Get stored pointer.
    MOV     AL, BYTE PTR [EAX]   ; AL = first character.

    ;------------------------------------------------------------
    ; Debug Print: Print first character (as char and ASCII code).
    ;MOV     EDX, OFFSET str_MsgFirstChar   ; "FIRST CHAR: "
    ;CALL    CrLf
    ;CALL    WriteString
    ;CALL    WriteChar          ; Print character in AL.
    ;CALL    CrLf
    ;MOVZX   EAX, AL            ; Zero-extend AL.
    ;MOV     EDX, OFFSET str_MsgASCII   ; " ASCII: "
    ;CALL    WriteString
    ;CALL    WriteDec           ; Print ASCII code.
    ;CALL    CrLf

    ;------------------------------------------------------------
    ; Determine sign based solely on the first character.
    CMP     AL, '-'            ; If first character is '-'
    JE      setNegative
    CMP     AL, '+'            ; If first character is '+'
    JE      setPositive
    CMP     AL, '0'
    JB      defaultPositive    ; If not a digit, default positive.
    CMP     AL, '9'
    JA      defaultPositive    ; If not a digit, default positive.

defaultPositive:
    ; First character is a digit; treat as positive.
    MOV     EBX, [EBP+12]       ; Load pointer to int_Sign byte.
    MOV     DWORD PTR [EBX], 0   ; Set int_Sign to 0 (positive).
    JMP     printSign

setNegative:
    ; First character is '-' sign.
    MOV     EBX, [EBP+12]       ; Load pointer to int_Sign byte.
    MOV     DWORD PTR [EBX], 1   ; Set int_Sign to 1 (negative).
    JMP     shiftString

setPositive:
    ; First character is '+' sign.
    MOV     EBX, [EBP+12]       ; Load pointer to int_Sign byte.
    MOV     DWORD PTR [EBX], 0   ; Set int_Sign to 0 (positive).
    JMP     shiftString

    ;------------------------------------------------------------
shiftString:
    ; Debug Print: Indicate that the string is being shifted.
    ;MOV     EDX, OFFSET str_MsgShifting  ; "SHIFTING STRING..."
    ;CALL    CrLf
    ;CALL    WriteString

    ; Shift the string left by one byte (remove the sign).
    ; Use the local variable stored at [EBP-4] as the pointer.
    MOV     EAX, [EBP-4]        ; EAX = original string pointer.
    ADD     EAX, 1              ; Point to the second character.
    MOV     ESI, EAX            ; Source pointer = string + 1.
    MOV     EAX, [EBP-4]        ; Get original pointer again.
    MOV     EDI, EAX            ; Destination pointer = original string pointer.
    CLD                         ; Clear direction flag.
shift_loop:
    LODSB                       ; Load byte from source (ESI) into AL; increments ESI.
    STOSB                       ; Store byte in AL into destination (EDI); increments EDI.
    CMP     AL, 0               ; Check for null terminator.
    JNE     shift_loop
    ; Debug Print: Print shifted string.
    MOV     EDX, OFFSET str_MsgShiftedString  ; "SHIFTED STRING: "
    ;CALL    CrLf
    ;CALL    WriteString
    MOV     EDX, [EBP-4]        ; Local variable still holds original pointer.
    ;CALL    WriteString
    JMP     printSign

    ;------------------------------------------------------------
printSign:
    ; Debug Print: Print the obtained int_Sign value.
    ;MOV     EDX, OFFSET str_MsgIntSign  ; "INT SIGN: "
    ;CALL    CrLf
    ;CALL    WriteString
    ;MOV     EAX, [EBP+12]       ; Load pointer to int_Sign byte.
    ;MOVZX   EAX, BYTE PTR [EAX] ; Get the sign value.
    ;CALL    WriteDec            ; Print the sign value.
    ;CALL    CrLf

    ;------------------------------------------------------------
done:
    POP     EAX
    POP     EBX
    POP     ECX
    POP     EDX
    POP     ESI
    POP     EDI
    MOV     ESP, EBP
    POP     EBP
    RET     8
GetSign ENDP



; ==========================================================================================================================
; Extracts a temperature reading, in string delimited format, from memory. 
; receives: Address of the Temperature array
; returns:
; preconditions: passed address references of array
; postconditions: values saved in array
; registers changed: none
; ==========================================================================================================================
Extract_StrCrntTemp PROC
    ;-------------------------------------------------------------------
    ; Local Variables:
    ;   prev_DlmterPos      - stores the parameter int_Prev_DlmterPos (previous delimiter index)
    ;   crnt_DlmterPos      - stores the parameter int_CrntDlmterPos (current delimiter index)
    ;   file_TempReadingsLoc- stores the parameter offset_File_TempReadings (file buffer base address)
    ;   startPos            - computed starting index for extraction (prev_DlmterPos + 1)
    ;   stringLength        - length of the substring to extract (crnt_DlmterPos - startPos)
    LOCAL prev_DlmterPos:DWORD
    LOCAL crnt_DlmterPos:DWORD
    LOCAL file_TempReadingsLoc:DWORD
    LOCAL startPos:DWORD
    LOCAL stringLength:DWORD

    PUSH    EAX
    PUSH    EBX
    PUSH    ECX
    PUSH    EDX
    PUSH    ESI
    PUSH    EDI

    ;-------------------------------------------------------------------
    ; Parameter Handling:
    ; Parameters (from caller):
    ;   [EBP+8]  : int_CrntDlmterPos   - current delimiter index.
    ;   [EBP+12] : int_Prev_DlmterPos    - previous delimiter index.
    ;   [EBP+16] : offset_str_CurntTemp  - destination address for the extracted string.
    ;   [EBP+20] : offset_File_TempReadings - file buffer address.
    ;
    ; Load parameters into local variables with the updated order.
    MOV     EAX, [EBP+12]
    MOV     prev_DlmterPos, EAX          ; Save previous delimiter position.
    MOV     EAX, [EBP+8]
    MOV     crnt_DlmterPos, EAX          ; Save current delimiter position.
    MOV     EAX, [EBP+20]
    MOV     file_TempReadingsLoc, EAX   ; Save file buffer base address.


    ; Debugging Messages
    MOV     EDX, OFFSET str_MsgLoadedFile
    CALL    CrLf
    CALL    CrLf
    CALL    WriteString
    MOV     EDX, file_TempReadingsLoc
    CALL    CrLf
    CALL    WriteString


    MOV     EDX, OFFSET str_MsgPrevDlmtrPos
    CALL    CrLf
    CALL    WriteString
    MOV     EAX, prev_DlmterPos
    CALL    WriteInt

    MOV     EDX, OFFSET str_MsgCrntDlmtrPos
    CALL    CrLf
    CALL    WriteString
    MOV     EAX, crnt_DlmterPos
    CALL    WriteInt


    ;-------------------------------------------------------------------
    ; Compute the Starting Position for Extraction:
    ; startPos = prev_DlmterPos + 1  (skip the previous delimiter)
    MOV     EAX, prev_DlmterPos
    ADD     EAX, 1                     ; Advance one position past previous delimiter.
    MOV     startPos, EAX              ; Save computed starting index.

    ;-------------------------------------------------------------------
    ; Compute the Length of the Substring to Extract:
    ; stringLength = crnt_DlmterPos - startPos
    MOV     EAX, crnt_DlmterPos
    SUB     EAX, startPos              ; Compute number of characters to copy.
    MOV     stringLength, EAX          ; Save the computed string length.

    ;-------------------------------------------------------------------
    ; Debug Prints (Optional):
    ; Display file buffer base address, start position, and string length.
    ;MOV     EAX, file_TempReadingsLoc
    ;CALL    CrLf
    ;CALL    WriteDec
    ;CALL    CrLf

    MOV     EAX, startPos
    ;CALL    WriteDec
    ;CALL    CrLf

    MOV     EAX, stringLength
    ;CALL    WriteDec
    ;CALL    CrLf

    ;-------------------------------------------------------------------
    ; Check for non-positive string length (i.e. negative or zero length).
    ; CMP     stringLength, 0
    ; JLE     SkipCopy

    ;-------------------------------------------------------------------
    ; Set Up Source and Destination Pointers for Copy:
    ; Source pointer = file_TempReadingsLoc + startPos
    MOV     EAX, file_TempReadingsLoc
    ADD     EAX, startPos              ; Compute effective source address.
    MOV     ESI, EAX                   ; ESI now points to the substring in the file buffer.
    ;
    ; Destination pointer = offset_str_CurntTemp (passed as parameter at [EBP+16])
    MOV     EAX, [EBP+16]
    MOV     EDI, EAX                   ; EDI points to the destination buffer.

    ;-------------------------------------------------------------------
    ; Copy the Substring Using REP MOVSB:
    ; Clear the Direction Flag to ensure auto-increment.
    CLD
    ; Set ECX = stringLength (number of bytes to copy).
    MOV     ECX, stringLength
    REP     MOVSB                    ; Copy ECX bytes from DS:ESI to ES:EDI.

    ;SkipCopy:
        ;-------------------------------------------------------------------
        ; Append the NULL Terminator:
        MOV     AL, 0                     ; Prepare NULL in AL.
        STOSB                             ; Store AL at destination and increment EDI.

        ; Debugging messages
        ;MOV     EDX, [EBP+16]
        ;CALL    CrLF
        ;CALL    CrLf
        ;CALL    WriteString

    ;-------------------------------------------------------------------
    ; Restore Registers and Return:
    POP     EDI
    POP     ESI
    POP     EDX
    POP     ECX
    POP     EBX
    POP     EAX
    RET     20                        ; Clean up 20 bytes (4 parameters) from the stack.
Extract_StrCrntTemp ENDP




; ==========================================================================================================================
; Searches for the next delimiter in the file buffer
; receives: Address of the buffered file, offset of the matrix Length and Width placeholder
; returns:
; preconditions: passed address offsets
; postconditions: values saved in memory
; registers changed: none
; ==========================================================================================================================
Get_NextDlmtrPos PROC
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

    ; Debugging messages
    ;MOV     EDX, OFFSET str_MsgPrevDlmtrPos
    ;CALL    CrLf
    ;CALL    WriteString
    ;MOV     EAX, prevDlmterPos
    ;CALL    WriteInt

    MOV     EAX, [EBP+16]
    MOV     offset_File_TempReadingsLoc, EAX  ; Save file buffer base address.


    ; Debugging messages
    ;MOV     EDX, OFFSET str_MsgLoadedFile
    ;CALL    CrLF
    ;CALL    WriteString
    ;MOV     EDX, offset_File_TempReadingsLoc
    ;CALL    CrLF
    ;CALL    WriteString

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
        ;MOV     EDX, OFFSET str_MsgCrntDlmtrPos
        ;CALL    CrLf
        ;CALL    WriteString
        ;CALL    WriteDec


        POP     EDI
        POP     ESI
        POP     EDX
        POP     ECX
        POP     EBX
        POP     EAX
        RET     12
Get_NextDlmtrPos ENDP




; ==========================================================================================================================
; Gathers the Length and Width og the matrix
; receives: Address of the buffered file, offset of the matrix Length and Width placeholder
; returns:
; preconditions: passed address offsets
; postconditions: values saved in memory
; registers changed: none
; ==========================================================================================================================
Get_MatrixSize PROC

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

Get_MatrixSize ENDP

; ==========================================================================================================================
; Init Temp Matrix
; receives: Address of the Temperature array
; returns:
; preconditions: passed address references of array
; postconditions: values saved in array
; registers changed: none
; ==========================================================================================================================

Init_TempMatrix PROC
    PUSH    EBP
    MOV     EBP, ESP

    PUSH    EAX
    PUSH    EBX
    PUSH    ECX
    PUSH    EDX
    PUSH    ESI
    PUSH    EDI

    ;-------------------------------------------------------------------
    ; Parameters:
    ;   [EBP+12] : OFFSET arr_TempMatrix
    ;   [EBP+8] : length_Arr_TempMatrix


    ; Debugging prints
    ;MOV     EAX, [EBP+12]
    ;CALL    CrLf
    ;CALL     WriteDec
    ;MOV     EAX, [EBP + 8]
    ;CALL    CrLf
    ;CALL    WriteDec
    ;CALL    CrLf


    ;==================================================================
    ; Initialize Temp Matrix with value 1
    MOV ECX, [EBP + 8]                                  ; Loop counter - LENGHTOF Array
    MOV EDI, [EBP+12]                                   ; Load address of the array into EDI
    MOV EAX, 1                                          ; Value to initialize (1)

    ; Loop to initialize Temp Matrix
    _InitLoop:
        MOV DWORD PTR [EDI], EAX                         ; Store 1 at current position
        ADD EDI, 4                                       ; Move to the next DWORD (4 bytes)
        LOOP _InitLoop                                   ; Decrement ECX, loop if not zero


        MOV ECX, [EBP+8]                                ; Set loop counter
        MOV ESI, [EBP+12]                               ; Load base address of array

    ; Print array
    ;_PrintLoop:
    ;    MOV EAX, DWORD PTR [ESI]                        ; Load current array value
    ;    CALL WriteDec                                   ; Print number
    ;    MOV AL, ' '
    ;    CALL WriteChar
    ;    ADD ESI, 4                                      ; Move to next DWORD
    ;    LOOP _PrintLoop                                 ; Repeat until ECX = 0

 
    ;-------------------------------------------------------------------
    ; Restore Registers and Return:
    POP     EDI
    POP     ESI
    POP     EDX
    POP     ECX
    POP     EBX
    POP     EAX
    POP     EBP
    RET     8
Init_TempMatrix ENDP



END main