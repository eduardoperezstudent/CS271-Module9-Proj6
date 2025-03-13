TITLE Temperature Basic Stats      (Proj5_pereze4.asm)

; Author: Eduardo Perez
; Last Modified: March 3, 2025
; OSU email address: pereze4@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 5               Due Date: March 3, 2025
; Description: Generates temperature Readings, then provides Min, Max and Ave calculations.
;
; Implementation note 1:    The procedure 'findDailyHighs' differentiates between single and multiple rows by checking the array LENGTH
;
; Implementation note 2:    Temperature will increase or decrese depending, if limits are reached, and at specific iterations of 'hours', denoting sunset/sunrise.
;                           Known issue: sunset/sunrise time detection doesn't work if TEMPS_PER_DAY is 2 or less
;                           
; Implementation note 3:    The maximum fluctuation of a temperature per reading is given by formula: 2(MAX_TEMP - MIN_TEMP) / TEMPS_PER_DAY. Capped at 9
;
; Implementation note 4:    Initial temp of array is set to 3/4 of MAX_TEMP. This is to compensate for initial temp decreases

INCLUDE Irvine32.inc

; Global Variables
DAYS_MEASURED = 14
TEMPS_PER_DAY = 11
MIN_TEMP = 20
MAX_TEMP = 80

.data
; Information only. Description of Local Variables
; Local Variables in Procedures. Some variable names are re-used.
;int_MaxDeltaTemp            DWORD   0       ; Max change in temperature
;int_DirectionTemp           DWORD   1       ; determines if temp will inc or dec
;int_CurrentDeltaTemp        DWORD   0       ; the random change in temp for the current hour. Range: 0 --> int_MaxDeltaTemp
;int_CurrentTemp             DWORD   0       ; temporarily stores the current hour reading
;int_CurrentDay              DWORD   0       ; 'N index in loops'
;int_CurrentHour             DWORD   0       ; 'M index in loops'
;int_DailyHigh               DWORD   0       ; stores the daily max reading
;int_DailyLow                DWORD   0       ; stores the daily min reading




str_Greeting            BYTE    "I am Eduardo Perez. This is the Temperature Basic Stats program.", 10, 17, \
                                "It displays all the Temperature readings per day", 10, 17, \
                                "It then provides the all the Maximum Minimum readings per day.", 10, 17, \
                                "Lastly, it calculates and displays the Average of the Daily Max and Min values", 17, 0
str_EC                  BYTE    "EC1: Columns of the Temperature array corresponds to one day.", 10, 17, \
                                "EC2: Generated Temperatures follows a diurnal cycle.", 10, 17, 0
str_Farewell            BYTE    10, "Thank you for using. Goodbye", 10, 17, 0
str_msgTempArray        BYTE    10, "The temperature readings (one column is one day):", 10, 17, 0
str_msgRowMaxPerDay     BYTE    10, 10, "The Maximum readings per day:", 10, 17, 0
str_msgRowMinPerDay     BYTE    10, "The Minimum readings per day:", 10, 17, 0
str_msgAveHigh          BYTE    10, 17,"The Average of the daily Highs is: ", 0
str_msgAveLow           BYTE    10, 17,"The Average of the daily Lows is: ", 0


arr_TempReadings            DWORD       644 DUP(1), 0FFFFFFFFh      ; stores the Temp matrix, append with value to check array length within called procedure
arr_DailyLows               DWORD       28 DUP(1), 0FFFFFFFFh       ; Row of minimum readings per day, append with a SENTINEL value to check array length within called procedure
arr_DailyHighs              DWORD       28 DUP(1), 0FFFFFFFFh       ; Row of maximum readings per day, append with a SENTINEL value to check array length within called procedure




.code
main PROC

    CALL    Randomize

	PUSH    OFFSET str_Greeting
	CALL    printGreeting

	PUSH    OFFSET str_EC
	CALL    printGreeting

    PUSH    OFFSET arr_TempReadings
    CALL    generateTemperatures

    PUSH    OFFSET str_msgTempArray
    PUSH    OFFSET arr_TempReadings
    CALL    displayTempArray

    PUSH    OFFSET arr_TempReadings
    CALL    findDailyHighs
    
    PUSH    OFFSET str_msgRowMaxPerDay
    PUSH    OFFSET arr_DailyHighs
    CALL    displayTempArray

    PUSH    OFFSET arr_TempReadings
    CALL    findDailyLows
    
    PUSH    OFFSET str_msgRowMinPerDay
    PUSH    OFFSET arr_DailyLows
    CALL    displayTempArray


    PUSH    OFFSET arr_DailyLows
    PUSH    OFFSET arr_DailyHighs
    CALL    calcAverageLowHighTemps
    ; The calculated average values are returned/saved in the last element of the respective input arrays.
    ; This overwrites the sentinel value, not a Temperature reading




    MOV     EBX, OFFSET arr_DailyHighs
    MOV     EAX, [EBX + 112]
    PUSH    OFFSET str_msgAveHigh
    PUSH    EAX  
    CALL    displayTempwithString


    MOV     EBX, OFFSET arr_DailyLows
    MOV     EDX, [EBX + 112]
    PUSH    OFFSET str_msgAveLow
    PUSH    EDX  
    CALL    displayTempwithString


 	PUSH    offset str_Farewell
	CALL    printGreeting

	INVOKE ExitProcess,0
main ENDP


; ==========================================================================================================================
; Prints both the average high and the average low temperature
; receives: addressess of string and input values to display
; returns:
; preconditions: passed address references of string and int
; postconditions:
; registers changed: none
; ==========================================================================================================================
displayTempwithString   PROC
    PUSH    EBP
    MOV     EBP, ESP
    PUSH	EAX
    PUSH	EDX


    ; Stack Layout:
    ; [EBP + 12] = OFFSET some String
    ; [EBP + 8] = OFFSET some INT value
    ; [EBP + 4] = return address
    ; [EBP] = old ebp


    ; Print String Part
    MOV     EDX, [EBP + 12]
    CALL    WriteString 

    ; Print Integer part
    MOV     EAX, [EBP + 8]
    CALL    WriteDec
    CALL    CrLf

    POP	    EDX
    POP	    EAX
    POP     EBP
    RET     8

displayTempwithString  ENDP


; ==========================================================================================================================
; Calculates separately the average Highs and Lows values
; receives: addressess of array containing the daily Highs and Lows
; returns:  average values are saved in the last element of the respective arrays.
;           This overwrites the sentinel value, not a Temperature reading
; preconditions: passed address references of arrays
; postconditions:
; registers changed: none
; ==========================================================================================================================
calcAverageLowHighTemps  PROC
    LOCAL   int_averageHighLoc:DWORD, int_averageLowLoc:DWORD

    PUSH	EAX
    PUSH	EBX
    PUSH	ECX
    PUSH	EDX
    PUSH	ESI
    PUSH	EDI


    ; Stack Layout:
    ; [EBP + 20] = OFFSET int_averageHigh
    ; [EBP + 16] = OFFSET int_averageLow
    ; [EBP + 12] = OFFSET arr_DailyLows
    ; [EBP + 8] = OFFSET arr_DailyHighs
    ; [EBP + 4] = return address
    ; [EBP] = old ebp


    ; Load values to Local variables
    MOV     EAX, [EBP + 20]
    MOV    int_averageHighLoc, EAX
    MOV     EAX, [EBP + 16]
    MOV    int_averageLowLoc, EAX 


    ; Start Highs averager
    ; Initialize counter and accumulator
    MOV     ECX, 0
    MOV     EAX, 0
    MOV     EDI, [EBP + 8]                ; memory address of arr_DailyLows

    ; Loops through all daily High values
    _loop_AveragerHighs:
        ; Get address of current element in arr_DailyHighs

        MOV     EBX, [EDI]                          ; Save in register the current daily high
        ADD     EAX, EBX                            ; Add current HighTemp to accumulator
        ADD     EDI, 4                              ; Add offset of current row element to address of array

        ; End Iteration
        INC     ECX
        CMP     ECX, DAYS_MEASURED
        JB     _loop_AveragerHighs

    ; Divide summation by N to get ave
    MOV     EDX, 0
    MOV     EBX, DAYS_MEASURED
    DIV     EBX
    MOV     int_averageHighLoc, EAX
    ; Return Value
    MOV     EDI, [EBP + 8]
    MOV     [EDI+112], EAX                          ; Overwrite last element of arr_DailyHighs, which was not a Temp reading but a sentinel value



    ; Start Lows averager
    ; Initialize counter and accumulator
    MOV     ECX, 0
    MOV     EAX, 0
    MOV     EDI, [EBP + 12]                ; memory address of arr_DailyLows


    ; Loops through all daily Low values
    _loop_AveragerLows:
        ; Get address of current element in arr_DailyLows

        MOV     EBX, [EDI]                          ; Save in register the current daily low
        ADD     EAX, EBX                            ; Add current HighTemp to accumulator
        ADD     EDI, 4                              ; Add offset of current row element to address of array

        ; End Iteration
        INC     ECX
        CMP     ECX, DAYS_MEASURED
        JB     _loop_AveragerLows

    ; Divide summation by N to get ave
    MOV     EDX, 0
    MOV     EBX, DAYS_MEASURED
    DIV     EBX
    MOV     int_averageLowLoc, EAX
    MOV     EDI, [EBP + 12]
    MOV     [EDI + 112], EAX                    ; Overwrite last element of arr_DailyHighs, which was not a Temp reading but a sentinel value


    POP	    EDI
    POP 	ESI
    POP	    EDX
    POP	    ECX
    POP 	EBX
    POP	    EAX
    RET     8

calcAverageLowHighTemps  ENDP


; ==========================================================================================================================
; Compares each reading in a day then saves the min value
; receives: addressess of array of temperature readings
; returns:  address of array of daily min values
; preconditions: passed address references of temp array
; postconditions: saved daily min values in array
; registers changed: none
; ==========================================================================================================================
findDailyLows   PROC
    LOCAL   int_CurrentDayLoc:DWORD, int_CurrentHourLoc:DWORD, int_DailyLowLoc:DWORD

    PUSH	EAX
    PUSH	EBX
    PUSH	ECX
    PUSH	EDX
    PUSH	ESI
    PUSH	EDI

    ; Local Variables:
    ; int_CurrentDay
    ; int_CurrentHour
    ; int_DailyLow
    ; str_msgRowMinPerDay
    ; arr_DailyLows

    ; Stack Layout:
    ; [EBP + 8] = OFFSET arr_TempReadings
    ; [EBP + 4] = return address
    ; [EBP] = old ebp


    ;initialize outer loop counter
    MOV     ECX, 0
    MOV     int_CurrentDayLoc, ECX

    ; Outerloop - number of days
    _loop_Days4:
        
        MOV     ECX, 0      ; Initialize inner loop
        MOV     int_CurrentHourLoc, ECX

        ; Innerloop - number of readings
        _loop_Hours4:
            MOV     int_CurrentHourLoc, ECX            ;   counter ECX has since incremented, update int_CurrentHour 

            ; Obtain current temperature for iteration
            ; Get OFFSET of the Temp
            MOV     EAX, TEMPS_PER_DAY      
            MUL     int_CurrentDayLoc               ; current row * row width
            ADD     EAX, int_CurrentHourLoc         ; get current column Index in current Row
            MOV     EDX, 4                          ; Multiply by DTYPE DWORD of 4 Bytes, now obtained the BYTE OFFSET in array
            MUL     EDX
            MOV     EDI, [EBP + 8]                  ; Move in EDI ,the address of the first element in the array
            ADD     EDI, EAX                        ; Add offset of current element to address of first element of array           
            MOV     EAX, [EDI]                      ; Move to EAX the Temp value saved in array memory location addressed by EDI

            ; Check if Initial daily reading
            CMP     ECX, 0
            JE     _get_InitialDailyReading

            ; Res: Go here if not initial
            ; set Temp as current Temp
            ; compare current Temp to Daily low Temp
                        
            ; Compare To Daily Min
                CMP     EAX, int_DailyLowLoc
                JB     _set_DailyLow                   
                JMP     _end_Loop_Hours4 
                
            ; set new Min value if current temp is lower than current Min
            _set_DailyLow:
                MOV     int_DailyLowLoc, EAX                ;
                JMP     _end_Loop_Hours4

            ; Res: Jump here if initial
            _get_InitialDailyReading:           
                MOV     int_DailyLowLoc, EAX                ;
                JMP     _end_Loop_Hours4

                  
            ; End Iteration hour
            _end_Loop_Hours4:
                INC     ECX
                CMP     ECX, TEMPS_PER_DAY
                JB      _loop_Hours4
                ; Inner Loop ends
        
        ; Return to outer loop
        
        ; Save Daily Low Value in array
        ; Get offset in array to save the Low Value
        MOV     EDI, OFFSET arr_DailyLows           ; memory address of arr_DailyLows
        MOV     EDX,  int_CurrentDayLoc             ; get current Day iteration
        SHL     EDX, 2                              ; Multiply by DTYPE DWORD of 4 Bytes, now obtained the BYTE OFFSET in array
        ADD     EDI, EDX                            ; Add offset of current row element to address of array
        ; Save in Array
        MOV     EAX, int_DailyLowLoc
        MOV     [EDI],  EAX                         ; Save the new Temp in memory location addressed by EDI


        ; Prepare for next outer loop, increment counter
        MOV     ECX, int_CurrentDayLoc
        INC     ECX
        MOV     int_CurrentDayLoc, ECX
        CMP     ECX,  DAYS_MEASURED
        JB      _loop_Days4

    POP	    EDI
    POP 	ESI
    POP	    EDX
    POP	    ECX
    POP 	EBX
    POP	    EAX
    RET     4

findDailyLows   ENDP



; ==========================================================================================================================
; Compares each reading in a day then saves the max value
; receives: addressess of array of temperature readings
; returns:  address of array of daily max values
; preconditions: passed address references of temp array
; postconditions: saved daily max values in array
; registers changed: none
; ==========================================================================================================================
findDailyHighs   PROC
    
    LOCAL   int_CurrentDayLoc:DWORD, int_CurrentHourLoc:DWORD, int_DailyHighLoc:DWORD
    PUSH	EAX
    PUSH	EBX
    PUSH	ECX
    PUSH	EDX
    PUSH	ESI
    PUSH	EDI

    ; Local Variables:
    ; int_CurrentDay
    ; int_CurrentHour
    ; int_DailyHigh
    ; str_msgRowMaxPerDay
    ; arr_DailyHighs

    ; Stack Layout:
    ; [EBP + 8] = OFFSET arr_TempReadings
    ; [EBP + 4] = return address
    ; [EBP] = old ebp


    ;initialize outer loop counter
    MOV     ECX, 0
    MOV     int_CurrentDayLoc, ECX

    ; Outerloop - number of days
    _loop_Days3:       
        MOV     ECX, 0      ; Initialize inner loop
        MOV     int_CurrentHourLoc, ECX

        ; Innerloop - number of readings
        _loop_Hours3:
            MOV     int_CurrentHourLoc, ECX        ;   counter ECX has since incremented, update int_CurrentHour 

            ; Obtain current temperature for iteration
            ; Get OFFSET of the Temp
            MOV     EAX, TEMPS_PER_DAY      
            MUL     int_CurrentDayLoc               ; current row * row width
            ADD     EAX, int_CurrentHourLoc         ; get current column Index in current Row
            MOV     EDX, 4                          ; Multiply by DTYPE DWORD of 4 Bytes, now obtained the BYTE OFFSET in array
            MUL     EDX
            MOV     EDI, [EBP + 8]                  ; Move in EDI ,the address of the first element in the array
            ADD     EDI, EAX                        ; Add offset of current element to address of first element of array
            
            MOV     EAX, [EDI]                      ; Move to EAX the Temp value saved in array memory location addressed by EDI


            ; Check if Initial daily reading
            CMP     ECX, 0
            JE     _get_InitialDailyReading

            ; Compare to Daily MAX
            CMP     EAX, int_DailyHighLoc
            JA      _set_DailyHigh                      

            JMP     _end_Loop_Hours3               

            ; Update the Max value with the current Temp iteration
            _set_DailyHigh:
                MOV     int_DailyHighLoc, EAX
                JMP     _end_Loop_Hours3


            ; Res: Jump here if initial
            _get_InitialDailyReading:           
                MOV     int_DailyHighLoc, EAX
                JMP     _end_Loop_Hours3
                     
            ; End Iteration hour
            _end_Loop_Hours3:
                INC     ECX
                CMP     ECX, TEMPS_PER_DAY
                JB      _loop_Hours3
                ; Inner Loop ends

        ; Return to outer loop
        
        ; Save Daily High Value in array
        ; Get offset in array to save the High Value
        MOV     EDI, OFFSET arr_DailyHighs          ; memory address of arr_DailyHighs
        MOV     EDX,  int_CurrentDayLoc             ; get current Day iteration
        SHL     EDX, 2                              ; Multiply by DTYPE DWORD of 4 Bytes, now obtained the BYTE OFFSET in array
        ADD     EDI, EDX                            ; Add offset of current row element to address of array
        ; Save in Array
        MOV     EAX, int_DailyHighLoc
        MOV     [EDI],  EAX                         ; Save the new Temp in memory location addressed by EDI


        ; Prepare for next loop, increment counter
        MOV     ECX, int_CurrentDayLoc
        INC     ECX
        MOV     int_CurrentDayLoc, ECX
        CMP     ECX,  DAYS_MEASURED
        JB      _loop_Days3

    POP	    EDI
    POP 	ESI
    POP	    EDX
    POP	    ECX
    POP 	EBX
    POP	    EAX
    RET     4

findDailyHighs   ENDP


; ==========================================================================================================================
; Prints the Temp array
; receives: addressess of string to print and array of temperature readings
; returns:
; preconditions: passed address references
; postconditions: prinout of strings and temp matrix
; registers changed: none
; ==========================================================================================================================
displayTempArray  PROC

        LOCAL   int_CurrentDayLoc:DWORD, int_CurrentHourLoc:DWORD
        PUSH	EAX
        PUSH	EBX
        PUSH	ECX
        PUSH	EDX
        PUSH	ESI
        PUSH	EDI

        ; Local Variables:
        ; int_CurrentDay
        ; int_CurrentHour

        ; Stack Layout:
        ; [EBP + 12] = OFFSET Some String
        ; [EBP + 8] = OFFSET some Array
        ; [EBP + 4] = return address
        ; [EBP] = old ebp

    ; Display Message
        MOV     EDX, [EBP + 12]
        CALL    WriteString

    ; Check if Row or array
    MOV     ESI, [EBP + 8]          ; address of arr_DailyHighs
    MOV     ECX, 0                  ; ECX will count elements

    _get_LengthArray:
        MOV     EAX, [ESI + ECX*4]      ; read DWORD at offset (each element is 4 bytes)
        CMP     EAX, 0FFFFFFFFh         ; check for last array value
        JE      _found_End              ; stop when value is found
        INC     ECX
        JMP     _get_LengthArray

    _found_End:
        ; ECX now holds the number of valid elements
        CMP     ECX, 28                 ; arr_DailyLows and arr_DailyHighs has LENGTH of 28
        JBE      _print_Row
    ; End Check if Row or array

    
    ; Save in EDI OFFSET arr_TempReadings
    MOV     EDI, [EBP + 8]


    ;initialize outer loop counter
    MOV     ECX, 0
    MOV     int_CurrentHourLoc, ECX
 
    ; Outerloop - number of Hours
    _loop_Hours2:
        CALL    CrLf
        MOV     ECX, 0                              ; Initialize inner loop
        MOV     int_CurrentDayLoc, ECX
        
        ; Innerloop - number of Days
        _loop_Days2:
            MOV     int_CurrentDayLoc, ECX             ; counter ECX has since incremented, update int_CurrentHour

        ; Get Temp value from array day reading: Day = N, Hour = M
        ; Get OFFSET of the Temp
            MOV     EAX, TEMPS_PER_DAY      
            MUL     int_CurrentDayLoc               ; current row * row width
            ADD     EAX, int_CurrentHourLoc         ; get current column Index in current Row
            MOV     EDX, 4                          ; Multiply by DTYPE DWORD of 4 Bytes, now obtained the BYTE OFFSET in array
            MUL     EDX
            MOV     EDI, [EBP + 8]                  ; Move in EDI ,the address of the first element in the array
            ADD     EDI, EAX                        ; Add offset of current element to address of first element of array
        ; Print Vlaue
            MOV     EAX, [EDI]                      ; Move to EAX the Temp value saved in array memory location addressed by EDI
            CALL    WriteDec
            MOV     AL, " "
            CALL    WriteChar

            ; End Iteration hour
            INC     ECX
            CMP     ECX, DAYS_MEASURED
            JB      _loop_Days2
            ; Inner Loop ends
                
        ; Return to outer loop
        MOV     ECX, int_CurrentHourLoc
        INC     ECX
        MOV     int_CurrentHourLoc, ECX
        CMP     ECX,  TEMPS_PER_DAY
        JB      _loop_Hours2
        CALL    CrLf
        ; Outer Loop ends
        JMP     _end_DisplayTempArray

       _print_Row:
            MOV     ECX, 0
            CALL    CrLf
            ; Iterate through all values in array to print each
            _loop_PrintRow:
                MOV     EDX, ECX
                SHL     EDX, 2
                MOV     ESI, [EBP + 8]
                ADD     ESI, EDX
                MOV     EAX, [ESI]
                CALL    WriteDec
                MOV     AL, " "
                CALL    WriteChar
            
                INC     ECX
                CMP     ECX, DAYS_MEASURED
                JB      _loop_PrintRow

            CALL    CrLf
            CALL    CrLf


    ; End procedure
    _end_DisplayTempArray:
        POP	    EDI
        POP 	ESI
        POP	    EDX
        POP	    ECX
        POP 	EBX
        POP	    EAX
        RET     8

displayTempArray  ENDP



; ==========================================================================================================================
; Generates random Temp values
; receives: addressess of temp array
; returns:
; preconditions: passed address reference
; postconditions:
; registers changed: none
; ==========================================================================================================================
generateTemperatures PROC
    LOCAL   int_MaxDeltaTempLoc:DWORD, int_DirectionTempLoc:DWORD, int_CurrentDeltaTempLoc:DWORD, int_CurrentTempLoc:DWORD, int_CurrentDayLoc:DWORD, int_CurrentHourLoc:DWORD
    PUSH	EAX
    PUSH	EBX
    PUSH	ECX
    PUSH	EDX
    PUSH	ESI
    PUSH	EDI


    ; Stack Layout:
    ; [EBP + 8] = OFFSET arr_TempReadings
    ; [EBP + 4] = return address
    ; [EBP] = old ebp
    ; [EBP - 4] = int_MaxDeltaTempLoc
    ; [EBP - 8] = int_DirectionTempLoc

    ; (EC@) Call sub procedure to get maximum Temperature fluctuation per readings: Maximum 'DeltaT'

    MOV     EAX, EBP
    SUB     EAX, 4
    PUSH    EAX                         ; Push [EBP - 4] = int_MaxDeltaTempLoc
    CALL    get_MaxDeltaTemp


    ; set initial Temp Reading, denoting a temperature after midnight
    MOV     EAX, MAX_TEMP
    MOV     EBX, 3
    MUL     EBX
    SHR     EAX, 2
    MOV		int_CurrentTempLoc, EAX


    ;initialize outer loop counter
    MOV     ECX, 0
    MOV     int_CurrentDayLoc, ECX

    ; Outerloop - number of days
    _loop_Days:

        MOV     ECX, 0                              ; Initialize inner loop
        MOV     int_CurrentHourLoc, ECX
        ; Innerloop - number of days
        _loop_Hours:
             MOV     int_CurrentHourLoc, ECX        ; counter ECX has since incremented, update int_CurrentHour           

            ;  (**EC2) Call subprocedure to determine if the next temperature should decrease or decrease
            PUSH    int_CurrentTempLoc
            MOV     EAX, EBP
            SUB     EAX, 8
            PUSH    EAX                             ; PUSH [EBP - 8] = int_DirectionTempLoc
            PUSH    int_CurrentHourLoc
            CALL    get_DirectionTemp


            MOV     EAX, int_MaxDeltaTempLoc
            CALL    RandomRange
            MOV     int_CurrentDeltaTempLoc, EAX


            ;  Save iteration Temp
            ;  Load Local Variables to registers
            MOV     EAX, int_CurrentTempLoc
            MOV     EBX, int_CurrentDeltaTempLoc
            MOV     EDX, int_DirectionTempLoc


            ;  Check Direction
                _check_Direction:
                    MOV     EDX, int_DirectionTempLoc
                    CMP     EDX, 1
                    JE     _decrease_Temp


            ;  If Direction 0 --> Increase
                _increase_Temp:
                    ADD     EAX, EBX
                    CMP     EAX, MAX_TEMP                   ; Check if new Temp exceeds upper limit, shave it if yes
                    JA     _set_TempToMax
                    JMP     _save_NewTemp

                    ; Shave the new Temp
                    _set_TempToMax:
                    MOV     EAX, MAX_TEMP
                    JMP     _save_NewTemp

            ;  If Direction 1 --> Decrease
                _decrease_Temp:
                    SUB     EAX, EBX
                    CMP     EAX, MIN_TEMP                   ; Check if new Temp exceeds lower limit, shave it if yes
                    JL     _set_TempToMin
                    JMP     _save_NewTemp

                    ; Shave the new Temp
                    _set_TempToMin:
                    MOV     EAX, MIN_TEMP
                    JMP     _save_NewTemp

            ;  Save new Temp in Array
                _save_NewTemp:
                    ; Get current Array offset
                    MOV     int_CurrentTempLoc, EAX         ; Overwrite current Temp with the new Temp
                    MOV     EBX, EAX                        ; Save in EBX the new Temp
                    MOV     EAX, TEMPS_PER_DAY
                    MUL     int_CurrentDayLoc               ; current row * row width
                    ADD     EAX, int_CurrentHourLoc         ; get current column Index in current Row
                    MOV     EDX, 4                          ; Multiply by DTYPE DWORD of 4 Bytes, now obtained the BYTE OFFSET in array
                    MUL     EDX
                    MOV     EDI, [EBP + 8]                  ; Move in EDI ,the address of the first element in the array
                    ADD     EDI, EAX                        ; Add offset of current element to address of first element of array

                    ; Save in Array
                    MOV     [EDI],  EBX                     ; Save the new Temp in memory location addressed by EDI
                            
            ; End Iteration hour
            INC     ECX
            CMP     ECX, TEMPS_PER_DAY
            JB      _loop_Hours
            ; Inner Loop ends
        
        ; Return to outer loop
        MOV     ECX, int_CurrentDayLoc
        INC     ECX
        MOV     int_CurrentDayLoc, ECX
        CMP     ECX,  DAYS_MEASURED
        JB      _loop_Days

    POP	    EDI
    POP 	ESI
    POP	    EDX
    POP	    ECX
    POP 	EBX
    POP	    EAX
    RET     4

generateTemperatures ENDP


; ==========================================================================================================================
; Calculates if Temp will decrease base on conditions: time is before/after sunrise/sunset, and if Temp limits reached
; receives: address for current hour index, address for the Direction toggle, address for the current Temp
; returns:  The Direction toggle - '0': Increase, '1' : Decrease
; preconditions: passed address references
; postconditions:
; registers changed: none
; ==========================================================================================================================
get_DirectionTemp PROC

    ; Stack Layout:
    ; [EBP + 16] = int_CurrentTemp
    ; [EBP + 12] = OFFSET  int_DirectionTemp
    ; [EBP + 8] = int_CurrentHour
    ; [EBP + 4] = return address
    ; [EBP] = old ebp

    ; Register Usage:
    ; EBP - store stack base address


    PUSH    EBP
    MOV     EBP, ESP            ; base pointer for stack-passed parameters
    PUSH	EAX
    PUSH	EBX
    PUSH	ECX
    PUSH	EDX
    PUSH	ESI
    PUSH	EDI


    ; INC if Temp reached lower limit
    MOV     EAX, [EBP + 16]
    CMP     EAX, MIN_TEMP
    JBE     _inc_Temp

    ; DEC if Temp reached upper limet
    MOV     EAX, [EBP + 16]
    CMP     EAX, MAX_TEMP
    JAE     _dec_Temp

    ; DEC/INC if After/Before sunrise/sunset
    ; Check if Sunrise
    MOV     EBX, [EBP + 8]
    MOV     EAX, TEMPS_PER_DAY
    SHR     EAX, 2              ; divided by 4 the number of hours readings per day, denoting sunrise


    CMP     EBX, EAX            ; INC (set int_DirectionTemp = 0) Temp if current hour is after sunrise; otherwise DEC (int_DirectionTemp = 1) 
    JBE      _dec_Temp

    ; Check if Sunset
    MOV     EBX, [EBP + 8]
    MOV     EAX, TEMPS_PER_DAY
    MOV     EDX, 3
    MUL     EDX                 ; multiply by 3 then divided by 4 the number of hours readings per day, denoting sunset
    SHR     EAX, 2


    CMP     EBX, EAX            ; DEC (set int_DirectionTemp = 1) Temp if current hour is after sunset; otherwise INC (int_DirectionTemp = 0) 
    JB     _inc_Temp
          

    ; Toggle direction to "Decrease"
    _dec_Temp:
        MOV     ESI, 1
        JMP     _end_get_DirectionTemp

    ; Toggle direction to "Increase"
    _inc_Temp:
        MOV     ESI, 0

    ; End sub-procedure"
    _end_get_DirectionTemp:
        MOV		EDI, [EBP + 12]     ; save int_DirectionTemp to memory
	    MOV		[EDI], ESI

        POP	    EDI
        POP 	ESI
        POP	    EDX
        POP	    ECX
        POP 	EBX
        POP	    EAX   
        POP     EBP
        RET     12

get_DirectionTemp ENDP


; ==========================================================================================================================
; Calculates Max hourly Temperature fluctuation
; receives: OFFSET int_MaxDeltaTemp
; returns:
; preconditions:
; postconditions: saved value in Memory
; registers changed: none
; ==========================================================================================================================
get_MaxDeltaTemp PROC
    PUSH    EBP
    MOV     EBP, ESP   ;base pointer for stack-passed parameters
    PUSH	EAX
    PUSH	EBX
    PUSH	ECX
    PUSH	EDX
    PUSH	ESI
    PUSH	EDI


    ; Stack Layout:
    ; [EBP + 8] = OFFSET int_MaxDeltaTemp
    ; [EBP + 4] = return address
    ; [EBP] = old ebp

    ; Register Usage:
    ; EBP - store stack base address

    ; 2 * (MAX_TEMP - MIN_TEMP) / TEMPS_PER_DAY
    MOV     EAX, MAX_TEMP
    SUB     EAX, MIN_TEMP
    SHL     EAX, 1
    MOV     EDX, 0
    MOV     EBX, TEMPS_PER_DAY
    DIV     EBX
    
    CMP     EAX, 10
    JB     _end_get_MaxDeltaTemp
    

    ; if resulting DeltaT is greater than 10, shave it
    _shave_DeltaT:
        MOV     EAX, 9


    _end_get_MaxDeltaTemp:
        ; Save EAX in OFFSET of int_MaxDeltaTemp
        MOV		EDI, [EBP + 8]
	    MOV		[EDI], EAX
    
        POP	    EDI
        POP 	ESI
        POP	    EDX
        POP	    ECX
        POP 	EBX
        POP	    EAX
        POP     EBP
        RET     4

get_MaxDeltaTemp ENDP


; ==========================================================================================================================
; Basically a decorated 'WriteString' that preserves the EDX register
; receives: addressess of string
; returns:
; preconditions: passed address reference
; postconditions: string is displayed
; registers changed: none
; ==========================================================================================================================
printGreeting PROC
    PUSH    EBP
    MOV     EBP, ESP
    PUSH    EDX


    ; Stack Layout:
    ; [EBP + 8] = OFFSET str to display
    ; [EBP + 4] = return address
    ; [EBP] = old ebp

    CALL    CrLf
    MOV     EDX, [EBP + 8]
    CALL    WriteString
    CALL    CrLf

    ; Register Usage:
    ; EBP - store stack base address
    ; EDX - store offset for WriteString

    POP     EDX
    POP     EBP
    RET     4

printGreeting ENDP

END main