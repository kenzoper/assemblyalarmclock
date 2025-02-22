
$NOLIST
$MODN76E003
$LIST

;  N76E003 pinout:
;                               -------
;       PWM2/IC6/T0/AIN4/P0.5 -|1    20|- P0.4/AIN5/STADC/PWM3/IC3
;               TXD/AIN3/P0.6 -|2    19|- P0.3/PWM5/IC5/AIN6
;               RXD/AIN2/P0.7 -|3    18|- P0.2/ICPCK/OCDCK/RXD_1/[SCL]
;                    RST/P2.0 -|4    17|- P0.1/PWM4/IC4/MISO
;        INT0/OSCIN/AIN1/P3.0 -|5    16|- P0.0/PWM3/IC3/MOSI/T1
;              INT1/AIN0/P1.7 -|6    15|- P1.0/PWM2/IC2/SPCLK
;                         GND -|7    14|- P1.1/PWM1/IC1/AIN7/CLO
;[SDA]/TXD_1/ICPDA/OCDDA/P1.6 -|8    13|- P1.2/PWM0/IC0
;                         VDD -|9    12|- P1.3/SCL/[STADC]
;            PWM5/IC7/SS/P1.5 -|10   11|- P1.4/SDA/FB/PWM1
;                               -------
;



CLK           EQU 16600000 ; Microcontroller system frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

HOURS         equ P1.6
MINUTES       equ P1.2
SECONDS       equ P0.5
SOUND_OUT     equ P1.7
TOGGLE_MODE   equ P1.1
ALARM_ON_OFF  equ P1.5
TOGGLE_AM_PM  equ P1.0

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
Seconds_counter:  ds 1 
Minutes_counter:  ds 1
Hours_counter: ds 1
Hours_counter_alarm: ds 1
Minutes_counter_alarm: ds 1



; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
full_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
adjust_mode: dbit 1       ; 0 - adjust time, 1 - adjust alarm
on_off: dbit 1 ; control if alarm is on or off
alarm_sounded: dbit 1
alarm_silenced: dbit 1
am_pm: dbit 1
am_pm_alarm: dbit 1


cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P1.3
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P1.4
LCD_D4 equ P0.0
LCD_D5 equ P0.1
LCD_D6 equ P0.2
LCD_D7 equ P0.3

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'Time    :  :  A ', 0
Second_Message:   db 'Alarm 12:00A off', 0
On_Message: db 'on ',0
Off_Message: db 'off', 0
AM_Message: db 'A', 0
PM_Message: db 'P', 0
Indicator: db '-', 0
Clear_msg: db ' ', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	orl CKCON, #0b00001000 ; Input for timer 0 is sysclk/1
	mov a, TMOD
	anl a, #0xf0 ; 11110000 Clear the bits for timer 0
	orl a, #0x01 ; 00000001 Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz wave at pin SOUND_OUT   ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	; Timer 0 doesn't have 16-bit auto-reload, so
	clr TR0
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	setb TR0
	cpl SOUND_OUT ; Connect speaker the pin assigned to 'SOUND_OUT'!
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	orl T2MOD, #0x80 ; Enable timer 2 autoreload
	mov RCMP2H, #high(TIMER2_RELOAD)
	mov RCMP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
	orl EIE, #0x80 ; Enable timer 2 interrupt ET2=1
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for Timer 2                 ;
;---------------------------------;
Timer2_ISR:
    clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in the ISR. It is bit addressable.
    cpl P0.4 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
    
    ; The two registers used in the ISR must be saved in the stack
    push acc
    push psw
    
    ; Increment the 16-bit one millisecond counter
    inc Count1ms+0    ; Increment the low 8-bits first
    mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
    jnz Inc_Done
    inc Count1ms+1

Inc_Done:
    ; Check if half second has passed
    mov a, Count1ms+0
    cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
    mov a, Count1ms+1
    cjne a, #high(1000), Timer2_ISR_done
    
    ; 500 milliseconds have passed. Set a flag so the main program knows
    setb full_seconds_flag ; Let the main program know half second had passed
    
    jb alarm_sounded, beep_toggle1
    sjmp skip_toggle
    
beep_toggle1:
	jb on_off, beep_toggle2
	sjmp skip_toggle
beep_toggle2:
	jnb alarm_silenced, beep_toggle3
	sjmp skip_toggle
beep_toggle3:
	cpl TR0
    
skip_toggle:
    ; Reset to zero the milliseconds counter, it is a 16-bit variable
    clr a
    mov Count1ms+0, a
    mov Count1ms+1, a
    
    ; Increment seconds
    mov a, Seconds_counter
    add a, #0x01
    sjmp Timer2_ISR_da

Timer2_ISR_da:
    da a ; Decimal adjust instruction. Check datasheet for more details!
    
    cjne a, #0x60, store_seconds
    clr a                      
    mov Seconds_counter, a     
    mov a, Minutes_counter     
    add a, #0x01               
    da a                       
    cjne a, #0x60, store_minutes
    clr a                      
    mov Minutes_counter, a
    mov a, Hours_counter
    add a, #0x01
    da a
    cjne a, #0x12, next_check
	cpl am_pm
next_check:
    cjne a, #0x13, store_hours
    mov a, #0x01
store_hours:
	mov Hours_counter, a
	sjmp Timer2_ISR_done    

store_minutes:
    mov Minutes_counter, a     
	sjmp Timer2_ISR_done
	
store_seconds:
    mov Seconds_counter, a        
    
    
Timer2_ISR_done:
    pop psw
    pop acc
    reti


;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
    ; Initialization
    mov SP, #0x7F
    mov P0M1, #0x00
    mov P0M2, #0x00
    mov P1M1, #0x00
    mov P1M2, #0x00
    mov P3M1, #0x00
    mov P3M2, #0x00
          
    lcall Timer0_Init
    lcall Timer2_Init
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
    Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    Set_Cursor(2, 1)
    Send_Constant_String(#Second_Message)
    setb full_seconds_flag
    setb adjust_mode
    clr am_pm_alarm
    clr am_pm
    clr alarm_sounded
    clr on_off
    mov Seconds_counter, #0x00
    mov Minutes_counter, #0x00
    mov Hours_counter, #0x12
    mov Minutes_counter_alarm, #0x00
    mov Hours_counter_alarm, #0x12

    ; After initialization the program stays in this 'forever' loop
loop:
    jb TOGGLE_MODE, am_pm_toggle  
    Wait_Milli_Seconds(#50)  
    jb TOGGLE_MODE, am_pm_toggle 
    jnb TOGGLE_MODE, $      
  
  	cpl adjust_mode
  
    ljmp loop_b 
    
am_pm_toggle:
    jb TOGGLE_AM_PM, loop_hours  
    Wait_Milli_Seconds(#50)  
    jb TOGGLE_AM_PM, loop_hours  
    jnb TOGGLE_AM_PM, $      
  
  	jb adjust_mode, toggle_alarm
  	cpl am_pm
  	sjmp continue
  	
toggle_alarm:
	cpl am_pm_alarm
continue:
    ljmp loop_b 
            
    
loop_hours:
	jb HOURS, loop_minutes
	Wait_Milli_Seconds(#50)
	jb HOURS, loop_minutes
	jnb HOURS, $
	
	jb adjust_mode,hours_alarm
	
	mov a, Hours_counter
	add a, #0x01
	da a
	cjne a, #0x12, next_check1
	cpl am_pm
next_check1:
	cjne a, #0x13, update_hours
	mov a, #0x01
update_hours:
	mov Hours_counter, a
	sjmp loop_b 
hours_alarm:
	mov a, Hours_counter_alarm
	add a, #0x01
	da a
	cjne a,#0x13, update_alarm_hours
	mov a, #0x01
update_alarm_hours:
	mov Hours_counter_alarm, a
	sjmp loop_b

loop_minutes:
	jb MINUTES, loop_seconds
	Wait_Milli_Seconds(#50)
	jb MINUTES, loop_seconds
	jnb MINUTES, $
	
	jb adjust_mode,minutes_alarm
	
	mov a, Minutes_counter
	add a, #0x01
	da a
	cjne a, #0x60, update_minutes
	clr a
update_minutes:
	mov Minutes_counter, a
	sjmp loop_b
minutes_alarm:
	mov a, Minutes_counter_alarm
	add a, #0x01
	da a
	cjne a,#0x60, update_alarm_minutes
	clr a
update_alarm_minutes:
	mov Minutes_counter_alarm, a
	sjmp loop_b

loop_seconds:
	jb adjust_mode, skip_seconds
	jb SECONDS, loop_on_off
	Wait_Milli_Seconds(#50)
	jb SECONDS, loop_on_off
	jnb SECONDS, $
	
	mov a, Seconds_counter
	add a, #0x01
	da a
	cjne a, #0x60, update_seconds
	clr a
update_seconds:
	mov Seconds_counter, a
	sjmp loop_b        
skip_seconds:
            
loop_on_off:
    jb ALARM_ON_OFF, loop_a  
    Wait_Milli_Seconds(#50)  
    jb ALARM_ON_OFF, loop_a 
    jnb ALARM_ON_OFF, $      
  
  	cpl on_off
  
    sjmp loop_b   

loop_a:
    jb full_seconds_flag, skip_jump
    ljmp loop
    
skip_jump: ;ensure target address is not too far
	    
loop_b:
    clr full_seconds_flag       ; Clear this flag in the main loop, but it is set in the ISR for timer 2
    
    ; Display the seconds counter
    Set_Cursor(1, 13)           ; Set LCD cursor for seconds display
    mov a, Seconds_counter      ; Load the seconds counter value into the accumulator
    Display_BCD(a)              ; Display the BCD value of the seconds counter

    ; Display the minutes counter
    Set_Cursor(1, 10)           ; Set LCD cursor for minutes display
    mov a, Minutes_counter      ; Load the minutes counter value into the accumulator
    Display_BCD(a)              ; Display the BCD value of the minutes counter
    
    ; Display the hours counter
    Set_Cursor(1, 7)            ; Set LCD cursor for hours display
    mov a, Hours_counter        ; Load the hours counter value into the accumulator
    Display_BCD(a)              ; Display the BCD value of the hours counter
    
    ; Display the alarm hours counter
    Set_Cursor(2, 7)            
    mov a, Hours_counter_alarm  
    Display_BCD(a)  
    
    ; Display the alarm minutes counter
    Set_Cursor(2, 10)            
    mov a, Minutes_counter_alarm  
    Display_BCD(a)   
    
    jb adjust_mode, alarm_indicator
    Set_Cursor(2,6)
    Send_Constant_String(#Clear_msg)
    Set_Cursor(1,5)
    Send_COnstant_String(#Indicator)
    sjmp skip_indicator
    
alarm_indicator:
	Set_Cursor(2,6)
    Send_Constant_String(#Indicator)
    Set_Cursor(1,5)
    Send_COnstant_String(#Clear_msg)


skip_indicator: 
    
    jb am_pm_alarm, display_pm_alarm
    Set_Cursor(2,12)
    Send_Constant_String(#AM_Message)
    sjmp display_done1
    
display_pm_alarm:
	Set_Cursor(2,12)
	Send_Constant_String(#PM_Message)  
	
display_done1:
	jb am_pm, display_pm
    Set_Cursor(1,15)
    Send_Constant_String(#AM_Message)
    sjmp display_done2
    
display_pm:
	Set_Cursor(1,15)
	Send_Constant_String(#PM_Message)  


display_done2:   
    jnb on_off, display_off
    Set_Cursor(2, 14)
    Send_Constant_String(#On_Message)
    sjmp alarm_check                
	
display_off:
    Set_Cursor(2, 14)
    Send_Constant_String(#Off_Message)
    
alarm_check:
	jb alarm_sounded, check1
	
	jnb on_off, alarm_off   
	
	mov a, Hours_counter
	cjne a, Hours_counter_alarm, alarm_off
	
	mov a, Minutes_counter
	cjne a, Minutes_counter_alarm, alarm_off
	
	jnb am_pm, check_am_pm
	jb am_pm_alarm, alarm_match
	sjmp alarm_off

check_am_pm:
	jnb am_pm_alarm, alarm_match
	sjmp alarm_off

alarm_match:
	setb TR0
	setb alarm_sounded
	sjmp continue_loop

check1:
	jb on_off, check2  
	clr TR0
	setb alarm_silenced
check2:
	mov a, Hours_counter
	cjne a, Hours_counter_alarm, alarm_off
	
	mov a, Minutes_counter
	cjne a, Minutes_counter_alarm, alarm_off

	sjmp continue_loop
	
alarm_off:
	clr TR0
	clr alarm_sounded
	clr alarm_silenced

continue_loop:
	ljmp loop


END

