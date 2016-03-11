	NAME SERIAL


	
	GLOBAL RS_INI
	GLOBAL RS_TX
	GLOBAL RS_RX
	GLOBAL RS_TXT
	GLOBAL SERERR ;SET ON MAIN

	;DEVSER EQU $20 ; SET ON MAIN

	RBR EQU DEVSER+0	;RECEIVER BUFFER REGISTER (READ ONLY) (DLAB = 0)
	THR EQU DEVSER+0	;TRANSMITER HOLDING REGISTER (WRITE ONLY) (DLAB = 0)
	IER EQU DEVSER+1	;INTERRUPT ENABLE REGISTER (DLAB = 0)
				;BIT 0= ENABLE RECEIVED DATA AVAILABLE INTERRUPT
				;BIT 1= ENABLE TRANSMITTER HOLDING REGISTER EMPTY INTERRUPT
				;BIT 2= ENABLE RECEIVER LINE STATUS INTERRUPT
				;BIT 3= ENABLE MODEM STATUS INTERRUPT
	IIR EQU DEVSER+2	;INTERRUPT IDENT. REGISTER (READ ONLY)
				;BIT 0=0 IF INTERRUPT PENDING
				;BIT 1-3 INTERRUPT ID
				;BIT 4,5 =0
				;BIT 6,7 FIFOS ENABLED					
	FCR EQU DEVSER+2	;FIFO CONTROL REGISTER (WRITE ONLY)
				;BIT 0 FIFO ENABLE
				;BIT 1 RCVR FIFO RESET
				;BIT 2 XMIT FIFO RESET
				;BIT 3 DMA MODE SELECT
				;BIT 4,5 RESERVED
				;BIT 6 RCVR TRIGGER LSBIT
				;BIT 7 RCVR TRIGGER MSBIT
	LCR EQU DEVSER+3	;LINE CONTROL REGISTER
				;BIT 0,1 WORD LENGTH SELECT
				;BIT 2 NUMBER OF STOP BITS
				;BIT 3 PARITY ENABLE
				;BIT 4 EVEN PARITY SELECT
				;BIT 5 STICK PARITY
				;BIT 6 SET BREAK
				;BIT 7 DIVISOR LATCH ACCESS BIT *** DLAB ****
	MCR EQU DEVSER+4	;MODEM CONTROL REGISTER 
	LSR EQU DEVSER+5	;LINE STATUS REGISTER
				;BIT 0 DATA READY
				;BIT 1 OVERRUN ERROR
				;BIT 2 PARITY ERROR
				;BIT 3 FRAMING ERROR
				;BIT 4 BREAK INTERRUPT
				;BIT 5 TRANSMITTER HOLDING REGISTER
				;BIT 6 TRANSMITTER EMPTY
				;BIT 7 ERROR IN RCVR FIFO
	MSR EQU DEVSER+6	;MODEM STATUS REGISTER
	SCR EQU DEVSER+7	;SCRATCH REGISTER
	DLL EQU DEVSER+0	;DIVISOR LATCH (LEAST SIGNIFICANT BYTE) (DLAB = 1)
	DLM EQU DEVSER+1	;DIVISOR LATCH (MOST SIGNIFICANT BYTE) (DLAB = 1)

;	SERERR EQU RAMAD+2		;SERIAL ERROR
	
				;A=5 RECEIVE INTERRUPTS ON
RS_INI:	
	LD      A,$80          	; Mask to set DLAB on
	OUT     (LCR),A         ; Send to LINe Control Register
	LD      A,12           ; Divisor of 12 = 9600 bps with 1.8432 MHz clock
	;LD      A,96           ; Divisor of 96 = 1200 bps with 1.8432 MHz clock
	;LD      A,72            ; Divisor of 96 = 7200?? bps with 14.7456 MHz clock
				; 72=1600BPS WITH 1.8432
  	
	OUT     (DLL),A        	; Set LSB of divisor
	LD      A,00            ; This will be the MSB of the divisior
	OUT     (DLM),A        	; Send to the MSB register
	LD      A,$03          	; 8 bits, 1 stop, no parity (and clear DLAB)
	OUT     (LCR),A        	; Write new value to LCR
	;LD A, 1+2+4+8							; set fifo mode and reset fifio counters		//was 0 and commented 
  LD A, 0							
  OUT 	(FCR),A						;//was commented
	LD      A,$00          	; 5 A=0 Disable all INterrupts
	OUT     (IER),A        	; Send to INterrupt Enable Register	
	RET
		
; Sends byte IN A to the UART
RS_TX:	CALL    RSTXRD
        OUT     (THR),A
	;CALL CHKERR2 		;CHECK ERROR ON TRANSMITION
        RET

; RETurns when UART is ready to receive

RSTXRD: PUSH    AF
RSTXLP: IN      A,(LSR)        	; fetch the control register
        BIT     5,A             ; bit will be set if UART is ready
        JR      Z,RSTXLP
        POP     AF
        RET		


RS_TXT: LD A,(HL)
	INC HL	
	CP 0
	RET Z
        CALL RS_TX
	JR RS_TXT
	



; Wait for a byte from the UART, and save it IN A

RS_RX:  LD A,1					;ready to receive SIGNAL DSR 1
        OUT (MCR),A
				CALL    RSRXRD              
        XOR A						;NOT ready to receive SIGNAL DSR 0
        OUT (MCR),A        
				;CALL CHKERR 		;CHECK ERROR ON RECEIVED CHAR
				IN      A,(RBR)
        RET

 

; RETurns when UART has received data


RSRXRD: PUSH    AF

RSRXLP: IN      A,(LSR)        	; fetch the conrtol register
        BIT     0,A             ; bit will be set if UART has data
        JR      Z,RSRXLP
        POP     AF
        RET		
	

SHLCD:	PUSH AF
	;CALL LCDLN2
	POP AF
	;CALL LCDCHR	
	RET



CHKERR: PUSH AF
	PUSH HL
	PUSH BC
	IN A,(LSR) 		;FETCH LINE STATUS REGISTER
	BIT 4,A
	JR Z,LBLPE
	LD A,'B' ;$20		;BREAK
	JR EXIT
LBLPE:	BIT 2,A	
	JR Z,LBLFE
	LD A,'P' ;$40		;PARITY
	JR EXIT
LBLFE:	BIT 3,A
	JR Z,LBLOE
	LD A,'F' ;$60		;FRAME	
	JR EXIT
LBLOE:	BIT 1,A
	JR Z,LBLNE
	LD A,'O'  ;$80		;OVERRUN
	JR EXIT
LBLNE:	LD A,'N'		;NO ERROR
EXIT:	LD HL,SERERR
	LD (HL),A
;	CALL SHLCD
	POP BC
	POP HL
	POP AF
	RET

	END
