	NAME UTILS
		
	GLOBAL DELAY
	GLOBAL DEL2
	GLOBAL B2BCD
	GLOBAL BCD2HA
	

	


DELAY:	PUSH AF
	PUSH DE
	LD DE,100	;2000
DEL1:	DEC DE
	LD A,D            ;TEST FOR DE=00
	OR E
	JR NZ,DEL1 	
	POP DE
	POP AF
	RET

DEL2:	PUSH BC
	LD B,80
S1:	CALL DELAY
	DJNZ S1
	POP BC
	RET
	
	
; DE =THE TXT BUFFER	
; HL  =THE NUMBER
OUTASC	PUSH AF
	PUSH DE
	CALL B2Bcd
	POP DE
	CALL BCD2HA
	POP AF
	RET	
	
	;;--------------------------------------------------
;; Binary to BCD conversion
;;
;; Converts a 16-bit unsigned integer into a 6-digit
;; BCD number. 1181 Tcycles
;;
;; input: HL = unsigned integer to convert
;; output: C:HL = 6-digit BCD number
;; destroys: A,F,B,C,D,E,H,L
;;--------------------------------------------------
B2Bcd:
LD BC, 16*256+0 ; handle 16 bits, one bit per iteration
LD DE, 0
cvtLp:
ADD HL, HL
LD A, E
ADC A, A
DAA
LD E, A
LD A, D
ADC A, A
DAA
LD D, A
LD A, C
ADC A, A
DAA
LD C, A
DJNZ cvtLp
EX DE,HL
RET

;;----------------------------------------------------
;; Converts a 6-digit BCD number to a hex ASCII string
;;
;; input: DE = pointer to start of ASCII string
;; C:HL number to be converted
;; output: DE = pointer past end of ASCII string
;; destroys: A,F,D,E
;;-----------------------------------------------------
Bcd2HA:
LD A, C
CALL cvtUN
LD A, C
CALL cvtLN
LD A, H
CALL cvtUN
LD A, H
CALL cvtLN
LD A, L
CALL cvtUN
LD A, L
JR cvtLN
cvtUN:
RRA ; move upper nibble into lower nibble
RRA
RRA
RRA
cvtLN:
AND  0Fh ; isolate lower nibble
ADD A, 90h ; old trick
DAA ; for converting
ADC A, 40h ; one nibble
DAA ; to hex ASCII
LD (DE), A
INC DE
RET




	


	END