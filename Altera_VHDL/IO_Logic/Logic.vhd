----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    13:23:19 07/05/2014 
-- Design Name: 
-- Module Name:    Inout - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;




entity Logic is
Port ( 

			   -- 12-1
			  CLOCK:in STD_LOGIC; -- main clock 4Mhz
			--  CLOCKout:out STD_LOGIC; -- main clock 4Mhz
           VADDRLow : in  std_logic_vector(8-1 downto 0) ; -- A0-A7
			 
			  -- 83 - 71
			  
			  DATA : inout  std_logic_vector(8-1 downto 0) ;  -- D0-D7
			  
			  
			  -- 70 - 57
			  RAMEN:OUT STD_LOGIC; -- ~ROMEN OUT
			  ROMEN:OUT STD_LOGIC; -- ~ROMEN OUT
			  sIORQ:in STD_LOGIC; -- ~IORQ active low
			  MREQ:in STD_LOGIC; -- ~MREQ
			  NMI:OUT STD_LOGIC; -- ~NMI vcc
			  INT:OUT STD_LOGIC; -- ~INT we have an interrupt
			--  RDO:OUT STD_LOGIC; -- ~RDout copy from rd
			--  WRO:OUT STD_LOGIC; -- ~WRout copy from wr
			  RS232INT:in STD_LOGIC; -- RS232 interrupt we have an event
			  RS232SEL:out STD_LOGIC; --active low
			--  RESETin:in STD_LOGIC; -- ~RESET in from button
			  -- 45 - 56
			  A15:in STD_LOGIC; -- A15 
			  RD:inout STD_LOGIC; -- ~RD from z80
			  WR:inout STD_LOGIC; -- ~WR from z80
			  BUSACK:IN STD_LOGIC; -- ~BUSACK from z80
			  sWAIT:in STD_LOGIC; -- ~WAIT to z80
			  BUSREQ:out STD_LOGIC; -- ~BUSREQ to z80
			  RESET:in STD_LOGIC; -- ~RESET 
			  sM1:in STD_LOGIC; -- ~M1 active low from z80
			 -- nRESET:out STD_LOGIC; -- RESET inv of ~reset active high
			  --PS/2 Interface
			  PS2_Clk:inOUT std_logic;
			  PS2_Data:in std_logic;	
			  
			  -- 32 - 43
			 
			  
			  
			  BUSACKout2:out STD_LOGIC; -- ~BUSACK to devices	
			  BUSACKout:out STD_LOGIC; -- ~BUSACK to devices			  
			  BUSREQin:in STD_LOGIC; -- ~BUSREQ from devices
			 -- sWAITin:in STD_LOGIC; -- ~WAIT from Devices
			 -- LCD16SEL:out STD_LOGIC; --LCD 16x2 selected
			  
			  
			  -- 15 - 31
			  sUCR:out STD_LOGIC; --10 PIXEL PER CHAR
			  s80L:out STD_LOGIC; -- 80 CHARS PER LINE (MEANS 1 PIXEL HORIZ)
			  s3240:out STD_LOGIC; --
			  s3240_2:out STD_LOGIC; --
			  sFS:out STD_LOGIC; --
			  sRV:out STD_LOGIC; -- REVERSE FIELD
			  sTVPower:out STD_LOGIC; --	
			  SETADDR:out STD_LOGIC; --	set the video address
			  VIDEO9:out STD_LOGIC; --	set the video address 9TH BIT 
			  
			  CLK50:in STD_LOGIC; --	50hz clock for the clock
			  CLKCOP:in STD_LOGIC; --	 clock for the reg cop int
			  
			  
			  --other
			 -- RDMEM:out STD_LOGIC
			  REVO:out STD_LOGIC; --	
			  EPRWR:out STD_LOGIC; --	
			  
		--	  TEST:out STD_LOGIC; --
		--	  TEST1:out STD_LOGIC; --
			 -- IKB:out STD_LOGIC; --
			 CTS:in STD_LOGIC; -- v24 cts
			 RTS:out STD_LOGIC; -- v24 rts
			 RX:in STD_LOGIC; -- v24 rx
			 TX:out STD_LOGIC; -- v24 tx
			 
			  
			  LCDRS:out STD_LOGIC; -- PUT RS = DATA(1) 
			  LCDEN:out STD_LOGIC; ----LCD 16x2 selected WHEN 1
			  
			 -- HALTO:out STD_LOGIC; --	
			--  ZEROO:out STD_LOGIC; --	
			  TWOO:out STD_LOGIC; --
			  IOH:out STD_LOGIC; --			  
			  DISPEN:out STD_LOGIC --	
			  );
end Logic;

architecture Behavioral of Logic is


	COMPONENT PS2Main
	PORT(
		Clk : IN std_logic;
		Reset : IN std_logic;
		PS2_Clk : IN std_logic;
		PS2_Data : IN std_logic;
		DoRead : IN std_logic;          
		Scan_Err : OUT std_logic;
		Scan_DAV : OUT std_logic;
		Outenable : IN std_logic;
		Scan_Code : OUT std_logic_vector(7 downto 0)
		);
	END COMPONENT;



   COMPONENT d_ff_srss
	PORT(
		d : IN std_logic;
		clk : IN std_logic;
		reset : IN std_logic;
		set : IN std_logic;		
		q : OUT std_logic
		);
	END COMPONENT;

signal intr:integer range 0 to 255;
signal COP:STD_LOGIC;
signal TVL9:STD_LOGIC;
signal TVL8:STD_LOGIC;

 									
signal mydata :  std_logic_vector(8-1 downto 0);
signal COPCTL :  std_logic_vector(8-1 downto 0);
signal COPCTL2 :  std_logic_vector(8-1 downto 0);
signal ENABLEREG :  std_logic_vector(8-1 downto 0);
--signal validdata:STD_LOGIC;

signal COPINT:STD_LOGIC; -- set from cop
signal COPINTpre:STD_LOGIC; -- set from cop
signal FRMFREQ:STD_LOGIC:='1'; -- 50Hz should get it from other XC9572 FS
signal FRMFREQpre:STD_LOGIC;
signal cntr:integer:=0;
signal CLK4:STD_LOGIC; -- clock for 50hz

--PS/2 Interfacing
Signal DoRead,Scan_Err,Scan_DAV,KBOutenable:std_logic:='0';
Signal KB_Int:std_logic:='1'; --active low
Signal KB_Stop:std_logic:='0'; 

Constant KBPORT:std_logic_vector(8-1 downto 0) :=x"46";--70;
--Constant LCD16PORT:std_logic_vector(8-1 downto 0) :=x"40";--64;
--Constant LCD16PORT2:std_logic_vector(8-1 downto 0) :=x"41";--65;
Constant RS232PORT:std_logic_vector(8-1 downto 0) :=x"20";--32;
Constant RS232PORT1:std_logic_vector(8-1 downto 0) :=x"21";--33;
Constant RS232PORT2:std_logic_vector(8-1 downto 0) :=x"22";--34;
Constant RS232PORT3:std_logic_vector(8-1 downto 0) :=x"23";--35;
Constant RS232PORT4:std_logic_vector(8-1 downto 0) :=x"24";--36;
Constant RS232PORT5:std_logic_vector(8-1 downto 0) :=x"25";--37;
Constant RS232PORT6:std_logic_vector(8-1 downto 0) :=x"26";--38;
Constant RS232PORT7:std_logic_vector(8-1 downto 0) :=x"27";--39;
Constant LCD16NEWPT:std_logic_vector(8-1 downto 0) :=x"40";--64;




signal DATAin:std_logic_vector(8-1 downto 0);
signal DATAout:std_logic_vector(8-1 downto 0);
signal VidCTRsig:std_logic_vector(8-1 downto 0);
Signal RevRamRom:std_logic:='0';
Signal ROMEN1,RAMEN1:std_logic;
Signal A15Z:std_logic;
Signal RevRRset:std_logic;
Signal A15Znot,MREQnot,BUSACKnot,RevRAmRomnot:std_logic;
signal BUSACKr:std_logic;

Signal rs232ce:std_logic;
Signal IRQ:std_logic;

Signal sRVsig,s3240sig,sUCRsig,s80Lsig,sFSsig,sVideo9:std_logic:='1';
Signal EPRWRsig:std_logic:='0';


signal tclkcnt:integer range 0 to 63;
Signal tCLK,GENCLK:STD_LOGIC;
Signal CPLDdev:STD_LOGIC:='0';--1 when dataout is given by cpld

Signal RDin:STD_LOGIC:='0';
Signal RDout:STD_LOGIC:='0';
Signal WRin:STD_LOGIC:='0';

signal dispset,dispreset:STD_LOGIC:='0';
signal dispval:STD_LOGIC:='0';
signal dispvis:STD_LOGIC:='1';

signal crton:std_logic:='0';
signal rtvon:std_logic:='0';


signal halfclock:std_logic:='0';


signal cop80:std_logic:='0';
signal sBUSREQ:std_logic:='0';


SIGNAL frmfreqon:std_logic:='1';

signal sLCDEN:std_logic:='0';
signal sLCDRS:std_logic:='0';

signal sT1:std_logic:='0';
signal sT2:std_logic:='0';

--signal copcnt:integer range 0 to 32;
--SIGNAL sCopint:std_logic:='1';

begin


--process(clock)
--begin
      --if rising_edge(clock) then
        
--			halfclock <= not halfclock;
      --end if;
--end process;


--Process (clock)
--begin
 --if  rising_edge(clock) then
-- tCLK<='0';
-- tclkcnt<=tclkcnt+1;
-- if tclkcnt=23 then
--  tclkcnt<=0;
--  tCLK<='1';
 
 --end if;
-- end if;
--end process;

process (RevRRset,RESET)
begin
  if RESET='0' then
	 RevRamRom<='0';
  elsif rising_edge(RevRRset) then
	 RevRamRom<='1';
  end if;
end process;


--process (sIORQ,RESETin)
--begin
--  if RESETin='0' then
    --IOH<='1';
	--elsif  sIORQ'event and sIORQ = '0' and BUSACK='1'  then 
     --   IOH<='0';	
	--end if;
--end process;


process (GENCLK,RESET,DATA)--
begin
	if RESET='0' then
	 
	  
	 
	  RevRRset<='0';
	  
	elsif  rising_edge(GENCLK)  then 	  
		
	 
	--  HALFCLOCK<=NOT HALFCLOCK;
	
	
	  RevRRset<='0';
	  if  RevRamRom='0' and A15='1' and busack='1' then
	    RevRRset<='1'; --FLIP ROM AND RAM
	  end if;
	  
	
	end if; 
	 
end process;	 


--process (halfclock)
--begin
--  if  rising_edge(halfclock)  then
--    copcnt<=copcnt+1;
	 --if copcnt=24 then
	   --copcnt<=0;
	 --end if;
  
--  end if;

--end process;



	Inst_disp_FFmem: d_ff_srss PORT MAP(
	   d => dispvis,
		clk => CLOCK,
		reset => dispreset,
		set => dispset,
		q => dispvis
	);
	
	Inst_PS2Main: PS2Main PORT MAP(
		Clk => CLOCK,
		Reset => Reset,
		PS2_Clk => PS2_Clk,
		PS2_Data => PS2_Data,
		DoRead => DoRead,  -- the cpu has read the scan code
		Scan_Err => Scan_Err,  
		Scan_DAV => Scan_DAV,  -- we have a char ready should raise an int
		Outenable => KBOutenable,
		Scan_Code => MYDATA 
	);	
	

	  --ROM ENABLE - RAM ENABLE
	  A15Z<=A15 and busack; --only A15 from Z80 device	  	  
	  A15Znot<=not A15;
	  MREQnot<= MREQ;
	  BUSACKnot<=BUSACK;
	  BUSACKr<=not BUSACK;
	  RevRAmRomnot<=not RevRAmRom;
	  
	
	 ROMEN1<= '0' WHEN RevRAmRom='0' and MREQ='0' AND A15='0' AND BUSACK='1' --ROM AT 0000
		  ELSE '0' WHEN RevRAmRom='1' and MREQ='0' AND A15='1' AND BUSACK='1' --ROM AT 8000
		  ELSE '1'; 

	 RAMEN1<= '0' WHEN RevRAmRom='0' and MREQ='0' AND A15='1'  --RAM AT 8000
		  ELSE '0' WHEN RevRAmRom='1' and MREQ='0' AND A15='0'  --RAM AT 0000
		  ELSE '0' WHEN BUSACK='0' 									  --DISPLAY READ
		  ELSE '1'; 
	
	  
	 RD<= '0' WHEN BUSACK='0' else 'Z';--inout signal we use it only when busack is active
	 WR<= '1' WHEN BUSACK='0' else 'Z';
	 
	
	
	 ROMEN<= ROMEN1;
    RAMEN<= RAMEN1;
	 
	
 
	 
	-- LCD16SEL<=lcd16ce ; --active HIGH??
	 RS232SEL<=rs232ce ;-- active LOW??
	 
	
   DATAin<=DATA;
	RDin<=RD;
	WRin<=WR;	
	
	--RDO <= RDin;
	--WRO <= WR;
	--RESET <= RESETin;
	--nRESET	<= not RESET;
	--sWAIT <= sWAITin;
	
	NMI <='1';
	--INT <='1';
	
	

	BUSACKout<=BUSACK;
	BUSACKout2<=BUSACK;

	--Clockout<=halfclock; -- for testing
	GENCLK<=clock;
	

	
	DATA<=DATAout when CPLDdev='0' else (others=>'Z');
	

   REVO<=RevRAmRom;-- led should be on when we reverse ram and rom
	
 
	
--	RevRamRom <='1' WHEN RevRamRom='0' and A15='1' and busack='1'
--		    ELSE	'1' when RevRamRom='1'
--			 ELSE '0';
--INTERRUPT HANDLER
	--IOH<=IRQ;
	IRQ <= '0' WHEN sIORQ='0' and BUSACK='1' and sM1='1' 
	 ELSE  '1';
	 
--	ZEROO <= '1' WHEN IRQ='0' and  VADDRLow=x"00"
--	 ELSE '0';
--	HALTO <= '1' WHEN IRQ='0' and  VADDRLow=x"01"
--	 ELSE '0';
--	TWOO  <= '1' WHEN IRQ='0' and  VADDRLow=x"02"
--	 ELSE '0';
	--Z80 INTERRUPT IN
	process (clk50,genclk)
	begin
 	  if  (genclk='1' and IRQ='0' and  VADDRLow=x"04" ) or frmfreqon='1' then
		  FRMFREQ<='1';

     elsif falling_edge(clk50) then
		 if  frmfreqon='0' then
		  FRMFREQ<='0';		 
	    end if;
	  end if;
	end process;

	process (CLKCOP,genclk)
	begin
	  if  genclk='1' and IRQ='0' and  VADDRLow=x"14" AND RDin='0' then
		  COPINT<='1';		  
	  elsif falling_edge(CLKCOP) then
		  COPINT<='0';		 	    
	  end if;
	end process;

	
 --  FRMFREQ<='0' WHEN CLK50='0'  AND frmfreqon='0' and rising_edge(GENCLK)
--		 ELSE '1' WHEN	(IRQ='0' and  VADDRLow=x"04" ) or frmfreqon='1' --9/9/2016 frfreqon
--		-- ELSE '1' WHEN IRQ='0' and  VADDRLow=x"14" AND RDin='0' --4/5/2016 9/9/2016 commented
--	    ELSE FRMFREQ;
   FRMFREQpre<=FRMFREQ;
--   COPINT <='0' WHEN CLKCOP='0' 
--	    ELSE '1' WHEN IRQ='0' and  VADDRLow=x"14" AND RDin='0' --was WRin='0' 7/2/2016 ,VADDRLow=x"06" 4/5/2016
--	   -- ELSE '1' WHEN IRQ='0' and  VADDRLow=x"06" AND WRin='0' --was WRin='0' 7/2/2016 ,VADDRLow=x"06" 20/12/2016
--		 ELSE COPINT;
	COPINTpre<=COPINT;
	
--	TEST<=FRMFREQ;
--	TEST1<=COPINT;
	
	KB_Int <= not Scan_DAV ;
	--INT <= FRMFREQpre and COPINTpre and KB_int ; -- ACTIVE LOW
	INT <= FRMFREQ and COPINT and KB_int ; -- ACTIVE LOW --9/9/2016
	PS2_CLK<= '0' WHEN KB_INT='0' ELSE 'Z';
	KB_Stop<='1' WHEN KB_Int='0'  and mydata=x"77" --BREAK KEY
	   ELSE  '0' WHEN COPCTL=x"D0"
		ELSE KB_Stop;--Break key pressed
	--TEST<=FRMFREQ;
	--TEST1<=COPINT;
	
	CPLDdev <= '0' WHEN IRQ='0' and  RDin='0' AND (VADDRLow=x"06" OR VADDRLow=x"14" OR VADDRLow=KBPORT OR VADDRLow=x"03" OR VADDRLow=x"16") -- WE GIVE DATA 
	  ELSE '1';

	Doread <='1' WHEN IRQ='0' AND  WRin='0' AND  VADDRLow=KBPORT+1  --KEYBOARD SIGNAL WE READ THE CHAR
	    ELSE '0';
 	rs232ce<='0' WHEN IRQ='0' AND (VADDRLow=RS232PORT or VADDRLow=RS232PORT1 or VADDRLow=RS232PORT2 or VADDRLow=RS232PORT3
										or VADDRLow=RS232PORT4 or VADDRLow=RS232PORT5 or VADDRLow=RS232PORT6 or VADDRLow=RS232PORT7)
	    ELSE '1';
	 --NOT NEEDED
   --KBOutEnable <= '1' 	WHEN IRQ='0' and VADDRLow=KBPORT	--KEYBOARD CIRCUIT IS ENABLED SO WE CAN READ MYDATA
	--			ELSE  '0';	

	EPRWRsig <= '1' WHEN IRQ='0' and  VADDRLow=x"7C" --124 EPROM WRITABLE
		    ELSE '0' WHEN IRQ='0' and  VADDRLow=x"7D" --125 EPROM NOT WRITEABLE		
	       ELSE '0' WHEN RESET='0'
		    ELSE EPRWRsig;
   EPRWR <= not EPRWRsig; --34
	sT1<=	Data(0) WHEN IRQ='0' and  VADDRLow=90 and WRin='0'   --124 EPROM WRITABLE	 
	 else sT1;
	TWOO <= sT1;--33

	
	dispreset <= '1' WHEN IRQ='0' and  VADDRLow=x"7E" --126
	 ELSE '0'; 
	dispset <= '1' WHEN IRQ='0' and  VADDRLow=x"7F" --127
	 ELSE '0';	 


	--OUT 12,13,14,15
	VidCTRsig <= DATAin WHEN IRQ='0' AND WRin='0' AND (VADDRLow=x"0C" or VADDRLow=x"0D" or VADDRLow=x"0E" or VADDRLow=x"0F")
	 ELSE VidCTRsig;
	sRVsig   <= VidCTRsig(0); -- 1 reverse screen colors (0)
	sFSsig   <= VidCTRsig(1); -- 1 generates 256 chars from 8bit, 0 128 chars and 128 reverse chars (1)
	s3240sig <= VidCTRsig(2); -- 1 256 or 512 horz pixels , 0 320 or 640 (0)
	sUCRsig  <= VidCTRsig(3); -- 1 256 chars 8x8 , 0 256 chars 8x10 (0)
	s80Lsig  <= VidCTRsig(6); -- 1 80 chars , 0 40 chars (0)
	--sRVsig      <=  DATAin(0) WHEN IRQ='0' AND WRin='0' AND (VADDRLow=x"0C" or VADDRLow=x"0D" or VADDRLow=x"0E" or VADDRLow=x"0F")
--	  ELSE sRVsig;
--	sFSsig         <=  DATAin(1) WHEN IRQ='0' AND WRin='0' AND (VADDRLow=x"0C" or VADDRLow=x"0D" or VADDRLow=x"0E" or VADDRLow=x"0F")
	  --ELSE sFSsig;
	--s3240sig    <=  DATAin(2) WHEN IRQ='0' AND WRin='0' AND (VADDRLow=x"0C" or VADDRLow=x"0D" or VADDRLow=x"0E" or VADDRLow=x"0F")
--	  ELSE s3240sig;
	--sUCRsig     <=  not DATAin(3) WHEN IRQ='0' AND WRin='0' AND (VADDRLow=x"0C" or VADDRLow=x"0D" or VADDRLow=x"0E" or VADDRLow=x"0F")
--	  ELSE sUCRsig;
--	s80Lsig     <=  DATAin(6) WHEN IRQ='0' AND WRin='0' AND (VADDRLow=x"0C" or VADDRLow=x"0D" or VADDRLow=x"0E" or VADDRLow=x"0F")
--	  ELSE s80Lsig;


 
	  
	--out 7 EnableReg:=Value;
	ENABLEREG<=DATAin WHEN IRQ='0' AND WRin='0' AND (VADDRLow=x"07" )
	  ELSE ENABLEREG;	
	frmfreqon <= ENABLEREG(0); --0 enables frame frequency interrupts
	rtvon  <=  ENABLEREG(2); -- 1 enables display
	RTS <=  ENABLEREG(4);
	TX <=  ENABLEREG(5);
	
	--in  22 read for v24 NOT NEEDED WE JUST SEND THEM STRAIGHT TO Z80
	--RX <=  DATAin(0) when IRQ='0' and RDin='0' and (VADDRLow=x"16")
	--	ELSE RX;
	--CTS <=  DATAin(1) when IRQ='0' and RDin='0' and (VADDRLow=x"16")
	--	ELSE CTS;
	
	
	  --out 6 cop control
	COPCTL<=DATAin WHEN IRQ='0' AND WRin='0' and  (VADDRLow=x"06")
		ELSE COPCTL;
   
	

		
  --out 6 cop comm
	--COP80<= '1' WHEN  IRQ='0' AND WRin='0' AND (VADDRLow=x"06" ) AND COPCTL=x"80" --TOGGLE COP80 WHEN OUT 6,$80
	--	ELSE '0' WHEN  IRQ='0' AND WRin='0' AND (VADDRLow=x"06" ) --AND DATAin/=x"80";
	--	ELSE '0' WHEN RESET='0'
	--	ELSE COP80;
	-- TEST<='1' WHEN  COPCTL=x"80"
	--	ELSE '0';
		--TEST<=KB_STOP; --TEST THIS IF IT WORKS
		
	COPCTL2<= 		"00110"&KB_Stop&"00" WHEN IRQ='0' and  RDin='0' AND  VADDRLow=x"06" AND KB_Int='0' -- AND COP80='0' --KEYB iNTERRUPT IN 6
				ELSE  "00000"&KB_Stop&"01" WHEN IRQ='0' AND  RDin='0' AND  VADDRLow=x"06" AND KB_Int='1'  AND COPCTL=x"80"		--0 at bits 4-7 means regint IN 6
				ELSE  "00000"&KB_Stop&"00" WHEN IRQ='0' AND  RDin='0' AND  VADDRLow=x"06" AND KB_Int='1'  AND COPCTL/=x"80"
				ELSE  COPCTL2;
	
--IN CPLDEV SHOULD BE 0 SO WE GIVE THE DATA
   DATAout <= 
			 COPCTL2 WHEN IRQ='0' and  RDin='0' AND  VADDRLow=x"06"
	  ELSE mydata WHEN IRQ='0' AND RDin='0' AND VADDRLow=KBPORT -- from ps/2 data
	--  ELSE COPINTpre&'1'&FRMFREQpre&"00101" WHEN IRQ='0' and  RDin='0' AND  VADDRLow=x"14" --IN 20 STATUS REGISTER
	  ELSE COPINT&'1'&FRMFREQ&"00101" WHEN IRQ='0' and  RDin='0' AND  VADDRLow=x"14" --IN 20 STATUS REGISTER 9/9/2016
																				--bit 1 is pwrup should be 0 when we are ready
     ELSE COPCTL WHEN IRQ='0' AND RDin='0' AND VADDRLow=x"3"	
	  ELSE "101000"&CTS&RX WHEN IRQ='0' AND RDin='0' AND VADDRLow=x"16"	--IN 22 GET V24 SIGNALS (ZEROES NOT USED)
	  ELSE "00000000";
	  --ELSE (others=>'Z');	    																		
	
	--IKB <= KB_INT;
	
	
	sUCR<=sUCRsig;
	s80L<=s80Lsig;
	s3240<=s3240sig;
	s3240_2<=s3240sig;
	sFS<=sFSsig;
	sRV<=sRVsig;	
	sTVPower<=rtvon; -- rtvon  is like newbrain enablereg bit 2
	VIDEO9<=sVideo9;
	SetAddr <= '1' WHEN IRQ='0' AND WRin='0' AND (VADDRLow=x"09" )
	  ELSE '0';
	sVideo9 <= '1' WHEN IRQ='0' AND WRin='0' AND (VADDRLow=x"08" )
		ELSE	  '0' WHEN IRQ='0' AND WRin='0' AND (VADDRLow=x"09" )
		ELSE sVideo9;
	
	--DISPEN<=dispvis; -- if video is on dispvis is out 126 127
	DISPEN<=rtvon;
	sBUSREQ <= BUSREQin when rtvon='1' else '1';-- was dispvis
	BUSREQ <= sBUSREQ;

	
	sLCDEN <= '1' WHEN IRQ='0' AND (VADDRLow=LCD16NEWPT )
   	ELSE '0';  --OUT PORT,DATA
	--sLCDRS <= DATAin(1) WHEN sIORQ='0' and BUSACK='1' AND WRin='0' AND (VADDRLow=LCD16NEWPT+1 ) AND rising_edge(GENCLK)  ELSE sLCDRS; --OUT PORT+1,RS SIGNAL
	sLCDRS <= DATAin(1) WHEN IRQ='0' AND WRin='0' AND (VADDRLow=LCD16NEWPT+1 )  ELSE sLCDRS;
	
	LCDEN <=sLCDEN; -- PIN 41 MPLE
	LCDRS <=sLCDRS; -- PIN 40 ASPRO
	
	
end Behavioral;

