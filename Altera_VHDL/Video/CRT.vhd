----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    17:02:12 07/05/2012 
-- Design Name: 
-- Module Name:    memsig - Behavioral 
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
--use IEEE.STD_LOGIC_ARITH.ALL;


-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;
use IEEE.std_logic_unsigned.all; 

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity CRT is
    Port ( 
			  ENABLE: in STD_LOGIC; -- enable disable video	
			  BUSACK: in STD_LOGIC; -- Bus Ack
			  DATA : in  std_logic_vector(8-1 downto 0) ; 
           VIDADDR : out  std_logic_vector(16-1 downto 0) ;
           
			  CLOCKIN : in  STD_LOGIC;
			--  RESET : in  STD_LOGIC;
           PXLOUT : out  STD_LOGIC;
			  PXLOUT2 : out  STD_LOGIC;
		--	  PXLOUT3 : out  STD_LOGIC;
			  
        --   FRMST : in  STD_LOGIC;
          -- DATAREAD : in  STD_LOGIC;
        --   DATAOUT : in  STD_LOGIC;
			  
	
			  CHARDATA : in  std_logic_vector(8-1 downto 0)  ;
			  CHARCOUNT : out  std_logic_vector(4-1 downto 0);
			  
			  
			  --VIDEO SIGNALS
			  UCR:in STD_LOGIC; --10 PIXEL PER CHAR
			  s80L:in STD_LOGIC; -- 80 CHARS PER LINE (MEANS 1 PIXEL HORIZ)
			  S3240:in STD_LOGIC; --
			  S3240_2:in STD_LOGIC;
			  sFS:in STD_LOGIC; --
			  RV:in STD_LOGIC; -- REVERSE FIELD
			--  TVP:in STD_LOGIC; -- eNABLE
			  SETADDR: in STD_LOGIC;	
			  VIDEO9: in STD_LOGIC;  -- 1 WHEN VIDEO ADDR IS BIG
			  
			  TVCLK: OUT STD_LOGIC;
			  Busreq:out STD_LOGIC;
			 -- HSYNCO:out STD_LOGIC;
			 -- VSYNCO:out STD_LOGIC;
           CSYNC: out std_logic;  --composite sync		
			  TEST:OUT STD_LOGIC;
			  NBCLKINT:out STD_LOGIC;
			  NBCOPINT:out STD_LOGIC
			  
			
			  
		--	  TEST1:out STD_LOGIC;
		--	  TEST:out STD_LOGIC
			 --TXTON:in STD_LOGIC --
			  -- TEST:out STD_LOGIC;
				
				--SA:out STD_LOGIC
				
				
			  );
end CRT;

architecture Behavioral of CRT is

constant centergap : integer := 48;-- 48 for 16Mhz , 112 for 13.5Mhz
constant dmultip: integer range 1 to 64:=32; -- 32 for 1024 16mhz, 27 for 720 13.5Mhz
constant leftgap : integer :=(12*dmultip)/2+centergap; --4.7+5.7=10.4us+1.6=12us  =240

--constant staddr:integer :=642;
--Signal staddr: integer range 0 to 32767  := 640; --start video address = 642 decimal 1604
constant stcollo  :std_logic_vector(7-1 downto 0):="0000010";  --"0000010"
constant encollo  :std_logic_vector(7-1 downto 0):="0101001";  --"0101001"

constant stcollo2 :std_logic_vector(7-1 downto 0):="1000010";  --"1000010"
constant encollo2 :std_logic_vector(7-1 downto 0):="1101001";  --"1101001"

constant stcolhi  :std_logic_vector(7-1 downto 0):="0000100";
constant encolhi  :std_logic_vector(7-1 downto 0):="1010011";

Signal staddr:std_logic_vector(8-1 downto 0):="00000101";
--Signal staddrIN:std_logic_vector(8-1 downto 0):="00000101";
--Signal addrcnt : integer range 0 to 32767;
--Signal addrvec:std_logic_vector(15-1 downto 0);
Signal bitcnt : integer range 0 to 8:=0;

--signal row:integer range 0 to 313:= 0;
signal col:integer range 0 to 645 := 0;
signal row:integer range 0 to 250 := 0;
signal DATANEXT: std_logic_vector(8-1 downto 0);--holds the next data byte 
signal DATACUR: std_logic_vector(8-1 downto 0);--holds the next data byte 
signal byteval: std_logic_vector(8-1 downto 0); -- current byte to shift out
signal Datain: std_logic_vector(8-1 downto 0); -- Data buffer
signal CHARDATAin: std_logic_vector(8-1 downto 0); -- Data buffer
signal istext:std_logic;
signal isgraph:std_logic;
signal istest:std_logic;
signal zerocnt:integer range 0 to 4:=0;--check for graph or end of screen
signal rowcnt:std_logic_vector(4-1 downto 0); --count char pattern row
signal redo:std_logic; -- for 40 or 80 line chars
signal screenend:std_logic;
signal start:std_logic;
--signal charend:std_logic;

signal addrcol:std_logic_vector(7-1 downto 0);--7 bits 
signal addrrow:std_logic_vector(8-1 downto 0);--8 bits
--signal strtcnt:std_logic_vector(2-1 downto 0);--2 bits
signal pxlshft:std_logic;
--signal datout:std_logic;

signal FRMST:std_logic;
signal DATAREAD:std_logic;
--signal HSYNCO:std_logic;
--signal VSYNCO:std_logic;
--signal PXLOUT:std_logic;

signal OUTOK:std_logic;

--signal UCR:std_logic:='0';
--signal s80L:std_logic:='0';
--signal S3240:std_logic:='0';
--signal sFS:std_logic:='0';
--signal RV:std_logic:='0';

signal sTVCLK:std_logic:='0';
signal VISLN:std_logic:='0';
signal EOT:std_logic:='0';
signal SKIP:std_logic:='0';
signal RECOUNT:std_logic:='0';
signal RVF:std_logic:='0';
signal sRVUP:std_logic:='0';
signal RVUP:std_logic:='0';
signal pxl:std_logic:='0';

signal sBusreq,enbus,sEOT2,sNewFrm,sOdd,slastbus:std_logic;
signal snarrow:std_logic;


begin



process (CLOCKIN,enbus,FRMST,DATAREAD,ENABLE) --CLOCK

procedure initframe is
begin
     if s80L='0' then
			 addrcol<=stcollo; --stcollo; --skip 2 bytes for initilization
	  else
			 addrcol<=stcolhi; --skip 4 bytes for initilization
	  end if;
	  
	  addrrow<=staddr	  ;--"00000101";--5=start address =640
    
	  
	  bitcnt <= 7;
	  redo<='1';
	 -- istest <= '1';
	  istext <= '1';
	  isgraph <= '0'; 
	  zerocnt <=0;
	  --col <=0;
	  row <=0;
	  rowcnt<="0000";	   --"0000"
	  --sNewFrm<='0';
	  sOdd<='1';
	  
	  --screenend<='0';  
	  start<='0';
	  
	  pxl<='0';
	--  datout<='0';
	  --EOT<='0';

	  RECOUNT<='0';
	  sRVUP<='0';
	  	  
	  byteval <= "00000000";
end procedure;





procedure setTextOrGraph is
begin

	if zerocnt=2 and istext='1' then
	   istext<='0'; -- text end
   end if;		

--	if istext='0' and isgraph='0' and zerocnt = 0  then--check this 0,0,20,20 end the text screen and no graphics
 --     screenend <='1'; 
  -- end if;	
	
			IF ISGRAPH='0' AND zerocnt=4 THEN -- check for graphics stream
				 IF (S80L AND S3240)='1' THEN			
				    IF RECOUNT='1'  THEN-- 8 BYTES ON NARROW 80LINE SCREEN
					  isgraph<='1'; -- graph on
					  SKIP<='1';				  
					 ELSE 
					  RECOUNT<='1';
					  zerocnt <=0;
					 END IF; 
				 ELSE
				    isgraph<='1'; -- graph on
					 SKIP<='1';
				 END IF;	 
			END IF; 		
	--isgraph<='0';-- disable graph temp 9/5/2016
end procedure;

procedure DoGraphNxtRow is
begin
  if addrcol="1111111"  then
        addrrow<=addrrow+1;				 
  end if; 
end procedure;

procedure DoTextNxtRow is

     procedure doHiDef is
     Begin
                if addrcol=encolhi then  --4-84 for 80l
						rowcnt<=rowcnt+1;
					--	col<=0;
						addrcol<=stcolhi;-- start 0n 4
						--EOT<='0';
						if (UCR='0' and rowcnt=9)  or (UCR='1' and rowcnt=7) then	
						  rowcnt<= "0000";
						  addrrow<=addrrow+1;						  
						end if; 
					 end if;	
     end procedure;
	  procedure doLoDef is
	  begin
  					  if addrcol=encollo  then  --2-41 set 7th bit for odd '01'
						rowcnt<=rowcnt+1;
						row<=row+1;
					--	col<=0;
						addrcol<=stcollo;	-- start on 2
						--EOT<='0';
						if (UCR='0' and rowcnt=9) or (UCR='1' and rowcnt=7) then					     
						  rowcnt<= "0000";	
						  addrcol<=stcollo2;	--instead of adding a row we just move a little further skip 24 excess bytes							  
						end if; 	  					  
					  elsif addrcol=encollo2   then  --66-106 add new row for even	'01'
						rowcnt<=rowcnt+1;
						row<=row+1;
					--	col<=0;
						addrcol<=stcollo2;
						--EOT<='0';
						if (UCR='0' and rowcnt=9) or (UCR='1' and rowcnt=7)  then					     
						  rowcnt<= "0000";
						  addrcol<=stcollo;					 
						  addrrow<=addrrow+1;
						end if; 						
					  end if;												   
	  end procedure;


begin
    if s80L='1' then -- hi def 80 pixels per line
	  doHiDef; --9/5/2016 temp
    else --s80l='0' low def 40 pixels per line					  
	  doLoDef;
	 end if; -- else s80l =0  
end procedure;

procedure prepNxtByte is
begin			  
			  
			  if istext='1' then			   	
				 DATANEXT<=chardataIN;		 
			  else --graph
			    DATANEXT<=DATAIN; -- get the next data as we had the time to stabilize ram  
			  end if;	
			  
			  if DATAIN="00000000" then -- test end of screen or eot or graph
				    zerocnt<=zerocnt+1;
			  else
				    zerocnt<=0;
			  end if;  		
			
end procedure;

procedure setNxtByte is
begin
            byteval<=DATANEXT;					
				IF ISTEXT='1' AND DATAIN="00000000" THEN
				  byteval<=DATAIN;					  	        	
			   END IF;			  
end procedure;		

procedure doNewByte is
begin
				SKIP<='0';	 
			   sRVUP <= DATAIN(7);	--	for charset 1,3 char 128-255 reversed
				
			   setTextOrGraph;	
				
			   IF (S3240='1' AND ISGRAPH='1'  AND (col<64 OR COL>575) ) THEN --512 PIXELS	
				--TODO:SUPPORT NARROW SCREEN IN ANOTHER WAY WITH LESS MACROCELLS
				--IF (S3240='1' AND ISGRAPH='1'  AND snarrow='0' ) THEN --512 PIXELS						
					SKIP<='1';
				ELSE	
				  addrcol<=addrcol+1; --count column bytes 127 for graphics	
				  --				  
				END IF;	
				
				
				
				if isgraph='1' then
				  DoGraphNxtRow;
				END IF; 
				if istext='1' then
				  DoTextNxtRow;							  
			   end if;		-- if is text						
			
end procedure;

procedure bithandler is
begin
         if bitcnt=0 then				
				doNewByte;	
			elsif bitcnt=6  then	--PREPARE NEXT BYTE				  
			  prepNxtByte;
			 elsif bitcnt=7  then	
			  -- bitcnt<=0;		
           	setNxtByte;					
			end if; --if bit
		
         pxl<=byteval(bitcnt); 		
end procedure;

procedure NextBit is
begin
             bitcnt <= bitcnt + 1;
				 if bitcnt=7  then	
			      bitcnt<=0;	
				 end if;	
				  
end procedure;


procedure framestart is
begin
  if  start='0' then --getting the data byte we need >70ns			
		   start<='1';			
			--bitcnt <= 0;  --was 1
			--redo<='0';
			
			--addrcol<=stcollo;			
			
			addrcol(6)<=video9;--set from NB through out 8
			
			--addrcol<=addrcol+1;
--			if (DATAIN="00000000") then
		--	  EOT<='1';
	--		END IF;		
			--byteval<=chardataIN;	
	      pxl<='0';--byteval(bitcnt);

	end if;	

end procedure;

--process start
begin
 
  if FRMST='1' then -- frame start
  
  	initframe; 
  	
  elsif rising_edge(CLOCKIN) then --CLOCK
      
		--if enbus='1' then
       -- EOT<='0';		
		  --if istext='1' then			   	
			 -- BYTEVAL<=chardataIN;
		--  END IF;	 
		  
	    -- BYTEVAL	<="01000010";
		  --doNewByte;
      --end if;
		pxlshft<='0';
	  	
     --get start address from NB through out 8,a
		IF SETADDR='1' AND BUSACK='1' THEN --AND SCREENEND='1' -- FROM Z80		
			STADDR<=DATAIN ;
		END IF;	
		

		
		
	--if DATAREAD='1' and busack='0' and screenend='0' then --4/5/2016
	if DATAREAD='1'  and screenend='0' then
		
	   framestart;--run just once at 1st pixel
	  
	  --main screen paint
	      
			
		 if s80l='1' then
		    NextBit;		    
		 end if;
		 
		  redo<=not redo;	 
		  if s80L='0' and redo='1' then
		    NextBit;
		  else 
		    if eot='0' or slastbus='1' then
		     bithandler;  
			 elsif bitcnt=0 then 
			  addrcol<=addrcol+1; 
			  --col<=col+1;
			  DoTextNxtRow;	
			 end if; 
		  end if;  
		     
      
		 				
		
		pxlshft <= pxl AND NOT SKIP; 	 				
		
		
		--these should be deleted not needed when we put screenend =1 somewhere else
		--  col<=col+1;		 	  
		--   if col>=639 then
		--	  col<=0;
			--  row<=row+1;
			  --EOT<='0';
			--  bitcnt<=0;
		--	end if;
			--if row>249 then
			 -- screenend<='1';
			 -- EOT<='0';
			--end if;
			
		 
	
	  end if ; -- if frmst & Dataread
	end if; -- if clock	

end process;


	sEOT2 <= '0' when enbus='1'
	      else '1' when zerocnt=1 and istext='1'
			else sEOT2;
	EOT <='0' when enbus='1' 
				else '1' when sEOT2='1' and zerocnt=0
				else EOT;
  
   RVUP <= sRVUP WHEN sFS='0' AND ISTEXT='1'
	     ELSE '0';
   	
	RVF <= RV XOR RVUP;
	
	--PXLOUT<= pxlshft XOR RVF when busack='0' and dataread='1' and start='1' and EOT='0' 
	  PXLOUT<= --'1' when dataread='1' and bitcnt=5 and addrcol="0000011"  --we always print the previous and prepare the next byte
	          --'1' when sEOT2='1' and (busack='0' or dataread='1')
				 --'1' when busack='0'
				  pxlshft XOR RVF when busack='0' and dataread='1' and start='1' and EOT='0' --and (addrcol="0000010" or addrcol="0000011" or addrcol="0000100"  )
				--else '1' when (enbus='1') --and  (col=0 or col=639) else
				ELSE '0' XOR RVF when  busack='0' and dataread='1' and start='1' and EOT='1' 
				ELSE '0' ;
	
--   PXLOUT<= pxlshft XOR RVF when busack='0' and dataread='1' and start='1' and EOT='0'  
--				ELSE '0' ;	
	
	--PXLOUT2<= '1' when dataread='1' and (col=0 OR col=639 OR row=0 or row=249 OR col=319 or row=124 ) else '0';

   VIDADDR <= '0'&addrrow&addrcol when  busack='0' 
			else (others=>'Z');


	--CHARCOUNT <= rowcnt when busack='0' and dataread='1' 
	CHARCOUNT <= rowcnt;-- when busack='0' and dataread='1' 
	    --else (others=>'0');										  --UCR=0 ~ 8x10chars  UCR=1 ~ 8x8 char
	
	
	
   sTVCLK <= '1' WHEN (BITCNT=3  and dataread='1') or (FRMST='1') 
				 ELSE '0';--textclock in char
	TVCLK <= sTVCLK;
	
	--EOT<='1' when (col>2*8*8)
	 -- else '0';
	Busreq<=
	   sBusreq when slastbus='1'
		else '1' when (EOT='1' and ISTEXT='1') --or SCREENEND='1'		
		else sBusreq;
	
	--Busreq<=sBusreq;
	
	
	--TEST<= (not EOT) ;
	
   DATAin <= DATA;
	CHARDATAin <=CHARDATA;	
	
  -- VIDADDR <= '0'&std_logic_vector(to_unsigned(addrrow,8))&std_logic_vector(to_unsigned(addrcol,7)) when busack='0' and dataread='1' else (others=>'Z');			
 --VIDADDR <= std_logic_vector(to_unsigned(addrcnt,16)) when   dataread='1' else (others=>'Z');
 --VIDADDR <= '0'&addrvec when busack='0' and dataread='1' else (others=>'Z');
 --CHARCOUNT <= std_logic_vector(to_unsigned(rowcnt,4)) when busack='0' else (others=>'0');

 	
	--STADDR<=DATAIN when SETADDR='1' AND BUSACK='1'
	-- else STADDR;
	
   screenend<='1' when row>249 or (istext='0' and isgraph='0' and zerocnt = 0 )
	 else '0' when FRMST='1'
	 else screenend;
 
   vga1 : entity work.vga port map(CLOCKIN,sBusreq,busack,ENABLE,DATAREAD,CSYNC, FRMST,NBCLKINT,NBCOPINT,enbus,slastbus,snarrow); 
	
end Behavioral;

