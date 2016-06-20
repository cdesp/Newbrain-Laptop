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

entity NBVideoContr is
    Port ( 
			  ENABLE: in STD_LOGIC; -- enable disable video	
			  BUSACK: in STD_LOGIC; -- Bus Ack
			  DATA : in  std_logic_vector(8-1 downto 0) ; 
           VIDADDR : out  std_logic_vector(16-1 downto 0) ;           
			  CLOCKIN : in  STD_LOGIC;
           PXLOUT : out  STD_LOGIC;
			  PXLOUT2 : out  STD_LOGIC;			  	
			  CHARDATA : in  std_logic_vector(8-1 downto 0)  ;
			  CHARCOUNT : out  std_logic_vector(4-1 downto 0);			  			  
			  --VIDEO SIGNALS
			  sUCR:in STD_LOGIC; --10 PIXEL PER CHAR
			  s80L:in STD_LOGIC; -- 80 CHARS PER LINE (MEANS 1 PIXEL HORIZ)
			  s3240:in STD_LOGIC; --
			  sFS:in STD_LOGIC; --
			  sRV:in STD_LOGIC; -- REVERSE FIELD
			--  TVP:in STD_LOGIC; -- eNABLE
			  sSETADDR: in STD_LOGIC;	
			  sVIDEO9: in STD_LOGIC;  -- 1 WHEN VIDEO ADDR IS BIG			  
			  TVCLK: OUT STD_LOGIC;
			  Busreq:out STD_LOGIC;
			 -- HSYNCO:out STD_LOGIC;
			 -- VSYNCO:out STD_LOGIC;
           CSYNC: out std_logic;  --composite sync		
			  TEST:OUT STD_LOGIC;
			  NBCLKINT:out STD_LOGIC;
			  NBCOPINT:out STD_LOGIC						
			  );
end NBVideoContr;

architecture Behavioral of NBVideoContr is


--constant staddr:integer :=642;
--Signal staddr: integer range 0 to 32767  := 640; --start video address = 642 decimal 1604
constant stcollo  :std_logic_vector(7-1 downto 0):="0000010";  --"0000010"
constant encollo  :std_logic_vector(7-1 downto 0):="0101001";  --"0101001"

constant stcollo2 :std_logic_vector(7-1 downto 0):="1000010";  --"1000010"
constant encollo2 :std_logic_vector(7-1 downto 0):="1101001";  --"1101001"

constant stcolhi  :std_logic_vector(7-1 downto 0):="0000100";
constant encolhi  :std_logic_vector(7-1 downto 0):="1010011";

Signal staddr:std_logic_vector(8-1 downto 0):="00000101";
Signal bitcnt : integer range 0 to 7:=0;
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
signal addrcol:std_logic_vector(7-1 downto 0);--7 bits 
signal addrrow:std_logic_vector(8-1 downto 0);--8 bits
signal pxlshft:std_logic;
signal FRMST:std_logic;
signal DATAREAD:std_logic;
--signal HSYNCO:std_logic;
--signal VSYNCO:std_logic;
--signal PXLOUT:std_logic;
signal OUTOK:std_logic;
signal sTVCLK:std_logic:='0';
signal VISLN:std_logic:='0';
signal EOT,sEOT2:std_logic:='0'; -- for end of text code 0 on text screen
signal SKIP:std_logic:='0'; -- for narrow screen
signal snarrow:std_logic;
signal RECOUNT:std_logic:='0';
signal RVF:std_logic:='0';
signal sRVUP:std_logic:='0';
signal RVUP:std_logic:='0';
signal pxl:std_logic:='0';
signal sBusreq,enbus,slastbus:std_logic;

------------------------------------
--Video Signals
signal videov, videoh,hsync, vsync : std_logic:='0';
signal hcount : integer range 0 to 1024:=0; --1023 for 16 mhz 720 for 13.5
signal vcount : integer range 1 to 312:=1; -- vert lines 625 312 for a frame
-- convert to use dmultip to use the 27 Mhz crystal real numbers can't be used
--that means 16 times the clock makes 1 microsecond
constant multip : integer range 1 to 32:=16; -- 16 for 1024 16mhz, 13.5 for 720 13.5Mhz
constant dmultip: integer range 1 to 64:=32; -- 32 for 1024 16mhz, 27 for 720 13.5Mhz
constant maxpixels:integer:=1024-1; --1023 for 16mhz, 863 for 13,5
constant maxlines:integer:=312-1;
constant centergap : integer := 48;-- 48 for 16Mhz , 112 for 13.5Mhz
constant vertgap :integer := 45;
constant vertgap2 :integer := 22;
constant leftgap : integer :=(12*dmultip)/2+centergap; --4.7+5.7=10.4us+1.6=12us  =240

constant SSc : integer :=40; --640 div 8

--signal clk:std_logic;
--signal clktmp:std_logic:='0';
signal copcnt:integer range 0 to 195:=0;


--signal sBusReq:std_logic;
signal enbus1:std_logic;
--Signal stinit:std_logic:='0';

--16Mhz Clock makes 1024pixels at 64us
--13.5Mhz Clock makes 864pixels at 64us



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
	  rowcnt<="0000";	   --"0000"	  
	  screenend<='0';  
	  start<='0';	  
	  pxl<='0';
	  RECOUNT<='0';
	  sRVUP<='0';	  	  
	  byteval <= "00000000";
end procedure;

procedure setTextOrGraph is
begin

	if zerocnt=2 and istext='1' then
	   istext<='0'; -- text end
   end if;		
   if istext='1' and (enbus='1' or slastbus='1') then
     zerocnt<=0;
	end if;  
	

	--if (istext='0' and isgraph='0' and zerocnt = 0) or row>249  then--check this 0,0,20,20 end the text screen and no graphics
	if (istext='0' and isgraph='0' and zerocnt = 0) or (VCOUNT>40+250)  then--check this 0,0,20,20 end the text screen and no graphics
     screenend <='1'; 														
   end if;	
	
			IF ISGRAPH='0' AND zerocnt=4 THEN -- check for graphics stream
				 IF (S80L AND S3240)='1' THEN			
				    IF RECOUNT='1'  THEN-- 8 BYTES ON NARROW 80LINE SCREEN
					  isgraph<='1'; -- graph on
					 ELSE 
					  RECOUNT<='1';
					  zerocnt <=0;
					 END IF; 
				 ELSE
				    isgraph<='1'; -- graph on
				 END IF;	 
			END IF; 		
	
	--isgraph<='0';-- disable graph temp 19/6/2016
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
                if addrcol=ENColHi then  --4-84 for 80l
						rowcnt<=rowcnt+1;
						addrcol<=stcolhi;-- start 0n 4
						if (sUCR='0' and rowcnt=9)  or (sUCR='1' and rowcnt=7) then	
						  rowcnt<= "0000";
						  addrrow<=addrrow+1;						  
						end if; 
					 end if;	
     end procedure;
	  procedure doLoDef is
	  begin				  			  
  					  if addrcol=ENCollo  then  --2-41 set 7th bit for odd '01'
						rowcnt<=rowcnt+1;
						addrcol<=stcollo;	-- start on 2
						if (sUCR='0' and rowcnt=9) or (sUCR='1' and rowcnt=7) then					     
						  rowcnt<= "0000";	
						  addrcol<=stcollo2;	--instead of adding a row we just move a little further skip 24 excess bytes							  
						end if; 	  					  
					  elsif addrcol=encollo2   then  --66-106 add new row for even	'01'
						rowcnt<=rowcnt+1;
						addrcol<=stcollo2;
						if (sUCR='0' and rowcnt=9) or (sUCR='1' and rowcnt=7)  then					     
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
				 
			   sRVUP <= DATAIN(7);	--	for charset 1,3 char 128-255 reversed
				
			   setTextOrGraph;	
				
			  -- IF (S3240='1' AND ISGRAPH='1'  AND (HCOUNT<leftgap+64 OR HCOUNT>leftgap+575) ) THEN --512 PIXELS	
				--IF (S3240='1' AND ISGRAPH='1'  AND (col<64 OR COL>575) ) THEN --512 PIXELS	
				--TODO:SUPPORT NARROW SCREEN IN ANOTHER WAY WITH LESS MACROCELLS
		--		IF (S3240='1' AND ISGRAPH='1'  AND snarrow='0' ) THEN --512 PIXELS						
		    --  IF SNArrow='0' THEN
			--		SKIP<='1';
			--	ELSE	
			   --count column bytes 127 for graphics	
				
				IF SKIP='0' THEN
				  addrcol<=addrcol+1; 
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
	CASE BITCNT IS
	  WHEN 0=>DONewByte;
	  WHEN 6=>PREPNxtByte;
	  WHEN 7=>SETNxtByte;
	  when others => 
	END CASE;
		
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
			--THIS WORKS FOR LOW DEF ONLY 
			--TO PUT IT IN THE OTHER START ADDRESS STCOlhi
			addrcol(6)<=svideo9;--set from NB through out 8			
	      pxl<='0';--byteval(bitcnt);
	end if;	
end procedure;

--process start
begin
 
  if FRMST='1' then -- frame start
  
  	initframe; 
  	
  elsif rising_edge(CLOCKIN) then --CLOCK
      
		pxlshft<='0';
	  	
     --get start address from NB through out 9,a
		IF sSETADDR='1' AND BUSACK='1' THEN --AND SCREENEND='1' -- FROM Z80		
			STADDR<=DATAIN ;
		END IF;	
	--if DATAREAD='1' and busack='0' and screenend='0' then --4/5/2016
	if DATAREAD='1'  and screenend='0' then
	   framestart;--run just once at 1st pixel
	  --main screen paint
		 if s80l='1' then
		    NextBit;		    
		 end if;	 
		  redo<=not redo;	 --DOUBLE PIXELS FOR LOW DEF
		  if s80L='0' and redo='1' then
		    NextBit;
		  else 
          if eot='0' or slastbus='1' then
		     bithandler;  
			 elsif bitcnt=0 then 
			  addrcol<=addrcol+1; 		
			 end if; 		  
		  end if;  		           		 			
		--pxlshft <= pxl AND NOT SKIP; 	 				
		pxlshft <= pxl ;		 
	  end if ; -- if frmst & Dataread
	end if; -- if clock	

end process;


---Video Signals
hcounter: process (CLOCKIN)
begin
        if (CLOCKIN'event and CLOCKIN='1') then 
            if hcount>maxpixels then 
                hcount <= 0;
            else 
                hcount <= hcount + 1;
            end if;
        end if;
end process;

process (hcount,VCOUNT)
begin
    
	 --640pixels visible 
	  videoh <= '0';
	--  if vcount=41 and  (hcount>=leftgap-2) and (hcount<(640+leftgap)) then --two more pixels for the 1st line
	 --    videoh <= '1';
	 -- elsif (hcount>=leftgap) and (hcount<(640+leftgap)) then 
	  if (hcount>=leftgap) and (hcount<(640+leftgap)) then 
        videoh <= '1';
     end if;
end process;


vcounter: process (CLOCKIN)
begin
        if (CLOCKIN'event and CLOCKIN='1') then 
            if hcount>maxpixels then 
                if vcount>maxlines then --312 including the 0
                    vcount <= 1;
                else 
                    vcount <= vcount + 1;						  
                end if;
                if copcnt>=195 then -- 194 CAUSE WE START FROM 1 IF START FROM 0 NOT COMPILE
                    copcnt <= 1;
                else 
                    copcnt <= copcnt + 1;						  
                end if;
					 
            end if;
        end if; 
end process;

sync: process (CLOCKIN)

	

	procedure shortp2 is --total must be 64us
	begin	
	  if hcount<=32 then		--2.35us gap low  --- 2,32,34
       vsync <= '0';							--						} 32us half
	  elsif hcount<=32+480 then	--29.65us			---	480=30us
       vsync <= '1';	  
	  elsif hcount<=32+480+32 then	--2.35us gap low
       vsync <= '0';
	  else
	    vsync <= '1';							--30us
	  end if;
	
	end;
	procedure long1short1 is
	Begin
	  if hcount<=480 then		--27.3us	---30,32,34
       vsync <= '0';
	  elsif hcount<=480+32 then	--4.7us gap high
       vsync <= '1';	  
	  elsif hcount<=480+32+32 then	--2.35us gap low -- 32= 3us
       vsync <= '0';
	  else
	    vsync <= '1';							--29.65us 	
	  end if;

	end;
	procedure longp2 is
	begin
	  if hcount<=480 then		--27.3us		-- 448 = 28us
       vsync <= '0';
	  elsif hcount<=480+32 then	--4.7us  -- 64 = 4us
       vsync <= '1';	  
	  elsif hcount<=480+32+480 then	--repeat
	    vsync <= '0';
	  else
	    vsync <= '1';							--
	  end if;
	
	end;

	procedure dohsync is
	begin
				vsync <= '1';--1
            if  hcount<=75  then --4.7us  --- 4  1,65us is front porch
                hsync <= '0';
            else 
                hsync <= '1';
            end if;		
	end;

begin
        if (CLOCKIN'event and CLOCKIN='1') then 
				hsync <= '1';
				vsync <= '1';--1
				case vcount is
				  When 1 to 3 => longP2;
				 -- When 2 => long1short1;
				  When 4 to 6 => shortP2;
				  when 7 to 309 => dohsync;
				  When 310 to 312 => shortP2;
				  when others => vsync<='1';
				end case;
        end if;
end process;


	sEOT2 <= '0' when enbus='1'
	      else '1' when zerocnt=1 and istext='1'
			else sEOT2;
	EOT <='0' when enbus='1' 
				else '1' when sEOT2='1' and zerocnt=0
				else EOT;
  
   RVUP <= sRVUP WHEN sFS='0' AND ISTEXT='1'
	     ELSE '0';
   	
	RVF <= sRV XOR RVUP;
	
	--PXLOUT<= pxlshft XOR RVF when busack='0' and dataread='1' and start='1' and EOT='0' 
	  PXLOUT<= --'1' when dataread='1' and bitcnt=5 and addrcol="0000011"  --we always print the previous and prepare the next byte
	          --'1' when sEOT2='1' and (busack='0' or dataread='1')
				 --'1' when busack='0' ELSE
				-- '1' when Datain="00000000" and dataread='1'
				'0' WHEN SKIP='1'  ELSE
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
	--TEST<= (not EOT) ;
   DATAin <= DATA;
	CHARDATAin <=CHARDATA;	
   --screenend<='1' when row>249 or (istext='0' and isgraph='0' and zerocnt = 0 )
	-- else '0' when FRMST='1'
	-- else screenend;
   Busreq<=
	   sBusreq when slastbus='1'
		else '1' when (EOT='1' and ISTEXT='1') --or SCREENEND='1'	
		else sBusreq;
	--Busreq<=sBusreq; 
 --Video Signals
 
 --	dataO <= videoh and videov when busack='0' else '0';-- if we should output to screen --4/5/2016
	DATAREAD <= videoh and videov when sBusReq='0' else '0';-- if we should output to screen 
		
	--this is for the whole 1024 pixels OF 250 line maybe more
	enbus1<='1' when (hcount>leftgap-48) and (hcount<leftgap+640+8) else '0'; --128 Enabled Bus Request
	enbus<='1' when (hcount>leftgap-48) and (hcount<leftgap)  --Reset EOT Signal = End OF Text
	 else '0';
	slastbus<='1' when (hcount>leftgap+640-48) and (hcount<leftgap+640)--Enable BusReq when EOT=1 just for the last char
	  else '0';  

	SKIP<='1' WHEN S3240='1' AND ISGRAPH='1' AND SNARROW='0' ELSE '0';  
	--snarrow<='1';
	snarrow<='0' when (hcount<leftgap+64) or (hcount>leftgap+575)--Narrow Graphics Screen Area
	 else '1';
	----Busreq <= '0' when vcount>39 and vcount <291  and VGAen='1' else '1'; -- active low	
	
	sBusReq <= '0' when (vcount>40 and vcount <291) and enbus1='1' and ENABLE='1' else '1'; -- active low	
	--BusReq<=sBusReq;
	
--	VISLN <=videov when   busack='0' else '0';
	NBCOPINT <= '0' WHEN copcnt=1 else '1'; -- every 12,5 ms
	NBCLKINT  <= '0' when vcount=1 else '1'; -- every 20 ms
	frmst <= '1' WHEN VCOUNT=40 ELSE '0';
	--hsyncO <= hsync;
	--vsyncO <= vsync;
	--csyncO <= hsync xnor vsync;

	--clk<=clkin;
	videov <= '1' when (vcount>40) and (vcount<=40+250) else '0';
	
	csync <= hsync xnor vsync when (vcount<7) or (vcount>309) or (hcount<=80) else '1'  ;
	
end Behavioral;

