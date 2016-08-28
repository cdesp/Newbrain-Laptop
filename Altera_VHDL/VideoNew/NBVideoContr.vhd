----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    17:02:12 21/06/2016 
-- Design Name: 
-- Module Name:    NBVideoContr - Behavioral 
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
use IEEE.NUMERIC_STD.all;
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
			  NBCOPINT:out STD_LOGIC;

			  PXLCLKOUT:out STD_LOGIC;
			  CLK4:in STD_LOGIC;
			  M1:in STD_LOGIC;
			  
			  SysClk:in STD_LOGIC;
			  SysClk16:out STD_LOGIC;
			  SysClk4:out STD_LOGIC;
			  
			  BTN1:IN STD_LOGIC;
			  BTN2:IN STD_LOGIC;
			  BTN3:IN STD_LOGIC;
		  
			  LED1:out STD_LOGIC;
			  LED2:out STD_LOGIC;
			  LED3:out STD_LOGIC;
			  LED4:out STD_LOGIC;
			  LED5:out STD_LOGIC;
			  LED6:out STD_LOGIC;
			  LED7:out STD_LOGIC;
			  LED8:out STD_LOGIC
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

constant TestPat  :std_logic_vector(8-1 downto 0):="10011001";

Signal staddr:std_logic_vector(15-1 downto 0);--:="00000101";
--Signal grphaddr:std_logic_vector(15-1 downto 0);--:="00000101";
Signal nxtstaddr:std_logic_vector(8-1 downto 0):="00000101";
Signal VAddr:std_logic_vector(15-1 downto 0);
Signal Addr:integer range 0 to 32767;
Signal grphaddr:integer range 0 to 32767;
Signal bitcnt : integer range 0 to 7:=0;
--signal DATANEXT: std_logic_vector(8-1 downto 0);--holds the next data byte 
signal DATACUR: std_logic_vector(8-1 downto 0);--holds the next data byte 
signal CharDATACUR: std_logic_vector(8-1 downto 0);--holds the next data byte 
signal DATACURnext: std_logic_vector(8-1 downto 0);--holds the next data byte 
signal CharDATACURnext: std_logic_vector(8-1 downto 0);--holds the next data byte 
signal byteval: std_logic_vector(8-1 downto 0); -- current byte to shift out
signal Datain: std_logic_vector(8-1 downto 0); -- Data buffer
signal CHARDATAin: std_logic_vector(8-1 downto 0); -- Data buffer
signal istext:std_logic;
signal isgraph:std_logic;
signal istest:std_logic;
signal zerocnt:integer range 0 to 10:=0;--check for graph or end of screen
signal linecnt:integer range 0 to 30:=0;--count text lines max 25
signal chartp:integer range 0 to 85:=0;--count chars to print per line max 80
signal rowcnt:std_logic_vector(4-1 downto 0); --count char pattern row
signal rows:integer range 0 to 500:=0;--pixel row counter
signal grphrow:integer range 0 to 500:=0; --what row graphic screen start
signal grphdiff:integer range 0 to 500:=0; --what row graphic screen start
signal redo:std_logic; -- for 40 or 80 line chars
signal screenend:std_logic;
signal start:std_logic;
signal addrcol:std_logic_vector(7-1 downto 0);--6 bits 
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
signal pxltest:std_logic:='0';
signal sBusreq,enbus,slastbus:std_logic;
Signal col :integer range 0 to 645:=0; --640 max
signal colend:integer range 0 to 645:=640;
signal prnrow:integer range 0 to 250:=0;--printable rows counter
--signal  colend:integer range 0 to 640:=320;
signal chrend:std_logic;
signal lnend:std_logic;
constant tvckbit:integer:=0;
signal strt:boolean;

signal stmp:std_logic;
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
signal clk8:std_logic;--8Mhz clock
signal pxlclk:std_logic;
--Signal stinit:std_logic:='0';

signal showvid:std_logic:='0';
signal VClock:std_logic;  --16mhz
signal cpuClock:std_logic;  --4mhz
signal c16,c4,c8:std_logic;  

--16Mhz Clock makes 1024pixels at 64us
--13.5Mhz Clock makes 864pixels at 64us

signal areset_sig,locked_sig:std_logic;


signal t1,t2:std_logic;
signal isNarrow:std_logic;

signal nxtrow,nxtpixel,nxtchar,nxtline:std_logic;
signal prenxtchar:std_logic;
signal char80,getdata:std_LOGIC;
signal charsperline:integer range 0 to 130;--max 128 for text 64 or 128
signal graphperline:integer range 0 to 130;--max 128 for graph 40 or 80


begin


---Video Signals
hcounter: process (vclock)
begin
        if rising_edge(vclock) then 
            if hcount>maxpixels then 
                hcount <= 0;
            else 
                hcount <= hcount + 1;
            end if;
        end if;
end process;


vcounter: process (vclock)
begin
        if rising_edge(vclock) then 
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

sync: process (vclock)

	

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
            if  hcount<=75  then --4.7us  --- 4  1,65us is front porch
                hsync <= '0';
            end if;		
	end;

begin
        if (vclock'event and vclock='1') then 
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


--=================================================================================================
--=================================================================================================
--=================================================================================================
--=================================================================================================

--process (pxlclk,videov)
--variable t:boolean;  --bit counter
--
--procedure donextpixel is
--Begin
--	    if char80='0' then
--	--	  t:=not t;
--		  --t:=true;
--	--	 else
--	--	  t:=true; 
--	      if col mod 2=0 then
--		     t:=true;
--		   elsE
--		     t:=false;
--		   end if; 
--		 else 
--		   t:=true; 
--		 end if;
--		 col<=col+1;
--	    if t=true then        
--		  nxtpixel<='1';	 
--		 end if; 
--
--end procedure;
--
--begin
--  
--  if rising_edge(pxlclk) and videov='1' then
--    nxtpixel<='0';	   
--    nxtrow<='0';   
--    if (hcount=leftgap and char80='1') or (hcount=leftgap and char80='0')  then
--	  col<=0;	  
--	  t:=true;
--    elsif col<colend  then	
--	   donextpixel;
--	 elsif col=colend then
--	   nxtrow<='1';
--	 end if; 
--  end if;	 
--end process;
--
--process (pxlclk,col,nxtpixel,hcount)
--begin
--   
--  if hcount=leftgap-2 then
--    getdata<='1';
--	 bitcnt<=7;
--	 stvCLK<='1';
--	 nxtchar<='0';
-- 	-- datacur<=datacurnext;
--	 --charDATACUR<=charDATACURnext;
--  elsif hcount=leftgap-1 then	 
--    nxtchar<='1';
--  elsif hcount=leftgap then
--	 bitcnt<=0;  	 
--	 getdata<='0';
-- 	 datacur<=datacurnext;
--	 charDATACUR<=charDATACURnext;	 
--	 nxtchar<='1';
--	 stvCLK<='0';
--  elsif col=colend then	 
--    nxtchar<='0';
--  elsif rising_edge(pxlclk) and nxtpixel='1'  then
--    getdata<='0';
--    nxtchar<='0';
--    bitcnt<=bitcnt+1; 
--	 stvCLK<='0';
--	 if bitcnt=2 then
--	      
--		stvCLK<='1';
--	 elsif (bitcnt=7 and char80='1') or (bitcnt=7 and char80='0' and col mod 2=1)  then	 
--		nxtchar<='1';
--		datacur<=datacurnext;
--		charDATACUR<=charDATACURnext;
--		
----	 elsif bitcnt>7 then
-- --     bitcnt<=0; 
--  --    nxtchar<='1';		
--	 end if;	
--  end if;  
--end process;
--
--process (nxtrow,frmst)
--begin
--  if frmst='1' then
--    rows<=0;
--	 nxtline<='0';	  
--  elsif rising_edge(nxtrow) then
--   rows<=rows+1;
--	nxtline<='0';	  
--	if rows mod 10=0 then  -- todo 8 pixel height of char
--	  rowcnt<="0000";
--	  nxtline<='1';	  
--	 else
--	  rowcnt<=rowcnt+1;     
--	end if;
--  end if;	
--
--end process;
--
--process (pxlCLK,nxtchar,hcount,col)
--Begin
--  if rising_edge(pxlCLK) then --and hcount>leftgap+6 then
--    prenxtchar<=nxtchar;
--	if hcount<=leftgap-7 then
--	  chartp<=0;
--	  prenxtchar<='0';
--	elsif nxtchar='1' and ((prenxtchar/=nxtchar and char80='0') or char80='1')   then
--     chartp<=chartp+1; 	  
--	end if;  
--	
--end if;
--
----   if hcount<leftgap-1 then
----	  chartp<=0;
----	elsif hcount<=leftgap+2 then
----    chartp<=1;
----	elsif rising_edge(nxtchar) then --and hcount>leftgap+6 then
----	    chartp<=chartp+1; 
----	end if;  
--end process;
--
--
--process (nxtline,frmst,rows)
--Begin
--  if frmst='1' then
--    linecnt<=1;
--  elsif rising_edge(nxtline) and rows>3 then --next line
--    linecnt<=linecnt+1;
--  end if;
--end process;
--
--process (pxlclk,frmst,staddr,linecnt,chartp,istext)
--begin
-- if frmst='1' then
--   addr<=to_integer(unsigned(staddr));
-- elsif rising_edge(pxlCLK) then   
--    if isgraph='1' then
--	   addr<=grphaddr+grphdiff*graphperline+chartp;	 --done: change 40 to a var depends on s80l	  
--	 else
--	   addr<=to_integer(unsigned(staddr))+(linecnt-1)*charsperline+chartp ; 	 
--	 end if;
-- end if; 
--end process;
--
--process (frmst,nxtSTADDR)
--Begin
-- -- staddr<=nxtSTADDR&"0000010";
--     if char80='0' then
--	  	  staddr<=nxtSTADDR&"0000010";
--	  else	  
--	     staddr<=nxtSTADDR&"0000100"; 
--	  end if;	  
--End process;
--
--process (pxlCLK,frmst,getdata,col)
--Begin
--  if frmst='1' then
--    zerocnt<=0; 
--	 stmp<='0';
----  elsif getdata='1' then
----      DatACUR<=Datain;
----	   charDATACUR<=chardaTAin; 
----      if chartp=1 then
----		  stmp<='1';
----		 elsE
----		   stmp<='0'; 
----		end if;
--  elsif rising_edge(pxlCLK) then
--      if getdata='1' then
--        DatACURnext<=Datain;
--	     charDATACURnext<=chardaTAin; 
--		end if;
--		
--	if isgraph='1' then
--	  if (nxtpixel='1' and bitcnt=7 and char80='0') or (nxtpixel='1' and bitcnt=0 and char80='1') then
--      DatACURnext<=Datain;	 
--		if chartp=3 then
--		  if grphdiff=3 and datacur=128 then
--		    stmp<='1';
--		  end if;
--		end if;
--		
--		if chartp=10 then
--		  if grphdiff=10 then
--		     DatACURnext<="00000001";
--		  elsIF grphdiff=11 then	  
--		      DatACURnext<="00000010";
--		  elsIF grphdiff=12 then	  
--		      DatACURnext<="00000100";
--		  elsIF grphdiff=13 then	  
--		      DatACURnext<="00001000";
--		  elsIF grphdiff=14 then	  
--		      DatACURnext<="00010000";
--		  elsIF grphdiff=15 then	  
--		      DatACURnext<="00100000";
--		  elsIF grphdiff=16 then	  
--		      DatACURnext<="01000000";
--		  elsIF grphdiff=17 then	  
--		      DatACURnext<="10000000";				
--		  end if;		
--		end if;
--		if chartp=3 then
--		  if grphdiff=10 then
--		      DatACURnext<="00000001";
--		  elsIF grphdiff=11 then	  
--		      DatACURnext<="00000001";
--		  elsIF grphdiff=12 then	  
--		      DatACURnext<="00000001";
--		  elsIF grphdiff=13 then	  
--		      DatACURnext<="00000001";
--		  elsIF grphdiff=15 then	  
--		      DatACURnext<="10000000";
--		  elsIF grphdiff=16 then	  
--		      DatACURnext<="10000000";
--		  elsIF grphdiff=17 then	  
--		      DatACURnext<="10000000";
--		  elsIF grphdiff=18 then	  
--		      DatACURnext<="10000000";				
--		  end if;		
--		end if;
--		
--   	if datacur=3 then
--		  stmp<='1';
--		end if;
--	  end if; 		
--	else
--	  stmp<='0';
--   -- if (nxtpixel='1' and bitcnt=0 and char80='0') or (nxtpixel='1' and bitcnt=0 and char80='1')then
--	   if (nxtpixel='1' and bitcnt=7 and char80='0') 
--		 or (nxtpixel='1' and bitcnt=0 and char80='1') or col=6  then
--      DatACURnext<=Datain;
--	   charDATACURnext<=chardaTAin;  
--   	 if screenend='0' and isgraph='0' and dataCur="00000000" then
--	      zerocnt<=zerocnt+1;
--	    else 
--	      zerocnt<=0;
--       end if;
--	 end if;	 
--	end if;
--	
--  end if;	 
--end process;
--
--
--
--process (pxlCLK,zerocnt,frmst,istext,isgraph,screenend,rows)
--Begin
--   if frmst='1' or rows>249 then
--    istext<='1';
--	 isgraph<='0';
--	 screenend<='0';
--  elsif rising_edge(pxlCLK) then
--    if istext='1' and zerocnt=2 then 
--      istext<='0';
--		
--    end if;	 
--    if istext='0' and isgraph='0' and zerocnt=4 then --todo get all graphs
--      isgraph<='1';
--  	   grphaddr<=addr-4;
--  	   grphrow<=rows;
--    end if;	 
--    if (istext='0' and isgraph='0' and zerocnt=0) then -- or rows>249 then
--     screenend<='1';
--    end if;	  
--  end if; 
--End process;
--
--Process (pxlCLK)
--Begin
-- if rising_edge(pxlCLK) then
--   if istext='1' then
--     pxl<=charDATACUR(bitcnt);   
--	elsif isgraph='1' then
--     pxl<=DATACUR(bitcnt);
--	else 
--	  pxl<='0';
--	end if;  
-- end if;
--
--End process;


process (pxlcLK,frmst,hcount)
Variable vcol:integer range 0 to 650;
Variable vrow:integer range 0 to 255;
variable vrcnt:integer range 0 to 20;
Variable vchar:integer range 0 to 90;
Variable vline:integer range 0 to 30;
variable b:integer range 0 to 10;
variable z,testz:integer range 0 to 20;
variable enter:boolean;
Variable vgraphstart:integer range 0 to 32767;
Variable vgraphrow:integer range 0 to 255;
Begin
  if frmst='1' then
	 sTVCLK<='0';
    stmp<='0';
    istext<='1';
	 isgraph<='0';
	 screenend<='0';
    vrow:=0; 
	 vline:=0;
    if char80='0' then
   	  staddr<=nxtSTADDR&"0000010";
    else	  
	     staddr<=nxtSTADDR&"0000100"; 
	 end if;	  
  elsif rising_edge(pxlclk) and videov='1' then
   sTVCLK<='0';
   enter:=(hcount>=leftgap-7 and hcount<=leftgap+colend and vcount=41) or (hcount>leftgap and hcount<=leftgap+colend);
	
	if hcount=leftgap-8 then
    vcol:=0;  
	 vchar:=0;	
	 b:=0; 
	end if; 
	if hcount=leftgap-4 then
	  sTVCLK<='1';
	end if;
	if hcount=leftgap-2 then
	 DataCURnext<=Datain;
	 CharDataCURnext<=charDataIN;	  
	end if;
	if hcount=leftgap then
	 vcol:=0;
	 b:=0;
	 z:=0;
	 Datacur<=DataCURnext;
	 CharDatacur<=CharDataCURnext;
    if datACURnext="00000000" then
     z:=1;
    end if;	 
	end if;
	
	vrcnt:=vrow mod 10;-- todo : for 8 pixel char height
	rowcnt<=std_logic_vector(to_unsigned( vrcnt, 4));  -- 0 to 8 or 0 to 10 
	
	if isgraph='1' then
     addr<=vgraphstart+(vrow-vgraphrow)*graphperline+vchar;
	  	 -- addr<=to_integer(unsigned(staddr))+vline*charsperline+vrcnt*graphperline+vchar;	 

	else
	  addr<=to_integer(unsigned(staddr))+vline*charsperline+vchar;	 
	  
	end if; 
	
	if vgraphstart=1218 then
	 stmp<='1';
	end if; 
	
	
	linecnt<=vline;
	chartp<=vchar;
   
	col<=vcol;
	rows<=vrow;
	--	 b:=vcol mod 8;bitcnt<=b;		
	if b=0 then 
	 Datacur<=DataCURnext;
	 CharDatacur<=CharDataCURnext;	
	 if hcount>leftgap+1 and hcount<=leftgap+colend then
   	 if (isgraph='0') then
		   if DataCURnext="00000000" then
	        z:=z+1;
	      else 
  	        z:=0; 	
	      end if;
		end if;	
      if char80='0' then testz:=integer(z/2); 
		else testz:=z;
		end if;
	   if vrow>6 then
 		  if istext='1' and testz=2 then
  		    istext<='0';
		  end if;
		  if istext='0' and isgraph='0' and testz=4 then
		   isgraph<='1';
			vgraphstart:=addr-3;
			vgraphrow:=vrow;
		  end if;
		  if istext='0' and isgraph='0' and z=0  then
		   screenend<='1';
		  end if; 
      end if;
	 end if;	
	elsif b=2 then
	 sTVCLK<='1';
	elsif b=6 then  
	 DataCURnext<=Datain;
	 CharDataCURnext<=charDataIN;
	end if; 
	
	if istext='1' then
     pxl<=CharDataCUR(bitcnt);
	elsif isgraph='1' then
	  pxl<=DataCUR(bitcnt);
	else 
	  pxl<='0';
	end if;  
	

	zerocnt<=testz;
	
	bitcnt<=b;		
	b:=0;
   if enter then 	 
	 if (char80='1') then
	   vchar:=integer(vcol / 8)+1; -- 8 for hi or 16 for low def
	 else 
	   vchar:=integer(vcol / 16)+1; -- 8 for hi or 16 for low def
	 end if;	
	 vcol:=vcol+1;	 
	 if (char80='0') then --and (vcol mod 2=0)) then
	  b:=integer((vcol mod 16)/2);
	 elsif (char80='1') then
	   b:=vcol mod 8;
	 end if;	
	 if vcol=colend then	  
	  vrow:=vrow+1;
	  vline:=integer(vrow/10);
	 end if; 
	end if;--enter
	
	
  end if;

End process;


process (cpuClock,busACK)
begin
   if rising_edge(cpuClock) then 
     --get start address from NB through out 9,a
      IF BTN1='1' THEN
	     nxtSTADDR<="00000101";
      ELSIF sSETADDR='1' AND BUSACK='1'   THEN  -- FROM Z80		
			nxtSTADDR<=DATAin ;			
			--nxtSTADDR<="00000101";
		END IF;	
		
	end if;
end process;

	--sTvCLK <='1' when bitcnt=7 else '0';

   Vaddr<=std_logic_vector(to_unsigned(addr, 15));    
	
   DATAin <= DATA ;
	CHARDATAin <=CHARDATA;	
   VIDADDR <= '0'&Vaddr when busack='0' else (others=>'Z');
	TVCLK <= sTVCLK;
	CHARCOUNT <= rowcnt;  
	NBCOPINT <= '0' WHEN copcnt>=1 and copcnt<=4 else '1'; -- every 12,5 ms
	NBCLKINT  <= '0' when vcount>=1 and vcount<=4 else '1'; -- every 20 ms
	frmst <= '1' WHEN VCOUNT=41 and hcount= leftgap-8 ELSE '0';
	videov <= '1' when (vcount>40) and (vcount<=40+251) else '0';--visible lines
	csync <= hsync xnor vsync when (vcount<7) or (vcount>309) or (hcount<=80) else '1'  ;  	

   PXLOUT<= pxlshft or pxltest  when hcount>leftgap and hcount<=leftgap+640 and videov='1' and busack='0' 
			else '0';
	--pxlOUT2<=pxltest when hcount>leftgap and hcount<=leftgap+640+1 and videov='1' and busack='0';
	
	pxltest <=	--'0';
	    --'1' when (linecnt=1 and chartp=0) or (linecnt=2 and chartp=4) or (linecnt=3 and chartp=2)
	  -- else '1' when linecnt=2 and (bitcnt=6 or bitcnt=7) and chartp=2
		--else '1' when linecnt=4 and (bitcnt=0 or bitcnt=1) and chartp=2		
		--else '1' when rows>10 and rows<20
		--   '1' when (bitcnt=0 or bitcnt=1) and rows mod 2=0
		--  else '1' when chartp=1 or chartp=3 or chartp=4 or chartp=69 or chartp=79
		 -- '1' when (col=0 or col=colend-1) or col=colend/2
		 -- else '1' when rowcnt=9
		  
		-- '1' when chartp=0  and (rows mod 4=0)
		-- else '1' when chartp=1  and (rows mod 8=0)
		-- else '1' when chartp=2  and (rows mod 16=0)
		 --else '1' when nxtchar='1' and rows>10
		   '1' when hcount=leftgap and (rows mod 4=0)
			else '1' when isgraph='1' and (hcount=leftgap-2 or hcount=leftgap-3)
			else '1' when istext='1' and (hcount=leftgap-2 or hcount=leftgap-3)
			else '1' when screenend='1' and (hcount=leftgap-5 or hcount=leftgap-4)
			--else '1' when zerocnt=2 and bitcnt<3
			--else '1' when zerocnt=4
			
		  --else '1' when bitcnt=7
		  
		  --else '1' when rows=1  or rows=250 
		--else '0'; 
		--		'1' when (hcount=leftgap+1+2*8 or vcount=41+10) and videov='1'
		--	else '1' when (col=0 or col=colend+1)	and videov='1'
			--else '1' when (hcount=leftgap or hcount=leftgap+640+3)	and videov='1'
				else '0';

 --Video Signals
 		
	--this is for the whole 1024 pixels OF 250 line maybe more
	enbus1<='1' when (hcount>leftgap-48) and (hcount<leftgap+640+8) else '0'; --128 Enabled Bus Request
	
	sBusReq <= '0' when (vcount>40 and vcount <291) and enbus1='1' and  enable='1' else '1'; -- active low	
	BusReq<=sBusReq;
	

	
	pxlSHFT<='0' when screenend='1'
	    else pxl ;
	 --else pxlShft;

	
	--byteval<=CharDataIN when bitcnt=0 and istext='1'
	  --  else DataIN when bitcnt=0 and isgraph='1';
		
   clk8<= not clk8 when rising_edge(c16);
	
   pxlclk<=c16;
--	pxlclk<=c16 when char80='1'
--	else  c8;

	PXLCLKOUT<=PXLCLK; 
	 
--	TEST<=clk8;  
	colend<=640;
	--colend<=320 when char80='0'
	--	else 640;
		

   Led1<=ENABLE;	
	Led2<='1' when bitcnt>4 else '0';	    
	Led3<='1' WHEN STADDR="00000101"
	     ELSE '0';
		  
	Led4<='0' when frmst='1'
	    else '1' when zerocnt=4 and vcount<155;
		 
	Led5<=isgraph;	
	Led6<=istext;	
	Led7<=stmp;
	Led8<=char80;	

	sysClk16<=C16;--out
	sysClk4<=C4;--out
	

	--VClock<=CloCKIN;
	cpuClock<=clk4;	
	Vclock<=c16;
	--cpuClock<=c4;

	char80<=s80l or btn2;
   charsperline<=64 when char80='0'
	 else 128;
   graphperline<=40 when char80='0'
	 else 80;


	grphdiff<= rows-grphrow when rows>=grphrow
	  else 0;
	
	myPLL_inst : work.myPLL PORT MAP (
		areset	 => areset_sig,
		inclk0	 => sysClk,
		c0	 		 => c16,
		c1	 		 => c4,
		c2			 => c8,
		locked	 => locked_sig
	);
	
end Behavioral;

