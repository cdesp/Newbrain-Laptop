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
           VIDADDR : inout  std_logic_vector(16-1 downto 0) ;           
			  CLOCKIN : in  STD_LOGIC;
           PXLOUT : out  STD_LOGIC;
			  PXLOUT2 : out  STD_LOGIC;			  	
			  CHARDATA : in  std_logic_vector(8-1 downto 0)  ;
			  CHARCOUNT : out  std_logic_vector(4-1 downto 0);			  			  
			  --VIDEO SIGNALS
			  sUCR:in STD_LOGIC; --10 PIXEL PER CHAR
			  s80L:in STD_LOGIC; -- 80 CHARS PER LINE (MEANS 1 PIXEL HORIZ)
			  s3240:in STD_LOGIC; -- narrow graphics screen
			  sFS:in STD_LOGIC; --
			  sRV:in STD_LOGIC; -- REVERSE FIELD
			--  TVP:in STD_LOGIC; -- eNABLE
			  sSETADDR: in STD_LOGIC;	
			  sVIDEO9: in STD_LOGIC;  -- 1 WHEN low VIDEO ADDR start from 1
			  TVCLK: OUT STD_LOGIC;
			  Busreq:out STD_LOGIC;
			 -- HSYNCO:out STD_LOGIC;
			 -- VSYNCO:out STD_LOGIC;
           CSYNC: out std_logic;  --composite sync		
			  TEST:OUT STD_LOGIC;
			  NBCLKINT:out STD_LOGIC;
			  NBCOPINT:out STD_LOGIC;
			  
			  Q7:in STD_LOGIC;
			  QD7:out STD_LOGIC;

			  PXLCLKOUT:out STD_LOGIC;
			  CLK4:in STD_LOGIC;
			  M1:in STD_LOGIC;
			  VidIsol:out STD_LOGIC;
			  
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
Signal nxtstaddr1:std_logic_vector(8-1 downto 0):="00000101";
Signal nxtstaddr2:std_logic_vector(8-1 downto 0):="00000101";
Signal nxtstaddr3:std_logic_vector(8-1 downto 0):="00000101";
signal nxtcnt:integer:=0;

Signal VAddr:std_logic_vector(15-1 downto 0);
Signal RAMAddr:std_logic_vector(13-1 downto 0);
signal RAMDATAin: std_logic_vector(8-1 downto 0);
signal RAMDATAout: std_logic_vector(8-1 downto 0);
signal RAMDATAoutRev: std_logic_vector(8-1 downto 0);

signal RAMWe: std_logic;
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
signal noBusrq,noBusrq2:std_logic;
signal narrowgap:integer range 0 to 10;
signal charHeight:integer range 0 to 10;
signal setvideo9:std_logic;
signal sQ7,sQD7,sQD72:std_logic;
Signal tFS:std_logic;

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
	 noBusrq<='0';
    if char80='0' then
   	  staddr<=nxtSTADDR&"0000010";
    else	  
	     staddr<=nxtSTADDR&"0000100"; 
	 end if;	  
	 if setvideo9='1' then
	  	  staddr(6)<='1';
	end if;
  elsif rising_edge(pxlclk) and videov='1' then
  
   sTVCLK<='0';
   enter:=(hcount>=leftgap-7 and hcount<=leftgap+colend and vcount=41) or (hcount>leftgap and hcount<=leftgap+colend);
	--enter:=enter and noBusrq='0';
	--if hcount<leftgap then
	-- noBusrq<='0';
--	 testz:=0;
	--end if; 	
	if hcount=leftgap-8 then
    vcol:=0;  
	 vchar:=0;	
	 b:=0; 
	 testz:=0;	 
	end if; 
	if hcount=leftgap-4 then
	  sTVCLK<='1';
	end if;
	--if hcount=leftgap-2 then
	if hcount=leftgap-3 then --1/10/2016
	 DataCURnext<=Datain;
	 sQD7<=Datain(7);	 
	end if;
	if hcount=leftgap-1 then
	 -- CharDataCURnext<=charDataIN;	 --1/10/2016
	  CharDataCURnext<= RAMDATAoutRev;
	end if;  
	if hcount=leftgap then
	 nobusrq<='0';
	 vcol:=0;
	 b:=0;
	 z:=0;
	 Datacur<=DataCURnext;
	 CharDatacur<=CharDataCURnext;	
	 sQD7<=DataCURnext(7);    
    if datACURnext="00000000" then
     z:=1;
    end if;	 
	end if;
	
	vrcnt:=vrow mod charHeight;-- Done : for 8 pixel char height   was 10
	rowcnt<=std_logic_vector(to_unsigned( vrcnt, 4));  -- 0 to 8 or 0 to 10 
	
	if isgraph='1' then
	 if s3240='0' then
     addr<=vgraphstart+(vrow-vgraphrow)*graphperline+vchar;
	 else --narrow screen
	   
	   addr<=vgraphstart+(vrow-vgraphrow)*(graphperline-(2*narrowgap))+vchar-narrowgap;
	 end if;
	else
	  addr<=to_integer(unsigned(staddr))+vline*charsperline+vchar;	 
	end if; 
		
	linecnt<=vline;
	chartp<=vchar;
   
	col<=vcol;
	rows<=vrow;
		
	if noBusrq='0' then
	if b=0 then 
	 Datacur<=DataCURnext;
	 CharDatacur<=CharDataCURnext;	 
	 sRVUP <= DataCURnext(7); 			 
	 if hcount>leftgap+1 and hcount<=leftgap+colend then
   	 if (isgraph='0') then
		   if DataCURnext="00000000" then
	        z:=z+1;
	      else 
			  if istext='1' and testz=1 then -- line ended release the cpu
			    noBusrq<='1';
			  end if;	 
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
			if s3240='0' then
			  vgraphstart:=addr-3;
			else 
			  if char80='0' then
	         vgraphstart:=addr+1;
			  else
			   vgraphstart:=addr+5;
			  end if; 
			end if;
			vgraphrow:=vrow;
		  end if;
		  if istext='0' and isgraph='0' and z=0  then
		   screenend<='1';
		  end if; 
      end if;
	 end if;		
	elsif b=3 then  
	 --sTVCLK<='1';
	 DataCURnext<=Datain;
	 sQ7<=Q7;	 
	elsif b=4 then  
	 sTVCLK<='1';
	 sQD7<=DataCURnext(7);
   elsif b=5 then
	 sTVCLK<='1';	 	 
	elsif b=6 then	  
	 sTVCLK<='1';
	  --CharDataCURnext<=charDataIN; --1/10/2016
	  CharDataCURnext<= RAMDATAoutRev;
	end if; 
	end if;  --nobusrq
	
	
	if istext='1' then
     pxl<=CharDataCUR(bitcnt);
	elsif isgraph='1' then
	 pxl<=DataCUR(bitcnt);
	 if s3240='1' then	   
	   if vchar<=narrowgap or vchar>graphperline-narrowgap then --narrow screen skip 4 or 8 bytes at start and 4 at the end
		 pxl<='0';
		end if; 
	 end if; 
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
	  vline:=integer(vrow/charHeight);--was 10
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
		 -- setvideo9<='0'; 
      ELSIF sSETADDR='1' AND BUSACK='1'   THEN  -- FROM Z80		
			nxtSTADDR<=DATAin ;			
			if nxtcnt=0 then
		     nxtSTADDR1<=DATAin ;			 	
			  nxtSTADDR2<=DATAin ;			 	
			  nxtSTADDR3<=DATAin ;			 	
			  nxtcnt<=3;
			else
	        nxtSTADDR1<=nxtSTADDR2;
			  nxtSTADDR2<=nxtSTADDR3;
			  nxtSTADDR3<=DATAin;  
			end if;
			
			if nxtSTADDR1=nxtSTADDR2 and nxtSTADDR1=nxtSTADDR3 then
			  nxtSTADDR<=nxtSTADDR1 ;			
			end if;
			
		--	setvideo9<='0'; 
			--nxtSTADDR<="00000101";
		elsE
      --  if svideo9='1' then
	   --    setvideo9<='1'; 
	   --  end if;	
		END IF;	
		
	end if;
end process;

	sQD72<='0' WHEN sFS='0' AND ISTEXT='1'
	     ELSE sQD7;
	QD7<=sQD72;
	
	--sTvCLK <='1' when bitcnt=7 else '0';
	RVUP <= sRVUP WHEN sFS='0' AND ISTEXT='1'
	     ELSE '0';
   	
	RVF <= sRV XOR RVUP;

   Vaddr<=std_logic_vector(to_unsigned(addr, 15));    
	RAMaddr<=sUCR&rowcnt&sQD72&DataCURnext(7-1 downto 0);
	   --     1     4     1       7
	DATAin <= DATA ;
	CHARDATAin <=CHARDATA;	
	--VIDADDR <= '0'&Vaddr when vidisol='0' else (others=>'Z');
   VIDADDR <= '0'&Vaddr when busack='0' and sBusReq='0' else (others=>'Z');
	VidIsol<= '0' when busack='0' and sBusReq='0'
	  else '1';
	
--	setvideo9<='1' when svideo9='1' AND BUSACK='1'
--	    else '0' when sSETADDR='1' AND BUSACK='1'
--		 else setvideo9;
	setvideo9<='0';
	   --    
	
	TVCLK <= sTVCLK;
	CHARCOUNT <= rowcnt;  
	NBCOPINT <= '0' WHEN copcnt>=1 and copcnt<=4 else '1'; -- every 12,5 ms
	NBCLKINT  <= '0' when vcount>=1 and vcount<=4 else '1'; -- every 20 ms
	frmst <= '1' WHEN VCOUNT=41 and hcount= leftgap-8 ELSE '0';
	videov <= '1' when (vcount>40) and (vcount<=40+251) else '0';--visible lines
	csync <= hsync xnor vsync when (vcount<7) or (vcount>309) or (hcount<=80) else '1'  ;  	

   PXLOUT<= (pxlshft XOR RVF) or pxltest  when hcount>leftgap and hcount<=leftgap+640 and videov='1' and busack='0' 
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
		  --'1' when nobusrq2='1'
		   '1' when hcount=leftgap and (rows mod 4=0)
			--else '1' when isgraph='1' and (hcount=leftgap-2 or hcount=leftgap-3)
			--else '1' when istext='1' and (hcount=leftgap-2 or hcount=leftgap-3)
			--else '1' when screenend='1' and (hcount=leftgap-5 or hcount=leftgap-4)
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
	enbus1<='1' when (hcount>leftgap-40) and (hcount<leftgap+640) and (vcount>40 and vcount <291) else '0'; --128 Enabled Bus Request
	
	nobusrq2<=nobusrq when  hcount>leftgap+8  else '0';
	sBusReq <= '0' when enbus1='1' and  enable='1' and nobusrq2='0' else '1'; -- active low	
	--sBusReq <= '0' when enbus1='1' and  enable='1' else '1'; -- active low	
	BusReq<=sBusReq;
	

	
	pxlSHFT<='0' when screenend='1'
	    else pxl ;
	colend<=640;
	--colend<=320 when char80='0'
	--	else 640;
	char80<=s80l;-- or btn2;
   charsperline<=64 when char80='0'
	 else 128;
   graphperline<=40 when char80='0'
	 else 80;
   narrowgap<=4 when char80='0'
	 else 8;
	grphdiff<= rows-grphrow when rows>=grphrow
	  else 0;
   charHeight<=10 when sUCR='0' else 8;
		
   clk8<= not clk8 when rising_edge(c16);
	
   pxlclk<=c16;
--	pxlclk<=c16 when char80='1'
--	else  c8;

	PXLCLKOUT<=PXLCLK; 
	 
--	TEST<=clk8;  
	
		

   Led1<=ENABLE;	
	Led2<=setVideo9;	    
	Led3<='1' WHEN STADDR="000001010000010"
	     ELSE '0';		  
	Led4<='0';
		 
	Led5<=isgraph;	
	Led6<=istext;	
	Led7<=stmp;
	Led8<=sUCR;	

	sysClk16<=C16;--out
	sysClk4<=C4;--out
	

	--VClock<=CloCKIN;
	cpuClock<=clk4;	
	Vclock<=c16;
	--cpuClock<=c4;

	
	myPLL_inst : work.myPLL PORT MAP (
		areset	 => areset_sig,
		inclk0	 => sysClk,
		c0	 		 => c16,
		c1	 		 => c4,
		c2			 => c8,
		locked	 => locked_sig
	);
	
	myRam_inst : work.RAM8k PORT MAP (
	  Clock => pxlclk,
	  wren      => RAMWe,
	  address => RamAddr,
	  data => RAMDATAin,
	  q => RAMDATAout
	);
	RAMDATAoutRev<=RAMDATAout(0)&RAMDATAout(1)&RAMDATAout(2)&RAMDATAout(3)&RAMDATAout(4)&RAMDATAout(5)&RAMDATAout(6)&RAMDATAout(7);
	
end Behavioral;

