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



--constant staddr:integer :=642;
--Signal staddr: integer range 0 to 32767  := 640; --start video address = 642 decimal 1604
Signal staddr:std_logic_vector(8-1 downto 0):="00000101";
--Signal staddrIN:std_logic_vector(8-1 downto 0):="00000101";
--Signal addrcnt : integer range 0 to 32767;
--Signal addrvec:std_logic_vector(15-1 downto 0);
Signal bitcnt : integer range 0 to 7:=0;

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


begin

process (CLOCKIN,FRMST,DATAREAD,ENABLE) --CLOCK
begin
 
  if FRMST='1' then -- frame start
  
  
	 -- strtcnt<="00";
	  
  	  if s80L='0' then
			 addrcol<="0000010"; --skip 2 bytes for initilization
	  else
			 addrcol<="0000100"; --skip 4 bytes for initilization
	  end if;
	  
	  addrrow<=staddr	  ;--"00000101";--5=start address =640
    
	  
	  bitcnt <= 7;
	 -- istest <= '1';
	  istext <= '1';
	  isgraph <= '0'; 
	  zerocnt <=0;
	  col <=0;
	  row <=0;
	  rowcnt<="0000";	  
	  redo<='0';
	  screenend<='0';  
	  start<='0';
	  pxlshft<='0';
	--  datout<='0';
	  EOT<='0';
	  RECOUNT<='0';
	  	  
	  byteval <= "00000000";
	 elsif rising_edge(CLOCKIN) then --CLOCK
      --istest <= '0';
		pxlshft<='0';
	  	
     -- datout<='0';
		IF SETADDR='1' AND BUSACK='1' THEN --AND SCREENEND='1' -- FROM Z80
		
			STADDR<=DATAIN ;
		END IF;	
		

		
		
	if DATAREAD='1' and busack='0' and (screenend='0') then
		
	   if  start='0' then --getting the data byte we need >70ns			
		   start<='1';			
			bitcnt <= 7;
			addrcol(6)<=video9;
		   if (DATAIN="00000000") then
			  EOT<='1';
			  datanext<=DATAIN;		
			else 
			  datanext<=chardataIN;	        
			end if;  
			byteval<=datanext;			
		end if;	
	  --main screen paint

	      pxlshft <= byteval(bitcnt) AND NOT SKIP; 	 				
		  
			if ( s80L='0') then	--all pixels are doubled		  
			  redo <= not redo;			  
			else 
           redo <= '0';
			end if;  

		if redo='0' then
  		   bitcnt <= bitcnt + 1;

			IF bitcnt=0 then
				SKIP<='0';	  
				if zerocnt=2 and istext='1' then
				   istext<='0'; -- text end
				end if;		
			   
				
				IF ISGRAPH='0' AND zerocnt=4 THEN
				 IF (S80L AND S3240)='1' THEN			
				    IF RECOUNT='1'  THEN-- 8 BYTES ON NARROW 80LINE SCREEN
					  isgraph<='1'; -- graph on
					  EOT<='0';
					  SKIP<='1';				  
					 ELSE 
					  RECOUNT<='1';
					  zerocnt <=0;
					 END IF; 
				 ELSE
				    isgraph<='1'; -- graph on
					 EOT<='0';
					 SKIP<='1';
				 END IF;	 
				END IF; 	
				
				
				if istext='0' and isgraph='0' and zerocnt = 0 then--check this 0,0,20,20 end the text screen
		         screenend <='1'; 
		      end if;	

				
			
				
			--elsif bitcnt=1  then	--NEXT ADDRESS
 		   	
			  -- IF NOT (S3240='1' AND ISGRAPH='1' AND NEXTISGRAPH='0' AND (col<64 OR COL>575) ) THEN --512 PIXELS
				
			   IF (S3240='1' AND ISGRAPH='1'  AND (col<64 OR COL>575) ) THEN --512 PIXELS					
					SKIP<='1';
				ELSE	
				  addrcol<=addrcol+1; --count column bytes 127 for graphics								  
				END IF;
				
				if isgraph='1' then
				  if addrcol="1111111"  then
			        addrrow<=addrrow+1;				 
				  end if; 
				 END IF; 
				if istext='1' then
				   if s80L='1' then -- hi def 80 pixels per line
					 if addrcol="1010011" then  --4-84 for 80l
						rowcnt<=rowcnt+1;
						addrcol<="0000100";-- start 0n 4
						EOT<='0';
						if (UCR='0' and rowcnt=9)  or (UCR='1' and rowcnt=7) then	
						  rowcnt<= "0000";
						  addrrow<=addrrow+1;
						  EOT<='0';
						end if; 
					 end if;	
					else --s80l='0' low def 40 pixels per line					  
  					  if addrcol="0101001" then  --2-41 set 7th bit for odd
						rowcnt<=rowcnt+1;
						addrcol<="0000010";	-- start on 2
						EOT<='0';
						if (UCR='0' and rowcnt=9) or (UCR='1' and rowcnt=7) then					     
						  rowcnt<= "0000";	
						  addrcol<="1000010";	--instead of adding a row we just move a little further skip 24 excess bytes					  
						  EOT<='0';
						end if; 	
					  end if;
					  if addrcol="1101001"  then  --66-106 add new row for even				  		 
						rowcnt<=rowcnt+1;
						addrcol<="1000010";
						EOT<='0';
						if (UCR='0' and rowcnt=9) or (UCR='1' and rowcnt=7) then					     
						  rowcnt<= "0000";
						  addrcol<="0000010";
						  addrrow<=addrrow+1;
						  EOT<='0';
						end if; 						
					  end if;												   
					  
					end if; -- else s80l =0  				   	 				  
			   end if;		-- if is text				
		--   elsif (bitcnt=3 OR bitcnt=4)  AND ISTEXT='1' then	--NEXT CHAR	
	   --    sTVCLK <= '1';	
			
			elsif bitcnt=3  then	--PREPARE NEXT BYTE	
			  if istext='1' then			   	
				 DATANEXT<=chardataIN;
			  elsif isgraph='1' then
				DATANEXT<=DATAIN; -- get the next data as we had the time to stabilize ram
			  else
				DATANEXT<= DATAIN;  
			  end if;	
			    
				if DATAIN="00000000" then -- test end of screen or eot or graph
				  zerocnt<=zerocnt+1;
				else
				  zerocnt<=0;
				end if;  
							
			
			elsif bitcnt=7  then -- NEXT BYTE
				--bitcnt<=0;
				
				IF ISTEXT='1' AND DATAIN="00000000" THEN
  			     EOT<='1';
				  byteval<=DATAIN;
				else 
		        byteval<=datanext;		
			   END IF;			  				
         
			END IF; --END BITCNT
			
		end if;  --IF REDO END
		 
	--	END IF; --IF SKIP=0  
		
		  col<=col+1;		   
		   if col>=639 then
			  col<=0;
			  row<=row+1;
			end if;
			if row>249 then
			  screenend<='1';
			end if;
			
		 
	
	  end if ; -- if frmst & Dataread
	end if; -- if clock	

end process;
   
	PXLOUT<= pxlshft XOR rv when busack='0' and dataread='1' and start='1' and EOT='0'
				ELSE '0' XOR rv when busack='0' and dataread='1' and start='1' and EOT='1'
				ELSE '0' ;
	
	--PXLOUT<=pxlshft when busack='0' and dataread='1' and start='1' and RV='0' 
	--        else not pxlshft when busack='0' and dataread='1' and start='1' and RV='1' 
	--        else '0';	
	--PXLOUT3<=datout when dataread='1' else '0';
	PXLOUT2<= '1' when dataread='1' and (col=0 OR col=639 OR row=0 or row=249 OR col=319 or row=124 ) else '0';

   VIDADDR <= '0'&addrrow&addrcol when  busack='0' 
			else (others=>'Z');


	CHARCOUNT <= rowcnt when busack='0' and dataread='1' --TODO:change R4 with UCR always count 0-7
	    else (others=>'0');										  --UCR=0 ~ 8x10chars  UCR=1 ~ 8x8 char
	
	--STADDR <= "00000101";
--	STADDR <= DATA WHEN SETADDR='1'
--	 ELSE STADDR;
	
   sTVCLK <= '1' WHEN (BITCNT=2  and dataread='1') ELSE
	          '1' WHEN FRMST='1'
				 ELSE '0';--textclock in char
	
	
	
	TVCLK <= sTVCLK;
	TEST<=S3240_2;
	
   DATAin <= DATA;
	CHARDATAin <=CHARDATA;	
	
  -- VIDADDR <= '0'&std_logic_vector(to_unsigned(addrrow,8))&std_logic_vector(to_unsigned(addrcol,7)) when busack='0' and dataread='1' else (others=>'Z');			
 --VIDADDR <= std_logic_vector(to_unsigned(addrcnt,16)) when   dataread='1' else (others=>'Z');
 --VIDADDR <= '0'&addrvec when busack='0' and dataread='1' else (others=>'Z');
 --CHARCOUNT <= std_logic_vector(to_unsigned(rowcnt,4)) when busack='0' else (others=>'0');

   vga1 : entity work.vga port map(CLOCKIN,busreq,busack,ENABLE,DATAREAD,CSYNC, FRMST,NBCLKINT,NBCOPINT); 
	
end Behavioral;

