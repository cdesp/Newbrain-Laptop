library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;



entity vga is
    port(clk: in std_logic;   --clk:A9		
			Busreq: out std_logic;
			Busack: in std_logic;
			VGAen: in std_logic;
			
			dataO: out std_logic; --we output pixel
			--hsyncO: out std_logic;
			--vsyncO: out std_logic;
			csyncO: out std_logic; 
			
		--	VISLN: out std_logic; 
			 
			FRMst: out std_logic; -- frame start		
					
			CLOCKINT: out std_logic;
			COPREGINT: out std_logic
			);
end vga;

architecture synt of vga is

signal videov, videoh,hsync, vsync : std_logic:='0';
signal hcount : integer range 0 to 1024:=0; --1023 for 16 mhz 720 for 13.5
signal vcount : integer range 1 to 312:=1; -- vert lines 625 312 for a frame
signal LCDdbl : std_logic:='0';
signal internCLK,internReset: std_logic;



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
signal clktmp:std_logic:='0';
signal copcnt:integer range 0 to 195:=0;

signal enbus:std_logic:='0';
--Signal stinit:std_logic:='0';

--16Mhz Clock makes 1024pixels at 64us
--13.5Mhz Clock makes 864pixels at 64us
begin

--process (clkin)
--begin
-- if rising_edge(clkin) then    

    --clktmp<=not clktmp;	 
	 --if clktmp='1' then
	 --  clk<='1';
	 -- else
    --  clk<='0';
	 -- end if;
--  end if;

--end process;


hcounter: process (clk)
begin
        if (clk'event and clk='1') then 
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
	  --   videoh <= '1';
	  --elsif (hcount>=leftgap) and (hcount<(640+leftgap)) then 
	  if (hcount>=leftgap) and (hcount<(640+leftgap)) then 
        videoh <= '1';
     end if;
end process;


vcounter: process (clk)
begin
        if (clk'event and clk='1') then 
            if hcount>maxpixels then 
                if vcount>maxlines then --312 including the 0
                    vcount <= 1;
                else 
                    vcount <= vcount + 1;						  
                end if;
            end if;
        end if; 
end process;

COPcounter: process (clk)
begin
        if (clk'event and clk='1') then 
            if hcount>maxpixels then 				    
                if copcnt>=195 then -- 194 CAUSE WE START FROM 1 IF START FROM 0 NOT COMPILE
                    copcnt <= 1;
                else 
                    copcnt <= copcnt + 1;						  
                end if;
            end if;
        end if; 
end process;


--process (vcount)

   
--begin
    
	 --CLOCKINT and COPREGINT are active low signals
	 
	-- if vcount = 0 then -- 50Hz-20ms NB clock interrupt
	--   CLOCKINT <= '0';
	-- else
   --   CLOCKINT <= '1';		 
	-- end if;

	 --cop REGINT occurs every 12.5ms 
	 -- vcount goes from 0 to 311 =312 times
	 -- COP INT every 195 lines
	
	-- if (vcount = 0) or (vcount = 78) or (vcount = 156) or (vcount = 234)  then 
	
	 --if (vcount = 0) or (vcount = 156)  then 
	 --  COPREGINT <= '0';
	 --else
    --  COPREGINT <= '1';		 
	 --end if;
	 
	
	 
	 

    -- CRT display stuff

--	 if (vcount>40) and (vcount<=40+250)  then --279 +vertgap 45 lines total 256 vert lines
  --      videov <= '1';
	-- else
--		 videov <= '0';
--end if;
	 
   --if vcount=41 then --we want two more pixels in 1st line
--     stinit<='1';
   --else 
--     stinit<='0'; 
   --end if;
	 
	-- if vcount = 38 then
	--   frmst <= '1';
	-- else
   --   frmst <= '0';		 
	-- end if;
	 
	-- If (vcount > 27) and (vcount < 280) and (VGAEn='1') then
	--   busreq <= '0';
	-- else
	-- 	busreq <= '1';
	--end if;	
		
	 
--end process;

sync: process (clk)

	

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
        if (clk'event and clk='1') then 
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



	dataO <= videoh and videov when busack='0' else '0';-- if we should output to screen 
		
	--this is for the whole 1024 pixels OF 250 line maybe more
	enbus<='1' when (hcount>leftgap-96) and (hcount<leftgap+640+96) else '0';
	 
	--Busreq <= '0' when vcount>39 and vcount <291  and VGAen='1' else '1'; -- active low	
	Busreq <= '0' when (vcount>40 and vcount <291) and enbus='1' and VGAen='1' else '1'; -- active low	
	
--	VISLN <=videov when   busack='0' else '0';
	COPREGINT <= '0' WHEN copcnt=1 else '1'; -- every 12,5 ms
	CLOCKINT  <= '0' when vcount=1 else '1'; -- every 20 ms
	frmst <= '1' WHEN VCOUNT=40 ELSE '0';
	--hsyncO <= hsync;
	--vsyncO <= vsync;
	--csyncO <= hsync xnor vsync;

	--clk<=clkin;
	videov <= '1' when (vcount>40) and (vcount<=40+250) else '0';
	
	csyncO <= hsync xnor vsync when (vcount<7) or (vcount>309) or (hcount<=80) else '1'  ;

	  
end synt;