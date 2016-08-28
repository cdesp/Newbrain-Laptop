library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.all;
use IEEE.std_logic_unsigned.all; 

entity piso is
    port(
         clk,load  : in std_logic;
         pi        : in std_logic_vector(7 downto 0);
			ENABLED   : in std_logic;
         so        : out std_logic);
end piso;

architecture arch of piso is

    signal t   : std_logic;
    signal temp: std_logic_vector(7 downto 0);

begin

   process (clk,pi,load)
    begin
       if (load='1') then
             temp(7 downto 0) <= pi(7 downto 0);
       elsif (CLK'event and CLK='1') AND ENABLED='1' then
             t <= temp(0);
             temp(6 downto 0) <= temp(7 downto 1);
             temp(7) <= '0';
       end if;
    end process;

so <= t when enabled='1'
else '0';

end arch;
