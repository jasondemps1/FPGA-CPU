library ieee;
use ieee.std_logic_1164.all;

-- Clock Gating as per Altera's recommendations. (ID: 308019)
entity clock_gate is
  port (
    clk_in  : in std_logic;
	 clk_en  : in std_logic;
    clk_out : out std_logic
    );
end entity;

architecture clock_gate of clock_gate is
  signal clk_en_tmp : std_logic;
begin
  
  clk_out <= clk_en_tmp and clk_in;
    
  process(clk_in)
  begin
    if falling_edge(clk_in) then
      clk_en_tmp <= clk_en;
    end if;
  end process;

end architecture;
