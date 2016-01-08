library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

entity clock_div is
  generic (N : natural);
  port(
    halt    : buffer std_logic;
    rst     : in     std_logic;
    clk_in  : in     std_logic;
    clk_out : buffer std_logic
    );
end clock_div;

architecture clock_div of clock_div is
  signal counter : unsigned(31 downto 0) := (others => '0');
  --signal halt_tmp : std_logic;
begin

  process(rst, clk_in, halt, counter, clk_out)
  begin
    if rst then
      halt <= not(halt);
    end if;

    if halt = '0' then
      if rising_edge(clk_in) then
        if counter < N then
          counter <= counter + 1;
        else
          counter <= (others => '0');
          clk_out <= not(clk_out);
        end if;
      end if;
    else
      counter <= (others => '0');
      clk_out <= '0';
    end if;

  end process;

end clock_div;
