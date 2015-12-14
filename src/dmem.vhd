-- dmem - Data memory
-- Digipen Institute of Technology

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dmem is
  port(dm_addr     : in  unsigned(10 downto 2);
       dm_data_wr  : in  unsigned(31 downto 0);
       dm_data_rd  : out unsigned(31 downto 0);
       dm_we       : in  std_logic;
       clock       : in  std_logic;
       dm_data_out : out unsigned(31 downto 0)
       );
end dmem;

architecture dmem of dmem is

begin
  process(clock)
    type dmem_t is array(0 to 511) of unsigned(31 downto 0);
    variable ram : dmem_t := (others => x"00000000");
  begin
    if rising_edge(clock) then
      if dm_we = '1' then
        ram(to_integer(dm_addr)) := dm_data_wr;
        dm_data_out <= dm_data_wr;
      end if;
      dm_data_rd  <= ram(to_integer(dm_addr));
    end if;

  end process;
end dmem;
