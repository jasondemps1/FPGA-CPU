-- Instruction memory
-- Digipen Institute of Technology

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity imem is
  port(im_addr  : in  unsigned(10 downto 1);
       im_instr : out unsigned(15 downto 0);
       clock    : in  std_logic);
end imem;

architecture imem of imem is
  type imem_t is array(0 to 1023) of unsigned(15 downto 0);
  signal rom                     : imem_t;
  attribute ram_init_file        : string;
  attribute ram_init_file of rom : signal is "cpu.mif";
begin
  process(clock)
  begin
    if rising_edge(clock) then
      im_instr <= rom(to_integer(im_addr));
    end if;
  end process;
end imem;
