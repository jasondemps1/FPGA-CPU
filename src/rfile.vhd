-- rfile - Register File
-- Digipen Institute of Technology

-- rfile - Register File
-- Triple port 16 word register file, 3 Read ports, 1 Write port
-- Digipen Instutute of Technology
-- 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rfile is
  port(radr1, radr2, radr3, wadr : in  unsigned(3 downto 0);
       wdata                     : in  unsigned(31 downto 0);
       rdata1, rdata2, rdata3    : out unsigned(31 downto 0);
       write                     : in  std_logic;
       clock                     : in  std_logic);
end rfile;

architecture rfile of rfile is
  type rfile_t is array(0 to 15) of unsigned(31 downto 0);
  signal regs1, regs2, regs3        : rfile_t := (others => x"00000000");
  signal fwd1, fwd2, fwd3           : boolean;
  signal bypass_data, rd1, rd2, rd3 : unsigned(31 downto 0);

begin
  process(clock)
  
  begin
    if rising_edge(clock) then
      rd1 <= regs1(to_integer(radr1));
      rd2 <= regs2(to_integer(radr2));
      rd3 <= regs3(to_integer(radr3));

      fwd1 <= false;
      fwd2 <= false;
      fwd3 <= false;

      if write = '1' then
        regs1(to_integer(wadr)) <= wdata;
        regs2(to_integer(wadr)) <= wdata;
        regs3(to_integer(wadr)) <= wdata;

        fwd1 <= (wadr = radr1);
        fwd2 <= (wadr = radr2);
        fwd3 <= (wadr = radr3);

        bypass_data <= wdata;
      end if;

    end if;
  end process;

  rdata1 <= bypass_data when fwd1 else rd1;
  rdata2 <= bypass_data when fwd2 else rd2;
  rdata3 <= bypass_data when fwd3 else rd3;
end rfile;
