library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

use work.helpers.all;

entity Interceptor is
  generic (
--    N          : natural;
    lookup_tbl : lookup_t
    );

  port(
    clock    : in  std_logic;
    addr     : in  unsigned(10 downto 2);
    data     : in  unsigned(31 downto 0);
    data_out : out unsigned(31 downto 0)
    );
end Interceptor;

architecture Interceptor of Interceptor is
  constant N : natural := lookup_tbl'left;
begin

  process (addr, data)
  begin
    for i in 1 to N loop
      -- Traverse lookup array and try to match an address. If we do, output
      -- it. *First Match*
      if addr = lookup_tbl(i) then
        data_out <= data;
      end if;
    end loop;
  end process;
  --  if addr = 3 then
  --    data_out <= data;
  --  end if;
  --end process;

end Interceptor;
