library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

use work.helpers.all;

entity Store is
  port(
    defer_load : in  std_logic;
    dm_data_rd : in  unsigned(31 downto 0);
    instr      : in  unsigned(15 downto 0);
    wadr       : out unsigned(3 downto 0);
    wdata      : out unsigned(31 downto 0);
    write_reg  : out std_logic
    );
end Store;

architecture Store of Store is
  signal wadr_tmp   : unsigned(3 downto 0);
  signal wdata_tmp  : unsigned(31 downto 0);
  signal wr_reg_tmp : std_logic;
begin

  process (defer_load, dm_data_rd, instr)
  begin
    wadr      <= (others => '0');
    wdata     <= (others => '0');
    write_reg <= '0';

    if defer_load then                  --rising_edge(defer_load) then
      wadr      <= Register_Format(instr(2 downto 0), reg_len);
      write_reg <= '1';

      case instr(15 downto 11) is
        when "01001" =>                 -- LDR (#Imm8)
          wdata <= dm_data_rd;
        when "01010" =>                 -- LDRSB
          wdata <= unsigned(signed(dm_data_rd and resize(X"FF", dm_data_rd'length)));
        when "01011" =>                 -- Regs
          case instr(10 downto 9) is
            when "00" =>                -- LDR
              wdata <= dm_data_rd;
            when "01" =>                -- LDRH
              wdata <= dm_data_rd and resize(X"FFFF", dm_data_rd'length);
            when "10" =>                -- LDRB
              wdata <= dm_data_rd and resize(X"FF", dm_data_rd'length);
            when "11" =>                -- LDRSH
              wdata <= unsigned(signed(dm_data_rd and resize(X"FFFF", dm_data_rd'length)));
            when others =>
              wadr  <= (others => 'X');
              wdata <= (others => 'X');
          end case;

        when "01101" =>                 -- LDR (#Imm5)
          wdata <= dm_data_rd;
        when "01111" =>                 -- LDRB (#Imm5)
          wdata <= dm_data_rd and resize(X"FF", dm_data_rd'length);
        when "10001" =>                 -- LDRH (#imm5)
          wdata <= dm_data_rd and resize(X"FFFF", dm_data_rd'length);
        when "10011" =>                 -- LDR (#imm + PC)
          wadr  <= resize(instr(10 downto 8), wadr'length);
          wdata <= dm_data_rd;
        when others =>
          wadr  <= (others => 'X');
          wdata <= (others => 'X');
      end case;


    end if;
  end process;

end Store;
