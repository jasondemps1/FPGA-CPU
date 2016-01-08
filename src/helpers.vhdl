library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

package helpers is
  constant NOP     : unsigned(15 downto 0) := x"EF00";
  constant reg_len : natural               := 4;

  type lookup_t is array(integer range <>) of integer range 0 to 512; --unsigned(10 downto 2);

  -- Special Registers
  constant SP : unsigned(3 downto 0) := "1101";
  constant LR : unsigned(3 downto 0) := "1110";

  pure function Register_Format(
    reg     : unsigned;
    max_len : natural := reg_len
    ) return unsigned;
end package;

package body helpers is
  pure function Register_Format(
    reg     : unsigned;
    max_len : natural := reg_len
    ) return unsigned is

  begin
    if reg'length < max_len then
      return "0" & reg;
    else
      return reg;
    end if;
  end Register_Format;

end helpers;
